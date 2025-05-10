#!/usr/bin/env Rscript

# Crime & Safety Data Fetcher
# This script handles retrieval of crime data from FBI Uniform Crime Reports
# and Bureau of Justice Statistics

library(tidyverse)
library(httr)
library(jsonlite)
library(readxl)
library(lubridate)
library(sf)
library(zoo) # For interpolation if needed

#' Fetch crime & safety data
#'
#' Retrieves crime data from FBI Uniform Crime Reports and Bureau of Justice Statistics
#' for crime rates and incarceration metrics at the county level.
#'
#' @param years Vector of years to include
#' @param cache_dir Directory to store cache files
#' @param refresh_cache Whether to refresh the cache
#' @param allow_interpolation Whether to interpolate missing values
#' @param data_quality_flags List of standardized data quality flags
#' @param offline_mode Whether to skip all downloads and use only cached data
#' @param parallel Whether to use parallel processing
#' @param parallel_config Optional parallel processing configuration
#' @return A data frame with crime data for all requested years
fetch_crime_data <- function(years, 
                           cache_dir = "data/cache", 
                           refresh_cache = FALSE,
                           allow_interpolation = TRUE,
                           data_quality_flags = list(
                             direct = "direct",
                             interpolated = "interpolated",
                             extrapolated = "extrapolated",
                             missing = NA,
                             imputed = "imputed"
                           ),
                           offline_mode = FALSE,
                           parallel = FALSE,
                           parallel_config = NULL) {
  # Helper function for clean output
  print_msg <- function(msg) {
    # Check if being run interactively
    is_interactive_run <- !exists("is_sourced") || (is.logical(is_sourced) && !is_sourced)
    if (is_interactive_run) {
      message(msg)
    } else {
      cat(msg, "\n")
    }
  }
  
  # Setup parallel processing if enabled
  if (parallel) {
    # Use module_core.r's setup_parallel_processing if available
    if (exists("setup_parallel_processing")) {
      # Configure parallel processing with adaptive strategy
      if (is.null(parallel_config)) {
        parallel_config <- setup_parallel_processing(
          use_parallel = TRUE,
          num_cores = NULL,  # Auto-detect
          strategy = "auto", # Choose best strategy for platform
          memory_limit_gb = 8,
          chunk_size = 200
        )
      }
      print_msg("Parallel processing enabled for crime data")
    } else {
      # Basic parallel setup
      print_msg("Using basic parallel processing setup for crime data")
      if (!requireNamespace("future", quietly = TRUE)) {
        install.packages("future")
        library(future)
      }
      if (!requireNamespace("future.apply", quietly = TRUE)) {
        install.packages("future.apply")
        library(future.apply)
      }
      
      # Determine number of cores
      num_cores <- parallel::detectCores() - 1
      num_cores <- max(2, num_cores) # At least 2 cores
      
      # Choose strategy based on OS
      strategy <- if (.Platform$OS.type == "windows") {
        "multisession"
      } else {
        "multicore"
      }
      
      future::plan(strategy, workers = num_cores)
      options(future.globals.maxSize = 8 * 1024^3) # 8GB
      
      parallel_config <- list(
        enabled = TRUE,
        cores = num_cores,
        strategy = strategy,
        memory_limit_gb = 8,
        chunk_size = 200
      )
    }
  }
  
  # Define cache file
  cache_file <- file.path(cache_dir, "crime_data.rds")
  
  # Use cache if available and not refreshing
  if (!refresh_cache && file.exists(cache_file)) {
    print_msg("Loading cached crime data...")
    crime_data <- readRDS(cache_file)
    
    # Check if all requested years are in the cache
    cached_years <- unique(crime_data$year)
    missing_years <- setdiff(years, cached_years)
    
    # Check for empty cache with just placeholder data
    if (nrow(crime_data) <= 1) {
      print_msg("Cached crime data appears to be empty or a placeholder. Will process files again.")
      # Force refresh by continuing past this point
    } else if (length(missing_years) == 0) {
      print_msg("Using complete cached crime data.")
      return(crime_data %>% filter(year %in% years))
    } else {
      print_msg(paste("Cache missing years:", paste(missing_years, collapse=", ")))
    }
  }
  
  # Make sure cache directory exists
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    print_msg(paste("Created cache directory at:", cache_dir))
  }
  
  # Make data directory if needed
  data_dir <- "data/crime"
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
    print_msg(paste("Created crime data directory at:", data_dir))
  }
  
  # Helper function to safely download and read files
  safe_download <- function(url, destfile, description) {
    if (offline_mode) {
      print_msg(paste("Skipping download of", description, "(offline mode)"))
      return(file.exists(destfile))
    }
    
    print_msg(paste("Downloading", description, "from:", url))
    tryCatch({
      download.file(url, destfile, mode = "wb", quiet = TRUE)
      print_msg(paste("Successfully downloaded", description))
      return(TRUE)
    }, error = function(e) {
      print_msg(paste("Error downloading", description, ":", conditionMessage(e)))
      return(FALSE)
    })
  }
  
  # Function to check for local crime data files
  find_local_crime_files <- function() {
    # List of directories to check
    crime_dirs <- c(
      "data/crime",
      "data/cache/crime",
      "data/criminal_justice"
    )
    
    # List of possible file extensions
    file_exts <- c("\\.csv$", "\\.xlsx$", "\\.xls$")
    
    # Search for files
    all_files <- c()
    for (dir in crime_dirs) {
      if (dir.exists(dir)) {
        for (ext in file_exts) {
          files <- list.files(dir, pattern = ext, full.names = TRUE, recursive = TRUE)
          all_files <- c(all_files, files)
        }
      }
    }
    
    # Filter for UCR/FBI/BJS files
    crime_files <- grep("ucr|fbi|bjs|uniform.*crime|crime.*report|justice.*statistics", 
                      all_files, value = TRUE, ignore.case = TRUE)
    
    return(crime_files)
  }
  
  # Function to get FBI UCR data
  get_ucr_data <- function() {
    # FBI UCR data is available from 2000 to 2021
    # The format changed in 2021 with the transition to NIBRS
    
    # Define variables we want to extract
    ucr_variables <- c(
      "violent_crime_rate" = "Violent crimes per 100,000 population",
      "property_crime_rate" = "Property crimes per 100,000 population",
      "homicide_rate" = "Homicides per 100,000 population"
    )
    
    # FBI UCR data list to store results
    ucr_data_list <- list()
    
    # Filter to years up to 2021
    ucr_years <- years[years >= 2000 & years <= 2021]
    
    # First, check for local UCR data files
    local_files <- find_local_crime_files()
    ucr_files <- grep("ucr|uniform.*crime|fbi", local_files, value = TRUE, ignore.case = TRUE)
    
    if (length(ucr_files) > 0) {
      print_msg(paste("Found", length(ucr_files), "local UCR data files"))
      
      # List to store processed files
      ucr_processed_files <- list()
      
      for (file in ucr_files) {
        print_msg(paste("Processing local UCR file:", basename(file)))
        
        # Try to extract year from filename
        year_match <- regexpr("(19|20)[0-9]{2}", basename(file))
        file_year <- NULL
        if (year_match > 0) {
          file_year <- as.integer(substr(basename(file), year_match, year_match + 3))
          print_msg(paste("Extracted year:", file_year))
        }
        
        # Skip if we can't determine year or it's not in requested years
        if (is.null(file_year) || !(file_year %in% ucr_years)) {
          next
        }
        
        # Try to read file
        tryCatch({
          # Determine file type
          if (grepl("\\.csv$", file, ignore.case = TRUE)) {
            file_data <- read_csv(file, show_col_types = FALSE)
          } else if (grepl("\\.xlsx$|\\.xls$", file, ignore.case = TRUE)) {
            file_data <- read_excel(file)
          } else {
            print_msg(paste("Unsupported file format:", file))
            next
          }
          
          # Check if file has usable data
          if (nrow(file_data) == 0) {
            print_msg("File has no data rows, skipping.")
            next
          }
          
          # Check for FIPS/GEOID column
          geoid_col <- grep("FIPS|fips|geoid|GEOID|county.*code|COUNTY.*CODE", 
                           names(file_data), value = TRUE)[1]
          
          if (is.na(geoid_col)) {
            print_msg("Could not identify GEOID column in UCR data file, skipping.")
            next
          }
          
          # Rename and format GEOID
          file_data <- file_data %>%
            rename(GEOID = all_of(geoid_col)) %>%
            mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
          
          # Find columns for variables of interest
          violent_col <- grep("violent.*rate|violent.*per|violent.*100", 
                             names(file_data), value = TRUE)[1]
          
          property_col <- grep("property.*rate|property.*per|property.*100", 
                              names(file_data), value = TRUE)[1]
          
          homicide_col <- grep("homicide.*rate|murder.*rate|homicide.*per|murder.*per", 
                              names(file_data), value = TRUE)[1]
          
          print_msg(paste("Found columns: Violent crime:", !is.na(violent_col),
                        "Property crime:", !is.na(property_col),
                        "Homicide:", !is.na(homicide_col)))
          
          # Create data frame for this year
          year_data <- data.frame(
            GEOID = file_data$GEOID,
            year = file_year
          )
          
          # Add variables if columns were found
          if (!is.na(violent_col)) {
            year_data$violent_crime_rate <- file_data[[violent_col]]
            year_data$violent_crime_rate_data_quality <- data_quality_flags$direct
            year_data$violent_crime_rate_data_source <- "FBI Uniform Crime Reports"
            year_data$violent_crime_rate_data_vintage <- as.character(file_year)
          }
          
          if (!is.na(property_col)) {
            year_data$property_crime_rate <- file_data[[property_col]]
            year_data$property_crime_rate_data_quality <- data_quality_flags$direct
            year_data$property_crime_rate_data_source <- "FBI Uniform Crime Reports"
            year_data$property_crime_rate_data_vintage <- as.character(file_year)
          }
          
          if (!is.na(homicide_col)) {
            year_data$homicide_rate <- file_data[[homicide_col]]
            year_data$homicide_rate_data_quality <- data_quality_flags$direct
            year_data$homicide_rate_data_source <- "FBI Uniform Crime Reports"
            year_data$homicide_rate_data_vintage <- as.character(file_year)
          }
          
          # Add to list
          ucr_data_list[[as.character(file_year)]] <- year_data
          ucr_processed_files <- c(ucr_processed_files, basename(file))
          
          print_msg(paste("Processed UCR data for", file_year, 
                        "from file", basename(file)))
        }, error = function(e) {
          print_msg(paste("Error processing file", basename(file), ":", 
                        conditionMessage(e)))
        })
      }
      
      print_msg(paste("Successfully processed", length(ucr_processed_files), 
                    "UCR data files:", paste(ucr_processed_files, collapse=", ")))
    } else {
      print_msg("No local UCR data files found. Trying download for each year...")
      
      # Try downloading for each year if needed
      for (year in ucr_years) {
        # Define file paths
        ucr_file <- file.path(data_dir, paste0("ucr_county_", year, ".csv"))
        
        # Check if we need to download
        need_download <- !file.exists(ucr_file) || refresh_cache
        
        if (need_download) {
          # FBI UCR URL
          # For 2021+, use crime-data-explorer API
          # For earlier years, use archived data
          if (year >= 2021) {
            ucr_url <- paste0(
              "https://crime-data-explorer.fr.cloud.gov/api/summarized/agencies/counties/",
              year, 
              "/offenses"
            )
          } else {
            ucr_url <- paste0(
              "https://s3-us-gov-west-1.amazonaws.com/cg-d4b776d0-d898-4153-90c8-8336f86bdfec/",
              year, 
              "/county_crime.csv"
            )
          }
          
          # Try to download
          if (!safe_download(ucr_url, ucr_file, paste("FBI UCR data for", year))) {
            print_msg(paste("Could not download FBI UCR data for", year))
            next
          }
        } else {
          print_msg(paste("Using existing FBI UCR file for", year))
        }
        
        # Process the data if file exists
        if (file.exists(ucr_file)) {
          print_msg(paste("Reading FBI UCR data for", year))
          
          # Read the file
          tryCatch({
            ucr_data <- read_csv(ucr_file, show_col_types = FALSE)
            
            # Get column names
            print_msg(paste("UCR data has", ncol(ucr_data), "columns and", nrow(ucr_data), "rows"))
            
            # Check for FIPS/GEOID column
            geoid_col <- grep("FIPS|fips|geoid|GEOID|county.*code|COUNTY.*CODE", 
                             names(ucr_data), value = TRUE)[1]
            
            if (is.na(geoid_col)) {
              print_msg("Could not identify GEOID column in UCR data")
              next
            }
            
            # Rename and format GEOID
            ucr_data <- ucr_data %>%
              rename(GEOID = all_of(geoid_col)) %>%
              mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
            
            # Find columns for our variables of interest
            
            # Violent crime rate
            violent_col <- grep("violent.*rate|violent.*per|violent.*100", 
                               names(ucr_data), value = TRUE)[1]
            
            # Property crime rate
            property_col <- grep("property.*rate|property.*per|property.*100", 
                                names(ucr_data), value = TRUE)[1]
            
            # Homicide rate
            homicide_col <- grep("homicide.*rate|murder.*rate|homicide.*per|murder.*per", 
                                names(ucr_data), value = TRUE)[1]
            
            print_msg(paste("Found columns: Violent crime:", !is.na(violent_col),
                          "Property crime:", !is.na(property_col),
                          "Homicide:", !is.na(homicide_col)))
            
            # Create data frame for this year
            year_data <- data.frame(
              GEOID = ucr_data$GEOID,
              year = year
            )
            
            # Add violent crime rate if available
            if (!is.na(violent_col)) {
              year_data$violent_crime_rate <- ucr_data[[violent_col]]
              year_data$violent_crime_rate_data_quality <- data_quality_flags$direct
              year_data$violent_crime_rate_data_source <- "FBI Uniform Crime Reports"
              year_data$violent_crime_rate_data_vintage <- as.character(year)
            }
            
            # Add property crime rate if available
            if (!is.na(property_col)) {
              year_data$property_crime_rate <- ucr_data[[property_col]]
              year_data$property_crime_rate_data_quality <- data_quality_flags$direct
              year_data$property_crime_rate_data_source <- "FBI Uniform Crime Reports"
              year_data$property_crime_rate_data_vintage <- as.character(year)
            }
            
            # Add homicide rate if available
            if (!is.na(homicide_col)) {
              year_data$homicide_rate <- ucr_data[[homicide_col]]
              year_data$homicide_rate_data_quality <- data_quality_flags$direct
              year_data$homicide_rate_data_source <- "FBI Uniform Crime Reports"
              year_data$homicide_rate_data_vintage <- as.character(year)
            }
            
            # Add to list
            ucr_data_list[[as.character(year)]] <- year_data
            
            print_msg(paste("Processed FBI UCR data for", year))
          }, error = function(e) {
            print_msg(paste("Error reading FBI UCR data for", year, ":", conditionMessage(e)))
          })
        }
      }
    }
    
    # Combine all years
    if (length(ucr_data_list) > 0) {
      combined_ucr <- bind_rows(ucr_data_list)
      print_msg(paste("Combined FBI UCR data with", nrow(combined_ucr), "rows"))
      return(combined_ucr)
    } else {
      print_msg("No FBI UCR data processed successfully")
      return(NULL)
    }
  }
  
  # Function to get Bureau of Justice Statistics data
  get_bjs_data <- function() {
    # BJS has county-level jail incarceration data
    
    # Define variables we want to extract
    bjs_variables <- c(
      "jail_incarceration_rate" = "County jail inmates per 100,000 population",
      "pretrial_detention_rate" = "Pretrial detainees per 100,000 population"
    )
    
    # BJS data list to store results
    bjs_data_list <- list()
    
    # Filter to years up to 2020
    bjs_years <- years[years >= 2000 & years <= 2020]
    
    # First, check for local BJS data files
    local_files <- find_local_crime_files()
    bjs_files <- grep("bjs|justice.*statistics|jail", local_files, value = TRUE, ignore.case = TRUE)
    
    if (length(bjs_files) > 0) {
      print_msg(paste("Found", length(bjs_files), "local BJS data files"))
      
      # List to store processed files
      bjs_processed_files <- list()
      
      for (file in bjs_files) {
        print_msg(paste("Processing local BJS file:", basename(file)))
        
        # Try to extract year from filename
        year_match <- regexpr("(19|20)[0-9]{2}", basename(file))
        file_year <- NULL
        if (year_match > 0) {
          file_year <- as.integer(substr(basename(file), year_match, year_match + 3))
          print_msg(paste("Extracted year:", file_year))
        }
        
        # Skip if we can't determine year or it's not in requested years
        if (is.null(file_year) || !(file_year %in% bjs_years)) {
          next
        }
        
        # Try to read file
        tryCatch({
          # Determine file type
          if (grepl("\\.csv$", file, ignore.case = TRUE)) {
            file_data <- read_csv(file, show_col_types = FALSE)
          } else if (grepl("\\.xlsx$|\\.xls$", file, ignore.case = TRUE)) {
            file_data <- read_excel(file)
          } else {
            print_msg(paste("Unsupported file format:", file))
            next
          }
          
          # Check if file has usable data
          if (nrow(file_data) == 0) {
            print_msg("File has no data rows, skipping.")
            next
          }
          
          # Check for FIPS/GEOID column
          geoid_col <- grep("FIPS|fips|geoid|GEOID|county.*code|COUNTY.*CODE", 
                           names(file_data), value = TRUE)[1]
          
          if (is.na(geoid_col)) {
            print_msg("Could not identify GEOID column in BJS data file, skipping.")
            next
          }
          
          # Rename and format GEOID
          file_data <- file_data %>%
            rename(GEOID = all_of(geoid_col)) %>%
            mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
          
          # Find columns for variables of interest
          jail_col <- grep("jail.*rate|incarceration.*rate|jail.*per|incarceration.*per", 
                          names(file_data), value = TRUE)[1]
          
          pretrial_col <- grep("pretrial.*rate|pretrial.*per", 
                              names(file_data), value = TRUE)[1]
          
          print_msg(paste("Found columns: Jail incarceration:", !is.na(jail_col),
                        "Pretrial detention:", !is.na(pretrial_col)))
          
          # Create data frame for this year
          year_data <- data.frame(
            GEOID = file_data$GEOID,
            year = file_year
          )
          
          # Add variables if columns were found
          if (!is.na(jail_col)) {
            year_data$jail_incarceration_rate <- file_data[[jail_col]]
            year_data$jail_incarceration_rate_data_quality <- data_quality_flags$direct
            year_data$jail_incarceration_rate_data_source <- "Bureau of Justice Statistics"
            year_data$jail_incarceration_rate_data_vintage <- as.character(file_year)
          }
          
          if (!is.na(pretrial_col)) {
            year_data$pretrial_detention_rate <- file_data[[pretrial_col]]
            year_data$pretrial_detention_rate_data_quality <- data_quality_flags$direct
            year_data$pretrial_detention_rate_data_source <- "Bureau of Justice Statistics"
            year_data$pretrial_detention_rate_data_vintage <- as.character(file_year)
          }
          
          # Add to list
          bjs_data_list[[as.character(file_year)]] <- year_data
          bjs_processed_files <- c(bjs_processed_files, basename(file))
          
          print_msg(paste("Processed BJS data for", file_year, 
                        "from file", basename(file)))
        }, error = function(e) {
          print_msg(paste("Error processing file", basename(file), ":", 
                        conditionMessage(e)))
        })
      }
      
      print_msg(paste("Successfully processed", length(bjs_processed_files), 
                    "BJS data files:", paste(bjs_processed_files, collapse=", ")))
    } else {
      print_msg("No local BJS data files found. Trying download for each year...")
      
      # Try downloading for each year if needed
      for (year in bjs_years) {
        # Define file paths
        bjs_file <- file.path(data_dir, paste0("bjs_jail_", year, ".csv"))
        
        # Check if we need to download
        need_download <- !file.exists(bjs_file) || refresh_cache
        
        if (need_download) {
          # BJS URL - placeholder, real URLs would depend on specific BJS data structure
          bjs_url <- paste0(
            "https://bjs.ojp.gov/content/pub/data/jail/county_jail_", 
            year, 
            ".csv"
          )
          
          # Try to download
          if (!safe_download(bjs_url, bjs_file, paste("BJS jail data for", year))) {
            print_msg(paste("Could not download BJS jail data for", year))
            # BJS data typically requires manual download from their site
            next
          }
        } else {
          print_msg(paste("Using existing BJS jail file for", year))
        }
        
        # Process the data if file exists
        if (file.exists(bjs_file)) {
          print_msg(paste("Reading BJS jail data for", year))
          
          # Read the file
          tryCatch({
            bjs_data <- read_csv(bjs_file, show_col_types = FALSE)
            
            # Get column names
            print_msg(paste("BJS data has", ncol(bjs_data), "columns and", nrow(bjs_data), "rows"))
            
            # Check for FIPS/GEOID column
            geoid_col <- grep("FIPS|fips|geoid|GEOID|county.*code|COUNTY.*CODE", 
                             names(bjs_data), value = TRUE)[1]
            
            if (is.na(geoid_col)) {
              print_msg("Could not identify GEOID column in BJS data")
              next
            }
            
            # Rename and format GEOID
            bjs_data <- bjs_data %>%
              rename(GEOID = all_of(geoid_col)) %>%
              mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
            
            # Find columns for our variables of interest
            
            # Jail incarceration rate
            jail_col <- grep("jail.*rate|incarceration.*rate|jail.*per|incarceration.*per", 
                            names(bjs_data), value = TRUE)[1]
            
            # Pretrial detention rate
            pretrial_col <- grep("pretrial.*rate|pretrial.*per", 
                                names(bjs_data), value = TRUE)[1]
            
            print_msg(paste("Found columns: Jail incarceration:", !is.na(jail_col),
                          "Pretrial detention:", !is.na(pretrial_col)))
            
            # Create data frame for this year
            year_data <- data.frame(
              GEOID = bjs_data$GEOID,
              year = year
            )
            
            # Add jail incarceration rate if available
            if (!is.na(jail_col)) {
              year_data$jail_incarceration_rate <- bjs_data[[jail_col]]
              year_data$jail_incarceration_rate_data_quality <- data_quality_flags$direct
              year_data$jail_incarceration_rate_data_source <- "Bureau of Justice Statistics"
              year_data$jail_incarceration_rate_data_vintage <- as.character(year)
            }
            
            # Add pretrial detention rate if available
            if (!is.na(pretrial_col)) {
              year_data$pretrial_detention_rate <- bjs_data[[pretrial_col]]
              year_data$pretrial_detention_rate_data_quality <- data_quality_flags$direct
              year_data$pretrial_detention_rate_data_source <- "Bureau of Justice Statistics"
              year_data$pretrial_detention_rate_data_vintage <- as.character(year)
            }
            
            # Add to list
            bjs_data_list[[as.character(year)]] <- year_data
            
            print_msg(paste("Processed BJS jail data for", year))
          }, error = function(e) {
            print_msg(paste("Error reading BJS jail data for", year, ":", conditionMessage(e)))
          })
        }
      }
    }
    
    # Combine all years
    if (length(bjs_data_list) > 0) {
      combined_bjs <- bind_rows(bjs_data_list)
      print_msg(paste("Combined BJS data with", nrow(combined_bjs), "rows"))
      return(combined_bjs)
    } else {
      print_msg("No BJS data processed successfully")
      return(NULL)
    }
  }
  
  # Get data from different sources - use parallel processing if enabled
  if (parallel && requireNamespace("future.apply", quietly = TRUE)) {
    print_msg("Using parallel processing to fetch data from multiple crime data sources")
    
    # Define the data sources to fetch
    data_sources <- c("ucr", "bjs")
    
    # Create a function to process one data source
    process_data_source <- function(source) {
      print_msg(paste("Processing crime data source:", source))
      
      if (source == "ucr") {
        return(get_ucr_data())
      } else if (source == "bjs") {
        return(get_bjs_data())
      } else {
        return(NULL)
      }
    }
    
    # Use future.apply to process data sources in parallel
    # Set up progress reporting if available
    if (requireNamespace("progressr", quietly = TRUE)) {
      # Create a progress handler
      progressr::handlers(progressr::handler_progress())
      
      # Process with progress tracking
      crime_data_sources <- progressr::with_progress({
        p <- progressr::progressor(steps = length(data_sources))
        
        future.apply::future_lapply(data_sources, function(source) {
          result <- process_data_source(source)
          p(message = paste("Processed crime data source:", source))
          return(result)
        })
      })
    } else {
      # Process without progress tracking
      crime_data_sources <- future.apply::future_lapply(data_sources, process_data_source)
    }
    
    # Convert results to named list
    names(crime_data_sources) <- data_sources
    
    # Filter out NULL results
    crime_data_list <- crime_data_sources[!sapply(crime_data_sources, is.null)]
    crime_data_list <- crime_data_list[sapply(crime_data_list, function(x) !is.null(x) && nrow(x) > 0)]
    
  } else {
    # Sequential processing
    print_msg("Using sequential processing to fetch data from multiple crime data sources")
    ucr_data <- get_ucr_data()
    bjs_data <- get_bjs_data()
    
    # Combine all data sources
    crime_data_list <- list()
    
    if (!is.null(ucr_data) && nrow(ucr_data) > 0) {
      crime_data_list[["ucr"]] <- ucr_data
    }
    
    if (!is.null(bjs_data) && nrow(bjs_data) > 0) {
      crime_data_list[["bjs"]] <- bjs_data
    }
  }
  
  # Process if we have data
  if (length(crime_data_list) > 0) {
    # Combine all sources and handle overlapping columns
    # Start with first dataset
    combined_crime_data <- crime_data_list[[1]]
    
    # Add each additional dataset
    if (length(crime_data_list) > 1) {
      for (i in 2:length(crime_data_list)) {
        next_data <- crime_data_list[[i]]
        
        # Identify common columns for joining
        join_cols <- intersect(names(combined_crime_data), names(next_data))
        
        # Ensure we have at least GEOID and year for joining
        if (all(c("GEOID", "year") %in% join_cols)) {
          # Find columns that might overlap (excluding join columns and flags)
          overlap_cols <- setdiff(
            intersect(names(combined_crime_data), names(next_data)),
            c(join_cols, grep("_data_quality$|_data_source$|_data_vintage$", 
                             names(next_data), value = TRUE))
          )
          
          if (length(overlap_cols) > 0) {
            # For overlapping columns, prefer data from the current dataset
            # Remove those columns from the combined data before joining
            combined_crime_data <- combined_crime_data %>%
              select(-all_of(overlap_cols))
          }
          
          # Join with the next dataset
          combined_crime_data <- full_join(
            combined_crime_data, 
            next_data, 
            by = join_cols
          )
        } else {
          print_msg("Warning: Cannot join datasets - missing common keys")
        }
      }
    }
    
    # Check if we need to handle missing years
    available_years <- unique(combined_crime_data$year)
    missing_years <- setdiff(years, available_years)
    
    if (length(missing_years) > 0 && allow_interpolation) {
      print_msg(paste("Interpolating data for missing years:", paste(missing_years, collapse=", ")))
      
      # Get all variables except join columns and metadata columns
      measure_cols <- setdiff(
        names(combined_crime_data),
        c("GEOID", "year", grep("_data_quality$|_data_source$|_data_vintage$", 
                               names(combined_crime_data), value = TRUE))
      )
      
      # Process each county separately for interpolation
      counties <- unique(combined_crime_data$GEOID)
      
      # Define function to interpolate a single county
      interpolate_county <- function(county) {
        # Get data for this county
        county_data <- combined_crime_data %>%
          filter(GEOID == county) %>%
          arrange(year)
        
        # Create grid with all years for this county
        county_grid <- data.frame(
          GEOID = county,
          year = years,
          stringsAsFactors = FALSE
        )
        
        # Process each measure column
        for (col in measure_cols) {
          # Get values for this column
          x_values <- county_data$year
          y_values <- county_data[[col]]
          
          # Identify valid data points (not NA)
          valid_indices <- !is.na(y_values)
          
          # Skip if no valid data
          if (sum(valid_indices) < 2) {
            next
          }
          
          x_valid <- x_values[valid_indices]
          y_valid <- y_values[valid_indices]
          
          # Use approx for interpolation
          interp_result <- approx(x_valid, y_valid, xout = county_grid$year, rule = 1)
          county_grid[[col]] <- interp_result$y
          
          # Set quality flags based on interpolation status
          quality_col <- paste0(col, "_data_quality")
          source_col <- paste0(col, "_data_source")
          vintage_col <- paste0(col, "_data_vintage")
          
          # Get original quality flags to preserve direct data
          if (quality_col %in% names(county_data)) {
            original_quality <- county_data[[quality_col]]
            original_years <- county_data$year
            
            # Create initial quality flags
            county_grid[[quality_col]] <- NA
            
            # For each year in our grid
            for (i in 1:nrow(county_grid)) {
              grid_year <- county_grid$year[i]
              
              # If this is an original data year, preserve its quality flag
              if (grid_year %in% original_years) {
                idx <- which(original_years == grid_year)
                county_grid[[quality_col]][i] <- original_quality[idx]
              } else if (grid_year > max(x_valid) || grid_year < min(x_valid)) {
                # Extrapolation
                county_grid[[quality_col]][i] <- data_quality_flags$extrapolated
              } else {
                # Interpolation
                county_grid[[quality_col]][i] <- data_quality_flags$interpolated
              }
            }
            
            # Also copy source and vintage
            if (source_col %in% names(county_data)) {
              source_values <- county_data[[source_col]]
              county_grid[[source_col]] <- source_values[1]  # Use first source
            }
            
            if (vintage_col %in% names(county_data)) {
              county_grid[[vintage_col]] <- paste0("derived_from_", 
                                                 paste(sort(unique(x_valid)), collapse="_"))
            }
          }
        }
        
        return(county_grid)
      }
      
      # Process counties in parallel if enabled
      interp_data_list <- if (parallel && requireNamespace("future.apply", quietly = TRUE)) {
        print_msg(paste("Using parallel processing for county interpolation with", length(counties), "counties"))
        
        # Setup progress tracking if available
        if (requireNamespace("progressr", quietly = TRUE)) {
          progressr::handlers(progressr::handler_progress())
          result_list <- progressr::with_progress({
            p <- progressr::progressor(steps = length(counties))
            
            future.apply::future_lapply(counties, function(county) {
              result <- interpolate_county(county)
              p(message = paste("Processed county", county))
              return(result)
            })
          })
        } else {
          # No progress tracking
          result_list <- future.apply::future_lapply(counties, interpolate_county)
        }
        
        # Convert to named list
        names(result_list) <- counties
        result_list
      } else {
        # Sequential processing
        print_msg(paste("Using sequential processing for county interpolation with", length(counties), "counties"))
        result_list <- list()
        
        for (county in counties) {
          result_list[[county]] <- interpolate_county(county)
        }
        
        result_list
      }
      
      # Combine all counties
      if (length(interp_data_list) > 0) {
        interp_data <- bind_rows(interp_data_list)
        
        # Add any missing metadata columns with NA values
        meta_cols <- grep("_data_quality$|_data_source$|_data_vintage$", 
                         names(combined_crime_data), value = TRUE)
        
        for (col in meta_cols) {
          if (!(col %in% names(interp_data))) {
            interp_data[[col]] <- NA
          }
        }
        
        # Use the interpolated data instead
        combined_crime_data <- interp_data
      }
    } else if (length(missing_years) > 0) {
      print_msg("Missing years but interpolation not allowed. Years will remain missing.")
    }
    
    # Filter to just the requested years
    combined_crime_data <- combined_crime_data %>%
      filter(year %in% years)
    
    # Sort data
    combined_crime_data <- combined_crime_data %>%
      arrange(GEOID, year)
    
    # Cache the processed data
    saveRDS(combined_crime_data, cache_file)
    print_msg(paste("Cached crime data to:", cache_file))
    
    return(combined_crime_data)
  } else {
    # No data available - create empty dataset with proper structure
    print_msg("No crime data found. Creating empty dataset with proper structure.")
    
    # Crime variables to include
    crime_vars <- c(
      "violent_crime_rate",
      "property_crime_rate",
      "homicide_rate",
      "jail_incarceration_rate",
      "pretrial_detention_rate"
    )
    
    # Try to get a county list if possible
    counties <- NULL
    tryCatch({
      # Check if we're running in a pipeline environment with get_county_list
      if (exists("get_county_list", mode="function")) {
        counties <- get_county_list()
        print_msg(paste("Using", nrow(counties), "counties from get_county_list function"))
      }
    }, error = function(e) {
      print_msg("Error getting county list from function")
    })
    
    # Fallback if counties is still NULL
    if (is.null(counties)) {
      # Create a placeholder with a few counties
      counties <- data.frame(
        GEOID = c("01001", "01003", "01005"), # Sample counties
        stringsAsFactors = FALSE
      )
      print_msg("Using placeholder county list for empty dataset structure")
    }
    
    # Create grid with all counties and years
    grid <- expand.grid(
      GEOID = counties$GEOID,
      year = years,
      stringsAsFactors = FALSE
    )
    
    # Add NAME column if available
    if ("NAME" %in% names(counties)) {
      grid$NAME <- counties$NAME[match(grid$GEOID, counties$GEOID)]
    }
    
    # Add empty variable columns with NAs and proper data quality flags
    for (var in crime_vars) {
      grid[[var]] <- NA_real_
      grid[[paste0(var, "_data_quality")]] <- data_quality_flags$missing
      grid[[paste0(var, "_data_source")]] <- "NO_DATA_AVAILABLE"
      grid[[paste0(var, "_data_vintage")]] <- NA_character_
    }
    
    crime_data <- as_tibble(grid)
    print_msg(paste("Created empty crime dataset with", nrow(crime_data), "rows"))
    
    # Cache the empty data
    saveRDS(crime_data, cache_file)
    print_msg(paste("Cached empty crime data structure to:", cache_file))
    
    # Provide clear error message about missing data
    print_msg("ERROR: No crime data files found. Please download crime data.")
    print_msg("Required files should be placed in: data/crime/")
    print_msg("File formats needed:")
    print_msg("1. FBI UCR data: CSV files with columns for GEOID/FIPS, violent crime rate, property crime rate")
    print_msg("2. BJS data: CSV files with columns for GEOID/FIPS, jail incarceration rate")
    print_msg("Filenames should include the year and data source (e.g., ucr_county_2021.csv)")
    
    return(crime_data)
  }
}

# This lets the function be used when the script is sourced
is_sourced <- function() {
  return(!identical(environment(), globalenv()))
}

# If the script is run directly, test the function
if (!is_sourced()) {
  cat("Testing crime data fetcher...\n")
  
  # Get current year
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  
  # Test for last 5 years
  test_years <- (current_year-4):current_year
  
  # Check for required packages for parallel processing
  has_parallel_deps <- requireNamespace("future", quietly = TRUE) && 
                       requireNamespace("future.apply", quietly = TRUE)
  
  # Use parallel processing if dependencies are available
  use_parallel <- has_parallel_deps
  if (use_parallel) {
    cat("Using parallel processing for crime data fetching test\n")
  } else {
    cat("Parallel processing dependencies not available, using sequential processing\n")
  }
  
  # Test the function
  result <- fetch_crime_data(
    years = test_years,
    cache_dir = "data/cache",
    refresh_cache = FALSE,
    allow_interpolation = TRUE,
    data_quality_flags = list(
      direct = "direct",
      interpolated = "interpolated",
      extrapolated = "extrapolated",
      missing = NA,
      imputed = "imputed"
    ),
    parallel = use_parallel
  )
  
  # Report data quality metrics
  cat("Data quality summary:\n")
  quality_cols <- grep("_data_quality$", names(result), value = TRUE)
  for (col in quality_cols) {
    var_name <- gsub("_data_quality$", "", col)
    quality_counts <- table(result[[col]], useNA = "always")
    cat(paste(" -", var_name, ":", paste(names(quality_counts), quality_counts, sep="=", collapse=", "), "\n"))
  }
  
  cat("Test completed with", nrow(result), "rows of data.\n")
}