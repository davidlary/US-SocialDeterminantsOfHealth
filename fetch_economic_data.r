#!/usr/bin/env Rscript

# Economic Data Fetcher
# This script handles retrieval of economic data from USDA Economic Research Service, 
# Bureau of Labor Statistics, and Opportunity Insights sources

library(tidyverse)
library(httr)
library(jsonlite)
library(readxl)
library(lubridate)
library(sf)
library(zoo) # For interpolation if needed

#' Fetch economic data
#'
#' Retrieves economic data from BLS, USDA Economic Research Service, and Opportunity Insights
#' for employment, economic typology, and economic mobility at the county level.
#'
#' @param years Vector of years to include
#' @param cache_dir Directory to store cache files
#' @param refresh_cache Whether to refresh the cache
#' @param allow_interpolation Whether to interpolate missing values
#' @param data_quality_flags List of standardized data quality flags
#' @param offline_mode Whether to skip all downloads and use only cached data
#' @return A data frame with economic data for all requested years
fetch_economic_data <- function(years, 
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
                              parallel = TRUE,
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
  
  # Define cache file
  cache_file <- file.path(cache_dir, "economic_data.rds")
  
  # Use cache if available and not refreshing
  if (!refresh_cache && file.exists(cache_file)) {
    print_msg("Loading cached economic data...")
    economic_data <- readRDS(cache_file)
    
    # Check if all requested years are in the cache
    cached_years <- unique(economic_data$year)
    missing_years <- setdiff(years, cached_years)
    
    # Check for empty cache with just placeholder data
    if (nrow(economic_data) <= 1) {
      print_msg("Cached economic data appears to be empty or a placeholder. Will process files again.")
      # Force refresh by continuing past this point
    } else if (length(missing_years) == 0) {
      print_msg("Using complete cached economic data.")
      return(economic_data %>% filter(year %in% years))
    } else {
      print_msg(paste("Cache missing years:", paste(missing_years, collapse=", ")))
    }
  }
  
  # Make sure cache directory exists
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    print_msg(paste("Created cache directory at:", cache_dir))
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
      print_msg("Parallel processing enabled for economic data with adaptive strategy")
    } else {
      # Basic parallel setup
      print_msg("Using basic parallel processing setup for economic data")
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
  
  # Make data directory if needed
  data_dir <- "data/economic"
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
    print_msg(paste("Created economic data directory at:", data_dir))
  }
  
  # Function to check for local economic data files
  find_local_economic_files <- function() {
    # List of directories to check
    economic_dirs <- c(
      "data/economic",
      "data/cache/economic",
      "data/econ"
    )
    
    # List of possible file extensions
    file_exts <- c("\\.csv$", "\\.xlsx$", "\\.xls$")
    
    # Search for files
    all_files <- c()
    for (dir in economic_dirs) {
      if (dir.exists(dir)) {
        for (ext in file_exts) {
          files <- list.files(dir, pattern = ext, full.names = TRUE, recursive = TRUE)
          all_files <- c(all_files, files)
        }
      }
    }
    
    # Filter for different types of economic data
    economic_files <- list(
      ers = grep("ers|usda|typology|employment", all_files, value = TRUE, ignore.case = TRUE),
      bls = grep("bls|labor|employment|jobs", all_files, value = TRUE, ignore.case = TRUE),
      opportunity = grep("opportunity|mobility|atlas", all_files, value = TRUE, ignore.case = TRUE),
      acs = grep("acs|inequality|gini", all_files, value = TRUE, ignore.case = TRUE)
    )
    
    return(economic_files)
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
  
  # Function to get USDA Economic Research Service data
  get_ers_data <- function() {
    # USDA ERS has data on county typology and economic indicators
    
    # Define the variables we want from ERS
    ers_variables <- c(
      "employment_volatility_index" = "Index of employment stability/volatility",
      "economic_typology" = "County economic typology",
      "persistent_poverty_county" = "Flag for counties with persistent poverty",
      "persistent_child_poverty_county" = "Flag for counties with persistent child poverty"
    )
    
    # Check for local files first
    local_files <- find_local_economic_files()
    ers_files <- local_files$ers
    
    if (length(ers_files) > 0) {
      print_msg(paste("Found", length(ers_files), "local ERS data files"))
      
      # Look for typology and employment files
      typology_file <- grep("typology|type", ers_files, value = TRUE)[1]
      employment_file <- grep("employ|job|labor", ers_files, value = TRUE)[1]
      
      if (!is.na(typology_file)) {
        print_msg(paste("Using local typology file:", basename(typology_file)))
      } else {
        # County Typology Codes
        typology_file <- file.path(data_dir, "county_typology.csv")
        
        # Check if we need to download
        need_download <- !file.exists(typology_file) || refresh_cache
        
        if (need_download) {
          # USDA ERS county typology URL
          typology_url <- "https://www.ers.usda.gov/webdocs/DataFiles/48652/2015CountyTypologyCodes.csv"
          
          # Try to download
          if (!safe_download(typology_url, typology_file, "USDA ERS County Typology")) {
            print_msg("Could not download USDA ERS County Typology data")
          }
        } else {
          print_msg("Using existing USDA ERS County Typology file")
        }
      }
      
      if (!is.na(employment_file)) {
        print_msg(paste("Using local employment file:", basename(employment_file)))
      } else {
        # Employment data
        # For employment volatility and other metrics
        employment_file <- file.path(data_dir, "county_employment.csv")
        
        # Check if we need to download
        need_download <- !file.exists(employment_file) || refresh_cache
        
        if (need_download) {
          # USDA ERS unemployment data URL
          emp_url <- "https://www.ers.usda.gov/webdocs/DataFiles/48747/Unemployment.csv"
          
          # Try to download
          if (!safe_download(emp_url, employment_file, "USDA ERS Unemployment Data")) {
            print_msg("Could not download USDA ERS Unemployment data")
          }
        } else {
          print_msg("Using existing USDA ERS Unemployment file")
        }
      }
    } else {
      # No local files found, try to download
      print_msg("No local ERS files found, attempting to download")
      
      # County Typology Codes
      typology_file <- file.path(data_dir, "county_typology.csv")
      
      # Check if we need to download
      need_download <- !file.exists(typology_file) || refresh_cache
      
      if (need_download) {
        # USDA ERS county typology URL
        typology_url <- "https://www.ers.usda.gov/webdocs/DataFiles/48652/2015CountyTypologyCodes.csv"
        
        # Try to download
        if (!safe_download(typology_url, typology_file, "USDA ERS County Typology")) {
          print_msg("Could not download USDA ERS County Typology data")
        }
      } else {
        print_msg("Using existing USDA ERS County Typology file")
      }
      
      # Employment data
      # For employment volatility and other metrics
      employment_file <- file.path(data_dir, "county_employment.csv")
      
      # Check if we need to download
      need_download <- !file.exists(employment_file) || refresh_cache
      
      if (need_download) {
        # USDA ERS unemployment data URL
        emp_url <- "https://www.ers.usda.gov/webdocs/DataFiles/48747/Unemployment.csv"
        
        # Try to download
        if (!safe_download(emp_url, employment_file, "USDA ERS Unemployment Data")) {
          print_msg("Could not download USDA ERS Unemployment data")
        }
      } else {
        print_msg("Using existing USDA ERS Unemployment file")
      }
    }
    
    # Process the typology data if file exists
    typology_data <- NULL
    if (file.exists(typology_file)) {
      print_msg("Reading USDA ERS County Typology data")
      
      # Read the file
      tryCatch({
        # Determine file type
        if (grepl("\\.csv$", typology_file, ignore.case = TRUE)) {
          typology_data <- read_csv(typology_file, show_col_types = FALSE)
        } else if (grepl("\\.xlsx$|\\.xls$", typology_file, ignore.case = TRUE)) {
          typology_data <- read_excel(typology_file)
        } else {
          print_msg(paste("Unsupported file format:", typology_file))
          return(NULL)
        }
        
        # Get column names
        print_msg(paste("Typology data has", ncol(typology_data), "columns and", nrow(typology_data), "rows"))
        
        # Check if this looks like typology data
        # Should have FIPS code and typology information
        fips_col <- grep("FIPS|fips|geoid|GEOID", names(typology_data), value = TRUE)[1]
        
        if (is.na(fips_col)) {
          print_msg("Could not identify FIPS column in typology data")
        } else {
          # Rename to GEOID
          typology_data <- typology_data %>%
            rename(GEOID = all_of(fips_col)) %>%
            # Ensure GEOID is properly formatted (5 digits with leading zeros)
            mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
          
          # Check for columns containing typology information
          typology_col <- grep("type|Type|TYPE|typology|Typology", names(typology_data), value = TRUE)[1]
          poverty_col <- grep("poverty|Poverty|POVERTY", names(typology_data), value = TRUE)[1]
          child_poverty_col <- grep("child.*poverty|Child.*Poverty", names(typology_data), value = TRUE)[1]
          
          print_msg(paste("Found columns: Typology:", !is.na(typology_col),
                        "Poverty:", !is.na(poverty_col),
                        "Child Poverty:", !is.na(child_poverty_col)))
        }
      }, error = function(e) {
        print_msg(paste("Error reading typology data:", conditionMessage(e)))
      })
    }
    
    # Process the employment data if file exists
    emp_data <- NULL
    if (file.exists(employment_file)) {
      print_msg("Reading USDA ERS Employment data")
      
      # Read the file
      tryCatch({
        # Determine file type
        if (grepl("\\.csv$", employment_file, ignore.case = TRUE)) {
          emp_data <- read_csv(employment_file, show_col_types = FALSE)
        } else if (grepl("\\.xlsx$|\\.xls$", employment_file, ignore.case = TRUE)) {
          emp_data <- read_excel(employment_file)
        } else {
          print_msg(paste("Unsupported file format:", employment_file))
          return(NULL)
        }
        
        # Get column names
        print_msg(paste("Employment data has", ncol(emp_data), "columns and", nrow(emp_data), "rows"))
        
        # Check if this looks like employment data
        # Should have FIPS code and year information
        fips_col <- grep("FIPS|fips|geoid|GEOID", names(emp_data), value = TRUE)[1]
        year_col <- grep("^year$|^yr$|^Year$", names(emp_data), value = TRUE)[1]
        
        if (is.na(fips_col)) {
          print_msg("Could not identify FIPS column in employment data")
        } else if (is.na(year_col)) {
          print_msg("Could not identify year column in employment data")
        } else {
          # Rename to GEOID and year
          emp_data <- emp_data %>%
            rename(
              GEOID = all_of(fips_col),
              year = all_of(year_col)
            ) %>%
            # Ensure GEOID is properly formatted (5 digits with leading zeros)
            mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
          
          # Filter to requested years
          emp_data <- emp_data %>%
            filter(year %in% years)
          
          # Check for volatility-related columns
          volatility_col <- grep("volatility|Volatility|stability|Stability", names(emp_data), value = TRUE)[1]
          
          print_msg(paste("Found employment volatility column:", !is.na(volatility_col)))
        }
      }, error = function(e) {
        print_msg(paste("Error reading employment data:", conditionMessage(e)))
      })
    }
    
    # Combine typology and employment data
    combined_ers_data <- NULL
    
    # Process typology data which applies to all years
    if (!is.null(typology_data)) {
      # Identify columns we'll use
      typology_col <- grep("type|Type|TYPE|typology|Typology", names(typology_data), value = TRUE)[1]
      poverty_col <- grep("poverty|Poverty|POVERTY", names(typology_data), value = TRUE)[1]
      child_poverty_col <- grep("child.*poverty|Child.*Poverty", names(typology_data), value = TRUE)[1]
      
      # Create data for each year
      ers_data_by_year <- list()
      
      for (year in years) {
        # Create data frame for this year
        year_data <- data.frame(
          GEOID = typology_data$GEOID,
          year = year
        )
        
        # Add economic typology if available
        if (!is.na(typology_col)) {
          year_data$economic_typology <- typology_data[[typology_col]]
          year_data$economic_typology_data_quality <- data_quality_flags$direct
          year_data$economic_typology_data_source <- "USDA ERS County Typology"
          year_data$economic_typology_data_vintage <- "2015" # Base year for typology
        }
        
        # Add persistent poverty if available
        if (!is.na(poverty_col)) {
          year_data$persistent_poverty_county <- typology_data[[poverty_col]]
          year_data$persistent_poverty_county_data_quality <- data_quality_flags$direct
          year_data$persistent_poverty_county_data_source <- "USDA ERS County Typology"
          year_data$persistent_poverty_county_data_vintage <- "2015" # Base year
        }
        
        # Add persistent child poverty if available
        if (!is.na(child_poverty_col)) {
          year_data$persistent_child_poverty_county <- typology_data[[child_poverty_col]]
          year_data$persistent_child_poverty_county_data_quality <- data_quality_flags$direct
          year_data$persistent_child_poverty_county_data_source <- "USDA ERS County Typology"
          year_data$persistent_child_poverty_county_data_vintage <- "2015" # Base year
        }
        
        ers_data_by_year[[as.character(year)]] <- year_data
      }
      
      # Combine all years
      if (length(ers_data_by_year) > 0) {
        combined_ers_data <- bind_rows(ers_data_by_year)
      }
    }
    
    # Add employment data if available
    if (!is.null(emp_data) && !is.null(combined_ers_data)) {
      # Find employment volatility column
      volatility_col <- grep("volatility|Volatility|stability|Stability", names(emp_data), value = TRUE)[1]
      
      if (!is.na(volatility_col)) {
        # Create a temporary data frame with just volatility
        emp_volatility <- emp_data %>%
          select(GEOID, year, volatility = all_of(volatility_col))
        
        # Join with combined data
        combined_ers_data <- left_join(
          combined_ers_data,
          emp_volatility,
          by = c("GEOID", "year")
        )
        
        # Rename and add metadata
        combined_ers_data <- combined_ers_data %>%
          rename(employment_volatility_index = volatility) %>%
          mutate(
            employment_volatility_index_data_quality = data_quality_flags$direct,
            employment_volatility_index_data_source = "USDA ERS",
            employment_volatility_index_data_vintage = as.character(year)
          )
      }
    } else if (!is.null(emp_data) && is.null(combined_ers_data)) {
      # If we only have employment data, start with that
      volatility_col <- grep("volatility|Volatility|stability|Stability", names(emp_data), value = TRUE)[1]
      
      if (!is.na(volatility_col)) {
        combined_ers_data <- emp_data %>%
          rename(employment_volatility_index = all_of(volatility_col)) %>%
          mutate(
            employment_volatility_index_data_quality = data_quality_flags$direct,
            employment_volatility_index_data_source = "USDA ERS",
            employment_volatility_index_data_vintage = as.character(year)
          )
      }
    }
    
    return(combined_ers_data)
  }
  
  # Function to get Bureau of Labor Statistics data
  get_bls_data <- function() {
    # BLS LAUS (Local Area Unemployment Statistics) for job growth and unemployment
    
    # Define the variables we want from BLS
    bls_variables <- c(
      "job_growth_rate" = "Annual job growth rate"
    )
    
    # Check for local files first
    local_files <- find_local_economic_files()
    bls_files <- local_files$bls
    
    # BLS LAUS data list to store results
    bls_data_list <- list()
    
    if (length(bls_files) > 0) {
      print_msg(paste("Found", length(bls_files), "local BLS data files"))
      
      # Process each local file
      for (file in bls_files) {
        print_msg(paste("Processing local BLS file:", basename(file)))
        
        # Try to extract year from filename
        year_match <- regexpr("(19|20)[0-9]{2}", basename(file))
        file_year <- NULL
        if (year_match > 0) {
          file_year <- as.integer(substr(basename(file), year_match, year_match + 3))
          print_msg(paste("Extracted year:", file_year))
        }
        
        # Skip if we can't determine year or it's not in requested years
        if (is.null(file_year) || !(file_year %in% years)) {
          next
        }
        
        # Try to read file
        tryCatch({
          # Determine file type
          if (grepl("\\.csv$", file, ignore.case = TRUE)) {
            file_data <- read_csv(file, show_col_types = FALSE, guess_max = 10000)
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
            print_msg("Could not identify GEOID column in BLS data file, skipping.")
            next
          }
          
          # Rename and format GEOID
          file_data <- file_data %>%
            rename(GEOID = all_of(geoid_col)) %>%
            mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
          
          # Find job growth rate column
          growth_col <- grep("growth|Growth|change|Change", names(file_data), value = TRUE)[1]
          
          # Create data frame for this year
          year_data <- data.frame(
            GEOID = file_data$GEOID,
            year = file_year
          )
          
          # Add job growth rate if available
          if (!is.na(growth_col)) {
            year_data$job_growth_rate <- file_data[[growth_col]]
            year_data$job_growth_rate_data_quality <- data_quality_flags$direct
            year_data$job_growth_rate_data_source <- "Bureau of Labor Statistics"
            year_data$job_growth_rate_data_vintage <- as.character(file_year)
          }
          
          # Add to list
          bls_data_list[[as.character(file_year)]] <- year_data
          
        }, error = function(e) {
          print_msg(paste("Error processing file", basename(file), ":", 
                        conditionMessage(e)))
        })
      }
    } else {
      print_msg("No local BLS files found, attempting to download")
      
      # Process each year
      for (year in years) {
        # Skip future years
        if (year > as.integer(format(Sys.Date(), "%Y"))) {
          next
        }
        
        # Define file paths
        bls_file <- file.path(data_dir, paste0("bls_laus_", year, ".csv"))
        
        # Check if we need to download
        need_download <- !file.exists(bls_file) || refresh_cache
        
        if (need_download) {
          # BLS LAUS URL
          # Note: BLS data requires API key for bulk downloads, this is simplified
          # Real implementation would use BLS API with proper key
          bls_url <- paste0(
            "https://download.bls.gov/pub/time.series/la/la.data.", year, ".csv"
          )
          
          # Try to download
          if (!safe_download(bls_url, bls_file, paste("BLS LAUS data for", year))) {
            print_msg(paste("Could not download BLS LAUS data for", year))
            next
          }
        } else {
          print_msg(paste("Using existing BLS LAUS file for", year))
        }
        
        # Process the data if file exists
        if (file.exists(bls_file)) {
          print_msg(paste("Reading BLS LAUS data for", year))
          
          # Read the file
          tryCatch({
            # BLS files can be large, use optimizations
            bls_data <- read_csv(
              bls_file, 
              show_col_types = FALSE,
              guess_max = 10000
            )
            
            # Get column names
            print_msg(paste("BLS data has", ncol(bls_data), "columns and", nrow(bls_data), "rows"))
            
            # BLS LAUS data is complex and needs special processing
            # This is a simplified example of how it might work
            # A full implementation would require careful parsing of BLS series codes
            
            # Check if this has county-level data
            # BLS uses series IDs which include geographic codes
            series_col <- grep("series|Series|SERIES", names(bls_data), value = TRUE)[1]
            
            if (is.na(series_col)) {
              print_msg("Could not identify series column in BLS data")
              next
            }
            
            # Filter to county-level series (usually starts with LAU)
            county_series <- bls_data %>%
              filter(grepl("^LAU", .data[[series_col]]))
            
            if (nrow(county_series) == 0) {
              print_msg("No county-level series found in BLS data")
              next
            }
            
            # Extract FIPS codes from series IDs
            # This is highly dependent on BLS series ID structure
            # A real implementation would need specific logic
            
            # For this example, create a simplified dataset
            year_data <- data.frame(
              GEOID = rep(NA, nrow(county_series)),
              year = year
            )
            
            # Add job growth rate if available
            growth_col <- grep("growth|Growth|change|Change", names(county_series), value = TRUE)[1]
            
            if (!is.na(growth_col)) {
              year_data$job_growth_rate <- county_series[[growth_col]]
              year_data$job_growth_rate_data_quality <- data_quality_flags$direct
              year_data$job_growth_rate_data_source <- "Bureau of Labor Statistics"
              year_data$job_growth_rate_data_vintage <- as.character(year)
            }
            
            # Add to list
            bls_data_list[[as.character(year)]] <- year_data
            
            print_msg(paste("Processed BLS LAUS data for", year))
          }, error = function(e) {
            print_msg(paste("Error reading BLS LAUS data for", year, ":", conditionMessage(e)))
          })
        }
      }
    }
    
    # Combine all years
    if (length(bls_data_list) > 0) {
      combined_bls <- bind_rows(bls_data_list)
      print_msg(paste("Combined BLS data with", nrow(combined_bls), "rows"))
      return(combined_bls)
    } else {
      print_msg("No BLS data processed successfully")
      return(NULL)
    }
  }
  
  # Function to get Opportunity Insights data
  get_opportunity_data <- function() {
    # Opportunity Insights data on economic mobility
    
    # Define the variables we want
    opportunity_variables <- c(
      "income_mobility_index" = "Measure of intergenerational economic mobility",
      "absolute_upward_mobility" = "Expected income rank for children from low-income families",
      "mean_commute_distance" = "Average commute distance",
      "job_density_index" = "Number of jobs within typical commute distance"
    )
    
    # Check for local files first
    local_files <- find_local_economic_files()
    opportunity_files <- local_files$opportunity
    
    if (length(opportunity_files) > 0) {
      print_msg(paste("Found", length(opportunity_files), "local Opportunity Insights data files"))
      opportunity_file <- opportunity_files[1]
      print_msg(paste("Using local Opportunity Insights file:", basename(opportunity_file)))
    } else {
      # No local files found, try to download
      print_msg("No local Opportunity Insights files found, attempting to download")
      
      # Opportunity Atlas data file
      opportunity_file <- file.path(data_dir, "opportunity_atlas.csv")
      
      # Check if we need to download
      need_download <- !file.exists(opportunity_file) || refresh_cache
      
      if (need_download) {
        # Opportunity Insights URL
        opportunity_url <- "https://opportunityinsights.org/wp-content/uploads/2018/10/county_outcomes.csv"
        
        # Try to download
        if (!safe_download(opportunity_url, opportunity_file, "Opportunity Insights data")) {
          print_msg("Could not download Opportunity Insights data")
          return(NULL)
        }
      } else {
        print_msg("Using existing Opportunity Insights file")
      }
    }
    
    # Process the data if file exists
    opportunity_data <- NULL
    if (file.exists(opportunity_file)) {
      print_msg("Reading Opportunity Insights data")
      
      # Read the file
      tryCatch({
        # Determine file type
        if (grepl("\\.csv$", opportunity_file, ignore.case = TRUE)) {
          opportunity_data <- read_csv(opportunity_file, show_col_types = FALSE)
        } else if (grepl("\\.xlsx$|\\.xls$", opportunity_file, ignore.case = TRUE)) {
          opportunity_data <- read_excel(opportunity_file)
        } else {
          print_msg(paste("Unsupported file format:", opportunity_file))
          return(NULL)
        }
        
        # Get column names
        print_msg(paste("Opportunity data has", ncol(opportunity_data), "columns and", nrow(opportunity_data), "rows"))
        
        # Check for county identifier
        county_col <- grep("county|County|COUNTY", names(opportunity_data), value = TRUE)[1]
        state_col <- grep("state|State|STATE", names(opportunity_data), value = TRUE)[1]
        fips_col <- grep("FIPS|fips|geoid|GEOID", names(opportunity_data), value = TRUE)[1]
        
        if (is.na(fips_col) && (!is.na(county_col) && !is.na(state_col))) {
          print_msg("Opportunity data uses county/state names, not FIPS - may need mapping")
          # Ideally we would map county/state names to GEOID
          # But for this example, we'll skip this complex task
          return(NULL)
        } else if (is.na(fips_col)) {
          print_msg("Could not identify FIPS or county/state columns in Opportunity data")
          return(NULL)
        }
        
        # Rename and format GEOID
        opportunity_data <- opportunity_data %>%
          rename(GEOID = all_of(fips_col)) %>%
          mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
        
        # Find columns for our variables of interest
        
        # Economic mobility
        mobility_col <- grep("mobility|Mobility|econ.*mobility", names(opportunity_data), value = TRUE)[1]
        
        # Absolute upward mobility
        upward_col <- grep("upward|Upward|absolute.*mobility", names(opportunity_data), value = TRUE)[1]
        
        # Commute distance
        commute_col <- grep("commute.*dist|commute.*time|dist.*commute", names(opportunity_data), value = TRUE)[1]
        
        # Job density
        job_density_col <- grep("job.*density|density.*job|employment.*density", names(opportunity_data), value = TRUE)[1]
        
        print_msg(paste("Found columns: Mobility:", !is.na(mobility_col),
                      "Upward mobility:", !is.na(upward_col),
                      "Commute distance:", !is.na(commute_col),
                      "Job density:", !is.na(job_density_col)))
        
        # Opportunity Insights data applies to specific years
        # We'll assign it to all requested years
        opportunity_data_by_year <- list()
        
        for (year in years) {
          # Only include years in the Opportunity Insights range (2000-2018)
          if (year < 2000 || year > 2018) {
            next
          }
          
          # Create data frame for this year
          year_data <- data.frame(
            GEOID = opportunity_data$GEOID,
            year = year
          )
          
          # Add income mobility if available
          if (!is.na(mobility_col)) {
            year_data$income_mobility_index <- opportunity_data[[mobility_col]]
            year_data$income_mobility_index_data_quality <- data_quality_flags$direct
            year_data$income_mobility_index_data_source <- "Opportunity Insights"
            year_data$income_mobility_index_data_vintage <- "2018" # Base year for dataset
          }
          
          # Add upward mobility if available
          if (!is.na(upward_col)) {
            year_data$absolute_upward_mobility <- opportunity_data[[upward_col]]
            year_data$absolute_upward_mobility_data_quality <- data_quality_flags$direct
            year_data$absolute_upward_mobility_data_source <- "Opportunity Insights"
            year_data$absolute_upward_mobility_data_vintage <- "2018" # Base year for dataset
          }
          
          # Add commute distance if available
          if (!is.na(commute_col)) {
            year_data$mean_commute_distance <- opportunity_data[[commute_col]]
            year_data$mean_commute_distance_data_quality <- data_quality_flags$direct
            year_data$mean_commute_distance_data_source <- "Opportunity Insights"
            year_data$mean_commute_distance_data_vintage <- "2018" # Base year for dataset
          }
          
          # Add job density if available
          if (!is.na(job_density_col)) {
            year_data$job_density_index <- opportunity_data[[job_density_col]]
            year_data$job_density_index_data_quality <- data_quality_flags$direct
            year_data$job_density_index_data_source <- "Opportunity Insights"
            year_data$job_density_index_data_vintage <- "2018" # Base year for dataset
          }
          
          opportunity_data_by_year[[as.character(year)]] <- year_data
        }
        
        # Combine all years
        if (length(opportunity_data_by_year) > 0) {
          opportunity_data <- bind_rows(opportunity_data_by_year)
          print_msg(paste("Processed Opportunity Insights data with", nrow(opportunity_data), "rows"))
        } else {
          print_msg("No years in range for Opportunity Insights data")
          opportunity_data <- NULL
        }
      }, error = function(e) {
        print_msg(paste("Error reading Opportunity Insights data:", conditionMessage(e)))
        opportunity_data <- NULL
      })
    }
    
    return(opportunity_data)
  }
  
  # Function to get income inequality data from ACS (American Community Survey)
  get_acs_inequality_data <- function() {
    # ACS has data on income inequality ratios
    
    # Define the variables we want from ACS
    acs_variables <- c(
      "income_inequality_ratio" = "Ratio of income at 80th percentile to income at 20th percentile"
    )
    
    # Check for local files first
    local_files <- find_local_economic_files()
    acs_files <- local_files$acs
    
    # ACS data list to store results
    acs_data_list <- list()
    
    if (length(acs_files) > 0) {
      print_msg(paste("Found", length(acs_files), "local ACS inequality data files"))
      
      # Process each local file
      for (file in acs_files) {
        print_msg(paste("Processing local ACS file:", basename(file)))
        
        # Try to extract year from filename
        year_match <- regexpr("(19|20)[0-9]{2}", basename(file))
        file_year <- NULL
        if (year_match > 0) {
          file_year <- as.integer(substr(basename(file), year_match, year_match + 3))
          print_msg(paste("Extracted year:", file_year))
        }
        
        # Skip if we can't determine year or it's not in requested years
        if (is.null(file_year) || !(file_year %in% years)) {
          next
        }
        
        # Skip years before ACS started (2005+)
        if (file_year < 2005) {
          print_msg(paste("Skipping year", file_year, "- ACS data starts from 2005"))
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
            print_msg("Could not identify GEOID column in ACS inequality file, skipping.")
            next
          }
          
          # Rename and format GEOID
          file_data <- file_data %>%
            rename(GEOID = all_of(geoid_col)) %>%
            mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
          
          # Find inequality ratio column
          inequality_col <- grep("inequality|Inequality|gini|Gini|ratio|Ratio", 
                                names(file_data), value = TRUE)[1]
          
          # Create data frame for this year
          year_data <- data.frame(
            GEOID = file_data$GEOID,
            year = file_year
          )
          
          # Add inequality ratio if available
          if (!is.na(inequality_col)) {
            year_data$income_inequality_ratio <- file_data[[inequality_col]]
            year_data$income_inequality_ratio_data_quality <- data_quality_flags$direct
            year_data$income_inequality_ratio_data_source <- "American Community Survey"
            year_data$income_inequality_ratio_data_vintage <- as.character(file_year)
          }
          
          # Add to list
          acs_data_list[[as.character(file_year)]] <- year_data
          
        }, error = function(e) {
          print_msg(paste("Error processing file", basename(file), ":", 
                        conditionMessage(e)))
        })
      }
    } else {
      print_msg("No local ACS inequality files found, checking file paths")
      
      # Process each year
      for (year in years) {
        # Skip years before ACS started (2005+) and future years
        if (year < 2005 || year > as.integer(format(Sys.Date(), "%Y"))) {
          next
        }
        
        # Define file paths
        acs_file <- file.path(data_dir, paste0("acs_inequality_", year, ".csv"))
        
        # Check if we have the file
        if (file.exists(acs_file)) {
          print_msg(paste("Found ACS inequality file for", year))
          
          # Read the file
          tryCatch({
            acs_data <- read_csv(acs_file, show_col_types = FALSE)
            
            # Get column names
            print_msg(paste("ACS data has", ncol(acs_data), "columns and", nrow(acs_data), "rows"))
            
            # Check for GEOID column
            geoid_col <- grep("GEOID|geoid|fips|FIPS", names(acs_data), value = TRUE)[1]
            
            if (is.na(geoid_col)) {
              print_msg("Could not identify GEOID column in ACS data")
              next
            }
            
            # Rename and format GEOID
            acs_data <- acs_data %>%
              rename(GEOID = all_of(geoid_col)) %>%
              mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
            
            # Look for inequality ratio column
            inequality_col <- grep("inequality|Inequality|gini|Gini|ratio|Ratio", names(acs_data), value = TRUE)[1]
            
            if (is.na(inequality_col)) {
              print_msg("Could not identify inequality column in ACS data")
              next
            }
            
            # Create data for this year
            year_data <- data.frame(
              GEOID = acs_data$GEOID,
              year = year,
              income_inequality_ratio = acs_data[[inequality_col]],
              income_inequality_ratio_data_quality = data_quality_flags$direct,
              income_inequality_ratio_data_source = "American Community Survey",
              income_inequality_ratio_data_vintage = as.character(year)
            )
            
            # Add to list
            acs_data_list[[as.character(year)]] <- year_data
            
            print_msg(paste("Processed ACS inequality data for", year))
          }, error = function(e) {
            print_msg(paste("Error reading ACS inequality data for", year, ":", conditionMessage(e)))
          })
        } else {
          print_msg(paste("No ACS inequality data file found for", year))
          # Note: Census API would be used in a real implementation
          # This would require a Census API key and proper queries
        }
      }
    }
    
    # Combine all years
    if (length(acs_data_list) > 0) {
      combined_acs <- bind_rows(acs_data_list)
      print_msg(paste("Combined ACS inequality data with", nrow(combined_acs), "rows"))
      return(combined_acs)
    } else {
      print_msg("No ACS inequality data processed successfully")
      return(NULL)
    }
  }
  
  # Get data from different sources - use parallel processing if enabled
  if (parallel && requireNamespace("future.apply", quietly = TRUE)) {
    print_msg("Using parallel processing to fetch data from multiple sources")
    
    # Define the data sources to fetch
    data_sources <- c("ers", "bls", "opportunity", "acs_inequality")
    
    # Create a function to process one data source
    process_data_source <- function(source) {
      print_msg(paste("Processing data source:", source))
      
      if (source == "ers") {
        return(get_ers_data())
      } else if (source == "bls") {
        return(get_bls_data())
      } else if (source == "opportunity") {
        return(get_opportunity_data())
      } else if (source == "acs_inequality") {
        return(get_acs_inequality_data())
      } else {
        return(NULL)
      }
    }
    
    # Set up progress reporting if available
    if (requireNamespace("progressr", quietly = TRUE)) {
      # Create a progress handler
      progressr::handlers(progressr::handler_progress())
      
      # Process with progress tracking
      results <- progressr::with_progress({
        p <- progressr::progressor(steps = length(data_sources))
        
        future.apply::future_lapply(data_sources, function(source) {
          result <- process_data_source(source)
          p(message = paste("Processed data source:", source))
          return(list(source = source, data = result))
        })
      })
    } else {
      # Process without progress tracking
      results <- future.apply::future_lapply(data_sources, function(source) {
        result <- process_data_source(source)
        return(list(source = source, data = result))
      })
    }
    
    # Extract results into their respective variables
    ers_data <- NULL
    bls_data <- NULL
    opportunity_data <- NULL
    acs_inequality_data <- NULL
    
    for (result in results) {
      if (result$source == "ers") {
        ers_data <- result$data
      } else if (result$source == "bls") {
        bls_data <- result$data
      } else if (result$source == "opportunity") {
        opportunity_data <- result$data
      } else if (result$source == "acs_inequality") {
        acs_inequality_data <- result$data
      }
    }
  } else {
    # Sequential processing
    print_msg("Using sequential processing to fetch data from multiple sources")
    ers_data <- get_ers_data()
    bls_data <- get_bls_data()
    opportunity_data <- get_opportunity_data()
    acs_inequality_data <- get_acs_inequality_data()
  }
  
  # Combine all data sources
  economic_data_list <- list()
  
  if (!is.null(ers_data) && nrow(ers_data) > 0) {
    economic_data_list[["ers"]] <- ers_data
  }
  
  if (!is.null(bls_data) && nrow(bls_data) > 0) {
    economic_data_list[["bls"]] <- bls_data
  }
  
  if (!is.null(opportunity_data) && nrow(opportunity_data) > 0) {
    economic_data_list[["opportunity"]] <- opportunity_data
  }
  
  if (!is.null(acs_inequality_data) && nrow(acs_inequality_data) > 0) {
    economic_data_list[["acs"]] <- acs_inequality_data
  }
  
  # Process if we have data
  if (length(economic_data_list) > 0) {
    # Combine all sources and handle overlapping columns
    # Start with first dataset
    combined_economic_data <- economic_data_list[[1]]
    
    # Add each additional dataset
    if (length(economic_data_list) > 1) {
      for (i in 2:length(economic_data_list)) {
        next_data <- economic_data_list[[i]]
        
        # Identify common columns for joining
        join_cols <- intersect(names(combined_economic_data), names(next_data))
        
        # Ensure we have at least GEOID and year for joining
        if (all(c("GEOID", "year") %in% join_cols)) {
          # Find columns that might overlap (excluding join columns and flags)
          overlap_cols <- setdiff(
            intersect(names(combined_economic_data), names(next_data)),
            c(join_cols, grep("_data_quality$|_data_source$|_data_vintage$", 
                             names(next_data), value = TRUE))
          )
          
          if (length(overlap_cols) > 0) {
            # For overlapping columns, prefer data from the current dataset
            # Remove those columns from the combined data before joining
            combined_economic_data <- combined_economic_data %>%
              select(-all_of(overlap_cols))
          }
          
          # Join with the next dataset
          combined_economic_data <- full_join(
            combined_economic_data, 
            next_data, 
            by = join_cols
          )
        } else {
          print_msg("Warning: Cannot join datasets - missing common keys")
        }
      }
    }
    
    # Check if we need to handle missing years
    available_years <- unique(combined_economic_data$year)
    missing_years <- setdiff(years, available_years)
    
    if (length(missing_years) > 0 && allow_interpolation) {
      print_msg(paste("Interpolating data for missing years:", paste(missing_years, collapse=", ")))
      
      # Get all variables except join columns and metadata columns
      measure_cols <- setdiff(
        names(combined_economic_data),
        c("GEOID", "year", grep("_data_quality$|_data_source$|_data_vintage$", 
                               names(combined_economic_data), value = TRUE))
      )
      
      # Process each county separately for interpolation
      counties <- unique(combined_economic_data$GEOID)
      
      # List to store interpolated data
      interp_data_list <- list()
      
      for (county in counties) {
        # Get data for this county
        county_data <- combined_economic_data %>%
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
          # Skip categorical variables which shouldn't be interpolated
          if (col == "economic_typology" || 
              col == "persistent_poverty_county" || 
              col == "persistent_child_poverty_county") {
            # Instead, find the value from the nearest year
            if (col %in% names(county_data)) {
              # For each year in our grid
              for (i in 1:nrow(county_grid)) {
                grid_year <- county_grid$year[i]
                
                # Try to find this year directly
                if (grid_year %in% county_data$year) {
                  year_idx <- which(county_data$year == grid_year)
                  county_grid[[col]][i] <- county_data[[col]][year_idx]
                  
                  # Also copy the metadata
                  quality_col <- paste0(col, "_data_quality")
                  source_col <- paste0(col, "_data_source")
                  vintage_col <- paste0(col, "_data_vintage")
                  
                  if (quality_col %in% names(county_data)) {
                    county_grid[[quality_col]][i] <- county_data[[quality_col]][year_idx]
                  }
                  
                  if (source_col %in% names(county_data)) {
                    county_grid[[source_col]][i] <- county_data[[source_col]][year_idx]
                  }
                  
                  if (vintage_col %in% names(county_data)) {
                    county_grid[[vintage_col]][i] <- county_data[[vintage_col]][year_idx]
                  }
                } else {
                  # Find nearest year
                  nearest_year <- county_data$year[which.min(abs(county_data$year - grid_year))]
                  nearest_idx <- which(county_data$year == nearest_year)
                  
                  # Copy the value
                  county_grid[[col]][i] <- county_data[[col]][nearest_idx]
                  
                  # Set appropriate quality flags
                  quality_col <- paste0(col, "_data_quality")
                  source_col <- paste0(col, "_data_source")
                  vintage_col <- paste0(col, "_data_vintage")
                  
                  if (quality_col %in% names(county_data)) {
                    county_grid[[quality_col]][i] <- 
                      if (grid_year > max(county_data$year) || grid_year < min(county_data$year)) {
                        data_quality_flags$extrapolated # Extrapolation
                      } else {
                        data_quality_flags$interpolated # Interpolation
                      }
                  }
                  
                  if (source_col %in% names(county_data)) {
                    county_grid[[source_col]][i] <- county_data[[source_col]][nearest_idx]
                  }
                  
                  if (vintage_col %in% names(county_data)) {
                    county_grid[[vintage_col]][i] <- paste0("derived_from_", nearest_year)
                  }
                }
              }
            }
            next
          }
          
          # For numeric columns, use linear interpolation
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
        
        # Add to list
        interp_data_list[[county]] <- county_grid
      }
      
      # Combine all counties
      if (length(interp_data_list) > 0) {
        interp_data <- bind_rows(interp_data_list)
        
        # Add any missing metadata columns with NA values
        meta_cols <- grep("_data_quality$|_data_source$|_data_vintage$", 
                         names(combined_economic_data), value = TRUE)
        
        for (col in meta_cols) {
          if (!(col %in% names(interp_data))) {
            interp_data[[col]] <- NA
          }
        }
        
        # Use the interpolated data instead
        combined_economic_data <- interp_data
      }
    } else if (length(missing_years) > 0) {
      print_msg("Missing years but interpolation not allowed. Years will remain missing.")
    }
    
    # Filter to just the requested years
    combined_economic_data <- combined_economic_data %>%
      filter(year %in% years)
    
    # Sort data
    combined_economic_data <- combined_economic_data %>%
      arrange(GEOID, year)
    
    # Cache the processed data
    saveRDS(combined_economic_data, cache_file)
    print_msg(paste("Cached economic data to:", cache_file))
    
    return(combined_economic_data)
  } else {
    # No data available - create empty dataset with proper structure
    print_msg("No economic data found. Creating empty dataset with proper structure.")
    
    # Economic variables to include
    economic_vars <- c(
      "employment_volatility_index",
      "job_growth_rate",
      "income_inequality_ratio",
      "economic_typology",
      "persistent_poverty_county",
      "persistent_child_poverty_county",
      "economic_distress_index",
      "income_mobility_index",
      "absolute_upward_mobility",
      "mean_commute_distance",
      "job_density_index"
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
    for (var in economic_vars) {
      grid[[var]] <- NA_real_
      grid[[paste0(var, "_data_quality")]] <- data_quality_flags$missing
      grid[[paste0(var, "_data_source")]] <- "NO_DATA_AVAILABLE"
      grid[[paste0(var, "_data_vintage")]] <- NA_character_
    }
    
    economic_data <- as_tibble(grid)
    print_msg(paste("Created empty economic dataset with", nrow(economic_data), "rows"))
    
    # Cache the empty data
    saveRDS(economic_data, cache_file)
    print_msg(paste("Cached empty economic data structure to:", cache_file))
    
    # Provide clear error message about missing data
    print_msg("ERROR: No economic data files found. Please download economic data.")
    print_msg("Required files should be placed in: data/economic/")
    print_msg("File formats needed:")
    print_msg("1. USDA ERS data: CSV files with county typology codes and employment data")
    print_msg("2. BLS data: CSV files with labor statistics and job growth rates")
    print_msg("3. Opportunity Insights data: CSV files with economic mobility metrics")
    print_msg("4. ACS inequality data: CSV files with income inequality metrics")
    print_msg("Files should include FIPS/GEOID column and relevant economic metrics.")
    
    return(economic_data)
  }
}

# This lets the function be used when the script is sourced
is_sourced <- function() {
  return(!identical(environment(), globalenv()))
}

# If the script is run directly, test the function
if (!is_sourced()) {
  cat("Testing economic data fetcher...\n")
  
  # Get current year
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  
  # Test for last 5 years
  test_years <- (current_year-4):current_year
  
  # Test the function
  result <- fetch_economic_data(
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
    )
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