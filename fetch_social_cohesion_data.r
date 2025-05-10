#!/usr/bin/env Rscript

# Social Cohesion Data Fetcher
# This script handles retrieval of social cohesion data from MIT Election Lab, 
# County Health Rankings, and other sources

library(tidyverse)
library(httr)
library(jsonlite)
library(readxl)
library(lubridate)
library(sf)
library(zoo) # For interpolation if needed

#' Find local social cohesion data files
#'
#' Searches multiple directories for social cohesion data files, including
#' election data from MIT Election Lab and County Health Rankings data.
#'
#' @return A list of file paths organized by data type
find_local_social_cohesion_files <- function() {
  # List of directories to check
  social_dirs <- c(
    "data/social_cohesion",
    "data/cache/social_cohesion",
    "data/social",
    "data/social_capital",
    "data/elections",
    "data/chr"
  )
  
  # Also check subdirectories for specific data types
  for (base_dir in c("data", "data/cache")) {
    for (subdir in c("elections", "election_data", "chr", "county_health_rankings", "social")) {
      social_dirs <- c(social_dirs, file.path(base_dir, subdir))
    }
  }
  
  # List of possible file extensions
  file_exts <- c("\\.csv$", "\\.xlsx$", "\\.xls$", "\\.zip$", "\\.txt$", "\\.rds$")
  
  # Search for files
  all_files <- c()
  for (dir in social_dirs) {
    if (dir.exists(dir)) {
      for (ext in file_exts) {
        files <- list.files(dir, pattern = ext, full.names = TRUE, recursive = TRUE)
        all_files <- c(all_files, files)
      }
    }
  }
  
  # Filter for different types of social cohesion data
  social_files <- list(
    elections = grep("election|vote|ballot|president|congress|turnout|MIT|harvard", 
                    all_files, value = TRUE, ignore.case = TRUE),
    chr = grep("chr|county.*health.*rank|health.*rank|social.*association|social.*connect", 
              all_files, value = TRUE, ignore.case = TRUE)
  )
  
  # Sort by modification time (newest first)
  for (type in names(social_files)) {
    if (length(social_files[[type]]) > 0) {
      file_info <- file.info(social_files[[type]])
      social_files[[type]] <- social_files[[type]][order(file_info$mtime, decreasing = TRUE)]
    }
  }
  
  return(social_files)
}

#' Fetch social cohesion data
#'
#' Retrieves social cohesion data from MIT Election Data and Science Lab,
#' County Health Rankings, and other sources for social capital metrics at the county level.
#'
#' @param years Vector of years to include
#' @param cache_dir Directory to store cache files
#' @param refresh_cache Whether to refresh the cache
#' @param allow_interpolation Whether to interpolate missing values
#' @param data_quality_flags List of standardized data quality flags
#' @param offline_mode Whether to skip all downloads and use only cached data
#' @param parallel Whether to use parallel processing
#' @param parallel_config Optional parallel processing configuration
#' @return A data frame with social cohesion data for all requested years
fetch_social_cohesion_data <- function(years, 
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
      print_msg("Parallel processing enabled for social cohesion data")
    } else {
      # Basic parallel setup
      print_msg("Using basic parallel processing setup for social cohesion data")
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
  cache_file <- file.path(cache_dir, "social_cohesion_data.rds")
  
  # Use cache if available and not refreshing
  if (!refresh_cache && file.exists(cache_file)) {
    print_msg("Loading cached social cohesion data...")
    social_data <- readRDS(cache_file)
    
    # Check if all requested years are in the cache
    cached_years <- unique(social_data$year)
    missing_years <- setdiff(years, cached_years)
    
    # Check for empty cache with just placeholder data
    if (nrow(social_data) <= 1 || 
        (is.data.frame(social_data) && 
         any(sapply(names(social_data), function(col) {
           if (grepl("_data_source$", col)) {
             return(any(grepl("SIMULATED|NO_DATA_AVAILABLE", social_data[[col]])))
           }
           return(FALSE)
         })))) {
      print_msg("Cached social cohesion data appears to be empty or a placeholder. Will process files again.")
      # Force refresh by continuing past this point
    } else if (length(missing_years) == 0) {
      print_msg("Using complete cached social cohesion data.")
      return(social_data %>% filter(year %in% years))
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
  data_dir <- "data/social_cohesion"
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
    print_msg(paste("Created social cohesion data directory at:", data_dir))
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
  
  # Function to get MIT Election Data
  get_election_data <- function() {
    # MIT Election Lab has county-level election data
    # Data is available for election years
    
    # Find local election data files
    local_files <- find_local_social_cohesion_files()
    election_files <- local_files$elections
    
    # Define variables we want to extract
    election_variables <- c(
      "voter_turnout_rate" = "Voter turnout rate in general elections",
      "voter_registration_rate" = "Voter registration as percentage of eligible population",
      "political_competition_index" = "Index measuring political competition"
    )
    
    # Filter to election years (presidential and midterm)
    election_years <- years[years %in% c(
      2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022
    )]
    
    # MIT Election data list to store results
    election_data_list <- list()
    
    # Process each election year
    for (year in election_years) {
      # Skip future years
      if (year > as.integer(format(Sys.Date(), "%Y"))) {
        next
      }
      
      # Define file paths
      # Separate files for presidential and non-presidential elections
      is_presidential <- year %% 4 == 0
      election_type <- if (is_presidential) "president" else "house"
      
      # Check for existing files that match this year
      year_pattern <- paste0("_", year, "\\.")
      existing_files <- grep(year_pattern, election_files, value = TRUE)
      
      # Also check for files that might contain this year in their name
      more_matches <- grep(paste0(election_type, ".*", year), election_files, value = TRUE)
      existing_files <- unique(c(existing_files, more_matches))
      
      # Default file path if we need to download
      election_file <- file.path(
        data_dir, 
        paste0("election_", election_type, "_", year, ".csv")
      )
      
      # Check if we need to download
      need_download <- length(existing_files) == 0 || refresh_cache
      
      # Use existing file if available
      if (length(existing_files) > 0 && !need_download) {
        election_file <- existing_files[1]  # Use the first (newest) file
        print_msg(paste("Using existing", election_type, "election file for", year, ":", basename(election_file)))
      } else if (need_download && !offline_mode) {
        # MIT Election Lab URL
        # Different structure for presidential vs. congressional elections
        if (is_presidential) {
          election_url <- paste0(
            "https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/VOQCHQ/",
            year, 
            "_countypres.csv"
          )
        } else {
          election_url <- paste0(
            "https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/IG0UN2/",
            year, 
            "_county.csv"
          )
        }
        
        # Make sure the directory exists
        if (!dir.exists(dirname(election_file))) {
          dir.create(dirname(election_file), recursive = TRUE)
        }
        
        # Try to download
        success <- safe_download(election_url, election_file, paste(election_type, "election data for", year))
        
        if (!success) {
          print_msg(paste("Could not download", election_type, "election data for", year))
          
          # Try to find any election files for this year or prior years
          for (y in year:max(2000, year-6)) {  # Try up to 6 years back
            year_pattern <- paste0("_", y, "\\.|", y, "_")
            type_pattern <- paste0(if(y %% 4 == 0) "president" else "house")
            potential_files <- grep(paste0("(", year_pattern, ")|(", type_pattern, ")"), 
                                   election_files, value = TRUE)
            
            if (length(potential_files) > 0) {
              election_file <- potential_files[1]  # Use the first (newest) file
              print_msg(paste("No data available for", year, "- using election file from", y, ":", 
                             basename(election_file)))
              break
            }
          }
          
          # If still no files found, skip this year
          if (!file.exists(election_file)) {
            print_msg(paste("No election data found for year", year, "or recent prior years"))
            next
          }
        }
      } else if (offline_mode && need_download) {
        print_msg(paste("Offline mode: Cannot download", election_type, "election data for", year))
        
        # Try to find any election files for this or prior years
        found_file <- FALSE
        for (y in year:max(2000, year-6)) {  # Try up to 6 years back
          year_pattern <- paste0("_", y, "\\.|", y, "_")
          type_pattern <- paste0(if(y %% 4 == 0) "president" else "house")
          potential_files <- grep(paste0("(", year_pattern, ")|(", type_pattern, ")"), 
                                 election_files, value = TRUE)
          
          if (length(potential_files) > 0) {
            election_file <- potential_files[1]  # Use the first (newest) file
            print_msg(paste("Using available election file from", y, ":", basename(election_file)))
            found_file <- TRUE
            break
          }
        }
        
        if (!found_file) {
          print_msg(paste("No election data files found for year", year, "or recent prior years"))
          next
        }
      }
      
      # Process the data if file exists
      if (file.exists(election_file)) {
        print_msg(paste("Reading", election_type, "election data for", year, "from", basename(election_file)))
        
        # Read the file
        tryCatch({
          # Determine file type and read accordingly
          file_ext <- tolower(tools::file_ext(election_file))
          
          if (file_ext == "csv") {
            election_data <- read_csv(election_file, show_col_types = FALSE)
          } else if (file_ext %in% c("xlsx", "xls")) {
            election_data <- read_excel(election_file)
          } else if (file_ext == "txt") {
            # Try to determine delimiter
            election_data <- read_delim(election_file, delim = "\t", show_col_types = FALSE)
          } else {
            print_msg(paste("Unsupported file format for", basename(election_file)))
            next
          }
          
          # Get column names
          print_msg(paste("Election data has", ncol(election_data), "columns and", nrow(election_data), "rows"))
          
          # Check for FIPS/GEOID column
          geoid_col <- grep("FIPS|fips|geoid|GEOID|county.*code|COUNTY.*CODE", 
                           names(election_data), value = TRUE)[1]
          
          if (is.na(geoid_col)) {
            print_msg("Could not identify GEOID column in election data")
            next
          }
          
          # Rename and format GEOID
          election_data <- election_data %>%
            rename(GEOID = all_of(geoid_col)) %>%
            mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
          
          # Find columns for our variables of interest
          
          # Voter turnout
          turnout_col <- grep("turnout|Turnout|TURNOUT", names(election_data), value = TRUE)[1]
          
          # Voter registration
          registration_col <- grep("regist|Regist|REGIST", names(election_data), value = TRUE)[1]
          
          # Political competition
          competition_col <- grep("competition|Competition|COMPETITION|margin|Margin", 
                                names(election_data), value = TRUE)[1]
          
          print_msg(paste("Found columns: Turnout:", !is.na(turnout_col),
                        "Registration:", !is.na(registration_col),
                        "Competition:", !is.na(competition_col)))
          
          # Create data frame for this year
          year_data <- data.frame(
            GEOID = election_data$GEOID,
            year = year
          )
          
          # Add voter turnout if available
          if (!is.na(turnout_col)) {
            year_data$voter_turnout_rate <- election_data[[turnout_col]]
            year_data$voter_turnout_rate_data_quality <- data_quality_flags$direct
            year_data$voter_turnout_rate_data_source <- "MIT Election Data and Science Lab"
            year_data$voter_turnout_rate_data_vintage <- as.character(year)
          }
          
          # Add voter registration if available
          if (!is.na(registration_col)) {
            year_data$voter_registration_rate <- election_data[[registration_col]]
            year_data$voter_registration_rate_data_quality <- data_quality_flags$direct
            year_data$voter_registration_rate_data_source <- "MIT Election Data and Science Lab"
            year_data$voter_registration_rate_data_vintage <- as.character(year)
          }
          
          # Add political competition if available
          if (!is.na(competition_col)) {
            year_data$political_competition_index <- election_data[[competition_col]]
            year_data$political_competition_index_data_quality <- data_quality_flags$direct
            year_data$political_competition_index_data_source <- "MIT Election Data and Science Lab"
            year_data$political_competition_index_data_vintage <- as.character(year)
          }
          
          # Add to list
          election_data_list[[as.character(year)]] <- year_data
          
          print_msg(paste("Processed", election_type, "election data for", year))
        }, error = function(e) {
          print_msg(paste("Error reading", election_type, "election data for", year, ":", conditionMessage(e)))
        })
      }
    }
    
    # Combine all years
    if (length(election_data_list) > 0) {
      combined_election <- bind_rows(election_data_list)
      print_msg(paste("Combined election data with", nrow(combined_election), "rows"))
      return(combined_election)
    } else {
      print_msg("No election data processed successfully")
      return(NULL)
    }
  }
  
  # Function to get County Health Rankings data
  get_chr_data <- function() {
    # County Health Rankings has social association data from 2014 onwards
    
    # Find local CHR data files
    local_files <- find_local_social_cohesion_files()
    chr_files <- local_files$chr
    
    # Define variables we want to extract
    chr_variables <- c(
      "social_association_rate" = "Social associations per 10,000 population",
      "religious_congregation_rate" = "Religious congregations per 10,000 population",
      "nonprofit_organizations_per_10k" = "Nonprofit organizations per 10,000 population"
    )
    
    # County Health Rankings data list to store results
    chr_data_list <- list()
    
    # Limit to years 2014 and later
    chr_years <- years[years >= 2014]
    
    for (year in chr_years) {
      # Skip future years
      if (year > as.integer(format(Sys.Date(), "%Y"))) {
        next
      }
      
      # Check for existing files that match this year
      year_pattern <- paste0("chr_", year, "|", year, ".*[Cc]ounty.*[Hh]ealth")
      existing_files <- grep(year_pattern, chr_files, value = TRUE)
      
      # Default file path if we need to download
      chr_file <- file.path(data_dir, paste0("chr_", year, ".csv"))
      
      # Check if we need to download
      need_download <- length(existing_files) == 0 || refresh_cache
      
      # Use existing file if available
      if (length(existing_files) > 0 && !need_download) {
        chr_file <- existing_files[1]  # Use the first (newest) file
        print_msg(paste("Using existing County Health Rankings file for", year, ":", basename(chr_file)))
      } else if (need_download && !offline_mode) {
        # County Health Rankings URL
        chr_url <- paste0(
          "https://www.countyhealthrankings.org/sites/default/files/media/document/",
          year, 
          "%20County%20Health%20Rankings%20Data%20-%20v1.csv"
        )
        
        # Make sure the directory exists
        if (!dir.exists(dirname(chr_file))) {
          dir.create(dirname(chr_file), recursive = TRUE)
        }
        
        # Try to download
        success <- safe_download(chr_url, chr_file, paste("County Health Rankings data for", year))
        
        if (!success) {
          print_msg(paste("Could not download County Health Rankings data for", year))
          
          # Try to find any CHR files for this year or prior years
          for (y in year:max(2014, year-3)) {  # Try up to 3 years back, no earlier than 2014
            year_pattern <- paste0("chr_", y, "|", y, ".*[Cc]ounty.*[Hh]ealth")
            potential_files <- grep(year_pattern, chr_files, value = TRUE)
            
            if (length(potential_files) > 0) {
              chr_file <- potential_files[1]  # Use the first (newest) file
              print_msg(paste("No data available for", year, "- using CHR file from", y, ":", 
                              basename(chr_file)))
              break
            }
          }
          
          # If still no files found, skip this year
          if (!file.exists(chr_file)) {
            print_msg(paste("No County Health Rankings data found for year", year, "or recent prior years"))
            next
          }
        }
      } else if (offline_mode && need_download) {
        print_msg(paste("Offline mode: Cannot download County Health Rankings data for", year))
        
        # Try to find any CHR files for this or prior years
        found_file <- FALSE
        for (y in year:max(2014, year-3)) {  # Try up to 3 years back
          year_pattern <- paste0("chr_", y, "|", y, ".*[Cc]ounty.*[Hh]ealth")
          potential_files <- grep(year_pattern, chr_files, value = TRUE)
          
          if (length(potential_files) > 0) {
            chr_file <- potential_files[1]  # Use the first (newest) file
            print_msg(paste("Using available CHR file from", y, ":", basename(chr_file)))
            found_file <- TRUE
            break
          }
        }
        
        if (!found_file) {
          print_msg(paste("No County Health Rankings files found for year", year, "or recent prior years"))
          next
        }
      }
      
      # Process the data if file exists
      if (file.exists(chr_file)) {
        print_msg(paste("Reading County Health Rankings data for", year, "from", basename(chr_file)))
        
        # Read the file
        tryCatch({
          # Determine file type and read accordingly
          file_ext <- tolower(tools::file_ext(chr_file))
          
          if (file_ext == "csv") {
            chr_data <- read_csv(chr_file, show_col_types = FALSE)
          } else if (file_ext %in% c("xlsx", "xls")) {
            chr_data <- read_excel(chr_file)
          } else if (file_ext == "txt") {
            # Try to determine delimiter
            chr_data <- read_delim(chr_file, delim = "\t", show_col_types = FALSE)
          } else {
            print_msg(paste("Unsupported file format for", basename(chr_file)))
            next
          }
          
          # Get column names
          print_msg(paste("CHR data has", ncol(chr_data), "columns and", nrow(chr_data), "rows"))
          
          # Check for FIPS/GEOID column
          fips_col <- grep("FIPS|fips|county.*code|COUNTY.*CODE", names(chr_data), value = TRUE)[1]
          
          if (is.na(fips_col)) {
            print_msg("Could not identify FIPS column in CHR data")
            
            # Try to construct FIPS from state and county codes
            state_col <- grep("state.*fips|state.*code", names(chr_data), value = TRUE)[1]
            county_col <- grep("county.*fips|county.*code", names(chr_data), value = TRUE)[1]
            
            if (!is.na(state_col) && !is.na(county_col)) {
              chr_data <- chr_data %>%
                mutate(GEOID = sprintf("%02d%03d", 
                                     as.numeric(.data[[state_col]]), 
                                     as.numeric(.data[[county_col]])))
              print_msg("Created GEOID from state and county codes")
            } else {
              print_msg("Could not create GEOID from state and county codes")
              next
            }
          } else {
            # Rename and format GEOID
            chr_data <- chr_data %>%
              rename(GEOID = all_of(fips_col)) %>%
              mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
          }
          
          # Find columns for our variables of interest
          
          # Social association rate
          association_col <- grep("social.*association|association.*rate", 
                                names(chr_data), value = TRUE)[1]
          
          # Religious congregation rate
          religious_col <- grep("religious|congregation|church", 
                              names(chr_data), value = TRUE)[1]
          
          # Nonprofit organizations
          nonprofit_col <- grep("nonprofit|non.profit", 
                              names(chr_data), value = TRUE)[1]
          
          print_msg(paste("Found columns: Social associations:", !is.na(association_col),
                        "Religious congregations:", !is.na(religious_col),
                        "Nonprofits:", !is.na(nonprofit_col)))
          
          # Create data frame for this year
          year_data <- data.frame(
            GEOID = chr_data$GEOID,
            year = year
          )
          
          # Add social association rate if available
          if (!is.na(association_col)) {
            year_data$social_association_rate <- chr_data[[association_col]]
            year_data$social_association_rate_data_quality <- data_quality_flags$direct
            year_data$social_association_rate_data_source <- "County Health Rankings"
            year_data$social_association_rate_data_vintage <- as.character(year)
          }
          
          # Add religious congregation rate if available
          if (!is.na(religious_col)) {
            year_data$religious_congregation_rate <- chr_data[[religious_col]]
            year_data$religious_congregation_rate_data_quality <- data_quality_flags$direct
            year_data$religious_congregation_rate_data_source <- "County Health Rankings"
            year_data$religious_congregation_rate_data_vintage <- as.character(year)
          }
          
          # Add nonprofit organizations if available
          if (!is.na(nonprofit_col)) {
            year_data$nonprofit_organizations_per_10k <- chr_data[[nonprofit_col]]
            year_data$nonprofit_organizations_per_10k_data_quality <- data_quality_flags$direct
            year_data$nonprofit_organizations_per_10k_data_source <- "County Health Rankings"
            year_data$nonprofit_organizations_per_10k_data_vintage <- as.character(year)
          }
          
          # Add to list
          chr_data_list[[as.character(year)]] <- year_data
          
          print_msg(paste("Processed County Health Rankings data for", year))
        }, error = function(e) {
          print_msg(paste("Error reading County Health Rankings data for", year, ":", conditionMessage(e)))
        })
      }
    }
    
    # Combine all years
    if (length(chr_data_list) > 0) {
      combined_chr <- bind_rows(chr_data_list)
      print_msg(paste("Combined County Health Rankings data with", nrow(combined_chr), "rows"))
      return(combined_chr)
    } else {
      print_msg("No County Health Rankings data processed successfully")
      return(NULL)
    }
  }
  
  # Get data from different sources - use parallel processing if enabled
  if (parallel && requireNamespace("future.apply", quietly = TRUE)) {
    print_msg("Using parallel processing to fetch data from multiple social cohesion sources")
    
    # Define the data sources to fetch
    data_sources <- c("election", "chr")
    
    # Create a function to process one data source
    process_data_source <- function(source) {
      print_msg(paste("Processing social cohesion data source:", source))
      
      if (source == "election") {
        return(get_election_data())
      } else if (source == "chr") {
        return(get_chr_data())
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
      social_data_sources <- progressr::with_progress({
        p <- progressr::progressor(steps = length(data_sources))
        
        future.apply::future_lapply(data_sources, function(source) {
          result <- process_data_source(source)
          p(message = paste("Processed social cohesion data source:", source))
          return(result)
        })
      })
    } else {
      # Process without progress tracking
      social_data_sources <- future.apply::future_lapply(data_sources, process_data_source)
    }
    
    # Convert results to named list
    names(social_data_sources) <- data_sources
    
    # Filter out NULL results
    social_data_list <- social_data_sources[!sapply(social_data_sources, is.null)]
    social_data_list <- social_data_list[sapply(social_data_list, function(x) !is.null(x) && nrow(x) > 0)]
    
  } else {
    # Sequential processing
    print_msg("Using sequential processing to fetch data from multiple social cohesion sources")
    election_data <- get_election_data()
    chr_data <- get_chr_data()
    
    # Combine all data sources
    social_data_list <- list()
    
    if (!is.null(election_data) && nrow(election_data) > 0) {
      social_data_list[["election"]] <- election_data
    }
    
    if (!is.null(chr_data) && nrow(chr_data) > 0) {
      social_data_list[["chr"]] <- chr_data
    }
  }
  
  # Process if we have data
  if (length(social_data_list) > 0) {
    # Combine all sources and handle overlapping columns
    # Start with first dataset
    combined_social_data <- social_data_list[[1]]
    
    # Add each additional dataset
    if (length(social_data_list) > 1) {
      for (i in 2:length(social_data_list)) {
        next_data <- social_data_list[[i]]
        
        # Identify common columns for joining
        join_cols <- intersect(names(combined_social_data), names(next_data))
        
        # Ensure we have at least GEOID and year for joining
        if (all(c("GEOID", "year") %in% join_cols)) {
          # Find columns that might overlap (excluding join columns and flags)
          overlap_cols <- setdiff(
            intersect(names(combined_social_data), names(next_data)),
            c(join_cols, grep("_data_quality$|_data_source$|_data_vintage$", 
                             names(next_data), value = TRUE))
          )
          
          if (length(overlap_cols) > 0) {
            # For overlapping columns, prefer data from the current dataset
            # Remove those columns from the combined data before joining
            combined_social_data <- combined_social_data %>%
              select(-all_of(overlap_cols))
          }
          
          # Join with the next dataset
          combined_social_data <- full_join(
            combined_social_data, 
            next_data, 
            by = join_cols
          )
        } else {
          print_msg("Warning: Cannot join datasets - missing common keys")
        }
      }
    }
    
    # Fill in missing years for election data
    # Election data only exists for even-numbered years
    
    # Get available years and all variables
    available_years <- unique(combined_social_data$year)
    
    # For electoral data, interpolate missing odd-numbered years if allowed
    if (allow_interpolation && any(available_years %% 2 == 0)) {  # If we have even-numbered years (election years)
      missing_odd_years <- sort(years[years %% 2 == 1])  # Odd-numbered years
      missing_odd_years <- missing_odd_years[missing_odd_years > min(available_years) & 
                                             missing_odd_years < max(available_years)]
      
      if (length(missing_odd_years) > 0) {
        print_msg(paste("Interpolating electoral data for non-election years:", 
                      paste(missing_odd_years, collapse=", ")))
        
        # Interpolate for each missing year
        odd_year_data_list <- list()
        
        for (odd_year in missing_odd_years) {
          # Find surrounding even years
          prev_year <- odd_year - 1
          next_year <- odd_year + 1
          
          # Get data for those years
          prev_year_data <- combined_social_data %>% filter(year == prev_year)
          next_year_data <- combined_social_data %>% filter(year == next_year)
          
          # Join them to get all counties
          all_counties <- full_join(
            prev_year_data %>% select(GEOID),
            next_year_data %>% select(GEOID),
            by = "GEOID"
          )
          
          # Create data frame for this odd year
          odd_year_data <- data.frame(
            GEOID = all_counties$GEOID,
            year = odd_year
          )
          
          # Get all electoral variables
          electoral_vars <- c(
            "voter_turnout_rate",
            "voter_registration_rate",
            "political_competition_index"
          )
          
          # Add interpolated values for each variable
          for (var in electoral_vars) {
            if (var %in% names(prev_year_data) && var %in% names(next_year_data)) {
              # Create matched data for interpolation
              interp_data <- full_join(
                prev_year_data %>% select(GEOID, !!sym(var)),
                next_year_data %>% select(GEOID, !!sym(var)),
                by = "GEOID", 
                suffix = c("_prev", "_next")
              )
              
              # Linear interpolation (midpoint)
              var_prev <- paste0(var, "_prev")
              var_next <- paste0(var, "_next")
              
              # Calculate interpolated value
              interp_data <- interp_data %>%
                mutate(!!sym(var) := (!!sym(var_prev) + !!sym(var_next)) / 2)
              
              # Add to odd year data
              odd_year_data <- left_join(
                odd_year_data,
                interp_data %>% select(GEOID, !!sym(var)),
                by = "GEOID"
              )
              
              # Add quality flags
              odd_year_data[[paste0(var, "_data_quality")]] <- data_quality_flags$interpolated
              odd_year_data[[paste0(var, "_data_source")]] <- "MIT Election Data and Science Lab"
              odd_year_data[[paste0(var, "_data_vintage")]] <- 
                paste0("interpolated_", prev_year, "_", next_year)
            }
          }
          
          # Add to list
          odd_year_data_list[[as.character(odd_year)]] <- odd_year_data
        }
        
        # Combine with original data
        if (length(odd_year_data_list) > 0) {
          odd_years_data <- bind_rows(odd_year_data_list)
          combined_social_data <- bind_rows(combined_social_data, odd_years_data)
        }
      }
    }
    
    # Check if we need to handle remaining missing years
    available_years <- unique(combined_social_data$year)
    missing_years <- setdiff(years, available_years)
    
    if (length(missing_years) > 0 && allow_interpolation) {
      print_msg(paste("Interpolating data for missing years:", paste(missing_years, collapse=", ")))
      
      # Get all variables except join columns and metadata columns
      measure_cols <- setdiff(
        names(combined_social_data),
        c("GEOID", "year", grep("_data_quality$|_data_source$|_data_vintage$", 
                               names(combined_social_data), value = TRUE))
      )
      
      # Process each county separately for interpolation
      counties <- unique(combined_social_data$GEOID)
      
      # Define function to interpolate a single county
      interpolate_county <- function(county) {
        # Get data for this county
        county_data <- combined_social_data %>%
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
                         names(combined_social_data), value = TRUE)
        
        for (col in meta_cols) {
          if (!(col %in% names(interp_data))) {
            interp_data[[col]] <- NA
          }
        }
        
        # Use the interpolated data instead
        combined_social_data <- interp_data
      }
    } else if (length(missing_years) > 0) {
      print_msg("Missing years but interpolation not allowed. Years will remain missing.")
    }
    
    # Filter to just the requested years
    combined_social_data <- combined_social_data %>%
      filter(year %in% years)
    
    # Sort data
    combined_social_data <- combined_social_data %>%
      arrange(GEOID, year)
    
    # Cache the processed data
    saveRDS(combined_social_data, cache_file)
    print_msg(paste("Cached social cohesion data to:", cache_file))
    
    return(combined_social_data)
  } else {
    # No data available - create empty dataset with NAs and proper error messages
    print_msg("No social cohesion data available and simulation not allowed. Creating empty dataset with NAs.")
    
    # Social cohesion variables to include
    social_vars <- c(
      "voter_turnout_rate",
      "voter_registration_rate",
      "political_competition_index",
      "social_association_rate",
      "religious_congregation_rate",
      "nonprofit_organizations_per_10k"
    )
    
    # Get county list using get_county_list() or fallback to sample counties
    # Try to get a comprehensive list if possible
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
      # Sample counties
      counties <- data.frame(
        GEOID = c("01001", "01003", "01005", "01007", "01009"), # Sample counties
        NAME = c("Autauga County, Alabama", "Baldwin County, Alabama", 
                "Barbour County, Alabama", "Bibb County, Alabama", 
                "Blount County, Alabama")
      )
      print_msg("Using sample county list for empty dataset")
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
    
    # Add empty variable columns with NAs
    for (var in social_vars) {
      grid[[var]] <- NA_real_
      grid[[paste0(var, "_data_quality")]] <- data_quality_flags$missing
      grid[[paste0(var, "_data_source")]] <- "NOT_AVAILABLE"
      grid[[paste0(var, "_data_vintage")]] <- NA_character_
    }
    
    social_data <- as_tibble(grid)
    print_msg(paste("Created empty social cohesion dataset with", nrow(social_data), "rows"))
    
    # Cache the empty data
    saveRDS(social_data, cache_file)
    print_msg(paste("Cached empty social cohesion data to:", cache_file))
    
    return(social_data)
  }
}

# This lets the function be used when the script is sourced
is_sourced <- function() {
  return(!identical(environment(), globalenv()))
}

# If the script is run directly, test the function
if (!is_sourced()) {
  cat("Testing social cohesion data fetcher...\n")
  
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
    cat("Using parallel processing for social cohesion data fetching test\n")
  } else {
    cat("Parallel processing dependencies not available, using sequential processing\n")
  }
  
  # Test the function
  result <- fetch_social_cohesion_data(
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
