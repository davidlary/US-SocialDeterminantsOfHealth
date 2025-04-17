library(tidyverse)
library(tidycensus)
library(ipumsr)
library(tigris)
library(sf)
library(duckdb)
library(httr)
library(readxl)
library(parallel)
library(foreach)
library(doParallel)
library(future)
library(future.apply)
library(progressr)
library(purrr)

#' Fetch comprehensive county-level data from multiple sources with caching
#'
#' This function extends the original fetch_county_data function to include
#' data from IPUMS NHGIS and CDC PLACES in addition to Census Bureau data.
#' It also implements caching to avoid refetching data that has already been retrieved.
#'
#' @param crosswalk The extended variable crosswalk
#' @param years Vector of years to fetch data for (default: 1979-2023)
#' @param include_places Logical; whether to include CDC PLACES data
#' @param include_nhgis Logical; whether to include IPUMS NHGIS data
#' @param include_life_expectancy Logical; whether to include IHME Life Expectancy data
#' @param use_cache Logical; whether to use cached data when available (default: TRUE)
#' @param refresh_cache Logical; whether to refresh the cache even if it exists (default: FALSE)
#' @return A list containing data frames from each source
fetch_extended_data <- function(crosswalk, 
                               years = 1979:2023,
                               include_places = TRUE,
                               include_nhgis = TRUE,
                               include_life_expectancy = TRUE,
                               use_cache = TRUE,
                               refresh_cache = FALSE,
                               parallel = TRUE,          # Enable parallel processing
                               num_cores = NULL,         # Number of cores to use (NULL = auto-detect)
                               parallel_strategy = "multisession", # Strategy: "multisession", "multicore", or "sequential"
                               cache_options = list(
                                 refresh_census = NULL,   # Set to TRUE to force refresh only Census data
                                 refresh_places = NULL,   # Set to TRUE to force refresh only PLACES data
                                 refresh_nhgis = NULL,    # Set to TRUE to force refresh only NHGIS data
                                 refresh_life_expectancy = NULL, # Set to TRUE to force refresh only life expectancy data
                                 max_cache_age_days = 30, # Max age of cache files before they're considered stale
                                 cache_dir = "data/cache" # Directory for cache storage
                               )) {
  
  # Initialize results list
  all_data <- list()
  
  # Setup function to conditionally print based on verbosity
  quiet_cat <- function(message, important = FALSE) {
    # If log_message exists (from main script), use it
    if (exists("log_message")) {
      log_message(message, show_console = important)
    } else {
      # Check if running interactively or being sourced
      is_interactive_run <- !exists("is_sourced") || !is_sourced
      
      # Otherwise fall back to message for cleaner output
      if (is_interactive_run) {
        message(trimws(message))
      } else {
        # Still log to console when sourced, but only once
        cat(message)
      }
    }
  }
  
  # Initialize parallel processing
  if (parallel) {
    # Determine number of cores to use
    if (is.null(num_cores)) {
      num_cores <- max(1, parallel::detectCores() - 1)  # Leave one core free for the OS
    }
    
    quiet_cat(paste("Setting up parallel processing with", num_cores, "cores using", parallel_strategy, "strategy\n"), important = TRUE)
    
    # Set up the parallel backend based on the strategy
    if (parallel_strategy == "multisession") {
      future::plan(future::multisession, workers = num_cores)
    } else if (parallel_strategy == "multicore") {
      future::plan(future::multicore, workers = num_cores)
    } else {
      # Default to sequential if not recognized
      future::plan(future::sequential)
      quiet_cat("Falling back to sequential processing (non-parallel)\n", important = TRUE)
    }
    
    # Register the parallel backend for foreach
    doParallel::registerDoParallel(cores = num_cores)
    
    # Define console_output if it doesn't exist
    if (!exists("console_output")) {
      console_output <- TRUE  # Default to showing output
    }
    
    # Initialize progress reporting but don't set handlers yet
    # We'll use with_progress() later when needed
  } else {
    quiet_cat("Parallel processing disabled. Using sequential processing.\n")
    future::plan(future::sequential)
  }
  
  # Process cache options
  if (is.null(cache_options$cache_dir)) {
    cache_options$cache_dir <- "data/cache"
  }
  
  # Extract and set cache directory
  cache_dir <- cache_options$cache_dir
  
  # Extract specific refresh options (default to global refresh_cache if not set)
  refresh_census <- if(is.null(cache_options$refresh_census)) refresh_cache else cache_options$refresh_census
  refresh_places <- if(is.null(cache_options$refresh_places)) refresh_cache else cache_options$refresh_places
  refresh_nhgis <- if(is.null(cache_options$refresh_nhgis)) refresh_cache else cache_options$refresh_nhgis
  
  # Set max cache age
  max_cache_age_days <- if(is.null(cache_options$max_cache_age_days)) 30 else cache_options$max_cache_age_days
  
  # Ensure cache directory exists
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    
    # Only show this in the log file, not on console if verbosity is reduced
    if (exists("log_message")) {
      log_message(paste("Created cache directory at:", cache_dir, "\n"))
    } else {
      cat("Created cache directory at:", cache_dir, "\n")
    }
  }
  
  # Function to check cache directory size
  check_cache_size <- function(dir = cache_dir, warn_size_mb = 500) {
    # Get all files in the cache directory recursively
    files <- list.files(dir, full.names = TRUE, recursive = TRUE)
    
    # Calculate total size
    total_size_bytes <- sum(file.info(files)$size, na.rm = TRUE)
    total_size_mb <- total_size_bytes / (1024 * 1024)
    
    # Format with 2 decimal places
    total_size_mb_formatted <- format(round(total_size_mb, 2), nsmall = 2)
    
    quiet_cat(paste("Cache directory size:", total_size_mb_formatted, "MB\n"))
    
    # Warn if cache is getting large - important, so show even with reduced verbosity
    if (total_size_mb > warn_size_mb) {
      quiet_cat(paste("WARNING: Cache directory is larger than", warn_size_mb, "MB.\n"), important = TRUE)
      quiet_cat("Consider using the clean_cache() function to remove old cache files.\n", important = TRUE)
    }
    
    return(total_size_bytes)
  }
  
  # Function to get cache info for diagnostics
  get_cache_info <- function(dir = cache_dir) {
    # Get all cache files
    files <- list.files(dir, full.names = TRUE, recursive = TRUE)
    
    # Create info dataframe
    info <- data.frame(
      file = basename(files),
      path = files,
      size_mb = file.info(files)$size / (1024 * 1024),
      modified = file.info(files)$mtime,
      stringsAsFactors = FALSE
    )
    
    # Sort by modification time (newest first)
    info <- info[order(info$modified, decreasing = TRUE), ]
    
    return(info)
  }
  
  # Function to clean old cache files
  clean_cache <- function(dir = cache_dir, older_than_days = 30, dry_run = TRUE) {
    # Get cache info
    info <- get_cache_info(dir)
    
    # Determine cutoff date
    cutoff <- Sys.time() - (older_than_days * 24 * 60 * 60)
    
    # Find files older than cutoff
    old_files <- info[info$modified < cutoff, ]
    
    if (nrow(old_files) == 0) {
      cat("No cache files older than", older_than_days, "days found.\n")
      return(invisible(NULL))
    }
    
    # Report files that would be removed
    cat("Found", nrow(old_files), "cache files older than", older_than_days, "days:\n")
    for (i in 1:nrow(old_files)) {
      cat("  -", old_files$file[i], "(", format(round(old_files$size_mb[i], 2), nsmall = 2), "MB,", 
          format(old_files$modified[i], "%Y-%m-%d %H:%M:%S"), ")\n")
    }
    
    total_size <- sum(old_files$size_mb)
    cat("Total space that would be freed:", format(round(total_size, 2), nsmall = 2), "MB\n")
    
    # Remove files if not a dry run
    if (!dry_run) {
      cat("Removing files...\n")
      for (file in old_files$path) {
        if (file.exists(file)) {
          result <- tryCatch({
            file.remove(file)
            TRUE
          }, error = function(e) {
            cat("Error removing file", basename(file), ":", conditionMessage(e), "\n")
            FALSE
          })
          
          if (result) {
            cat("  Removed:", basename(file), "\n")
          }
        }
      }
      cat("Cache cleanup complete.\n")
    } else {
      cat("Dry run - no files were actually removed.\n")
      cat("Run clean_cache(older_than_days = ", older_than_days, ", dry_run = FALSE) to remove these files.\n", sep = "")
    }
  }
  
  # Register cache utility functions in global environment for user access
  assign("check_census_cache_size", check_cache_size, envir = .GlobalEnv)
  assign("get_census_cache_info", get_cache_info, envir = .GlobalEnv)
  assign("clean_census_cache", clean_cache, envir = .GlobalEnv)
  
  # Check cache size and print info
  check_cache_size()
  
  # Generate a cache key based on input parameters
  # This will help identify when the cache needs to be refreshed due to parameter changes
  cache_key <- digest::digest(list(years, include_places, include_nhgis))
  
  # Add timestamp and version info to cache metadata
  cache_metadata <- list(
    key = cache_key,
    created = Sys.time(),
    r_version = R.version.string,
    params = list(
      years = years,
      include_places = include_places,
      include_nhgis = include_nhgis
    )
  )
  
  cache_info_file <- file.path(cache_dir, "cache_info.rds")
  
  # Save or check cache info
  if (file.exists(cache_info_file) && !refresh_cache) {
    old_cache_metadata <- tryCatch({
      readRDS(cache_info_file)
    }, error = function(e) {
      cat("Cache info file exists but is corrupted. Will refresh cache.\n")
      NULL
    })
    
    if (is.null(old_cache_metadata) || 
        !is.list(old_cache_metadata) || 
        !("key" %in% names(old_cache_metadata)) ||
        old_cache_metadata$key != cache_key) {
      quiet_cat("Cache parameters have changed. Will refresh cache.\n")
      refresh_cache <- TRUE
    } else {
      # Print cache age
      if ("created" %in% names(old_cache_metadata)) {
        cache_age <- difftime(Sys.time(), old_cache_metadata$created, units = "hours")
        quiet_cat(paste("Using cache created", format(round(cache_age, 1), nsmall = 1), "hours ago.\n"))
      }
    }
  }
  
  # Save current cache metadata
  saveRDS(cache_metadata, cache_info_file)
  
  # First, fetch Census data using the original function as a base
  source("fetch_county_data_final.r")
  
  # Define cache files for each data source
  census_cache_file <- file.path(cache_dir, "census_data.rds")
  places_cache_file <- file.path(cache_dir, "places_data.rds")
  nhgis_cache_file <- file.path(cache_dir, "nhgis_data.rds")
  
  # Function to check if cache file is stale based on age
  is_cache_stale <- function(cache_file, max_age_days = max_cache_age_days) {
    if (!file.exists(cache_file)) return(TRUE)
    
    # Get file modification time
    file_time <- file.info(cache_file)$mtime
    age_days <- as.numeric(difftime(Sys.time(), file_time, units = "days"))
    
    if (age_days > max_age_days) {
      cat("Cache file", basename(cache_file), "is", round(age_days, 1), "days old (older than", 
          max_age_days, "days) - treating as stale.\n")
      return(TRUE)
    }
    
    return(FALSE)
  }
  
  # Check for cached Census data
  need_refresh_census <- refresh_census || is_cache_stale(census_cache_file)
  
  if (use_cache && file.exists(census_cache_file) && !need_refresh_census) {
    # Loading from cache - only important enough to show if actually using cache
    quiet_cat("Loading cached Census Bureau data...\n", important = TRUE)
    tryCatch({
      census_data <- readRDS(census_cache_file)
      
      # Check file modification time for informational purposes
      file_time <- file.info(census_cache_file)$mtime
      age_days <- as.numeric(difftime(Sys.time(), file_time, units = "days"))
      
      # Successfully loaded - important enough to show
      quiet_cat(paste("Successfully loaded Census data from cache (", round(age_days, 1), " days old).\n", sep=""), important = TRUE)
    }, error = function(e) {
      # Error messages are important - always show
      quiet_cat(paste("Error loading cached Census data:", conditionMessage(e), "\n"), important = TRUE)
      quiet_cat("Will fetch fresh Census data.\n", important = TRUE)
      quiet_cat("Fetching Census Bureau data (ACS, Decennial, PEP)...\n", important = TRUE)
      # Pass parallel processing parameters to fetch_county_data
      census_data <<- fetch_county_data(crosswalk, parallel = parallel, num_cores = num_cores)
      # Save to cache
      saveRDS(census_data, census_cache_file)
      quiet_cat("Saved fresh Census data to cache.\n")
    })
  } else {
    if (need_refresh_census) {
      if (refresh_census) {
        quiet_cat("Refresh requested specifically for Census data.\n")  
      } else if (is_cache_stale(census_cache_file)) {
        quiet_cat("Census data cache is stale.\n")
      } else if (refresh_cache) {
        quiet_cat("Global cache refresh requested.\n")
      }
    }
    
    # Fresh data fetch - important milestone in the process
    quiet_cat("Fetching Census Bureau data (ACS, Decennial, PEP)...\n", important = TRUE)
    # Pass parallel processing parameters to fetch_county_data
    census_data <- fetch_county_data(crosswalk, parallel = parallel, num_cores = num_cores)
    # Save to cache
    saveRDS(census_data, census_cache_file)
    quiet_cat("Saved Census data to cache.\n")
  }
  
  all_data$census <- census_data
  
  # Function to download a file with progress bar
  download_with_progress <- function(url, destfile) {
    if (!file.exists(destfile)) {
      quiet_cat(paste("Downloading", basename(destfile), "...\n"), important = TRUE)
      
      # Create directory if it doesn't exist
      dir.create(dirname(destfile), showWarnings = FALSE, recursive = TRUE)
      
      # Download with progress bar and better error handling
      tryCatch({
        response <- GET(url, 
                       write_disk(destfile, overwrite = TRUE), 
                       timeout(300), # 5 minute timeout
                       progress())
        
        if (http_status(response)$category != "Success") {
          warning("Failed to download ", url, ": ", http_status(response)$reason)
          return(FALSE)
        }
        
        # Verify file was downloaded successfully
        if (file.exists(destfile) && file.info(destfile)$size > 0) {
          return(TRUE)
        } else {
          warning("Download may be incomplete: ", destfile, " has zero size")
          return(FALSE)
        }
      }, error = function(e) {
        warning("Error downloading file: ", e$message)
        return(FALSE)
      })
    } else {
      quiet_cat(paste("File", basename(destfile), "already exists. Using cached version.\n"))
      return(TRUE)
    }
  }
  
  # Helper function to fetch and process CDC PLACES data - defined before it's used
  fetch_and_process_places <- function() {
    # CDC PLACES data URL (2022 release or most recent)
    places_url <- "https://data.cdc.gov/api/views/cwsq-ngmh/rows.csv?accessType=DOWNLOAD"
    places_file <- "data/cdc_places/PLACES_County_Data_2022.csv"
    
    # Download if not already present
    if (download_with_progress(places_url, places_file)) {
      places_data <- process_cdc_places_data(places_file)
      # Save to cache
      saveRDS(places_data, places_cache_file)
      quiet_cat("Saved CDC PLACES data to cache.\n")
      all_data$places <- places_data
    } else {
      warning("Failed to download CDC PLACES data.")
      all_data$places <- tibble()
    }
  }
  
  # IMPROVED CDC PLACES DATA PROCESSING WITH CACHING
  if (include_places) {
    quiet_cat("\nProcessing CDC PLACES data...\n", important = TRUE)
    
    # Check for cached CDC PLACES data
    need_refresh_places <- refresh_places || is_cache_stale(places_cache_file)
    
    if (use_cache && file.exists(places_cache_file) && !need_refresh_places) {
      quiet_cat("Loading cached CDC PLACES data...\n", important = TRUE)
      tryCatch({
        places_data <- readRDS(places_cache_file)
        
        # Check file modification time for informational purposes
        file_time <- file.info(places_cache_file)$mtime
        age_days <- as.numeric(difftime(Sys.time(), file_time, units = "days"))
        
        quiet_cat(paste("Successfully loaded CDC PLACES data from cache (", round(age_days, 1), " days old).\n", sep=""), important = TRUE)
        all_data$places <- places_data
      }, error = function(e) {
        quiet_cat(paste("Error loading cached CDC PLACES data:", conditionMessage(e), "\n"), important = TRUE)
        quiet_cat("Will fetch fresh CDC PLACES data.\n", important = TRUE)
        fetch_and_process_places()
      })
    } else {
      if (need_refresh_places) {
        if (refresh_places) {
          quiet_cat("Refresh requested specifically for CDC PLACES data.\n")  
        } else if (is_cache_stale(places_cache_file)) {
          quiet_cat("CDC PLACES data cache is stale.\n")
        } else if (refresh_cache) {
          quiet_cat("Global cache refresh requested.\n")
        }
      }
      
      quiet_cat("Fetching fresh CDC PLACES data...\n", important = TRUE)
      fetch_and_process_places()
    }
  } else {
    all_data$places <- tibble()
  }
  
  # Note: Helper function fetch_and_process_places has been moved before its first use
  
  # Helper function to process NHGIS files - defined before its first use
  process_nhgis_files <- function() {
    # NHGIS data requires user download from https://nhgis.org/
    # We'll check if files exist and process them if available
    
    nhgis_files <- list.files("data/nhgis", pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
    
    if (length(nhgis_files) > 0) {
      quiet_cat(paste("Found", length(nhgis_files), "NHGIS data files.\n"))
      
      # Create cache of file modification times to detect changes
      file_mtimes <- sapply(nhgis_files, file.mtime)
      mtimes_cache_file <- file.path(cache_dir, "nhgis_mtimes.rds")
      need_refresh <- TRUE
      
      if (use_cache && file.exists(mtimes_cache_file) && !refresh_cache) {
        old_mtimes <- tryCatch({
          readRDS(mtimes_cache_file)
        }, error = function(e) {
          NULL
        })
        
        if (!is.null(old_mtimes) && 
            length(old_mtimes) == length(file_mtimes) && 
            all(names(old_mtimes) %in% names(file_mtimes)) &&
            all(old_mtimes == file_mtimes[names(old_mtimes)])) {
          # No changes in files
          need_refresh <- FALSE
        }
      }
      
      # Save current mtimes
      saveRDS(file_mtimes, mtimes_cache_file)
      
      if (use_cache && !need_refresh && file.exists(nhgis_cache_file) && !refresh_cache) {
        quiet_cat("NHGIS files haven't changed since last run. Using cache.\n")
        nhgis_data <- readRDS(nhgis_cache_file)
      } else {
        # Process each NHGIS file - using parallel processing if enabled
        if (parallel) {
          quiet_cat("Using parallel processing for NHGIS files\n", important = TRUE)
          
          # Use with_progress to safely handle progress reporting
          nhgis_data_list <- progressr::with_progress({
            # Set up progress reporting
            p <- progressr::progressor(along = nhgis_files)
            
            # Process files in parallel using future.apply
            future.apply::future_lapply(nhgis_files, function(file) {
              # Update progress
              p(sprintf("NHGIS: %s", basename(file)))
            
            tryCatch({
              quiet_cat(paste("Processing NHGIS file:", basename(file), "\n"))
              
              # Read NHGIS data
              data <- read_csv(file, show_col_types = FALSE)
              
              # Check if this is a time series file
              if (any(str_detect(names(data), "^YEAR"))) {
                # Time series data - has years as part of variable names
                # Extract years from column names
                year_cols <- names(data)[str_detect(names(data), "^[A-Z]+\\d{4}")]
                years <- unique(as.numeric(str_extract(year_cols, "\\d{4}")))
                
                # Reshape to long format
                data_long <- data %>%
                  # Keep only ID variables and year columns
                  select(GISJOIN, STUSPS, COUNTY, matches("^[A-Z]+\\d{4}")) %>%
                  # Convert to long format
                  pivot_longer(
                    cols = matches("^[A-Z]+\\d{4}"),
                    names_to = c("variable", "year"),
                    names_pattern = "([A-Z]+)(\\d{4})",
                    values_to = "value"
                  ) %>%
                  mutate(
                    year = as.numeric(year),
                    source = "IPUMS NHGIS"
                  )
                
                # Match variables to standardized names
                nhgis_vars <- crosswalk %>%
                  filter(!is.na(nhgis_var)) %>%
                  select(std_name, nhgis_var)
                
                # Create a wide format with standardized names
                data_wide <- data_long %>%
                  # Join with crosswalk
                  left_join(
                    nhgis_vars %>% 
                      rename(variable = nhgis_var),
                    by = "variable"
                  ) %>%
                  filter(!is.na(std_name)) %>%
                  # Create GEOID from GISJOIN
                  mutate(
                    # NHGIS GISJOIN is G + state FIPS + county FIPS
                    GEOID = str_sub(GISJOIN, 2, 3) + str_sub(GISJOIN, 5, 7),
                    GEOID = str_pad(GEOID, 5, "left", "0")
                  ) %>%
                  # Create NAME
                  mutate(
                    NAME = paste0(COUNTY, " County, ", STUSPS)
                  ) %>%
                  # Pivot to wide format with standardized names
                  pivot_wider(
                    id_cols = c(GEOID, NAME, year, source),
                    names_from = std_name,
                    values_from = value
                  ) %>%
                  # Add quality flags
                  mutate(
                    data_quality = "harmonized",
                    data_source = "IPUMS NHGIS",
                    data_vintage = paste0("NHGIS ", basename(file))
                  )
                
                return(data_wide)
              } else {
                # Single-year data
                quiet_cat(paste("Skipping non-time-series file:", basename(file), "\n"))
                return(NULL)
              }
            }, error = function(e) {
              warning("Error processing NHGIS file ", basename(file), ": ", e$message)
              return(NULL)
            })
          }, future.packages = c("tidyverse", "dplyr", "readr", "stringr"))
          })
        } else {
          # Sequential processing
          nhgis_data_list <- lapply(nhgis_files, function(file) {
            tryCatch({
              quiet_cat(paste("Processing NHGIS file:", basename(file), "\n"))
              
              # Read NHGIS data
              data <- read_csv(file, show_col_types = FALSE)
              
              # Check if this is a time series file
              if (any(str_detect(names(data), "^YEAR"))) {
                # Time series data - has years as part of variable names
                # Extract years from column names
                year_cols <- names(data)[str_detect(names(data), "^[A-Z]+\\d{4}")]
                years <- unique(as.numeric(str_extract(year_cols, "\\d{4}")))
                
                # Reshape to long format
                data_long <- data %>%
                  # Keep only ID variables and year columns
                  select(GISJOIN, STUSPS, COUNTY, matches("^[A-Z]+\\d{4}")) %>%
                  # Convert to long format
                  pivot_longer(
                    cols = matches("^[A-Z]+\\d{4}"),
                    names_to = c("variable", "year"),
                    names_pattern = "([A-Z]+)(\\d{4})",
                    values_to = "value"
                  ) %>%
                  mutate(
                    year = as.numeric(year),
                    source = "IPUMS NHGIS"
                  )
                
                # Match variables to standardized names
                nhgis_vars <- crosswalk %>%
                  filter(!is.na(nhgis_var)) %>%
                  select(std_name, nhgis_var)
                
                # Create a wide format with standardized names
                data_wide <- data_long %>%
                  # Join with crosswalk
                  left_join(
                    nhgis_vars %>% 
                      rename(variable = nhgis_var),
                    by = "variable"
                  ) %>%
                  filter(!is.na(std_name)) %>%
                  # Create GEOID from GISJOIN
                  mutate(
                    # NHGIS GISJOIN is G + state FIPS + county FIPS
                    GEOID = str_sub(GISJOIN, 2, 3) + str_sub(GISJOIN, 5, 7),
                    GEOID = str_pad(GEOID, 5, "left", "0")
                  ) %>%
                  # Create NAME
                  mutate(
                    NAME = paste0(COUNTY, " County, ", STUSPS)
                  ) %>%
                  # Pivot to wide format with standardized names
                  pivot_wider(
                    id_cols = c(GEOID, NAME, year, source),
                    names_from = std_name,
                    values_from = value
                  ) %>%
                  # Add quality flags
                  mutate(
                    data_quality = "harmonized",
                    data_source = "IPUMS NHGIS",
                    data_vintage = paste0("NHGIS ", basename(file))
                  )
                
                return(data_wide)
              } else {
                # Single-year data
                quiet_cat(paste("Skipping non-time-series file:", basename(file), "\n"))
                return(NULL)
              }
            }, error = function(e) {
              warning("Error processing NHGIS file ", basename(file), ": ", e$message)
              return(NULL)
            })
          })
        }
        
        # Combine all NHGIS data
        nhgis_data <- bind_rows(compact(nhgis_data_list))
        
        if (nrow(nhgis_data) > 0) {
          quiet_cat(paste("Successfully processed IPUMS NHGIS data with", nrow(nhgis_data), "rows and", 
              ncol(nhgis_data), "columns.\n"))
          # Save to cache
          saveRDS(nhgis_data, nhgis_cache_file)
          quiet_cat("Saved IPUMS NHGIS data to cache.\n")
        } else {
          warning("No valid NHGIS data could be processed.")
          nhgis_data <- tibble()
        }
      }
      
      all_data$nhgis <- nhgis_data
    } else {
      quiet_cat("No IPUMS NHGIS data files found in 'data/nhgis' directory.\n", important = TRUE)
      quiet_cat("To include NHGIS data, download time series extracts from https://nhgis.org/\n", important = TRUE)
      quiet_cat("and place them in the 'data/nhgis' directory.\n", important = TRUE)
      all_data$nhgis <- tibble()
    }
  }
  
  # Fetch IPUMS NHGIS data with caching
  if (include_nhgis) {
    quiet_cat("\nChecking for IPUMS NHGIS data...\n", important = TRUE)
    
    # Check for cached NHGIS data
    need_refresh_nhgis <- refresh_nhgis || is_cache_stale(nhgis_cache_file)
    
    if (use_cache && file.exists(nhgis_cache_file) && !need_refresh_nhgis) {
      quiet_cat("Loading cached IPUMS NHGIS data...\n", important = TRUE)
      tryCatch({
        nhgis_data <- readRDS(nhgis_cache_file)
        
        # Check file modification time for informational purposes
        file_time <- file.info(nhgis_cache_file)$mtime
        age_days <- as.numeric(difftime(Sys.time(), file_time, units = "days"))
        
        quiet_cat(paste("Successfully loaded IPUMS NHGIS data from cache (", round(age_days, 1), " days old).\n", sep=""), important = TRUE)
        all_data$nhgis <- nhgis_data
      }, error = function(e) {
        quiet_cat(paste("Error loading cached NHGIS data:", conditionMessage(e), "\n"), important = TRUE)
        quiet_cat("Will process NHGIS files directly.\n", important = TRUE)
        process_nhgis_files()
      })
    } else {
      if (need_refresh_nhgis) {
        if (refresh_nhgis) {
          quiet_cat("Refresh requested specifically for IPUMS NHGIS data.\n")  
        } else if (is_cache_stale(nhgis_cache_file)) {
          quiet_cat("IPUMS NHGIS data cache is stale.\n")
        } else if (refresh_cache) {
          quiet_cat("Global cache refresh requested.\n")
        }
      }
      
      quiet_cat("Processing NHGIS files directly...\n", important = TRUE)
      process_nhgis_files()
    }
  } else {
    all_data$nhgis <- tibble()
  }
  
  # Note: Helper function process_nhgis_files has been moved before its first use
  
  # Fetch and process life expectancy data from IHME if requested
  if (include_life_expectancy) {
    quiet_cat("\nProcessing IHME Life Expectancy data...\n", important = TRUE)
    
    # Define cache file
    life_exp_cache_file <- file.path(cache_dir, "life_expectancy_data.rds")
    
    # Check if cache should be refreshed
    need_refresh_life_exp <- if(is.null(cache_options$refresh_life_expectancy)) refresh_cache else cache_options$refresh_life_expectancy
    need_refresh_life_exp <- need_refresh_life_exp || is_cache_stale(life_exp_cache_file)
    
    if (use_cache && file.exists(life_exp_cache_file) && !need_refresh_life_exp) {
      quiet_cat("Loading cached life expectancy data...\n", important = TRUE)
      tryCatch({
        life_exp_data <- readRDS(life_exp_cache_file)
        
        # Check file modification time for informational purposes
        file_time <- file.info(life_exp_cache_file)$mtime
        age_days <- as.numeric(difftime(Sys.time(), file_time, units = "days"))
        
        quiet_cat(paste("Successfully loaded life expectancy data from cache (", round(age_days, 1), " days old).\n", sep=""), important = TRUE)
        all_data$life_expectancy <- life_exp_data
      }, error = function(e) {
        quiet_cat(paste("Error loading cached life expectancy data:", conditionMessage(e), "\n"), important = TRUE)
        quiet_cat("Will fetch fresh life expectancy data.\n", important = TRUE)
        life_exp_data <- fetch_life_expectancy_data(years, cache_dir, refresh_cache = TRUE)
        all_data$life_expectancy <- life_exp_data
      })
    } else {
      quiet_cat("Fetching fresh life expectancy data...\n", important = TRUE)
      life_exp_data <- fetch_life_expectancy_data(years, cache_dir, refresh_cache = TRUE)
      all_data$life_expectancy <- life_exp_data
    }
  } else {
    all_data$life_expectancy <- tibble()
  }
  
  # Return the combined data list
  return(all_data)
}

#' Helper function to create an empty life expectancy structure with all required fields
#'
#' @param counties A dataframe with GEOID and NAME columns
#' @param years Vector of years to include
#' @return A placeholder dataframe with all life expectancy fields set to NA
empty_life_exp_structure <- function(counties, years) {
  if (nrow(counties) == 0) {
    # Create at least some basic counties when none are available
    state_fips <- c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", 
                   "13", "15", "16", "17", "18", "19", "20", "21", "22", "23", 
                   "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", 
                   "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", 
                   "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56")
    
    counties <- tibble(
      STATEFP = rep(state_fips, each = 3),
      COUNTYFP = rep(c("001", "003", "005"), times = length(state_fips)),
      GEOID = paste0(STATEFP, COUNTYFP),
      NAME = paste0("County ", COUNTYFP, ", State ", STATEFP)
    )
  }
  
  expand.grid(
    GEOID = counties$GEOID,
    year = years,
    stringsAsFactors = FALSE
  ) %>%
    left_join(counties, by = "GEOID") %>%
    mutate(
      # Overall life expectancy
      life_expectancy = NA,
      life_expectancy_lower = NA,
      life_expectancy_upper = NA,
      
      # Gender-specific
      life_expectancy_female = NA,
      life_expectancy_male = NA,
      
      # Race-specific (combined gender)
      life_expectancy_latino = NA,
      life_expectancy_black = NA,
      life_expectancy_white = NA,
      life_expectancy_aian = NA,
      life_expectancy_api = NA,
      
      # Race-specific confidence intervals
      life_expectancy_latino_lower = NA,
      life_expectancy_latino_upper = NA,
      life_expectancy_black_lower = NA,
      life_expectancy_black_upper = NA,
      life_expectancy_white_lower = NA,
      life_expectancy_white_upper = NA,
      life_expectancy_aian_lower = NA,
      life_expectancy_aian_upper = NA,
      life_expectancy_api_lower = NA,
      life_expectancy_api_upper = NA,
      
      # Race-gender combined
      life_expectancy_female_latino = NA,
      life_expectancy_female_black = NA,
      life_expectancy_female_white = NA,
      life_expectancy_female_aian = NA,
      life_expectancy_female_api = NA,
      
      life_expectancy_male_latino = NA,
      life_expectancy_male_black = NA,
      life_expectancy_male_white = NA,
      life_expectancy_male_aian = NA,
      life_expectancy_male_api = NA,
      
      # Metadata
      data_source = "IHME (Placeholder)",
      data_quality = "missing",
      data_vintage = "IHME Placeholder"
    )
}

#' Fetch and process county-level life expectancy data from IHME
#'
#' This function retrieves life expectancy data from the Institute for Health Metrics and Evaluation (IHME).
#' IHME produces county-level life expectancy estimates at birth for the United States.
#'
#' @param years Vector of years to include in the dataset
#' @param cache_dir Directory to store cache files
#' @param refresh_cache Whether to refresh the cache
#' @return A data frame with life expectancy data by county and year
fetch_life_expectancy_data <- function(years, cache_dir = "data/cache", refresh_cache = FALSE) {
  # Helper function for clean output
  print_msg <- function(msg) {
    # Check if being run interactively
    is_interactive_run <- !exists("is_sourced") || !is_sourced
    if (is_interactive_run) {
      message(msg)
    } else {
      cat(msg, "\n")
    }
  }
  
  # Define the cache file path
  cache_file <- file.path(cache_dir, "life_expectancy_data.rds")
  
  # Check if cache should be used
  if (!refresh_cache && file.exists(cache_file)) {
    print_msg("Loading cached life expectancy data...")
    return(readRDS(cache_file))
  }
  
  print_msg("Fetching life expectancy data from IHME...")
  
  # Create directory for IHME data if it doesn't exist
  ihme_dir <- "data/ihme"
  if (!dir.exists(ihme_dir)) {
    dir.create(ihme_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Check multiple potential locations for IHME files
  ihme_locations <- list(
    "csv" = file.path(ihme_dir, "CSV"),                # Primary location
    "extracted" = file.path(ihme_dir, "extracted"),    # Secondary location
    "root" = ihme_dir                                  # Fallback location
  )
  
  print_msg("Checking for IHME life expectancy files in multiple locations...")
  
  # Find the first directory that exists and has CSV files
  ihme_csv_dir <- NULL
  for (loc_name in names(ihme_locations)) {
    loc_path <- ihme_locations[[loc_name]]
    if (dir.exists(loc_path) && 
        length(list.files(loc_path, pattern = ".*\\.(CSV|csv)$")) > 0) {
      ihme_csv_dir <- loc_path
      print_msg(paste("Found IHME CSV files in:", loc_path))
      break
    }
  }
  
  # Check if files in the new format exist
  new_format_files_exist <- !is.null(ihme_csv_dir) && 
                            length(list.files(ihme_csv_dir, pattern = "IHME_USA_LE_COUNTY_.*\\.(CSV|csv)$", ignore.case = TRUE)) > 0
  
  # If CSV dir exists but no LT files found, provide more info
  if (!is.null(ihme_csv_dir) && !new_format_files_exist) {
    csv_files <- list.files(ihme_csv_dir, pattern = ".*\\.(CSV|csv)$")
    print_msg(paste("Found", length(csv_files), "CSV files, but none match the expected format."))
    if (length(csv_files) > 0) {
      print_msg(paste("Example file:", csv_files[1]))
    }
  }
  
  # Legacy format file paths (kept for backward compatibility)
  legacy_files <- c(
    file.path(ihme_dir, "IHME_USA_COUNTY_LE_MORTALITY_RISK_1980_2019.csv"),
    file.path(ihme_dir, "IHME_USA_COUNTY_LE_MORTALITY_RISK_1980_2010.csv")
  )
  legacy_format_files_exist <- any(file.exists(legacy_files))
  
  # If no files exist in either format, create placeholder data
  if (!new_format_files_exist && !legacy_format_files_exist) {
    print_msg("IHME life expectancy data files not found in either new or legacy format.")
    print_msg("The files have been downloaded to the 'data/ihme' directory, but may need to be extracted.")
    print_msg("Please ensure the CSV files are extracted to the 'data/ihme/extracted' directory.")
    
    # Create placeholder data with counties
    print_msg("Creating IHME life expectancy placeholder data...")
    
    # Get counties for placeholder data
    counties <- tryCatch({
      # First try with tigris
      tigris::counties(cb = TRUE, year = 2020) %>%
        sf::st_drop_geometry() %>%
        select(GEOID, NAME) %>%
        mutate(GEOID = as.character(GEOID))
    }, error = function(e) {
      # If tigris fails, create a basic set of counties manually
      print_msg("Error using tigris to get counties. Using fallback method.")
      
      # Create a basic set of counties manually (one from each state)
      state_fips <- c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", 
                     "13", "15", "16", "17", "18", "19", "20", "21", "22", "23", 
                     "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", 
                     "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", 
                     "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56")
      
      # Create a dataframe with basic county info
      tibble(
        STATEFP = rep(state_fips, each = 3),
        COUNTYFP = rep(c("001", "003", "005"), times = length(state_fips)),
        GEOID = paste0(STATEFP, COUNTYFP),
        NAME = paste0("County ", COUNTYFP, ", State ", STATEFP)
      )
    })
    
    # Create empty life expectancy dataset with right structure
    empty_life_exp <- empty_life_exp_structure(counties, years)
    
    print_msg("Created life expectancy placeholder with the correct structure.")
    return(empty_life_exp)
  }
  
  # Process datasets based on format available
  datasets <- list()
  
  # Process new format files if they exist
  if (new_format_files_exist) {
    print_msg("Processing IHME life expectancy files in new format...")
    print_msg(paste("Using IHME files from directory:", ihme_csv_dir))
    
    # Find all 'BOTH' sex files - these contain life expectancy for both sexes combined
    both_sex_files <- list.files(
      ihme_csv_dir, 
      pattern = "IHME_USA_LE_COUNTY_.*_BOTH_.*\\.(CSV|csv)$", 
      full.names = TRUE, 
      ignore.case = TRUE
    )
    
    # Find all 'FEMALE' sex files
    female_files <- list.files(
      ihme_csv_dir, 
      pattern = "IHME_USA_LE_COUNTY_.*_FEMALE_.*\\.(CSV|csv)$", 
      full.names = TRUE, 
      ignore.case = TRUE
    )
    
    # Find all 'MALE' sex files
    male_files <- list.files(
      ihme_csv_dir, 
      pattern = "IHME_USA_LE_COUNTY_.*_MALE_.*\\.(CSV|csv)$", 
      full.names = TRUE, 
      ignore.case = TRUE
    )
    
    # Process each group of files
    life_exp_data <- NULL
    
    # Get the years from the filenames
    file_years <- unique(sub(".*_LT_(\\d{4})_.*", "\\1", basename(both_sex_files)))
    print_msg(paste("Found data for years:", paste(file_years, collapse = ", ")))
    
    # Only process the requested years
    available_years <- intersect(as.character(years), file_years)
    
    if (length(available_years) > 0) {
      # Process each year
      year_datasets <- list()
      
      for (year in available_years) {
        print_msg(paste("Processing data for year", year))
        
        # Find the files for this year
        both_file <- grep(paste0("_LT_", year, "_BOTH_"), both_sex_files, value = TRUE)[1]
        female_file <- grep(paste0("_LT_", year, "_FEMALE_"), female_files, value = TRUE)[1]
        male_file <- grep(paste0("_LT_", year, "_MALE_"), male_files, value = TRUE)[1]
        
        # Process the 'BOTH' file (overall life expectancy)
        if (!is.na(both_file)) {
          tryCatch({
            # Read the data
            both_data <- read_csv(both_file, show_col_types = FALSE)
            
            # Filter to keep only county-level data (has a FIPS code)
            # For birth expectancy, we only want <1 year age group
            county_data_birth <- both_data %>%
              filter(
                !is.na(fips) & fips != "",
                age_name == "<1 year"  # Life expectancy at birth
              )
            
            # Process county total (all races) data
            county_total <- county_data_birth %>%
              filter(race_id == 1) %>%  # Total race (all races)
              select(fips, location_name, val, lower, upper)
            
            # Process data by race for this county
            # Race ID mapping: 1=Total, 2=Latino, 4=Black, 5=White, 6=AIAN (American Indian/Alaska Native), 7=API (Asian/Pacific Islander)
            race_map <- c(
              "2" = "latino", 
              "4" = "black", 
              "5" = "white", 
              "6" = "aian", 
              "7" = "api"
            )
            
            county_race_data <- county_data_birth %>%
              filter(race_id %in% c(2, 4, 5, 6, 7)) %>%
              mutate(race_code = race_map[as.character(race_id)]) %>%
              select(fips, race_code, val, lower, upper) %>%
              # Reshape to wide format with race-specific columns
              pivot_wider(
                id_cols = fips,
                names_from = race_code,
                values_from = c(val, lower, upper),
                names_glue = "{.value}_{.name}"
              )
            
            # Process female data if available
            female_data <- NULL
            if (!is.na(female_file)) {
              female_data_all <- read_csv(female_file, show_col_types = FALSE) %>%
                filter(
                  !is.na(fips) & fips != "",
                  age_name == "<1 year"
                )
              
              # Total female
              female_data <- female_data_all %>%
                filter(race_id == 1) %>%
                select(fips, val) %>%
                rename(female_val = val)
              
              # Female by race
              female_race_data <- female_data_all %>%
                filter(race_id %in% c(2, 4, 5, 6, 7)) %>%
                mutate(race_code = race_map[as.character(race_id)]) %>%
                select(fips, race_code, val) %>%
                pivot_wider(
                  id_cols = fips,
                  names_from = race_code,
                  values_from = val,
                  names_glue = "female_val_{.name}"
                )
              
              # Join female race data
              if (nrow(female_race_data) > 0) {
                female_data <- female_data %>%
                  left_join(female_race_data, by = "fips")
              }
            }
            
            # Process male data if available
            male_data <- NULL
            if (!is.na(male_file)) {
              male_data_all <- read_csv(male_file, show_col_types = FALSE) %>%
                filter(
                  !is.na(fips) & fips != "",
                  age_name == "<1 year"
                )
              
              # Total male
              male_data <- male_data_all %>%
                filter(race_id == 1) %>%
                select(fips, val) %>%
                rename(male_val = val)
              
              # Male by race
              male_race_data <- male_data_all %>%
                filter(race_id %in% c(2, 4, 5, 6, 7)) %>%
                mutate(race_code = race_map[as.character(race_id)]) %>%
                select(fips, race_code, val) %>%
                pivot_wider(
                  id_cols = fips,
                  names_from = race_code,
                  values_from = val,
                  names_glue = "male_val_{.name}"
                )
              
              # Join male race data
              if (nrow(male_race_data) > 0) {
                male_data <- male_data %>%
                  left_join(male_race_data, by = "fips")
              }
            }
            
            # Create a base dataset
            temp_data <- county_total %>%
              mutate(
                GEOID = str_pad(fips, 5, "left", "0"),
                year_val = as.numeric(year),
                county_name = location_name,
                
                # Overall life expectancy
                life_expectancy = val,
                life_expectancy_lower = lower,
                life_expectancy_upper = upper
              )
            
            # Join with race data if available
            if (!is.null(county_race_data) && nrow(county_race_data) > 0) {
              temp_data <- temp_data %>%
                left_join(county_race_data, by = "fips")
            }
            
            # Join with gender data if available
            if (!is.null(female_data) && nrow(female_data) > 0) {
              temp_data <- temp_data %>%
                left_join(female_data, by = "fips")
            }
            
            if (!is.null(male_data) && nrow(male_data) > 0) {
              temp_data <- temp_data %>%
                left_join(male_data, by = "fips")
            }
            
            # Create the final dataset with all variables, handling missing columns
            year_data <- temp_data %>%
              mutate(
                # Gender-specific (placeholders if not available)
                life_expectancy_female = if("female_val" %in% names(.)) female_val else NA,
                life_expectancy_male = if("male_val" %in% names(.)) male_val else NA,
                
                # Race-specific (combined gender)
                life_expectancy_latino = if("val_latino" %in% names(.)) val_latino else NA,
                life_expectancy_black = if("val_black" %in% names(.)) val_black else NA,
                life_expectancy_white = if("val_white" %in% names(.)) val_white else NA,
                life_expectancy_aian = if("val_aian" %in% names(.)) val_aian else NA,
                life_expectancy_api = if("val_api" %in% names(.)) val_api else NA,
                
                # Race-specific confidence intervals
                life_expectancy_latino_lower = if("lower_latino" %in% names(.)) lower_latino else NA,
                life_expectancy_latino_upper = if("upper_latino" %in% names(.)) upper_latino else NA,
                life_expectancy_black_lower = if("lower_black" %in% names(.)) lower_black else NA,
                life_expectancy_black_upper = if("upper_black" %in% names(.)) upper_black else NA,
                life_expectancy_white_lower = if("lower_white" %in% names(.)) lower_white else NA,
                life_expectancy_white_upper = if("upper_white" %in% names(.)) upper_white else NA,
                life_expectancy_aian_lower = if("lower_aian" %in% names(.)) lower_aian else NA,
                life_expectancy_aian_upper = if("upper_aian" %in% names(.)) upper_aian else NA,
                life_expectancy_api_lower = if("lower_api" %in% names(.)) lower_api else NA,
                life_expectancy_api_upper = if("upper_api" %in% names(.)) upper_api else NA,
                
                # Race-gender combined
                life_expectancy_female_latino = if("female_val_latino" %in% names(.)) female_val_latino else NA,
                life_expectancy_female_black = if("female_val_black" %in% names(.)) female_val_black else NA,
                life_expectancy_female_white = if("female_val_white" %in% names(.)) female_val_white else NA,
                life_expectancy_female_aian = if("female_val_aian" %in% names(.)) female_val_aian else NA,
                life_expectancy_female_api = if("female_val_api" %in% names(.)) female_val_api else NA,
                
                life_expectancy_male_latino = if("male_val_latino" %in% names(.)) male_val_latino else NA,
                life_expectancy_male_black = if("male_val_black" %in% names(.)) male_val_black else NA,
                life_expectancy_male_white = if("male_val_white" %in% names(.)) male_val_white else NA,
                life_expectancy_male_aian = if("male_val_aian" %in% names(.)) male_val_aian else NA,
                life_expectancy_male_api = if("male_val_api" %in% names(.)) male_val_api else NA,
                
                # Metadata
                data_source = "IHME",
                data_quality = "estimate",
                data_vintage = paste0("IHME ", year)
              ) %>%
              # Select final columns in consistent order
              select(
                GEOID,
                year = year_val,
                NAME = county_name,
                
                # Standard life expectancy variables
                life_expectancy,
                life_expectancy_lower,
                life_expectancy_upper,
                life_expectancy_female,
                life_expectancy_male,
                
                # Race-specific variables
                life_expectancy_latino,
                life_expectancy_black,
                life_expectancy_white,
                life_expectancy_aian,
                life_expectancy_api,
                
                # Race-specific confidence intervals
                life_expectancy_latino_lower,
                life_expectancy_latino_upper,
                life_expectancy_black_lower,
                life_expectancy_black_upper,
                life_expectancy_white_lower,
                life_expectancy_white_upper,
                life_expectancy_aian_lower,
                life_expectancy_aian_upper,
                life_expectancy_api_lower,
                life_expectancy_api_upper,
                
                # Race-gender combined
                life_expectancy_female_latino,
                life_expectancy_female_black,
                life_expectancy_female_white,
                life_expectancy_female_aian,
                life_expectancy_female_api,
                
                life_expectancy_male_latino,
                life_expectancy_male_black,
                life_expectancy_male_white,
                life_expectancy_male_aian,
                life_expectancy_male_api,
                
                # Metadata
                data_source,
                data_quality,
                data_vintage
              )
            
            year_datasets[[year]] <- year_data
            print_msg(paste("Successfully processed IHME data for year", year))
            
          }, error = function(e) {
            warning(paste("Error processing IHME data for year", year, ":", conditionMessage(e)))
          })
        } else {
          warning(paste("No 'BOTH' file found for year", year))
        }
      }
      
      # Combine all years
      if (length(year_datasets) > 0) {
        datasets$new_format <- bind_rows(year_datasets)
        print_msg(paste("Successfully processed data for", length(year_datasets), "years in new format."))
      }
    } else {
      warning("No files found matching requested years in new format.")
    }
  }
  
  # Process legacy format files if they exist and we need to supplement years not in new format
  if (legacy_format_files_exist) {
    processed_years <- if(!is.null(datasets$new_format)) unique(datasets$new_format$year) else numeric(0)
    missing_years <- setdiff(years, processed_years)
    
    if (length(missing_years) > 0) {
      print_msg(paste("Processing legacy format files to cover years:", paste(missing_years, collapse = ", ")))
      
      # Process most recent legacy dataset (2019)
      if (file.exists(legacy_files[1])) {
        tryCatch({
          recent_data <- read_csv(legacy_files[1], show_col_types = FALSE)
          
          # Check for expected columns
          if (all(c("fips", "year", "le_agestandardized", "le_agestandardized_female", "le_agestandardized_male") %in% names(recent_data))) {
            # Process and clean data
            recent_processed <- recent_data %>%
              filter(year %in% missing_years) %>%
              transmute(
                GEOID = str_pad(fips, 5, "left", "0"),
                year = as.numeric(year),
                # Overall life expectancy
                life_expectancy = le_agestandardized,
                life_expectancy_lower = NA,  # Not available in legacy format
                life_expectancy_upper = NA,  
                
                # Gender-specific
                life_expectancy_female = le_agestandardized_female,
                life_expectancy_male = le_agestandardized_male,
                
                # Race-specific placeholders (not available in legacy format)
                life_expectancy_latino = NA,
                life_expectancy_black = NA,
                life_expectancy_white = NA,
                life_expectancy_aian = NA,
                life_expectancy_api = NA,
                
                # Race-specific confidence intervals (not available)
                life_expectancy_latino_lower = NA,
                life_expectancy_latino_upper = NA,
                life_expectancy_black_lower = NA,
                life_expectancy_black_upper = NA,
                life_expectancy_white_lower = NA,
                life_expectancy_white_upper = NA,
                life_expectancy_aian_lower = NA,
                life_expectancy_aian_upper = NA,
                life_expectancy_api_lower = NA,
                life_expectancy_api_upper = NA,
                
                # Race-gender combined (not available)
                life_expectancy_female_latino = NA,
                life_expectancy_female_black = NA,
                life_expectancy_female_white = NA,
                life_expectancy_female_aian = NA,
                life_expectancy_female_api = NA,
                
                life_expectancy_male_latino = NA,
                life_expectancy_male_black = NA,
                life_expectancy_male_white = NA,
                life_expectancy_male_aian = NA,
                life_expectancy_male_api = NA,
                
                # Metadata
                data_source = "IHME",
                data_quality = "estimate",
                data_vintage = "IHME 2019 Legacy"
              )
            
            datasets$legacy_recent <- recent_processed
            print_msg("Successfully processed legacy IHME dataset (2019).")
          } else {
            warning("Legacy IHME dataset missing expected columns")
          }
        }, error = function(e) {
          warning("Error processing legacy IHME dataset: ", conditionMessage(e))
        })
      }
      
      # Process older legacy dataset (2010) for additional years
      if (file.exists(legacy_files[2])) {
        still_missing_years <- if(!is.null(datasets$legacy_recent)) 
          setdiff(missing_years, unique(datasets$legacy_recent$year)) else missing_years
        
        if (length(still_missing_years) > 0) {
          tryCatch({
            older_data <- read_csv(legacy_files[2], show_col_types = FALSE)
            
            # Check for expected columns (older dataset might have different column names)
            if (all(c("fips", "year", "le_agestandardized", "le_agestandardized_female", "le_agestandardized_male") %in% names(older_data)) ||
                all(c("fips", "year", "le_both", "le_female", "le_male") %in% names(older_data))) {
              
              # Determine column mapping based on what's available
              le_column <- if ("le_agestandardized" %in% names(older_data)) "le_agestandardized" else "le_both"
              le_female_column <- if ("le_agestandardized_female" %in% names(older_data)) "le_agestandardized_female" else "le_female"
              le_male_column <- if ("le_agestandardized_male" %in% names(older_data)) "le_agestandardized_male" else "le_male"
              
              # Process and clean data
              older_processed <- older_data %>%
                filter(year %in% still_missing_years) %>%
                transmute(
                  GEOID = str_pad(fips, 5, "left", "0"),
                  year = as.numeric(year),
                  # Overall life expectancy
                  life_expectancy = .data[[le_column]],
                  life_expectancy_lower = NA,  # Not available in legacy format
                  life_expectancy_upper = NA,  
                  
                  # Gender-specific
                  life_expectancy_female = .data[[le_female_column]],
                  life_expectancy_male = .data[[le_male_column]],
                  
                  # Race-specific placeholders (not available in legacy format)
                  life_expectancy_latino = NA,
                  life_expectancy_black = NA,
                  life_expectancy_white = NA,
                  life_expectancy_aian = NA,
                  life_expectancy_api = NA,
                  
                  # Race-specific confidence intervals (not available)
                  life_expectancy_latino_lower = NA,
                  life_expectancy_latino_upper = NA,
                  life_expectancy_black_lower = NA,
                  life_expectancy_black_upper = NA,
                  life_expectancy_white_lower = NA,
                  life_expectancy_white_upper = NA,
                  life_expectancy_aian_lower = NA,
                  life_expectancy_aian_upper = NA,
                  life_expectancy_api_lower = NA,
                  life_expectancy_api_upper = NA,
                  
                  # Race-gender combined (not available)
                  life_expectancy_female_latino = NA,
                  life_expectancy_female_black = NA,
                  life_expectancy_female_white = NA,
                  life_expectancy_female_aian = NA,
                  life_expectancy_female_api = NA,
                  
                  life_expectancy_male_latino = NA,
                  life_expectancy_male_black = NA,
                  life_expectancy_male_white = NA,
                  life_expectancy_male_aian = NA,
                  life_expectancy_male_api = NA,
                  
                  # Metadata
                  data_source = "IHME",
                  data_quality = "estimate",
                  data_vintage = "IHME 2010 Legacy"
                )
              
              datasets$legacy_older <- older_processed
              print_msg("Successfully processed older legacy IHME dataset (2010).")
            } else {
              warning("Older legacy IHME dataset missing expected columns")
            }
          }, error = function(e) {
            warning("Error processing older legacy IHME dataset: ", conditionMessage(e))
          })
        }
      }
    }
  }
  
  # Combine all datasets
  # Make sure we have at least one valid dataset
  if (length(datasets) == 0) {
    print_msg("No valid datasets found. Creating placeholder dataset...")
    # Create placeholder dataset with basic structure
    counties <- tryCatch({
      tigris::counties(cb = TRUE, year = 2020) %>%
        sf::st_drop_geometry() %>%
        transmute(
          GEOID = as.character(GEOID),
          NAME = paste0(NAME, ", ", STUSPS)
        )
    }, error = function(e) {
      # Fallback county list
      tibble(
        GEOID = character(0),
        NAME = character(0)
      )
    })
    
    combined_data <- empty_life_exp_structure(counties, years)
  } else {
    # Check if any dataset is valid
    valid_dataset <- FALSE
    for (ds in datasets) {
      if (!is.null(ds) && nrow(ds) > 0) {
        valid_dataset <- TRUE
        break
      }
    }
    
    if (!valid_dataset) {
      print_msg("No valid data in datasets. Creating placeholder dataset...")
      counties <- tibble(
        GEOID = character(0),
        NAME = character(0)
      )
      combined_data <- empty_life_exp_structure(counties, years)
    } else {
      # Filter out NULL datasets and bind the valid ones
      valid_datasets <- datasets[!sapply(datasets, is.null)]
      combined_data <- bind_rows(valid_datasets) %>%
        arrange(GEOID, year)
    }
  }
  
  # Clean up NAME columns - ensure we have exactly one NAME column
  if (sum(grepl("^NAME", names(combined_data))) > 1) {
    # Multiple NAME columns - keep the first non-NA version and rename it
    combined_data <- combined_data %>%
      mutate(
        NAME_final = coalesce(!!!syms(grep("^NAME", names(.), value = TRUE)))
      ) %>%
      select(-matches("^NAME\\.[xy]$"), -matches("^NAME$")) %>%
      rename(NAME = NAME_final)
  } else if (!"NAME" %in% names(combined_data) && "county_name" %in% names(combined_data)) {
    # If we have county_name but no NAME, rename it
    combined_data <- combined_data %>%
      rename(NAME = county_name)
  } else if (sum(grepl("^NAME", names(combined_data))) == 0) {
    # No NAME column at all - try to add it using tigris
    tryCatch({
      counties <- tigris::counties(cb = TRUE, year = 2020) %>%
        sf::st_drop_geometry() %>%
        transmute(
          GEOID = as.character(GEOID),
          NAME = paste0(NAME, ", ", STUSPS)
        )
      
      combined_data <- combined_data %>%
        left_join(counties, by = "GEOID")
    }, error = function(e) {
      warning("Could not add county names: ", conditionMessage(e))
      # Add a placeholder NAME column
      combined_data$NAME <- paste0("County ", substr(combined_data$GEOID, 3, 5))
    })
  }
  
  # Check if we have data for all requested years
  missing_years <- setdiff(years, unique(combined_data$year))
  if (length(missing_years) > 0) {
    print_msg(paste("Missing data for years:", paste(missing_years, collapse = ", ")))
    print_msg("Creating placeholder data for missing years...")
    
    # Get counties from the existing data - check if NAME exists, otherwise try county_name
    if ("NAME" %in% names(combined_data)) {
      counties_df <- combined_data %>% 
        select(GEOID, NAME) %>% 
        distinct()
    } else if ("county_name" %in% names(combined_data)) {
      counties_df <- combined_data %>% 
        select(GEOID, county_name) %>%
        rename(NAME = county_name) %>%
        distinct()
    } else {
      # Create county info from GEOID
      counties_df <- combined_data %>%
        select(GEOID) %>%
        distinct() %>%
        mutate(NAME = paste0("County ", substr(GEOID, 3, 5), ", State ", substr(GEOID, 1, 2)))
    }
    
    # Create placeholder data for missing years using our helper function
    missing_data <- empty_life_exp_structure(counties_df, missing_years)
    
    # Add missing years to the combined dataset
    combined_data <- bind_rows(combined_data, missing_data) %>%
      arrange(GEOID, year)
  }
  
  # Save to cache
  saveRDS(combined_data, cache_file)
  print_msg("Saved life expectancy data to cache.")
  
  return(combined_data)
}

# FIXED CDC PLACES DATA PROCESSING FUNCTION
process_cdc_places_data <- function(places_file) {
  cat("Processing CDC PLACES data...\n")
  
  # Read and process PLACES data with verbose error handling
  tryCatch({
    # Read with readr for better error handling
    places_data_raw <- read_csv(places_file, show_col_types = FALSE, 
                               progress = TRUE, guess_max = 10000)
    
    # Print some diagnostics
    cat("CDC PLACES file loaded. Dimensions:", dim(places_data_raw)[1], "rows,", 
        dim(places_data_raw)[2], "columns\n")
    cat("First few column names:", paste(head(names(places_data_raw), 10), collapse=", "), "...\n")
    
    # Print a preview of the data structure
    cat("First few rows of key columns:\n")
    if(all(c("CountyFIPS", "LocationName", "Measure", "Data_Value") %in% names(places_data_raw))) {
      head_data <- places_data_raw %>% 
        select(CountyFIPS, LocationName, Measure, Data_Value) %>% 
        head(5)
      print(head_data)
    } else {
      cat("Expected column structure not found\n")
    }
    
    # Define health measure mapping in a more explicit way
    measure_mapping <- tibble(
      measure_code = c(
        "CASTHMA", "ARTHRITIS", "BPHIGH", "CANCER", "CHD", 
        "CHECKUP", "DEPRESSION", "DIABETES", "HIGHCHOL", "KIDNEY", 
        "OBESITY", "STROKE", "PHLTH", "MHLTH", "CSMOKING", 
        "DENTAL", "SLEEP", "ACCESS2", "BINGE", "COPD", "LPA"
      ),
      std_name = c(
        "asthma_pct", "arthritis_pct", "high_blood_pressure_pct", 
        "cancer_pct", "coronary_heart_disease_pct", "annual_checkup_pct", 
        "depression_pct", "diabetes_pct", "high_cholesterol_pct", 
        "kidney_disease_pct", "obesity_pct", "stroke_pct", 
        "poor_physical_health_pct", "poor_mental_health_pct", "smoking_pct", 
        "dental_visit_pct", "insufficient_sleep_pct", "no_health_insurance_pct", 
        "binge_drinking_pct", "copd_pct", "physical_inactivity_pct"
      )
    )
    
    # Determine year column
    year_col <- NULL
    for(col in c("Year", "year", "YEAR", "DataYear", "data_year")) {
      if(col %in% names(places_data_raw)) {
        year_col <- col
        break
      }
    }
    
    if(is.null(year_col)) {
      year_col <- "Year"
      places_data_raw$Year <- 2022  # Default to most recent if no year column
    }
    
    # Check data structure and adapt
    if("Measure" %in% names(places_data_raw) && "Data_Value" %in% names(places_data_raw)) {
      # Standard CDC format - measures are in rows
      cat("Using standard CDC PLACES data format with Measure and Data_Value columns\n")
      
      # Filter to only county-level data if GeographicLevel exists
      if("GeographicLevel" %in% names(places_data_raw)) {
        places_data <- places_data_raw %>%
          filter(GeographicLevel == "County")
      } else {
        places_data <- places_data_raw
      }
      
      # Extract county FIPS and name columns
      fips_col <- if("CountyFIPS" %in% names(places_data)) "CountyFIPS" else 
                  if("LocationID" %in% names(places_data)) "LocationID" else NULL
      
      name_col <- if("LocationName" %in% names(places_data)) "LocationName" else 
                 if("CountyName" %in% names(places_data)) "CountyName" else NULL
      
      if(is.null(fips_col) || is.null(name_col)) {
        warning("Cannot find county identifier columns in CDC PLACES data")
        return(tibble())
      }
      
      cat("Using", fips_col, "for county FIPS codes and", name_col, "for county names\n")
      
      # Create base dataset with IDs
      places_processed <- places_data %>%
        select(!!fips_col, !!name_col, !!year_col) %>%
        distinct() %>%
        rename(
          GEOID = !!fips_col,
          NAME = !!name_col,
          year = !!year_col
        ) %>%
        mutate(
          GEOID = str_pad(GEOID, 5, "left", "0"),
          source = "CDC PLACES",
          data_quality = "direct",
          data_source = "CDC PLACES",
          data_vintage = paste0("PLACES ", year)
        )
      
      # Process each health measure and pivot to wide format
      # Get unique measures in the dataset
      unique_measures <- unique(places_data$Measure)
      cat("Found", length(unique_measures), "unique health measures\n")
      cat("Sample measures:", paste(head(unique_measures, 5), collapse=", "), "...\n")
      
      # Map available measures to standard names
      matched_measures <- measure_mapping %>%
        filter(measure_code %in% unique_measures)
      
      cat("Successfully mapped", nrow(matched_measures), "measures to standard variable names\n")
      
      # For each mapped measure, add to the dataset
      for(i in 1:nrow(matched_measures)) {
        measure <- matched_measures$measure_code[i]
        std_name <- matched_measures$std_name[i]
        
        cat("Processing measure", measure, "as", std_name, "\n")
        
        # Make sure the measure code is not empty or NA
        if (is.na(measure) || measure == "") {
          quiet_cat("Skipping empty or NA measure code\n")
          next
        }
        
        # Extract and join this measure's data using safer approach
        tryCatch({
          # Be extra careful with filter operation when measure could be NA
          # First create a safe non-NA variable to match against
          safe_measure <- measure
          
          # Safely filter using more robust is.na handling
          measure_data <- places_data %>%
            filter(!is.na(Measure) & Measure == safe_measure) %>%
            select(!!fips_col, Data_Value) %>%
            rename(
              GEOID = !!fips_col,
              !!std_name := Data_Value
            ) %>%
            mutate(GEOID = str_pad(GEOID, 5, "left", "0"))
        }, error = function(e) {
          quiet_cat(paste("Error filtering for measure", measure, ":", conditionMessage(e), "\n"), important = TRUE)
          measure_data <- tibble(GEOID = character(), !!std_name := numeric())
        })
        
        # Join to main dataset
        places_processed <- places_processed %>%
          left_join(measure_data, by = "GEOID")
      }
      
      cat("Processed", nrow(places_processed), "rows of CDC PLACES data with", 
          nrow(matched_measures), "health metrics\n")
      
      return(places_processed)
      
    } else {
      # Non-standard format - need to adapt
      warning("Could not identify standard CDC PLACES data format")
      return(tibble())
    }
    
  }, error = function(e) {
    warning("Error processing CDC PLACES data: ", e$message)
    cat("Error details:", conditionMessage(e), "\n")
    return(tibble())
  })
}
