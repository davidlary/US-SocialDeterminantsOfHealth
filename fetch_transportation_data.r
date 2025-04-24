#!/usr/bin/env Rscript

# Transportation Data Fetcher
# This script handles retrieval of transportation data from NHTS, All Transit Database, and ACS

library(tidyverse)
library(httr)
library(jsonlite)
library(readxl)
library(lubridate)
library(sf)
library(zoo) # For interpolation if needed

#' Find local transportation data files
#'
#' Searches multiple directories for transportation data files, including
#' NHTS, All Transit Database, and ACS transportation-related data.
#'
#' @return A list of file paths organized by data type
find_local_transportation_files <- function() {
  # List of directories to check
  transport_dirs <- c(
    "data/transportation",
    "data/cache/transportation",
    "data/transit",
    "data/nhts",
    "data/transport"
  )
  
  # Also check subdirectories for specific data types
  for (base_dir in c("data", "data/cache")) {
    for (subdir in c("transportation", "transit", "nhts", "transport", "acs_transportation")) {
      transport_dirs <- c(transport_dirs, file.path(base_dir, subdir))
    }
  }
  
  # List of possible file extensions
  file_exts <- c("\\.csv$", "\\.xlsx$", "\\.xls$", "\\.zip$", "\\.txt$", "\\.rds$")
  
  # Search for files
  all_files <- c()
  for (dir in transport_dirs) {
    if (dir.exists(dir)) {
      for (ext in file_exts) {
        files <- list.files(dir, pattern = ext, full.names = TRUE, recursive = TRUE)
        all_files <- c(all_files, files)
      }
    }
  }
  
  # Filter for different types of transportation data
  transport_files <- list(
    nhts = grep("nhts|household.*travel|travel.*survey", 
               all_files, value = TRUE, ignore.case = TRUE),
    transit = grep("transit|all.*transit|connectivity|public.*transport", 
                  all_files, value = TRUE, ignore.case = TRUE),
    acs_transport = grep("acs.*transport|commut|vehicle.*household|zero.*vehicle", 
                       all_files, value = TRUE, ignore.case = TRUE)
  )
  
  # Sort by modification time (newest first)
  for (type in names(transport_files)) {
    if (length(transport_files[[type]]) > 0) {
      file_info <- file.info(transport_files[[type]])
      transport_files[[type]] <- transport_files[[type]][order(file_info$mtime, decreasing = TRUE)]
    }
  }
  
  return(transport_files)
}

#' Fetch transportation data
#'
#' Retrieves transportation data from National Household Travel Survey,
#' All Transit Database, and American Community Survey for transportation metrics at the county level.
#'
#' @param years Vector of years to include
#' @param cache_dir Directory to store cache files
#' @param refresh_cache Whether to refresh the cache
#' @param allow_interpolation Whether to interpolate missing values
#' @param data_quality_flags List of standardized data quality flags
#' @param offline_mode Whether to skip all downloads and use only cached data
#' @param parallel Whether to use parallel processing
#' @param parallel_config Optional parallel processing configuration
#' @return A data frame with transportation data for all requested years
fetch_transportation_data <- function(years, 
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
      print_msg("Parallel processing enabled for transportation data")
    } else {
      # Basic parallel setup
      print_msg("Using basic parallel processing setup for transportation data")
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
  cache_file <- file.path(cache_dir, "transportation_data.rds")
  
  # Use cache if available and not refreshing
  if (!refresh_cache && file.exists(cache_file)) {
    print_msg("Loading cached transportation data...")
    transportation_data <- readRDS(cache_file)
    
    # Check if all requested years are in the cache
    cached_years <- unique(transportation_data$year)
    missing_years <- setdiff(years, cached_years)
    
    # Check for empty cache with just placeholder data
    if (nrow(transportation_data) <= 1 || 
        (is.data.frame(transportation_data) && "data_source" %in% names(transportation_data) && 
         any(grepl("SIMULATED", transportation_data$data_source)))) {
      print_msg("Cached transportation data appears to be empty or a placeholder. Will process files again.")
      # Force refresh by continuing past this point
    } else if (length(missing_years) == 0) {
      print_msg("Using complete cached transportation data.")
      return(transportation_data %>% filter(year %in% years))
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
  data_dir <- "data/transportation"
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
    print_msg(paste("Created transportation data directory at:", data_dir))
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
  
  # Function to get National Household Travel Survey data
  get_nhts_data <- function() {
    # Find available local NHTS files
    local_files <- find_local_transportation_files()
    nhts_files <- local_files$nhts
    
    # NHTS is conducted periodically (2001, 2009, 2017)
    # We need to map these to the years in our range
    
    # Define the NHTS survey years
    nhts_years <- c(2001, 2009, 2017)
    
    # Map requested years to nearest NHTS survey
    year_mapping <- sapply(years, function(y) {
      nhts_years[which.min(abs(nhts_years - y))]
    })
    
    # Unique NHTS years needed
    unique_nhts_years <- unique(year_mapping)
    
    # NHTS data list to store results
    nhts_data_list <- list()
    
    # Process each NHTS survey year
    for (nhts_year in unique_nhts_years) {
      # Check for existing files that match this year
      year_pattern <- paste0("nhts.*", nhts_year, "|", nhts_year, ".*nhts|travel.*survey.*", nhts_year)
      existing_files <- grep(year_pattern, nhts_files, value = TRUE)
      
      # Default file path if we need to download
      nhts_file <- file.path(data_dir, paste0("nhts_", nhts_year, ".csv"))
      
      # Check if we need to download
      need_download <- length(existing_files) == 0 || refresh_cache
      
      # Use existing file if available
      if (length(existing_files) > 0 && !need_download) {
        nhts_file <- existing_files[1]  # Use the first (newest) file
        print_msg(paste("Using existing NHTS file for", nhts_year, ":", basename(nhts_file)))
      } else if (need_download && !offline_mode) {
        # NHTS data requires registration and download from their website
        # URLs change for each survey, so we'll provide placeholder URL structure
        nhts_url <- paste0(
          "https://nhts.ornl.gov/assets/", 
          nhts_year, 
          "/download/CountyLevel.csv"
        )
        
        # Make sure the directory exists
        if (!dir.exists(dirname(nhts_file))) {
          dir.create(dirname(nhts_file), recursive = TRUE)
        }
        
        # Try to download
        success <- safe_download(nhts_url, nhts_file, paste("NHTS data for", nhts_year))
        
        if (!success) {
          print_msg(paste("Could not download NHTS data for", nhts_year))
          
          # Try to find any NHTS files for any year
          if (length(nhts_files) > 0) {
            # If we have any NHTS files, use the newest one available
            nhts_file <- nhts_files[1]
            found_year <- as.numeric(regmatches(basename(nhts_file), regexpr("\\d{4}", basename(nhts_file)))[1])
            if (!is.na(found_year)) {
              print_msg(paste("Using available NHTS data from", found_year, "as fallback"))
            } else {
              print_msg(paste("Using available NHTS file:", basename(nhts_file)))
            }
          } else {
            print_msg("No NHTS data files found")
            next
          }
        }
      } else if (offline_mode && need_download) {
        print_msg(paste("Offline mode: Cannot download NHTS data for", nhts_year))
        
        # Look for any available NHTS files
        if (length(nhts_files) > 0) {
          nhts_file <- nhts_files[1]
          found_year <- as.numeric(regmatches(basename(nhts_file), regexpr("\\d{4}", basename(nhts_file)))[1])
          if (!is.na(found_year)) {
            print_msg(paste("Using available NHTS data from", found_year))
          } else {
            print_msg(paste("Using available NHTS file:", basename(nhts_file)))
          }
        } else {
          print_msg("No NHTS data files found in offline mode")
          next
        }
      }
      
      # Process the data if file exists
      if (file.exists(nhts_file)) {
        print_msg(paste("Reading NHTS data from", basename(nhts_file)))
        
        # Read the file
        tryCatch({
          # Determine file type and read accordingly
          file_ext <- tolower(tools::file_ext(nhts_file))
          
          if (file_ext == "csv") {
            nhts_data <- read_csv(nhts_file, show_col_types = FALSE)
          } else if (file_ext %in% c("xlsx", "xls")) {
            nhts_data <- read_excel(nhts_file)
          } else if (file_ext == "txt") {
            # Try to determine delimiter
            nhts_data <- read_delim(nhts_file, delim = "\t", show_col_types = FALSE)
          } else {
            print_msg(paste("Unsupported file format for", basename(nhts_file)))
            next
          }
          
          # Get column names
          print_msg(paste("NHTS data has", ncol(nhts_data), "columns and", nrow(nhts_data), "rows"))
          
          # Check for FIPS/GEOID column
          geoid_col <- grep("FIPS|fips|geoid|GEOID|county.*code|COUNTY.*CODE", names(nhts_data), value = TRUE)[1]
          
          if (is.na(geoid_col)) {
            print_msg("Could not identify GEOID column in NHTS data")
            next
          }
          
          # Rename and format GEOID
          nhts_data <- nhts_data %>%
            rename(GEOID = all_of(geoid_col)) %>%
            mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
          
          # Find columns for our variables of interest
          
          # Vehicle miles traveled
          vmt_col <- grep("vmt|VMT|vehicle.*mile|VEHICLE.*MILE", names(nhts_data), value = TRUE)[1]
          
          # Transportation cost burden
          cost_col <- grep("cost|Cost|COST|expense|Expense|EXPENSE", names(nhts_data), value = TRUE)[1]
          
          print_msg(paste("Found columns: VMT:", !is.na(vmt_col),
                        "Cost burden:", !is.na(cost_col)))
          
          # Create data frame for this survey year
          nhts_year_data <- data.frame(
            GEOID = nhts_data$GEOID
          )
          
          # Add vehicle miles traveled if available
          if (!is.na(vmt_col)) {
            nhts_year_data$vehicle_miles_traveled_per_capita <- nhts_data[[vmt_col]]
          }
          
          # Add transportation cost burden if available
          if (!is.na(cost_col)) {
            nhts_year_data$transportation_cost_burden_pct <- nhts_data[[cost_col]]
          }
          
          # Map this NHTS data to all corresponding years
          for (year in years[year_mapping == nhts_year]) {
            year_data <- nhts_year_data %>%
              mutate(year = year)
            
            # Add quality flags based on whether this is the exact NHTS year or interpolated
            quality_level <- if (year == nhts_year) data_quality_flags$direct else data_quality_flags$interpolated
            
            # Add quality flags for all variables
            if (!is.na(vmt_col)) {
              year_data$vehicle_miles_traveled_per_capita_data_quality <- quality_level
              year_data$vehicle_miles_traveled_per_capita_data_source <- "National Household Travel Survey"
              year_data$vehicle_miles_traveled_per_capita_data_vintage <- as.character(nhts_year)
            }
            
            if (!is.na(cost_col)) {
              year_data$transportation_cost_burden_pct_data_quality <- quality_level
              year_data$transportation_cost_burden_pct_data_source <- "National Household Travel Survey"
              year_data$transportation_cost_burden_pct_data_vintage <- as.character(nhts_year)
            }
            
            # Add to list
            nhts_data_list[[as.character(year)]] <- year_data
          }
          
          print_msg(paste("Processed NHTS data for", nhts_year))
        }, error = function(e) {
          print_msg(paste("Error reading NHTS data for", nhts_year, ":", conditionMessage(e)))
        })
      }
    }
    
    # Combine all years
    if (length(nhts_data_list) > 0) {
      combined_nhts <- bind_rows(nhts_data_list)
      print_msg(paste("Combined NHTS data with", nrow(combined_nhts), "rows"))
      return(combined_nhts)
    } else {
      print_msg("No NHTS data processed successfully")
      return(NULL)
    }
  }
  
  # Function to get All Transit Database data
  get_transit_data <- function() {
    # Find available local transit files
    local_files <- find_local_transportation_files()
    transit_files <- local_files$transit
    
    # All Transit Database data is available from 2012 onwards
    # We'll try to get data for each year in the requested range
    
    # Define variables we want to extract
    transit_variables <- c(
      "transit_connectivity_index" = "Measure of transit connectivity",
      "transit_access_jobs" = "Number of jobs accessible by transit within 30 minutes",
      "transit_performance_index" = "Composite measure of transit performance"
    )
    
    # Transit data list to store results
    transit_data_list <- list()
    
    # Limit to years 2012 and later
    transit_years <- years[years >= 2012]
    
    for (year in transit_years) {
      # Skip future years
      if (year > as.integer(format(Sys.Date(), "%Y"))) {
        next
      }
      
      # Check for existing files that match this year
      year_pattern <- paste0("transit.*", year, "|", year, ".*transit|connectivity.*", year)
      existing_files <- grep(year_pattern, transit_files, value = TRUE)
      
      # Default file path if we need to download
      transit_file <- file.path(data_dir, paste0("transit_", year, ".csv"))
      
      # Check if we need to download
      need_download <- length(existing_files) == 0 || refresh_cache
      
      # Use existing file if available
      if (length(existing_files) > 0 && !need_download) {
        transit_file <- existing_files[1]  # Use the first (newest) file
        print_msg(paste("Using existing transit file for", year, ":", basename(transit_file)))
      } else if (need_download && !offline_mode) {
        # All Transit URL - placeholder structure
        transit_url <- paste0(
          "https://alltransit.cnt.org/data/download/counties_", 
          year, 
          ".csv"
        )
        
        # Make sure the directory exists
        if (!dir.exists(dirname(transit_file))) {
          dir.create(dirname(transit_file), recursive = TRUE)
        }
        
        # Try to download
        success <- safe_download(transit_url, transit_file, paste("All Transit data for", year))
        
        if (!success) {
          print_msg(paste("Could not download All Transit data for", year))
          
          # Try to find any transit files for any year
          if (length(transit_files) > 0) {
            # If we have any transit files, use the newest one available
            transit_file <- transit_files[1]
            found_year <- as.numeric(regmatches(basename(transit_file), regexpr("\\d{4}", basename(transit_file)))[1])
            if (!is.na(found_year)) {
              print_msg(paste("Using available transit data from", found_year, "as fallback"))
            } else {
              print_msg(paste("Using available transit file:", basename(transit_file)))
            }
          } else {
            print_msg("No transit data files found")
            next
          }
        }
      } else if (offline_mode && need_download) {
        print_msg(paste("Offline mode: Cannot download transit data for", year))
        
        # Look for any available transit files
        if (length(transit_files) > 0) {
          transit_file <- transit_files[1]
          found_year <- as.numeric(regmatches(basename(transit_file), regexpr("\\d{4}", basename(transit_file)))[1])
          if (!is.na(found_year)) {
            print_msg(paste("Using available transit data from", found_year))
          } else {
            print_msg(paste("Using available transit file:", basename(transit_file)))
          }
        } else {
          print_msg("No transit data files found in offline mode")
          next
        }
      }
      
      # Process the data if file exists
      if (file.exists(transit_file)) {
        print_msg(paste("Reading transit data from", basename(transit_file)))
        
        # Read the file
        tryCatch({
          # Determine file type and read accordingly
          file_ext <- tolower(tools::file_ext(transit_file))
          
          if (file_ext == "csv") {
            transit_data <- read_csv(transit_file, show_col_types = FALSE)
          } else if (file_ext %in% c("xlsx", "xls")) {
            transit_data <- read_excel(transit_file)
          } else if (file_ext == "txt") {
            # Try to determine delimiter
            transit_data <- read_delim(transit_file, delim = "\t", show_col_types = FALSE)
          } else {
            print_msg(paste("Unsupported file format for", basename(transit_file)))
            next
          }
          
          # Get column names
          print_msg(paste("Transit data has", ncol(transit_data), "columns and", nrow(transit_data), "rows"))
          
          # Check for GEOID column
          geoid_col <- grep("FIPS|fips|geoid|GEOID|county.*code|COUNTY.*CODE", names(transit_data), value = TRUE)[1]
          
          if (is.na(geoid_col)) {
            print_msg("Could not identify GEOID column in Transit data")
            next
          }
          
          # Rename and format GEOID
          transit_data <- transit_data %>%
            rename(GEOID = all_of(geoid_col)) %>%
            mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
          
          # Find columns for our variables of interest
          
          # Connectivity index
          connectivity_col <- grep("connect|Connect|CONNECT", names(transit_data), value = TRUE)[1]
          
          # Jobs access
          jobs_col <- grep("jobs|Jobs|JOBS|employment|Employment", names(transit_data), value = TRUE)[1]
          
          # Performance index
          performance_col <- grep("perform|Perform|PERFORM", names(transit_data), value = TRUE)[1]
          
          print_msg(paste("Found columns: Connectivity:", !is.na(connectivity_col),
                        "Jobs access:", !is.na(jobs_col),
                        "Performance:", !is.na(performance_col)))
          
          # Create data frame for this year
          year_data <- data.frame(
            GEOID = transit_data$GEOID,
            year = year
          )
          
          # Add connectivity index if available
          if (!is.na(connectivity_col)) {
            year_data$transit_connectivity_index <- transit_data[[connectivity_col]]
            year_data$transit_connectivity_index_data_quality <- data_quality_flags$direct
            year_data$transit_connectivity_index_data_source <- "All Transit Database"
            year_data$transit_connectivity_index_data_vintage <- as.character(year)
          }
          
          # Add jobs access if available
          if (!is.na(jobs_col)) {
            year_data$transit_access_jobs <- transit_data[[jobs_col]]
            year_data$transit_access_jobs_data_quality <- data_quality_flags$direct
            year_data$transit_access_jobs_data_source <- "All Transit Database"
            year_data$transit_access_jobs_data_vintage <- as.character(year)
          }
          
          # Add performance index if available
          if (!is.na(performance_col)) {
            year_data$transit_performance_index <- transit_data[[performance_col]]
            year_data$transit_performance_index_data_quality <- data_quality_flags$direct
            year_data$transit_performance_index_data_source <- "All Transit Database"
            year_data$transit_performance_index_data_vintage <- as.character(year)
          }
          
          # Add to list
          transit_data_list[[as.character(year)]] <- year_data
          
          print_msg(paste("Processed All Transit data for", year))
        }, error = function(e) {
          print_msg(paste("Error reading All Transit data for", year, ":", conditionMessage(e)))
        })
      }
    }
    
    # Combine all years
    if (length(transit_data_list) > 0) {
      combined_transit <- bind_rows(transit_data_list)
      print_msg(paste("Combined All Transit data with", nrow(combined_transit), "rows"))
      return(combined_transit)
    } else {
      print_msg("No All Transit data processed successfully")
      return(NULL)
    }
  }
  
  # Function to get ACS transportation data
  get_acs_data <- function() {
    # Find available local ACS transportation files
    local_files <- find_local_transportation_files()
    acs_files <- local_files$acs_transport
    
    # ACS has data on zero-vehicle households and commute metrics
    # Available from 2005 onwards
    
    # Define variables we want to extract
    acs_variables <- c(
      "zero_vehicle_households_pct" = "Percentage of households with no vehicles",
      "public_transit_trips_per_capita" = "Public transit trips per capita"
    )
    
    # ACS data list to store results
    acs_data_list <- list()
    
    # Limit to years 2005 and later
    acs_years <- years[years >= 2005]
    
    for (year in acs_years) {
      # Skip future years
      if (year > as.integer(format(Sys.Date(), "%Y"))) {
        next
      }
      
      # Check for existing files that match this year
      year_pattern <- paste0("acs.*transport.*", year, "|", year, ".*acs.*transport|commut.*", year)
      existing_files <- grep(year_pattern, acs_files, value = TRUE)
      
      # Default file path if we need to download
      acs_file <- file.path(data_dir, paste0("acs_transportation_", year, ".csv"))
      
      # Check if we need to download
      need_download <- length(existing_files) == 0 || refresh_cache
      
      # Use existing file if available
      if (length(existing_files) > 0 && !need_download) {
        acs_file <- existing_files[1]  # Use the first (newest) file
        print_msg(paste("Using existing ACS transportation file for", year, ":", basename(acs_file)))
      } else {
        # In a real implementation, we would use Census API
        # This would require a Census API key and proper queries
        print_msg(paste("ACS transportation data file not found for", year))
        
        # Try to find any ACS transportation files for any year
        if (length(acs_files) > 0) {
          # If we have any ACS transportation files, use the newest one available
          acs_file <- acs_files[1]
          found_year <- as.numeric(regmatches(basename(acs_file), regexpr("\\d{4}", basename(acs_file)))[1])
          if (!is.na(found_year)) {
            print_msg(paste("Using available ACS transportation data from", found_year, "as fallback"))
          } else {
            print_msg(paste("Using available ACS transportation file:", basename(acs_file)))
          }
        } else {
          print_msg("No ACS transportation data files found")
          next
        }
      }
      
      # Process the data if file exists
      if (file.exists(acs_file)) {
        print_msg(paste("Reading ACS transportation data from", basename(acs_file)))
        
        # Read the file
        tryCatch({
          # Determine file type and read accordingly
          file_ext <- tolower(tools::file_ext(acs_file))
          
          if (file_ext == "csv") {
            acs_data <- read_csv(acs_file, show_col_types = FALSE)
          } else if (file_ext %in% c("xlsx", "xls")) {
            acs_data <- read_excel(acs_file)
          } else if (file_ext == "txt") {
            # Try to determine delimiter
            acs_data <- read_delim(acs_file, delim = "\t", show_col_types = FALSE)
          } else {
            print_msg(paste("Unsupported file format for", basename(acs_file)))
            next
          }
          
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
          
          # Find columns for our variables of interest
          
          # Zero-vehicle households
          zero_vehicle_col <- grep("no.*vehicle|zero.*vehicle|without.*vehicle", 
                                  names(acs_data), value = TRUE)[1]
          
          # Public transit trips
          transit_trips_col <- grep("transit.*trip|public.*transport.*trip", 
                                   names(acs_data), value = TRUE)[1]
          
          print_msg(paste("Found columns: Zero-vehicle households:", !is.na(zero_vehicle_col),
                        "Transit trips:", !is.na(transit_trips_col)))
          
          # Create data frame for this year
          year_data <- data.frame(
            GEOID = acs_data$GEOID,
            year = year
          )
          
          # Add zero-vehicle households if available
          if (!is.na(zero_vehicle_col)) {
            year_data$zero_vehicle_households_pct <- acs_data[[zero_vehicle_col]]
            year_data$zero_vehicle_households_pct_data_quality <- data_quality_flags$direct
            year_data$zero_vehicle_households_pct_data_source <- "American Community Survey"
            year_data$zero_vehicle_households_pct_data_vintage <- as.character(year)
          }
          
          # Add public transit trips if available
          if (!is.na(transit_trips_col)) {
            year_data$public_transit_trips_per_capita <- acs_data[[transit_trips_col]]
            year_data$public_transit_trips_per_capita_data_quality <- data_quality_flags$direct
            year_data$public_transit_trips_per_capita_data_source <- "American Community Survey"
            year_data$public_transit_trips_per_capita_data_vintage <- as.character(year)
          }
          
          # Add to list
          acs_data_list[[as.character(year)]] <- year_data
          
          print_msg(paste("Processed ACS transportation data for", year))
        }, error = function(e) {
          print_msg(paste("Error reading ACS transportation data for", year, ":", conditionMessage(e)))
        })
      }
    }
    
    # Combine all years
    if (length(acs_data_list) > 0) {
      combined_acs <- bind_rows(acs_data_list)
      print_msg(paste("Combined ACS transportation data with", nrow(combined_acs), "rows"))
      return(combined_acs)
    } else {
      print_msg("No ACS transportation data processed successfully")
      return(NULL)
    }
  }
  
  # Get data from different transportation sources - use parallel processing if enabled
  if (parallel && requireNamespace("future.apply", quietly = TRUE)) {
    print_msg("Using parallel processing to fetch data from multiple transportation sources")
    
    # Define the data sources to fetch
    data_sources <- c("nhts", "transit", "acs")
    
    # Create a function to process one data source
    process_data_source <- function(source) {
      print_msg(paste("Processing transportation data source:", source))
      
      if (source == "nhts") {
        return(get_nhts_data())
      } else if (source == "transit") {
        return(get_transit_data())
      } else if (source == "acs") {
        return(get_acs_data())
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
      transportation_data_sources <- progressr::with_progress({
        p <- progressr::progressor(steps = length(data_sources))
        
        future.apply::future_lapply(data_sources, function(source) {
          result <- process_data_source(source)
          p(message = paste("Processed transportation data source:", source))
          return(result)
        })
      })
    } else {
      # Process without progress tracking
      transportation_data_sources <- future.apply::future_lapply(data_sources, process_data_source)
    }
    
    # Convert results to named list
    names(transportation_data_sources) <- data_sources
    
    # Filter out NULL results
    transportation_data_list <- transportation_data_sources[!sapply(transportation_data_sources, is.null)]
    transportation_data_list <- transportation_data_list[sapply(transportation_data_list, function(x) !is.null(x) && nrow(x) > 0)]
    
  } else {
    # Sequential processing
    print_msg("Using sequential processing to fetch data from multiple transportation sources")
    nhts_data <- get_nhts_data()
    transit_data <- get_transit_data()
    acs_data <- get_acs_data()
    
    # Combine all data sources
    transportation_data_list <- list()
    
    if (!is.null(nhts_data) && nrow(nhts_data) > 0) {
      transportation_data_list[["nhts"]] <- nhts_data
    }
    
    if (!is.null(transit_data) && nrow(transit_data) > 0) {
      transportation_data_list[["transit"]] <- transit_data
    }
    
    if (!is.null(acs_data) && nrow(acs_data) > 0) {
      transportation_data_list[["acs"]] <- acs_data
    }
  }
  
  # Process if we have data
  if (length(transportation_data_list) > 0) {
    # Combine all sources and handle overlapping columns
    # Start with first dataset
    combined_transportation_data <- transportation_data_list[[1]]
    
    # Add each additional dataset
    if (length(transportation_data_list) > 1) {
      for (i in 2:length(transportation_data_list)) {
        next_data <- transportation_data_list[[i]]
        
        # Identify common columns for joining
        join_cols <- intersect(names(combined_transportation_data), names(next_data))
        
        # Ensure we have at least GEOID and year for joining
        if (all(c("GEOID", "year") %in% join_cols)) {
          # Find columns that might overlap (excluding join columns and flags)
          overlap_cols <- setdiff(
            intersect(names(combined_transportation_data), names(next_data)),
            c(join_cols, grep("_data_quality$|_data_source$|_data_vintage$", 
                             names(next_data), value = TRUE))
          )
          
          if (length(overlap_cols) > 0) {
            # For overlapping columns, prefer data from the current dataset
            # Remove those columns from the combined data before joining
            combined_transportation_data <- combined_transportation_data %>%
              select(-all_of(overlap_cols))
          }
          
          # Join with the next dataset
          combined_transportation_data <- full_join(
            combined_transportation_data, 
            next_data, 
            by = join_cols
          )
        } else {
          print_msg("Warning: Cannot join datasets - missing common keys")
        }
      }
    }
    
    # Check if we need to handle missing years
    available_years <- unique(combined_transportation_data$year)
    missing_years <- setdiff(years, available_years)
    
    if (length(missing_years) > 0 && allow_interpolation) {
      print_msg(paste("Interpolating data for missing years:", paste(missing_years, collapse=", ")))
      
      # Get all variables except join columns and metadata columns
      measure_cols <- setdiff(
        names(combined_transportation_data),
        c("GEOID", "year", grep("_data_quality$|_data_source$|_data_vintage$", 
                               names(combined_transportation_data), value = TRUE))
      )
      
      # Process each county separately for interpolation
      counties <- unique(combined_transportation_data$GEOID)
      
      # Define function to interpolate a single county
      interpolate_county <- function(county) {
        # Get data for this county
        county_data <- combined_transportation_data %>%
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
                         names(combined_transportation_data), value = TRUE)
        
        for (col in meta_cols) {
          if (!(col %in% names(interp_data))) {
            interp_data[[col]] <- NA
          }
        }
        
        # Use the interpolated data instead
        combined_transportation_data <- interp_data
      }
    } else if (length(missing_years) > 0) {
      print_msg("Missing years but interpolation not allowed. Years will remain missing.")
    }
    
    # Filter to just the requested years
    combined_transportation_data <- combined_transportation_data %>%
      filter(year %in% years)
    
    # Sort data
    combined_transportation_data <- combined_transportation_data %>%
      arrange(GEOID, year)
    
    # Cache the processed data
    saveRDS(combined_transportation_data, cache_file)
    print_msg(paste("Cached transportation data to:", cache_file))
    
    return(combined_transportation_data)
  } else {
    # No data available - create empty dataset with NAs and provide clear error messages
    print_msg("No transportation data available and simulation not allowed. Creating empty dataset with NAs.")
    
    # Get variable list for transportation variables
    transportation_vars <- c(
      "vehicle_miles_traveled_per_capita",
      "transportation_cost_burden_pct",
      "zero_vehicle_households_pct",
      "public_transit_trips_per_capita",
      "transit_connectivity_index",
      "transit_access_jobs",
      "transit_performance_index"
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
    
    # Add NAME column
    grid$NAME <- counties$NAME[match(grid$GEOID, counties$GEOID)]
    
    # Add empty variable columns with NAs
    for (var in transportation_vars) {
      grid[[var]] <- NA_real_
      grid[[paste0(var, "_data_quality")]] <- data_quality_flags$missing
      grid[[paste0(var, "_data_source")]] <- "NOT_AVAILABLE"
      grid[[paste0(var, "_data_vintage")]] <- NA_character_
    }
    
    transportation_data <- as_tibble(grid)
    print_msg(paste("Created empty transportation dataset with", nrow(transportation_data), "rows"))
    
    # Cache the empty data
    saveRDS(transportation_data, cache_file)
    print_msg(paste("Cached empty transportation data to:", cache_file))
    
    return(transportation_data)
  }
}

# This lets the function be used when the script is sourced
is_sourced <- function() {
  return(!identical(environment(), globalenv()))
}

# If the script is run directly, test the function
if (!is_sourced()) {
  cat("Testing transportation data fetcher...\n")
  
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
    cat("Using parallel processing for transportation data fetching test\n")
  } else {
    cat("Parallel processing dependencies not available, using sequential processing\n")
  }
  
  # Test the function
  result <- fetch_transportation_data(
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