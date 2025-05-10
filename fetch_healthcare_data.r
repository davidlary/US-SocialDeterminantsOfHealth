#!/usr/bin/env Rscript

# Healthcare Access Data Fetcher
# This script handles retrieval of healthcare access data from HRSA and CMS sources

library(tidyverse)
library(httr)
library(jsonlite)
library(readxl)
library(lubridate)
library(sf)
library(zoo) # For interpolation if needed

#' Fetch healthcare access data
#'
#' Retrieves healthcare access data from the HRSA Area Health Resources Files
#' and CMS Geographic Variation data sources at the county level.
#'
#' @param years Vector of years to include
#' @param cache_dir Directory to store cache files
#' @param refresh_cache Whether to refresh the cache
#' @param allow_interpolation Whether to interpolate missing years from available data
#' @param data_quality_flags List of flags for data quality tracking
#' @param offline_mode If TRUE, will only use cached data without attempting downloads
#' @param parallel Whether to use parallel processing
#' @param parallel_config Optional parallel processing configuration
#' @return A data frame with healthcare access data for all requested years
fetch_healthcare_data <- function(years, 
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
      print_msg("Parallel processing enabled for healthcare data")
    } else {
      # Basic parallel setup
      print_msg("Using basic parallel processing setup for healthcare data")
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
  cache_file <- file.path(cache_dir, "healthcare_access_data.rds")
  
  # Use cache if available and not refreshing
  if (!refresh_cache && file.exists(cache_file)) {
    print_msg("Loading cached healthcare access data...")
    healthcare_data <- readRDS(cache_file)
    
    # Check if all requested years are in the cache
    cached_years <- unique(healthcare_data$year)
    missing_years <- setdiff(years, cached_years)
    
    # Check for empty cache with just placeholder data
    if (nrow(healthcare_data) <= 1 || 
        (is.data.frame(healthcare_data) && 
         any(sapply(names(healthcare_data), function(col) {
           if (grepl("_data_source$", col)) {
             return(any(grepl("SIMULATED|NO_DATA_AVAILABLE", healthcare_data[[col]])))
           }
           return(FALSE)
         })))) {
      print_msg("Cached healthcare data appears to be empty or a placeholder. Will process files again.")
      # Force refresh by continuing past this point
    } else if (length(missing_years) == 0) {
      print_msg("Using complete cached healthcare access data.")
      return(healthcare_data %>% filter(year %in% years))
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
  data_dir <- "data/healthcare"
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
    print_msg(paste("Created healthcare data directory at:", data_dir))
  }
  
  # Function to find local healthcare data files
  find_local_healthcare_files <- function() {
    # List of directories to check
    healthcare_dirs <- c(
      "data/healthcare",
      "data/cache/healthcare",
      "data/health",
      "data/medical"
    )
    
    # List of possible file extensions
    file_exts <- c("\\.csv$", "\\.xlsx$", "\\.xls$", "\\.zip$", "\\.txt$")
    
    # Search for files
    all_files <- list()
    for (dir in healthcare_dirs) {
      if (dir.exists(dir)) {
        for (ext in file_exts) {
          files <- list.files(dir, pattern = ext, full.names = TRUE, recursive = TRUE)
          if (length(files) > 0) {
            # Get file info with modification times
            file_info <- file.info(files)
            file_info$path <- rownames(file_info)
            all_files[[paste(dir, ext, sep = "_")]] <- file_info
          }
        }
      }
    }
    
    # Combine all files and sort by recency
    if (length(all_files) > 0) {
      all_file_info <- bind_rows(all_files)
      all_file_info <- all_file_info[order(all_file_info$mtime, decreasing = TRUE), ]
      all_paths <- all_file_info$path
    } else {
      all_paths <- character(0)
    }
    
    # Filter for different types of healthcare data
    healthcare_files <- list(
      ahrf = grep("ahrf|area.*health.*resource|health.*resource.*file", 
                 all_paths, value = TRUE, ignore.case = TRUE),
      cms = grep("cms|medicare|medicaid|geographic.*variation", 
                all_paths, value = TRUE, ignore.case = TRUE),
      aha = grep("aha|hospital.*association", 
                all_paths, value = TRUE, ignore.case = TRUE),
      cdc = grep("cdc|wonder|places|500.*cities", 
                all_paths, value = TRUE, ignore.case = TRUE)
    )
    
    return(healthcare_files)
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
  
  # Function to get HRSA Area Health Resources Files data
  get_ahrf_data <- function() {
    # AHRF data is available as an annual release
    # Each release contains multiple years of data in a single file
    # We'll try to get the most recent release
    
    # Define the variables we want to extract
    ahrf_variables <- c(
      "primary_care_physicians_per_100k" = "primary_care_physicians",
      "mental_health_providers_per_100k" = "mental_health_providers",
      "dentists_per_100k" = "dentists",
      "hospital_beds_per_1000" = "hospital_beds",
      "fqhc_access_pct" = "fqhc_access",
      "pharmacies_per_100k" = "pharmacies",
      "preventable_hospital_stays" = "preventable_hospital_stays"
    )
    
    # Find local healthcare files
    local_files <- find_local_healthcare_files()
    ahrf_files <- local_files$ahrf
    
    print_msg(paste("Found", length(ahrf_files), "potential AHRF data files"))
    
    # Check if we have any local AHRF files
    if (length(ahrf_files) > 0) {
      # Use the most recent file
      ahrf_file <- ahrf_files[1]
      print_msg(paste("Using most recent AHRF file:", ahrf_file))
    } else {
      # Default file path if we need to download
      ahrf_file <- file.path(data_dir, "ahrf_current.csv")
      
      # Check if we need to download the file
      need_download <- !file.exists(ahrf_file) || refresh_cache
      
      if (need_download) {
      # HRSA AHRF is typically available for download via a form
      # Here we're using a direct URL which may change, so we have multiple fallbacks
      urls <- c(
        # These URLs are examples and would need to be updated regularly
        "https://data.hrsa.gov/DataDownload/AHRF/AHRF_2022-2023.ZIP",
        "https://data.hrsa.gov/data/download/AHRF_SAS/ArchivedAHRF/ahrf2022.ZIP",
        "https://data.hrsa.gov/DataDownload/AHRF/AHRF_2021-2022.ZIP"
      )
      
      # Try each URL until one works
      download_success <- FALSE
      for (url in urls) {
        zip_file <- file.path(data_dir, "ahrf_current.zip")
        if (safe_download(url, zip_file, "HRSA AHRF data")) {
          # Try to unzip the file
          tryCatch({
            # Unzip the file
            unzip(zip_file, exdir = file.path(data_dir, "ahrf_temp"))
            
            # Find the data file (usually a CSV, SAS, or XLSX file)
            csv_files <- list.files(file.path(data_dir, "ahrf_temp"), 
                                   pattern = "\\.csv$|\\.CSV$", 
                                   full.names = TRUE, 
                                   recursive = TRUE)
            
            if (length(csv_files) > 0) {
              # Use the first CSV file found
              file.copy(csv_files[1], ahrf_file, overwrite = TRUE)
              download_success <- TRUE
              
              # Clean up temp directory
              unlink(file.path(data_dir, "ahrf_temp"), recursive = TRUE)
              
              break # Exit the URL loop
            } else {
              # Try looking for Excel files
              excel_files <- list.files(file.path(data_dir, "ahrf_temp"), 
                                      pattern = "\\.xls$|\\.xlsx$", 
                                      full.names = TRUE, 
                                      recursive = TRUE)
              
              if (length(excel_files) > 0) {
                # Convert Excel to CSV
                tryCatch({
                  excel_data <- readxl::read_excel(excel_files[1])
                  write_csv(excel_data, ahrf_file)
                  download_success <- TRUE
                  
                  # Clean up temp directory
                  unlink(file.path(data_dir, "ahrf_temp"), recursive = TRUE)
                  
                  break # Exit the URL loop
                }, error = function(e) {
                  print_msg(paste("Error converting Excel to CSV:", conditionMessage(e)))
                })
              }
            }
          }, error = function(e) {
            print_msg(paste("Error unzipping AHRF file:", conditionMessage(e)))
          })
        }
      }
      
      if (!download_success) {
        print_msg("Could not download or extract AHRF data from any URL")
      }
      } else {
        print_msg(paste("Using existing AHRF file:", ahrf_file))
      }
    }
    
    # Process the data if file exists
    if (file.exists(ahrf_file)) {
      print_msg("Reading AHRF data...")
      
      # Read the file
      tryCatch({
        ahrf_data <- read_csv(ahrf_file, show_col_types = FALSE)
        
        # Get column names
        print_msg(paste("AHRF data has", ncol(ahrf_data), "columns and", nrow(ahrf_data), "rows"))
        
        # Check if this looks like AHRF data
        # AHRF typically has FIPS codes in columns named "FIPS" or similar
        fips_col <- grep("FIPS|fips|^F_", names(ahrf_data), value = TRUE)[1]
        if (is.na(fips_col)) {
          # Try state and county code columns
          state_col <- grep("STATE|state", names(ahrf_data), value = TRUE)[1]
          county_col <- grep("COUNTY|county", names(ahrf_data), value = TRUE)[1]
          
          if (!is.na(state_col) && !is.na(county_col)) {
            # Create FIPS from state and county codes
            ahrf_data <- ahrf_data %>%
              mutate(GEOID = sprintf("%02d%03d", 
                                    as.numeric(!!sym(state_col)),
                                    as.numeric(!!sym(county_col))))
          } else {
            print_msg("Could not identify FIPS or state/county columns in AHRF data")
            return(NULL)
          }
        } else {
          # Rename the FIPS column to GEOID
          ahrf_data <- ahrf_data %>%
            rename(GEOID = all_of(fips_col)) %>%
            # Ensure GEOID is properly formatted (5 digits with leading zeros)
            mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
        }
        
        # AHRF data is typically in wide format with one row per county and one column per variable-year
        # We need to identify the columns for our variables of interest
        
        # Identify columns for primary care physicians
        pcp_cols <- grep("PHYS_PC|PC_PHYS|MD_PC|DOCS_PC", 
                        names(ahrf_data), value = TRUE)
        
        # Identify columns for mental health providers
        mh_cols <- grep("MH_PROV|PSYCH|MH_WORK", 
                       names(ahrf_data), value = TRUE)
        
        # Identify columns for dentists
        dent_cols <- grep("DENT", 
                         names(ahrf_data), value = TRUE)
        
        # Identify columns for hospital beds
        hosp_cols <- grep("HOSP_BED|BED_", 
                         names(ahrf_data), value = TRUE)
        
        # Identify columns for FQHCs
        fqhc_cols <- grep("FQHC|CHC", 
                         names(ahrf_data), value = TRUE)
        
        # Identify columns for pharmacies
        pharm_cols <- grep("PHARM", 
                          names(ahrf_data), value = TRUE)
        
        # Identify columns for preventable hospital stays
        prev_cols <- grep("PREV_HOSP|AMBULATORY|ACSC", 
                         names(ahrf_data), value = TRUE)
        
        print_msg(paste("Found columns for our variables:",
                      "PCP:", length(pcp_cols),
                      "MH:", length(mh_cols),
                      "Dentists:", length(dent_cols),
                      "Hospital beds:", length(hosp_cols),
                      "FQHCs:", length(fqhc_cols),
                      "Pharmacies:", length(pharm_cols),
                      "Preventable stays:", length(prev_cols)))
        
        # Extract year from column names
        # AHRF columns typically have year indicators in them (e.g., DOCS_PC_17 for 2017)
        # We need to create a mapping of column names to years
        
        extract_year <- function(col_names) {
          # Create a mapping of column names to years and variable types
          result <- list()
          
          for (col in col_names) {
            # Try to find a year pattern in the column name
            # Common patterns: _YY, _YYYY, YY_, YYYY_
            year_match <- str_match(col, "_(\\d{2})$|_(\\d{4})$|(\\d{2})_|(\\d{4})_")
            
            if (!is.na(year_match[1])) {
              # Extract the year part
              year_part <- na.omit(year_match[2:length(year_match)])[1]
              
              # Convert 2-digit year to 4-digit
              if (nchar(year_part) == 2) {
                year <- as.numeric(year_part)
                if (year < 50) {
                  year <- 2000 + year
                } else {
                  year <- 1900 + year
                }
              } else {
                year <- as.numeric(year_part)
              }
              
              result[[col]] <- year
            }
          }
          
          return(result)
        }
        
        # Extract years for each variable type
        pcp_years <- extract_year(pcp_cols)
        mh_years <- extract_year(mh_cols)
        dent_years <- extract_year(dent_cols)
        hosp_years <- extract_year(hosp_cols)
        fqhc_years <- extract_year(fqhc_cols)
        pharm_years <- extract_year(pharm_cols)
        prev_years <- extract_year(prev_cols)
        
        print_msg(paste("Extracted years for variables:",
                      "PCP:", length(pcp_years),
                      "MH:", length(mh_years),
                      "Dentists:", length(dent_years),
                      "Hospital beds:", length(hosp_years),
                      "FQHCs:", length(fqhc_years),
                      "Pharmacies:", length(pharm_years),
                      "Preventable stays:", length(prev_years)))
        
        # Create a list to store data for each year
        yearly_data <- list()
        
        # Process each requested year
        for (year in years) {
          # Create a data frame for this year
          year_data <- data.frame(
            GEOID = ahrf_data$GEOID,
            year = year
          )
          
          # Add each variable if data is available for this year
          
          # Primary care physicians
          pcp_col <- names(pcp_years)[pcp_years == year]
          if (length(pcp_col) > 0) {
            year_data$primary_care_physicians_per_100k <- 
              as.numeric(ahrf_data[[pcp_col[1]]]) * 100000 / 
              as.numeric(ahrf_data$POP_ESTIMATE)
            
            year_data$primary_care_physicians_per_100k_data_quality <- data_quality_flags$direct
            year_data$primary_care_physicians_per_100k_data_source <- "HRSA AHRF"
            year_data$primary_care_physicians_per_100k_data_vintage <- as.character(year)
          }
          
          # Mental health providers
          mh_col <- names(mh_years)[mh_years == year]
          if (length(mh_col) > 0) {
            year_data$mental_health_providers_per_100k <- 
              as.numeric(ahrf_data[[mh_col[1]]]) * 100000 / 
              as.numeric(ahrf_data$POP_ESTIMATE)
            
            year_data$mental_health_providers_per_100k_data_quality <- data_quality_flags$direct
            year_data$mental_health_providers_per_100k_data_source <- "HRSA AHRF"
            year_data$mental_health_providers_per_100k_data_vintage <- as.character(year)
          }
          
          # Dentists
          dent_col <- names(dent_years)[dent_years == year]
          if (length(dent_col) > 0) {
            year_data$dentists_per_100k <- 
              as.numeric(ahrf_data[[dent_col[1]]]) * 100000 / 
              as.numeric(ahrf_data$POP_ESTIMATE)
            
            year_data$dentists_per_100k_data_quality <- data_quality_flags$direct
            year_data$dentists_per_100k_data_source <- "HRSA AHRF"
            year_data$dentists_per_100k_data_vintage <- as.character(year)
          }
          
          # Hospital beds
          hosp_col <- names(hosp_years)[hosp_years == year]
          if (length(hosp_col) > 0) {
            year_data$hospital_beds_per_1000 <- 
              as.numeric(ahrf_data[[hosp_col[1]]]) * 1000 / 
              as.numeric(ahrf_data$POP_ESTIMATE)
            
            year_data$hospital_beds_per_1000_data_quality <- data_quality_flags$direct
            year_data$hospital_beds_per_1000_data_source <- "HRSA AHRF"
            year_data$hospital_beds_per_1000_data_vintage <- as.character(year)
          }
          
          # FQHCs
          fqhc_col <- names(fqhc_years)[fqhc_years == year]
          if (length(fqhc_col) > 0) {
            year_data$fqhc_access_pct <- 
              as.numeric(ahrf_data[[fqhc_col[1]]]) * 100 / 
              as.numeric(ahrf_data$POP_ESTIMATE)
            
            year_data$fqhc_access_pct_data_quality <- data_quality_flags$direct
            year_data$fqhc_access_pct_data_source <- "HRSA AHRF"
            year_data$fqhc_access_pct_data_vintage <- as.character(year)
          }
          
          # Pharmacies
          pharm_col <- names(pharm_years)[pharm_years == year]
          if (length(pharm_col) > 0) {
            year_data$pharmacies_per_100k <- 
              as.numeric(ahrf_data[[pharm_col[1]]]) * 100000 / 
              as.numeric(ahrf_data$POP_ESTIMATE)
            
            year_data$pharmacies_per_100k_data_quality <- data_quality_flags$direct
            year_data$pharmacies_per_100k_data_source <- "HRSA AHRF"
            year_data$pharmacies_per_100k_data_vintage <- as.character(year)
          }
          
          # Preventable hospital stays
          prev_col <- names(prev_years)[prev_years == year]
          if (length(prev_col) > 0) {
            year_data$preventable_hospital_stays <- 
              as.numeric(ahrf_data[[prev_col[1]]])
            
            year_data$preventable_hospital_stays_data_quality <- data_quality_flags$direct
            year_data$preventable_hospital_stays_data_source <- "HRSA AHRF"
            year_data$preventable_hospital_stays_data_vintage <- as.character(year)
          }
          
          # Add to list
          yearly_data[[as.character(year)]] <- year_data
        }
        
        # Combine all years
        if (length(yearly_data) > 0) {
          combined_ahrf <- bind_rows(yearly_data)
          print_msg(paste("Combined AHRF data with", nrow(combined_ahrf), "rows"))
          return(combined_ahrf)
        } else {
          print_msg("No AHRF data processed successfully")
          return(NULL)
        }
      }, error = function(e) {
        print_msg(paste("Error reading AHRF data:", conditionMessage(e)))
        return(NULL)
      })
    } else {
      print_msg("AHRF data file not found")
      return(NULL)
    }
  }
  
  # Function to get CMS Geographic Variation data
  get_cms_data <- function() {
    # CMS Geographic Variation data is available for recent years
    # We'll try to get data for each year in the requested range
    
    # Define variables we want to extract
    cms_variables <- c(
      "medicare_spending_per_beneficiary" = "Medicare spending per beneficiary",
      "preventive_services_pct" = "Percentage receiving preventive services",
      "ambulatory_care_sensitive_conditions" = "Ambulatory care sensitive conditions"
    )
    
    # Find local CMS files
    local_files <- find_local_healthcare_files()
    cms_files <- local_files$cms
    
    print_msg(paste("Found", length(cms_files), "potential CMS data files"))
    
    # Check for files that match year patterns
    year_specific_files <- list()
    for (year in years) {
      # Skip future years
      if (year > as.integer(format(Sys.Date(), "%Y"))) {
        next
      }
      
      # Look for files with this year in the name
      year_files <- grep(paste0("_", year, "\\.|_", year, "$"), cms_files, value = TRUE)
      if (length(year_files) > 0) {
        year_specific_files[[as.character(year)]] <- year_files[1]  # Use the first match if multiple
        print_msg(paste("Found CMS file for year", year, ":", year_files[1]))
      }
    }
    
    # CMS data is available by year
    cms_data_list <- list()
    
    for (year in years) {
      # Skip future years
      if (year > as.integer(format(Sys.Date(), "%Y"))) {
        next
      }
      
      # Check if we have a year-specific file first
      if (as.character(year) %in% names(year_specific_files)) {
        cms_file <- year_specific_files[[as.character(year)]]
        print_msg(paste("Using year-specific CMS file for", year, ":", cms_file))
      } else {
        # Define file paths for downloading
        cms_file <- file.path(data_dir, paste0("cms_gv_", year, ".csv"))
        
        # Check if we need to download
        need_download <- !file.exists(cms_file) || refresh_cache
        
        if (need_download) {
        # CMS URLs follow a pattern but it may change
        cms_url <- paste0(
          "https://data.cms.gov/provider-data/sites/default/files/",
          "resources/", year, "/",
          "Medicare-Geographic-Variation-County-Level-", year, ".csv"
        )
        
        # Try to download
        if (!safe_download(cms_url, cms_file, paste("CMS data for", year))) {
          # Try alternative URL patterns
          alt_url <- paste0(
            "https://data.cms.gov/Medicare/Geographic-Variation-County-Level-", 
            year, "/main-data.csv"
          )
          
          if (!safe_download(alt_url, cms_file, paste("CMS data for", year, "(alt)"))) {
            # Try yet another pattern
            alt_url2 <- paste0(
              "https://data.cms.gov/Medicare-Geographic-Variation/",
              "Geographic-Variation-County-Level-", year, "/main-data.csv"
            )
            
            if (!safe_download(alt_url2, cms_file, paste("CMS data for", year, "(alt2)"))) {
              print_msg(paste("Could not download CMS data for", year))
              
              # If we have any CMS files, use the most recent one
              if (length(cms_files) > 0) {
                cms_file <- cms_files[1]  # Most recent file (already sorted)
                print_msg(paste("No data available for", year, "- using most recent available CMS file:", cms_file))
              } else {
                next
              }
            }
          }
        }
      } else {
        print_msg(paste("Using existing CMS file for", year))
      }
    }
    
    # Process the data if file exists
    if (file.exists(cms_file)) {
      print_msg(paste("Reading CMS data for", year))
      
      # Read the file
      tryCatch({
          cms_data <- read_csv(cms_file, show_col_types = FALSE)
          
          # Get column names
          print_msg(paste("CMS data has", ncol(cms_data), "columns and", nrow(cms_data), "rows"))
          
          # Check if this looks like CMS data
          # CMS typically has FIPS codes or state/county codes
          fips_col <- grep("FIPS|fips|County Code", names(cms_data), value = TRUE)[1]
          
          if (is.na(fips_col)) {
            # Try state and county code columns
            state_col <- grep("State|STATE", names(cms_data), value = TRUE)[1]
            county_col <- grep("County|COUNTY", names(cms_data), value = TRUE)[1]
            
            if (!is.na(state_col) && !is.na(county_col)) {
              # CMS often uses names rather than codes, so we'll need to map them
              # This is a complex task, so we'll skip it for this example
              print_msg("CMS data uses state/county names, not codes - skipping")
              next
            } else {
              print_msg("Could not identify FIPS or state/county columns in CMS data")
              next
            }
          }
          
          # Rename the FIPS column to GEOID
          cms_data <- cms_data %>%
            rename(GEOID = all_of(fips_col)) %>%
            # Ensure GEOID is properly formatted (5 digits with leading zeros)
            mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
          
          # CMS data typically has one column per measure
          # We need to identify the columns for our variables of interest
          
          # Find columns for Medicare spending
          spend_col <- grep("Total|Per Capita|Spending|Cost", 
                           names(cms_data), value = TRUE)[1]
          
          # Find columns for preventive services
          prev_col <- grep("Preventive|Screening|Prevention", 
                          names(cms_data), value = TRUE)[1]
          
          # Find columns for ambulatory care sensitive conditions
          amb_col <- grep("Ambulatory|ACSC|Avoidable", 
                         names(cms_data), value = TRUE)[1]
          
          print_msg(paste("Found columns:",
                        "Spending:", ifelse(is.na(spend_col), "No", "Yes"),
                        "Preventive:", ifelse(is.na(prev_col), "No", "Yes"),
                        "Ambulatory:", ifelse(is.na(amb_col), "No", "Yes")))
          
          # Create data frame for this year
          year_data <- data.frame(
            GEOID = cms_data$GEOID,
            year = year
          )
          
          # Add Medicare spending if available
          if (!is.na(spend_col)) {
            year_data$medicare_spending_per_beneficiary <- 
              as.numeric(cms_data[[spend_col]])
            
            year_data$medicare_spending_per_beneficiary_data_quality <- data_quality_flags$direct
            year_data$medicare_spending_per_beneficiary_data_source <- "CMS Geographic Variation"
            year_data$medicare_spending_per_beneficiary_data_vintage <- as.character(year)
          }
          
          # Add preventive services if available
          if (!is.na(prev_col)) {
            year_data$preventive_services_pct <- 
              as.numeric(cms_data[[prev_col]])
            
            year_data$preventive_services_pct_data_quality <- data_quality_flags$direct
            year_data$preventive_services_pct_data_source <- "CMS Geographic Variation"
            year_data$preventive_services_pct_data_vintage <- as.character(year)
          }
          
          # Add ambulatory care sensitive conditions if available
          if (!is.na(amb_col)) {
            year_data$ambulatory_care_sensitive_conditions <- 
              as.numeric(cms_data[[amb_col]])
            
            year_data$ambulatory_care_sensitive_conditions_data_quality <- data_quality_flags$direct
            year_data$ambulatory_care_sensitive_conditions_data_source <- "CMS Geographic Variation"
            year_data$ambulatory_care_sensitive_conditions_data_vintage <- as.character(year)
          }
          
          # Add to list
          cms_data_list[[as.character(year)]] <- year_data
          
          print_msg(paste("Processed CMS data for", year))
      }, error = function(e) {
        print_msg(paste("Error reading CMS data for", year, ":", conditionMessage(e)))
      })
      }
    }
    
    # Combine all years
    if (length(cms_data_list) > 0) {
      combined_cms <- bind_rows(cms_data_list)
      print_msg(paste("Combined CMS data with", nrow(combined_cms), "rows"))
      return(combined_cms)
    } else {
      print_msg("No CMS data processed successfully")
      return(NULL)
    }
  }
  
  # Get data from different healthcare sources - use parallel processing if enabled
  if (parallel && requireNamespace("future.apply", quietly = TRUE)) {
    print_msg("Using parallel processing to fetch data from multiple healthcare sources")
    
    # Define the data sources to fetch
    data_sources <- c("ahrf", "cms")
    
    # Create a function to process one data source
    process_data_source <- function(source) {
      print_msg(paste("Processing healthcare data source:", source))
      
      if (source == "ahrf") {
        return(get_ahrf_data())
      } else if (source == "cms") {
        return(get_cms_data())
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
      healthcare_data_sources <- progressr::with_progress({
        p <- progressr::progressor(steps = length(data_sources))
        
        future.apply::future_lapply(data_sources, function(source) {
          result <- process_data_source(source)
          p(message = paste("Processed healthcare data source:", source))
          return(result)
        })
      })
    } else {
      # Process without progress tracking
      healthcare_data_sources <- future.apply::future_lapply(data_sources, process_data_source)
    }
    
    # Convert results to named list
    names(healthcare_data_sources) <- data_sources
    
    # Filter out NULL results
    healthcare_data_list <- healthcare_data_sources[!sapply(healthcare_data_sources, is.null)]
    healthcare_data_list <- healthcare_data_list[sapply(healthcare_data_list, function(x) !is.null(x) && nrow(x) > 0)]
    
  } else {
    # Sequential processing
    print_msg("Using sequential processing to fetch data from multiple healthcare sources")
    ahrf_data <- get_ahrf_data()
    cms_data <- get_cms_data()
    
    # Combine all data sources
    healthcare_data_list <- list()
    
    if (!is.null(ahrf_data) && nrow(ahrf_data) > 0) {
      healthcare_data_list[["ahrf"]] <- ahrf_data
    }
    
    if (!is.null(cms_data) && nrow(cms_data) > 0) {
      healthcare_data_list[["cms"]] <- cms_data
    }
  }
  
  # Process if we have data
  if (length(healthcare_data_list) > 0) {
    # Combine all sources and handle overlapping columns
    # Start with first dataset
    combined_healthcare_data <- healthcare_data_list[[1]]
    
    # Add each additional dataset
    if (length(healthcare_data_list) > 1) {
      for (i in 2:length(healthcare_data_list)) {
        next_data <- healthcare_data_list[[i]]
        
        # Identify common columns for joining
        join_cols <- intersect(names(combined_healthcare_data), names(next_data))
        
        # Ensure we have at least GEOID and year for joining
        if (all(c("GEOID", "year") %in% join_cols)) {
          # Find columns that might overlap (excluding join columns and flags)
          overlap_cols <- setdiff(
            intersect(names(combined_healthcare_data), names(next_data)),
            c(join_cols, grep("_data_quality$|_data_source$|_data_vintage$", 
                             names(next_data), value = TRUE))
          )
          
          if (length(overlap_cols) > 0) {
            # For overlapping columns, prefer data from the current dataset
            # Remove those columns from the combined data before joining
            combined_healthcare_data <- combined_healthcare_data %>%
              select(-all_of(overlap_cols))
          }
          
          # Join with the next dataset
          combined_healthcare_data <- full_join(
            combined_healthcare_data, 
            next_data, 
            by = join_cols
          )
        } else {
          print_msg("Warning: Cannot join datasets - missing common keys")
        }
      }
    }
    
    # Check if we need to handle missing years
    available_years <- unique(combined_healthcare_data$year)
    missing_years <- setdiff(years, available_years)
    
    if (length(missing_years) > 0 && allow_interpolation) {
      print_msg(paste("Need to handle missing years:", paste(missing_years, collapse=", ")))
      
      # Enhanced interpolation/extrapolation
      # This will use actual interpolation between years rather than just copying
      
      # Identify all numeric variables to interpolate
      # Exclude identification columns and quality flags
      numeric_cols <- sapply(names(combined_healthcare_data), function(col) {
        !col %in% c("GEOID", "NAME", "year") && 
          !grepl("_data_quality$|_data_source$|_data_vintage$", col) &&
          is.numeric(combined_healthcare_data[[col]])
      })
      numeric_cols <- names(combined_healthcare_data)[numeric_cols]
      
      # Create interpolated data for each missing year
      interp_data_list <- list()
      
      for (year in missing_years) {
        # Check if this is an interpolation or extrapolation
        is_interpolation <- year > min(available_years) && year < max(available_years)
        
        if (is_interpolation) {
          print_msg(paste("Interpolating data for", year))
          
          # Interpolate for each county
          county_list <- unique(combined_healthcare_data$GEOID)
          
          # Define the interpolation function for a single county
          interpolate_county_data <- function(county) {
            # Get data for this county
            county_data <- combined_healthcare_data %>% filter(GEOID == county)
            
            # If we have at least 2 years for this county, interpolate
            if (nrow(county_data) >= 2) {
              # Create a new row for this county and year
              new_row <- county_data[1, setdiff(names(county_data), numeric_cols)] %>%
                mutate(year = year)
              
              # For each numeric column, interpolate
              for (col in numeric_cols) {
                # Extract existing data points
                x <- county_data$year
                y <- county_data[[col]]
                
                # Remove NA values
                valid <- !is.na(y)
                x_valid <- x[valid]
                y_valid <- y[valid]
                
                # If we have at least 2 valid points, interpolate
                if (length(x_valid) >= 2) {
                  # Use approx for linear interpolation
                  interp <- approx(x_valid, y_valid, xout = year)
                  new_row[[col]] <- interp$y
                  
                  # Add quality flags
                  new_row[[paste0(col, "_data_quality")]] <- data_quality_flags$interpolated
                  new_row[[paste0(col, "_data_source")]] <- 
                    county_data[[paste0(col, "_data_source")]][1]
                  new_row[[paste0(col, "_data_vintage")]] <- 
                    paste0("interpolated_", min(x_valid), "_", max(x_valid))
                } else if (length(x_valid) == 1) {
                  # Just one data point, use it directly
                  new_row[[col]] <- y_valid[1]
                  
                  # Add quality flags
                  new_row[[paste0(col, "_data_quality")]] <- data_quality_flags$extrapolated
                  new_row[[paste0(col, "_data_source")]] <- 
                    county_data[[paste0(col, "_data_source")]][1]
                  new_row[[paste0(col, "_data_vintage")]] <- 
                    paste0("extrapolated_from_", x_valid[1])
                }
              }
              
              return(new_row)
            } else {
              return(NULL)
            }
          }
          
          # Process counties in parallel if enabled
          interp_county_list <- if (parallel && requireNamespace("future.apply", quietly = TRUE)) {
            print_msg(paste("Using parallel processing for county interpolation with", length(county_list), "counties"))
            
            # Setup progress tracking if available
            if (requireNamespace("progressr", quietly = TRUE)) {
              progressr::handlers(progressr::handler_progress())
              result_list <- progressr::with_progress({
                p <- progressr::progressor(steps = length(county_list))
                
                future.apply::future_lapply(county_list, function(county) {
                  result <- interpolate_county_data(county)
                  p(message = paste("Processed county", county))
                  return(result)
                })
              })
            } else {
              # No progress tracking
              result_list <- future.apply::future_lapply(county_list, interpolate_county_data)
            }
            
            # Convert list to named list
            names(result_list) <- county_list
            result_list[!sapply(result_list, is.null)]
          } else {
            # Sequential processing
            print_msg(paste("Using sequential processing for county interpolation with", length(county_list), "counties"))
            result_list <- list()
            
            for (county in county_list) {
              result <- interpolate_county_data(county)
              if (!is.null(result)) {
                result_list[[county]] <- result
              }
            }
            
            result_list
          }
          
          # Combine all counties for this year
          if (length(interp_county_list) > 0) {
            interp_data_list[[as.character(year)]] <- bind_rows(interp_county_list)
          }
        } else {
          # Extrapolation - find closest year and adjust
          print_msg(paste("Extrapolating data for", year))
          
          closest_year <- available_years[which.min(abs(available_years - year))]
          
          # Get data for closest year
          closest_data <- combined_healthcare_data %>%
            filter(year == closest_year) %>%
            mutate(year = year)
          
          # Update quality flags for this data
          for (col in numeric_cols) {
            quality_col <- paste0(col, "_data_quality")
            vintage_col <- paste0(col, "_data_vintage")
            
            if (quality_col %in% names(closest_data)) {
              closest_data[[quality_col]] <- data_quality_flags$extrapolated
              closest_data[[vintage_col]] <- paste0("extrapolated_from_", closest_year)
            }
          }
          
          interp_data_list[[as.character(year)]] <- closest_data
        }
      }
      
      # Combine interpolated data with original data
      if (length(interp_data_list) > 0) {
        interp_data <- bind_rows(interp_data_list)
        combined_healthcare_data <- bind_rows(combined_healthcare_data, interp_data)
      }
    } else if (length(missing_years) > 0) {
      print_msg(paste("Missing years:", paste(missing_years, collapse=", "), "but interpolation is disabled"))
    }
    
    # Filter to just the requested years
    combined_healthcare_data <- combined_healthcare_data %>%
      filter(year %in% years)
    
    # Sort data
    combined_healthcare_data <- combined_healthcare_data %>%
      arrange(GEOID, year)
    
    # Cache the processed data
    saveRDS(combined_healthcare_data, cache_file)
    print_msg(paste("Cached healthcare access data to:", cache_file))
    
    return(combined_healthcare_data)
  } else {
    # No data available - create empty dataset with proper structure
    print_msg("No healthcare access data available. Creating empty dataset with proper structure.")
    
    # Get variable list for healthcare variables
    healthcare_vars <- c(
      "primary_care_physicians_per_100k",
      "mental_health_providers_per_100k",
      "dentists_per_100k",
      "hospital_beds_per_1000",
      "fqhc_access_pct",
      "pharmacies_per_100k",
      "preventable_hospital_stays",
      "medicare_spending_per_beneficiary",
      "preventive_services_pct",
      "ambulatory_care_sensitive_conditions"
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
    for (var in healthcare_vars) {
      grid[[var]] <- NA_real_
      grid[[paste0(var, "_data_quality")]] <- data_quality_flags$missing
      grid[[paste0(var, "_data_source")]] <- "NO_DATA_AVAILABLE"
      grid[[paste0(var, "_data_vintage")]] <- NA_character_
    }
    
    healthcare_data <- as_tibble(grid)
    print_msg(paste("Created empty healthcare dataset with", nrow(healthcare_data), "rows"))
    
    # Provide clear error message about missing data
    print_msg("ERROR: No healthcare data files found. Please download healthcare data.")
    print_msg("Required files should be placed in one of these directories:")
    for (dir in c("data/healthcare", "data/health", "data/medical")) {
      print_msg(paste("  -", dir))
    }
    print_msg("File formats needed:")
    print_msg("1. HRSA Area Health Resources Files (AHRF): Annual survey of county-level healthcare resources")
    print_msg("   - Download from: https://data.hrsa.gov/topics/health-workforce/ahrf")
    print_msg("   - Expected file name: ahrf_current.csv or similar")
    print_msg("2. CMS Geographic Variation: Medicare data by county")
    print_msg("   - Download from: https://data.cms.gov/tools/geographic-variation-dashboard")
    print_msg("   - Expected file name: cms_gv_YYYY.csv where YYYY is the year")
    
    # Cache the empty data
    saveRDS(healthcare_data, cache_file)
    print_msg(paste("Cached empty healthcare data to:", cache_file))
    
    return(healthcare_data)
  }
}

# This lets the function be used when the script is sourced
is_sourced <- function() {
  return(!identical(environment(), globalenv()))
}

# If the script is run directly, test the function
if (!is_sourced()) {
  cat("Testing healthcare access data fetcher...\n")
  
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
    cat("Using parallel processing for healthcare data fetching test\n")
  } else {
    cat("Parallel processing dependencies not available, using sequential processing\n")
  }
  
  # Test the function
  result <- fetch_healthcare_data(
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
  
  cat("Test completed with", nrow(result), "rows of data.\n")
  
  # Report data quality metrics
  cat("Data quality summary:\n")
  quality_cols <- grep("_data_quality$", names(result), value = TRUE)
  for (col in quality_cols) {
    var_name <- gsub("_data_quality$", "", col)
    quality_counts <- table(result[[col]], useNA = "always")
    cat(paste(" -", var_name, ":", paste(names(quality_counts), quality_counts, sep="=", collapse=", "), "\n"))
  }
}
