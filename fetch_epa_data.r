#!/usr/bin/env Rscript

# EPA Environmental Data Fetcher
# This script handles retrieval of environmental data from EPA sources
# Enhanced version with additional EPA data sources and improved data handling

library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)
library(sf)
library(readr)
library(readxl)
library(zoo) # For interpolation if needed

#' Fetch EPA environmental data
#'
#' Enhanced version that retrieves environmental data from multiple EPA sources:
#' - EPA Air Quality System (AQS) - air pollutant measurements
#' - EPA EJSCREEN - environmental justice screening and mapping
#' - EPA ECHO (Enforcement and Compliance History Online) - facility compliance
#' - EPA Toxic Release Inventory (TRI) - toxic chemical releases
#'
#' Features:
#' - Comprehensive county-level environmental indicators
#' - Robust error handling with rate limiting protection
#' - Advanced interpolation for missing years
#' - Improved offline operation support
#' - Data validation and quality flags
#'
#' @param years Vector of years to include
#' @param cache_dir Directory to store cache files
#' @param refresh_cache Whether to refresh the cache
#' @param allow_simulation Whether to generate simulated data if real data not available
#' @param offline_mode If TRUE, will only use cached data without attempting downloads
#' @param allow_interpolation Whether to interpolate missing years
#' @param data_quality_flags List with standardized data quality flags 
#' @return A data frame with environmental data for all requested years
fetch_epa_data <- function(years, 
                          cache_dir = "data/cache", 
                          refresh_cache = FALSE,
                          allow_simulation = FALSE,
                          allow_interpolation = TRUE,
                          data_quality_flags = list(
                            direct = "direct",
                            interpolated = "interpolated",
                            extrapolated = "extrapolated",
                            simulated = "simulated",
                            missing = NA,
                            imputed = "imputed"
                          ),
                          offline_mode = FALSE) {
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
  cache_file <- file.path(cache_dir, "epa_environmental_data.rds")
  
  # Use cache if available and not refreshing
  if (!refresh_cache && file.exists(cache_file)) {
    print_msg("Loading cached EPA environmental data...")
    env_data <- readRDS(cache_file)
    
    # Check if all requested years are in the cache
    cached_years <- unique(env_data$year)
    missing_years <- setdiff(years, cached_years)
    
    # Check for empty cache with just placeholder data
    if (nrow(env_data) <= 1 || 
        (is.data.frame(env_data) && "data_source" %in% names(env_data) && 
         any(grepl("SIMULATED", env_data$data_source)))) {
      print_msg("Cached EPA data appears to be empty or a placeholder. Will process files again.")
      # Force refresh by continuing past this point
    } else if (length(missing_years) == 0) {
      print_msg("Using complete cached EPA environmental data.")
      return(env_data %>% filter(year %in% years))
    } else {
      print_msg(paste("Cache missing years:", paste(missing_years, collapse=", ")))
    }
  }
  
  # Make sure cache directory exists
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    print_msg(paste("Created cache directory at:", cache_dir))
  }
  
  # Make data directory if needed - ensure path is relative to current working directory
  # Rather than assuming a "data" directory, create a subdirectory in the cache_dir
  data_dir <- file.path(cache_dir, "epa")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
    print_msg(paste("Created EPA data directory at:", data_dir))
  }
  
  # Make subdirectories for each data source
  for (subdir in c("aqs", "ejscreen", "echo", "tri")) {
    subdir_path <- file.path(data_dir, subdir)
    if (!dir.exists(subdir_path)) {
      dir.create(subdir_path, showWarnings = FALSE, recursive = TRUE)
      print_msg(paste("Created", subdir, "data directory at:", subdir_path))
    }
  }
  
  # Helper function to validate data files
  validate_data_file <- function(file_path, expected_type = "csv") {
    if (!file.exists(file_path)) return(FALSE)
    
    # Check file size (shouldn't be too small)
    file_size <- file.info(file_path)$size
    if (file_size < 1000) {
      print_msg(paste("Warning: File", file_path, "appears to be too small (", 
                      file_size, "bytes)"))
      return(FALSE)
    }
    
    # Try to read the file to validate it works
    if (expected_type == "csv") {
      tryCatch({
        # Try to read the first few rows
        headers <- readr::read_lines(file_path, n_max = 2)
        return(length(headers) > 0)
      }, error = function(e) {
        print_msg(paste("Error validating CSV file:", conditionMessage(e)))
        return(FALSE)
      })
    } else if (expected_type == "excel") {
      tryCatch({
        # Just try to read sheet names to validate
        sheets <- readxl::excel_sheets(file_path)
        return(length(sheets) > 0)
      }, error = function(e) {
        print_msg(paste("Error validating Excel file:", conditionMessage(e)))
        return(FALSE)
      })
    } else if (expected_type == "json") {
      tryCatch({
        # Try to parse the JSON
        json_data <- jsonlite::fromJSON(file_path)
        return(!is.null(json_data))
      }, error = function(e) {
        print_msg(paste("Error validating JSON file:", conditionMessage(e)))
        return(FALSE)
      })
    } else if (expected_type == "zip") {
      tryCatch({
        # Check if it's a valid zip file
        zip_files <- unzip(file_path, list = TRUE)
        return(nrow(zip_files) > 0)
      }, error = function(e) {
        print_msg(paste("Error validating ZIP file:", conditionMessage(e)))
        return(FALSE)
      })
    }
    
    return(TRUE)
  }
  
  # Enhanced safe download function with retries and rate limiting protection
  safe_download <- function(url, destfile, description, retries = 3, wait_time = 2, 
                           expected_type = tools::file_ext(destfile)) {
    if (offline_mode) {
      print_msg("Skipping download in offline mode")
      return(file.exists(destfile))
    }
    
    print_msg(paste("Downloading", description, "from:", url))
    
    for (attempt in 1:retries) {
      success <- tryCatch({
        # Set timeout to 60 seconds
        options(timeout = 60)
        
        # Try to download with httr for better control
        response <- httr::GET(url, httr::write_disk(destfile, overwrite = TRUE),
                             httr::timeout(60))
        
        # Check status code
        status_code <- httr::status_code(response)
        
        # Handle rate limiting (429 Too Many Requests)
        if (status_code == 429) {
          # Check for Retry-After header
          retry_after <- httr::headers(response)[["Retry-After"]]
          if (!is.null(retry_after)) {
            retry_after <- as.numeric(retry_after)
          } else {
            # Default to 30 seconds if no Retry-After header
            retry_after <- 30
          }
          
          print_msg(paste("Rate limited. Waiting", retry_after, "seconds before retrying..."))
          Sys.sleep(retry_after)
          return(FALSE)  # Will trigger retry
        }
        
        # Handle other error codes
        if (status_code != 200) {
          print_msg(paste("HTTP error:", status_code))
          return(FALSE)
        }
        
        # Validate downloaded file
        file_valid <- validate_data_file(destfile, expected_type = expected_type)
        
        if (!file_valid) {
          print_msg("Downloaded file appears to be invalid")
          return(FALSE)
        }
        
        TRUE
      }, error = function(e) {
        print_msg(paste("Error on download attempt", attempt, ":", conditionMessage(e)))
        FALSE
      })
      
      if (success) {
        print_msg("Download successful!")
        return(TRUE)
      } else if (attempt < retries) {
        print_msg(paste("Retrying in", wait_time, "seconds... (Attempt", attempt, "of", retries, ")"))
        Sys.sleep(wait_time)
        # Increase wait time for next retry (exponential backoff)
        wait_time <- wait_time * 2
      }
    }
    
    print_msg(paste("Failed to download after", retries, "attempts"))
    
    # Check if we have an existing file to use
    if (file.exists(destfile) && validate_data_file(destfile, expected_type)) {
      print_msg("Using existing file as fallback")
      return(TRUE)
    }
    
    return(FALSE)
  }
  
  # Function to get Air Quality Data from EPA AQS
  get_air_quality_data <- function() {
    # Air Quality System (AQS) data is available through the EPA API
    # Note: In a production environment, you would need to register for an API key
    # This example uses the annual summary data which is available for download
    
    aqs_data_list <- list()
    
    for (year in years) {
      # Skip future years
      if (year > as.integer(format(Sys.Date(), "%Y"))) {
        next
      }
      
      # Annual summary files are available for recent years (usually back to 2000)
      annual_file <- file.path(data_dir, paste0("annual_aqs_", year, ".zip"))
      
      # URL for annual summary files (check EPA website for current URLs)
      annual_url <- paste0(
        "https://aqs.epa.gov/aqsweb/airdata/annual_conc_by_county_", 
        year, 
        ".zip"
      )
      
      # Try to download if file doesn't exist or refresh is requested
      if (!file.exists(annual_file) || refresh_cache) {
        success <- safe_download(annual_url, annual_file, 
                                paste("AQS annual data for", year))
        
        if (!success && file.exists(annual_file)) {
          print_msg(paste("Using existing file for", year))
        } else if (!success) {
          print_msg(paste("No data available for", year))
          next
        }
      } else {
        print_msg(paste("Using existing AQS file for", year))
      }
      
      # Read the data if file exists
      if (file.exists(annual_file)) {
        # Read the ZIP file directly
        tryCatch({
          # Unzip to a temporary file then read
          temp_dir <- tempdir()
          unzip(annual_file, exdir = temp_dir)
          
          # Find the CSV file
          csv_file <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE)[1]
          
          if (!is.na(csv_file)) {
            # Read the CSV
            aqs_data <- read_csv(csv_file, show_col_types = FALSE)
            
            # Check if read was successful
            if (nrow(aqs_data) > 0) {
              print_msg(paste("Read", nrow(aqs_data), "rows from AQS data for", year))
              
              # Extract county-level summaries
              # Common columns in AQS data: State Code, County Code, Parameter Name, Arithmetic Mean
              if (all(c("State Code", "County Code", "Parameter Name", "Arithmetic Mean") %in% names(aqs_data))) {
                # Process county data
                county_aqs <- aqs_data %>%
                  # Create FIPS code
                  mutate(
                    GEOID = sprintf("%02d%03d", `State Code`, `County Code`),
                    year = year,
                    Parameter = `Parameter Name`,
                    Value = `Arithmetic Mean`
                  ) %>%
                  select(GEOID, year, Parameter, Value)
                
                # Pivot to get key parameters
                param_mapping <- c(
                  "pm25_annual_mean" = "PM2.5 - Local Conditions",
                  "ozone_annual_mean" = "Ozone",
                  "no2_annual_mean" = "Nitrogen dioxide (NO2)",
                  "so2_annual_mean" = "Sulfur dioxide"
                )
                
                # Create list of parameters actually in the data
                available_params <- intersect(unname(param_mapping), unique(county_aqs$Parameter))
                
                if (length(available_params) > 0) {
                  # Filter to just the parameters we want
                  county_aqs <- county_aqs %>%
                    filter(Parameter %in% available_params)
                  
                  # Pivot to wide format
                  wide_aqs <- county_aqs %>%
                    pivot_wider(
                      id_cols = c(GEOID, year),
                      names_from = Parameter,
                      values_from = Value
                    )
                  
                  # Rename columns to standardized names
                  for (std_name in names(param_mapping)) {
                    param <- param_mapping[[std_name]]
                    if (param %in% names(wide_aqs)) {
                      wide_aqs <- wide_aqs %>%
                        rename(!!std_name := all_of(param))
                    }
                  }
                  
                  # Add data quality flags
                  for (std_name in names(param_mapping)) {
                    if (std_name %in% names(wide_aqs)) {
                      wide_aqs[[paste0(std_name, "_data_quality")]] <- data_quality_flags$direct
                      wide_aqs[[paste0(std_name, "_data_source")]] <- "EPA Air Quality System"
                      wide_aqs[[paste0(std_name, "_data_vintage")]] <- as.character(year)
                    }
                  }
                  
                  # Add to list
                  aqs_data_list[[as.character(year)]] <- wide_aqs
                }
              } else {
                print_msg("AQS data doesn't have expected columns - format may have changed")
              }
            }
          } else {
            print_msg("No CSV file found in the ZIP archive")
          }
        }, error = function(e) {
          print_msg(paste("Error processing AQS data for", year, ":", conditionMessage(e)))
        })
      }
    }
    
    # Combine all years
    if (length(aqs_data_list) > 0) {
      combined_aqs <- bind_rows(aqs_data_list)
      print_msg(paste("Combined AQS data with", nrow(combined_aqs), "rows"))
      return(combined_aqs)
    } else {
      print_msg("No AQS data processed successfully")
      return(NULL)
    }
  }
  
  # Function to get EJSCREEN data
  get_ejscreen_data <- function() {
    # EJSCREEN data is available for download from the EPA
    # It's typically available from 2015 onwards
    # This function will get the most recent available year
    
    # Define EJSCREEN variables of interest
    ejscreen_vars <- c(
      "proximity_to_hazardous_waste" = "PNPL",
      "proximity_to_npl_sites" = "PRMP",
      "wastewater_discharge" = "PWDIS",
      "traffic_proximity" = "PTRA",
      "lead_paint_indicator" = "PLEAD"
    )
    
    # Start with most recent year and work backwards
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    
    # Try to get data for each year
    ejscreen_data_list <- list()
    
    for (year in seq(min(2016, current_year), current_year, by = 1)) {
      # Skip if year is not in requested years
      if (!year %in% years) next
      
      # Check if we have cached data for this year
      year_file <- file.path(data_dir, paste0("ejscreen_", year, ".csv"))
      
      if (!file.exists(year_file) || refresh_cache) {
        # Try to download the data
        # EJSCREEN URLs follow a pattern, though they may change
        ejscreen_url <- paste0(
          "https://gaftp.epa.gov/EJSCREEN/", 
          year,
          "/StatePctile/", 
          "EJSCREEN_StatePctile_", 
          year, 
          ".csv"
        )
        
        success <- safe_download(ejscreen_url, year_file, 
                               paste("EJSCREEN data for", year))
        
        if (!success) {
          # Try alternative URL
          alt_url <- paste0(
            "https://gaftp.epa.gov/EJSCREEN/", 
            year,
            "/", 
            "EJSCREEN_Full_", 
            year, 
            ".csv"
          )
          
          success <- safe_download(alt_url, year_file, 
                                 paste("EJSCREEN data for", year, "(alt)"))
          
          if (!success && file.exists(year_file)) {
            print_msg(paste("Using existing file for", year))
          } else if (!success) {
            print_msg(paste("No EJSCREEN data available for", year))
            next
          }
        }
      } else {
        print_msg(paste("Using existing EJSCREEN file for", year))
      }
      
      # Read the data if file exists
      if (file.exists(year_file)) {
        tryCatch({
          # EJSCREEN files can be large, read with low memory optimizations
          ej_data <- read_csv(
            year_file, 
            show_col_types = FALSE,
            guess_max = 10000,
            col_select = c("ID", "STATE_NAME", "CNTY_NAME", names(ejscreen_vars))
          )
          
          if (nrow(ej_data) > 0) {
            print_msg(paste("Read", nrow(ej_data), "rows from EJSCREEN for", year))
            
            # Process to county level if block group level
            # EJSCREEN is often at block group level, need to aggregate
            if (!"CNTY_NAME" %in% names(ej_data) && "COUNTY" %in% names(ej_data)) {
              # Rename to standard
              ej_data <- ej_data %>%
                rename(CNTY_NAME = COUNTY)
            }
            
            # Extract FIPS from ID if possible
            if ("ID" %in% names(ej_data)) {
              # EJSCREEN IDs are usually block group FIPS (12 digits)
              # Extract the county portion (first 5 digits)
              ej_data <- ej_data %>%
                mutate(GEOID = substr(ID, 1, 5))
            } else {
              print_msg("No ID column found in EJSCREEN data")
              next
            }
            
            # Aggregate to county level if necessary
            county_ej <- ej_data %>%
              group_by(GEOID) %>%
              summarize(across(all_of(unname(ejscreen_vars)), mean, na.rm = TRUE), .groups = "drop")
            
            # Add year
            county_ej$year <- year
            
            # Rename columns to standardized names
            for (std_name in names(ejscreen_vars)) {
              ej_var <- ejscreen_vars[[std_name]]
              if (ej_var %in% names(county_ej)) {
                county_ej <- county_ej %>%
                  rename(!!std_name := all_of(ej_var))
                
                # Add data quality flags
                county_ej[[paste0(std_name, "_data_quality")]] <- data_quality_flags$direct
                county_ej[[paste0(std_name, "_data_source")]] <- "EPA EJSCREEN"
                county_ej[[paste0(std_name, "_data_vintage")]] <- as.character(year)
              }
            }
            
            # Add to list
            ejscreen_data_list[[as.character(year)]] <- county_ej
          }
        }, error = function(e) {
          print_msg(paste("Error processing EJSCREEN data for", year, ":", conditionMessage(e)))
        })
      }
    }
    
    # Combine all years
    if (length(ejscreen_data_list) > 0) {
      combined_ejscreen <- bind_rows(ejscreen_data_list)
      print_msg(paste("Combined EJSCREEN data with", nrow(combined_ejscreen), "rows"))
      return(combined_ejscreen)
    } else {
      print_msg("No EJSCREEN data processed successfully")
      return(NULL)
    }
  }
  
  # Function to get ECHO (Enforcement and Compliance History Online) data
  # This adds information about EPA-regulated facilities and their compliance status
  get_echo_data <- function() {
    # ECHO data is available through EPA's ECHO API
    # We'll use the state-level downloads which provide facility counts by county
    echo_data_list <- list()
    
    for (year in years) {
      # Skip future years
      if (year > as.integer(format(Sys.Date(), "%Y"))) {
        next
      }
      
      # For older years, we may need to look for archived data
      is_historical <- year < (as.integer(format(Sys.Date(), "%Y")) - 5)
      
      # Define filename for this year
      echo_file <- file.path(data_dir, "echo", paste0("echo_facilities_", year, ".csv"))
      
      # URL for current ECHO data (check EPA website for current URLs)
      # Note: URLs may change over time
      echo_base_url <- "https://echo.epa.gov/files/echodownloads/facility_downloads"
      echo_url <- paste0(echo_base_url, "/facilities_download_", year, ".zip")
      
      # For historical data, try archive URL
      if (is_historical) {
        echo_url <- paste0(echo_base_url, "/archive/facilities_download_", year, ".zip")
      }
      
      # Try to download if file doesn't exist or refresh is requested
      echo_file_available <- FALSE
      if (!file.exists(echo_file) || refresh_cache) {
        # First try to download the zip file to a temporary location
        temp_zip <- tempfile(fileext = ".zip")
        success <- safe_download(echo_url, temp_zip, 
                               paste("ECHO facilities data for", year),
                               expected_type = "zip")
        
        if (success) {
          # Extract the CSV file from the zip
          tryCatch({
            # Unzip to temp directory
            temp_dir <- tempdir()
            unzip(temp_zip, exdir = temp_dir)
            
            # Find the main CSV file
            csv_files <- list.files(temp_dir, pattern = ".*facilities.*\\.csv$", 
                                 full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
            
            if (length(csv_files) > 0) {
              # Copy first matching file to the destination
              file.copy(csv_files[1], echo_file, overwrite = TRUE)
              echo_file_available <- TRUE
              print_msg(paste("Extracted ECHO data for", year))
            } else {
              print_msg(paste("No CSV file found in ECHO download for", year))
            }
          }, error = function(e) {
            print_msg(paste("Error extracting ECHO data for", year, ":", conditionMessage(e)))
          })
          
          # Remove temp zip file
          if (file.exists(temp_zip)) file.remove(temp_zip)
        }
      } else {
        echo_file_available <- TRUE
        print_msg(paste("Using existing ECHO file for", year))
      }
      
      # Process the data if available
      if (echo_file_available && file.exists(echo_file)) {
        tryCatch({
          # Read the CSV file efficiently (only select columns we need)
          echo_raw <- read_csv(echo_file, show_col_types = FALSE, 
                            col_select = c("FAC_COUNTY", "FAC_STATE", "FAC_DERIVED_STCNTYFIPS",
                                         "CWA_CURR_VIO", "CAA_CURR_VIO", "RCRA_CURR_VIO",
                                         "SDWA_CURR_VIO", "FAC_ACTIVE_FLAG"),
                            guess_max = 10000)
          
          # Process to county level
          if (nrow(echo_raw) > 0) {
            # Ensure we have a FIPS column
            if ("FAC_DERIVED_STCNTYFIPS" %in% names(echo_raw)) {
              echo_raw <- echo_raw %>%
                mutate(GEOID = sprintf("%05d", as.numeric(FAC_DERIVED_STCNTYFIPS)))
            } else {
              print_msg("ECHO data doesn't have county FIPS codes - skipping")
              next
            }
            
            # Fix any missing values in violation columns
            vio_cols <- c("CWA_CURR_VIO", "CAA_CURR_VIO", "RCRA_CURR_VIO", "SDWA_CURR_VIO")
            for (col in vio_cols) {
              if (col %in% names(echo_raw)) {
                echo_raw[[col]] <- ifelse(is.na(echo_raw[[col]]) | echo_raw[[col]] == "", "No", echo_raw[[col]])
              }
            }
            
            # Count facilities and violations by county
            county_echo <- echo_raw %>%
              group_by(GEOID) %>%
              summarize(
                total_regulated_facilities = n(),
                active_facilities = sum(FAC_ACTIVE_FLAG == "Y", na.rm = TRUE),
                cwa_violations = sum(CWA_CURR_VIO == "Yes", na.rm = TRUE),
                caa_violations = sum(CAA_CURR_VIO == "Yes", na.rm = TRUE),
                rcra_violations = sum(RCRA_CURR_VIO == "Yes", na.rm = TRUE),
                sdwa_violations = sum(SDWA_CURR_VIO == "Yes", na.rm = TRUE),
                facilities_with_violations = sum(CWA_CURR_VIO == "Yes" | 
                                              CAA_CURR_VIO == "Yes" | 
                                              RCRA_CURR_VIO == "Yes" | 
                                              SDWA_CURR_VIO == "Yes", na.rm = TRUE),
                .groups = "drop"
              ) %>%
              mutate(
                year = year,
                pct_facilities_with_violations = facilities_with_violations / total_regulated_facilities * 100
              )
            
            # Add data quality and source flags
            for (col in c("total_regulated_facilities", "active_facilities", 
                         "cwa_violations", "caa_violations", "rcra_violations", 
                         "sdwa_violations", "facilities_with_violations", 
                         "pct_facilities_with_violations")) {
              county_echo[[paste0(col, "_data_quality")]] <- data_quality_flags$direct
              county_echo[[paste0(col, "_data_source")]] <- "EPA ECHO"
              county_echo[[paste0(col, "_data_vintage")]] <- as.character(year)
            }
            
            # Add to list
            echo_data_list[[as.character(year)]] <- county_echo
            print_msg(paste("Processed ECHO data for", nrow(county_echo), "counties in", year))
          }
        }, error = function(e) {
          print_msg(paste("Error processing ECHO data for", year, ":", conditionMessage(e)))
        })
      }
    }
    
    # Combine all years
    if (length(echo_data_list) > 0) {
      combined_echo <- bind_rows(echo_data_list)
      print_msg(paste("Combined ECHO data with", nrow(combined_echo), "rows"))
      return(combined_echo)
    } else {
      print_msg("No ECHO data processed successfully")
      return(NULL)
    }
  }
  
  # Function to get Toxic Release Inventory (TRI) data
  # This adds information about toxic chemical releases by county
  get_tri_data <- function() {
    # TRI data is available for download from the EPA
    tri_data_list <- list()
    
    for (year in years) {
      # Skip future years
      if (year > as.integer(format(Sys.Date(), "%Y")) - 1) { # TRI typically has 1-year lag
        next
      }
      
      # Define filename for this year
      tri_file <- file.path(data_dir, "tri", paste0("tri_basic_", year, ".csv"))
      
      # URL for TRI data (check EPA website for current URLs)
      tri_url <- paste0("https://data.epa.gov/efservice/downloads/tri/mv_tri_basic_download/", 
                        year, "_US/csv")
      
      # Try to download if file doesn't exist or refresh is requested
      tri_file_available <- FALSE
      if (!file.exists(tri_file) || refresh_cache) {
        success <- safe_download(tri_url, tri_file, 
                               paste("TRI data for", year))
        
        if (success) {
          tri_file_available <- TRUE
        } else if (file.exists(tri_file)) {
          tri_file_available <- TRUE
          print_msg(paste("Using existing TRI file for", year))
        }
      } else {
        tri_file_available <- TRUE
        print_msg(paste("Using existing TRI file for", year))
      }
      
      # Process the data if available
      if (tri_file_available && file.exists(tri_file)) {
        tryCatch({
          # Read the CSV file efficiently (only select columns we need)
          tri_raw <- read_csv(tri_file, show_col_types = FALSE,
                           col_select = c("FRS_ID", "FACILITY_NAME", "COUNTY", "ST", 
                                        "FIPS_CODE", "TOTAL_AIR", "TOTAL_WATER", 
                                        "TOTAL_ONSITE_LAND", "TOTAL_RELEASES"),
                           guess_max = 10000)
          
          if (nrow(tri_raw) > 0) {
            # Ensure we have a FIPS column
            if ("FIPS_CODE" %in% names(tri_raw)) {
              tri_raw <- tri_raw %>%
                mutate(GEOID = sprintf("%05d", as.numeric(FIPS_CODE)))
            } else {
              print_msg("TRI data doesn't have county FIPS codes - skipping")
              next
            }
            
            # Aggregate to county level
            county_tri <- tri_raw %>%
              mutate(across(c(TOTAL_AIR, TOTAL_WATER, TOTAL_ONSITE_LAND, TOTAL_RELEASES), 
                          ~ as.numeric(.), .names = "{.col}")) %>%
              group_by(GEOID) %>%
              summarize(
                tri_facilities = n_distinct(FRS_ID),
                tri_air_releases = sum(TOTAL_AIR, na.rm = TRUE),
                tri_water_releases = sum(TOTAL_WATER, na.rm = TRUE),
                tri_land_releases = sum(TOTAL_ONSITE_LAND, na.rm = TRUE),
                tri_total_releases = sum(TOTAL_RELEASES, na.rm = TRUE),
                .groups = "drop"
              ) %>%
              mutate(year = year)
            
            # Add data quality and source flags
            for (col in c("tri_facilities", "tri_air_releases", "tri_water_releases", 
                         "tri_land_releases", "tri_total_releases")) {
              county_tri[[paste0(col, "_data_quality")]] <- data_quality_flags$direct
              county_tri[[paste0(col, "_data_source")]] <- "EPA Toxic Release Inventory"
              county_tri[[paste0(col, "_data_vintage")]] <- as.character(year)
            }
            
            # Add to list
            tri_data_list[[as.character(year)]] <- county_tri
            print_msg(paste("Processed TRI data for", nrow(county_tri), "counties in", year))
          }
        }, error = function(e) {
          print_msg(paste("Error processing TRI data for", year, ":", conditionMessage(e)))
        })
      }
    }
    
    # Combine all years
    if (length(tri_data_list) > 0) {
      combined_tri <- bind_rows(tri_data_list)
      print_msg(paste("Combined TRI data with", nrow(combined_tri), "rows"))
      return(combined_tri)
    } else {
      print_msg("No TRI data processed successfully")
      return(NULL)
    }
  }
  
  # Get data from different EPA sources with improved error handling
  aqs_data <- tryCatch({
    get_air_quality_data()
  }, error = function(e) {
    print_msg(paste("Error getting AQS data:", conditionMessage(e)))
    NULL
  })
  
  ejscreen_data <- tryCatch({
    get_ejscreen_data()
  }, error = function(e) {
    print_msg(paste("Error getting EJSCREEN data:", conditionMessage(e)))
    NULL
  })
  
  echo_data <- tryCatch({
    get_echo_data()
  }, error = function(e) {
    print_msg(paste("Error getting ECHO data:", conditionMessage(e)))
    NULL
  })
  
  tri_data <- tryCatch({
    get_tri_data()
  }, error = function(e) {
    print_msg(paste("Error getting TRI data:", conditionMessage(e)))
    NULL
  })
  
  # Combine all data sources
  env_data_list <- list()
  
  if (!is.null(aqs_data) && nrow(aqs_data) > 0) {
    env_data_list[["aqs"]] <- aqs_data
    print_msg(paste("Added", nrow(aqs_data), "rows of AQS air quality data"))
  }
  
  if (!is.null(ejscreen_data) && nrow(ejscreen_data) > 0) {
    env_data_list[["ejscreen"]] <- ejscreen_data
    print_msg(paste("Added", nrow(ejscreen_data), "rows of EJSCREEN environmental justice data"))
  }
  
  if (!is.null(echo_data) && nrow(echo_data) > 0) {
    env_data_list[["echo"]] <- echo_data
    print_msg(paste("Added", nrow(echo_data), "rows of ECHO facility compliance data"))
  }
  
  if (!is.null(tri_data) && nrow(tri_data) > 0) {
    env_data_list[["tri"]] <- tri_data
    print_msg(paste("Added", nrow(tri_data), "rows of TRI toxic release data"))
  }
  
  # Process if we have data
  if (length(env_data_list) > 0) {
    # Combine all sources and handle overlapping columns
    # Start with first dataset
    combined_env_data <- env_data_list[[1]]
    
    # Add each additional dataset
    if (length(env_data_list) > 1) {
      for (i in 2:length(env_data_list)) {
        next_data <- env_data_list[[i]]
        
        # Identify common columns for joining
        join_cols <- intersect(names(combined_env_data), names(next_data))
        
        # Ensure we have at least GEOID and year for joining
        if (all(c("GEOID", "year") %in% join_cols)) {
          # Find columns that might overlap (excluding join columns and flags)
          overlap_cols <- setdiff(
            intersect(names(combined_env_data), names(next_data)),
            c(join_cols, grep("_data_quality$|_data_source$|_data_vintage$", 
                             names(next_data), value = TRUE))
          )
          
          if (length(overlap_cols) > 0) {
            # For overlapping columns, prefer data from the current dataset
            # Remove those columns from the combined data before joining
            combined_env_data <- combined_env_data %>%
              select(-all_of(overlap_cols))
          }
          
          # Join with the next dataset
          combined_env_data <- full_join(
            combined_env_data, 
            next_data, 
            by = join_cols
          )
        } else {
          print_msg("Warning: Cannot join datasets - missing common keys")
        }
      }
    }
    
    # Check if we need to interpolate for missing years
    available_years <- unique(combined_env_data$year)
    missing_years <- setdiff(years, available_years)
    
    if (length(missing_years) > 0 && allow_interpolation) {
      print_msg(paste("Need to handle missing years:", paste(missing_years, collapse=", ")))
      
      # Enhanced interpolation/extrapolation with detailed approach
      # Identify all numeric variables to interpolate
      # Exclude identification columns and quality flags
      numeric_cols <- sapply(names(combined_env_data), function(col) {
        !col %in% c("GEOID", "NAME", "year") && 
          !grepl("_data_quality$|_data_source$|_data_vintage$", col) &&
          is.numeric(combined_env_data[[col]])
      })
      numeric_cols <- names(combined_env_data)[numeric_cols]
      
      # Create interpolated data for each missing year
      interp_data_list <- list()
      
      for (year in missing_years) {
        # Check if this is an interpolation or extrapolation
        is_interpolation <- year > min(available_years) && year < max(available_years)
        
        if (is_interpolation) {
          print_msg(paste("Interpolating data for", year))
          
          # Interpolate for each county
          county_list <- unique(combined_env_data$GEOID)
          
          interp_county_list <- list()
          
          for (county in county_list) {
            # Get data for this county
            county_data <- combined_env_data %>% filter(GEOID == county)
            
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
                  # Use zoo::na.approx for interpolation (handles time series better)
                  ts_data <- zoo(y_valid, x_valid)
                  # Use linear interpolation
                  interp_value <- na.approx(ts_data, xout = year, na.rm = TRUE, rule = 2)
                  new_row[[col]] <- as.numeric(interp_value)
                  
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
              
              interp_county_list[[county]] <- new_row
            }
          }
          
          # Combine all counties for this year
          if (length(interp_county_list) > 0) {
            interp_data_list[[as.character(year)]] <- bind_rows(interp_county_list)
          }
        } else {
          # Extrapolation - use linear trend from available years
          print_msg(paste("Extrapolating data for", year))
          
          # Find if we need to extrapolate forward or backward
          is_forward <- year > max(available_years)
          
          # Find boundary years for extrapolation
          if (is_forward) {
            # Forward extrapolation - use last few years
            boundary_years <- sort(tail(sort(available_years), 3))
          } else {
            # Backward extrapolation - use first few years
            boundary_years <- sort(head(sort(available_years), 3))
          }
          
          # Extract data for boundary years
          boundary_data <- combined_env_data %>%
            filter(year %in% boundary_years)
          
          # Extrapolate for each county
          county_list <- unique(boundary_data$GEOID)
          extrap_county_list <- list()
          
          for (county in county_list) {
            # Get data for this county
            county_data <- boundary_data %>% filter(GEOID == county)
            
            # If we have enough years for this county, extrapolate
            if (nrow(county_data) >= 2) {
              # Create a new row for this county and year
              new_row <- county_data[1, setdiff(names(county_data), numeric_cols)] %>%
                mutate(year = year)
              
              # For each numeric column, extrapolate with linear trend
              for (col in numeric_cols) {
                # Extract existing data points
                x <- county_data$year
                y <- county_data[[col]]
                
                # Remove NA values
                valid <- !is.na(y)
                x_valid <- x[valid]
                y_valid <- y[valid]
                
                # If we have at least 2 valid points, fit a linear model and extrapolate
                if (length(x_valid) >= 2) {
                  # Simple linear regression
                  lm_model <- lm(y_valid ~ x_valid)
                  # Predict at the extrapolation year
                  extrap_value <- predict(lm_model, newdata = data.frame(x_valid = year))
                  
                  # Apply sanity check to extrapolated values
                  # - For percentages, cap between 0 and 100
                  # - For counts, ensure not negative
                  # - For concentrations, ensure not negative
                  if (grepl("pct|percent|rate", col, ignore.case = TRUE)) {
                    extrap_value <- pmax(0, pmin(100, extrap_value))
                  } else {
                    extrap_value <- pmax(0, extrap_value)
                  }
                  
                  new_row[[col]] <- as.numeric(extrap_value)
                  
                  # Add quality flags
                  new_row[[paste0(col, "_data_quality")]] <- data_quality_flags$extrapolated
                  new_row[[paste0(col, "_data_source")]] <- 
                    county_data[[paste0(col, "_data_source")]][1]
                  new_row[[paste0(col, "_data_vintage")]] <- 
                    paste0("extrapolated_from_", paste(boundary_years, collapse="_"))
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
              
              extrap_county_list[[county]] <- new_row
            }
          }
          
          # Combine all counties for this year
          if (length(extrap_county_list) > 0) {
            interp_data_list[[as.character(year)]] <- bind_rows(extrap_county_list)
          }
        }
      }
      
      # Combine interpolated data with original data
      if (length(interp_data_list) > 0) {
        interp_data <- bind_rows(interp_data_list)
        combined_env_data <- bind_rows(combined_env_data, interp_data)
      }
    }
    
    # Filter to just the requested years
    combined_env_data <- combined_env_data %>%
      filter(year %in% years)
    
    # Sort data
    combined_env_data <- combined_env_data %>%
      arrange(GEOID, year)
    
    # Cache the processed data
    saveRDS(combined_env_data, cache_file)
    print_msg(paste("Cached EPA environmental data to:", cache_file))
    
    return(combined_env_data)
  } else if (allow_simulation) {
    # Create simulated data
    print_msg("No EPA environmental data found. Creating simulated data...")
    
    # Environmental variables to simulate
    env_vars <- c(
      "pm25_annual_mean" = "PM2.5 annual mean concentration (μg/m³)",
      "ozone_annual_mean" = "Ozone annual mean concentration (ppm)",
      "air_quality_days_unhealthy" = "Number of days with unhealthy air quality",
      "air_toxics_cancer_risk" = "Air toxics cancer risk (per million)",
      "respiratory_hazard_index" = "Respiratory hazard index",
      "proximity_to_hazardous_waste" = "Count of hazardous waste facilities within 5km",
      "proximity_to_npl_sites" = "Proximity to National Priorities List sites",
      "wastewater_discharge" = "Wastewater discharge",
      "traffic_proximity" = "Count of vehicles at major roads within 500m",
      "lead_paint_indicator" = "Percentage of housing units built pre-1960"
    )
    
    # Get county list from built-in data or create basic list
    counties <- data.frame(
      GEOID = c("01001", "01003", "01005", "01007", "01009"), # Sample counties
      NAME = c("Autauga County, Alabama", "Baldwin County, Alabama", 
               "Barbour County, Alabama", "Bibb County, Alabama", 
               "Blount County, Alabama")
    )
    
    # Try to get a more comprehensive list if possible
    tryCatch({
      # Check for tidycensus
      if (requireNamespace("tidycensus", quietly = TRUE)) {
        library(tidycensus)
        
        # Try to get counties from Census API
        if (Sys.getenv("CENSUS_API_KEY") != "") {
          counties <- tidycensus::get_decennial(
            geography = "county",
            variables = "P001001", # Total population
            year = 2020,
            geometry = FALSE
          ) %>%
            select(GEOID, NAME) %>%
            distinct()
          
          print_msg(paste("Using", nrow(counties), "counties from Census API"))
        }
      }
    }, error = function(e) {
      print_msg("Using sample county list for simulation")
    })
    
    # Create simulated data for each year
    sim_data_list <- list()
    for (year in years) {
      # Create base data frame with counties and year
      year_data <- counties %>%
        mutate(year = year)
      
      # Add simulated values for each variable
      for (var_name in names(env_vars)) {
        # Simulate values based on variable type
        if (var_name == "pm25_annual_mean") {
          # PM2.5 typically 5-20 μg/m³
          year_data[[var_name]] <- runif(nrow(year_data), 5, 20)
        } else if (var_name == "ozone_annual_mean") {
          # Ozone typically 0.02-0.08 ppm
          year_data[[var_name]] <- runif(nrow(year_data), 0.02, 0.08)
        } else if (var_name == "air_quality_days_unhealthy") {
          # Days with unhealthy air - typically 0-50
          year_data[[var_name]] <- round(runif(nrow(year_data), 0, 50))
        } else if (var_name == "air_toxics_cancer_risk") {
          # Cancer risk per million - typically 20-60
          year_data[[var_name]] <- runif(nrow(year_data), 20, 60)
        } else if (var_name == "respiratory_hazard_index") {
          # Hazard index - typically 0.5-2.0
          year_data[[var_name]] <- runif(nrow(year_data), 0.5, 2.0)
        } else if (var_name == "proximity_to_hazardous_waste") {
          # Count of facilities - typically 0-5
          year_data[[var_name]] <- round(runif(nrow(year_data), 0, 5))
        } else if (var_name == "lead_paint_indicator") {
          # Percentage - typically 0-50%
          year_data[[var_name]] <- runif(nrow(year_data), 0, 50)
        } else {
          # Default - medium positive numbers
          year_data[[var_name]] <- runif(nrow(year_data), 0, 100)
        }
        
        # Add quality flags
        year_data[[paste0(var_name, "_data_quality")]] <- data_quality_flags$simulated
        year_data[[paste0(var_name, "_data_source")]] <- "SIMULATED EPA Data"
        year_data[[paste0(var_name, "_data_vintage")]] <- paste0("simulated_", year)
      }
      
      sim_data_list[[as.character(year)]] <- year_data
    }
    
    # Combine all years
    simulated_data <- bind_rows(sim_data_list)
    
    # Cache the simulated data
    saveRDS(simulated_data, cache_file)
    print_msg(paste("Cached simulated EPA environmental data to:", cache_file))
    
    return(simulated_data)
  } else {
    # No data and simulation not allowed - create empty dataset with NAs
    print_msg("No EPA environmental data available. Creating empty dataset with NAs since simulation not allowed...")
    
    # Environmental variables we would have included
    env_vars <- c(
      "pm25_annual_mean",
      "ozone_annual_mean",
      "air_quality_days_unhealthy",
      "air_toxics_cancer_risk",
      "respiratory_hazard_index",
      "proximity_to_hazardous_waste",
      "proximity_to_npl_sites",
      "wastewater_discharge",
      "traffic_proximity",
      "lead_paint_indicator",
      "total_regulated_facilities",
      "active_facilities",
      "cwa_violations",
      "caa_violations",
      "rcra_violations",
      "sdwa_violations",
      "facilities_with_violations",
      "pct_facilities_with_violations",
      "tri_facilities",
      "tri_air_releases",
      "tri_water_releases",
      "tri_land_releases",
      "tri_total_releases"
    )
    
    # Get county list from built-in data or create basic list
    counties <- data.frame(
      GEOID = c("01001", "01003", "01005", "01007", "01009"), # Sample counties
      NAME = c("Autauga County, Alabama", "Baldwin County, Alabama", 
               "Barbour County, Alabama", "Bibb County, Alabama", 
               "Blount County, Alabama")
    )
    
    # Try to get a more comprehensive list if possible
    tryCatch({
      # Check for tidycensus
      if (requireNamespace("tidycensus", quietly = TRUE)) {
        library(tidycensus)
        
        # Try to get counties from Census API
        if (Sys.getenv("CENSUS_API_KEY") != "") {
          counties <- tidycensus::get_decennial(
            geography = "county",
            variables = "P001001", # Total population
            year = 2020,
            geometry = FALSE
          ) %>%
            select(GEOID, NAME) %>%
            distinct()
          
          print_msg(paste("Using", nrow(counties), "counties from Census API"))
        }
      }
    }, error = function(e) {
      print_msg("Using sample county list for empty dataset")
    })
    
    # Create empty data for each year
    empty_data_list <- list()
    for (year in years) {
      # Create base data frame with counties and year
      year_data <- counties %>%
        mutate(year = year)
      
      # Add NA values for each variable
      for (var_name in env_vars) {
        year_data[[var_name]] <- NA_real_
        
        # Add quality flags
        year_data[[paste0(var_name, "_data_quality")]] <- data_quality_flags$missing
        year_data[[paste0(var_name, "_data_source")]] <- "NOT_AVAILABLE"
        year_data[[paste0(var_name, "_data_vintage")]] <- NA_character_
      }
      
      empty_data_list[[as.character(year)]] <- year_data
    }
    
    # Combine all years
    empty_data <- bind_rows(empty_data_list)
    
    # Cache the empty data
    saveRDS(empty_data, cache_file)
    print_msg(paste("Cached empty EPA environmental data to:", cache_file))
    
    return(empty_data)
  }
}

# This lets the function be used when the script is sourced
is_sourced <- function() {
  return(!identical(environment(), globalenv()))
}

# If the script is run directly, test the function
if (!is_sourced()) {
  cat("Testing EPA environmental data fetcher...\n")
  
  # Get current year
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  
  # Test for last 5 years
  test_years <- (current_year-4):current_year
  
  # Define standardized data quality flags
  data_quality_flags <- list(
    direct = "direct",
    interpolated = "interpolated",
    extrapolated = "extrapolated",
    simulated = "simulated",
    missing = NA,
    imputed = "imputed"
  )
  
  # Test the function with various settings
  cat("\n----- TEST 1: With simulation allowed -----\n")
  result_sim <- fetch_epa_data(
    years = test_years,
    cache_dir = "data/cache",
    refresh_cache = FALSE,
    allow_simulation = TRUE,
    allow_interpolation = TRUE,
    data_quality_flags = data_quality_flags,
    offline_mode = FALSE
  )
  
  cat("Test 1 completed with", nrow(result_sim), "rows of data.\n")
  
  # Report data quality metrics
  if (!is.null(result_sim)) {
    cat("\nData quality metrics:\n")
    
    # Find all data quality columns
    quality_cols <- grep("_data_quality$", names(result_sim), value = TRUE)
    
    for (qcol in quality_cols[1:min(5, length(quality_cols))]) { # Limit to 5 variables to avoid excessive output
      # Get variable name without suffix
      var_name <- gsub("_data_quality$", "", qcol)
      
      # Count occurrences of each quality flag
      quality_counts <- table(result_sim[[qcol]], useNA = "ifany")
      
      cat(paste0("\n", var_name, ":\n"))
      for (flag in names(quality_counts)) {
        if (is.na(flag)) {
          cat("  Missing: ", quality_counts[[which(is.na(names(quality_counts)))]], "\n")
        } else {
          cat("  ", flag, ": ", quality_counts[[flag]], "\n")
        }
      }
    }
  }
  
  cat("\n----- TEST 2: No simulation, with interpolation -----\n")
  result_interp <- fetch_epa_data(
    years = test_years,
    cache_dir = "data/cache",
    refresh_cache = FALSE,
    allow_simulation = FALSE,
    allow_interpolation = TRUE,
    data_quality_flags = data_quality_flags,
    offline_mode = FALSE
  )
  
  cat("Test 2 completed with", nrow(result_interp), "rows of data.\n")
  
  cat("\n----- TEST 3: No simulation, no interpolation -----\n")
  result_none <- tryCatch({
    fetch_epa_data(
      years = test_years,
      cache_dir = "data/cache",
      refresh_cache = FALSE,
      allow_simulation = FALSE,
      allow_interpolation = FALSE,
      data_quality_flags = data_quality_flags,
      offline_mode = FALSE
    )
  }, error = function(e) {
    cat("Error as expected with no simulation and no interpolation:", conditionMessage(e), "\n")
    return(NULL)
  })
  
  if (!is.null(result_none)) {
    cat("Test 3 completed with", nrow(result_none), "rows of data.\n")
  }
  
  cat("\n----- TEST 4: Offline mode -----\n")
  result_offline <- fetch_epa_data(
    years = test_years,
    cache_dir = "data/cache",
    refresh_cache = FALSE,
    allow_simulation = TRUE,
    allow_interpolation = TRUE,
    data_quality_flags = data_quality_flags,
    offline_mode = TRUE
  )
  
  cat("Test 4 completed with", nrow(result_offline), "rows of data.\n")
}
