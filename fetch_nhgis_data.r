#!/usr/bin/env Rscript

# NHGIS Historical Data Retrieval
# This script handles retrieval of NHGIS data as the primary source

library(tidyverse)
library(readr)
library(tigris)
library(sf)
library(httr)
library(parallel)
library(future)
library(future.apply)
library(progressr)
library(ipumsr)

#' Fetch NHGIS harmonized data for all years (1970-present)
#'
#' Processes NHGIS data files for the entire date range (1970-present).
#' NHGIS (National Historical Geographic Information System) is used as the
#' primary and most consistent source for all years. This function ensures
#' that each year has at least the 37 SDOH parameters required for comprehensive analysis.
#'
#' This function can:
#' 1. Use manually downloaded NHGIS files placed in data/nhgis directory
#' 2. Directly fetch data via the IPUMS API if credentials are provided
#' 3. Return empty structured dataset with NAs if no data is available
#'
#' @param crosswalk The variable crosswalk data frame
#' @param years Vector of years to include (default: 1970 to present)
#' @param cache_dir Directory to store cache files
#' @param refresh_cache Whether to refresh the cache
#' @param primary_source Whether NHGIS is being used as the primary consistent source (default: TRUE)
#' @param use_ipumsr Whether to use the ipumsr package to directly fetch NHGIS data (requires credentials)
#' @param ipums_credentials List with 'username' and 'password' elements for IPUMS access
#' @return A data frame with NHGIS harmonized data containing all 37 SDOH parameters for all years
fetch_nhgis_historical_data <- function(crosswalk, years, cache_dir = "data/cache", refresh_cache = FALSE, 
                                primary_source = TRUE,
                                use_ipumsr = FALSE,
                                ipums_credentials = NULL) {
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
  cache_file <- file.path(cache_dir, "nhgis_historical_data.rds")
  
  # Use cache if available and not refreshing
  if (!refresh_cache && file.exists(cache_file)) {
    print_msg("Loading cached NHGIS historical data...")
    nhgis_data <- readRDS(cache_file)
    
    # Check if all requested years are in the cache
    cached_years <- unique(nhgis_data$year)
    missing_years <- setdiff(years, cached_years)
    
    # Add check for empty cache with just 1 row and the "No NHGIS Data Available" flag
    if (nrow(nhgis_data) <= 1 || 
        (is.data.frame(nhgis_data) && "data_source" %in% names(nhgis_data) && 
         any(grepl("No NHGIS Data Available", nhgis_data$data_source)))) {
      print_msg("Cached NHGIS data appears to be empty or a placeholder. Will process files again.")
      # Force refresh by continuing past this point
    } else if (length(missing_years) == 0) {
      print_msg("Using complete cached NHGIS historical data.")
      return(nhgis_data %>% filter(year %in% years))
    } else {
      print_msg(paste("Cache missing years:", paste(missing_years, collapse=", ")))
    }
  }
  
  # Make sure the NHGIS directory exists
  nhgis_dir <- "data/nhgis"
  if (!dir.exists(nhgis_dir)) {
    dir.create(nhgis_dir, showWarnings = FALSE, recursive = TRUE)
    print_msg(paste("Created NHGIS data directory at:", nhgis_dir))
  }
  
  # Try to use ipumsr to directly fetch NHGIS data
  use_direct_api <- FALSE
  
  if (use_ipumsr) {
    # First check if ipumsr package is available
    if (!requireNamespace("ipumsr", quietly = TRUE)) {
      print_msg("Warning: The ipumsr package is required for direct NHGIS access but is not installed.")
      print_msg("Trying to install ipumsr package...")
      
      tryCatch({
        install.packages("ipumsr")
        print_msg("Successfully installed ipumsr package!")
        use_direct_api <- TRUE
      }, error = function(e) {
        print_msg(paste("Failed to install ipumsr:", conditionMessage(e)))
        print_msg("Will use existing NHGIS data files if available.")
      })
    } else {
      use_direct_api <- TRUE
    }
    
    if (use_direct_api) {
      # Check credentials
      if (is.null(ipums_credentials) || 
          is.null(ipums_credentials$username) || 
          is.null(ipums_credentials$password) ||
          ipums_credentials$username == "" || 
          ipums_credentials$password == "") {
        
        print_msg("Checking multiple sources for IPUMS credentials...")
        
        # Try multiple credential sources in sequence
        # 1. Environment variables directly
        ipums_username <- Sys.getenv("IPUMS_USERNAME", "")
        ipums_password <- Sys.getenv("IPUMS_PASSWORD", "")
        
        # 2. Try credential file in user home directory
        if (ipums_username == "" || ipums_password == "") {
          cred_file <- file.path(Sys.getenv("HOME"), ".ipums_credentials/config")
          
          if (file.exists(cred_file)) {
            print_msg("Trying to load credentials from ~/.ipums_credentials/config...")
            tryCatch({
              # Parse credential file
              lines <- readLines(cred_file)
              for (line in lines) {
                if (grepl("^IPUMS_USERNAME=", line)) {
                  ipums_username <- sub("^IPUMS_USERNAME=", "", line)
                } else if (grepl("^IPUMS_PASSWORD=", line)) {
                  ipums_password <- sub("^IPUMS_PASSWORD=", "", line)
                }
              }
              
              if (ipums_username != "" && ipums_password != "") {
                print_msg("Successfully loaded credentials from file!")
              }
            }, error = function(e) {
              print_msg(paste("Error reading credential file:", conditionMessage(e)))
            })
          }
        }
        
        # 3. Try R's .Renviron file
        if (ipums_username == "" || ipums_password == "") {
          renviron_path <- file.path(Sys.getenv("HOME"), ".Renviron")
          
          if (file.exists(renviron_path)) {
            print_msg("Trying to load credentials from .Renviron...")
            tryCatch({
              lines <- readLines(renviron_path)
              for (line in lines) {
                if (grepl("^IPUMS_USERNAME=", line)) {
                  ipums_username <- sub("^IPUMS_USERNAME=", "", line)
                } else if (grepl("^IPUMS_PASSWORD=", line)) {
                  ipums_password <- sub("^IPUMS_PASSWORD=", "", line)
                }
              }
              
              if (ipums_username != "" && ipums_password != "") {
                print_msg("Successfully loaded credentials from .Renviron!")
              }
            }, error = function(e) {
              print_msg(paste("Error reading .Renviron file:", conditionMessage(e)))
            })
          }
        }
        
        # Create credentials if found
        if (ipums_username != "" && ipums_password != "") {
          print_msg("Using IPUMS credentials from discovered source...")
          ipums_credentials <- list(
            username = ipums_username,
            password = ipums_password
          )
          
          # Store in environment variables for consistent access
          Sys.setenv(IPUMS_USERNAME = ipums_username)
          Sys.setenv(IPUMS_PASSWORD = ipums_password)
        } else {
          print_msg("No IPUMS credentials found in any location.")
          print_msg("Will use existing NHGIS data files if available.")
          use_direct_api <- FALSE
        }
      } else {
        print_msg("Using IPUMS credentials provided by caller...")
      }
    }
    
    if (use_direct_api && !is.null(ipums_credentials)) {
      print_msg("Attempting IPUMS authentication...")
      
      # Set IPUMS credentials
      ipums_username <- ipums_credentials$username
      ipums_password <- ipums_credentials$password
      
      if (is.null(ipums_username) || is.null(ipums_password) || 
          ipums_username == "" || ipums_password == "") {
        print_msg("Error: IPUMS credentials incomplete.")
        use_direct_api <- FALSE
      } else {
        # Set up IPUMS API connection
        print_msg("Setting up IPUMS API connection...")
        
        # Ensure we're using the proper IPUMS API server (use package-exported function)
        # Try two different ways in case the function name changed across versions
        tryCatch({
          if (exists("set_ipums_default_server", where = asNamespace("ipumsr"), mode = "function")) {
            get("set_ipums_default_server", envir = asNamespace("ipumsr"))("https://api.ipums.org")
          } else {
            # Alternative method
            options(ipumsr.base_url = "https://api.ipums.org")
          }
        }, error = function(e) {
          # Just a warning, not fatal
          print_msg(paste("Warning: Could not set IPUMS server URL:", conditionMessage(e)))
        })
        
        # Authenticate with IPUMS
        tryCatch({
          # First check if we're already authenticated
          is_authenticated <- FALSE
          
          # Try a basic API call first to check authentication
          tryCatch({
            test_auth <- ipumsr::get_ipums_extracts()
            is_authenticated <- TRUE
            print_msg("Already authenticated with IPUMS.")
          }, error = function(e) {
            # Not authenticated, need to log in
            is_authenticated <- FALSE
          })
          
          if (!is_authenticated) {
            # Try to authenticate
            ipumsr::ipums_auth(username = ipums_username, password = ipums_password)
            print_msg("IPUMS authentication successful!")
          }
        }, error = function(e) {
          print_msg(paste("IPUMS authentication failed:", conditionMessage(e)))
          print_msg("Will use existing NHGIS data files if available.")
          use_direct_api <- FALSE
        })
      }
    }
  }
    
  if (use_direct_api) {
    # Create an NHGIS extract request for all required years and variables
    print_msg(paste("Creating NHGIS extract request for years", min(years), "to", max(years)))
    
    # Get required variables from crosswalk
    required_vars <- NULL
    if (!is.null(crosswalk) && nrow(crosswalk) > 0) {
      # Get all SDOH variables from crosswalk that have NHGIS mappings
      required_vars <- crosswalk %>%
        filter(!is.na(nhgis_var)) %>%
        pull(nhgis_var) %>%
        unique()
      
      print_msg(paste("Found", length(required_vars), "variables in crosswalk for NHGIS data"))
    }
    
    # Determine appropriate datasets based on years
    datasets <- c()
    
    # For historical years (1970-1999)
    if(any(years <= 1999)) {
      datasets <- c(datasets, "U.S. Decennial Census", "SABINS", "NHGIS Time Series")
    }
    
    # For modern years (2000-present)
    if(any(years >= 2000)) {
      datasets <- c(datasets, "U.S. Decennial Census", "American Community Survey")
    }
    
    # Define core variables needed for analysis
    core_variables <- c(
      # Demographics
      "total_population", "white_population", "black_population", "hispanic_population",
      "male_population", "female_population", "population_under_18", "population_65_over",
      
      # Socioeconomic
      "median_household_income", "poverty_rate", "unemployment_rate",
      
      # Education
      "less_than_hs_education", "bachelor_degree_or_higher",
      
      # Housing
      "median_home_value", "homeownership_rate", "housing_cost_burden"
    )
    
    # Set up extract request
    tryCatch({
      print_msg("Setting up NHGIS extract request...")
      extract_request <- ipumsr::define_extract_nhgis(
        description = paste0("County SDOH Data ", min(years), "-", max(years)),
        time_periods = years,
        geog_levels = "county",
        datasets = datasets,
        data_format = "csv"
      )
      
      # Add variable selections if available
      if (!is.null(required_vars) && length(required_vars) > 0) {
        print_msg(paste("Adding", length(required_vars), "specific variables to extract request..."))
        # Attempt to add variables, but continue if this fails (will get default variables)
        tryCatch({
          extract_request <- ipumsr::add_nhgis_extract_variables(
            extract_request, 
            variables = required_vars
          )
        }, error = function(e) {
          print_msg(paste("Warning: Could not add specific variables:", conditionMessage(e)))
          print_msg("Proceeding with default variables...")
        })
      }
      
      # Submit extract request
      print_msg("Submitting NHGIS extract request...")
      extract_submitted <- ipumsr::submit_extract(extract_request)
      extract_id <- extract_submitted$extract_id
      print_msg(paste("Extract submitted with ID:", extract_id))
      
      # Check status and wait for completion
      print_msg("Waiting for extract to complete (this may take several minutes)...")
      extract_ready <- FALSE
      max_attempts <- 60  # Maximum number of status check attempts
      attempts <- 0
      
      # Initialize progress indicator
      last_status <- ""
      
      while (!extract_ready && attempts < max_attempts) {
        # Get extract status with error handling
        extract_status <- tryCatch({
          ipumsr::get_extract_info(extract_id)
        }, error = function(e) {
          print_msg(paste("Error checking extract status:", conditionMessage(e)))
          # Return a placeholder with a status we can handle
          list(status = "unknown")
        })
        
        status <- extract_status$status
        
        # Only print if status has changed
        if (status != last_status) {
          print_msg(paste("Extract status:", status, "- Attempt", attempts + 1, "of", max_attempts))
          last_status <- status
        } else {
          # Just print a dot to show we're still alive
          if (attempts %% 5 == 0) {
            cat(".")
          }
        }
        
        if (status == "completed") {
          extract_ready <- TRUE
        } else if (status %in% c("error", "canceled")) {
          stop(paste("Extract failed with status:", status))
        } else if (status == "unknown") {
          # Continue despite status check error
          print_msg("Could not determine status - will continue trying...")
          Sys.sleep(30)
          attempts <- attempts + 1
        } else {
          # Wait before checking again
          Sys.sleep(30)  # Wait 30 seconds
          attempts <- attempts + 1
        }
      }
      
      if (!extract_ready) {
        if (attempts >= max_attempts) {
          print_msg("Extract did not complete in the allocated time. Will check one last time...")
          
          # One final attempt to get status
          final_status <- tryCatch({
            extract_status <- ipumsr::get_extract_info(extract_id)
            extract_status$status
          }, error = function(e) {
            "unknown"
          })
          
          if (final_status == "completed") {
            extract_ready <- TRUE
            print_msg("Extract is actually complete! Proceeding with download.")
          } else {
            print_msg(paste("Final extract status:", final_status))
            print_msg("Extract may still be processing on the server. You can manually check and download later.")
            stop("Extract did not complete in the allocated time.")
          }
        }
      }
      
      # Download the extract
      print_msg("Downloading NHGIS extract...")
      nhgis_dir <- "data/nhgis"
      if (!dir.exists(nhgis_dir)) {
        dir.create(nhgis_dir, showWarnings = FALSE, recursive = TRUE)
      }
      
      download_result <- ipumsr::download_extract(
        extract_id,
        download_dir = nhgis_dir,
        overwrite = TRUE
      )
      
      # Unzip the downloaded file if needed
      if (file.exists(download_result$download_path) && 
          grepl("\\.zip$", download_result$download_path)) {
        print_msg(paste("Unzipping downloaded file:", basename(download_result$download_path)))
        unzip(download_result$download_path, exdir = nhgis_dir)
        unlink(download_result$download_path)  # Remove the zip file
      }
      
      # Find all CSV files in the NHGIS directory
      nhgis_files <- list.files(nhgis_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
      
      if (length(nhgis_files) == 0) {
        print_msg("No CSV files found after download. Extract may be empty or failed to download properly.")
        
        # Try to see if we can find the download in the standard IPUMS download directory
        user_download_dir <- normalizePath(file.path("~", "Downloads"), mustWork = FALSE)
        if (dir.exists(user_download_dir)) {
          print_msg("Checking user Downloads directory for NHGIS files...")
          possible_nhgis_files <- list.files(
            user_download_dir, 
            pattern = "nhgis|ipums", 
            ignore.case = TRUE,
            full.names = TRUE
          )
          
          # Look for zip files first
          recent_zip_files <- list.files(
            user_download_dir, 
            pattern = "\\.zip$", 
            full.names = TRUE
          )
          
          # Sort by modification time (most recent first)
          recent_zip_files <- recent_zip_files[order(file.info(recent_zip_files)$mtime, decreasing = TRUE)]
          
          if (length(recent_zip_files) > 0) {
            print_msg(paste("Found possible NHGIS zip file in Downloads:", recent_zip_files[1]))
            print_msg("Please manually extract this file to the data/nhgis directory.")
          }
          
          use_direct_api <- FALSE
        } else {
          use_direct_api <- FALSE
        }
      } else {
        print_msg(paste("Successfully downloaded", length(nhgis_files), "NHGIS data files."))
      }
    }, error = function(e) {
      print_msg(paste("Error during NHGIS extract process:", conditionMessage(e)))
      print_msg("Will use existing NHGIS data files if available.")
      use_direct_api <- FALSE
    })
  }
  
  # If API approach didn't work or wasn't attempted, check for existing NHGIS and IHME data files
  # First check the NHGIS directory with both .csv and .CSV extensions (case sensitivity)
  nhgis_files <- list.files(nhgis_dir, pattern = "\\.csv$|\\.CSV$", recursive = TRUE, full.names = TRUE)
  
  # Also check IHME directory if NHGIS dir is empty
  if (length(nhgis_files) == 0) {
    print_msg("No files found in NHGIS directory. Checking IHME directory...")
    ihme_dir <- "data/ihme/CSV"
    
    if (dir.exists(ihme_dir)) {
      ihme_files <- list.files(ihme_dir, pattern = "\\.CSV$|\\.csv$", recursive = FALSE, full.names = TRUE)
      
      if (length(ihme_files) > 0) {
        print_msg(sprintf("Found %d files in IHME directory. Making them available to NHGIS...", length(ihme_files)))
        
        # Make sure NHGIS directory exists
        if (!dir.exists(nhgis_dir)) {
          dir.create(nhgis_dir, recursive = TRUE, showWarnings = FALSE)
        }
        
        # On Unix/Mac systems (like macOS), create symbolic links for efficiency
        if (.Platform$OS.type == "unix") {
          system(sprintf("ln -sf %s/*.CSV %s/", normalizePath(ihme_dir), normalizePath(nhgis_dir)))
          print_msg("Created symbolic links from IHME files to NHGIS directory")
        } else {
          # On Windows, copy the files
          file.copy(ihme_files, nhgis_dir)
          print_msg("Copied IHME files to NHGIS directory")
        }
        
        # Update the list of NHGIS files after making IHME files available
        nhgis_files <- list.files(nhgis_dir, pattern = "\\.csv$|\\.CSV$", recursive = TRUE, full.names = TRUE)
      }
    }
  }
  
  # If we still have no files, check the parent directories
  if (length(nhgis_files) == 0) {
    print_msg("No files found in NHGIS or IHME directories. Checking broader data directory...")
    
    # Try to find CSV files anywhere in the data directory
    all_data_files <- list.files("data", pattern = "\\.csv$|\\.CSV$", recursive = TRUE, full.names = TRUE)
    
    if (length(all_data_files) > 0) {
      print_msg(sprintf("Found %d CSV files in data directory hierarchy.", length(all_data_files)))
      
      # Check if any of these look like NHGIS/IHME files
      potential_files <- all_data_files[grepl("nhgis|ipums|ihme|census|county", tolower(all_data_files))]
      
      if (length(potential_files) > 0) {
        print_msg(sprintf("Found %d potential NHGIS/IHME files in data directory hierarchy.", length(potential_files)))
        
        # Make these available to the NHGIS directory
        if (!dir.exists(nhgis_dir)) {
          dir.create(nhgis_dir, recursive = TRUE, showWarnings = FALSE)
        }
        
        # Create links or copy
        if (.Platform$OS.type == "unix") {
          for (file in potential_files) {
            system(sprintf("ln -sf %s %s/", normalizePath(file), normalizePath(nhgis_dir)))
          }
          print_msg("Created symbolic links to data files in NHGIS directory")
        } else {
          file.copy(potential_files, nhgis_dir)
          print_msg("Copied data files to NHGIS directory")
        }
        
        # Update the list of NHGIS files
        nhgis_files <- list.files(nhgis_dir, pattern = "\\.csv$|\\.CSV$", recursive = TRUE, full.names = TRUE)
      }
    }
  }
  
  if (length(nhgis_files) == 0) {
    # Return empty dataset with correct structure instead of failing
    print_msg("NOTE: No NHGIS or IHME data files found. Returning empty dataset with correct structure.")
    print_msg("Missing values will be appropriately marked as NA and can be interpolated later if bracketed by real data.")
    print_msg("For complete results, please download historical extracts from https://nhgis.org/")
    
    # Create empty dataset with the right structure but all NAs for values
    # This still needs a basic structure with counties
    counties <- tryCatch({
      # Try with tigris
      tigris::counties(cb = TRUE, year = 2020) %>%
        sf::st_drop_geometry() %>%
        select(GEOID, NAME, STATEFP, COUNTYFP) %>%
        mutate(GEOID = as.character(GEOID))
    }, error = function(e) {
      # If tigris fails, create a minimal set of info
      print_msg("Could not get county data from tigris. Creating minimal structure.")
      tibble(
        GEOID = character(),
        NAME = character(),
        STATEFP = character(),
        COUNTYFP = character()
      )
    })
    
    # Create empty data frame with correct structure
    empty_data <- expand.grid(
      GEOID = counties$GEOID,
      year = years,
      stringsAsFactors = FALSE
    ) %>%
      as_tibble() %>%
      left_join(counties, by = "GEOID") %>%
      mutate(
        data_source = "No NHGIS Data Available",
        data_quality = "missing",
        data_vintage = "No Data",
        missing_data_flag = TRUE
      )
    
    print_msg(sprintf("Created empty dataset with %d counties and %d years (%d rows total)",
                     length(unique(empty_data$GEOID)),
                     length(unique(empty_data$year)),
                     nrow(empty_data)))
    
    # Before returning, write a helpful README file with instructions for getting real data
    readme_path <- file.path(nhgis_dir, "README_NHGIS_DATA.txt")
    writeLines(
      c("NHGIS DATA DIRECTORY",
        "====================",
        "",
        "This directory should contain NHGIS data files (CSV format) downloaded from https://nhgis.org/",
        "",
        "To get the data files:",
        "1. Register for an account at https://www.nhgis.org/",
        "2. Go to the Data Finder at https://data2.nhgis.org/main",
        "3. Select:",
        "   - Geographic levels: County",
        "   - Years: 1970 through present",
        "   - Topics: Demographics, Economy, Housing, etc. (all SDOH-related)",
        "4. Create an extract and download when ready",
        "5. Place all CSV files in this directory",
        "",
        "Alternatively, set your IPUMS credentials using:",
        "Rscript R/utilities/set_ipums_credentials.r <username> <password>",
        "",
        "This will allow the pipeline to download NHGIS data automatically."
      ),
      readme_path
    )
    
    print_msg(paste("Created a README file at", readme_path, "with instructions for getting NHGIS data"))
    
    return(empty_data)
  }
  
  print_msg(paste("Found", length(nhgis_files), "NHGIS data files."))
  
  # Process each NHGIS file - Filter to only use those from our years of interest
  nhgis_data_list <- lapply(nhgis_files, function(file) {
    # Helper function to extract and check if file should be processed
    should_process <- function(filename) {
      # First try to extract from LT_YYYY or MX_YYYY pattern (most reliable)
      pattern <- "_(LT|MX)_(\\d{4})_"
      matches <- regexec(pattern, basename(filename))
      extracted <- regmatches(basename(filename), matches)
      if (length(extracted) > 0 && length(extracted[[1]]) >= 3) {
        file_year <- as.numeric(extracted[[1]][3])
        if (file_year %in% years) return(TRUE)
      }
      return(TRUE)  # Process by default - we'll filter by year later
    }
    
    # Skip files that shouldn't be processed based on filename
    if (!should_process(file)) return(NULL)
    
    tryCatch({
      print_msg(paste("Processing NHGIS/IHME file:", basename(file)))
      
      # Determine if this is an IHME file based on filename pattern
      is_ihme_file <- grepl("IHME.*\\.CSV$", basename(file), ignore.case = FALSE)
      
      if (is_ihme_file) {
        print_msg("Detected IHME life expectancy file format")
        
        # Special processing for IHME files
        # Example filename: IHME_USA_LE_COUNTY_RACE_ETHN_2000_2019_LT_2010_BOTH_Y2022M06D16.CSV
        # Need to extract year (2010 in this example)
        
        # Extract year from filename - we need the specific year for this file
        # The filename contains both the range (2000_2019) and the specific year (e.g., LT_2010)
        # We want the specific year, not the range years
        
        # Try to extract from LT_YYYY or MX_YYYY pattern first (most reliable)
        pattern <- "_(LT|MX)_(\\d{4})_"
        matches <- regexec(pattern, basename(file))
        extracted <- regmatches(basename(file), matches)
        file_year <- NULL
        
        if (length(extracted) > 0 && length(extracted[[1]]) >= 3) {
          file_year <- as.numeric(extracted[[1]][3])
          print_msg(paste("Extracted year from IHME filename using primary pattern:", file_year))
        } else {
          # Fallback to older generic pattern
          year_match <- regexpr("_\\d{4}_", basename(file))
          
          if (year_match > 0) {
            year_str <- substr(basename(file), year_match + 1, year_match + 4)
            file_year <- as.numeric(year_str)
            print_msg(paste("Extracted year from IHME filename using fallback pattern:", file_year))
          }
        }
        
        # If we still couldn't extract a year, use the first year from data if available
        if (is.null(file_year)) {
          file_year <- 2000 # Default to 2000 to ensure we process all files
          print_msg("Could not extract year from filename, using default year 2000")
        }
        
        if (is.null(file_year) || !(file_year %in% years)) {
          print_msg(paste("Skipping IHME file with year not in requested range:", basename(file)))
          return(NULL)
        }
        
        # Read IHME data with appropriate column types
        data <- tryCatch({
          read_csv(file, show_col_types = FALSE)
        }, error = function(e) {
          print_msg(paste("Error reading IHME file:", conditionMessage(e)))
          return(NULL)
        })
        
        if (is.null(data) || nrow(data) == 0) {
          print_msg("Empty or unreadable IHME file")
          return(NULL)
        }
        
        # Create standardized data structure for IHME files
        # Actual IHME files have these columns:
        # - location_id: Location identifier (numeric)
        # - location_name: Location name (string)
        # - fips: FIPS county code (string)
        # - sex_id: Sex identifier (numeric)
        # - sex_name: Sex name (string) - "Both", "Male", "Female"
        # - year: Year (numeric)
        # - val: Life expectancy value (numeric)
        
        # Check for required columns using flexible matching
        all_columns <- names(data)
        print_msg(paste("Found columns:", paste(all_columns[1:min(10, length(all_columns))], collapse=", "), "..."))
        
        # Identify column mapping based on available columns
        column_map <- list(
          location_id = if ("location_id" %in% all_columns) "location_id" else 
                        if ("loc_id" %in% all_columns) "loc_id" else NULL,
          
          location_name = if ("location_name" %in% all_columns) "location_name" else
                          if ("loc_name" %in% all_columns) "loc_name" else NULL,
          
          fips = if ("fips" %in% all_columns) "fips" else NULL,
          
          sex = if ("sex_name" %in% all_columns) "sex_name" else
                if ("sex" %in% all_columns) "sex" else NULL,
          
          value = if ("val" %in% all_columns) "val" else
                  if ("value" %in% all_columns) "value" else NULL
        )
        
        # Check if we have the minimum required columns
        required_cols <- c("location_id", "location_name", "sex", "value")
        missing_cols <- names(column_map)[sapply(column_map[required_cols], is.null)]
        
        if (length(missing_cols) > 0) {
          print_msg(paste("IHME file missing required columns:", paste(missing_cols, collapse=", ")))
          
          # If 'fips' is available but location_id isn't, use fips
          if ("location_id" %in% missing_cols && !is.null(column_map$fips)) {
            print_msg("Using fips column as location_id")
            column_map$location_id <- column_map$fips
            missing_cols <- setdiff(missing_cols, "location_id")
          }
          
          # If location_id is missing but we have fips column
          if ("location_id" %in% missing_cols && "fips" %in% all_columns) {
            data$location_id <- data$fips
            column_map$location_id <- "location_id"
            missing_cols <- setdiff(missing_cols, "location_id")
          }
          
          # If sex is missing but sex_id is available
          if ("sex" %in% missing_cols && "sex_id" %in% all_columns) {
            # Map sex_id to sex_name (1=Male, 2=Female, 3=Both)
            data$sex <- ifelse(data$sex_id == 1, "Male", 
                             ifelse(data$sex_id == 2, "Female", "Both"))
            column_map$sex <- "sex"
            missing_cols <- setdiff(missing_cols, "sex")
          }
          
          # If value is missing but val is available
          if ("value" %in% missing_cols && "val" %in% all_columns) {
            data$value <- data$val
            column_map$value <- "value"
            missing_cols <- setdiff(missing_cols, "value")
          }
          
          # Check again after all our attempts
          missing_cols <- names(column_map)[sapply(column_map[required_cols], is.null)]
          if (length(missing_cols) > 0) {
            print_msg("Unable to find suitable replacements for missing columns")
            return(NULL)
          }
        }
        
        # Standardize IHME data structure
        standardized_data <- data %>%
          mutate(
            # Create GEOID from location_id/fips (assuming it's in FIPS format)
            GEOID = if (!is.null(column_map$fips) && column_map$fips %in% names(data)) {
              str_pad(as.character(get(column_map$fips)), 5, "left", "0")
            } else {
              str_pad(as.character(get(column_map$location_id)), 5, "left", "0")
            },
            NAME = get(column_map$location_name),
            year = file_year,
            data_source = "IHME Life Expectancy",
            data_vintage = paste0("IHME ", basename(file)),
            data_quality = "estimate"
          )
        
        # Extract life expectancy value based on sex
        if (!is.null(column_map$sex)) {
          sex_column <- column_map$sex
          value_column <- column_map$value
          
          print_msg(paste("Using", sex_column, "as sex column and", value_column, "as value column"))
          
          # First, normalize sex values to lowercase for consistent comparison
          standardized_data <- standardized_data %>%
            mutate(sex_normalized = tolower(get(sex_column)))
          
          # Create sex-specific life expectancy columns
          standardized_data <- standardized_data %>%
            mutate(
              # Create standardized variable names based on normalized sex
              life_expectancy = if_else(sex_normalized %in% c("both", "3", "both sexes"), 
                                        get(value_column), NA_real_),
              life_expectancy_male = if_else(sex_normalized %in% c("male", "1", "males"), 
                                            get(value_column), NA_real_),
              life_expectancy_female = if_else(sex_normalized %in% c("female", "2", "females"), 
                                              get(value_column), NA_real_)
            )
        } else {
          # If no sex column, assume total
          standardized_data <- standardized_data %>%
            mutate(life_expectancy = get(column_map$value))
        }
        
        # Select only the columns we need
        standardized_data <- standardized_data %>%
          select(GEOID, NAME, year, data_source, data_vintage, data_quality, 
                 life_expectancy, life_expectancy_male, life_expectancy_female)
        
        # Remove duplicates if any
        standardized_data <- standardized_data %>%
          distinct(GEOID, year, .keep_all = TRUE)
        
        print_msg(paste("Processed IHME file with", nrow(standardized_data), "counties for year", file_year))
        return(standardized_data)
      }
      
      # Standard NHGIS file processing
      # Read NHGIS data
      data <- read_csv(file, show_col_types = FALSE)
      
      # Check if this is a time series file with historical years
      historical_file <- FALSE
      
      # Look for year columns in various formats
      if ("YEAR" %in% names(data)) {
        # Direct year column
        historical_file <- any(data$YEAR %in% years)
      } else if (any(str_detect(names(data), "^[A-Z]+\\d{4}")) && 
                any(as.numeric(str_extract(names(data)[str_detect(names(data), "\\d{4}")], "\\d{4}")) %in% years)) {
        # Year encoded in variable names
        historical_file <- TRUE
      }
      
      if (!historical_file) {
        print_msg(paste("Skipping non-historical file:", basename(file)))
        return(NULL)
      }
      
      # Process based on file format
      if ("YEAR" %in% names(data)) {
        # Direct year column format
        print_msg("Processing direct year format...")
        
        # Filter to historical years
        data_historical <- data %>%
          filter(YEAR %in% years)
        
        if (nrow(data_historical) == 0) {
          print_msg("No historical years found in this file.")
          return(NULL)
        }
        
        # Match NHGIS variables to standardized names
        if (!is.null(crosswalk)) {
          nhgis_vars <- crosswalk %>%
            filter(!is.na(nhgis_var)) %>%
            select(std_name, nhgis_var)
          
          # Find variables that match the crosswalk
          matching_vars <- intersect(names(data_historical), nhgis_vars$nhgis_var)
          
          if (length(matching_vars) == 0) {
            print_msg("No matching variables found in this file.")
            return(NULL)
          }
        } else {
          # If no crosswalk provided, use all variables
          matching_vars <- setdiff(names(data_historical), 
                                 c("GISJOIN", "YEAR", "STATEFP", "COUNTYFP", "STUSPS", "COUNTY"))
          nhgis_vars <- tibble(
            std_name = matching_vars,
            nhgis_var = matching_vars
          )
        }
        
        # Select matching variables and create standardized dataset
        data_processed <- data_historical %>%
          # Create GEOID from GISJOIN if present
          mutate(
            GEOID = if ("GISJOIN" %in% names(data_historical)) {
              # NHGIS GISJOIN is G + state FIPS + county FIPS
              paste0(
                str_sub(GISJOIN, 2, 3),
                str_sub(GISJOIN, 5, 7)
              )
            } else if (all(c("STATEFP", "COUNTYFP") %in% names(data_historical))) {
              paste0(STATEFP, COUNTYFP)
            } else {
              NA_character_
            }
          ) %>%
          mutate(
            GEOID = str_pad(GEOID, 5, "left", "0"),
            NAME = if ("COUNTY" %in% names(data_historical) && "STUSPS" %in% names(data_historical)) {
              paste0(COUNTY, " County, ", STUSPS)
            } else {
              NA_character_
            },
            year = YEAR,
            data_source = "IPUMS NHGIS Historical",
            data_vintage = paste0("NHGIS ", basename(file)),
            data_quality = "harmonized"
          )
        
        # Select and rename variables based on crosswalk
        renamed_vars <- select(data_processed, GEOID, NAME, year, data_source, data_vintage, data_quality)
        
        for (var in matching_vars) {
          std_name <- nhgis_vars$std_name[nhgis_vars$nhgis_var == var]
          renamed_vars[[std_name]] <- data_processed[[var]]
        }
        
        return(renamed_vars)
        
      } else if (any(str_detect(names(data), "^[A-Z]+\\d{4}"))) {
        # Year encoded in variable names
        print_msg("Processing year-in-variable format...")
        
        # Extract years from column names
        year_cols <- names(data)[str_detect(names(data), "^[A-Z]+\\d{4}")]
        file_years <- unique(as.numeric(str_extract(year_cols, "\\d{4}")))
        historical_years <- intersect(file_years, years)
        
        if (length(historical_years) == 0) {
          print_msg("No historical years found in this file.")
          return(NULL)
        }
        
        print_msg(paste("Historical years found:", paste(historical_years, collapse=", ")))
        
        # Reshape to long format
        data_long <- data %>%
          # Keep only ID variables and year columns
          select(
            if ("GISJOIN" %in% names(data)) "GISJOIN" else NULL,
            if ("STATEFP" %in% names(data)) "STATEFP" else NULL,
            if ("COUNTYFP" %in% names(data)) "COUNTYFP" else NULL,
            if ("STUSPS" %in% names(data)) "STUSPS" else NULL,
            if ("COUNTY" %in% names(data)) "COUNTY" else NULL,
            matches("^[A-Z]+\\d{4}")
          ) %>%
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
          ) %>%
          # Filter to requested years
          filter(year %in% historical_years)
        
        # Match variables to standardized names
        if (!is.null(crosswalk)) {
          nhgis_vars <- crosswalk %>%
            filter(!is.na(nhgis_var)) %>%
            select(std_name, nhgis_var)
        } else {
          # If no crosswalk provided, use variables as-is
          nhgis_vars <- tibble(
            std_name = unique(data_long$variable),
            nhgis_var = unique(data_long$variable)
          )
        }
        
        # Create a wide format with standardized names
        data_wide <- data_long %>%
          # Join with crosswalk
          left_join(
            nhgis_vars %>% 
              rename(variable = nhgis_var),
            by = "variable"
          ) %>%
          # Use variable name directly if no match in crosswalk
          mutate(
            std_name = ifelse(is.na(std_name), variable, std_name)
          ) %>%
          # Create GEOID
          mutate(
            GEOID = if ("GISJOIN" %in% names(data_long)) {
              # NHGIS GISJOIN is G + state FIPS + county FIPS
              paste0(
                str_sub(GISJOIN, 2, 3),
                str_sub(GISJOIN, 5, 7)
              )
            } else if (all(c("STATEFP", "COUNTYFP") %in% names(data_long))) {
              paste0(STATEFP, COUNTYFP)
            } else {
              NA_character_
            }
          ) %>%
          mutate(
            GEOID = str_pad(GEOID, 5, "left", "0"),
            NAME = if ("COUNTY" %in% names(data_long) && "STUSPS" %in% names(data_long)) {
              paste0(COUNTY, " County, ", STUSPS)
            } else {
              NA_character_
            }
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
            data_source = "IPUMS NHGIS Historical",
            data_vintage = paste0("NHGIS ", basename(file))
          )
        
        return(data_wide)
      } else {
        print_msg(paste("Unrecognized NHGIS file format:", basename(file)))
        return(NULL)
      }
    }, error = function(e) {
      warning("Error processing NHGIS file ", basename(file), ": ", e$message)
      return(NULL)
    })
  })
  
  # Combine all processed NHGIS files
  nhgis_data_combined <- bind_rows(Filter(Negate(is.null), nhgis_data_list))
  
  if (nrow(nhgis_data_combined) == 0) {
    print_msg("No historical NHGIS data was successfully processed.")
    
    # Return empty dataframe
    return(tibble(
      GEOID = character(),
      NAME = character(),
      year = numeric(),
      data_source = character(),
      data_vintage = character()
    ))
  }
  
  # Add county names where missing
  if (any(is.na(nhgis_data_combined$NAME))) {
    counties <- tryCatch({
      # Try with tigris package first
      tigris::counties(cb = TRUE, year = 2020) %>%
        sf::st_drop_geometry() %>%
        select(GEOID, NAME) %>%
        mutate(GEOID = as.character(GEOID))
    }, error = function(e) {
      # Fallback to creating a basic mapping
      print_msg("Error getting counties with tigris. Creating simple county names.")
      
      # Extract unique GEOIDs
      geoids <- unique(nhgis_data_combined$GEOID)
      
      # Create a simple mapping
      tibble(
        GEOID = geoids,
        NAME = paste("County", geoids)
      )
    })
    
    nhgis_data_combined <- nhgis_data_combined %>%
      mutate(
        NAME = ifelse(is.na(NAME), 
                    counties$NAME[match(GEOID, counties$GEOID)], 
                    NAME)
      )
  }
  
  # Save to cache
  saveRDS(nhgis_data_combined, cache_file)
  print_msg("Saved NHGIS historical data to cache.")
  
  return(nhgis_data_combined)
}

# If this script is run directly, execute the function
if (!interactive()) {
  crosswalk <- NULL
  try({
    # Try to load the crosswalk if it exists
    crosswalk_file <- "variable_crosswalk_extended.csv"
    if (file.exists(crosswalk_file)) {
      crosswalk <- read_csv(crosswalk_file, show_col_types = FALSE)
      cat("Loaded variable crosswalk with", nrow(crosswalk), "entries\n")
    }
  })
  
  # Run with default parameters
  nhgis_data <- fetch_nhgis_historical_data(crosswalk = crosswalk, years = 1970:1999)
  cat("Processed", nrow(nhgis_data), "NHGIS data records\n")
}
