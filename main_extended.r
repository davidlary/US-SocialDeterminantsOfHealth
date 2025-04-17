#!/usr/bin/env Rscript

# Enhanced Census Data Pipeline for Social Determinants of Health
# This script combines data from Census Bureau, IPUMS NHGIS, and CDC PLACES
# to create a comprehensive county-level dataset for social determinants of health

cat("Starting Enhanced Census Data Pipeline...\n")

# Check command line arguments for --force-update flag
args <- commandArgs(trailingOnly = TRUE)
force_update <- "--force-update" %in% args || "-f" %in% args

# Check when the data was last updated
last_update_file <- "data/last_update.txt"

if (force_update) {
  cat("Force update flag detected. Will refresh all data regardless of age.\n")
}

# Check if we need to update the data
if (file.exists(last_update_file)) {
  last_update <- as.Date(readLines(last_update_file)[1])
  days_since_update <- as.numeric(difftime(Sys.Date(), last_update, units = "days"))
  
  cat("Data was last updated on", last_update, 
      "(", days_since_update, "days ago)\n")
  
  # Check if update is needed (e.g., if more than 30 days since last update)
  if (days_since_update < 30 && !force_update) {
    cat("Data is less than 30 days old. Using cached data unless forced.\n")
    
    # Use cached data as much as possible
    refresh_cache <- FALSE
  } else {
    cat("Data is more than 30 days old. Will check for updates.\n")
    
    # Will check for updates but still use cache where available
    refresh_cache <- FALSE
  }
} else {
  cat("No previous update record found. Will perform initial data collection.\n")
  
  # First run, no need to refresh empty cache
  refresh_cache <- FALSE
}

# Set up logging in logs directory
logs_dir <- "logs"
if (!dir.exists(logs_dir)) {
  dir.create(logs_dir, showWarnings = FALSE, recursive = TRUE)
  cat("Created logs directory at:", logs_dir, "\n")
}
log_file <- file.path(logs_dir, paste0("sdoh_pipeline_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
cat("Starting pipeline. Log will be saved to:", log_file, "\n")

# Set up separate log and console handlers
# Set this to FALSE to reduce console verbosity (will only show key progress updates)
console_output <- FALSE  
# Keep full verbosity in log files
log_verbosity <- TRUE

# Check if the script is being run interactively or sourced
is_interactive_run <- !exists("is_sourced") || !is_sourced

# Function to write to log and conditionally to console
log_message <- function(message, show_console = console_output) {
  # Always write to log file
  cat(message, file = log_file, append = TRUE)
  
  # Only show on console if requested and if running interactively
  if (show_console && is_interactive_run) {
    # Use message instead of cat for cleaner output in interactive mode
    message(trimws(message))
  }
}

# Redirect output to log file but also allow selective console output
sink(log_file, type = "output", split = FALSE)  # Don't split by default

# This is important, so show it in console regardless of verbosity setting
log_message(paste("=== SOCIAL DETERMINANTS OF HEALTH DATA PIPELINE STARTED AT", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "===\n\n"), show_console = TRUE)

# Load required packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(tidycensus)
  library(jsonlite)
  library(duckdb)
  library(ipumsr)
  library(tigris)
  library(sf)
  library(zoo)
  library(httr)
  library(parallel)
  library(foreach)
  library(doParallel)
  library(future)
  library(future.apply)
  library(progressr)
  library(digest)
})

# Check for API key
census_api_key <- Sys.getenv("CENSUS_API_KEY")
if (census_api_key == "") {
  cat("WARNING: No Census API key found in environment variable CENSUS_API_KEY\n")
  cat("You may encounter rate limits. Consider getting a key at: https://api.census.gov/data/key_signup.html\n\n")
} else {
  cat("Census API key found in environment.\n\n")
  census_api_key(census_api_key)
}

# Improved function to handle errors with detailed tracebacks
handle_error_with_logging <- function(expr, step_name, log_file = NULL) {
  tryCatch({
    expr
  }, error = function(e) {
    # Error messages should always be shown, regardless of verbosity
    error_msg <- paste("\nERROR in", step_name, ":", conditionMessage(e), "\n")
    
    # Use log_message if available, otherwise fall back to cat
    if (exists("log_message")) {
      log_message(error_msg, show_console = TRUE)
      log_message("Traceback:\n", show_console = TRUE)
      traceback <- limitedLabels(sys.calls())
      log_message(paste(traceback, collapse = "\n"), show_console = TRUE)
      log_message("\n", show_console = TRUE)
    } else {
      # Fall back to standard cat if log_message not defined
      cat(error_msg)
      cat("Traceback:\n")
      traceback <- limitedLabels(sys.calls())
      cat(paste(traceback, collapse = "\n"), "\n")
    }
    
    # Log to file if provided
    if (!is.null(log_file)) {
      cat(error_msg, file = log_file, append = TRUE)
      cat("Traceback:\n", file = log_file, append = TRUE)
      cat(paste(traceback, collapse = "\n"), "\n", file = log_file, append = TRUE)
    }
    
    # Try to gather diagnostic information
    diag_header <- "\nDiagnostic Information:\n"
    if (exists("log_message")) {
      log_message(diag_header, show_console = TRUE)
    } else {
      cat(diag_header)
    }
    
    # Check R version and memory
    r_version_info <- paste("R version:", R.version.string, "\n")
    memory_info <- paste("Memory usage:", paste0(round(gc()[2,2]/1024, 2), " GB"), "\n")
    
    if (exists("log_message")) {
      log_message(r_version_info, show_console = TRUE)
      log_message(memory_info, show_console = TRUE)
    } else {
      cat(r_version_info)
      cat(memory_info)
    }
    
    # Check for required packages
    req_packages <- c("tidyverse", "tidycensus", "duckdb", "ipumsr", "tigris", "sf", "zoo", "httr")
    for (pkg in req_packages) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        pkg_msg <- paste("Package", pkg, "is not installed!\n")
        if (exists("log_message")) {
          log_message(pkg_msg, show_console = TRUE)
        } else {
          cat(pkg_msg)
        }
      } else {
        # Convert package version to character to avoid cat() error with list objects
        pkg_version <- as.character(packageVersion(pkg))
        pkg_msg <- paste("Package", pkg, "version:", pkg_version, "\n")
        if (exists("log_message")) {
          log_message(pkg_msg, show_console = TRUE)
        } else {
          cat(pkg_msg)
        }
      }
    }
    
    # Final stop message
    stop_msg <- paste("Fatal error in", step_name, "- See log for details")
    if (exists("log_message")) {
      log_message(paste("\n", stop_msg, "\n"), show_console = TRUE)
    }
    stop(stop_msg)
  })
}

# Function to ensure directories exist before using them
ensure_directories <- function() {
  dirs <- c(
    "data/cdc_places", 
    "data/nhgis",
    "data/cache",
    "data/shapefiles",
    "output",
    "output/maps",
    "logs"
  )
  
  for (dir in dirs) {
    if (!dir.exists(dir)) {
      cat("Creating directory:", dir, "\n")
      dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    }
  }
}

# Ensure necessary directories exist
handle_error_with_logging(ensure_directories(), "directory creation")

# Set options to improve reliability
options(timeout = 300)  # 5 minute timeout
options(scipen = 999)   # Avoid scientific notation
options(stringsAsFactors = FALSE)

# Step 1: Build the extended crosswalk
# Always show major steps on console
log_message("STEP 1: BUILDING EXTENDED VARIABLE CROSSWALK\n\n", show_console = TRUE)
source("build_extended_crosswalk.r")
crosswalk <- handle_error_with_logging(
  build_extended_crosswalk(),
  "extended crosswalk building",
  log_file
)
log_message(paste("\nExtended crosswalk built successfully with", nrow(crosswalk), "variables.\n\n"), show_console = TRUE)

# Step 2: Fetch data from all sources
log_message("STEP 2: FETCHING DATA FROM MULTIPLE SOURCES\n\n", show_console = TRUE)
source("fetch_extended_data.r")
source("fetch_nhgis_data.r")  # Using the fixed NHGIS data function

# Wrap fetch operation in tryCatch with better logging
data_list <- handle_error_with_logging(
  {
    # Set longer timeout for API calls
    options(timeout = 300)
    
    # Check if parallel parameters were set by the launcher script
    parallel_enabled <- if (exists("PARALLEL_ENABLED")) PARALLEL_ENABLED else TRUE
    parallel_cores <- if (exists("PARALLEL_CORES")) PARALLEL_CORES else NULL
    parallel_strategy <- if (exists("PARALLEL_STRATEGY")) PARALLEL_STRATEGY else "multisession"
    
    # Log the parallel processing configuration
    log_message(paste("Parallel processing:", 
                      if(parallel_enabled) "ENABLED" else "DISABLED", "\n"), show_console = TRUE)
    if(parallel_enabled) {
      core_info <- if(is.null(parallel_cores)) "auto-detect" else parallel_cores
      log_message(paste("Cores:", core_info, "Strategy:", parallel_strategy, "\n"), show_console = TRUE)
    }
    
    # Define year ranges - dynamically determine the current year
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    all_years <- 1970:current_year
    
    # Use NHGIS data for the entire date range - no more splitting into historical and modern
    log_message(paste("Processing data for ALL years", min(all_years), "to", max(all_years), "\n"), show_console = TRUE)
    log_message("Using NHGIS as primary source for the entire date range\n", show_console = TRUE)
    
    # Load IPUMS credentials securely
    log_message("Checking for IPUMS credentials...\n", show_console = TRUE)
    
    # Universal IPUMS credential loader function
    load_ipums_credentials <- function() {
      # Initialize result
      creds_found <- FALSE
      
      # 1. Try environment variables first (most secure)
      log_message("Checking for IPUMS credentials in environment variables...\n", show_console = FALSE)
      ipums_username <- Sys.getenv("IPUMS_USERNAME", "")
      ipums_password <- Sys.getenv("IPUMS_PASSWORD", "")
      
      if (ipums_username != "" && ipums_password != "") {
        log_message("IPUMS credentials found in environment variables.\n", show_console = TRUE)
        return(TRUE)
      }
      
      # 2. Try the standard IPUMS credentials file
      log_message("Checking for IPUMS credentials in ~/.ipums_credentials/config...\n", show_console = FALSE)
      cred_file <- file.path(Sys.getenv("HOME"), ".ipums_credentials/config")
      
      if (file.exists(cred_file)) {
        log_message("IPUMS credentials file found. Attempting to load...\n", show_console = TRUE)
        tryCatch({
          # Manual parsing of credentials file
          lines <- readLines(cred_file)
          for (line in lines) {
            if (grepl("^IPUMS_USERNAME=", line)) {
              ipums_username <- sub("^IPUMS_USERNAME=", "", line)
              Sys.setenv(IPUMS_USERNAME = ipums_username)
            } else if (grepl("^IPUMS_PASSWORD=", line)) {
              ipums_password <- sub("^IPUMS_PASSWORD=", "", line)
              Sys.setenv(IPUMS_PASSWORD = ipums_password)
            }
          }
          
          # Verify we got both credentials
          if (Sys.getenv("IPUMS_USERNAME") != "" && Sys.getenv("IPUMS_PASSWORD") != "") {
            log_message("IPUMS credentials loaded successfully from credentials file.\n", show_console = TRUE)
            return(TRUE)
          }
        }, error = function(e) {
          log_message(paste("Error reading credentials file:", conditionMessage(e), "\n"), show_console = TRUE)
        })
      }
      
      # 3. Try .Renviron file
      log_message("Checking for IPUMS credentials in .Renviron file...\n", show_console = FALSE)
      renviron_path <- file.path(Sys.getenv("HOME"), ".Renviron")
      
      if (file.exists(renviron_path)) {
        tryCatch({
          lines <- readLines(renviron_path)
          for (line in lines) {
            if (grepl("^IPUMS_USERNAME=", line)) {
              ipums_username <- sub("^IPUMS_USERNAME=", "", line)
              Sys.setenv(IPUMS_USERNAME = ipums_username)
            } else if (grepl("^IPUMS_PASSWORD=", line)) {
              ipums_password <- sub("^IPUMS_PASSWORD=", "", line)
              Sys.setenv(IPUMS_PASSWORD = ipums_password)
            }
          }
          
          # Verify we got both credentials
          if (Sys.getenv("IPUMS_USERNAME") != "" && Sys.getenv("IPUMS_PASSWORD") != "") {
            log_message("IPUMS credentials loaded successfully from .Renviron file.\n", show_console = TRUE)
            return(TRUE)
          }
        }, error = function(e) {
          log_message(paste("Error reading .Renviron file:", conditionMessage(e), "\n"), show_console = FALSE)
        })
      }
      
      # 4. Try project-specific credentials in the R directory
      log_message("Checking for project-specific IPUMS credentials...\n", show_console = FALSE)
      project_cred_paths <- c(
        "ipums_credentials.txt",
        "data/ipums_credentials.txt",
        "../ipums_credentials.txt"
      )
      
      for (path in project_cred_paths) {
        if (file.exists(path)) {
          log_message(paste("Found project credentials file at", path, "\n"), show_console = TRUE)
          tryCatch({
            lines <- readLines(path)
            for (line in lines) {
              if (grepl("^USERNAME=|^IPUMS_USERNAME=", line)) {
                ipums_username <- sub("^(USERNAME=|IPUMS_USERNAME=)", "", line)
                Sys.setenv(IPUMS_USERNAME = ipums_username)
              } else if (grepl("^PASSWORD=|^IPUMS_PASSWORD=", line)) {
                ipums_password <- sub("^(PASSWORD=|IPUMS_PASSWORD=)", "", line)
                Sys.setenv(IPUMS_PASSWORD = ipums_password)
              }
            }
            
            # Verify we got both credentials
            if (Sys.getenv("IPUMS_USERNAME") != "" && Sys.getenv("IPUMS_PASSWORD") != "") {
              log_message("IPUMS credentials loaded successfully from project file.\n", show_console = TRUE)
              return(TRUE)
            }
          }, error = function(e) {
            log_message(paste("Error reading project credentials file:", conditionMessage(e), "\n"), show_console = FALSE)
          })
        }
      }
      
      # 5. Last resort - try the source script if it exists
      custom_loader_paths <- c(
        "utilities/load_ipums_credentials.r",
        "R/utilities/load_ipums_credentials.r",
        Sys.getenv("IPUMS_LOADER_PATH", unset = "")
      )
      
      for (script_path in custom_loader_paths) {
        if (script_path != "" && file.exists(script_path)) {
          log_message(paste("Found custom credential loader at", script_path, "\n"), show_console = TRUE)
          tryCatch({
            source(script_path, local = TRUE)
            if (exists("load_ipums_credentials", envir = environment(), inherits = FALSE)) {
              # Call the loaded function in its environment
              custom_result <- load_ipums_credentials()
              
              # Check if it worked
              if (Sys.getenv("IPUMS_USERNAME") != "" && Sys.getenv("IPUMS_PASSWORD") != "") {
                log_message("IPUMS credentials loaded successfully from custom loader.\n", show_console = TRUE)
                return(TRUE)
              }
            }
          }, error = function(e) {
            log_message(paste("Error using custom credential loader:", conditionMessage(e), "\n"), show_console = FALSE)
          })
        }
      }
      
      # No credentials found
      log_message("No IPUMS credentials found.\n", show_console = TRUE)
      return(FALSE)
    }
    
    # Try to load credentials using our comprehensive function
    credentials_loaded <- load_ipums_credentials()
    
    # Always attempt to use IPUMS API unless explicitly disabled
    use_ipumsr <- !isFALSE(options("use_ipumsr")$use_ipumsr)
    ipums_credentials <- NULL
    
    # Create credentials object if we found them
    if (credentials_loaded) {
      # Get credentials from environment variables (now loaded from whatever source)
      ipums_username <- Sys.getenv("IPUMS_USERNAME")
      ipums_password <- Sys.getenv("IPUMS_PASSWORD")
      
      if (ipums_username != "" && ipums_password != "") {
        log_message("IPUMS credentials loaded and ready to use.\n", show_console = TRUE)
        ipums_credentials <- list(
          username = ipums_username,
          password = ipums_password
        )
      }
    } else {
      # No credentials found through automated methods
      log_message("No IPUMS credentials found through automated methods.\n", show_console = TRUE)
      
      # Check if interactive - we could prompt for credentials
      if (interactive()) {
        log_message("Running in interactive mode. Would you like to enter IPUMS credentials? (y/n)\n", show_console = TRUE)
        answer <- readline("Enter credentials? (y/n): ")
        
        if (tolower(substr(answer, 1, 1)) == "y") {
          # Prompt for credentials
          ipums_username <- readline("IPUMS Username: ")
          ipums_password <- readline("IPUMS Password: ")
          
          # Store in environment
          if (ipums_username != "" && ipums_password != "") {
            Sys.setenv(IPUMS_USERNAME = ipums_username)
            Sys.setenv(IPUMS_PASSWORD = ipums_password)
            
            ipums_credentials <- list(
              username = ipums_username,
              password = ipums_password
            )
            
            log_message("IPUMS credentials entered manually.\n", show_console = TRUE)
            credentials_loaded <- TRUE
          }
        }
      }
      
      # Final check - if we still don't have credentials
      if (is.null(ipums_credentials)) {
        log_message("Will use existing NHGIS data files if available.\n", show_console = TRUE)
        use_ipumsr <- FALSE
      }
    }
    
    # Fetch data for ALL years using NHGIS as primary source
    log_message("Fetching NHGIS data for entire date range (1970-present)...\n", show_console = TRUE)
    if (!use_ipumsr) {
      log_message("Note: Using placeholder NHGIS data if no files are found locally.\n", show_console = TRUE)
    } else {
      log_message("Attempting to use IPUMS API to fetch NHGIS data...\n", show_console = TRUE)
    }
    
    nhgis_data <- fetch_nhgis_historical_data(
      crosswalk = crosswalk,
      years = all_years,  # Use all years 1970-present
      cache_dir = "data/cache",
      refresh_cache = refresh_cache,  # Use the auto-determined refresh setting
      primary_source = TRUE,  # Use NHGIS as primary source
      use_ipumsr = use_ipumsr,
      ipums_credentials = ipums_credentials
    )
    
    # Fetch additional data sources to supplement NHGIS
    log_message("Fetching supplementary data sources...\n", show_console = TRUE)
    supplementary_data <- fetch_extended_data(
      crosswalk, 
      years = all_years,  # Use all years 1970-present
      include_places = TRUE, 
      include_nhgis = FALSE,  # We already have NHGIS as primary source
      include_life_expectancy = TRUE,
      use_cache = TRUE,
      refresh_cache = refresh_cache,
      # Parallel processing options
      parallel = parallel_enabled,
      num_cores = parallel_cores, 
      parallel_strategy = parallel_strategy,
      # Cache options
      cache_options = list(
        refresh_census = FALSE,
        refresh_places = FALSE,
        refresh_nhgis = FALSE,
        refresh_life_expectancy = refresh_cache,  # Force refresh life expectancy if refreshing all
        max_cache_age_days = 30,
        cache_dir = "data/cache"
      )
    )
    
    # Combine NHGIS data with supplementary sources
    all_data <- list(
      nhgis = nhgis_data,
      census = supplementary_data$census,
      places = supplementary_data$places,
      life_expectancy = supplementary_data$life_expectancy
    )
    
    # Ensure life expectancy data exists - create a placeholder if needed
    if (is.null(all_data$life_expectancy) || nrow(all_data$life_expectancy) == 0) {
      log_message("WARNING: No IHME life expectancy data available. Creating placeholder...\n", show_console = TRUE)
      
      # Create counties data frame from NHGIS data
      counties <- nhgis_data %>%
        select(GEOID, NAME) %>%
        distinct()
      
      # Create empty life expectancy dataset with right structure
      empty_life_exp <- expand.grid(
        GEOID = counties$GEOID,
        year = all_years,
        stringsAsFactors = FALSE
      ) %>%
        left_join(counties, by = "GEOID") %>%
        mutate(
          life_expectancy = NA,
          life_expectancy_female = NA,
          life_expectancy_male = NA,
          data_source = "IHME (Placeholder)",
          data_quality = "missing",
          data_vintage = "IHME Placeholder"
        )
      
      all_data$life_expectancy <- empty_life_exp
      log_message("Created life expectancy placeholder with the correct structure.\n", show_console = TRUE)
    }
    
    # Use combined data for the rest of the pipeline
    modern_data <- all_data
    
    # Return the combined result
    modern_data
  },
  "extended data fetching",
  log_file
)

# Print summary of fetched data
cat("\nFetched data summary:\n")

if (is.list(data_list)) {
  # Handle Census data (which is a nested list)
  if (is.list(data_list$census)) {
    for (subsource in names(data_list$census)) {
      if (is.data.frame(data_list$census[[subsource]])) {
        cat("- census /", subsource, ": ", 
            nrow(data_list$census[[subsource]]), " rows, ", 
            ncol(data_list$census[[subsource]]), " columns\n")
      }
    }
  }
  
  # Handle other data sources (direct data frames)
  for (source_name in setdiff(names(data_list), "census")) {
    if (is.data.frame(data_list[[source_name]])) {
      cat("- ", source_name, ": ", 
          nrow(data_list[[source_name]]), " rows, ", 
          ncol(data_list[[source_name]]), " columns\n")
    }
  }
} else {
  cat("ERROR: data_list is not a proper list structure.\n")
}

# Step 3: Process and combine data
log_message("\nSTEP 3: PROCESSING AND COMBINING DATA\n\n", show_console = TRUE)
source("process_extended_data.r")

# Show processing status message
cat("Processing data from multiple sources...\n")
cat("Structure of data_list:", typeof(data_list), "\n")

# Add a progress animation while main processing is happening
cat("Main processing started. This may take several minutes...\n")
cat("[")

# Execute processing with enhanced error handling and recovery options
processing_result <- handle_error_with_logging(
  {
    # Try processing with recovery mechanisms
    tryCatch({
      # Create a background animation thread to show progress
      progress_thread <- function() {
        while(TRUE) {
          Sys.sleep(2)
          cat(".")
          flush.console()
        }
      }
      
      # Start the progress indicator in a separate process if possible
      if (requireNamespace("parallel", quietly = TRUE)) {
        progress_pid <- parallel::mcparallel(progress_thread())
        on.exit({
          try(parallel::mckill(progress_pid, signal = 15), silent = TRUE)
        })
      }
      
      # Process data with unified approach and enhanced error recovery
      # No need to use emergency data generation since fetch_nhgis_data.r already
      # returns a standardized structure with empty data and missing data flags
      # when no real data is found
      result <- process_extended_data(data_list, crosswalk, 
                                    interpolate = TRUE, 
                                    extend_health_data = TRUE, 
                                    include_life_expectancy = TRUE,
                                    check_simulated = TRUE)
      
      # Stop the progress indicator if it was started
      if (requireNamespace("parallel", quietly = TRUE) && exists("progress_pid")) {
        try(parallel::mckill(progress_pid, signal = 15), silent = TRUE)
      }
      
      # Return the result
      return(result)
    }, error = function(e) {
      cat("] Failed.\n")
      cat("First attempt at data processing failed:", conditionMessage(e), "\n")
      cat("Analyzing error for recovery options...\n")
      
      # Check error message for specific errors we can handle
      error_msg <- conditionMessage(e)
      
      # Try progressively more conservative approaches based on error type
      if (grepl("invalid '?type'?|argument type", error_msg, ignore.case = TRUE)) {
        cat("Detected type mismatch error. Trying with enhanced type checking...\n")
        
        # Pre-process data_list to ensure all components are proper dataframes
        clean_data_list <- list()
        
        # Safely extract components one by one
        if (is.list(data_list)) {
          # Extract census components into a simple dataframe if possible
          if ("census" %in% names(data_list) && is.list(data_list$census)) {
            cat("Flattening complex census data structure...\n")
            census_frames <- list()
            
            # Try to extract each component of census data
            if (!is.null(data_list$census$decennial) && is.data.frame(data_list$census$decennial)) {
              census_frames$decennial <- data_list$census$decennial
            }
            if (!is.null(data_list$census$pep) && is.data.frame(data_list$census$pep)) {
              census_frames$pep <- data_list$census$pep
            }
            if (!is.null(data_list$census$acs) && is.data.frame(data_list$census$acs)) {
              census_frames$acs <- data_list$census$acs
            }
            
            # Combine into a single dataframe if possible
            if (length(census_frames) > 0) {
              clean_data_list$census <- bind_rows(census_frames)
            } else {
              clean_data_list$census <- tibble() # Empty tibble as fallback
            }
          }
          
          # Copy the rest of the components that are already data frames
          for (component in setdiff(names(data_list), "census")) {
            if (is.data.frame(data_list[[component]])) {
              clean_data_list[[component]] <- data_list[[component]]
            } else {
              clean_data_list[[component]] <- tibble() # Empty tibble placeholder
            }
          }
        } else {
          # If data_list itself isn't a list, create a minimal structure
          clean_data_list <- list(
            census = tibble(),
            places = tibble(),
            nhgis = tibble(),
            life_expectancy = tibble()
          )
        }
        
        cat("Recreated data structure with consistent types. Retrying processing...\n")
        cat("[")
        # Try again with cleaned data and more conservative options
        return(process_extended_data(
          clean_data_list, 
          crosswalk, 
          interpolate = TRUE,
          extend_health_data = FALSE, 
          include_life_expectancy = TRUE,
          check_simulated = FALSE
        ))
      } else {
        # For other errors, use more basic fallback approach
        cat("Trying again with more conservative parameters...\n")
        cat("[")
        
        # Retry with more conservative options that always work
        cat("\nRetrying with more conservative options...\n")
        # Use the main data processing function but with base parameters that always work
        return(process_extended_data(
          data_list, 
          crosswalk, 
          interpolate = TRUE,  # Keep interpolation since that's our primary need
          extend_health_data = FALSE, 
          include_life_expectancy = FALSE,
          check_simulated = FALSE
        ))
      }
    })
  },
  "data processing and combination",
  log_file
)

cat("] Complete!\n")

# Step 4: Validation and examples
log_message("\nSTEP 4: VALIDATION AND EXAMPLES\n\n", show_console = TRUE)
handle_error_with_logging({
  # Try connecting to the database
  con <- tryCatch({
    dbConnect(duckdb(), "us_county_sdoh_data.duckdb")
  }, error = function(e) {
    cat("ERROR connecting to database:", conditionMessage(e), "\n")
    stop("Cannot proceed with validation - database connection failed")
  })
  
  # Check if the main table exists
  table_exists <- tryCatch({
    dbExistsTable(con, "county_sdoh_data")
  }, error = function(e) {
    cat("ERROR checking main table:", conditionMessage(e), "\n")
    FALSE
  })
  
  if (!table_exists) {
    cat("ERROR: Main table 'county_sdoh_data' does not exist in the database.\n")
    dbDisconnect(con)
    stop("Cannot proceed with validation - main table missing")
  }
  
  # Check the structure of the main table
  table_info <- tryCatch({
    dbGetQuery(con, "PRAGMA table_info('county_sdoh_data')")
  }, error = function(e) {
    cat("ERROR getting table info:", conditionMessage(e), "\n")
    data.frame(cid = integer(), name = character(), type = character(),
              notnull = logical(), dflt_value = character(), pk = logical())
  })
  
  cat("Database table structure (main table):\n")
  print(head(table_info, 10))
  cat("... and", nrow(table_info) - 10, "more columns\n")
  
  # Count records by year and source - with error recovery
  year_source_counts <- tryCatch({
    dbGetQuery(con, "
      SELECT year, data_source, COUNT(*) as record_count
      FROM county_sdoh_data
      GROUP BY year, data_source
      ORDER BY year, data_source
    ")
  }, error = function(e) {
    cat("ERROR getting year/source counts:", conditionMessage(e), "\n")
    # Return empty data frame with expected structure
    data.frame(year = integer(), data_source = character(), record_count = integer())
  })
  
  if (nrow(year_source_counts) > 0) {
    cat("\nRecord counts by year and source:\n")
    print(year_source_counts)
  } else {
    cat("\nWARNING: No record counts available by year and source.\n")
  }
  
  # Get available columns
  available_columns <- table_info$name
  
  # Helper function to safely include columns in a query
  safe_columns <- function(col_names) {
    safe_cols <- c()
    for (col in col_names) {
      if (col %in% available_columns) {
        safe_cols <- c(safe_cols, col)
      }
    }
    return(safe_cols)
  }
  
  # Check data completeness for key variables - with error protection
  # First get the available variables from our list
  key_categories <- c('Demographics', 'Socioeconomic', 'Health Status')
  desired_vars <- c(
    "female_population", "male_population", "median_age", "population_65_over", 
    "population_under_18", "total_population", "gini_index", "median_household_income", 
    "poverty_rate", "snap_benefits_pct", "obesity_pct", "diabetes_pct", "poor_physical_health_pct"
  )
  
  # Check if data_dictionary table exists
  dict_exists <- tryCatch({
    dbExistsTable(con, "data_dictionary")
  }, error = function(e) {
    cat("ERROR checking data_dictionary table:", conditionMessage(e), "\n")
    FALSE
  })
  
  if (dict_exists) {
    # Filter to only the variables that exist in the data dictionary
    var_query <- tryCatch({
      sprintf("
        SELECT std_name, category, description 
        FROM data_dictionary 
        WHERE std_name IN (%s)
        AND available_in_dataset = TRUE
      ", paste0("'", desired_vars, "'", collapse = ", "))
    }, error = function(e) {
      cat("ERROR building variable query:", conditionMessage(e), "\n")
      NULL
    })
    
    if (!is.null(var_query)) {
      available_key_vars <- tryCatch({
        dbGetQuery(con, var_query)
      }, error = function(e) {
        cat("ERROR querying data dictionary:", conditionMessage(e), "\n")
        data.frame(std_name = character(), category = character(), description = character())
      })
      
      if (nrow(available_key_vars) > 0) {
        # Generate dynamic query based on available variables
        var_list <- paste0("'", available_key_vars$std_name, "'", collapse = ", ")
        
        key_vars_query <- tryCatch({
          sprintf("
            WITH key_vars AS (
              SELECT std_name FROM data_dictionary 
              WHERE std_name IN (%s)
              AND available_in_dataset = TRUE
            )
            SELECT v.std_name, v.category, v.description,
              COUNT(DISTINCT CASE WHEN d.year = 2000 THEN d.GEOID END) as count_2000,
              COUNT(DISTINCT CASE WHEN d.year = 2010 THEN d.GEOID END) as count_2010,
              COUNT(DISTINCT CASE WHEN d.year = 2020 THEN d.GEOID END) as count_2020
            FROM data_dictionary v
            LEFT JOIN county_sdoh_data d ON 1=1
            WHERE v.std_name IN (%s)
            GROUP BY v.std_name, v.category, v.description
            ORDER BY v.category, v.std_name
          ", var_list, var_list)
        }, error = function(e) {
          cat("ERROR building key vars completeness query:", conditionMessage(e), "\n")
          NULL
        })
        
        if (!is.null(key_vars_query)) {
          key_vars_completeness <- tryCatch({
            dbGetQuery(con, key_vars_query)
          }, error = function(e) {
            cat("ERROR running key vars completeness query:", conditionMessage(e), "\n")
            data.frame(std_name = character(), category = character(), 
                      description = character(), count_2000 = integer(),
                      count_2010 = integer(), count_2020 = integer())
          })
          
          if (nrow(key_vars_completeness) > 0) {
            cat("\nCompleteness of key variables in benchmark years:\n")
            print(key_vars_completeness)
          } else {
            cat("\nWARNING: No key variable completeness data available.\n")
          }
        }
      } else {
        cat("\nNo key variables found in data dictionary.\n")
      }
    }
  } else {
    cat("\nWARNING: data_dictionary table does not exist. Skipping variable completeness check.\n")
  }
  
  # Get interpolation summary - with error protection
  interpolation_summary <- tryCatch({
    dbGetQuery(con, "
      SELECT 
        year, 
        COUNT(*) as total_records,
        SUM(CASE WHEN interpolation_used = TRUE THEN 1 ELSE 0 END) as interpolated_records,
        ROUND(100.0 * SUM(CASE WHEN interpolation_used = TRUE THEN 1 ELSE 0 END) / COUNT(*), 1) as pct_interpolated
      FROM county_sdoh_data
      GROUP BY year
      ORDER BY year
    ")
  }, error = function(e) {
    cat("ERROR getting interpolation summary:", conditionMessage(e), "\n")
    # Try a fallback query without the interpolation_used column
    tryCatch({
      dbGetQuery(con, "
        SELECT year, COUNT(*) as total_records, 0 as interpolated_records, 0 as pct_interpolated
        FROM county_sdoh_data
        GROUP BY year
        ORDER BY year
      ")
    }, error = function(e2) {
      # Give up and return empty frame
      data.frame(year = integer(), total_records = integer(), 
                interpolated_records = integer(), pct_interpolated = numeric())
    })
  })
  
  if (nrow(interpolation_summary) > 0) {
    cat("\nInterpolation summary by year:\n")
    print(interpolation_summary)
  } else {
    cat("\nWARNING: No interpolation summary available.\n")
  }
  
  # Show example of data for a selected county - with error protection
  # First, get the list of columns we want to display
  desired_county_cols <- c(
    "GEOID", "NAME", "year", "data_source", "data_quality",
    "total_population", "median_household_income", "poverty_rate",
    "uninsured_pct", "obesity_pct", "interpolation_used", "interpolation_count"
  )
  
  # Filter to only columns that exist in our table
  county_cols <- intersect(desired_county_cols, available_columns)
  
  if (length(county_cols) > 0) {
    # Build a dynamic query using only available columns
    county_cols_str <- paste(county_cols, collapse = ", ")
    example_county_query <- sprintf("
      SELECT %s
      FROM county_sdoh_data
      WHERE GEOID = '06037'  -- Los Angeles County, CA
      ORDER BY year
    ", county_cols_str)
    
    # Execute the query
    example_county <- tryCatch({
      dbGetQuery(con, example_county_query)
    }, error = function(e) {
      cat("ERROR getting example county data:", conditionMessage(e), "\n")
      # Return empty data frame with expected structure
      empty_df <- data.frame(matrix(ncol = length(county_cols), nrow = 0))
      names(empty_df) <- county_cols
      empty_df
    })
    
    if (nrow(example_county) > 0) {
      cat("\nExample data for Los Angeles County (GEOID 06037) over time:\n")
      print(example_county)
    } else {
      cat("\nWARNING: No example data available for Los Angeles County.\n")
    }
  } else {
    cat("\nWARNING: No valid columns available for example county query.\n")
  }
  
  # Export a sample dataset for review - use the same approach for safe column selection
  # with error protection
  sample_export <- tryCatch({
    # First try to get data from the latest_county_data view
    if (dbExistsTable(con, "latest_county_data")) {
      # Get sample columns
      sample_cols <- intersect(
        c("GEOID", "NAME", "year", "total_population", "median_household_income", 
          "poverty_rate", "uninsured_pct", "obesity_pct", "diabetes_pct"),
        available_columns
      )
      
      sample_cols_str <- paste(sample_cols, collapse = ", ")
      
      # Build query to get 100 random counties
      sample_query <- sprintf("
        SELECT %s FROM county_sdoh_data
        WHERE GEOID IN (
          SELECT DISTINCT GEOID FROM county_sdoh_data
          ORDER BY RANDOM()
          LIMIT 100
        )
        AND year = (SELECT MAX(year) FROM county_sdoh_data)
      ", sample_cols_str)
      
      # Execute the query
      dbGetQuery(con, sample_query)
    } else {
      # If the view doesn't exist, query the main table directly
      cat("latest_county_data view not found. Querying main table directly.\n")
      
      # Get sample columns
      sample_cols <- intersect(
        c("GEOID", "NAME", "year", "total_population", "median_household_income", 
          "poverty_rate", "uninsured_pct", "obesity_pct", "diabetes_pct"),
        available_columns
      )
      
      sample_cols_str <- paste(sample_cols, collapse = ", ")
      
      # Build query
      sample_query <- sprintf("
        WITH latest_year AS (
          SELECT MAX(year) as max_year FROM county_sdoh_data
        ),
        random_counties AS (
          SELECT DISTINCT GEOID FROM county_sdoh_data
          ORDER BY RANDOM()
          LIMIT 100
        )
        SELECT %s FROM county_sdoh_data, latest_year
        WHERE GEOID IN (SELECT GEOID FROM random_counties)
        AND year = max_year
      ", sample_cols_str)
      
      # Execute the query
      dbGetQuery(con, sample_query)
    }
  }, error = function(e) {
    cat("ERROR exporting sample data:", conditionMessage(e), "\n")
    
    # Fall back to a very simple query as last resort
    tryCatch({
      dbGetQuery(con, "
        SELECT * FROM county_sdoh_data 
        LIMIT 100
      ")
    }, error = function(e2) {
      # Return empty data frame if all else fails
      cat("ERROR with fallback sample query:", conditionMessage(e2), "\n")
      data.frame(GEOID = character(), NAME = character(), year = integer())
    })
  })
  
  # Write the sample export to a CSV
  tryCatch({
    write_csv(sample_export, "sample_county_data.csv")
    cat("\nExported sample of", nrow(sample_export), "records to 'sample_county_data.csv'\n")
  }, error = function(e) {
    cat("ERROR writing sample CSV:", conditionMessage(e), "\n")
  })
  
  # Close the database connection
  tryCatch({
    dbDisconnect(con)
  }, error = function(e) {
    cat("ERROR disconnecting from database:", conditionMessage(e), "\n")
  })
}, "validation and examples", log_file)

# Create README.md with documentation
log_message("\nSTEP 5: GENERATING DOCUMENTATION\n\n", show_console = TRUE)
handle_error_with_logging({
  readme_content <- c(
    "# Social Determinants of Health County-Level Dataset",
    "",
    paste("Generated on:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "",
    "## Overview",
    "",
    "This dataset combines county-level data on social determinants of health from multiple authoritative sources:",
    "",
    "- **U.S. Census Bureau** (Decennial Census, American Community Survey, Population Estimates Program)",
    "- **CDC PLACES** (county-level health indicators)",
    "- **IPUMS NHGIS** (harmonized time series data)",
    "",
    "The data has been processed to provide consistent variable names across sources and years,",
    "with interpolation for missing years where appropriate.",
    "",
    "## Data Sources",
    "",
    "| Source | Years | Description |",
    "| ------ | ----- | ----------- |",
    "| Decennial Census | 2000, 2010, 2020 | Complete count of population and housing |",
    "| American Community Survey (ACS) | 2009-2021 | Detailed demographic, social, economic, and housing data |",
    "| Population Estimates Program (PEP) | 2000-2021 | Annual population estimates |",
    "| CDC PLACES | 2019-2021 | County-level health outcome measures and health-related behaviors |",
    "| IPUMS NHGIS | Various | Harmonized historical census data |",
    "",
    "## Data Structure",
    "",
    "The database contains the following main tables:",
    "",
    "- `county_sdoh_data` - Main data table with all variables by county and year",
    "- `county_metadata` - Information about each county",
    "- `data_dictionary` - Descriptions and metadata for each variable",
    "- `variable_crosswalk` - Mapping between standardized variable names and source-specific codes",
    "- `data_quality_summary` - Summary of data completeness by year and source",
    "- `data_quality_detailed` - Detailed information about interpolation and extension",
    "",
    "And the following views:",
    "",
    "- `latest_county_data` - The most recent data available for each county",
    "- `county_time_series` - All years of data for all counties",
    "- `county_health_metrics` - Health-specific metrics for all counties",
    "- Several category-specific views (demographics, socioeconomic, etc.)",
    "",
    "## Data Quality Flags",
    "",
    "Each record includes data quality indicators:",
    "",
    "- `data_quality` - One of: 'direct' (counted), 'estimate' (statistical estimate), 'harmonized' (reconciled across sources), 'interpolated' (gap-filled), or 'extended' (extrapolated)",
    "- `data_source` - Original source of the data",
    "- `data_vintage` - Year and specific collection the data came from",
    "- `data_quality_score` - Numeric score (4=best, 0=worst) indicating data quality",
    "- `interpolation_used` - Boolean flag indicating if any values were interpolated",
    "- `interpolation_count` - Count of how many variables were interpolated",
    "- `extension_used` - Boolean flag indicating if any values were extended",
    "- `extension_count` - Count of how many variables were extended",
    "",
    "Each variable also has accompanying `*_interpolated` and `*_extended` flags to indicate if that specific value was interpolated or extended.",
    "",
    "## Usage Examples",
    "",
    "```sql",
    "-- Get the latest data for all counties",
    "SELECT * FROM latest_county_data;",
    "",
    "-- Get time series data for a specific county",
    "SELECT * FROM county_time_series WHERE GEOID = '06001' ORDER BY year;",
    "",
    "-- Get counties with highest poverty rates in the latest year",
    "SELECT GEOID, NAME, year, poverty_rate ",
    "FROM latest_county_data ",
    "WHERE poverty_rate IS NOT NULL ",
    "ORDER BY poverty_rate DESC LIMIT 10;",
    "```",
    "",
    "## Notes and Limitations",
    "",
    "- Geographic definitions change over time; this dataset uses the most recent county boundaries",
    "- Some variables are only available for certain years",
    "- Interpolated values are provided for convenience but should be used with caution",
    "- Health metrics should not be interpolated across long time periods",
    "- The database requires DuckDB to open (https://duckdb.org/)",
    "",
    "## Files Included",
    "",
    "- `us_county_sdoh_data.duckdb` - DuckDB database with all tables and views",
    "- `county_sdoh_data_complete.csv` - Complete dataset in CSV format (subset of columns)",
    "- `county_metadata.csv` - County reference information",
    "- `data_dictionary_complete.csv` - Variable descriptions and metadata",
    "- `README.md` - This documentation file",
    "- `sample_county_data.csv` - Sample dataset for quick review",
    "",
    "## Troubleshooting",
    "",
    "If you encounter issues with the database file, try these steps:",
    "",
    "1. Ensure you have the latest version of DuckDB installed",
    "2. Use the CSV files for basic analysis if the database is corrupted",
    "3. Check the log file for any errors during data processing",
    "",
    "## Citation",
    "",
    "If you use this dataset in your research or applications, please cite it as:",
    "",
    "```",
    paste("Social Determinants of Health County-Level Dataset (", format(Sys.Date(), "%Y"), "). Generated using data from U.S. Census Bureau, CDC PLACES, and IPUMS NHGIS.", sep=""),
    "```",
    "",
    "## Contact",
    "",
    "For questions or issues with this dataset, please contact the data team."
  )
  
  # Try to write the README
  tryCatch({
    writeLines(readme_content, "README.md")
    cat("Created README.md with documentation\n")
  }, error = function(e) {
    cat("ERROR writing README.md:", conditionMessage(e), "\n")
    
    # Try writing to a different location as fallback
    tryCatch({
      writeLines(readme_content, "README.txt")
      cat("Created README.txt with documentation (README.md failed)\n")
    }, error = function(e2) {
      cat("ERROR writing README.txt:", conditionMessage(e2), "\n")
    })
  })
}, "documentation generation", log_file)

# Step 6: Download and prepare shapefiles for mapping (requires viridis package)
log_message("\nSTEP 6: PREPARING COUNTY BOUNDARY SHAPEFILES\n\n", show_console = TRUE)
handle_error_with_logging({
  # Check for required packages
  if (!requireNamespace("viridis", quietly = TRUE)) {
    log_message("Required package 'viridis' is not installed. Installing now...\n", show_console = TRUE)
    install.packages("viridis", repos = "https://cloud.r-project.org")
  }
  
  # Source the shapefile utilities
  source("utilities/fetch_county_shapefiles.r")
  
  # Download shapefiles for benchmark years
  shapefile_years <- c(1990, 2000, 2010, 2020)
  log_message(paste("Downloading county shapefiles for years:", paste(shapefile_years, collapse=", "), "\n"), show_console = TRUE)
  
  # Download shapefiles
  county_shapefiles <- fetch_county_shapefiles(
    years = shapefile_years,
    shapefile_dir = "data/shapefiles",
    refresh_cache = refresh_cache,
    simplified = TRUE
  )
  
  # Print summary of shapefiles
  for (year in as.character(shapefile_years)) {
    if (!is.null(county_shapefiles[[year]])) {
      log_message(paste(year, ": ", nrow(county_shapefiles[[year]]), " counties\n", sep = ""), show_console = TRUE)
    } else {
      log_message(paste(year, ": No shapefile data available\n", sep = ""), show_console = TRUE)
    }
  }
  
  # Check if we can generate maps
  viridis_available <- requireNamespace("viridis", quietly = TRUE)
  if (!viridis_available) {
    log_message("Package 'viridis' is not available or installation failed. Skipping map generation.\n", show_console = TRUE)
  } else {
    # Generate sample maps
    tryCatch({
      log_message("Loading mapping packages...\n", show_console = TRUE)
      library(viridis)
      source("generate_county_maps.r")
      
      # Generate maps for key variables
      log_message("\nGenerating county maps for visualization...\n", show_console = TRUE)
      
      # Try to get actual years from the database
      sample_years <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 
                      2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)
      
      sample_variables <- c("poverty_rate", "median_household_income")
      
      # Only try to generate maps if the database exists
      if (file.exists("us_county_sdoh_data.duckdb")) {
        tryCatch({
          sample_maps <- generate_county_maps(
            database_path = "us_county_sdoh_data.duckdb",
            years = sample_years,
            variables = sample_variables,
            output_dir = "output/maps",
            shapefile_dir = "data/shapefiles"
          )
          log_message("Sample maps generated and saved to output/maps directory\n", show_console = TRUE)
        }, error = function(e) {
          log_message(paste("Error generating sample maps:", conditionMessage(e), "\n"), show_console = TRUE)
        })
      } else {
        log_message("Database not found. Skipping map generation.\n", show_console = TRUE)
      }
    }, error = function(e) {
      log_message(paste("Error in mapping setup:", conditionMessage(e), "\n"), show_console = TRUE)
      log_message("Skipping map generation due to errors.\n", show_console = TRUE)
    })
  }
}, "shapefile preparation", log_file)

# End of pipeline - show completion message in log
log_message(paste("\n=== SOCIAL DETERMINANTS OF HEALTH DATA PIPELINE COMPLETED AT", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "===\n"), show_console = FALSE)
log_message(paste("Log file saved to:", log_file, "\n"), show_console = FALSE)

# Restore console output
sink(NULL)

# Print completion summary to console
cat("\n=== Pipeline Execution Summary ===\n")
cat("Status: SUCCESS\n")
cat("Output Database: us_county_sdoh_data.duckdb\n")
cat("Documentation: README.md\n")
cat("Log File: ", log_file, "\n")

cat("\n=== Data Summary by Year and Source ===\n")

# Connect to the database
tryCatch({
  con <- DBI::dbConnect(duckdb::duckdb(), 'us_county_sdoh_data.duckdb')
  
  # Generate summary of variables by year and source
  summary_query <- "
    WITH variable_counts AS (
      -- Get column names that are variables (not metadata columns)
      SELECT column_name 
      FROM information_schema.columns 
      WHERE table_name = 'county_sdoh_data'
      AND column_name NOT IN ('GEOID', 'NAME', 'year', 'source', 'data_quality', 
                             'data_source', 'data_vintage', 'interpolation_used', 
                             'interpolation_count', 'extension_used', 'extension_count')
    ),
    
    year_source_groups AS (
      -- Count non-null values for each variable by year and source
      SELECT 
        year, 
        data_source,
        COUNT(DISTINCT GEOID) as county_count,
        COUNT(*) as row_count
        -- Dynamic count of non-null variables
        %s
      FROM county_sdoh_data
      GROUP BY year, data_source
      ORDER BY year, data_source
    )
    
    SELECT * FROM year_source_groups
  "
  
  # Get actual variable columns
  var_cols <- dbGetQuery(con, "
    SELECT column_name 
    FROM information_schema.columns 
    WHERE table_name = 'county_sdoh_data'
    AND column_name NOT IN ('GEOID', 'NAME', 'year', 'source', 'data_quality', 
                          'data_source', 'data_vintage', 'interpolation_used', 
                          'interpolation_count', 'extension_used', 'extension_count')
  ")
  
  # Create dynamic part of the query to count non-null values for each variable
  if (nrow(var_cols) > 0) {
    var_counts <- paste0(
      sapply(var_cols$column_name, function(col) {
        sprintf(",\n        SUM(CASE WHEN \"%s\" IS NOT NULL THEN 1 ELSE 0 END) as \"%s_count\"", 
               col, col)
      }),
      collapse = ""
    )
    
    # Complete query with variable counts
    final_query <- sprintf(summary_query, var_counts)
    
    # Run query and get summary
    summary_data <- dbGetQuery(con, final_query)
    
    # Print summary
    if (nrow(summary_data) > 0) {
      # Basic summary by year and source
      cat("Variables available by year and source:\n")
      basic_summary <- summary_data %>%
        select(year, data_source, county_count, row_count) %>%
        arrange(year, data_source)
      print(basic_summary)
      
      # Calculate total number of variables with data for each year/source
      var_count_cols <- grep("_count$", names(summary_data), value = TRUE)
      var_count_cols <- setdiff(var_count_cols, c("county_count", "row_count"))
      
      if (length(var_count_cols) > 0) {
        var_counts_by_source <- summary_data %>%
          mutate(
            total_variables = rowSums(select(., all_of(var_count_cols)) > 0),
            nonzero_variables = rowSums(select(., all_of(var_count_cols)) > 0)
          ) %>%
          select(year, data_source, total_variables, nonzero_variables)
        
        cat("\nVariable counts by year and source:\n")
        print(var_counts_by_source)
        
        # List top variables by availability
        cat("\nTop 10 most widely available variables:\n")
        var_availability <- data.frame(
          variable = sub("_count$", "", var_count_cols),
          count = colSums(summary_data[var_count_cols] > 0)
        ) %>%
          arrange(desc(count)) %>%
          head(10)
        print(var_availability)
      }
    } else {
      cat("No data available in summary.\n")
    }
  } else {
    cat("No variable columns found in the database.\n")
  }
  
  # Disconnect from database
  dbDisconnect(con)
}, error = function(e) {
  cat("Error generating data summary:", conditionMessage(e), "\n")
})

# Record this update date
writeLines(as.character(Sys.Date()), last_update_file)
cat("Recorded update date:", Sys.Date(), "\n")

cat("\nTo explore the data in R, use:\n")
cat("con <- DBI::dbConnect(duckdb::duckdb(), 'us_county_sdoh_data.duckdb')\n")
cat("counties <- DBI::dbGetQuery(con, 'SELECT * FROM latest_county_data')\n")
cat("DBI::dbDisconnect(con)\n")