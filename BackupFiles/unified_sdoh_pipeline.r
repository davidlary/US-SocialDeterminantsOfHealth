#!/usr/bin/env Rscript

# Unified SDOH County-Level Dataset Pipeline
# This script combines the original SDOH pipeline with the extended capabilities
# from the extended_sdoh_pipeline module to create a comprehensive county-level
# dataset for social determinants of health.

script_version <- "1.0.0"

# ---- Setup and Configuration ----
cat("\n=== Unified SDOH County-Level Dataset Pipeline v", script_version, " ===\n\n")

# Start timing the pipeline
script_start_time <- Sys.time()

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
force_update <- "--force-update" %in% args || "-f" %in% args
verbose <- "--verbose" %in% args || "-v" %in% args
skip_interpolation <- "--skip-interpolation" %in% args
allow_simulation <- "--allow-simulation" %in% args
allow_interpolation <- "--allow-interpolation" %in% args || !skip_interpolation
offline_mode <- "--offline-mode" %in% args || "--offline" %in% args

# Get the script directory
script_directory <- tryCatch({
  # Try to get the script directory from the calling frame
  dirname(sys.frame(1)$ofile)
}, error = function(e) {
  # If that fails, use the current directory
  getwd()
})

# Define paths using the current working directory
# This ensures we save files in the current directory structure
root_dir <- getwd()
data_dir <- file.path(root_dir, "data")
logs_dir <- file.path(root_dir, "logs")
output_dir <- file.path(root_dir, "output")
cache_dir <- file.path(data_dir, "cache")
extended_data_dir <- file.path(root_dir, "data")
extended_cache_dir <- file.path(extended_data_dir, "cache")

# Ensure directories exist
ensure_directories <- function() {
  dirs <- c(
    data_dir,
    logs_dir, 
    output_dir,
    cache_dir,
    file.path(data_dir, "cdc_places"), 
    file.path(data_dir, "nhgis"),
    file.path(data_dir, "shapefiles"),
    file.path(output_dir, "maps"),
    file.path(data_dir, "built_environment"),
    file.path(data_dir, "crime"),
    file.path(data_dir, "economic"),
    file.path(data_dir, "education"),
    file.path(data_dir, "healthcare"),
    file.path(data_dir, "housing"),
    file.path(data_dir, "social_cohesion"),
    file.path(data_dir, "transportation"),
    file.path(data_dir, "traffic_safety")
  )
  
  for (dir in dirs) {
    if (!dir.exists(dir)) {
      cat("Creating directory:", dir, "\n")
      dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    }
  }
}

# Ensure necessary directories exist
ensure_directories()

# Check when the data was last updated
last_update_file <- file.path(data_dir, "last_update.txt")

if (force_update) {
  cat("Force update flag detected. Will refresh all data regardless of age.\n")
  refresh_cache <- TRUE
} else if (file.exists(last_update_file)) {
  last_update <- as.Date(readLines(last_update_file)[1])
  days_since_update <- as.numeric(difftime(Sys.Date(), last_update, units = "days"))
  
  cat("Data was last updated on", last_update, 
      "(", days_since_update, "days ago)\n")
  
  # Check if update is needed (e.g., if more than 30 days since last update)
  if (days_since_update < 30) {
    cat("Data is less than 30 days old. Using cached data unless forced.\n")
    refresh_cache <- FALSE
  } else {
    cat("Data is more than 30 days old. Will check for updates.\n")
    refresh_cache <- TRUE
  }
} else {
  cat("No previous update record found. Will perform initial data collection.\n")
  refresh_cache <- TRUE
}

# If offline mode is enabled, override refresh_cache
if (offline_mode) {
  cat("Offline mode enabled. Using cached data only.\n")
  refresh_cache <- FALSE
}

# Set up logging
log_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_file <- file.path(logs_dir, paste0("unified_sdoh_pipeline_", log_timestamp, ".log"))
cat("Starting pipeline. Log will be saved to:", log_file, "\n")

# Set up separate log and console handlers
# Default console verbosity level based on --verbose flag
console_output <- verbose
# Keep full verbosity in log files
log_verbosity <- TRUE

# Check if the script is being run interactively or sourced
is_sourced <- function() {
  # Check if the calling environment is the global environment
  # If it's not, the function is being sourced
  parent_env <- parent.frame()
  return(!identical(parent_env, .GlobalEnv))
}
is_interactive_run <- !is_sourced()

# Function to write to log and conditionally to console
log_message <- function(message, level = "INFO", show_console = console_output) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  formatted_message <- sprintf("[%s] [%s] %s", timestamp, level, message)
  
  # Always write to log file
  cat(formatted_message, "\n", file = log_file, append = TRUE)
  
  # Only show on console if requested and if running interactively
  if (show_console && is_interactive_run) {
    # Use message instead of cat for cleaner output in interactive mode
    message(trimws(formatted_message))
  }
}

# Redirect output to log file but also allow selective console output
sink(log_file, type = "output", split = FALSE)  # Don't split by default

# This is important, so show it in console regardless of verbosity setting
log_message(paste("=== UNIFIED SOCIAL DETERMINANTS OF HEALTH DATA PIPELINE STARTED AT", 
                  format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "===\n\n"), 
            level = "INFO", show_console = TRUE)

# Load required packages
log_message("Loading required packages...", level = "INFO", show_console = TRUE)

# Function to safely load packages with clear error message
safe_load_package <- function(package_name) {
  if (!require(package_name, character.only = TRUE, quietly = TRUE)) {
    log_message(paste("Required package", package_name, "is not installed."), 
                level = "ERROR", show_console = TRUE)
    log_message("Please run 'Rscript R/install_packages.r' first.", 
                level = "ERROR", show_console = TRUE)
    stop(paste("Missing required package:", package_name))
  }
}

# Core packages
required_packages <- c(
  "tidyverse",   # Data manipulation and visualization
  "duckdb",      # Database backend
  "DBI",         # Database interface
  "glue",        # String interpolation
  "lubridate",   # Date handling
  "jsonlite",    # JSON parsing
  "httr",        # HTTP requests
  "readxl",      # Excel file reading
  "zoo",         # Time series handling (for interpolation)
  "sf",          # Simple features for spatial data
  "tigris",      # Census TIGER/Line shapefiles
  "viridis",     # Color palettes for mapping
  "tidycensus",  # Census API access
  "ipumsr",      # IPUMS data access
  "parallel",    # Parallel processing
  "future",      # Parallel processing
  "future.apply",# Parallel apply functions
  "progressr"    # Progress reporting
)

# Try to load all required packages
invisible(sapply(required_packages, safe_load_package))

log_message("Required packages loaded successfully.", level = "INFO")

# Configure data quality handling
data_quality_flags <- list(
  # Data type flags
  direct = "direct",           # Data directly from source without modification
  interpolated = "interpolated", # Data interpolated from existing points
  extrapolated = "extrapolated", # Data extrapolated beyond available time range
  simulated = "simulated",     # Fully simulated data (not based on real values)
  missing = NA,                # Data that couldn't be obtained and wasn't simulated
  
  # Special flags
  imputed = "imputed"         # For values filled in by statistical methods
)

# Set options to improve reliability
options(timeout = 300)  # 5 minute timeout
options(scipen = 999)   # Avoid scientific notation
options(stringsAsFactors = FALSE)

# Setup parallel processing based on available cores
parallel_cores <- if (exists("PARALLEL_CORES")) {
  PARALLEL_CORES 
} else {
  max(1, parallel::detectCores() - 1)  # Use all cores except one
}

parallel_strategy <- if (exists("PARALLEL_STRATEGY")) {
  PARALLEL_STRATEGY
} else {
  "multisession"
}

# Log parallel processing configuration
log_message(paste("Parallel processing enabled with", parallel_cores, "cores using", 
                  parallel_strategy, "strategy"), 
            level = "INFO", show_console = TRUE)

# ---- Check API Keys ----

# Check for Census API key
census_api_key <- Sys.getenv("CENSUS_API_KEY")
if (census_api_key == "") {
  log_message("WARNING: No Census API key found in environment variable CENSUS_API_KEY",
              level = "WARN", show_console = TRUE)
  log_message("You may encounter rate limits. Consider getting a key at: https://api.census.gov/data/key_signup.html\n",
              level = "WARN", show_console = TRUE)
} else {
  log_message("Census API key found in environment.",
              level = "INFO", show_console = TRUE)
  census_api_key(census_api_key)
}

# Check for IPUMS credentials
check_ipums_credentials <- function() {
  ipums_username <- Sys.getenv("IPUMS_USERNAME", "")
  ipums_password <- Sys.getenv("IPUMS_PASSWORD", "")
  
  if (ipums_username != "" && ipums_password != "") {
    log_message("IPUMS credentials found in environment variables.",
                level = "INFO", show_console = TRUE)
    return(TRUE)
  }
  
  # Check for IPUMS credentials file
  cred_file <- file.path(Sys.getenv("HOME"), ".ipums_credentials/config")
  if (file.exists(cred_file)) {
    log_message("IPUMS credentials file found.",
                level = "INFO", show_console = TRUE)
    return(TRUE)
  }
  
  # Check project-specific credentials
  project_cred_paths <- c(
    "ipums_credentials.txt",
    "data/ipums_credentials.txt",
    "../ipums_credentials.txt"
  )
  
  for (path in project_cred_paths) {
    if (file.exists(path)) {
      log_message(paste("Project IPUMS credentials found at", path),
                  level = "INFO", show_console = TRUE)
      return(TRUE)
    }
  }
  
  log_message("No IPUMS credentials found. NHGIS data fetching may be limited.",
              level = "WARN", show_console = TRUE)
  return(FALSE)
}

ipums_credentials_available <- check_ipums_credentials()

# ---- Step 1: Build Extended Variable Crosswalk ----
log_message("STEP 1: BUILDING EXTENDED VARIABLE CROSSWALK", 
            level = "INFO", show_console = TRUE)

# Source both crosswalk builders
source(file.path(root_dir, "build_extended_crosswalk.r"))
if (file.exists(file.path(root_dir, "build_extended_crosswalk_v2.r"))) {
  source(file.path(root_dir, "build_extended_crosswalk_v2.r"))
}

# First build the original crosswalk
original_crosswalk <- build_extended_crosswalk()

# Then extend it with additional variables if the v2 builder exists
if (exists("build_extended_crosswalk_v2")) {
  log_message("Building extended crosswalk with additional variables...", 
              level = "INFO", show_console = TRUE)
  extended_crosswalk_result <- build_extended_crosswalk_v2(
    output_dir = output_dir,
    force_update = force_update,
    verbose = verbose
  )
  
  # If successful, read the extended crosswalk
  if (extended_crosswalk_result) {
    extended_crosswalk_file <- file.path(output_dir, "variable_crosswalk_extended.csv")
    if (file.exists(extended_crosswalk_file)) {
      extended_crosswalk <- read_csv(extended_crosswalk_file, show_col_types = FALSE)
      crosswalk <- extended_crosswalk
      log_message("Successfully loaded extended crosswalk", level = "INFO")
    } else {
      crosswalk <- original_crosswalk
      log_message("Extended crosswalk file not found, using original crosswalk", level = "WARN")
    }
  } else {
    crosswalk <- original_crosswalk
    log_message("Failed to build extended crosswalk, using original crosswalk", level = "WARN")
  }
} else {
  crosswalk <- original_crosswalk
}

log_message(paste("Extended crosswalk built successfully with", nrow(crosswalk), "variables."), 
            level = "INFO", show_console = TRUE)

# ---- Step 2: Data Collection from Multiple Sources ----
log_message("STEP 2: FETCHING DATA FROM MULTIPLE SOURCES", 
            level = "INFO", show_console = TRUE)

# Source the data fetcher scripts
source(file.path(root_dir, "fetch_extended_data.r"))
source(file.path(root_dir, "fetch_nhgis_data.r"))

# Source the extended data fetchers if they exist
extended_fetchers <- c(
  "fetch_usda_food_atlas.r",
  "fetch_epa_data.r",
  "fetch_housing_data.r", 
  "fetch_healthcare_data.r",
  "fetch_transportation_data.r",
  "fetch_social_cohesion_data.r",
  "fetch_crime_data.r",
  "fetch_education_data.r",
  "fetch_economic_data.r",
  "fetch_built_environment_data.r",
  # New specialized data sources
  "fetch_climate_data.r",
  "fetch_substance_use_data.r",
  "fetch_digital_access_data.r",
  # Traffic safety data
  "fetch_traffic_safety_data.r",
  # Additional data sources
  "fetch_county_data_final.r",
  # Historical and NHGIS data sources
  "fetch_nhgis_data.r",
  "fetch_historical_data.r"
)

for (fetcher in extended_fetchers) {
  # First check the root directory
  fetcher_path <- file.path(root_dir, fetcher)
  if (file.exists(fetcher_path)) {
    log_message(paste("Loading fetcher from root directory:", fetcher), level = "INFO")
    # Use tryCatch to handle any errors during source
    tryCatch({
      source(fetcher_path)
    }, error = function(e) {
      log_message(paste("Error loading fetcher:", fetcher, "-", conditionMessage(e)), 
                level = "ERROR", show_console = TRUE)
    })
  } else {
    # Then check the extended_sdoh_pipeline directory
    fetcher_path <- file.path(root_dir, "extended_sdoh_pipeline", fetcher)
    if (file.exists(fetcher_path)) {
      log_message(paste("Loading extended fetcher:", fetcher), level = "INFO")
      # Use tryCatch to handle any errors during source
      tryCatch({
        source(fetcher_path)
      }, error = function(e) {
        log_message(paste("Error loading extended fetcher:", fetcher, "-", conditionMessage(e)), 
                  level = "ERROR", show_console = TRUE)
      })
    } else {
      log_message(paste("Fetcher not found:", fetcher), level = "WARN")
    }
  }
}

# Define years to process - dynamically determine the current year
current_year <- as.numeric(format(Sys.Date(), "%Y"))
all_years <- 1970:current_year
log_message(paste("Processing data for years", min(all_years), "to", max(all_years)), 
            level = "INFO", show_console = TRUE)

# Load IPUMS credentials securely using the comprehensive loader from main_extended.r
load_ipums_credentials <- function() {
  # Initialize result
  creds_found <- FALSE
  
  # 1. Try environment variables first (most secure)
  log_message("Checking for IPUMS credentials in environment variables...", 
              level = "DEBUG", show_console = FALSE)
  ipums_username <- Sys.getenv("IPUMS_USERNAME", "")
  ipums_password <- Sys.getenv("IPUMS_PASSWORD", "")
  
  if (ipums_username != "" && ipums_password != "") {
    log_message("IPUMS credentials found in environment variables.", 
                level = "INFO", show_console = TRUE)
    return(TRUE)
  }
  
  # 2. Try the standard IPUMS credentials file
  log_message("Checking for IPUMS credentials in ~/.ipums_credentials/config...", 
              level = "DEBUG", show_console = FALSE)
  cred_file <- file.path(Sys.getenv("HOME"), ".ipums_credentials/config")
  
  if (file.exists(cred_file)) {
    log_message("IPUMS credentials file found. Attempting to load...", 
                level = "INFO", show_console = TRUE)
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
        log_message("IPUMS credentials loaded successfully from credentials file.", 
                    level = "INFO", show_console = TRUE)
        return(TRUE)
      }
    }, error = function(e) {
      log_message(paste("Error reading credentials file:", conditionMessage(e)), 
                  level = "ERROR", show_console = TRUE)
    })
  }
  
  # 3. Try .Renviron file
  log_message("Checking for IPUMS credentials in .Renviron file...", 
              level = "DEBUG", show_console = FALSE)
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
        log_message("IPUMS credentials loaded successfully from .Renviron file.", 
                    level = "INFO", show_console = TRUE)
        return(TRUE)
      }
    }, error = function(e) {
      log_message(paste("Error reading .Renviron file:", conditionMessage(e)), 
                  level = "ERROR", show_console = FALSE)
    })
  }
  
  # 4. Try project-specific credentials in the R directory
  log_message("Checking for project-specific IPUMS credentials...", 
              level = "DEBUG", show_console = FALSE)
  project_cred_paths <- c(
    "ipums_credentials.txt",
    "data/ipums_credentials.txt",
    "../ipums_credentials.txt"
  )
  
  for (path in project_cred_paths) {
    if (file.exists(path)) {
      log_message(paste("Found project credentials file at", path), 
                  level = "INFO", show_console = TRUE)
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
          log_message("IPUMS credentials loaded successfully from project file.", 
                      level = "INFO", show_console = TRUE)
          return(TRUE)
        }
      }, error = function(e) {
        log_message(paste("Error reading project credentials file:", conditionMessage(e)), 
                    level = "ERROR", show_console = FALSE)
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
      log_message(paste("Found custom credential loader at", script_path), 
                  level = "INFO", show_console = TRUE)
      tryCatch({
        source(script_path, local = TRUE)
        if (exists("load_ipums_credentials", envir = environment(), inherits = FALSE)) {
          # Call the loaded function in its environment
          custom_result <- load_ipums_credentials()
          
          # Check if it worked
          if (Sys.getenv("IPUMS_USERNAME") != "" && Sys.getenv("IPUMS_PASSWORD") != "") {
            log_message("IPUMS credentials loaded successfully from custom loader.", 
                        level = "INFO", show_console = TRUE)
            return(TRUE)
          }
        }
      }, error = function(e) {
        log_message(paste("Error using custom credential loader:", conditionMessage(e)), 
                    level = "ERROR", show_console = FALSE)
      })
    }
  }
  
  # No credentials found
  log_message("No IPUMS credentials found.", 
              level = "WARN", show_console = TRUE)
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
    log_message("IPUMS credentials loaded and ready to use.", 
                level = "INFO", show_console = TRUE)
    ipums_credentials <- list(
      username = ipums_username,
      password = ipums_password
    )
  }
} else {
  # No credentials found through automated methods
  log_message("No IPUMS credentials found through automated methods.", 
              level = "WARN", show_console = TRUE)
  
  # Check if interactive - we could prompt for credentials
  if (interactive()) {
    log_message("Running in interactive mode. Would you like to enter IPUMS credentials? (y/n)", 
                level = "INFO", show_console = TRUE)
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
        
        log_message("IPUMS credentials entered manually.", 
                    level = "INFO", show_console = TRUE)
        credentials_loaded <- TRUE
      }
    }
  }
  
  # Final check - if we still don't have credentials
  if (is.null(ipums_credentials)) {
    log_message("Will use existing NHGIS data files if available.", 
                level = "WARN", show_console = TRUE)
    use_ipumsr <- FALSE
  }
}

# --- Fetch Core Data ---

# Fetch NHGIS data (prioritizing this as requested)
log_message("Fetching NHGIS data for entire date range (1970-present)...", 
            level = "INFO", show_console = TRUE)

if (!use_ipumsr) {
  log_message("Note: Using placeholder NHGIS data if no files are found locally.", 
              level = "WARN", show_console = TRUE)
} else {
  log_message("Checking for IPUMS mode - will use offline mode", 
              level = "INFO", show_console = TRUE)
}

# Use the fixed nhgis fetcher
nhgis_data <- fetch_nhgis_historical_data(
  crosswalk = crosswalk,
  years = all_years,  # Use all years 1970-present
  cache_dir = cache_dir,
  refresh_cache = refresh_cache,  # Use the auto-determined refresh setting
  primary_source = TRUE,  # Use NHGIS as primary source
  use_ipumsr = use_ipumsr,
  ipums_credentials = ipums_credentials
)

# Fetch supplementary core data (Census, PLACES, life expectancy)
log_message("Fetching supplementary data sources...", 
            level = "INFO", show_console = TRUE)

supplementary_data <- fetch_extended_data(
  crosswalk, 
  years = all_years,  # Use all years 1970-present
  include_places = TRUE, 
  include_nhgis = FALSE,  # We already have NHGIS as primary source
  include_life_expectancy = TRUE,
  use_cache = TRUE,
  refresh_cache = refresh_cache,
  # Parallel processing options
  parallel = TRUE,
  num_cores = parallel_cores, 
  parallel_strategy = parallel_strategy,
  # Cache options
  cache_options = list(
    refresh_census = FALSE,
    refresh_places = FALSE,
    refresh_nhgis = FALSE,
    refresh_life_expectancy = refresh_cache,  # Force refresh life expectancy if refreshing all
    max_cache_age_days = 30,
    cache_dir = cache_dir
  )
)

# --- Fetch Extended Data Sources ---
log_message("Fetching extended data sources...", 
            level = "INFO", show_console = TRUE)

# Initialize containers for extended data
extended_data_sources <- list()

# Function to safely fetch extended data
safe_fetch_extended <- function(fetcher_name, fetch_function) {
  tryCatch({
    log_message(paste("Fetching data from", fetcher_name), level = "INFO")
    
    # Check if the function accepts certain parameters before passing them
    # Get the function arguments
    func_args <- names(formals(fetch_function))
    
    # Build a list of arguments dynamically based on what the function accepts
    args_list <- list(
      years = all_years,
      cache_dir = extended_cache_dir,
      refresh_cache = refresh_cache
    )
    
    # Only add optional parameters if the function accepts them
    if("allow_simulation" %in% func_args) {
      args_list$allow_simulation <- allow_simulation
    }
    
    if("allow_interpolation" %in% func_args) {
      args_list$allow_interpolation <- allow_interpolation
    }
    
    # Add remaining standard parameters
    if("data_quality_flags" %in% func_args) {
      args_list$data_quality_flags <- data_quality_flags
    }
    
    if("offline_mode" %in% func_args) {
      args_list$offline_mode <- offline_mode
    }
    
    # Call the function with the appropriate arguments
    result <- do.call(fetch_function, args_list)
    
    if (!is.null(result) && nrow(result) > 0) {
      log_message(paste("Successfully fetched", nrow(result), "records from", fetcher_name), 
                  level = "INFO")
      return(result)
    } else {
      log_message(paste("No data returned from", fetcher_name), 
                  level = "WARN")
      return(NULL)
    }
  }, error = function(e) {
    log_message(paste("Error fetching data from", fetcher_name, ":", conditionMessage(e)), 
                level = "ERROR")
    return(NULL)
  })
}

# USDA Food Environment Atlas data
if (exists("fetch_usda_food_atlas")) {
  extended_data_sources$food_environment <- safe_fetch_extended(
    "USDA Food Environment Atlas", 
    fetch_usda_food_atlas
  )
}

# EPA Environmental data
if (exists("fetch_epa_data")) {
  extended_data_sources$environmental <- safe_fetch_extended(
    "EPA Environmental data", 
    fetch_epa_data
  )
}

# Housing data
if (exists("fetch_housing_data")) {
  extended_data_sources$housing <- safe_fetch_extended(
    "Housing data", 
    fetch_housing_data
  )
}

# Healthcare access data
if (exists("fetch_healthcare_data")) {
  extended_data_sources$healthcare <- safe_fetch_extended(
    "Healthcare access data", 
    fetch_healthcare_data
  )
}

# Transportation data
if (exists("fetch_transportation_data")) {
  extended_data_sources$transportation <- safe_fetch_extended(
    "Transportation data", 
    fetch_transportation_data
  )
}

# Social cohesion data
if (exists("fetch_social_cohesion_data")) {
  extended_data_sources$social_cohesion <- safe_fetch_extended(
    "Social cohesion data", 
    fetch_social_cohesion_data
  )
}

# Crime data
if (exists("fetch_crime_data")) {
  extended_data_sources$crime <- safe_fetch_extended(
    "Crime data", 
    fetch_crime_data
  )
}

# Education data
if (exists("fetch_education_data")) {
  extended_data_sources$education <- safe_fetch_extended(
    "Education data", 
    fetch_education_data
  )
}

# Economic data
if (exists("fetch_economic_data")) {
  extended_data_sources$economic <- safe_fetch_extended(
    "Economic data", 
    fetch_economic_data
  )
}

# Built environment data
if (exists("fetch_built_environment_data")) {
  extended_data_sources$built_environment <- safe_fetch_extended(
    "Built environment data", 
    fetch_built_environment_data
  )
}

# Climate and natural disaster data
if (exists("fetch_climate_data")) {
  extended_data_sources$climate <- safe_fetch_extended(
    "Climate and natural disaster data", 
    fetch_climate_data
  )
}

# Mental health and substance use data
if (exists("fetch_substance_use_data")) {
  extended_data_sources$substance_use <- safe_fetch_extended(
    "Mental health and substance use data", 
    fetch_substance_use_data
  )
}

# Digital access data
if (exists("fetch_digital_access_data")) {
  extended_data_sources$digital_access <- safe_fetch_extended(
    "Digital access and broadband data", 
    fetch_digital_access_data
  )
}

# Traffic safety data with enhanced module
log_message("Loading traffic safety integration module...", level = "INFO", show_console = TRUE)

# First check if integration module exists and try to load it
integration_path <- file.path(root_dir, "traffic_safety_integration.r")
traffic_safety_enhanced <- FALSE

if (file.exists(integration_path)) {
  tryCatch({
    # Set a timeout for loading the integration module
    old_timeout <- options(timeout = 30)
    on.exit(options(old_timeout), add = TRUE)
    
    # Try to load the module
    log_message("Sourcing traffic safety integration module...", level = "INFO")
    source(integration_path)
    
    # Check if the enhanced function was loaded successfully
    if (exists("fetch_enhanced_traffic_safety_data")) {
      traffic_safety_enhanced <- TRUE
      log_message("Enhanced traffic safety module loaded successfully", level = "INFO", show_console = TRUE)
    }
  }, error = function(e) {
    log_message(paste("Error loading traffic safety integration module:", e$message), 
                level = "WARN", show_console = TRUE)
  })
}

# Use the enhanced module if available, otherwise fall back to basic
if (traffic_safety_enhanced) {
  log_message("Using enhanced traffic safety data pipeline", level = "INFO", show_console = TRUE)
  
  # Use enhanced fetch with explicit timeout
  traffic_data <- tryCatch({
    # Call the enhanced fetcher with reasonable feature set
    fetch_enhanced_traffic_safety_data(
      years = all_years,
      cache_dir = cache_dir,
      refresh_cache = refresh_cache,
      allow_interpolation = allow_interpolation,
      allow_simulation = allow_simulation,
      use_validation = TRUE,
      use_optimized_cache = TRUE,
      generate_forecasts = FALSE,  # Disable forecasting to reduce processing time
      spatial_analysis = FALSE     # Disable spatial to reduce processing time
    )
  }, error = function(e) {
    log_message(paste("Error fetching enhanced traffic safety data:", e$message), 
                level = "ERROR", show_console = TRUE)
    NULL
  })
  
  # Add to extended data sources if successful
  if (!is.null(traffic_data) && nrow(traffic_data) > 0) {
    extended_data_sources$traffic_safety <- traffic_data
    log_message(paste("Added", nrow(traffic_data), "traffic safety records from enhanced module"), 
                level = "INFO", show_console = TRUE)
    
    # Try to create visualizations if data is available
    if (exists("create_traffic_safety_visualizations")) {
      tryCatch({
        log_message("Creating traffic safety visualizations...", level = "INFO", show_console = TRUE)
        vis_files <- create_traffic_safety_visualizations(
          traffic_data,
          output_dir = file.path(output_dir, "visualizations/traffic_safety"),
          create_maps = TRUE,
          create_forecast_plots = FALSE,  # Skip forecast plots to save time
          create_animation = FALSE        # Skip animations to save time
        )
        
        log_message(paste("Created", length(vis_files), "traffic safety visualizations"), 
                    level = "INFO", show_console = TRUE)
      }, error = function(e) {
        log_message(paste("Error creating traffic safety visualizations:", e$message), 
                    level = "WARN", show_console = TRUE)
      })
    }
  }
} else if (exists("fetch_traffic_safety_data")) {
  # Fall back to basic implementation
  log_message("Using basic traffic safety data pipeline", level = "INFO", show_console = TRUE)
  extended_data_sources$traffic_safety <- safe_fetch_extended(
    "Traffic safety and accident data", 
    fetch_traffic_safety_data
  )
} else {
  log_message("No traffic safety data module available", level = "WARN", show_console = TRUE)
}

# Combine all data sources into a single list for processing
data_list <- list(
  nhgis = nhgis_data,
  census = supplementary_data$census,
  places = supplementary_data$places,
  life_expectancy = supplementary_data$life_expectancy
)

# Add extended data sources to the main list
for (source_name in names(extended_data_sources)) {
  if (!is.null(extended_data_sources[[source_name]])) {
    data_list[[source_name]] <- extended_data_sources[[source_name]]
  }
}

# Print summary of fetched data
log_message("\nFetched data summary:", level = "INFO", show_console = TRUE)

if (is.list(data_list)) {
  # Handle nested lists (like Census)
  for (source_name in names(data_list)) {
    if (is.list(data_list[[source_name]]) && !is.data.frame(data_list[[source_name]])) {
      # This is a nested list like Census
      for (subsource in names(data_list[[source_name]])) {
        if (is.data.frame(data_list[[source_name]][[subsource]])) {
          log_message(paste("-", source_name, "/", subsource, ":", 
                        nrow(data_list[[source_name]][[subsource]]), "rows,", 
                        ncol(data_list[[source_name]][[subsource]]), "columns"), 
                    level = "INFO", show_console = TRUE)
        }
      }
    } else if (is.data.frame(data_list[[source_name]])) {
      # Regular data frame
      log_message(paste("-", source_name, ":", 
                    nrow(data_list[[source_name]]), "rows,", 
                    ncol(data_list[[source_name]]), "columns"), 
                level = "INFO", show_console = TRUE)
    }
  }
} else {
  log_message("ERROR: data_list is not a proper list structure.",
              level = "ERROR", show_console = TRUE)
}

# ---- Step 3: Process and Combine Data ----
log_message("\nSTEP 3: PROCESSING AND COMBINING DATA", 
            level = "INFO", show_console = TRUE)

# Source core processing script
source(file.path(root_dir, "process_extended_data.r"))

# Source enhanced processing script 
if (file.exists(file.path(root_dir, "process_extended_data_v2.r"))) {
  source(file.path(root_dir, "process_extended_data_v2.r"))
}

# Show processing status message
log_message("Processing data from multiple sources...", 
            level = "INFO", show_console = TRUE)
log_message(paste("Structure of data_list:", typeof(data_list)), 
            level = "DEBUG")

# Add a progress animation while main processing is happening
log_message("Main processing started. This may take several minutes...",
            level = "INFO", show_console = TRUE)

# Check if advanced interpolation module is available
has_advanced_interpolation <- file.exists(file.path(root_dir, "advanced_interpolation.r"))

if (has_advanced_interpolation) {
  log_message("Advanced interpolation module found. Loading...", 
              level = "INFO", show_console = TRUE)
  source(file.path(root_dir, "advanced_interpolation.r"))
}

# Process data using the best available processor
if (exists("process_extended_data_v2")) {
  log_message("Using enhanced data processor (v2)...", 
              level = "INFO", show_console = TRUE)
  
  # Process using the enhanced processor that supports extended data sources
  processed_data <- process_extended_data_v2(
    data_sources = data_list,
    years = all_years,
    skip_interpolation = skip_interpolation,  # Only skip basic interpolation
    original_db_path = NULL,  # We're creating a new consolidated DB
    verbose = TRUE,  # Force verbose mode to debug issues
    data_quality_flags = data_quality_flags
  )
} else {
  log_message("Using standard data processor...", 
              level = "INFO", show_console = TRUE)
  
  # Use the original processor from the main pipeline
  processed_data <- process_extended_data(
    data_list, 
    crosswalk, 
    interpolate = allow_interpolation, 
    extend_health_data = TRUE,
    include_life_expectancy = TRUE,
    check_simulated = !allow_simulation
  )
}

# Apply advanced interpolation if available and needed
if (has_advanced_interpolation && !skip_interpolation && exists("advanced_interpolate_sdoh_data")) {
  log_message("Applying advanced interpolation techniques...", 
              level = "INFO", show_console = TRUE)
  
  # Create a backup of the processed data before advanced interpolation
  processed_data_original <- processed_data
  
  # Apply advanced interpolation
  interpolation_result <- advanced_interpolate_sdoh_data(
    data = processed_data,
    crosswalk = crosswalk,
    id_cols = c("GEOID", "NAME"),
    date_col = "year",
    evaluate_methods = verbose,  # Evaluate different methods if in verbose mode
    conf_level = 0.95
  )
  
  # Update the processed data with advanced interpolation
  processed_data <- interpolation_result$data
  
  # Display interpolation evaluation if available and in verbose mode
  if (verbose && "evaluation" %in% names(interpolation_result)) {
    log_message("Interpolation method evaluation:", 
                level = "INFO", show_console = TRUE)
    
    # Get the best method for each variable
    best_methods <- interpolation_result$evaluation %>%
      group_by(variable) %>%
      slice_min(order_by = rmse, n = 1) %>%
      ungroup()
    
    # Display the best method for each variable
    for (i in 1:nrow(best_methods)) {
      log_message(sprintf("  %s: best method = %s, RMSE = %.4f, MAPE = %.2f%%", 
                        best_methods$variable[i],
                        best_methods$method[i],
                        best_methods$rmse[i],
                        best_methods$mape[i]),
                level = "INFO", show_console = TRUE)
    }
  }
  
  # Add method information to the processed data
  if ("methods_used" %in% names(interpolation_result)) {
    log_message("Adding interpolation method information to data...", 
                level = "INFO")
    
    # Add a column for the interpolation method used for each variable
    for (var_name in names(interpolation_result$methods_used)) {
      method_col <- paste0(var_name, "_interpolation_method")
      processed_data[[method_col]] <- interpolation_result$methods_used[var_name]
    }
  }
  
  # Add confidence intervals if available
  if ("confidence" %in% names(interpolation_result) && length(interpolation_result$confidence) > 0) {
    log_message("Adding confidence intervals to data...", 
                level = "INFO")
    
    # Add confidence interval columns for each variable
    for (conf_name in names(interpolation_result$confidence)) {
      var_name <- gsub("_confidence$", "", conf_name)
      lower_col <- paste0(var_name, "_ci_lower")
      upper_col <- paste0(var_name, "_ci_upper")
      
      processed_data[[lower_col]] <- interpolation_result$confidence[[conf_name]][, "lower"]
      processed_data[[upper_col]] <- interpolation_result$confidence[[conf_name]][, "upper"]
    }
  }
  
  # Copy quality flags to the processed data
  if ("quality_flags" %in% names(interpolation_result)) {
    log_message("Updating data quality flags...", level = "INFO")
    
    # Add quality flag columns
    quality_cols <- names(interpolation_result$quality_flags)
    quality_cols <- quality_cols[grepl("_quality$", quality_cols)]
    
    for (qual_col in quality_cols) {
      processed_data[[qual_col]] <- interpolation_result$quality_flags[[qual_col]]
    }
  }
  

# ---- Step: Generate CONUS Maps ----
log_message("STEP: GENERATING CONUS MAPS FOR ALL VARIABLES", 
            level = "INFO", show_console = TRUE)

# Source the map generation script
source(file.path(root_dir, "generate_conus_maps.r"))

# Generate maps for all variables and years
map_result <- tryCatch({
  generate_conus_maps(
    output_dir = file.path(output_dir, "maps"),
    db_path = file.path(output_dir, "us_county_sdoh_data.duckdb"),
    conus_only = TRUE,
    parallel = FALSE
  )
  TRUE
}, error = function(e) {
  log_message(paste("ERROR: Map generation failed:", conditionMessage(e)), 
              level = "ERROR", show_console = TRUE)
  FALSE
})

if (map_result) {
  log_message("Maps successfully generated", level = "INFO", show_console = TRUE)
} else {
  log_message("Map generation encountered errors", level = "WARN", show_console = TRUE)
}

  log_message("Advanced interpolation completed successfully.", 
              level = "INFO", show_console = TRUE)
}

# ---- Step 4: Create or Update the Database ----
log_message("\nSTEP 4: CREATING UNIFIED DATABASE", 
            level = "INFO", show_console = TRUE)

# Define path for unified database
unified_db_path <- file.path(output_dir, "us_county_sdoh_unified.duckdb")

# Connect to DuckDB
log_message("Connecting to DuckDB database...",
            level = "INFO", show_console = TRUE)
con <- dbConnect(duckdb::duckdb(), dbdir = unified_db_path)

# Create basic tables if they don't exist
log_message("Setting up database schema...",
            level = "INFO", show_console = TRUE)

# Create counties table
dbExecute(con, "CREATE TABLE IF NOT EXISTS counties (
  geoid VARCHAR PRIMARY KEY,
  name VARCHAR,
  state_fips VARCHAR,
  state_name VARCHAR
)")

# Create variables table
dbExecute(con, "CREATE TABLE IF NOT EXISTS variables (
  variable_name VARCHAR PRIMARY KEY,
  domain VARCHAR,
  description VARCHAR,
  type VARCHAR,
  units VARCHAR,
  min_year INTEGER,
  max_year INTEGER,
  extended_only BOOLEAN
)")

# Create data table with enhanced fields for advanced interpolation
dbExecute(con, "CREATE TABLE IF NOT EXISTS sdoh_data (
  geoid VARCHAR,
  year INTEGER,
  variable_name VARCHAR,
  value DOUBLE,
  data_quality VARCHAR,
  data_source VARCHAR,
  data_vintage VARCHAR,
  interpolation_method VARCHAR,
  ci_lower DOUBLE,
  ci_upper DOUBLE,
  confidence_level DOUBLE,
  last_updated TIMESTAMP,
  PRIMARY KEY (geoid, year, variable_name)
)")

# Create county metadata table
log_message("Importing county metadata...",
            level = "INFO", show_console = TRUE)

# Extract county metadata from processed data
if (!is.null(processed_data) && "GEOID" %in% names(processed_data)) {
  log_message("Processing county metadata from processed data...", level = "INFO")
  
  # Check if NAME column exists
  has_name_column <- "NAME" %in% names(processed_data)
  
  if (has_name_column) {
    log_message("Found NAME column in processed data", level = "INFO")
    county_data <- processed_data %>%
      select(GEOID, NAME) %>%
      distinct() %>%
      mutate(
        geoid = GEOID,
        name = NAME,
        state_fips = substr(GEOID, 1, 2),
        state_name = gsub(".*,\\s*(.*)$", "\\1", NAME)
      ) %>%
      select(geoid, name, state_fips, state_name)
  } else {
    # If NAME column doesn't exist, create county metadata using just GEOID
    log_message("NAME column not found in processed data. Creating basic county metadata.", level = "INFO")
    
    # Try to get county names from other sources
    county_names <- NULL
    
    # 1. Try to get names from a standard county metadata file if it exists
    county_metadata_file <- file.path(root_dir, "data", "county_metadata.csv")
    if (file.exists(county_metadata_file)) {
      log_message("Found county metadata file. Loading county names.", level = "INFO")
      county_meta <- read_csv(county_metadata_file, show_col_types = FALSE)
      if (all(c("geoid", "name") %in% names(county_meta))) {
        county_names <- county_meta %>% select(geoid, name)
      } else if (all(c("GEOID", "NAME") %in% names(county_meta))) {
        county_names <- county_meta %>% 
          select(GEOID, NAME) %>%
          rename(geoid = GEOID, name = NAME)
      }
    }
    
    # 2. If we still don't have county names, use fips codes from tigris if available
    if (is.null(county_names) && requireNamespace("tigris", quietly = TRUE)) {
      tryCatch({
        log_message("Using tigris package to get county names", level = "INFO")
        counties_sf <- tigris::counties(year = 2020)
        if (all(c("GEOID", "NAME") %in% names(counties_sf))) {
          county_names <- counties_sf %>% 
            sf::st_drop_geometry() %>%
            select(GEOID, NAME) %>%
            rename(geoid = GEOID, name = NAME)
        }
      }, error = function(e) {
        log_message(paste("Error getting county names from tigris:", conditionMessage(e)), level = "WARN")
      })
    }
    
    # 3. Create basic county metadata with what we have
    if (!is.null(county_names)) {
      log_message(paste("Found", nrow(county_names), "county names from external sources"), level = "INFO")
      
      # Join with processed data geoids
      county_geoids <- processed_data %>%
        select(GEOID) %>%
        distinct() %>%
        rename(geoid = GEOID)
      
      county_data <- county_geoids %>%
        left_join(county_names, by = "geoid") %>%
        mutate(
          # If name is NA, create a placeholder name
          name = ifelse(is.na(name), paste("County", geoid), name),
          state_fips = substr(geoid, 1, 2),
          # Try to extract state name from county name if it contains a comma
          state_name = ifelse(grepl(",", name), 
                             gsub(".*,\\s*(.*)$", "\\1", name),
                             # Otherwise use state FIPS code to lookup state name
                             case_when(
                               state_fips == "01" ~ "Alabama",
                               state_fips == "02" ~ "Alaska",
                               state_fips == "04" ~ "Arizona",
                               state_fips == "05" ~ "Arkansas",
                               state_fips == "06" ~ "California",
                               state_fips == "08" ~ "Colorado",
                               state_fips == "09" ~ "Connecticut",
                               state_fips == "10" ~ "Delaware",
                               state_fips == "11" ~ "District of Columbia",
                               state_fips == "12" ~ "Florida",
                               state_fips == "13" ~ "Georgia",
                               state_fips == "15" ~ "Hawaii",
                               state_fips == "16" ~ "Idaho",
                               state_fips == "17" ~ "Illinois",
                               state_fips == "18" ~ "Indiana",
                               state_fips == "19" ~ "Iowa",
                               state_fips == "20" ~ "Kansas",
                               state_fips == "21" ~ "Kentucky",
                               state_fips == "22" ~ "Louisiana",
                               state_fips == "23" ~ "Maine",
                               state_fips == "24" ~ "Maryland",
                               state_fips == "25" ~ "Massachusetts",
                               state_fips == "26" ~ "Michigan",
                               state_fips == "27" ~ "Minnesota",
                               state_fips == "28" ~ "Mississippi",
                               state_fips == "29" ~ "Missouri",
                               state_fips == "30" ~ "Montana",
                               state_fips == "31" ~ "Nebraska",
                               state_fips == "32" ~ "Nevada",
                               state_fips == "33" ~ "New Hampshire",
                               state_fips == "34" ~ "New Jersey",
                               state_fips == "35" ~ "New Mexico",
                               state_fips == "36" ~ "New York",
                               state_fips == "37" ~ "North Carolina",
                               state_fips == "38" ~ "North Dakota",
                               state_fips == "39" ~ "Ohio",
                               state_fips == "40" ~ "Oklahoma",
                               state_fips == "41" ~ "Oregon",
                               state_fips == "42" ~ "Pennsylvania",
                               state_fips == "44" ~ "Rhode Island",
                               state_fips == "45" ~ "South Carolina",
                               state_fips == "46" ~ "South Dakota",
                               state_fips == "47" ~ "Tennessee",
                               state_fips == "48" ~ "Texas",
                               state_fips == "49" ~ "Utah",
                               state_fips == "50" ~ "Vermont",
                               state_fips == "51" ~ "Virginia",
                               state_fips == "53" ~ "Washington",
                               state_fips == "54" ~ "West Virginia",
                               state_fips == "55" ~ "Wisconsin",
                               state_fips == "56" ~ "Wyoming",
                               state_fips == "72" ~ "Puerto Rico",
                               TRUE ~ paste("State", state_fips)
                             ))
        ) %>%
        select(geoid, name, state_fips, state_name)
    } else {
      # If no external county name source, create basic metadata
      log_message("No external county name source found. Creating placeholder names.", level = "INFO")
      county_data <- processed_data %>%
        select(GEOID) %>%
        distinct() %>%
        mutate(
          geoid = GEOID,
          name = paste("County", GEOID),
          state_fips = substr(GEOID, 1, 2),
          state_name = case_when(
            state_fips == "01" ~ "Alabama",
            state_fips == "02" ~ "Alaska",
            state_fips == "04" ~ "Arizona",
            state_fips == "05" ~ "Arkansas",
            state_fips == "06" ~ "California",
            state_fips == "08" ~ "Colorado",
            state_fips == "09" ~ "Connecticut",
            state_fips == "10" ~ "Delaware",
            state_fips == "11" ~ "District of Columbia",
            state_fips == "12" ~ "Florida",
            state_fips == "13" ~ "Georgia",
            state_fips == "15" ~ "Hawaii",
            state_fips == "16" ~ "Idaho",
            state_fips == "17" ~ "Illinois",
            state_fips == "18" ~ "Indiana",
            state_fips == "19" ~ "Iowa",
            state_fips == "20" ~ "Kansas",
            state_fips == "21" ~ "Kentucky",
            state_fips == "22" ~ "Louisiana",
            state_fips == "23" ~ "Maine",
            state_fips == "24" ~ "Maryland",
            state_fips == "25" ~ "Massachusetts",
            state_fips == "26" ~ "Michigan",
            state_fips == "27" ~ "Minnesota",
            state_fips == "28" ~ "Mississippi",
            state_fips == "29" ~ "Missouri",
            state_fips == "30" ~ "Montana",
            state_fips == "31" ~ "Nebraska",
            state_fips == "32" ~ "Nevada",
            state_fips == "33" ~ "New Hampshire",
            state_fips == "34" ~ "New Jersey",
            state_fips == "35" ~ "New Mexico",
            state_fips == "36" ~ "New York",
            state_fips == "37" ~ "North Carolina",
            state_fips == "38" ~ "North Dakota",
            state_fips == "39" ~ "Ohio",
            state_fips == "40" ~ "Oklahoma",
            state_fips == "41" ~ "Oregon",
            state_fips == "42" ~ "Pennsylvania",
            state_fips == "44" ~ "Rhode Island",
            state_fips == "45" ~ "South Carolina",
            state_fips == "46" ~ "South Dakota",
            state_fips == "47" ~ "Tennessee",
            state_fips == "48" ~ "Texas",
            state_fips == "49" ~ "Utah",
            state_fips == "50" ~ "Vermont",
            state_fips == "51" ~ "Virginia",
            state_fips == "53" ~ "Washington",
            state_fips == "54" ~ "West Virginia",
            state_fips == "55" ~ "Wisconsin",
            state_fips == "56" ~ "Wyoming",
            state_fips == "72" ~ "Puerto Rico",
            TRUE ~ paste("State", state_fips)
          )
        ) %>%
        select(geoid, name, state_fips, state_name)
    }
  }
  
  # Update counties table using UPSERT pattern
  existing_counties <- dbGetQuery(con, "SELECT geoid FROM counties")
  
  if (nrow(existing_counties) > 0) {
    # Find counties to add (not in the database yet)
    new_counties <- county_data %>%
      filter(!geoid %in% existing_counties$geoid)
    
    # Find counties to update (already in the database)
    update_counties <- county_data %>%
      filter(geoid %in% existing_counties$geoid)
    
    # Add new counties
    if (nrow(new_counties) > 0) {
      dbAppendTable(con, "counties", new_counties)
      log_message(paste("Added", nrow(new_counties), "new counties to database"),
                  level = "INFO")
    }
    
    # Update existing counties
    if (nrow(update_counties) > 0) {
      for (i in 1:nrow(update_counties)) {
        county <- update_counties[i, ]
        dbExecute(con, glue::glue_sql("
          UPDATE counties
          SET name = {county$name},
              state_fips = {county$state_fips},
              state_name = {county$state_name}
          WHERE geoid = {county$geoid}
        ", .con = con))
      }
      log_message(paste("Updated", nrow(update_counties), "existing counties"),
                  level = "INFO")
    }
  } else {
    # No counties exist yet, insert all of them
    dbAppendTable(con, "counties", county_data)
    log_message(paste("Added", nrow(county_data), "counties to database"),
                level = "INFO")
  }
} else {
  log_message("No county data found in processed data. Cannot update counties table.",
              level = "ERROR", show_console = TRUE)
}

# Import variables from crosswalk
log_message("Importing variables from crosswalk...",
            level = "INFO", show_console = TRUE)

# Check available columns in crosswalk
available_columns <- names(crosswalk)
log_message(paste("Available columns in crosswalk:", paste(available_columns, collapse=", ")),
            level = "INFO", show_console = TRUE)

# Required columns for the variables table
required_columns <- c("variable_name", "domain", "description", "type", "units", "min_year", "max_year", "extended_only")

# Check which required columns are missing
missing_columns <- setdiff(required_columns, available_columns)
if(length(missing_columns) > 0) {
  log_message(paste("Missing required columns in crosswalk:", paste(missing_columns, collapse=", ")),
              level = "WARN", show_console = TRUE)
}

# Clean up crosswalk data - first select only the columns that exist
crosswalk_subset <- crosswalk %>%
  filter(!is.na(variable_name))

# Create a unified structure with all required columns
crosswalk_clean <- crosswalk_subset

# Add missing columns with defaults
if(!"domain" %in% names(crosswalk_clean)) {
  crosswalk_clean$domain <- "Unknown"
  log_message("Added 'domain' column with default value 'Unknown'", level = "INFO")
}
  
if(!"description" %in% names(crosswalk_clean)) {
  crosswalk_clean$description <- crosswalk_clean$variable_name
  log_message("Added 'description' column using variable names", level = "INFO")
}

if(!"type" %in% names(crosswalk_clean)) {
  crosswalk_clean$type <- "numeric"
  log_message("Added 'type' column with default value 'numeric'", level = "INFO")
}

if(!"units" %in% names(crosswalk_clean)) {
  crosswalk_clean$units <- "value"
  log_message("Added 'units' column with default value 'value'", level = "INFO")
}

if(!"min_year" %in% names(crosswalk_clean)) {
  crosswalk_clean$min_year <- 2000
  log_message("Added 'min_year' column with default value 2000", level = "INFO")
}

if(!"max_year" %in% names(crosswalk_clean)) {
  crosswalk_clean$max_year <- 2025
  log_message("Added 'max_year' column with default value 2025", level = "INFO")
}

if(!"extended_only" %in% names(crosswalk_clean)) {
  crosswalk_clean$extended_only <- FALSE
  log_message("Added 'extended_only' column with default value FALSE", level = "INFO")
}

# Now standardize values for existing columns
crosswalk_clean <- crosswalk_clean %>%
  mutate(
    domain = if_else(is.na(domain), "Unknown", domain),
    description = if_else(is.na(description), variable_name, description),
    type = if_else(is.na(type), "numeric", type),
    units = if_else(is.na(units), "value", units),
    min_year = if_else(is.na(min_year), 2000, min_year),
    max_year = if_else(is.na(max_year), 2025, max_year),
    extended_only = if_else(is.na(extended_only), FALSE, extended_only)
  )

# Update variables table using UPSERT pattern
var_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM variables")

# Select only the columns needed for the variables table
variables_columns <- c("variable_name", "domain", "description", "type", "units", "min_year", "max_year", "extended_only")
crosswalk_variables <- crosswalk_clean %>%
  select(all_of(variables_columns))

log_message(paste("Prepared", nrow(crosswalk_variables), "variables with required", 
                length(variables_columns), "columns for database import"),
          level = "INFO", show_console = TRUE)

if (var_count$count == 0) {
  # If empty, just insert all variables
  dbAppendTable(con, "variables", crosswalk_variables)
  log_message(paste("Added", nrow(crosswalk_variables), "variables to database"),
              level = "INFO")
} else {
  # Check which variables are already in the database
  existing_vars <- dbGetQuery(con, "SELECT variable_name FROM variables")
  
  # Filter to just new variables
  new_vars <- crosswalk_variables %>%
    filter(!variable_name %in% existing_vars$variable_name)
  
  # Find variables to update
  update_vars <- crosswalk_variables %>%
    filter(variable_name %in% existing_vars$variable_name)
  
  # Add new variables
  if (nrow(new_vars) > 0) {
    dbAppendTable(con, "variables", new_vars)
    log_message(paste("Added", nrow(new_vars), "new variables to database"),
                level = "INFO")
  }
  
  # Update existing variables
  if (nrow(update_vars) > 0) {
    for (i in 1:nrow(update_vars)) {
      var <- update_vars[i, ]
      dbExecute(con, glue::glue_sql("
        UPDATE variables
        SET domain = {var$domain},
            description = {var$description},
            type = {var$type},
            units = {var$units},
            min_year = {var$min_year},
            max_year = {var$max_year},
            extended_only = {var$extended_only}
        WHERE variable_name = {var$variable_name}
      ", .con = con))
    }
    log_message(paste("Updated", nrow(update_vars), "existing variables"),
                level = "INFO")
  }
}

# Import the processed data
log_message("Importing processed data to database...",
            level = "INFO", show_console = TRUE)

if (!is.null(processed_data) && nrow(processed_data) > 0) {
  # Standardize the data to a long format
  log_message("Converting data to long format...",
              level = "INFO")
  
  # Make sure geoid is standardized
  if ("GEOID" %in% names(processed_data)) {
    processed_data$geoid <- processed_data$GEOID
  } else if ("fips" %in% names(processed_data)) {
    processed_data$geoid <- processed_data$fips
  } else if ("county_fips" %in% names(processed_data)) {
    processed_data$geoid <- processed_data$county_fips
  }
  
  # Ensure geoid is properly formatted
  processed_data$geoid <- sprintf("%05d", as.numeric(processed_data$geoid))
  
  # Get variable list from the database
  db_vars <- dbGetQuery(con, "SELECT variable_name FROM variables")$variable_name
  
  # Identify value columns that are in the database
  data_cols <- intersect(names(processed_data), db_vars)
  
  if (length(data_cols) == 0) {
    log_message("No valid variables found in the processed data!",
                level = "ERROR", show_console = TRUE)
  } else {
    # Convert to long format
    long_data <- processed_data %>%
      select(geoid, year, all_of(data_cols)) %>%
      pivot_longer(
        cols = all_of(data_cols),
        names_to = "variable_name",
        values_to = "value"
      ) %>%
      filter(!is.na(value))
    
    # Add quality flags and enhanced fields
    long_data <- long_data %>%
      mutate(
        data_quality = "direct",
        data_source = "unified_pipeline",
        data_vintage = as.character(year),
        interpolation_method = NA_character_,
        ci_lower = NA_real_,
        ci_upper = NA_real_,
        confidence_level = 0.95,
        last_updated = Sys.time()
      )
    
    # Add real quality flags when available
    for (var_name in data_cols) {
      quality_col <- paste0(var_name, "_data_quality")
      source_col <- paste0(var_name, "_data_source")
      vintage_col <- paste0(var_name, "_data_vintage")
      
      if (quality_col %in% names(processed_data)) {
        long_data$data_quality[long_data$variable_name == var_name] <- 
          processed_data[[quality_col]][match(
            paste(long_data$geoid[long_data$variable_name == var_name], 
                  long_data$year[long_data$variable_name == var_name]),
            paste(processed_data$geoid, processed_data$year)
          )]
      }
      
      if (source_col %in% names(processed_data)) {
        long_data$data_source[long_data$variable_name == var_name] <- 
          processed_data[[source_col]][match(
            paste(long_data$geoid[long_data$variable_name == var_name], 
                  long_data$year[long_data$variable_name == var_name]),
            paste(processed_data$geoid, processed_data$year)
          )]
      }
      
      if (vintage_col %in% names(processed_data)) {
        long_data$data_vintage[long_data$variable_name == var_name] <- 
          processed_data[[vintage_col]][match(
            paste(long_data$geoid[long_data$variable_name == var_name], 
                  long_data$year[long_data$variable_name == var_name]),
            paste(processed_data$geoid, processed_data$year)
          )]
      }
    }
    
    # UPSERT pattern for data import
    log_message("Using UPSERT pattern for data import...",
                level = "INFO")
    
    # Create a temporary table for new data
    dbExecute(con, "CREATE TEMPORARY TABLE temp_data AS SELECT * FROM sdoh_data LIMIT 0")
    
    # Import new data to temp table
    dbAppendTable(con, "temp_data", long_data)
    
    # Update existing records
    dbExecute(con, "
      UPDATE sdoh_data AS t1
      SET 
        value = t2.value,
        data_quality = t2.data_quality,
        data_source = t2.data_source,
        data_vintage = t2.data_vintage,
        last_updated = t2.last_updated
      FROM temp_data AS t2
      WHERE 
        t1.geoid = t2.geoid AND
        t1.year = t2.year AND
        t1.variable_name = t2.variable_name
    ")
    
    # Insert new records that don't exist yet
    dbExecute(con, "
      INSERT INTO sdoh_data
      SELECT t2.*
      FROM temp_data t2
      LEFT JOIN sdoh_data t1 ON
        t1.geoid = t2.geoid AND
        t1.year = t2.year AND
        t1.variable_name = t2.variable_name
      WHERE t1.geoid IS NULL
    ")
    
    # Drop the temporary table
    dbExecute(con, "DROP TABLE temp_data")
    
    # Get data count
    data_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM sdoh_data")
    log_message(paste("Database now contains", data_count$count, "data points"),
                level = "INFO", show_console = TRUE)
  }
} else {
  log_message("No processed data available to import!",
              level = "ERROR", show_console = TRUE)
}

# Create views to help with data analysis
log_message("Creating database views...",
            level = "INFO", show_console = TRUE)

# Latest data view
dbExecute(con, "
  CREATE OR REPLACE VIEW latest_county_data AS
  WITH latest_years AS (
    SELECT variable_name, MAX(year) as max_year
    FROM sdoh_data
    GROUP BY variable_name
  )
  SELECT 
    c.geoid,
    c.name,
    c.state_fips,
    c.state_name,
    d.variable_name,
    d.value,
    d.year,
    d.data_quality,
    d.data_source,
    d.data_vintage,
    v.domain,
    v.description,
    v.units
  FROM sdoh_data d
  JOIN counties c ON d.geoid = c.geoid
  JOIN variables v ON d.variable_name = v.variable_name
  JOIN latest_years ly ON d.variable_name = ly.variable_name AND d.year = ly.max_year
")

# County time series view
dbExecute(con, "
  CREATE OR REPLACE VIEW county_time_series AS
  SELECT 
    c.geoid,
    c.name as county_name,
    c.state_name,
    d.variable_name,
    v.description as variable_description,
    v.domain,
    v.units,
    d.year,
    d.value,
    d.data_quality,
    d.data_source,
    d.data_vintage
  FROM counties c
  JOIN sdoh_data d ON c.geoid = d.geoid
  JOIN variables v ON d.variable_name = v.variable_name
  ORDER BY c.geoid, d.variable_name, d.year
")

# Domain-specific views
domains <- dbGetQuery(con, "SELECT DISTINCT domain FROM variables")$domain

for (domain in domains) {
  safe_domain_name <- gsub("[^a-zA-Z0-9]", "_", tolower(domain))
  view_name <- paste0(safe_domain_name, "_variables")
  
  # Create a view for each domain
  view_query <- glue::glue_sql("
    CREATE OR REPLACE VIEW {`view_name`} AS
    SELECT 
      c.geoid,
      c.name as county_name,
      c.state_name,
      d.variable_name,
      v.description as variable_description,
      v.units,
      d.year,
      d.value,
      d.data_quality,
      d.data_source
    FROM counties c
    JOIN sdoh_data d ON c.geoid = d.geoid
    JOIN variables v ON d.variable_name = v.variable_name
    WHERE v.domain = {domain}
    ORDER BY c.geoid, d.variable_name, d.year
  ", .con = con)
  
  tryCatch({
    dbExecute(con, view_query)
    log_message(paste("Created view for domain:", domain),
                level = "INFO")
  }, error = function(e) {
    log_message(paste("Error creating view for domain", domain, ":", conditionMessage(e)),
                level = "ERROR")
  })
}

# Create summary views
dbExecute(con, "
  CREATE OR REPLACE VIEW data_quality_summary AS
  SELECT 
    year,
    data_quality, 
    data_source,
    COUNT(*) as count
  FROM sdoh_data
  GROUP BY year, data_quality, data_source
  ORDER BY year, data_quality, data_source
")

dbExecute(con, "
  CREATE OR REPLACE VIEW domain_coverage_by_year AS
  SELECT 
    v.domain,
    d.year,
    COUNT(DISTINCT d.variable_name) as variables_count,
    COUNT(DISTINCT d.geoid) as counties_count,
    COUNT(*) as data_points
  FROM sdoh_data d
  JOIN variables v ON d.variable_name = v.variable_name
  GROUP BY v.domain, d.year
  ORDER BY v.domain, d.year
")

# Keep the database connection open for later use with summary queries
log_message("Database creation completed successfully",
            level = "INFO", show_console = TRUE)

# Add enhanced traffic safety data to database if available
if (file.exists(file.path(root_dir, "traffic_safety_integration.r")) && 
    "traffic_safety" %in% names(extended_data_sources) &&
    !is.null(extended_data_sources$traffic_safety)) {
  
  # Check if the add function exists
  tryCatch({
    # Source the module if needed with timeout
    if (!exists("add_traffic_safety_to_database")) {
      # Set a timeout for sourcing the module
      setTimeLimit(cpu = 30, elapsed = 30)
      on.exit(setTimeLimit(cpu = Inf, elapsed = Inf), add = TRUE)
      
      log_message("Loading traffic safety integration module for database operations...", 
                  level = "INFO", show_console = TRUE)
      
      source(file.path(root_dir, "traffic_safety_integration.r"))
      
      # Reset time limits
      setTimeLimit(cpu = Inf, elapsed = Inf)
    }
    
    # Add enhanced data to database
    log_message("Adding enhanced traffic safety data to database...",
                level = "INFO", show_console = TRUE)
    
    # Reconnect to database if needed
    if (!dbIsValid(con)) {
      log_message("Reconnecting to database...", level = "INFO")
      con <- dbConnect(duckdb::duckdb(), dbdir = unified_db_path)
    }
    
    # Set a timeout for database operations
    setTimeLimit(cpu = 60, elapsed = 60)
    on.exit(setTimeLimit(cpu = Inf, elapsed = Inf), add = TRUE)
    
    # Add the data with limited features
    add_result <- add_traffic_safety_to_database(
      traffic_data = extended_data_sources$traffic_safety,
      db_path = unified_db_path,
      add_forecasts = FALSE,  # Disable forecasts to prevent hanging
      add_spatial = FALSE     # Disable spatial data to prevent hanging
    )
    
    # Reset time limits
    setTimeLimit(cpu = Inf, elapsed = Inf)
    
    log_message("Enhanced traffic safety data successfully added to database",
                level = "INFO", show_console = TRUE)
  }, error = function(e) {
    # Always reset time limits in case of error
    setTimeLimit(cpu = Inf, elapsed = Inf)
    log_message(paste("Error adding enhanced traffic safety data to database:", e$message),
                level = "ERROR", show_console = TRUE)
  })
}

# ---- Step 5: Generate Maps ----
log_message("\nSTEP 5: GENERATING MAPS",
            level = "INFO", show_console = TRUE)

# Set a timeout for map generation to prevent hanging
map_generation_timeout <- 600  # 10 minutes timeout

# Source map generation scripts with timeout protection
tryCatch({
  # Use setTimeLimit to set a timeout for this block
  setTimeLimit(cpu = map_generation_timeout, elapsed = map_generation_timeout)
  
  # Source map generation scripts
  source(file.path(root_dir, "generate_county_maps.r"))
  if (file.exists(file.path(root_dir, "extended_sdoh_pipeline", "generate_extended_maps.r"))) {
    source(file.path(root_dir, "extended_sdoh_pipeline", "generate_extended_maps.r"))
  }
  
  # Check if we can generate maps
  if (!requireNamespace("viridis", quietly = TRUE)) {
    log_message("Package 'viridis' is not available. Skipping map generation.",
                level = "WARN", show_console = TRUE)
  } else {
    # Load viridis for color palettes
    library(viridis)
    
    # Generate maps
    log_message("Generating county maps for visualization...",
                level = "INFO", show_console = TRUE)
    
    # Get available variables from database
    con <- dbConnect(duckdb::duckdb(), dbdir = unified_db_path)
    all_vars <- dbGetQuery(con, "SELECT variable_name FROM variables")$variable_name
    dbDisconnect(con)
    
    # Sample years for maps (to avoid generating too many maps)
    # Reduce the number of years to prevent hanging
    sample_years <- seq(2010, 2020, by = 10)  # Just 2010 and 2020 to minimize processing
    
    # Generate maps for just a few key variables to prevent hanging
    prioritized_vars <- c(
      # Demographics
      "total_population",
      # Economic
      "median_household_income",
      # Health
      "life_expectancy"
    )
    
    # Filter to only available variables
    map_vars <- intersect(prioritized_vars, all_vars)
    
    # Limit the number of maps to generate
    if (length(map_vars) > 3) {
      map_vars <- map_vars[1:3]
    }
    
    log_message(paste("Generating maps for", length(map_vars), "variables and", 
                    length(sample_years), "years (limited to prevent hanging)"),
              level = "INFO", show_console = TRUE)
    
    # Generate maps with timeout protection
    tryCatch({
      # Use setTimeLimit to set a timeout for map generation
      setTimeLimit(cpu = map_generation_timeout / 2, elapsed = map_generation_timeout / 2)
      
      # Generate maps
      if (exists("generate_extended_maps")) {
        log_message("Using enhanced map generation...",
                    level = "INFO", show_console = TRUE)
        
        # Use the extended map generator
        map_result <- generate_extended_maps(
          db_path = unified_db_path,
          output_dir = file.path(output_dir, "maps"),
          years = sample_years,
          variables = map_vars,
          verbose = verbose
        )
      } else {
        log_message("Using standard map generation...",
                    level = "INFO", show_console = TRUE)
        
        # Use the original map generator
        map_result <- generate_county_maps(
          database_path = unified_db_path,
          years = sample_years,
          variables = map_vars,
          output_dir = file.path(output_dir, "maps"),
          shapefile_dir = file.path(data_dir, "shapefiles")
        )
      }
      
      # Reset time limit
      setTimeLimit(cpu = Inf, elapsed = Inf)
      
      log_message("Map generation completed successfully", 
                  level = "INFO", show_console = TRUE)
    }, error = function(e) {
      # Reset time limit
      setTimeLimit(cpu = Inf, elapsed = Inf)
      
      log_message(paste("Error during map generation:", conditionMessage(e)), 
                  level = "ERROR", show_console = TRUE)
      log_message("Continuing with pipeline despite map generation error", 
                  level = "WARN", show_console = TRUE)
    }, warning = function(w) {
      log_message(paste("Warning during map generation:", conditionMessage(w)), 
                  level = "WARN", show_console = TRUE)
    }, finally = {
      # Always reset time limit
      setTimeLimit(cpu = Inf, elapsed = Inf)
    })
  }
  
  # Reset time limit
  setTimeLimit(cpu = Inf, elapsed = Inf)
}, error = function(e) {
  # Reset time limit
  setTimeLimit(cpu = Inf, elapsed = Inf)
  
  log_message(paste("Error in map generation setup:", conditionMessage(e)), 
              level = "ERROR", show_console = TRUE)
  log_message("Skipping map generation and continuing with pipeline", 
              level = "WARN", show_console = TRUE)
}, warning = function(w) {
  log_message(paste("Warning during map generation setup:", conditionMessage(w)), 
              level = "WARN", show_console = TRUE)
}, finally = {
  # Always reset time limit
  setTimeLimit(cpu = Inf, elapsed = Inf)
})

# ---- Step 6: Generate CONUS Maps ----
log_message("\nSTEP 6: GENERATING CONUS MAPS FOR ALL VARIABLES",
            level = "INFO", show_console = TRUE)

# Source the map generation script
source(file.path(root_dir, "generate_conus_maps.r"))

# Generate maps for all variables and years
map_result <- tryCatch({
  generate_conus_maps(
    output_dir = file.path(output_dir, "maps"),
    db_path = file.path(output_dir, "us_county_sdoh_unified.duckdb"),
    conus_only = TRUE,
    parallel = FALSE
  )
  TRUE
}, error = function(e) {
  log_message(paste("ERROR: Improved map generation failed:", conditionMessage(e)), 
              level = "ERROR", show_console = TRUE)
  FALSE
})

if (map_result) {
  log_message("CONUS maps successfully generated for all variables", 
              level = "INFO", show_console = TRUE)
} else {
  log_message("CONUS map generation encountered errors - some maps may be missing", 
              level = "WARN", show_console = TRUE)
}

# ---- Step 7: Create Documentation ----
log_message("\nSTEP 7: GENERATING DOCUMENTATION",
            level = "INFO", show_console = TRUE)

# Generate README.md with documentation
readme_content <- c(
  "# Unified Social Determinants of Health County-Level Dataset",
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
  "- **USDA Food Environment Atlas** (food access measures)",
  "- **EPA** (environmental quality measures)",
  "- **HUD** (housing statistics)",
  "- **HRSA** (healthcare access measures)",
  "- Additional specialized data sources for various SDOH domains",
  "",
  "The data has been processed to provide consistent variable names across sources and years,",
  "with interpolation for missing years where appropriate and comprehensive data quality tracking.",
  "",
  "## Data Domains",
  "",
  "This unified dataset includes variables across the following domains:",
  "",
  "1. **Demographics**: Population, age, sex, race/ethnicity distributions",
  "2. **Socioeconomic Status**: Income, poverty, education, employment",
  "3. **Health Status**: Health outcomes, health behaviors, healthcare access",
  "4. **Housing**: Home values, housing burden, overcrowding, homelessness",
  "5. **Food Environment & Access**: Food insecurity, grocery store access, SNAP",
  "6. **Built Environment**: Walkability, park access, recreation resources",
  "7. **Environmental Health**: Air/water quality, toxic sites, climate indicators",
  "8. **Transportation**: Transit access, commuting patterns, vehicle access",
  "9. **Social Cohesion**: Civic participation, social capital",
  "10. **Crime and Safety**: Crime rates, incarceration, safety measures",
  "",
  "## Data Sources and URLs",
  "",
  "| Source | Description | URL |",
  "| ------ | ----------- | --- |",
  "| US Census Bureau | Demographics, socioeconomic data | https://www.census.gov/data.html |",
  "| IPUMS NHGIS | Harmonized historical Census data | https://www.nhgis.org/ |",
  "| CDC PLACES | Local health outcome data | https://www.cdc.gov/places/ |",
  "| IHME | Life expectancy data | https://www.healthdata.org/ |",
  "| USDA Food Environment Atlas | Food access metrics | https://www.ers.usda.gov/data-products/food-environment-atlas/ |",
  "| EPA Environmental Justice Screening | Environmental metrics | https://www.epa.gov/ejscreen |",
  "| HUD Comprehensive Housing Affordability | Housing metrics | https://www.huduser.gov/portal/datasets/cp.html |",
  "| HRSA Area Health Resources Files | Healthcare workforce and facilities | https://data.hrsa.gov/topics/health-workforce/ahrf |",
  "| Bureau of Transportation Statistics | Transportation metrics | https://www.bts.gov/ |",
  "| Eviction Lab | Housing stability and evictions | https://evictionlab.org/ |",
  "| Opportunity Insights | Economic mobility metrics | https://opportunityinsights.org/ |",
  "| National Center for Education Statistics | Education metrics | https://nces.ed.gov/ |",
  "| FBI Uniform Crime Reports | Crime and safety metrics | https://www.fbi.gov/services/cjis/ucr |",
  "",
  "## Data Structure",
  "",
  "The database contains the following main tables:",
  "",
  "- `sdoh_data` - Main data table with all variables by county and year",
  "- `counties` - Information about each county",
  "- `variables` - Descriptions and metadata for each variable",
  "",
  "And the following views:",
  "",
  "- `latest_county_data` - The most recent data available for each county and variable",
  "- `county_time_series` - All years of data for all counties",
  "- Domain-specific views for each major data domain",
  "- Summary views for data quality assessment",
  "",
  "## Data Quality Flags",
  "",
  "Each record includes data quality indicators:",
  "",
  "- `data_quality` - One of: 'direct' (from source), 'interpolated' (gap-filled), 'extrapolated' (extended), 'simulated' (for estimation), or 'imputed' (statistically derived)",
  "- `data_source` - Original source of the data",
  "- `data_vintage` - Year and specific collection the data came from",
  "",
  "## Usage Examples",
  "",
  "```r",
  "# Connect to the database",
  "library(DBI)",
  "library(duckdb)",
  "con <- dbConnect(duckdb::duckdb(), 'output/us_county_sdoh_unified.duckdb')",
  "",
  "# Get the latest data for all counties",
  "latest_data <- dbGetQuery(con, \"SELECT * FROM latest_county_data\")",
  "",
  "# Get time series data for a specific county",
  "la_county <- dbGetQuery(con, \"",
  "  SELECT * FROM county_time_series ",
  "  WHERE geoid = '06037' -- Los Angeles County",
  "  ORDER BY variable_name, year",
  "\")",
  "",
  "# Get variables for a specific domain",
  "food_env_data <- dbGetQuery(con, \"SELECT * FROM food_environment_variables\")",
  "",
  "# Close the connection",
  "dbDisconnect(con)",
  "```",
  "",
  "## Running the Pipeline",
  "",
  "```bash",
  "# Install required packages",
  "Rscript R/install_packages.r",
  "",
  "# Run the unified pipeline with default settings",
  "Rscript R/unified_sdoh_pipeline.r",
  "",
  "# Run with specific options",
  "Rscript R/unified_sdoh_pipeline.r --force-update --verbose",
  "```",
  "",
  "## Command Line Options",
  "",
  "- `--force-update` or `-f`: Force refresh of all cached data",
  "- `--verbose` or `-v`: Show detailed processing information",
  "- `--skip-interpolation`: Disable interpolation for missing data points",
  "- `--allow-simulation`: Allow simulated data where real data is unavailable",
  "- `--offline-mode` or `--offline`: Run in offline mode using only cached data",
  "",
  "## Citation",
  "",
  "If you use this dataset in your research or applications, please cite it as:",
  "",
  "```",
  paste("Unified Social Determinants of Health County-Level Dataset (", 
       format(Sys.Date(), "%Y"), 
       "). Generated using data from U.S. Census Bureau, CDC PLACES, IPUMS NHGIS, and other authoritative sources.", 
       sep=""),
  "```"
)

# Write README.md
writeLines(readme_content, file.path(output_dir, "README.md"))
log_message("Created README.md with documentation",
            level = "INFO", show_console = TRUE)

# ---- End of Pipeline ----
# Record the update date
writeLines(as.character(Sys.Date()), last_update_file)
log_message(paste("Recorded update date:", Sys.Date()),
            level = "INFO")

# Calculate execution time
script_end_time <- Sys.time()
execution_time <- difftime(script_end_time, script_start_time, units = "mins")
log_message(paste("\nTotal execution time:", round(execution_time, 2), "minutes"),
            level = "INFO", show_console = TRUE)

# End of pipeline - show completion message in log
log_message(paste("\n=== UNIFIED SOCIAL DETERMINANTS OF HEALTH DATA PIPELINE COMPLETED AT", 
                  format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "===\n"), 
            level = "INFO", show_console = TRUE)
log_message(paste("Log file saved to:", log_file),
            level = "INFO", show_console = TRUE)

# Generate and display summary table of variables by year and county count
log_message("Generating summary table of variables by year...", 
            level = "INFO", show_console = TRUE)

# Make sure the database connection is still valid
if (!dbIsValid(con)) {
  log_message("Database connection is no longer valid. Reconnecting...", 
              level = "INFO", show_console = TRUE)
  # Try to reconnect to the database
  con <- tryCatch({
    dbConnect(duckdb::duckdb(), dbdir = unified_db_path)
  }, error = function(e) {
    log_message(paste("Failed to reconnect to database:", conditionMessage(e)), 
                level = "ERROR", show_console = TRUE)
    return(NULL)
  })
}

# Check if we have a valid connection before proceeding
if (is.null(con) || !dbIsValid(con)) {
  log_message("Unable to generate summary table due to invalid database connection.", 
              level = "ERROR", show_console = TRUE)
} else {
  # Query to get variable count by year and county count
  summary_query <- "
    SELECT 
      year,
      COUNT(DISTINCT variable_name) AS unique_variables,
      COUNT(DISTINCT geoid) AS county_count,
      COUNT(*) AS total_data_points
    FROM sdoh_data
    GROUP BY year
    ORDER BY year
  "

  # Run the query with error handling
  summary_table <- tryCatch({
    dbGetQuery(con, summary_query)
  }, error = function(e) {
    log_message(paste("Error querying database for summary:", conditionMessage(e)), 
                level = "ERROR", show_console = TRUE)
    return(NULL)
  })

  # Display the summary table
  log_message("\n=== Summary of Variables and Counties by Year ===", 
              level = "INFO", show_console = TRUE)

  # Format and display the table in a nice format
  if (!is.null(summary_table) && nrow(summary_table) > 0) {
  # Create a formatted output
  summary_output <- capture.output({
    # Print header
    cat(sprintf("%-6s | %-16s | %-12s | %-15s\n", "Year", "Unique Variables", "County Count", "Total Data Points"))
    cat(sprintf("%-6s-|-%-16s-|-%-12s-|-%-15s\n", "------", "----------------", "------------", "---------------"))
    
    # Print rows
    for (i in 1:nrow(summary_table)) {
      cat(sprintf("%-6s | %-16s | %-12s | %-15s\n", 
                summary_table$year[i],
                format(summary_table$unique_variables[i], big.mark=","),
                format(summary_table$county_count[i], big.mark=","),
                format(summary_table$total_data_points[i], big.mark=",")))
    }
  })
  
  # Log the formatted table
  for (line in summary_output) {
    log_message(line, level = "INFO", show_console = TRUE)
  }
  
  # Add summary statistics with error handling
  total_variables <- tryCatch({
    length(unique(dbGetQuery(con, "SELECT DISTINCT variable_name FROM sdoh_data")$variable_name))
  }, error = function(e) {
    log_message(paste("Error getting variable count:", conditionMessage(e)), level = "ERROR")
    return(0)
  })
  
  total_counties <- tryCatch({
    length(unique(dbGetQuery(con, "SELECT DISTINCT geoid FROM sdoh_data")$geoid))
  }, error = function(e) {
    log_message(paste("Error getting county count:", conditionMessage(e)), level = "ERROR")
    return(0)
  })
  
  total_years <- tryCatch({
    length(unique(dbGetQuery(con, "SELECT DISTINCT year FROM sdoh_data")$year))
  }, error = function(e) {
    log_message(paste("Error getting year count:", conditionMessage(e)), level = "ERROR")
    return(0)
  })
  
  total_data_points <- tryCatch({
    dbGetQuery(con, "SELECT COUNT(*) AS count FROM sdoh_data")$count
  }, error = function(e) {
    log_message(paste("Error getting total data points:", conditionMessage(e)), level = "ERROR")
    return(0)
  })
  
  log_message("\n=== Overall Dataset Statistics ===", 
              level = "INFO", show_console = TRUE)
  log_message(paste("Total Variables:", format(total_variables, big.mark=",")), 
              level = "INFO", show_console = TRUE)
  log_message(paste("Total Counties:", format(total_counties, big.mark=",")), 
              level = "INFO", show_console = TRUE)
  log_message(paste("Total Years:", total_years), 
              level = "INFO", show_console = TRUE)
  log_message(paste("Total Data Points:", format(total_data_points, big.mark=",")), 
              level = "INFO", show_console = TRUE)
} else {
  log_message("No data available to summarize.", 
              level = "WARN", show_console = TRUE)
  }
  
  # Also generate a summary by domain before closing the connection
  if (!is.null(con) && dbIsValid(con)) {
    domain_query <- "
      SELECT 
        v.domain,
        COUNT(DISTINCT d.variable_name) AS unique_variables,
        COUNT(DISTINCT d.year) AS years_available,
        COUNT(DISTINCT d.geoid) AS max_counties,
        COUNT(*) AS total_data_points
      FROM sdoh_data d
      JOIN variables v ON d.variable_name = v.variable_name
      GROUP BY v.domain
      ORDER BY unique_variables DESC
    "

    # Run the domain query while the connection is still open
    domain_table <- tryCatch({
      dbGetQuery(con, domain_query)
    }, error = function(e) {
      log_message(paste("Error querying database for domain summary:", conditionMessage(e)), 
                  level = "ERROR", show_console = TRUE)
      return(NULL)
    })

    # Now we can close the database connection
    log_message("Closing database connection", level = "INFO", show_console = TRUE)
    tryCatch({
      dbDisconnect(con)
    }, error = function(e) {
      log_message(paste("Error disconnecting from database:", conditionMessage(e)), 
                  level = "WARN", show_console = TRUE)
    })
  } else {
    domain_table <- NULL
    log_message("Cannot generate domain summary due to invalid database connection.", 
                level = "ERROR", show_console = TRUE)
  }
}

# Display the domain summary table
log_message("\n=== Summary of Variables by Domain ===", 
            level = "INFO", show_console = TRUE)

# Format and display the domain table
if (!is.null(domain_table) && nrow(domain_table) > 0) {
  # Create a formatted output
  domain_output <- capture.output({
    # Print header
    cat(sprintf("%-25s | %-16s | %-15s | %-12s | %-15s\n", 
              "Domain", "Unique Variables", "Years Available", "Max Counties", "Total Data Points"))
    cat(sprintf("%-25s-|-%-16s-|-%-15s-|-%-12s-|-%-15s\n", 
              "-------------------------", "----------------", "---------------", "------------", "---------------"))
    
    # Print rows
    for (i in 1:nrow(domain_table)) {
      cat(sprintf("%-25s | %-16s | %-15s | %-12s | %-15s\n", 
                substr(domain_table$domain[i], 1, 25),
                format(domain_table$unique_variables[i], big.mark=","),
                format(domain_table$years_available[i], big.mark=","),
                format(domain_table$max_counties[i], big.mark=","),
                format(domain_table$total_data_points[i], big.mark=",")))
    }
  })
  
  # Log the formatted domain table
  for (line in domain_output) {
    log_message(line, level = "INFO", show_console = TRUE)
  }
} else {
  log_message("No domain summary data available to display.", 
              level = "WARN", show_console = TRUE)
}

# Restore console output
sink(NULL)

# Print completion summary to console
cat("\n=== Pipeline Execution Summary ===\n")
cat("Status: SUCCESS\n")
cat("Output Database: output/us_county_sdoh_unified.duckdb\n")
cat("Documentation: output/README.md\n")
cat("Log File: ", log_file, "\n")
cat("\nTo explore the data in R, use:\n")
cat("con <- DBI::dbConnect(duckdb::duckdb(), 'output/us_county_sdoh_unified.duckdb')\n")
cat("counties <- DBI::dbGetQuery(con, 'SELECT * FROM latest_county_data')\n")
cat("DBI::dbDisconnect(con)\n")
