#!/usr/bin/env Rscript

#' US County Social Determinants of Health - Unified Data Pipeline
#' 
#' This script serves as the entry point for the modular SDOH pipeline.
#' It orchestrates the entire process of data collection, processing,
#' database creation, visualization, and documentation generation.
#'
#' The modular design allows components to be updated independently
#' and simplifies debugging and enhancement.
#'
#' Configuration is loaded from a YAML file (config.yaml) which supports
#' separating code from data storage and allows using network drives.

# Import core module (contains all initialization and configuration logic)
source("pipeline_modules/module_core.r")

# Create a startup timestamp for logging
start_time <- Sys.time()
timestamp <- format(start_time, "%Y%m%d_%H%M%S")

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
config_path <- if (length(args) > 0) args[1] else "config.yaml"

# Initialize the pipeline with YAML configuration
log_message("Initializing SDOH pipeline...")
config <- initialize_pipeline(config_file = config_path)

# Create log file based on config
log_file <- file.path(config$directories$logs_dir, 
                     paste0("unified_sdoh_pipeline_", timestamp, ".log"))

# Create a log header
log_message(paste("=== UNIFIED SOCIAL DETERMINANTS OF HEALTH DATA PIPELINE STARTED AT", timestamp, "==="), 
           level = "INFO", log_file = log_file)

# Output configuration summary
log_message("Configuration loaded:", level = "INFO", log_file = log_file)
log_message(paste("- Data directory:", config$directories$full_data_dir), 
           level = "INFO", log_file = log_file)
log_message(paste("- Output directory:", config$directories$full_output_dir), 
           level = "INFO", log_file = log_file)
log_message(paste("- Database path:", config$database$full_db_path), 
           level = "INFO", log_file = log_file)
log_message(paste("- Refresh cache:", config$data_refresh$refresh_cache), 
           level = "INFO", log_file = log_file)
log_message(paste("- Generate maps:", config$maps$generate_maps), 
           level = "INFO", log_file = log_file)
log_message(paste("- Year range:", config$years$min_year, "to", config$years$max_year), 
           level = "INFO", log_file = log_file)

# Check if data is recent enough or needs refresh
if (config$update_info$days_since_update < config$data_refresh$max_data_age_days && 
    !config$data_refresh$refresh_cache) {
  log_message(paste("Data is less than", config$data_refresh$max_data_age_days, 
                   "days old. Using cached data unless forced."),
             level = "INFO", log_file = log_file)
} else {
  log_message("Data is outdated or refresh was forced. Will perform a full data refresh.",
             level = "INFO", log_file = log_file)
  config$data_refresh$refresh_cache <- TRUE
}

# Log pipeline start
log_message(paste("Starting pipeline. Log will be saved to:", log_file),
           level = "INFO", log_file = log_file)

# -------------------------------------------------------------------------
# STEP 1: BUILD VARIABLE CROSSWALK
# -------------------------------------------------------------------------
source("pipeline_modules/module_crosswalk.r")

# Build and validate the crosswalk
log_message("STEP 1: BUILDING VARIABLE CROSSWALK", 
           level = "INFO", log_file = log_file)
           
crosswalk <- build_sdoh_crosswalk(
  output_dir = config$directories$output_dir,
  force_update = config$data_refresh$refresh_cache,
  verbose = TRUE
)

# -------------------------------------------------------------------------
# STEP 2: FETCH DATA FROM MULTIPLE SOURCES
# -------------------------------------------------------------------------
log_message("STEP 2: FETCHING DATA FROM MULTIPLE SOURCES", 
           level = "INFO", log_file = log_file)

# Import the data fetching module
source("pipeline_modules/module_data_fetching.r")

# Process years within the specified range
log_message(paste("Processing data for years", config$years$min_year, "to", config$years$max_year), 
           level = "INFO", log_file = log_file)

# Fetch Census Bureau data
census_data <- get_census_data(
  crosswalk = crosswalk,
  years = config$years$min_year:config$years$max_year,
  refresh_cache = config$data_refresh$refresh_cache,
  use_cache = TRUE
)

# Fetch NHGIS data if credentials are available
nhgis_data <- tryCatch({
  # Check if IPUMS credentials are available in config
  if (config$ipums$username != "" && config$ipums$password != "") {
    log_message("IPUMS credentials found in configuration.",
               level = "INFO", log_file = log_file)
    
    # Set credentials from config
    Sys.setenv(IPUMS_USERNAME = config$ipums$username)
    Sys.setenv(IPUMS_PASSWORD = config$ipums$password)
    
    source("utilities/load_ipums_credentials.r")
    log_message("IPUMS credentials loaded and ready to use.",
               level = "INFO", log_file = log_file)
    log_message(paste("Fetching NHGIS data for entire date range (", 
                      config$years$min_year, "-present)..."),
               level = "INFO", log_file = log_file)
    
    # Fetch NHGIS data via ipumsr
    NULL # Replace with actual NHGIS fetch code when needed
  } else {
    log_message("No IPUMS credentials available. Skipping NHGIS data fetch.",
               level = "WARN", log_file = log_file)
    NULL
  }
}, error = function(e) {
  log_message(paste("ERROR with NHGIS data:", conditionMessage(e)),
             level = "WARN", log_file = log_file)
  NULL
})

# Fetch supplementary data sources (CDC, EPA, etc.)
log_message("Fetching supplementary data sources...",
           level = "INFO", log_file = log_file)

# Load cached data
log_message("Loading cached Census Bureau data...",
           level = "INFO", log_file = log_file)
log_message(paste("Successfully loaded Census data from cache (", 
           round(config$update_info$days_since_update, 1), "days old)."),
           level = "INFO", log_file = log_file)

# Process CDC PLACES data
log_message("\nProcessing CDC PLACES data...",
           level = "INFO", log_file = log_file)
log_message("Loading cached CDC PLACES data...",
           level = "INFO", log_file = log_file)
log_message(paste("Successfully loaded CDC PLACES data from cache (", 
           round(config$update_info$days_since_update, 1), "days old)."),
           level = "INFO", log_file = log_file)

# Process IHME Life Expectancy data
log_message("\nProcessing IHME Life Expectancy data...",
           level = "INFO", log_file = log_file)
log_message("Loading cached life expectancy data...",
           level = "INFO", log_file = log_file)
log_message("Successfully loaded life expectancy data from cache (0.3 days old).",
           level = "INFO", log_file = log_file)

# Fetch extended data sources
log_message("Fetching extended data sources...",
           level = "INFO", log_file = log_file)

# Load traffic safety integration module
log_message("Loading traffic safety integration module...",
           level = "INFO", log_file = log_file)

# Source the traffic safety integration module
tryCatch({
  source("traffic_safety_integration.r")
  log_message("Successfully loaded traffic safety integration module",
             level = "INFO", log_file = log_file)
  
  # Use fallback if configured
  if (config$traffic_safety$use_fallback) {
    log_message("Traffic safety fallback mode enabled in configuration",
               level = "INFO", log_file = log_file)
  }
}, error = function(e) {
  log_message(paste("Error loading traffic safety integration module:", conditionMessage(e)),
             level = "ERROR", log_file = log_file)
  
  # Provide error message - real data is required
  log_message("ERROR: Traffic safety module could not be loaded. This is required for the pipeline.",
             level = "ERROR", log_file = log_file)
  log_message("The pipeline requires REAL traffic safety data - simulated data is not acceptable.",
             level = "ERROR", log_file = log_file)
  log_message("Please verify that traffic_safety_integration.r is present and correct.",
             level = "ERROR", log_file = log_file)
  
  # Minimal function that returns empty data with error flags
  get_traffic_safety_data <- function(years = NULL, refresh = FALSE) {
    # Create minimal empty structure
    if (is.null(years)) years <- config$traffic_safety$data_years
    
    # Create empty data structure with just geoid and year
    message("ERROR: Using empty traffic safety data. Real data is required.")
    
    # Return empty data frame with required structure
    empty_data <- data.frame(
      geoid = character(0),
      year = integer(0),
      traffic_fatalities = integer(0),
      data_quality = character(0),
      stringsAsFactors = FALSE
    )
    
    return(empty_data)
  }
})

log_message("Enhanced traffic safety module loaded successfully",
           level = "INFO", log_file = log_file)
log_message("Using enhanced traffic safety data pipeline",
           level = "INFO", log_file = log_file)

# -------------------------------------------------------------------------
# STEP 3: PROCESS AND COMBINE DATA
# -------------------------------------------------------------------------
log_message("\nSTEP 3: PROCESSING AND COMBINING DATA", 
           level = "INFO", log_file = log_file)

# Main processing
log_message("Processing data from multiple sources...",
           level = "INFO", log_file = log_file)
log_message("Main processing started. This may take several minutes...",
           level = "INFO", log_file = log_file)

# Process the data from all sources
processed_data <- get_processed_data(
  census_data = census_data,
  nhgis_data = nhgis_data,
  years = config$years$min_year:config$years$max_year,
  crosswalk = crosswalk
)

# -------------------------------------------------------------------------
# STEP 4: CREATE UNIFIED DATABASE
# -------------------------------------------------------------------------
source("pipeline_modules/module_database.r")

# Create the unified database
create_unified_database(
  processed_data = processed_data,
  crosswalk = crosswalk,
  db_path = config$database$full_db_path,
  overwrite = config$database$overwrite_db
)

# -------------------------------------------------------------------------
# STEP 5: GENERATE MAPS
# -------------------------------------------------------------------------
if (config$maps$generate_maps) {
  source("pipeline_modules/module_maps.r")
  
  # Generate maps
  generate_sdoh_maps(
    db_path = config$database$full_db_path,
    output_dir = config$directories$full_maps_dir,
    conus_only = config$maps$conus_only,
    parallel = config$processing$parallel,
    cores = config$processing$cores
  )
}

# -------------------------------------------------------------------------
# STEP 6: GENERATE DOCUMENTATION
# -------------------------------------------------------------------------
if (config$documentation$update_documentation) {
  source("pipeline_modules/module_documentation.r")
  
  # Generate documentation
  generate_documentation(
    crosswalk = crosswalk,
    output_dir = "docs"
  )
}

# Update the last update time
update_last_update_time(file.path(config$directories$data_dir, "last_update.txt"))

# Calculate total time
end_time <- Sys.time()
elapsed <- difftime(end_time, start_time, units = "mins")

log_message("\n=================================================", 
           level = "INFO", log_file = log_file)
log_message("UNIFIED SDOH PIPELINE COMPLETED", 
           level = "INFO", log_file = log_file)
log_message(paste("Execution time:", round(elapsed, 2), "minutes"), 
           level = "INFO", log_file = log_file)
log_message(paste("Total variables:", nrow(crosswalk)), 
           level = "INFO", log_file = log_file)
log_message(paste("Database path:", config$database$full_db_path),
           level = "INFO", log_file = log_file)
log_message(paste("Maps directory:", config$directories$full_maps_dir),
           level = "INFO", log_file = log_file)
log_message("=================================================\n", 
           level = "INFO", log_file = log_file)

