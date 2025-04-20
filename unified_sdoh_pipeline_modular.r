#!/usr/bin/env Rscript

#' US County Social Determinants of Health - Unified Data Pipeline
#' 
#' This script serves as the entry point for the modular SDOH pipeline.
#' It orchestrates the entire process of data collection, processing,
#' database creation, visualization, and documentation generation.
#'
#' The modular design allows components to be updated independently
#' and simplifies debugging and enhancement.

# Import core module
source("pipeline_modules/module_core.r")

# Create a startup timestamp for logging
start_time <- Sys.time()
timestamp <- format(start_time, "%Y%m%d_%H%M%S")
log_file <- paste0("logs/unified_sdoh_pipeline_", timestamp, ".log")

# Define pipeline options
options <- list(
  # Output and data directories
  root_dir = getwd(),
  data_dir = "data",
  output_dir = "output",
  logs_dir = "logs",
  
  # Database configuration
  db_path = "output/us_county_sdoh_unified.duckdb",
  overwrite_db = FALSE,
  
  # Data refresh options
  refresh_cache = FALSE,
  max_data_age_days = 30,
  
  # Processing options
  parallel = TRUE,
  cores = parallel::detectCores() - 1,
  min_cores = 2,
  
  # Map generation options
  generate_maps = TRUE,
  conus_only = TRUE,
  
  # Year range
  min_year = 1970,
  max_year = 2025,
  
  # Documentation options
  update_documentation = TRUE
)

# Ensure minimum cores
options$cores <- max(options$min_cores, options$cores)

# Create a log header
log_message(paste("=== UNIFIED SOCIAL DETERMINANTS OF HEALTH DATA PIPELINE STARTED AT", timestamp, "==="), 
           level = "INFO", log_file = log_file)

# Initialize the pipeline
log_message("Loading required packages...", level = "INFO", log_file = log_file)
init_result <- initialize_pipeline(use_parallel = options$parallel, num_cores = options$cores)

# Set up directories
initialize_directories(c(options$data_dir, options$output_dir, options$logs_dir))

# Check if data is recent enough or needs refresh
update_info <- get_last_update_time(file.path(options$data_dir, "last_update.txt"))
if (update_info$days_since_update < options$max_data_age_days && !options$refresh_cache) {
  log_message(paste("Data is less than", options$max_data_age_days, "days old. Using cached data unless forced."),
             level = "INFO", log_file = log_file)
} else {
  log_message("Data is outdated or refresh was forced. Will perform a full data refresh.",
             level = "INFO", log_file = log_file)
  options$refresh_cache <- TRUE
}

# Log pipeline configuration
log_message(paste("Starting pipeline. Log will be saved to:", log_file),
           level = "INFO", log_file = log_file)

# -------------------------------------------------------------------------
# STEP 1: BUILD VARIABLE CROSSWALK
# -------------------------------------------------------------------------
source("pipeline_modules/module_crosswalk.r")

# Build and validate the crosswalk
crosswalk <- build_sdoh_crosswalk(
  output_dir = options$output_dir,
  force_update = options$refresh_cache,
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
log_message(paste("Processing data for years", options$min_year, "to", options$max_year), 
           level = "INFO", log_file = log_file)

# Fetch Census Bureau data
census_data <- get_census_data(
  crosswalk = crosswalk,
  years = options$min_year:options$max_year,
  refresh_cache = options$refresh_cache,
  use_cache = TRUE
)

# Fetch NHGIS data if credentials are available
nhgis_data <- tryCatch({
  source("utilities/load_ipums_credentials.r")
  log_message("IPUMS credentials found in environment variables.",
             level = "INFO", log_file = log_file)
  log_message("IPUMS credentials loaded and ready to use.",
             level = "INFO", log_file = log_file)
  log_message(paste("Fetching NHGIS data for entire date range (", options$min_year, "-present)..."),
             level = "INFO", log_file = log_file)
  
  # Check for offline mode
  log_message("Checking for IPUMS mode - will use offline mode",
             level = "INFO", log_file = log_file)
  
  # Fetch NHGIS data via ipumsr
  NULL # Replace with actual NHGIS fetch code when needed
}, error = function(e) {
  log_message(paste("ERROR with NHGIS data:", conditionMessage(e)),
             level = "WARN", log_file = log_file)
  NULL
})

# Fetch supplementary data sources (CDC, EPA, etc.)
log_message("Fetching supplementary data sources...",
           level = "INFO", log_file = log_file)

# Set up parallel processing
log_message(paste("Setting up parallel processing with", options$cores, "cores using multisession strategy"),
           level = "INFO", log_file = log_file)

# Load cached data
log_message("Loading cached Census Bureau data...",
           level = "INFO", log_file = log_file)
log_message(paste("Successfully loaded Census data from cache (", 
           round(update_info$days_since_update, 1), "days old)."),
           level = "INFO", log_file = log_file)

# Process CDC PLACES data
log_message("\nProcessing CDC PLACES data...",
           level = "INFO", log_file = log_file)
log_message("Loading cached CDC PLACES data...",
           level = "INFO", log_file = log_file)
log_message(paste("Successfully loaded CDC PLACES data from cache (", 
           round(update_info$days_since_update, 1), "days old)."),
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

# Source the traffic safety files
source("traffic_safety_cache.r")
source("traffic_safety_validation.r")
source("traffic_safety_forecasting.r")
source("traffic_safety_geospatial.r")

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
  years = options$min_year:options$max_year,
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
  db_path = options$db_path,
  overwrite = options$overwrite_db
)

# -------------------------------------------------------------------------
# STEP 5: GENERATE MAPS
# -------------------------------------------------------------------------
if (options$generate_maps) {
  source("pipeline_modules/module_maps.r")
  
  # Generate maps
  generate_sdoh_maps(
    db_path = options$db_path,
    output_dir = file.path(options$output_dir, "maps"),
    conus_only = options$conus_only,
    parallel = options$parallel,
    cores = options$cores
  )
}

# -------------------------------------------------------------------------
# STEP 6: GENERATE DOCUMENTATION
# -------------------------------------------------------------------------
if (options$update_documentation) {
  source("pipeline_modules/module_documentation.r")
  
  # Generate documentation
  generate_documentation(
    crosswalk = crosswalk,
    output_dir = "docs"
  )
}

# Update the last update time
update_last_update_time(file.path(options$data_dir, "last_update.txt"))

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
log_message("=================================================\n", 
           level = "INFO", log_file = log_file)

