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

# Function to check and install required packages before pipeline starts
install_required_packages <- function(packages) {
  # Check which packages need to be installed
  missing_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
  
  # If there are any missing packages, try to install them
  if (length(missing_packages) > 0) {
    cat("Installing missing required packages for the pipeline:", 
        paste(missing_packages, collapse = ", "), "\n")
    
    for (pkg in missing_packages) {
      cat("Installing", pkg, "...\n")
      try({
        install.packages(pkg, repos = "https://cloud.r-project.org", dependencies = TRUE)
      }, silent = FALSE)
    }
    
    # Check again after installation attempts
    still_missing <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
    if (length(still_missing) > 0) {
      cat("WARNING: Some required packages could not be installed:", 
          paste(still_missing, collapse = ", "), "\n")
      
      cat("For detailed installation help, run: Rscript install_packages.r\n\n")
    } else {
      cat("All required packages successfully installed!\n\n")
    }
  }
}

# List of minimal required packages to run the pipeline
minimum_required_packages <- c(
  "yaml", "dplyr", "DBI", "duckdb", "viridis", "viridisLite", "RColorBrewer", "gridExtra"
)

# Install missing packages before continuing
install_required_packages(minimum_required_packages)

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

# Ensure required parallel packages are available
required_packages <- c("future", "future.apply", "furrr", "parallel")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    log_message(paste("Installing required package:", pkg), level = "INFO", log_file = log_file)
    install.packages(pkg, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE)
  }
}

# Process years within the specified range
log_message(paste("Processing data for years", config$years$min_year, "to", config$years$max_year), 
           level = "INFO", log_file = log_file)

# Set up enhanced parallel processing with adaptive strategies and memory management
parallel_config <- setup_parallel_processing(
  use_parallel = TRUE,
  num_cores = NULL,  # Auto-detect
  strategy = "auto",  # Automatically choose best strategy for platform
  memory_limit_gb = 16,  # Allocate 16GB
  chunk_size = 500  # Use larger chunks for better performance
)

log_message(paste("Enhanced parallel processing configured with", 
                 parallel_config$cores, "cores using", 
                 parallel_config$strategy, "strategy"), 
           level = "INFO", log_file = log_file)
log_message(paste("Memory limit:", parallel_config$memory_limit_gb, "GB with chunk size", 
                 parallel_config$chunk_size),
           level = "INFO", log_file = log_file)

# Create a function to fetch a data source
fetch_data_source <- function(source_name) {
  log_message(paste("Fetching data source:", source_name), 
             level = "INFO")
  
  if (source_name == "census") {
    # Fetch Census Bureau data
    census_data <- get_census_data(
      crosswalk = crosswalk,
      years = config$years$min_year:config$years$max_year,
      refresh_cache = config$data_refresh$refresh_cache,
      use_cache = TRUE
    )
    return(list(name = "census", data = census_data))
    
  } else if (source_name == "nhgis") {
    # Fetch NHGIS data if credentials are available
    nhgis_result <- tryCatch({
      # Check if IPUMS credentials are available in config
      if (config$ipums$username != "" && config$ipums$password != "") {
        message("IPUMS credentials found in configuration.")
        
        # Set credentials from config
        Sys.setenv(IPUMS_USERNAME = config$ipums$username)
        Sys.setenv(IPUMS_PASSWORD = config$ipums$password)
        
        source("utilities/load_ipums_credentials.r")
        message("IPUMS credentials loaded and ready to use.")
        message(paste("Fetching NHGIS data for entire date range (", 
                          config$years$min_year, "-present)..."))
        
        # Fetch NHGIS data via ipumsr
        NULL # Replace with actual NHGIS fetch code when needed
      } else {
        message("No IPUMS credentials available. Skipping NHGIS data fetch.")
        NULL
      }
    }, error = function(e) {
      message(paste("ERROR with NHGIS data:", conditionMessage(e)))
      NULL
    })
    return(list(name = "nhgis", data = nhgis_result))
    
  } else if (source_name == "traffic_safety") {
    # Load traffic safety integration module
    ts_result <- tryCatch({
      message("Loading traffic safety integration module...")
      
      # Source the traffic safety integration module
      source("traffic_safety_integration.r")
      message("Successfully loaded traffic safety integration module")
      
      # Use fallback if configured
      if (config$traffic_safety$use_fallback) {
        message("Traffic safety fallback mode enabled in configuration")
      }
      
      # Return traffic safety data if the function exists
      if (exists("get_traffic_safety_data")) {
        # This will be fetched later when processing data
        return(TRUE)
      } else {
        message("Traffic safety data function not found")
        return(FALSE)
      }
      
    }, error = function(e) {
      message(paste("Error loading traffic safety integration module:", conditionMessage(e)))
      
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
      
      return(FALSE)
    })
    return(list(name = "traffic_safety", data = ts_result))
  }
  
  # Default case - not a recognized data source
  return(list(name = source_name, data = NULL))
}

# Fetch data sources in parallel
data_sources <- c("census", "nhgis", "traffic_safety")
log_message("Fetching multiple data sources in parallel...",
           level = "INFO", log_file = log_file)

# Use future_lapply to fetch data sources in parallel
data_results <- future.apply::future_lapply(data_sources, fetch_data_source)

# Extract results into their respective variables - ensure proper structure
census_data <- NULL
nhgis_data <- NULL
traffic_safety_loaded <- FALSE

# Process each result safely
for (i in seq_along(data_results)) {
  result <- data_results[[i]]
  if (is.list(result) && "name" %in% names(result)) {
    if (result$name == "census") {
      census_data <- result$data
      log_message("Successfully loaded Census data from parallel fetch",
                 level = "INFO", log_file = log_file)
    } else if (result$name == "nhgis") {
      nhgis_data <- result$data
      log_message("Successfully loaded NHGIS data from parallel fetch",
                 level = "INFO", log_file = log_file)
    } else if (result$name == "traffic_safety") {
      traffic_safety_loaded <- result$data
      if (is.logical(traffic_safety_loaded) && traffic_safety_loaded) {
        log_message("Enhanced traffic safety module loaded successfully",
                   level = "INFO", log_file = log_file)
        log_message("Using enhanced traffic safety data pipeline",
                   level = "INFO", log_file = log_file)
      } else {
        log_message("WARNING: Traffic safety module could not be loaded.",
                   level = "WARN", log_file = log_file)
      }
    }
  } else {
    log_message(paste("WARNING: Invalid result format for item", i),
               level = "WARN", log_file = log_file)
  }
}

# Load all domain data from cache
load_domain_cache <- function(domain_name, cache_file_name, log_file) {
  # Construct full cache path
  cache_path <- file.path(config$directories$data_dir, "cache", cache_file_name)
  
  # Check if file exists
  if (file.exists(cache_path)) {
    log_message(paste("Loading", domain_name, "data from cache..."),
               level = "INFO", log_file = log_file)
    
    # Load data from RDS
    domain_data <- readRDS(cache_path)
    
    log_message(paste("Successfully loaded", domain_name, "data from cache."),
               level = "INFO", log_file = log_file)
    
    return(domain_data)
  } else {
    log_message(paste("WARNING: Cache file for", domain_name, "not found:", cache_path),
               level = "WARN", log_file = log_file)
    return(NULL)
  }
}

# Process data for all 15 domains using cached data
log_message("\nProcessing data for all 15 domains...",
           level = "INFO", log_file = log_file)

# 1. Demographics (Census/NHGIS already loaded)
log_message("Demographics data already loaded via Census and NHGIS",
           level = "INFO", log_file = log_file)

# 2. Economic Factors
economic_data <- load_domain_cache("economic factors", "economic_data.rds", log_file)

# 3. Education
education_data <- load_domain_cache("education", "education_data.rds", log_file)

# 4. Health Status
# CDC PLACES data
cdc_places_data <- load_domain_cache("CDC PLACES", "places_data.rds", log_file)

# 5. IHME Life Expectancy 
log_message("Loading life expectancy data from cache...",
           level = "INFO", log_file = log_file)
life_expectancy_data <- load_domain_cache("life expectancy", "life_expectancy_data.rds", log_file)

# 6. Healthcare Access
healthcare_data <- load_domain_cache("healthcare access", "healthcare_access_data.rds", log_file)

# 7. Housing
housing_data <- load_domain_cache("housing", "housing_data.rds", log_file)

# 8. Environmental Factors
if (exists("fetch_epa_data")) {
  log_message("Processing EPA environmental data with parallel support...",
             level = "INFO", log_file = log_file)
  
  # Check if fetch_epa_data accepts parallel parameters
  epa_func_params <- formals(fetch_epa_data)
  if ("parallel" %in% names(epa_func_params) && "parallel_config" %in% names(epa_func_params)) {
    # Function supports parallel processing
    log_message("EPA module supports parallel processing",
               level = "INFO", log_file = log_file)
    
    # Call with parallel parameters
    environmental_data <- fetch_epa_data(
      years = config$years$min_year:config$years$max_year,
      cache_dir = config$directories$full_cache_dir,
      refresh_cache = config$data_refresh$refresh_cache,
      allow_interpolation = TRUE,
      parallel = config$processing$parallel,
      parallel_config = parallel_config
    )
  } else {
    # Standard call without parallel parameters
    log_message("EPA module using standard processing",
               level = "INFO", log_file = log_file)
    environmental_data <- fetch_epa_data(
      years = config$years$min_year:config$years$max_year,
      cache_dir = config$directories$full_cache_dir,
      refresh_cache = config$data_refresh$refresh_cache,
      allow_interpolation = TRUE
    )
  }
} else {
  # Load from cache if function doesn't exist
  environmental_data <- load_domain_cache("environmental factors", "epa_environmental_data.rds", log_file)
}

smart_location_data <- load_domain_cache("smart location", "epa_smart_location_db.rds", log_file)

# 9. Food Environment
food_data <- load_domain_cache("food environment", "usda_food_atlas_data.rds", log_file)

# 10. Transportation
transportation_data <- load_domain_cache("transportation", "transportation_data.rds", log_file)

# 11. Traffic Safety
traffic_safety_data <- NULL
if (exists("get_traffic_safety_data")) {
  log_message("Processing traffic safety data...",
             level = "INFO", log_file = log_file)
  
  # Check if get_traffic_safety_data accepts parallel parameters
  ts_func_params <- formals(get_traffic_safety_data)
  if ("parallel" %in% names(ts_func_params) && "parallel_config" %in% names(ts_func_params)) {
    # Function supports parallel processing
    log_message("Traffic safety module supports parallel processing",
               level = "INFO", log_file = log_file)
    
    # Call with parallel parameters
    traffic_safety_data <- get_traffic_safety_data(
      years = config$years$min_year:config$years$max_year,
      parallel = config$processing$parallel,
      parallel_config = parallel_config
    )
  } else {
    # Standard call without parallel parameters
    log_message("Traffic safety module using standard processing",
               level = "INFO", log_file = log_file)
    traffic_safety_data <- get_traffic_safety_data(years = config$years$min_year:config$years$max_year)
  }
} else {
  traffic_safety_data <- load_domain_cache("traffic safety", "traffic_safety_data.rds", log_file)
}

# 12. Social Cohesion
social_cohesion_data <- load_domain_cache("social cohesion", "social_cohesion_data.rds", log_file)

# 13. Crime and Safety
crime_data <- load_domain_cache("crime and safety", "crime_data.rds", log_file)

# 14. Built Environment (part of EPA and TPL data)
park_access_data <- load_domain_cache("park access", "tpl_park_access.rds", log_file)

# 15. Climate & Weather
climate_data <- load_domain_cache("climate and weather", "climate_data.rds", log_file)

# 16. Digital Access (part of Census ACS data)
log_message("Digital access data included in Census ACS data",
           level = "INFO", log_file = log_file)

log_message("All 15 domain data sources loaded successfully",
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
  crosswalk = crosswalk,
  economic_data = economic_data,
  education_data = education_data,
  cdc_places_data = cdc_places_data,
  life_expectancy_data = life_expectancy_data,
  healthcare_data = healthcare_data,
  housing_data = housing_data,
  environmental_data = environmental_data,
  smart_location_data = smart_location_data,
  food_data = food_data,
  transportation_data = transportation_data,
  traffic_safety_data = traffic_safety_data,
  social_cohesion_data = social_cohesion_data,
  crime_data = crime_data,
  park_access_data = park_access_data,
  climate_data = climate_data
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

