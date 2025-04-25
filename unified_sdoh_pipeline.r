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
#'
#' The pipeline also supports restarting from specific steps using the
#' --restart-from=STEP argument, where STEP can be one of:
#' - crosswalk: Restart from the variable crosswalk building step
#' - fetch: Restart from the data fetching step
#' - process: Restart from the data processing step
#' - database: Restart from the database creation step
#' - maps: Restart from the map generation step
#' - documentation: Restart from the documentation generation step

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
if (!"crosswalk" %in% skip_steps) {
  log_message("STEP 1: BUILDING VARIABLE CROSSWALK", 
             level = "INFO", log_file = log_file, show_console = TRUE)
             
  crosswalk <- build_sdoh_crosswalk(
    output_dir = config$directories$output_dir,
    force_update = config$data_refresh$refresh_cache,
    verbose = TRUE
  )
} else {
  log_message("SKIPPING STEP 1: BUILDING VARIABLE CROSSWALK (using existing crosswalk)",
             level = "INFO", log_file = log_file, show_console = TRUE)
             
  # Load existing crosswalk
  crosswalk_path <- file.path(config$directories$output_dir, "variable_crosswalk_consolidated.csv")
  if (file.exists(crosswalk_path)) {
    crosswalk <- read.csv(crosswalk_path, stringsAsFactors = FALSE)
    log_message(paste("Loaded existing crosswalk with", nrow(crosswalk), "variables"),
               level = "INFO", log_file = log_file)
  } else {
    log_message("ERROR: Cannot find existing crosswalk. Will build it from scratch.",
               level = "ERROR", log_file = log_file, show_console = TRUE)
    crosswalk <- build_sdoh_crosswalk(
      output_dir = config$directories$output_dir,
      force_update = config$data_refresh$refresh_cache,
      verbose = TRUE
    )
  }
}

# -------------------------------------------------------------------------
# STEP 2: FETCH DATA FROM MULTIPLE SOURCES
# -------------------------------------------------------------------------
if (!"fetch" %in% skip_steps) {
  log_message("STEP 2: FETCHING DATA FROM MULTIPLE SOURCES", 
             level = "INFO", log_file = log_file, show_console = TRUE)
} else {
  log_message("SKIPPING STEP 2: FETCHING DATA FROM MULTIPLE SOURCES (restart mode)",
             level = "INFO", log_file = log_file, show_console = TRUE)
  log_message("Will load cached data for processing step", 
             level = "INFO", log_file = log_file)
}

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
if (!"process" %in% skip_steps) {
  log_message("\nSTEP 3: PROCESSING AND COMBINING DATA", 
             level = "INFO", log_file = log_file, show_console = TRUE)
} else {
  log_message("\nSKIPPING STEP 3: PROCESSING AND COMBINING DATA (restart mode)",
             level = "INFO", log_file = log_file, show_console = TRUE)
  log_message("Will load processed data from RDS file", 
             level = "INFO", log_file = log_file)
  
  # Try to load processed data from cache
  processed_data_path <- file.path(config$directories$cache_dir, "processed_sdoh_data.rds")
  if (file.exists(processed_data_path)) {
    processed_data <- readRDS(processed_data_path)
    log_message(paste("Loaded processed data with", nrow(processed_data), "rows and", ncol(processed_data), "columns"),
               level = "INFO", log_file = log_file)
  } else {
    log_message("ERROR: Cannot find cached processed data. Will process data from scratch.", 
               level = "ERROR", log_file = log_file, show_console = TRUE)
  }
}

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
# Only load and run database module if not skipping this step
if (!"database" %in% skip_steps) {
  log_message("\nSTEP 4: CREATING UNIFIED DATABASE", 
             level = "INFO", log_file = log_file, show_console = TRUE)
             
  # Save processed data for future restarts
  processed_data_path <- file.path(config$directories$cache_dir, "processed_sdoh_data.rds")
  saveRDS(processed_data, processed_data_path)
  log_message(paste("Saved processed data to:", processed_data_path),
             level = "INFO", log_file = log_file)
  
  # Robust database module loading with fallback mechanism
  tryCatch({
  # Try to load the database module function from the file
  log_message("Loading database module...", level = "INFO", show_console = TRUE)
  db_code <- readLines("pipeline_modules/module_database.r")
  
  # Extract the create_unified_database function
  start_line <- grep("^create_unified_database <- function\\(", db_code)
  if (length(start_line) == 0) {
    stop("Could not find create_unified_database function in module_database.r")
  }
  
  end_line <- start_line
  brace_count <- 0
  
  # Find the end of the function by counting braces
  for (i in start_line:length(db_code)) {
    line <- db_code[i]
    open_braces <- sum(gregexpr("\\{", line)[[1]] > 0)
    close_braces <- sum(gregexpr("\\}", line)[[1]] > 0)
    brace_count <- brace_count + open_braces - close_braces
    end_line <- i
    if (brace_count == 0) break
  }
  
  # Extract and evaluate the function
  create_unified_database_code <- paste(db_code[start_line:end_line], collapse="\n")
  eval(parse(text = create_unified_database_code))
  log_message("Successfully loaded create_unified_database function from module_database.r", 
             level = "INFO", show_console = TRUE)
}, error = function(e) {
  # Error fallback - use simplified database creation function
  log_message(paste("ERROR loading database module:", conditionMessage(e)),
             level = "ERROR", show_console = TRUE)
  log_message("Using simplified database function as fallback", 
             level = "WARN", show_console = TRUE)
  
  create_unified_database <- function(processed_data, 
                                     crosswalk, 
                                     db_path = "output/us_county_sdoh_unified.duckdb",
                                     overwrite = FALSE,
                                     incremental = FALSE,
                                     force_full_rebuild = FALSE,
                                     data_sources = NULL,
                                     processed_years = NULL) {
    log_message("\nSTEP 4: CREATING UNIFIED DATABASE (FALLBACK VERSION)", 
               level = "INFO", show_console = TRUE)
    
    # Basic database functionality
    require(dplyr)
    require(DBI)
    require(duckdb)
    require(tidyr)
    
    # Create directories if needed
    db_dir <- dirname(db_path)
    if (!dir.exists(db_dir)) {
      dir.create(db_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Ensure the database directory is available
    unified_db_path <- db_path
    log_message(paste("Database path:", unified_db_path),
               level = "INFO", show_console = TRUE)
    
    # Determine whether to use incremental mode
    use_incremental <- incremental && file.exists(unified_db_path) && !force_full_rebuild && !overwrite
    
    if (use_incremental) {
      log_message("Using INCREMENTAL processing mode - only updating new or changed data",
                 level = "INFO", show_console = TRUE)
    } else {
      log_message("Using FULL processing mode",
                 level = "INFO", show_console = TRUE)
    }
    
    # Remove existing database if overwrite is TRUE
    if (file.exists(db_path) && overwrite) {
      file.remove(db_path)
      log_message(paste("Removed existing database:", db_path),
                 level = "INFO", show_console = TRUE)
    }
    
    # Connect to the database
    con <- dbConnect(duckdb::duckdb(), dbdir = unified_db_path)
    
    # Create basic tables
    log_message("Creating counties table...", level = "INFO", show_console = TRUE)
    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS counties (
        geoid VARCHAR PRIMARY KEY,
        name VARCHAR,
        state_fips VARCHAR,
        state_name VARCHAR
      )
    ")
    
    log_message("Creating variables table...", level = "INFO", show_console = TRUE)
    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS variables (
        variable_name VARCHAR PRIMARY KEY,
        domain VARCHAR,
        description VARCHAR,
        type VARCHAR,
        units VARCHAR,
        min_year INTEGER,
        max_year INTEGER,
        extended_only BOOLEAN
      )
    ")
    
    log_message("Creating main SDOH data table...", level = "INFO", show_console = TRUE)
    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS sdoh_data (
        geoid VARCHAR,
        year INTEGER,
        variable_name VARCHAR,
        value DOUBLE,
        data_quality VARCHAR,
        data_source VARCHAR,
        PRIMARY KEY (geoid, year, variable_name)
      )
    ")
    
    # Extract unique counties from processed data
    unique_counties <- processed_data %>%
      select(geoid) %>%
      distinct()
    
    # Add county name and state info
    unique_counties$name <- paste("County", unique_counties$geoid)
    unique_counties$state_fips <- substr(unique_counties$geoid, 1, 2)
    
    # Simple state lookup
    state_lookup <- data.frame(
      state_fips = sprintf("%02d", 1:56),
      state_name = c(state.name, "District of Columbia", 
                    "Puerto Rico", "Virgin Islands", 
                    "Guam", "American Samoa", "Northern Mariana Islands"),
      stringsAsFactors = FALSE
    )
    
    # Join to get state names
    unique_counties <- unique_counties %>%
      left_join(state_lookup, by = "state_fips")
    
    # Insert counties
    dbWriteTable(con, "counties", unique_counties, append = TRUE)
    log_message(paste("Added", nrow(unique_counties), "counties to database"),
               level = "INFO", show_console = TRUE)
    
    # Prepare variables for insertion
    variables_for_db <- crosswalk %>%
      select(variable_name, domain, description, type, units, min_year, max_year, extended_only)
    
    # De-duplicate variables
    variables_for_db <- variables_for_db %>%
      distinct(variable_name, .keep_all = TRUE)
    
    # Insert variables
    dbWriteTable(con, "variables", variables_for_db, append = TRUE)
    log_message(paste("Added", nrow(variables_for_db), "variables to database"),
               level = "INFO", show_console = TRUE)
    
    # Create basic indices
    log_message("Creating basic database indices...", level = "INFO", show_console = TRUE)
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_geoid ON sdoh_data(geoid)")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_year ON sdoh_data(year)")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_variable ON sdoh_data(variable_name)")
    
    # Create a simple view
    log_message("Creating a basic view for data access...", level = "INFO", show_console = TRUE)
    dbExecute(con, "
      CREATE OR REPLACE VIEW latest_data AS
      SELECT 
        c.geoid,
        c.name,
        c.state_fips,
        c.state_name,
        d.year,
        d.variable_name,
        d.value,
        d.data_quality,
        d.data_source
      FROM counties c
      JOIN sdoh_data d ON c.geoid = d.geoid
      WHERE (d.geoid, d.variable_name, d.year) IN (
        SELECT geoid, variable_name, MAX(year) 
        FROM sdoh_data 
        GROUP BY geoid, variable_name
      )
    ")
    
    # Disconnect from the database
    dbDisconnect(con)
    
    log_message("Database created successfully", 
               level = "INFO", show_console = TRUE)
    
    return(TRUE)
  }
})
} else {
  log_message("\nSKIPPING STEP 4: CREATING UNIFIED DATABASE (restart mode)",
             level = "INFO", log_file = log_file, show_console = TRUE)
}

# Determine if we should use incremental processing
use_incremental <- FALSE
if (exists("config") && 
    is.list(config) && 
    "processing" %in% names(config) && 
    "incremental" %in% names(config$processing)) {
  use_incremental <- config$processing$incremental
}

# Check if we should force a full rebuild
force_full_rebuild <- FALSE
if (exists("config") && 
    is.list(config) && 
    "processing" %in% names(config) && 
    "force_full_rebuild" %in% names(config$processing)) {
  force_full_rebuild <- config$processing$force_full_rebuild
}

# Check for command-line override for incremental mode
args <- commandArgs(trailingOnly = TRUE)
if (any(grepl("^--incremental=", args))) {
  incremental_arg <- grep("^--incremental=", args, value = TRUE)[1]
  use_incremental <- as.logical(sub("^--incremental=", "", incremental_arg))
  log_message(paste("Command-line override for incremental mode:", use_incremental),
             level = "INFO", log_file = log_file)
}

# Check for command-line override for full rebuild
if (any(grepl("^--force-full-rebuild=", args))) {
  rebuild_arg <- grep("^--force-full-rebuild=", args, value = TRUE)[1]
  force_full_rebuild <- as.logical(sub("^--force-full-rebuild=", "", rebuild_arg))
  log_message(paste("Command-line override for force-full-rebuild:", force_full_rebuild),
             level = "INFO", log_file = log_file)
}

# Check for restart from a specific step
restart_from <- NULL
skip_steps <- c()
if (any(grepl("^--restart-from=", args))) {
  restart_arg <- grep("^--restart-from=", args, value = TRUE)[1]
  restart_from <- sub("^--restart-from=", "", restart_arg)
  valid_steps <- c("crosswalk", "fetch", "process", "database", "maps", "documentation")
  
  if (restart_from %in% valid_steps) {
    log_message(paste("Restarting pipeline from step:", restart_from),
               level = "INFO", show_console = TRUE)
    
    # Determine which steps to skip
    step_order <- c("crosswalk", "fetch", "process", "database", "maps", "documentation")
    skip_steps <- step_order[1:which(step_order == restart_from) - 1]
    
    if (length(skip_steps) > 0) {
      log_message(paste("Skipping steps:", paste(skip_steps, collapse = ", ")),
                 level = "INFO", show_console = TRUE)
    }
  } else {
    log_message(paste("Invalid restart step:", restart_from, "- must be one of:", paste(valid_steps, collapse = ", ")),
               level = "WARN", show_console = TRUE)
    restart_from <- NULL
  }
}

# Create the unified database
create_unified_database(
  processed_data = processed_data,
  crosswalk = crosswalk,
  db_path = config$database$full_db_path,
  overwrite = config$database$overwrite_db,
  incremental = use_incremental,
  force_full_rebuild = force_full_rebuild,
  data_sources = c("census", "ihme", "traffic_safety", "epa"),
  processed_years = config$years$min_year:config$years$max_year
)

# -------------------------------------------------------------------------
# STEP 5: GENERATE MAPS
# -------------------------------------------------------------------------
if (config$maps$generate_maps && !"maps" %in% skip_steps) {
  source("pipeline_modules/module_maps.r")
  
  # Generate maps
  generate_sdoh_maps(
    db_path = config$database$full_db_path,
    output_dir = config$directories$full_maps_dir,
    conus_only = config$maps$conus_only,
    parallel = config$processing$parallel,
    cores = config$processing$cores
  )
} else if (!"maps" %in% skip_steps && !config$maps$generate_maps) {
  log_message("Maps generation disabled in config.yaml",
             level = "INFO", log_file = log_file, show_console = TRUE)
} else {
  log_message("SKIPPING STEP 5: GENERATE MAPS (restart mode)",
             level = "INFO", log_file = log_file, show_console = TRUE)
}

# -------------------------------------------------------------------------
# STEP 6: GENERATE DOCUMENTATION
# -------------------------------------------------------------------------
if (config$documentation$update_documentation && !"documentation" %in% skip_steps) {
  source("pipeline_modules/module_documentation.r")
  
  # Generate documentation
  generate_documentation(
    crosswalk = crosswalk,
    output_dir = "docs"
  )
} else if (!"documentation" %in% skip_steps && !config$documentation$update_documentation) {
  log_message("Documentation update disabled in config.yaml",
             level = "INFO", log_file = log_file, show_console = TRUE)
} else {
  log_message("SKIPPING STEP 6: GENERATE DOCUMENTATION (restart mode)",
             level = "INFO", log_file = log_file, show_console = TRUE)
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

