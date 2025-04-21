#!/usr/bin/env Rscript

# module_core.r
# Core utilities and functions for the SDOH pipeline modules

# Load required packages for all modules
load_core_packages <- function() {
  # List of required packages for the core module
  required_packages <- c(
    "tidyverse", 
    "DBI", 
    "duckdb",
    "sf",
    "httr",
    "jsonlite", 
    "lubridate",
    "here",
    "future",
    "future.apply",
    "progressr",
    "yaml"
  )
  
  # Check and install missing packages if needed
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message(paste("Installing package:", pkg))
      install.packages(pkg, repos = "https://cloud.r-project.org")
    }
    
    library(pkg, character.only = TRUE)
  }
  
  return(TRUE)
}

# Logging utilities
log_message <- function(message, level = "INFO", show_console = TRUE, log_file = NULL) {
  timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
  formatted_message <- paste(timestamp, "[", level, "]", message)
  
  if (show_console) {
    cat(formatted_message, "\n")
  }
  
  if (!is.null(log_file)) {
    if (!dir.exists(dirname(log_file)) && dirname(log_file) != ".") {
      dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
    }
    cat(formatted_message, "\n", file = log_file, append = TRUE)
  }
  
  return(formatted_message)
}

# Setup parallel processing
setup_parallel_processing <- function(use_parallel = TRUE, num_cores = NULL) {
  if (!use_parallel) {
    return(FALSE)
  }
  
  if (is.null(num_cores)) {
    # Use default of N-1 cores (leave one for the OS)
    num_cores <- parallel::detectCores() - 1
    # Minimum of 2 cores
    num_cores <- max(2, num_cores)
  }
  
  # Set up parallel processing
  future::plan(future::multisession, workers = num_cores)
  
  log_message(paste("Parallel processing enabled with", num_cores, "cores using multisession strategy"),
              level = "INFO", show_console = TRUE)
  
  return(TRUE)
}

# Load config from YAML file and override with environment variables
load_config <- function(config_file = "config.yaml") {
  # Default configuration
  default_config <- list(
    directories = list(
      root_dir = getwd(),
      data_dir = "data",
      output_dir = "output",
      logs_dir = "logs",
      cache_dir = "data/cache",
      maps_dir = "output/maps",
      visualizations_dir = "output/visualizations"
    ),
    database = list(
      db_name = "us_county_sdoh_unified.duckdb",
      db_path = "output/us_county_sdoh_unified.duckdb",
      overwrite_db = FALSE
    ),
    data_refresh = list(
      refresh_cache = FALSE,
      max_data_age_days = 30
    ),
    processing = list(
      parallel = TRUE,
      cores = NULL,
      min_cores = 2
    ),
    maps = list(
      generate_maps = TRUE,
      conus_only = TRUE
    ),
    years = list(
      min_year = 1970,
      max_year = 2025
    ),
    documentation = list(
      update_documentation = TRUE
    ),
    api_keys = list(
      census_api_key = ""
    ),
    ipums = list(
      username = "",
      password = ""
    ),
    traffic_safety = list(
      use_fallback = FALSE,
      data_years = c(2020, 2021, 2022)
    )
  )
  
  # Load from YAML config file if it exists
  config <- default_config
  if (!is.null(config_file) && file.exists(config_file)) {
    log_message(paste("Loading configuration from file:", config_file),
                level = "INFO", show_console = TRUE)
    
    tryCatch({
      yaml_config <- yaml::read_yaml(config_file)
      
      # Merge with default config (recursive)
      config <- merge_lists(config, yaml_config)
      
      log_message("Configuration loaded successfully",
                  level = "INFO", show_console = TRUE)
    }, error = function(e) {
      log_message(paste("Error loading configuration file:", conditionMessage(e)),
                  level = "ERROR", show_console = TRUE)
      log_message("Using default configuration",
                  level = "WARN", show_console = TRUE)
    })
  } else {
    log_message("No configuration file found. Using default configuration.",
                level = "WARN", show_console = TRUE)
  }
  
  # Network path overrides - special case for separate data and output storage
  if (!is.null(config$network_paths)) {
    for (key in names(config$network_paths)) {
      if (key %in% names(config$directories) && !is.null(config$network_paths[[key]]) && 
          config$network_paths[[key]] != "") {
        log_message(paste("Using network path for", key, ":", config$network_paths[[key]]),
                    level = "INFO", show_console = TRUE)
        config$directories[[key]] <- config$network_paths[[key]]
      }
    }
  }
  
  # Override with environment variables if they exist
  env_census_api_key <- Sys.getenv("CENSUS_API_KEY", "")
  if (env_census_api_key != "") {
    config$api_keys$census_api_key <- env_census_api_key
  }
  
  env_ipums_username <- Sys.getenv("IPUMS_USERNAME", "")
  if (env_ipums_username != "") {
    config$ipums$username <- env_ipums_username
  }
  
  env_ipums_password <- Sys.getenv("IPUMS_PASSWORD", "")
  if (env_ipums_password != "") {
    config$ipums$password <- env_ipums_password
  }
  
  # Helper function to resolve paths correctly
  resolve_path <- function(path) {
    # If it starts with / or contains a drive letter (like C:), it's absolute
    if (grepl("^/|^[A-Za-z]:", path)) {
      return(path)  # Return as is - it's an absolute path
    } else {
      # It's a relative path, resolve it relative to root_dir
      return(file.path(config$directories$root_dir, path))
    }
  }
  
  # Resolve paths relative to root_dir
  config$directories$full_data_dir <- resolve_path(config$directories$data_dir)
  config$directories$full_output_dir <- resolve_path(config$directories$output_dir)
  config$directories$full_logs_dir <- resolve_path(config$directories$logs_dir)
  config$directories$full_cache_dir <- resolve_path(config$directories$cache_dir)
  config$directories$full_maps_dir <- resolve_path(config$directories$maps_dir)
  
  # Construct full DB path
  config$database$full_db_path <- resolve_path(config$database$db_path)
  
  # Check for required config elements
  if (config$api_keys$census_api_key != "") {
    log_message("Census API key found in configuration.",
                level = "INFO", show_console = TRUE)
  } else {
    log_message("No Census API key found. Some data sources may not be available.",
                level = "WARN", show_console = TRUE)
  }
  
  if (config$ipums$username != "" && config$ipums$password != "") {
    log_message("IPUMS credentials found in configuration.",
                level = "INFO", show_console = TRUE)
  } else {
    log_message("No IPUMS credentials found. NHGIS data will not be available.",
                level = "WARN", show_console = TRUE)
  }
  
  return(config)
}

# Helper function to recursively merge two lists
merge_lists <- function(x, y) {
  if (is.list(x) && is.list(y)) {
    # For each name in y, merge with the corresponding element in x
    for (name in names(y)) {
      if (name %in% names(x) && is.list(x[[name]]) && is.list(y[[name]])) {
        # Both x and y have this name and they're both lists, so recurse
        x[[name]] <- merge_lists(x[[name]], y[[name]])
      } else {
        # Either x doesn't have this name, or one of them isn't a list, so just use y's value
        x[[name]] <- y[[name]]
      }
    }
    return(x)
  } else {
    # If one of them isn't a list, just return y
    return(y)
  }
}

# Initialize output directories
initialize_directories <- function(dirs = c("data", "output", "logs")) {
  for (dir in dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      log_message(paste("Created directory:", dir),
                  level = "INFO", show_console = TRUE)
    }
  }
  
  return(TRUE)
}

# Get the last update time for the dataset
get_last_update_time <- function(update_file = "data/last_update.txt") {
  if (file.exists(update_file)) {
    last_update <- as.Date(readLines(update_file)[1])
    days_since_update <- as.numeric(difftime(Sys.Date(), last_update, units = "days"))
    
    log_message(paste("Data was last updated on", last_update, "(", round(days_since_update), "days ago)"),
                level = "INFO", show_console = TRUE)
    
    return(list(
      last_update = last_update,
      days_since_update = days_since_update
    ))
  } else {
    log_message("No previous data update found. Will perform a full update.",
                level = "INFO", show_console = TRUE)
    
    return(list(
      last_update = NULL,
      days_since_update = Inf
    ))
  }
}

# Update the last update time
update_last_update_time <- function(update_file = "data/last_update.txt") {
  writeLines(as.character(Sys.Date()), update_file)
  log_message(paste("Updated last_update.txt with current date:", Sys.Date()),
              level = "INFO", show_console = TRUE)
  
  return(TRUE)
}

# SDOH pipeline initialization
initialize_pipeline <- function(config_file = "config.yaml", 
                               use_parallel = NULL, 
                               num_cores = NULL) {
  # Load required packages
  load_core_packages()
  
  # Load configuration from YAML
  config <- load_config(config_file)
  
  # Use config settings unless overridden by function parameters
  use_parallel <- if (!is.null(use_parallel)) use_parallel else config$processing$parallel
  num_cores <- if (!is.null(num_cores)) num_cores else config$processing$cores
  
  # Determine min cores from config
  min_cores <- config$processing$min_cores
  if (!is.null(num_cores)) {
    num_cores <- max(min_cores, num_cores)
  }
  
  # Set up parallel processing if enabled
  if (use_parallel) {
    setup_parallel_processing(use_parallel, num_cores)
  }
  
  # Initialize directories from config
  dirs_to_create <- c(
    config$directories$data_dir,
    config$directories$output_dir,
    config$directories$logs_dir,
    config$directories$cache_dir,
    config$directories$maps_dir,
    config$directories$visualizations_dir
  )
  initialize_directories(dirs_to_create)
  
  # Get last update time
  update_file <- file.path(config$directories$data_dir, "last_update.txt")
  update_info <- get_last_update_time(update_file)
  
  # Add the update info to the config
  config$update_info <- update_info
  
  return(config)
}

# Helper function to check if a script was sourced or run directly
is_sourced <- function() {
  # Check if the calling environment is the global environment
  # If it's not, the function is being sourced
  parent_env <- parent.frame()
  return(!identical(parent_env, .GlobalEnv))
}