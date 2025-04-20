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
    "progressr"
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

# Load config from environment variables or file
load_config <- function(config_file = NULL) {
  config <- list()
  
  # Try to load from environment variables first
  config$census_api_key <- Sys.getenv("CENSUS_API_KEY", "")
  config$ipums_username <- Sys.getenv("IPUMS_USERNAME", "")
  config$ipums_password <- Sys.getenv("IPUMS_PASSWORD", "")
  
  # Load from config file if provided and exists
  if (!is.null(config_file) && file.exists(config_file)) {
    file_config <- readLines(config_file)
    for (line in file_config) {
      # Parse KEY=VALUE pairs
      if (grepl("=", line) && !startsWith(trimws(line), "#")) {
        parts <- strsplit(line, "=", fixed = TRUE)[[1]]
        key <- trimws(parts[1])
        value <- trimws(parts[2])
        config[[key]] <- value
      }
    }
  }
  
  # Check for required config elements
  if (config$census_api_key != "") {
    log_message("Census API key found in environment.",
                level = "INFO", show_console = TRUE)
  } else {
    log_message("No Census API key found. Some data sources may not be available.",
                level = "WARN", show_console = TRUE)
  }
  
  if (config$ipums_username != "" && config$ipums_password != "") {
    log_message("IPUMS credentials found.",
                level = "INFO", show_console = TRUE)
  } else {
    log_message("No IPUMS credentials found. NHGIS data will not be available.",
                level = "WARN", show_console = TRUE)
  }
  
  return(config)
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
initialize_pipeline <- function(config_file = NULL, 
                               use_parallel = TRUE, 
                               num_cores = NULL) {
  # Load required packages
  load_core_packages()
  
  # Load configuration
  config <- load_config(config_file)
  
  # Set up parallel processing if enabled
  if (use_parallel) {
    setup_parallel_processing(use_parallel, num_cores)
  }
  
  # Initialize directories
  initialize_directories()
  
  # Get last update time
  update_info <- get_last_update_time()
  
  return(list(
    config = config,
    update_info = update_info
  ))
}

# Helper function to check if a script was sourced or run directly
is_sourced <- function() {
  # Check if the calling environment is the global environment
  # If it's not, the function is being sourced
  parent_env <- parent.frame()
  return(!identical(parent_env, .GlobalEnv))
}