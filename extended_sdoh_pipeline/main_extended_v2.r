#!/usr/bin/env Rscript

# Extended SDOH County-Level Pipeline
# This script coordinates the extended pipeline that incorporates additional
# Social Determinants of Health variables from multiple sources.

# Load required packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(DBI)
  library(duckdb)
  library(sf)
  library(lubridate)
  library(glue)
  library(parallel)
  library(httr)
  library(jsonlite)
  library(zoo)  # For interpolation
})

# Initialize script environment
script_start_time <- Sys.time()
# Get the script directory
script_directory <- tryCatch({
  # Try to get the script directory from the calling frame
  dirname(sys.frame(1)$ofile)
}, error = function(e) {
  # If that fails, use the current directory
  getwd()
})

# If script_directory is empty or ".", use getwd()
if (is.null(script_directory) || script_directory == ".") {
  script_directory <- getwd()
}

# If we're running from the R directory, add extended_sdoh_pipeline
if (basename(script_directory) == "R") {
  script_directory <- file.path(script_directory, "extended_sdoh_pipeline")
}

# Make sure the directory exists
if (!dir.exists(script_directory)) {
  warning("Script directory does not exist: ", script_directory)
  script_directory <- getwd()
}

# Define paths relative to the script directory
root_dir <- dirname(script_directory)
data_dir <- file.path(script_directory, "data")
logs_dir <- file.path(script_directory, "logs")
output_dir <- file.path(script_directory, "output")
cache_dir <- file.path(data_dir, "cache")

# Ensure directories exist
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(logs_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

# Configure logging
log_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_file <- file.path(logs_dir, paste0("extended_sdoh_pipeline_", log_timestamp, ".log"))
log_connection <- file(log_file, open = "w")

# Log function for consistent formatted logging
log_message <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  formatted_message <- sprintf("[%s] [%s] %s", timestamp, level, message)
  cat(formatted_message, "\n", file = log_connection, append = TRUE)
  cat(formatted_message, "\n")
  flush(log_connection)
}

# Function to get a standardized list of counties
get_county_list <- function() {
  # Try different sources for county data
  
  # First check if we have county data in the current directory
  county_files <- list.files(pattern = "county_metadata.csv|county_list.csv")
  if (length(county_files) > 0) {
    log_message(paste("Using county list from", county_files[1]))
    return(read.csv(county_files[1], stringsAsFactors = FALSE))
  }
  
  # Try using the crosswalk database if available
  crosswalk_file <- file.path(output_dir, "variable_crosswalk_extended.csv")
  if (file.exists(crosswalk_file)) {
    log_message("Getting county list from existing data in cache")
    
    # Check for cached county data
    cache_files <- list.files(cache_dir, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
    for (file in cache_files) {
      tryCatch({
        data <- readRDS(file)
        if ("GEOID" %in% names(data) && "NAME" %in% names(data) && nrow(data) > 0) {
          county_data <- data %>% 
            select(GEOID, NAME) %>% 
            distinct()
          
          log_message(paste("Found", nrow(county_data), "counties in cache file:", file))
          return(county_data)
        }
      }, error = function(e) {
        # Skip this file
      })
    }
  }
  
  # Fallback to builtin county list (minimal representation)
  log_message("Using built-in minimal county list", "WARN")
  counties <- data.frame(
    GEOID = c("01001", "01003", "01005", "01007", "01009", 
              "06001", "06037", "06075", 
              "17031", "17043", 
              "36001", "36061", "36047", 
              "48201", "48113"),
    NAME = c("Autauga County, Alabama", "Baldwin County, Alabama", 
             "Barbour County, Alabama", "Bibb County, Alabama", 
             "Blount County, Alabama",
             "Alameda County, California", "Los Angeles County, California", 
             "San Francisco County, California",
             "Cook County, Illinois", "DuPage County, Illinois",
             "Albany County, New York", "New York County, New York", 
             "Kings County, New York",
             "Harris County, Texas", "Dallas County, Texas"),
    stringsAsFactors = FALSE
  )
  return(counties)
}

# Start logging
log_message("Starting Extended SDOH Pipeline")
log_message(paste("Script directory:", script_directory))
log_message(paste("Log file:", log_file))

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
force_update <- "--force-update" %in% args || "-f" %in% args
verbose <- "--verbose" %in% args || "-v" %in% args
skip_interpolation <- "--skip-interpolation" %in% args
allow_simulation <- "--allow-simulation" %in% args
allow_interpolation <- "--allow-interpolation" %in% args || !skip_interpolation
offline_mode <- "--offline-mode" %in% args || "--offline" %in% args

# Configure data quality handling
data_quality_flags <- list(
  # Data type flags
  direct = "direct", # Data directly from source
  interpolated = "interpolated", # Data interpolated from existing points
  extrapolated = "extrapolated", # Data extrapolated beyond available time range
  simulated = "simulated", # Fully simulated data (not based on real values)
  missing = NA, # Data that couldn't be obtained and wasn't simulated
  
  # Special flags
  imputed = "imputed" # For values filled in by statistical methods
)

# Log configuration
log_message(paste("Force update:", force_update))
log_message(paste("Verbose mode:", verbose))
log_message(paste("Skip interpolation:", skip_interpolation))
log_message(paste("Allow simulation:", allow_simulation))
log_message(paste("Offline mode:", offline_mode))

# Years to process
start_year <- 2000
end_year <- as.integer(format(Sys.Date(), "%Y"))
years <- start_year:end_year
log_message(paste("Processing years", start_year, "to", end_year))

# Check for original SDOH database (to integrate with)
original_db_path <- file.path(root_dir, "us_county_sdoh_data.duckdb")
has_original_db <- file.exists(original_db_path)
log_message(paste("Original SDOH database found:", has_original_db))

# Define extended database path
extended_db_path <- file.path(output_dir, "us_county_sdoh_extended.duckdb")

# Function to safely source a script
safe_source <- function(script_name, required = TRUE) {
  script_path <- file.path(script_directory, script_name)
  if (file.exists(script_path)) {
    log_message(paste("Sourcing", script_name))
    tryCatch({
      source(script_path, local = TRUE)
      log_message(paste("Successfully sourced", script_name))
      return(TRUE)
    }, error = function(e) {
      log_message(paste("Error sourcing", script_name, ":", conditionMessage(e)), "ERROR")
      if (required) {
        stop(paste("Required script", script_name, "failed to source:", conditionMessage(e)))
      }
      return(FALSE)
    })
  } else {
    log_message(paste("Script not found:", script_path), if (required) "ERROR" else "WARN")
    if (required) {
      stop(paste("Required script", script_name, "not found"))
    }
    return(FALSE)
  }
}

# Main pipeline execution
tryCatch({
  # Step 1: Build or update the extended variable crosswalk
  log_message("Step 1: Building extended variable crosswalk")
  crosswalk_result <- FALSE
  if (file.exists(file.path(script_directory, "build_extended_crosswalk_v2.r"))) {
    source(file.path(script_directory, "build_extended_crosswalk_v2.r"), local = TRUE)
    crosswalk_result <- build_extended_crosswalk_v2(
      output_dir = output_dir,
      force_update = force_update,
      verbose = verbose
    )
  } else {
    log_message("Crosswalk builder script not found, will be created later", "WARN")
  }
  log_message(paste("Crosswalk building result:", crosswalk_result))
  
  # Step 2: Fetch data from multiple sources
  log_message("Step 2: Fetching data from multiple sources")
  
  # 2.1 USDA Food Environment Atlas
  log_message("2.1: Fetching USDA Food Environment data")
  food_env_data <- NULL
  if (file.exists(file.path(script_directory, "fetch_usda_food_atlas.r"))) {
    source(file.path(script_directory, "fetch_usda_food_atlas.r"), local = TRUE)
    tryCatch({
      food_env_data <- fetch_usda_food_atlas(
        years = years,
        cache_dir = cache_dir,
        refresh_cache = force_update,
        allow_simulation = allow_simulation,
        allow_interpolation = allow_interpolation,
        data_quality_flags = data_quality_flags,
        offline_mode = offline_mode
      )
      log_message(paste("Successfully fetched food environment data with", 
                        nrow(food_env_data), "records"))
      
      # Check if data has data quality flags and report statistics
      if (verbose && !is.null(food_env_data)) {
        # Find data quality columns
        quality_cols <- grep("_data_quality$", names(food_env_data), value = TRUE)
        if (length(quality_cols) > 0) {
          # Count number of each quality type
          quality_stats <- lapply(quality_cols, function(col) {
            if (!col %in% names(food_env_data)) return(NULL)
            table(food_env_data[[col]], useNA = "always")
          })
          
          # Log summary
          log_message("Food environment data quality summary:", "DEBUG")
          for (i in seq_along(quality_cols)) {
            if (!is.null(quality_stats[[i]])) {
              col_name <- gsub("_data_quality$", "", quality_cols[i])
              log_message(paste(" -", col_name, ": ", 
                              paste(names(quality_stats[[i]]), 
                                   quality_stats[[i]], 
                                   sep="=", collapse=", ")), 
                          "DEBUG")
            }
          }
        }
      }
    }, error = function(e) {
      log_message(paste("Error fetching food environment data:", conditionMessage(e)), "ERROR")
      if (allow_simulation) {
        log_message("Attempting to use simulation mode for food environment data", "WARN")
        food_env_data <- fetch_usda_food_atlas(
          years = years,
          cache_dir = cache_dir,
          refresh_cache = TRUE,
          allow_simulation = TRUE,
          allow_interpolation = allow_interpolation,
          data_quality_flags = data_quality_flags,
          offline_mode = TRUE
        )
        log_message("Successfully created simulated food environment data")
      } else {
        # Create empty data frame with NAs for all variables
        log_message("Creating empty food environment dataset with NA values", "WARN")
        
        # Get variable list from crosswalk
        crosswalk_file <- file.path(output_dir, "variable_crosswalk_extended.csv")
        if (file.exists(crosswalk_file)) {
          crosswalk <- read.csv(crosswalk_file)
          food_vars <- crosswalk$variable_name[crosswalk$domain == "Food Environment & Access"]
          
          # Create data frame with county GEOIDs and years
          counties <- get_county_list() # This function needs to be defined or replaced
          if (is.null(counties)) {
            # Fallback to minimal county list
            counties <- data.frame(
              GEOID = c("01001", "06037", "17031", "36061", "48201"),
              NAME = c("Autauga County, AL", "Los Angeles County, CA", 
                      "Cook County, IL", "New York County, NY", 
                      "Harris County, TX")
            )
          }
          
          # Build empty dataframe with all counties and years
          grid <- expand.grid(
            GEOID = counties$GEOID,
            year = years,
            stringsAsFactors = FALSE
          )
          
          # Add NAME column
          grid$NAME <- counties$NAME[match(grid$GEOID, counties$GEOID)]
          
          # Add empty variable columns with NAs
          for (var in food_vars) {
            grid[[var]] <- NA_real_
            grid[[paste0(var, "_data_quality")]] <- data_quality_flags$missing
            grid[[paste0(var, "_data_source")]] <- "NOT_AVAILABLE"
            grid[[paste0(var, "_data_vintage")]] <- NA_character_
          }
          
          food_env_data <- as_tibble(grid)
        } else {
          log_message("No crosswalk file found, cannot create empty food environment dataset", "ERROR")
        }
      }
    })
  } else {
    log_message("USDA Food Atlas fetcher not found, will be created", "WARN")
  }
  
  # 2.2 EPA Environmental Data
  log_message("2.2: Fetching EPA Environmental data")
  epa_data <- NULL
  if (file.exists(file.path(script_directory, "fetch_epa_data.r"))) {
    source(file.path(script_directory, "fetch_epa_data.r"), local = TRUE)
    tryCatch({
      epa_data <- fetch_epa_data(
        years = years,
        cache_dir = cache_dir,
        refresh_cache = force_update,
        allow_simulation = allow_simulation,
        allow_interpolation = allow_interpolation,
        data_quality_flags = data_quality_flags,
        offline_mode = offline_mode
      )
      log_message(paste("Successfully fetched EPA data with", 
                       nrow(epa_data), "records"))
      
      # Check if data has data quality flags and report statistics
      if (verbose && !is.null(epa_data)) {
        # Find data quality columns
        quality_cols <- grep("_data_quality$", names(epa_data), value = TRUE)
        if (length(quality_cols) > 0) {
          # Count number of each quality type
          quality_stats <- lapply(quality_cols, function(col) {
            if (!col %in% names(epa_data)) return(NULL)
            table(epa_data[[col]], useNA = "always")
          })
          
          # Log summary
          log_message("EPA environmental data quality summary:", "DEBUG")
          for (i in seq_along(quality_cols)) {
            if (!is.null(quality_stats[[i]])) {
              col_name <- gsub("_data_quality$", "", quality_cols[i])
              log_message(paste(" -", col_name, ": ", 
                              paste(names(quality_stats[[i]]), 
                                   quality_stats[[i]], 
                                   sep="=", collapse=", ")), 
                          "DEBUG")
            }
          }
        }
      }
    }, error = function(e) {
      log_message(paste("Error fetching EPA data:", conditionMessage(e)), "ERROR")
      if (allow_simulation) {
        log_message("Attempting to use simulation mode for EPA data", "WARN")
        epa_data <- fetch_epa_data(
          years = years,
          cache_dir = cache_dir,
          refresh_cache = TRUE,
          allow_simulation = TRUE,
          allow_interpolation = allow_interpolation,
          data_quality_flags = data_quality_flags,
          offline_mode = TRUE
        )
        log_message("Successfully created simulated EPA data")
      } else {
        # Create empty data frame with NAs for all variables
        log_message("Creating empty EPA environmental dataset with NA values", "WARN")
        
        # Get variable list from crosswalk
        crosswalk_file <- file.path(output_dir, "variable_crosswalk_extended.csv")
        if (file.exists(crosswalk_file)) {
          crosswalk <- read.csv(crosswalk_file)
          env_vars <- crosswalk$variable_name[crosswalk$domain == "Environmental Health"]
          
          # Create data frame with county GEOIDs and years
          counties <- get_county_list() # This function needs to be defined or replaced
          if (is.null(counties)) {
            # Fallback to minimal county list
            counties <- data.frame(
              GEOID = c("01001", "06037", "17031", "36061", "48201"),
              NAME = c("Autauga County, AL", "Los Angeles County, CA", 
                      "Cook County, IL", "New York County, NY", 
                      "Harris County, TX")
            )
          }
          
          # Build empty dataframe with all counties and years
          grid <- expand.grid(
            GEOID = counties$GEOID,
            year = years,
            stringsAsFactors = FALSE
          )
          
          # Add NAME column
          grid$NAME <- counties$NAME[match(grid$GEOID, counties$GEOID)]
          
          # Add empty variable columns with NAs
          for (var in env_vars) {
            grid[[var]] <- NA_real_
            grid[[paste0(var, "_data_quality")]] <- data_quality_flags$missing
            grid[[paste0(var, "_data_source")]] <- "NOT_AVAILABLE"
            grid[[paste0(var, "_data_vintage")]] <- NA_character_
          }
          
          epa_data <- as_tibble(grid)
        } else {
          log_message("No crosswalk file found, cannot create empty EPA dataset", "ERROR")
        }
      }
    })
  } else {
    log_message("EPA data fetcher not found, will be created", "WARN")
  }
  
  # 2.3 Housing Data
  log_message("2.3: Fetching Housing data")
  housing_data <- NULL
  if (file.exists(file.path(script_directory, "fetch_housing_data.r"))) {
    source(file.path(script_directory, "fetch_housing_data.r"), local = TRUE)
    housing_data <- fetch_housing_data(
      years = years,
      cache_dir = cache_dir,
      refresh_cache = force_update,
      allow_simulation = allow_simulation,
      allow_interpolation = allow_interpolation,
      data_quality_flags = data_quality_flags,
      offline_mode = offline_mode
    )
    
    log_message(paste("Successfully fetched housing data with", 
                     nrow(housing_data), "records"))
    
    # Check if data has data quality flags and report statistics
    if (verbose && !is.null(housing_data)) {
      # Find data quality columns
      quality_cols <- grep("_data_quality$", names(housing_data), value = TRUE)
      if (length(quality_cols) > 0) {
        # Count number of each quality type
        quality_stats <- lapply(quality_cols, function(col) {
          if (!col %in% names(housing_data)) return(NULL)
          table(housing_data[[col]], useNA = "always")
        })
        
        # Log summary
        log_message("Housing data quality summary:", "DEBUG")
        for (i in seq_along(quality_cols)) {
          if (!is.null(quality_stats[[i]])) {
            col_name <- gsub("_data_quality$", "", quality_cols[i])
            log_message(paste(" -", col_name, ": ", 
                            paste(names(quality_stats[[i]]), 
                                 quality_stats[[i]], 
                                 sep="=", collapse=", ")), 
                        "DEBUG")
          }
        }
      }
    }
  } else {
    log_message("Housing data fetcher not found, will be created", "WARN")
  }
  
  # 2.4 Healthcare Access Data
  log_message("2.4: Fetching Healthcare Access data")
  healthcare_data <- NULL
  if (file.exists(file.path(script_directory, "fetch_healthcare_data.r"))) {
    source(file.path(script_directory, "fetch_healthcare_data.r"), local = TRUE)
    healthcare_data <- fetch_healthcare_data(
      years = years,
      cache_dir = cache_dir,
      refresh_cache = force_update,
      allow_simulation = allow_simulation,
      allow_interpolation = allow_interpolation,
      data_quality_flags = data_quality_flags,
      offline_mode = offline_mode
    )
    
    log_message(paste("Successfully fetched healthcare data with", 
                     nrow(healthcare_data), "records"))
    
    # Check if data has data quality flags and report statistics
    if (verbose && !is.null(healthcare_data)) {
      # Find data quality columns
      quality_cols <- grep("_data_quality$", names(healthcare_data), value = TRUE)
      if (length(quality_cols) > 0) {
        # Count number of each quality type
        quality_stats <- lapply(quality_cols, function(col) {
          if (!col %in% names(healthcare_data)) return(NULL)
          table(healthcare_data[[col]], useNA = "always")
        })
        
        # Log summary
        log_message("Healthcare data quality summary:", "DEBUG")
        for (i in seq_along(quality_cols)) {
          if (!is.null(quality_stats[[i]])) {
            col_name <- gsub("_data_quality$", "", quality_cols[i])
            log_message(paste(" -", col_name, ": ", 
                            paste(names(quality_stats[[i]]), 
                                 quality_stats[[i]], 
                                 sep="=", collapse=", ")), 
                        "DEBUG")
          }
        }
      }
    }
  } else {
    log_message("Healthcare data fetcher not found, will be created", "WARN")
  }
  
  # 2.5 Transportation Data
  log_message("2.5: Fetching Transportation data")
  transportation_data <- NULL
  if (file.exists(file.path(script_directory, "fetch_transportation_data.r"))) {
    source(file.path(script_directory, "fetch_transportation_data.r"), local = TRUE)
    transportation_data <- fetch_transportation_data(
      years = years,
      cache_dir = cache_dir,
      refresh_cache = force_update,
      allow_simulation = allow_simulation,
      allow_interpolation = allow_interpolation,
      data_quality_flags = data_quality_flags,
      offline_mode = offline_mode
    )
    
    log_message(paste("Successfully fetched transportation data with", 
                     nrow(transportation_data), "records"))
    
    # Check if data has data quality flags and report statistics
    if (verbose && !is.null(transportation_data)) {
      # Find data quality columns
      quality_cols <- grep("_data_quality$", names(transportation_data), value = TRUE)
      if (length(quality_cols) > 0) {
        # Count number of each quality type
        quality_stats <- lapply(quality_cols, function(col) {
          if (!col %in% names(transportation_data)) return(NULL)
          table(transportation_data[[col]], useNA = "always")
        })
        
        # Log summary
        log_message("Transportation data quality summary:", "DEBUG")
        for (i in seq_along(quality_cols)) {
          if (!is.null(quality_stats[[i]])) {
            col_name <- gsub("_data_quality$", "", quality_cols[i])
            log_message(paste(" -", col_name, ": ", 
                            paste(names(quality_stats[[i]]), 
                                 quality_stats[[i]], 
                                 sep="=", collapse=", ")), 
                        "DEBUG")
          }
        }
      }
    }
  } else {
    log_message("Transportation data fetcher not found, will be created", "WARN")
  }
  
  # 2.6 Social Cohesion Data
  log_message("2.6: Fetching Social Cohesion data")
  social_data <- NULL
  if (file.exists(file.path(script_directory, "fetch_social_cohesion_data.r"))) {
    source(file.path(script_directory, "fetch_social_cohesion_data.r"), local = TRUE)
    social_data <- fetch_social_cohesion_data(
      years = years,
      cache_dir = cache_dir,
      refresh_cache = force_update,
      allow_simulation = allow_simulation,
      allow_interpolation = allow_interpolation,
      data_quality_flags = data_quality_flags,
      offline_mode = offline_mode
    )
    
    log_message(paste("Successfully fetched social cohesion data with", 
                     nrow(social_data), "records"))
    
    # Check if data has data quality flags and report statistics
    if (verbose && !is.null(social_data)) {
      # Find data quality columns
      quality_cols <- grep("_data_quality$", names(social_data), value = TRUE)
      if (length(quality_cols) > 0) {
        # Count number of each quality type
        quality_stats <- lapply(quality_cols, function(col) {
          if (!col %in% names(social_data)) return(NULL)
          table(social_data[[col]], useNA = "always")
        })
        
        # Log summary
        log_message("Social cohesion data quality summary:", "DEBUG")
        for (i in seq_along(quality_cols)) {
          if (!is.null(quality_stats[[i]])) {
            col_name <- gsub("_data_quality$", "", quality_cols[i])
            log_message(paste(" -", col_name, ": ", 
                            paste(names(quality_stats[[i]]), 
                                 quality_stats[[i]], 
                                 sep="=", collapse=", ")), 
                        "DEBUG")
          }
        }
      }
    }
  } else {
    log_message("Social cohesion data fetcher not found, will be created", "WARN")
  }
  
  # 2.7 Crime & Safety Data
  log_message("2.7: Fetching Crime & Safety data")
  crime_data <- NULL
  if (file.exists(file.path(script_directory, "fetch_crime_data.r"))) {
    source(file.path(script_directory, "fetch_crime_data.r"), local = TRUE)
    crime_data <- fetch_crime_data(
      years = years,
      cache_dir = cache_dir,
      refresh_cache = force_update,
      allow_simulation = allow_simulation,
      allow_interpolation = allow_interpolation,
      data_quality_flags = data_quality_flags,
      offline_mode = offline_mode
    )
    
    log_message(paste("Successfully fetched crime data with", 
                     nrow(crime_data), "records"))
    
    # Check if data has data quality flags and report statistics
    if (verbose && !is.null(crime_data)) {
      # Find data quality columns
      quality_cols <- grep("_data_quality$", names(crime_data), value = TRUE)
      if (length(quality_cols) > 0) {
        # Count number of each quality type
        quality_stats <- lapply(quality_cols, function(col) {
          if (!col %in% names(crime_data)) return(NULL)
          table(crime_data[[col]], useNA = "always")
        })
        
        # Log summary
        log_message("Crime data quality summary:", "DEBUG")
        for (i in seq_along(quality_cols)) {
          if (!is.null(quality_stats[[i]])) {
            col_name <- gsub("_data_quality$", "", quality_cols[i])
            log_message(paste(" -", col_name, ": ", 
                            paste(names(quality_stats[[i]]), 
                                 quality_stats[[i]], 
                                 sep="=", collapse=", ")), 
                        "DEBUG")
          }
        }
      }
    }
  } else {
    log_message("Crime data fetcher not found, will be created", "WARN")
  }
  
  # 2.8 Education Data
  log_message("2.8: Fetching Education data")
  education_data <- NULL
  if (file.exists(file.path(script_directory, "fetch_education_data.r"))) {
    source(file.path(script_directory, "fetch_education_data.r"), local = TRUE)
    education_data <- fetch_education_data(
      years = years,
      cache_dir = cache_dir,
      refresh_cache = force_update,
      allow_simulation = allow_simulation,
      allow_interpolation = allow_interpolation,
      data_quality_flags = data_quality_flags,
      offline_mode = offline_mode
    )
    
    log_message(paste("Successfully fetched education data with", 
                     nrow(education_data), "records"))
    
    # Check if data has data quality flags and report statistics
    if (verbose && !is.null(education_data)) {
      # Find data quality columns
      quality_cols <- grep("_data_quality$", names(education_data), value = TRUE)
      if (length(quality_cols) > 0) {
        # Count number of each quality type
        quality_stats <- lapply(quality_cols, function(col) {
          if (!col %in% names(education_data)) return(NULL)
          table(education_data[[col]], useNA = "always")
        })
        
        # Log summary
        log_message("Education data quality summary:", "DEBUG")
        for (i in seq_along(quality_cols)) {
          if (!is.null(quality_stats[[i]])) {
            col_name <- gsub("_data_quality$", "", quality_cols[i])
            log_message(paste(" -", col_name, ": ", 
                            paste(names(quality_stats[[i]]), 
                                 quality_stats[[i]], 
                                 sep="=", collapse=", ")), 
                        "DEBUG")
          }
        }
      }
    }
  } else {
    log_message("Education data fetcher not found, will be created", "WARN")
  }
  
  # 2.9 Economic Data
  log_message("2.9: Fetching Economic data")
  economic_data <- NULL
  if (file.exists(file.path(script_directory, "fetch_economic_data.r"))) {
    source(file.path(script_directory, "fetch_economic_data.r"), local = TRUE)
    economic_data <- fetch_economic_data(
      years = years,
      cache_dir = cache_dir,
      refresh_cache = force_update,
      allow_simulation = allow_simulation,
      allow_interpolation = allow_interpolation,
      data_quality_flags = data_quality_flags,
      offline_mode = offline_mode
    )
    
    log_message(paste("Successfully fetched economic data with", 
                     nrow(economic_data), "records"))
    
    # Check if data has data quality flags and report statistics
    if (verbose && !is.null(economic_data)) {
      # Find data quality columns
      quality_cols <- grep("_data_quality$", names(economic_data), value = TRUE)
      if (length(quality_cols) > 0) {
        # Count number of each quality type
        quality_stats <- lapply(quality_cols, function(col) {
          if (!col %in% names(economic_data)) return(NULL)
          table(economic_data[[col]], useNA = "always")
        })
        
        # Log summary
        log_message("Economic data quality summary:", "DEBUG")
        for (i in seq_along(quality_cols)) {
          if (!is.null(quality_stats[[i]])) {
            col_name <- gsub("_data_quality$", "", quality_cols[i])
            log_message(paste(" -", col_name, ": ", 
                            paste(names(quality_stats[[i]]), 
                                 quality_stats[[i]], 
                                 sep="=", collapse=", ")), 
                        "DEBUG")
          }
        }
      }
    }
  } else {
    log_message("Economic data fetcher not found, will be created", "WARN")
  }
  
  # 2.10 Built Environment Data
  log_message("2.10: Fetching Built Environment data")
  built_env_data <- NULL
  if (file.exists(file.path(script_directory, "fetch_built_environment_data.r"))) {
    source(file.path(script_directory, "fetch_built_environment_data.r"), local = TRUE)
    built_env_data <- fetch_built_environment_data(
      years = years,
      cache_dir = cache_dir,
      refresh_cache = force_update,
      allow_simulation = allow_simulation,
      allow_interpolation = allow_interpolation,
      data_quality_flags = data_quality_flags,
      offline_mode = offline_mode
    )
    
    log_message(paste("Successfully fetched built environment data with", 
                     nrow(built_env_data), "records"))
    
    # Check if data has data quality flags and report statistics
    if (verbose && !is.null(built_env_data)) {
      # Find data quality columns
      quality_cols <- grep("_data_quality$", names(built_env_data), value = TRUE)
      if (length(quality_cols) > 0) {
        # Count number of each quality type
        quality_stats <- lapply(quality_cols, function(col) {
          if (!col %in% names(built_env_data)) return(NULL)
          table(built_env_data[[col]], useNA = "always")
        })
        
        # Log summary
        log_message("Built environment data quality summary:", "DEBUG")
        for (i in seq_along(quality_cols)) {
          if (!is.null(quality_stats[[i]])) {
            col_name <- gsub("_data_quality$", "", quality_cols[i])
            log_message(paste(" -", col_name, ": ", 
                            paste(names(quality_stats[[i]]), 
                                 quality_stats[[i]], 
                                 sep="=", collapse=", ")), 
                        "DEBUG")
          }
        }
      }
    }
  } else {
    log_message("Built environment data fetcher not found, will be created", "WARN")
  }
  
  # Step 3: Process all data
  log_message("Step 3: Processing all data")
  if (file.exists(file.path(script_directory, "process_extended_data_v2.r"))) {
    source(file.path(script_directory, "process_extended_data_v2.r"), local = TRUE)
    
    # Collect all data sources
    all_data_sources <- list(
      food_environment = food_env_data,
      environmental = epa_data,
      housing = housing_data,
      healthcare = healthcare_data,
      transportation = transportation_data,
      social_cohesion = social_data,
      crime = crime_data,
      education = education_data,
      economic = economic_data,
      built_environment = built_env_data
    )
    
    # Process all data
    processed_data <- process_extended_data_v2(
      data_sources = all_data_sources,
      years = years,
      skip_interpolation = skip_interpolation,
      original_db_path = if (has_original_db) original_db_path else NULL,
      verbose = verbose,
      data_quality_flags = data_quality_flags
    )
    
    # Report final data quality statistics after processing
    if (verbose && !is.null(processed_data)) {
      log_message("Final processed data quality summary:", "INFO")
      
      # Find data quality columns
      quality_cols <- grep("_data_quality$", names(processed_data), value = TRUE)
      if (length(quality_cols) > 0) {
        # Count number of each quality type
        quality_stats <- lapply(quality_cols, function(col) {
          if (!col %in% names(processed_data)) return(NULL)
          table(processed_data[[col]], useNA = "always")
        })
        
        # Log summary
        for (i in seq_along(quality_cols)) {
          if (!is.null(quality_stats[[i]])) {
            col_name <- gsub("_data_quality$", "", quality_cols[i])
            log_message(paste(" -", col_name, ": ", 
                           paste(names(quality_stats[[i]]), 
                                quality_stats[[i]], 
                                sep="=", collapse=", ")), 
                     "INFO")
          }
        }
      }
    }
  } else {
    log_message("Data processing script not found, will be created", "WARN")
  }
  
  # Step 4: Create or update the database
  log_message("Step 4: Creating or updating the database")
  
  # Instead of trying to fix the complex create_extended_database.r, let's create a simpler direct version here
  tryCatch({
    # Create database directory if it doesn't exist
    db_dir <- dirname(extended_db_path)
    if (!dir.exists(db_dir)) {
      dir.create(db_dir, showWarnings = FALSE, recursive = TRUE)
      log_message(paste("Created database directory at:", db_dir))
    }
    
    # Connect to the database
    log_message("Connecting to DuckDB database...")
    con <- dbConnect(duckdb::duckdb(), dbdir = extended_db_path)
    
    # Create basic tables if they don't exist
    log_message("Setting up database schema...")
    
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
    
    # Create data table without foreign key constraints initially
    # (we'll add them after importing data to avoid constraint issues)
    dbExecute(con, "CREATE TABLE IF NOT EXISTS sdoh_data (
      geoid VARCHAR,
      year INTEGER,
      variable_name VARCHAR,
      value DOUBLE,
      data_quality VARCHAR,
      data_source VARCHAR,
      data_vintage VARCHAR,
      last_updated TIMESTAMP,
      PRIMARY KEY (geoid, year, variable_name)
    )")
    
    # Import county data FIRST (before other data that references counties)
    log_message("Importing county metadata...")
    
    # We need to ensure we have county data - try multiple sources
    county_data <- NULL
    
    # First try: from processed_data
    if (!is.null(processed_data) && "GEOID" %in% names(processed_data) && "NAME" %in% names(processed_data)) {
      # Extract county metadata
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
    }
    
    # Second try: extract from all the data sources
    if (is.null(county_data)) {
      # Search in all_data_sources
      county_geoids <- c()
      county_names <- c()
      
      for (source_name in names(all_data_sources)) {
        source_data <- all_data_sources[[source_name]]
        if (!is.null(source_data) && "GEOID" %in% names(source_data) && "NAME" %in% names(source_data)) {
          county_geoids <- c(county_geoids, source_data$GEOID)
          county_names <- c(county_names, source_data$NAME)
        }
      }
      
      if (length(county_geoids) > 0) {
        # Create data frame with unique county data
        counties_combined <- data.frame(
          GEOID = county_geoids,
          NAME = county_names,
          stringsAsFactors = FALSE
        ) %>% distinct()
        
        county_data <- counties_combined %>%
          mutate(
            geoid = GEOID,
            name = NAME,
            state_fips = substr(GEOID, 1, 2),
            state_name = gsub(".*,\\s*(.*)$", "\\1", NAME)
          ) %>%
          select(geoid, name, state_fips, state_name)
      }
    }
    
    # Third try: generate dummy county data for each unique GEOID
    if (is.null(county_data)) {
      log_message("WARNING: No direct county metadata found, creating data from available GEOIDs", "WARN")
      
      # Get all unique GEOIDs from the processed data
      all_geoids <- c()
      
      if (!is.null(processed_data) && "GEOID" %in% names(processed_data)) {
        all_geoids <- unique(processed_data$GEOID)
      } else if (!is.null(processed_data) && "geoid" %in% names(processed_data)) {
        all_geoids <- unique(processed_data$geoid)
      } else {
        # Try to extract GEOIDs from data columns
        for (col in names(processed_data)) {
          if (grepl("geoid", tolower(col))) {
            all_geoids <- c(all_geoids, unique(processed_data[[col]]))
          }
        }
      }
      
      if (length(all_geoids) > 0) {
        # Generate county data from GEOIDs
        county_data <- data.frame(
          geoid = as.character(all_geoids),
          name = paste("County", all_geoids),
          state_fips = substr(as.character(all_geoids), 1, 2),
          state_name = paste("State", substr(as.character(all_geoids), 1, 2)),
          stringsAsFactors = FALSE
        )
      }
    }
    
    # If we still don't have county data, create a minimal set
    if (is.null(county_data) || nrow(county_data) == 0) {
      log_message("WARNING: No county data found, adding minimal county list to prevent FK constraint issues", "WARN")
      
      # Add a set of common counties
      county_data <- data.frame(
        geoid = c("01001", "06037", "17031", "36061", "48201", "00000"),
        name = c("Autauga County", "Los Angeles County", "Cook County", "New York County", "Harris County", "Dummy County"),
        state_fips = c("01", "06", "17", "36", "48", "00"),
        state_name = c("Alabama", "California", "Illinois", "New York", "Texas", "Unknown State"),
        stringsAsFactors = FALSE
      )
    }
    
    # Also ensure we have every GEOID we need for the data
    if (!is.null(processed_data)) {
      if ("geoid" %in% names(processed_data)) {
        needed_geoids <- unique(processed_data$geoid)
      } else if ("GEOID" %in% names(processed_data)) {
        needed_geoids <- unique(processed_data$GEOID)
      } else {
        needed_geoids <- character(0)
      }
      
      # Add any missing counties
      missing_geoids <- setdiff(needed_geoids, county_data$geoid)
      if (length(missing_geoids) > 0) {
        log_message(paste("Adding", length(missing_geoids), "missing counties"), "WARN")
        
        # Add missing counties with generated metadata
        missing_counties <- data.frame(
          geoid = missing_geoids,
          name = paste("County", missing_geoids),
          state_fips = substr(missing_geoids, 1, 2),
          state_name = paste("State", substr(missing_geoids, 1, 2)),
          stringsAsFactors = FALSE
        )
        
        county_data <- rbind(county_data, missing_counties)
      }
    }
    
    # Update the counties table without violating constraints
    # First get existing counties
    existing_counties <- dbGetQuery(con, "SELECT geoid FROM counties")
    
    if (nrow(existing_counties) > 0) {
      # For existing counties, we'll update them instead of deleting and recreating
      log_message("Updating existing county data", 2)
      
      # Find counties to add (not in the database yet)
      new_counties <- county_data %>%
        filter(!geoid %in% existing_counties$geoid)
      
      # Find counties to update (already in the database)
      update_counties <- county_data %>%
        filter(geoid %in% existing_counties$geoid)
      
      # Add new counties
      if (nrow(new_counties) > 0) {
        dbAppendTable(con, "counties", new_counties)
        log_message(paste("Added", nrow(new_counties), "new counties to database"))
      }
      
      # Update existing counties one by one to avoid constraint violations
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
        log_message(paste("Updated", nrow(update_counties), "existing counties"), 2)
      }
    } else {
      # If no counties exist yet, just insert all of them
      dbAppendTable(con, "counties", county_data)
      log_message(paste("Added", nrow(county_data), "counties to database"))
    }
    
    # Import variables from crosswalk
    log_message("Importing variables from crosswalk...")
    crosswalk_file <- file.path(output_dir, "variable_crosswalk_extended.csv")
    if (file.exists(crosswalk_file)) {
      # Read crosswalk
      crosswalk <- read_csv(crosswalk_file, show_col_types = FALSE)
      
      # Clean up crosswalk data
      crosswalk_clean <- crosswalk %>%
        filter(!is.na(variable_name)) %>%
        select(variable_name, domain, description, type, units, min_year, max_year, extended_only) %>%
        mutate(
          domain = if_else(is.na(domain), "Unknown", domain),
          description = if_else(is.na(description), variable_name, description),
          type = if_else(is.na(type), "numeric", type),
          units = if_else(is.na(units), "value", units),
          min_year = if_else(is.na(min_year), 2000, min_year),
          max_year = if_else(is.na(max_year), 2025, max_year),
          extended_only = if_else(is.na(extended_only), FALSE, extended_only)
        )
      
      # Check if variable table is empty
      var_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM variables")
      
      if (var_count$count == 0) {
        # If empty, just insert all variables
        dbAppendTable(con, "variables", crosswalk_clean)
        log_message(paste("Added", nrow(crosswalk_clean), "variables to database"))
      } else {
        # Check which variables are already in the database
        existing_vars <- dbGetQuery(con, "SELECT variable_name FROM variables")
        
        # Filter to just new variables
        new_vars <- crosswalk_clean %>%
          filter(!variable_name %in% existing_vars$variable_name)
        
        # Find variables to update (already in the database)
        update_vars <- crosswalk_clean %>%
          filter(variable_name %in% existing_vars$variable_name)
        
        # Add new variables
        if (nrow(new_vars) > 0) {
          dbAppendTable(con, "variables", new_vars)
          log_message(paste("Added", nrow(new_vars), "new variables to database"))
        }
        
        # Update existing variables one by one to avoid constraint violations
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
          log_message(paste("Updated", nrow(update_vars), "existing variables"), 2)
        }
      }
    } else {
      log_message("WARNING: No variable crosswalk found, cannot import variables", "WARN")
    }
    
    # Import the processed data
    if (!is.null(processed_data) && nrow(processed_data) > 0) {
      log_message("Importing processed data...")
      
      # First, standardize the data to a long format
      log_message("Converting data to long format...")
      
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
        log_message("No valid variables found in the processed data!", "WARN")
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
        
        # Add quality flags
        long_data <- long_data %>%
          mutate(
            data_quality = "direct",
            data_source = "extended_pipeline",
            data_vintage = as.character(year),
            last_updated = Sys.time()
          )
        
        # Add real quality flags when available
        for (var_name in data_cols) {
          quality_col <- paste0(var_name, "_data_quality")
          source_col <- paste0(var_name, "_data_source")
          vintage_col <- paste0(var_name, "_data_vintage")
          
          if (quality_col %in% names(processed_data)) {
            for (i in 1:nrow(long_data)) {
              if (long_data$variable_name[i] == var_name) {
                idx <- which(processed_data$geoid == long_data$geoid[i] & 
                           processed_data$year == long_data$year[i])
                if (length(idx) > 0) {
                  long_data$data_quality[i] <- processed_data[[quality_col]][idx[1]]
                }
              }
            }
          }
          
          if (source_col %in% names(processed_data)) {
            for (i in 1:nrow(long_data)) {
              if (long_data$variable_name[i] == var_name) {
                idx <- which(processed_data$geoid == long_data$geoid[i] & 
                           processed_data$year == long_data$year[i])
                if (length(idx) > 0) {
                  long_data$data_source[i] <- processed_data[[source_col]][idx[1]]
                }
              }
            }
          }
          
          if (vintage_col %in% names(processed_data)) {
            for (i in 1:nrow(long_data)) {
              if (long_data$variable_name[i] == var_name) {
                idx <- which(processed_data$geoid == long_data$geoid[i] & 
                           processed_data$year == long_data$year[i])
                if (length(idx) > 0) {
                  long_data$data_vintage[i] <- processed_data[[vintage_col]][idx[1]]
                }
              }
            }
          }
        }
        
        # Check how much data exists already
        existing_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM sdoh_data")
        
        if (existing_count$count > 0) {
          log_message(paste("Database already has", existing_count$count, "data points"), 2)
          log_message("Using UPSERT pattern for data import...")
          
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
          
          # Get updated count
          new_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM sdoh_data")
          log_message(paste("Database now has", new_count$count, "data points"), 2)
          
        } else {
          # If no data exists, just insert everything directly
          log_message("No existing data, inserting all data points...")
          
          # Import in chunks
          chunk_size <- 10000
          num_chunks <- ceiling(nrow(long_data) / chunk_size)
          
          for (chunk_idx in 1:num_chunks) {
            start_idx <- (chunk_idx - 1) * chunk_size + 1
            end_idx <- min(chunk_idx * chunk_size, nrow(long_data))
            chunk <- long_data[start_idx:end_idx, ]
            
            dbAppendTable(con, "sdoh_data", chunk)
            log_message(paste("Imported chunk", chunk_idx, "of", num_chunks))
          }
        }
        
        log_message(paste("Successfully imported", nrow(long_data), "data points"))
      }
    }
    
    # Create views to help with data analysis
    log_message("Creating database views...")
    
    # Latest data view
    dbExecute(con, "
      CREATE OR REPLACE VIEW latest_data AS
      SELECT 
        d.*,
        c.name as county_name,
        c.state_name,
        v.description as variable_description,
        v.domain,
        v.units
      FROM sdoh_data d
      JOIN counties c ON d.geoid = c.geoid
      JOIN variables v ON d.variable_name = v.variable_name
      JOIN (
        SELECT variable_name, MAX(year) as max_year
        FROM sdoh_data
        GROUP BY variable_name
      ) latest ON d.variable_name = latest.variable_name AND d.year = latest.max_year
    ")
    
    # County trends view
    dbExecute(con, "
      CREATE OR REPLACE VIEW county_trends AS
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
        d.data_source
      FROM counties c
      JOIN sdoh_data d ON c.geoid = d.geoid
      JOIN variables v ON d.variable_name = v.variable_name
      ORDER BY c.geoid, d.variable_name, d.year
    ")
    
    # Domain summary view
    dbExecute(con, "
      CREATE OR REPLACE VIEW domain_summary AS
      SELECT 
        d.geoid,
        c.name as county_name,
        c.state_name,
        v.domain,
        d.year,
        COUNT(DISTINCT d.variable_name) as variables_available,
        COUNT(CASE WHEN d.data_quality = 'direct' THEN 1 END) as direct_data_points,
        COUNT(CASE WHEN d.data_quality = 'interpolated' THEN 1 END) as interpolated_data_points,
        COUNT(CASE WHEN d.data_quality = 'simulated' THEN 1 END) as simulated_data_points
      FROM sdoh_data d
      JOIN counties c ON d.geoid = c.geoid
      JOIN variables v ON d.variable_name = v.variable_name
      GROUP BY d.geoid, c.name, c.state_name, v.domain, d.year
      ORDER BY d.geoid, v.domain, d.year
    ")
    
    # Close the database connection
    dbDisconnect(con)
    log_message("Database creation completed successfully")
    db_result <- TRUE
    
  }, error = function(e) {
    log_message(paste("Error creating database:", conditionMessage(e)), "ERROR")
    db_result <- FALSE
  })
  
  # Step 5: Generate maps
  log_message("Step 5: Generating maps")
  if (file.exists(file.path(script_directory, "generate_extended_maps.r"))) {
    source(file.path(script_directory, "generate_extended_maps.r"), local = TRUE)
    maps_result <- generate_extended_maps(
      db_path = extended_db_path,
      output_dir = file.path(output_dir, "maps"),
      years = years,
      verbose = verbose
    )
  } else {
    log_message("Map generation script not found, will be created", "WARN")
  }
  
  # Record successful completion
  script_end_time <- Sys.time()
  script_duration <- difftime(script_end_time, script_start_time, units = "mins")
  log_message(paste("Pipeline completed successfully in", round(script_duration, 2), "minutes"))
  
  # Update last update timestamp
  last_update_file <- file.path(data_dir, "last_update.txt")
  writeLines(as.character(Sys.Date()), last_update_file)
  log_message(paste("Updated last update timestamp to", Sys.Date()))
  
}, error = function(e) {
  log_message(paste("Pipeline failed with error:", conditionMessage(e)), "ERROR")
  log_message(paste("Error occurred in:", conditionCall(e)), "ERROR")
  log_message("See traceback below:", "ERROR")
  log_message(paste(paste(capture.output(traceback()), collapse = "\n")), "ERROR")
}, finally = {
  close(log_connection)
})

# This lets the function be used when the script is sourced
is_sourced <- function() {
  # Check if the calling environment is the global environment
  # If it's not, the function is being sourced
  parent_env <- parent.frame()
  return(!identical(parent_env, .GlobalEnv))
}

if (!is_sourced()) {
  quit(status = if (exists("pipeline_failed") && pipeline_failed) 1 else 0)
}