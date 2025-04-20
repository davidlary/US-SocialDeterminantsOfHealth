#!/usr/bin/env Rscript

# module_crosswalk.r
# Variable crosswalk module for the SDOH pipeline

# Load required packages
library(dplyr)
library(readr)
library(here)

# Make sure we have the log_message function
if (!exists("log_message")) {
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
}

# Import the consolidated crosswalk builder
source("consolidate_crosswalks.r")

#' Build and validate the variable crosswalk for the pipeline
#'
#' This function builds the unified crosswalk of all variables and validates
#' that all expected variables are present.
#'
#' @param output_dir Directory to store output files
#' @param force_update Whether to rebuild the crosswalk even if it exists
#' @param verbose Whether to print verbose output
#' @return The validated crosswalk dataframe
build_sdoh_crosswalk <- function(output_dir = "output",
                               force_update = FALSE,
                               verbose = TRUE) {
  
  log_message("STEP 1: BUILDING EXTENDED VARIABLE CROSSWALK", 
             level = "INFO", show_console = TRUE)
  
  # Use the unified crosswalk builder
  crosswalk <- build_unified_crosswalk(
    output_dir = output_dir,
    force_update = force_update,
    verbose = verbose,
    update_documentation = TRUE
  )
  
  # Validate the crosswalk 
  validate_crosswalk(crosswalk)
  
  return(crosswalk)
}

#' Validate the variable crosswalk for required variables
#'
#' Ensures that all core variables needed by the pipeline are present
#' in the crosswalk.
#'
#' @param crosswalk The crosswalk dataframe to validate
#' @return TRUE if validation passes, stops with error otherwise
validate_crosswalk <- function(crosswalk) {
  # Essential variables that must be present
  required_variables <- c(
    "total_population",
    "median_household_income",
    "poverty_rate",
    "unemployment_rate"
  )
  
  # Check if all required variables are present
  missing_vars <- setdiff(required_variables, crosswalk$variable_name)
  
  if (length(missing_vars) > 0) {
    stop(paste("Critical variables missing from crosswalk:", 
               paste(missing_vars, collapse = ", ")))
  }
  
  # Check if we have the expected minimum number of variables
  min_expected_vars <- 150
  if (nrow(crosswalk) < min_expected_vars) {
    warning(paste("Warning: Crosswalk has fewer variables than expected.",
                 "Found:", nrow(crosswalk), 
                 "Expected at least:", min_expected_vars))
  }
  
  log_message(paste("Variable crosswalk validated successfully with", nrow(crosswalk), "variables"),
             level = "INFO", show_console = TRUE)
  
  return(TRUE)
}

# Only run if executed directly (not sourced)
if (!exists("is_sourced") || !is_sourced()) {
  message("Testing crosswalk module...")
  crosswalk <- build_sdoh_crosswalk(force_update = TRUE)
  message(paste("Crosswalk built with", nrow(crosswalk), "variables"))
}