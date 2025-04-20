#!/usr/bin/env Rscript

# module_maps.r
# Map generation module for the SDOH pipeline

# Load required packages
library(dplyr)
library(sf)

#' Generate maps for the SDOH data
#'
#' This function handles map generation for the SDOH data,
#' using the CONUS maps generator.
#'
#' @param db_path Path to the DuckDB database
#' @param output_dir Directory to store output maps
#' @param conus_only Whether to limit maps to continental US
#' @param parallel Whether to use parallel processing
#' @param cores Number of cores to use for parallel processing
#' @return TRUE if successful, FALSE otherwise
generate_sdoh_maps <- function(db_path = "output/us_county_sdoh_unified.duckdb",
                             output_dir = "output/maps",
                             conus_only = TRUE,
                             parallel = FALSE,
                             cores = 2) {
  
  log_message("\nSTEP 6: GENERATING CONUS MAPS FOR ALL VARIABLES",
             level = "INFO", show_console = TRUE)
  
  # Source the map generation script
  source("generate_conus_maps.r")
  
  # Generate maps for all variables and years
  map_result <- tryCatch({
    generate_conus_maps(
      output_dir = output_dir,
      db_path = db_path,
      conus_only = conus_only,
      parallel = parallel,
      cores = cores
    )
    TRUE
  }, error = function(e) {
    log_message(paste("ERROR: Improved map generation failed:", conditionMessage(e)), 
                level = "ERROR", show_console = TRUE)
    FALSE
  })
  
  if (map_result) {
    log_message("Maps successfully generated", level = "INFO", show_console = TRUE)
  } else {
    log_message("Map generation encountered errors", level = "WARN", show_console = TRUE)
  }
  
  return(map_result)
}

# Only run if executed directly (not sourced)
if (!exists("is_sourced") || !is_sourced()) {
  message("Map generation cannot be run directly. Use the unified pipeline.")
}