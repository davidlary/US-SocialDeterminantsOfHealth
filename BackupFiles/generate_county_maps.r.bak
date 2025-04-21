#!/usr/bin/env Rscript

# NOTE: This script is now a wrapper for the enhanced generate_conus_maps.r script
# It provides backwards compatibility but uses the improved functionality

# Define is_sourced function to avoid conflicts
if (!exists("is_sourced")) {
  is_sourced <- function() {
    # Check if the calling environment is the global environment
    # If it's not, the function is being sourced
    parent_env <- parent.frame()
    return(!identical(parent_env, .GlobalEnv))
  }
}

# Source the enhanced map generation script
source("generate_conus_maps.r")

# Call the new function with parameters that match the old behavior
if (!is_sourced()) {
  generate_conus_maps(
    output_dir = "output/maps",
    db_path = "us_county_sdoh_unified.duckdb",
    conus_only = TRUE
  )
}