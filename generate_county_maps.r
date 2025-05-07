#!/usr/bin/env Rscript

# NOTE: This script is now a wrapper for the enhanced generate_conus_maps.r script
# It provides backwards compatibility but uses the improved functionality

source("generate_conus_maps.r")

# Call the new function with parameters to include all states and territories
generate_conus_maps(
  output_dir = "output/maps",
  db_path = "output/us_county_sdoh_unified.duckdb",
  conus_only = FALSE  # Include Alaska, Hawaii, and territories
)
