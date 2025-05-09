#!/usr/bin/env Rscript

# NOTE: This script is now a wrapper for the enhanced generate_conus_maps.r script
# It provides backwards compatibility but uses the improved functionality

source("generate_conus_maps.r")

# Call the new function with parameters that match the old behavior
generate_conus_maps(
  output_dir = "output/maps",
  db_path = "us_county_sdoh_data.duckdb",
  conus_only = TRUE
)
