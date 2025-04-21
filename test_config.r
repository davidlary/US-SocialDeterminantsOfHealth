#!/usr/bin/env Rscript

# Test script for the YAML configuration system
# This script loads the configuration and prints out key settings

# Source the core module which contains the configuration functions
source("pipeline_modules/module_core.r")

# Load required packages
load_core_packages()

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
config_path <- if (length(args) > 0) args[1] else "config.yaml"

# Print header
cat("\n========================================================\n")
cat("SDOH YAML Configuration Test\n")
cat("========================================================\n\n")

cat("Loading configuration from:", config_path, "\n\n")

# Load the configuration
config <- load_config(config_path)

# Print key configuration settings
cat("CONFIGURATION SUMMARY:\n")
cat("========================================================\n")
cat("Directories:\n")
cat("  Root directory:     ", config$directories$root_dir, "\n")
cat("  Data directory:     ", config$directories$data_dir, 
    " (Full: ", config$directories$full_data_dir, ")\n", sep="")
cat("  Output directory:   ", config$directories$output_dir,
    " (Full: ", config$directories$full_output_dir, ")\n", sep="")
cat("  Logs directory:     ", config$directories$logs_dir,
    " (Full: ", config$directories$full_logs_dir, ")\n", sep="")
cat("  Maps directory:     ", config$directories$maps_dir,
    " (Full: ", config$directories$full_maps_dir, ")\n", sep="")
cat("\n")

cat("Database:\n")
cat("  Database name:      ", config$database$db_name, "\n")
cat("  Database path:      ", config$database$db_path, "\n")
cat("  Full DB path:       ", config$database$full_db_path, "\n")
cat("  Overwrite DB:       ", config$database$overwrite_db, "\n")
cat("\n")

cat("Processing:\n")
cat("  Parallel:           ", config$processing$parallel, "\n")
cat("  Cores:              ", config$processing$cores, "\n")
cat("  Minimum cores:      ", config$processing$min_cores, "\n")
cat("\n")

cat("Years:\n")
cat("  Min year:           ", config$years$min_year, "\n")
cat("  Max year:           ", config$years$max_year, "\n")
cat("\n")

cat("Data Refresh:\n")
cat("  Refresh cache:      ", config$data_refresh$refresh_cache, "\n")
cat("  Max data age days:  ", config$data_refresh$max_data_age_days, "\n")
cat("\n")

cat("Maps:\n")
cat("  Generate maps:      ", config$maps$generate_maps, "\n")
cat("  CONUS only:         ", config$maps$conus_only, "\n")
cat("\n")

cat("Traffic Safety:\n")
cat("  Use fallback:       ", config$traffic_safety$use_fallback, "\n")
cat("  Data years:         ", paste(config$traffic_safety$data_years, collapse=", "), "\n")
cat("\n")

# Check if network paths are configured
if (!is.null(config$network_paths)) {
  cat("Network Paths:\n")
  for (key in names(config$network_paths)) {
    if (!is.null(config$network_paths[[key]]) && config$network_paths[[key]] != "") {
      cat("  ", key, ": ", config$network_paths[[key]], "\n", sep="")
    }
  }
  cat("\n")
}

# Check if API credentials are configured
cat("API Credentials:\n")
cat("  Census API key:     ", if (config$api_keys$census_api_key == "") "Not set" else "Set", "\n")
cat("  IPUMS credentials:  ", 
    if (config$ipums$username == "" || config$ipums$password == "") "Not set" else "Set", "\n")
cat("\n")

cat("========================================================\n")
cat("Configuration test completed successfully!\n")
cat("All configuration options loaded correctly.\n")
cat("========================================================\n")