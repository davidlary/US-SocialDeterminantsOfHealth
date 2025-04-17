#!/usr/bin/env Rscript

# Verify that all files have been copied correctly

cat("Verifying file structure...\n")

# Define expected directories
expected_dirs <- c(
  ".", 
  "./utilities", 
  "./extended_sdoh_pipeline",
  "./data",
  "./output"
)

# Define expected core files
expected_files <- c(
  "./README.md",
  "./ml_forecasting.r",
  "./ml_forecasting_examples.md",
  "./unified_sdoh_pipeline.r",
  "./interactive_dashboard.r",
  "./parallel_processor.r",
  "./api_server.r",
  "./test_pipeline.r",
  "./install_packages.r",
  "./main_extended.r",
  "./fetch_county_data_final.r",
  "./fetch_extended_data.r",
  "./fetch_historical_data.r",
  "./fetch_nhgis_data.r",
  "./generate_county_maps.r",
  "./process_extended_data.r",
  "./build_extended_crosswalk.r"
)

# Define expected utility files
expected_util_files <- c(
  "./utilities/fetch_county_shapefiles.r",
  "./utilities/load_ipums_credentials.r",
  "./utilities/run_pipeline_with_temp_key.r",
  "./utilities/set_api_key.r",
  "./utilities/set_ipums_credentials.r", 
  "./utilities/setup_ipums_credentials.r"
)

# Define expected extended pipeline files
expected_extended_files <- c(
  "./extended_sdoh_pipeline/README.md",
  "./extended_sdoh_pipeline/extended_variable_dictionary.md",
  "./extended_sdoh_pipeline/fetch_climate_data.r",
  "./extended_sdoh_pipeline/fetch_substance_use_data.r",
  "./extended_sdoh_pipeline/fetch_digital_access_data.r"
)

# We'll work from the current directory as the project root
# No need to change directory

# Check directories
for (dir in expected_dirs) {
  if (dir.exists(dir)) {
    cat(sprintf("✓ Directory exists: %s\n", dir))
  } else {
    cat(sprintf("✗ Missing directory: %s\n", dir))
  }
}

# Check files
missing_files <- c()
for (file in expected_files) {
  if (file.exists(file)) {
    cat(sprintf("✓ File exists: %s\n", file))
  } else {
    cat(sprintf("✗ Missing file: %s\n", file))
    missing_files <- c(missing_files, file)
  }
}

# Check utility files
for (file in expected_util_files) {
  if (file.exists(file)) {
    cat(sprintf("✓ File exists: %s\n", file))
  } else {
    cat(sprintf("✗ Missing file: %s\n", file))
    missing_files <- c(missing_files, file)
  }
}

# Check extended pipeline files
for (file in expected_extended_files) {
  if (file.exists(file)) {
    cat(sprintf("✓ File exists: %s\n", file))
  } else {
    cat(sprintf("✗ Missing file: %s\n", file))
    missing_files <- c(missing_files, file)
  }
}

# Count the number of fetchers in the extended_sdoh_pipeline directory
extended_fetchers <- list.files("./extended_sdoh_pipeline", pattern = "^fetch_.*\\.r$")
cat(sprintf("Found %d fetcher scripts in extended_sdoh_pipeline directory\n", length(extended_fetchers)))

# Summary
if (length(missing_files) == 0) {
  cat("\nVerification SUCCESS: All expected files are present.\n")
} else {
  cat("\nVerification WARNING: Some expected files are missing.\n")
  cat("Missing files:", paste(missing_files, collapse = ", "), "\n")
}

# Check for test database
if (file.exists("./output/test_sdoh.duckdb")) {
  cat("✓ Test database exists\n")
} else {
  cat("✗ Test database is missing\n")
}

cat("\nFile verification complete.\n")