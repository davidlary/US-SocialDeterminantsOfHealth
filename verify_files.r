#!/usr/bin/env Rscript

# Verify that all files have been copied correctly

cat("Verifying file structure...\n")

# Define expected directories
expected_dirs <- c(
  ".", 
  "./utilities", 
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
  "./build_extended_crosswalk.r",
  "./fetch_built_environment_data.r",
  "./fetch_climate_data.r",
  "./fetch_crime_data.r",
  "./fetch_digital_access_data.r",
  "./fetch_economic_data.r", 
  "./fetch_education_data.r",
  "./fetch_epa_data.r",
  "./fetch_healthcare_data.r",
  "./fetch_housing_data.r",
  "./fetch_social_cohesion_data.r",
  "./fetch_substance_use_data.r",
  "./fetch_transportation_data.r",
  "./fetch_usda_food_atlas.r",
  "./main_extended_v2.r",
  "./process_extended_data_v2.r",
  "./build_extended_crosswalk_v2.r"
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

# Define expected data folder files
expected_data_files <- c(
  "./data/README.md",
  "./data/extended_variable_dictionary.md"
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

# Check data folder files
for (file in expected_data_files) {
  if (file.exists(file)) {
    cat(sprintf("✓ File exists: %s\n", file))
  } else {
    cat(sprintf("✗ Missing file: %s\n", file))
    missing_files <- c(missing_files, file)
  }
}

# Count the number of fetchers in the R directory
fetchers <- list.files(".", pattern = "^fetch_.*\\.r$")
cat(sprintf("Found %d fetcher scripts in R directory\n", length(fetchers)))

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