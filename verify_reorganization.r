#!/usr/bin/env Rscript

# This script verifies that the codebase has been properly reorganized

# Function to check if a directory exists
check_dir <- function(dir_path, label = "Directory") {
  if (dir.exists(dir_path)) {
    cat(sprintf("✓ %s exists: %s\n", label, dir_path))
    return(TRUE)
  } else {
    cat(sprintf("✗ %s does not exist: %s\n", label, dir_path))
    return(FALSE)
  }
}

# Function to check if a file exists
check_file <- function(file_path, label = "File") {
  if (file.exists(file_path)) {
    cat(sprintf("✓ %s exists: %s\n", label, file_path))
    return(TRUE)
  } else {
    cat(sprintf("✗ %s does not exist: %s\n", label, file_path))
    return(FALSE)
  }
}

# Function to check if pattern exists in file
check_pattern_in_file <- function(file_path, pattern, label = "Pattern") {
  if (!file.exists(file_path)) {
    cat(sprintf("✗ Cannot check pattern: file does not exist: %s\n", file_path))
    return(FALSE)
  }
  
  content <- readLines(file_path)
  if (any(grepl(pattern, content, fixed = TRUE))) {
    cat(sprintf("✓ %s found in %s\n", label, file_path))
    return(TRUE)
  } else {
    cat(sprintf("✗ %s not found in %s\n", label, file_path))
    return(FALSE)
  }
}

# Get script directory
script_dir <- dirname(commandArgs(trailingOnly = FALSE)[grep("--file=", commandArgs(trailingOnly = FALSE))[1]])
if (script_dir == ".") {
  script_dir <- getwd()
}

cat("Starting verification of codebase reorganization\n")
cat("================================================\n\n")

# Check main directories
cat("Checking main directories...\n")
check_dir(file.path(script_dir, "data"), "Main data directory")
check_dir(file.path(script_dir, "output"), "Output directory")
check_dir(file.path(script_dir, "output", "maps"), "Maps output directory")
check_dir(file.path(script_dir, "logs"), "Logs directory")

# Check data subdirectories
cat("\nChecking data subdirectories...\n")
data_subdirs <- c(
  "built_environment",
  "cache",
  "cdc_places",
  "crime",
  "economic",
  "education",
  "epa",
  "healthcare",
  "housing",
  "ihme",
  "nhgis",
  "seer",
  "shapefiles",
  "social_cohesion",
  "transportation",
  "usda_food_atlas"
)

for (subdir in data_subdirs) {
  check_dir(file.path(script_dir, "data", subdir), paste0("Data subdirectory: ", subdir))
}

# Check that main R scripts are present
cat("\nChecking main R scripts...\n")
main_scripts <- c(
  "unified_sdoh_pipeline.r",
  "main_extended.r",
  "fetch_extended_data.r",
  "process_extended_data.r",
  "build_extended_crosswalk.r",
  "fetch_nhgis_data.r",
  "fetch_historical_data.r",
  "generate_county_maps.r",
  "check_data_quality.r",
  "install_packages.r",
  "verify_files.r"
)

for (script in main_scripts) {
  check_file(file.path(script_dir, script), paste0("Main script: ", script))
}

# Check that extended_sdoh_pipeline scripts are present in main R directory
cat("\nChecking that extended_sdoh_pipeline scripts are present in main R directory...\n")
extended_scripts <- c(
  "build_extended_crosswalk_v2.r",
  "fetch_built_environment_data.r",
  "fetch_climate_data.r",
  "fetch_crime_data.r",
  "fetch_digital_access_data.r",
  "fetch_economic_data.r",
  "fetch_education_data.r",
  "fetch_epa_data.r",
  "fetch_healthcare_data.r",
  "fetch_housing_data.r",
  "fetch_social_cohesion_data.r",
  "fetch_substance_use_data.r",
  "fetch_transportation_data.r",
  "fetch_usda_food_atlas.r",
  "main_extended_v2.r",
  "process_extended_data_v2.r"
)

for (script in extended_scripts) {
  check_file(file.path(script_dir, script), paste0("Extended script: ", script))
}

# Check that the safe is_sourced pattern is used in key files
cat("\nChecking that the safe is_sourced pattern is used in key files...\n")
safe_pattern <- "!exists(\"is_sourced\") || (is.logical(is_sourced) && !is_sourced)"
fetch_files <- list.files(script_dir, pattern = "^fetch_.*\\.r$", full.names = TRUE)

for (file in fetch_files) {
  check_pattern_in_file(file, safe_pattern, "Safe is_sourced pattern")
}

# Check that path references are updated in unified_sdoh_pipeline.r
cat("\nChecking that path references are updated in unified_sdoh_pipeline.r...\n")
check_pattern_in_file(
  file.path(script_dir, "unified_sdoh_pipeline.r"),
  "data_dir <- file.path(root_dir, \"data\")",
  "Correct data directory path"
)

# Verify README is updated
cat("\nVerifying README is updated...\n")
check_pattern_in_file(
  file.path(script_dir, "README.md"),
  "All R code from extended_sdoh_pipeline has been moved to the main R directory",
  "README contains reorganization info"
)

cat("\nVerification complete!\n")