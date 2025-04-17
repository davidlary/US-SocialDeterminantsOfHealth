#!/usr/bin/env Rscript

# Script to run the pipeline with a temporary API key
# This avoids the need to install the key permanently

# Set a temporary API key for this session only
# Replace "your_census_api_key_here" with a valid Census API key
# Get one at https://api.census.gov/data/key_signup.html
Sys.setenv(CENSUS_API_KEY = "your_census_api_key_here")

# Verify the key is set
cat("Using Census API key:", Sys.getenv("CENSUS_API_KEY"), "\n")

# Run the main pipeline
cat("Starting pipeline...\n")
source("main_extended.r")

cat("Pipeline execution complete!\n")