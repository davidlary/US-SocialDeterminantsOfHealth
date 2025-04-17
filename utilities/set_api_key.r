#!/usr/bin/env Rscript

# Script to set up Census API key
# For more information, visit: https://api.census.gov/data/key_signup.html

library(tidycensus)

# You need to replace "your_census_api_key_here" with your actual Census API key
# Sign up for one at https://api.census.gov/data/key_signup.html if you don't have one
census_api_key <- "37631d786612af84f997a3c2ff73390376930c39"

# Save the key for this session
Sys.setenv(CENSUS_API_KEY = census_api_key)

# Save the key for future sessions
census_api_key(census_api_key, install = TRUE)

cat("Census API key successfully set!\n")
cat("Your key has been stored in your .Renviron file for future use.\n")
cat("To verify, you can check if the following command returns your key:\n")
cat("Sys.getenv('CENSUS_API_KEY')\n")