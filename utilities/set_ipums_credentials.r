#!/usr/bin/env Rscript

# This script sets IPUMS credentials for use with the census_time_series pipeline
# It stores the credentials as environment variables that will be detected by the pipeline

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Check if arguments are provided
if (length(args) < 2) {
  cat("Usage: Rscript set_ipums_credentials.r <username> <password>\n")
  cat("\nThis script sets your IPUMS credentials as environment variables for the pipeline.\n")
  cat("The credentials are stored in the .Renviron file in your home directory.\n")
  quit(status = 1)
}

username <- args[1]
password <- args[2]

# Path to .Renviron file
renviron_path <- file.path(Sys.getenv("HOME"), ".Renviron")

# Check if file exists
if (file.exists(renviron_path)) {
  # Read existing file
  lines <- readLines(renviron_path)
  
  # Remove existing IPUMS variables
  lines <- lines[!grepl("^IPUMS_USERNAME=", lines)]
  lines <- lines[!grepl("^IPUMS_PASSWORD=", lines)]
  
  # Add new credentials
  lines <- c(lines, paste0("IPUMS_USERNAME=", username))
  lines <- c(lines, paste0("IPUMS_PASSWORD=", password))
  
  # Write back to file
  writeLines(lines, renviron_path)
} else {
  # Create new file
  writeLines(c(paste0("IPUMS_USERNAME=", username), 
               paste0("IPUMS_PASSWORD=", password)), 
             renviron_path)
}

cat("IPUMS credentials set successfully!\n")
cat("Username:", username, "\n")
cat("Password: [hidden]\n\n")
cat("The credentials are stored in:", renviron_path, "\n")
cat("They will be used by the census_time_series pipeline to access NHGIS data directly.\n")
cat("Run R with 'Sys.getenv(\"IPUMS_USERNAME\")' to verify settings.\n")
cat("\nNOTE: Restart your R session for these changes to take effect.\n")