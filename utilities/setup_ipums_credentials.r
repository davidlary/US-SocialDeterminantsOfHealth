#!/usr/bin/env Rscript

# This script securely sets up IPUMS credentials in a private local directory
# It creates a credentials file that is not stored in the git repository

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Check if arguments are provided
if (length(args) < 2) {
  cat("Usage: Rscript setup_ipums_credentials.r <username> <password>\n")
  cat("\nThis script securely stores your IPUMS credentials in a private directory.\n")
  cat("Your credentials will be stored in ~/.ipums_credentials/config with secure permissions.\n")
  quit(status = 1)
}

username <- args[1]
password <- args[2]

# Create credentials directory if it doesn't exist
creds_dir <- file.path(Sys.getenv("HOME"), ".ipums_credentials")
if (!dir.exists(creds_dir)) {
  dir.create(creds_dir, recursive = TRUE, showWarnings = FALSE)
  cat("Created credentials directory:", creds_dir, "\n")
  
  # Set directory permissions (700 - only owner can access)
  if (.Platform$OS.type == "unix") {
    system(paste("chmod 700", shQuote(creds_dir)))
    cat("Set directory permissions to 700 (owner access only)\n")
  }
}

# Path to credentials file
config_file <- file.path(creds_dir, "config")

# Create or update the credentials file
credentials_content <- c(
  "# IPUMS credentials for census_time_series pipeline",
  "# DO NOT commit this file to version control",
  paste0("IPUMS_USERNAME=", username),
  paste0("IPUMS_PASSWORD=", password)
)

# Write credentials to file
writeLines(credentials_content, config_file)

# Set file permissions (600 - only owner can read/write)
if (.Platform$OS.type == "unix") {
  system(paste("chmod 600", shQuote(config_file)))
  cat("Set file permissions to 600 (owner read/write only)\n")
}

cat("\nIPUMS credentials stored successfully!\n")
cat("Username:", username, "\n")
cat("Password: [hidden]\n")
cat("Credentials stored in:", config_file, "\n")
cat("\nThese credentials will be used by the pipeline to access NHGIS data directly.\n")
cat("Run the pipeline with: ./run_with_credentials.sh\n")