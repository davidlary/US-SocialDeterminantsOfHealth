#!/usr/bin/env Rscript

# This script securely loads IPUMS credentials from a private local file
# It's designed to NOT store the credentials in the git repository

# Define the location of the private credentials file
private_credentials_file <- file.path(Sys.getenv("HOME"), ".ipums_credentials/config")

# Function to load credentials and set them as environment variables
load_ipums_credentials <- function() {
  # Check if the credentials file exists
  if (!file.exists(private_credentials_file)) {
    cat("IPUMS credentials file not found at:", private_credentials_file, "\n")
    cat("Credentials will not be available.\n")
    return(FALSE)
  }
  
  # Read the credentials file
  credentials_lines <- readLines(private_credentials_file)
  
  # Parse and set each credential as an environment variable
  for (line in credentials_lines) {
    # Skip empty lines or comments
    if (grepl("^\\s*$", line) || grepl("^\\s*#", line)) next
    
    # Parse key-value pair
    if (grepl("=", line)) {
      key_value <- strsplit(line, "=", fixed = TRUE)[[1]]
      if (length(key_value) >= 2) {
        key <- trimws(key_value[1])
        value <- trimws(paste(key_value[-1], collapse = "="))  # Handle values with = in them
        
        # Set as environment variable
        Sys.setenv(key = key, value = value)
      }
    }
  }
  
  # Verify credentials were loaded
  username_loaded <- Sys.getenv("IPUMS_USERNAME") != ""
  password_loaded <- Sys.getenv("IPUMS_PASSWORD") != ""
  
  if (username_loaded && password_loaded) {
    cat("IPUMS credentials loaded successfully.\n")
    # Mask the password in the output
    masked_username <- Sys.getenv("IPUMS_USERNAME")
    masked_password <- paste0(substr(Sys.getenv("IPUMS_PASSWORD"), 1, 2), 
                             "****", 
                             substr(Sys.getenv("IPUMS_PASSWORD"), 
                                   nchar(Sys.getenv("IPUMS_PASSWORD"))-1, 
                                   nchar(Sys.getenv("IPUMS_PASSWORD"))))
    cat("Using username:", masked_username, "\n")
    cat("Using password:", masked_password, "\n")
    return(TRUE)
  } else {
    cat("Failed to load complete IPUMS credentials.\n")
    return(FALSE)
  }
}

# If this script is run directly, load the credentials
if (!interactive()) {
  load_ipums_credentials()
}

# Export the function for use in other scripts
assign("load_ipums_credentials", load_ipums_credentials, envir = .GlobalEnv)