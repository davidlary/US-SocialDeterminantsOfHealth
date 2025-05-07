#!/usr/bin/env Rscript

# Simple Traffic Safety Variable Verification Script
# This script checks if traffic safety variables are present in the database

# Load required packages
library(dplyr)
library(DBI)
library(duckdb)

# Create log function
log_message <- function(message) {
  cat(paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", message, "\n"))
}

log_message("VERIFYING TRAFFIC SAFETY VARIABLES")

# Load configuration
config <- yaml::read_yaml("config.yaml")
db_path <- config$database$db_path

# Check if database exists
if (!file.exists(db_path)) {
  log_message(paste("ERROR: Database not found at", db_path))
  quit(status = 1)
}

# Get list of traffic safety variables
traffic_safety_vars <- c(
  "traffic_fatalities", "traffic_fatality_rate",
  "pedestrian_fatalities", "pedestrian_fatality_rate",
  "bicycle_fatalities", "bicycle_fatality_rate",
  "motorcycle_fatalities", "motorcycle_fatality_rate",
  "alcohol_impaired_fatalities", "alcohol_impaired_fatality_rate",
  "speeding_related_fatalities", "speeding_related_fatality_rate"
)

# Connect to database
log_message(paste("Connecting to database at", db_path))
con <- dbConnect(duckdb(), dbdir = db_path)

# Check if traffic_safety variables exist in the database
vars_query <- "SELECT variable_name FROM variables"
vars_in_db <- dbGetQuery(con, vars_query)$variable_name

# Check which traffic safety variables are in the database
present_vars <- intersect(traffic_safety_vars, vars_in_db)
missing_vars <- setdiff(traffic_safety_vars, vars_in_db)

log_message(paste("Found", length(present_vars), "of", length(traffic_safety_vars), "traffic safety variables in database:"))
for (var in present_vars) {
  log_message(paste(" -", var))
}

if (length(missing_vars) > 0) {
  log_message(paste("WARNING:", length(missing_vars), "traffic safety variables are missing:"))
  for (var in missing_vars) {
    log_message(paste(" -", var))
  }
}

# For variables that are present, check if they have any data
if (length(present_vars) > 0) {
  log_message("\nChecking if traffic safety variables have actual data...")
  
  for (var in present_vars) {
    # Get count by data quality
    query <- paste0("
      SELECT data_quality, COUNT(*) as count
      FROM sdoh_data
      WHERE variable_name = '", var, "'
      GROUP BY data_quality
    ")
    
    data_quality <- dbGetQuery(con, query)
    
    # Check for non-NULL values
    non_null_query <- paste0("
      SELECT COUNT(*) as count
      FROM sdoh_data
      WHERE variable_name = '", var, "' AND value IS NOT NULL
    ")
    
    non_null_count <- dbGetQuery(con, non_null_query)$count
    
    log_message(paste("Variable:", var))
    if (nrow(data_quality) > 0) {
      for (i in 1:nrow(data_quality)) {
        log_message(paste("  -", data_quality$data_quality[i], ":", data_quality$count[i], "rows"))
      }
    }
    log_message(paste("  - Non-NULL values:", non_null_count))
  }
}

# Close connection
dbDisconnect(con, shutdown = TRUE)

log_message("VERIFICATION COMPLETE")