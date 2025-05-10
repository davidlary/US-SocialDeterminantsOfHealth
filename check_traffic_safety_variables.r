#!/usr/bin/env Rscript

# Check Traffic Safety Variables
# This script verifies that all traffic safety variables are properly included in the database

# Load required packages
library(DBI)
library(duckdb)

# Get the command line arguments (optional database path)
args <- commandArgs(trailingOnly = TRUE)
db_path <- if (length(args) > 0) args[1] else "output/us_county_sdoh_unified.duckdb"

# Source the traffic safety integration module to get variable names
source("traffic_safety_integration.r")

# Create a simple log function
log_message <- function(message) {
  timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
  cat(paste(timestamp, message), "\n")
}

# Connect to the database
log_message(paste("Connecting to database:", db_path))
con <- dbConnect(duckdb::duckdb(), dbdir = db_path)

log_message("Checking traffic safety variables in the database...")

# Get all traffic safety variables
traffic_vars <- get_traffic_safety_variable_names()
log_message(paste("Traffic safety module defines", length(traffic_vars), "variables to check"))

# Check which variables exist in the database
var_exists_query <- paste0("
  SELECT variable_name, COUNT(*) as count 
  FROM sdoh_data 
  WHERE variable_name IN ('", paste(traffic_vars, collapse = "', '"), "')
  GROUP BY variable_name
")

var_exists <- dbGetQuery(con, var_exists_query)
log_message(paste("Found", nrow(var_exists), "traffic safety variables in database"))

# Check for missing variables
missing_vars <- setdiff(traffic_vars, var_exists$variable_name)
if (length(missing_vars) > 0) {
  log_message("WARNING: The following traffic safety variables are missing from the database:")
  for (var in missing_vars) {
    log_message(paste(" -", var))
  }
} else {
  log_message("SUCCESS: All traffic safety variables found in the database!")
}

# Check data coverage for each variable
log_message("Checking data coverage for traffic safety variables:")
for (var in traffic_vars) {
  # Skip variables that don't exist at all
  if (var %in% missing_vars) next
  
  # Count non-NULL values
  count_query <- paste0("
    SELECT COUNT(*) as total_count, 
           SUM(CASE WHEN value IS NOT NULL THEN 1 ELSE 0 END) as notnull_count
    FROM sdoh_data
    WHERE variable_name = '", var, "'
  ")
  
  counts <- dbGetQuery(con, count_query)
  coverage_pct <- round(counts$notnull_count[1] / counts$total_count[1] * 100, 2)
  
  # Get years with data
  years_query <- paste0("
    SELECT DISTINCT year
    FROM sdoh_data
    WHERE variable_name = '", var, "'
    AND value IS NOT NULL
    ORDER BY year
  ")
  
  years <- dbGetQuery(con, years_query)
  years_str <- if (nrow(years) > 0) paste(years$year, collapse = ", ") else "NONE"
  
  log_message(sprintf(" - %-30s %6.2f%% coverage (%d/%d entries), Years: %s", 
                      var, coverage_pct, counts$notnull_count[1], 
                      counts$total_count[1], years_str))
}

# Verify data quality indicators for traffic safety variables
log_message("Checking data quality indicators:")

# Sample a few entries to see their data quality flags
quality_query <- paste0("
  SELECT t.variable_name, t.data_quality, COUNT(*) as count
  FROM (
    SELECT 
      s.variable_name,
      s.data_quality
    FROM sdoh_data s
    WHERE s.variable_name IN ('", paste(traffic_vars, collapse = "', '"), "')
  ) t
  GROUP BY t.variable_name, t.data_quality
  ORDER BY t.variable_name, t.data_quality
")

quality_results <- dbGetQuery(con, quality_query)

if (nrow(quality_results) > 0) {
  last_var <- ""
  for (i in 1:nrow(quality_results)) {
    var <- quality_results$variable_name[i]
    
    if (var != last_var) {
      log_message(paste(" -", var, "quality flags:"))
      last_var <- var
    }
    
    quality <- quality_results$data_quality[i]
    count <- quality_results$count[i]
    log_message(sprintf("     %-15s: %d entries", quality, count))
  }
} else {
  log_message("No data quality indicators found for traffic safety variables")
}

# Disconnect from database
dbDisconnect(con)
log_message("Database check complete")