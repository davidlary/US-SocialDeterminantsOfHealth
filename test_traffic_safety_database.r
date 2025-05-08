#!/usr/bin/env Rscript

# test_traffic_safety_database.r
# Simple script to test if traffic safety data is in the database

# Load required packages
if (!require("DBI")) install.packages("DBI")
if (!require("duckdb")) install.packages("duckdb")

library(DBI)
library(duckdb)

# Define database path
db_path <- "output/us_county_sdoh_unified.duckdb"

# Check if database exists
if (!file.exists(db_path)) {
  cat("Database file not found at:", db_path, "\n")
  # Try alternative paths
  alt_paths <- c(
    "us_county_sdoh_unified.duckdb",
    "us_county_sdoh_data.duckdb",
    "output/us_county_sdoh_data.duckdb"
  )
  
  for (path in alt_paths) {
    if (file.exists(path)) {
      cat("Found database at alternative location:", path, "\n")
      db_path <- path
      break
    }
  }
  
  if (!file.exists(db_path)) {
    stop("No database file found. Please run the unified pipeline first.")
  }
}

# Connect to the database
cat("Connecting to database:", db_path, "\n")
con <- dbConnect(duckdb(), db_path)

# Check if sdoh_data table exists
if (dbExistsTable(con, "sdoh_data")) {
  cat("Table sdoh_data exists in the database.\n")
  
  # Check for traffic safety variables
  traffic_query <- "
    SELECT variable_name, COUNT(*) as count
    FROM sdoh_data 
    WHERE variable_name LIKE '%traffic%' OR variable_name LIKE '%fatality%'
    GROUP BY variable_name
  "
  
  traffic_vars <- dbGetQuery(con, traffic_query)
  
  if (nrow(traffic_vars) > 0) {
    cat("\nTraffic safety variables found in the database:\n")
    print(traffic_vars)
    
    # Check if these variables have non-NULL values
    for (i in 1:nrow(traffic_vars)) {
      var_name <- traffic_vars$variable_name[i]
      non_null_query <- sprintf("
        SELECT COUNT(*) as count
        FROM sdoh_data
        WHERE variable_name = '%s' AND value IS NOT NULL
      ", var_name)
      
      non_null_count <- dbGetQuery(con, non_null_query)$count[1]
      cat(sprintf("Variable %s has %d non-NULL values\n", var_name, non_null_count))
    }
    
    cat("\nTEST PASSED: Traffic safety variables are present in the database.\n")
  } else {
    cat("\nERROR: No traffic safety variables found in the database.\n")
    cat("Please run verify_traffic_safety_database.r to fix this issue.\n")
  }
} else {
  cat("ERROR: Table sdoh_data does not exist in the database.\n")
  cat("Please run verify_traffic_safety_database.r to fix this issue.\n")
}

# Close database connection
dbDisconnect(con)