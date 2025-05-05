#!/usr/bin/env Rscript

# Final Verification Script
# This script performs a comprehensive verification of the SDOH pipeline output

# Load required packages
library(DBI)
library(duckdb)
library(dplyr)

# Create a log function
log_message <- function(message) {
  timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
  cat(paste(timestamp, message), "\n")
}

# Define verification functions
verify_database <- function(db_path) {
  log_message("Verifying database...")
  
  if (!file.exists(db_path)) {
    log_message("ERROR: Database file not found!")
    return(FALSE)
  }
  
  con <- dbConnect(duckdb(), dbdir = db_path)
  
  # Verify tables exist
  tables <- dbListTables(con)
  required_tables <- c("counties", "variables", "sdoh_data")
  missing_tables <- setdiff(required_tables, tables)
  
  if (length(missing_tables) > 0) {
    log_message(paste("ERROR: Missing required tables:", paste(missing_tables, collapse=", ")))
    dbDisconnect(con)
    return(FALSE)
  }
  
  # Verify variable count
  variable_count <- dbGetQuery(con, "SELECT COUNT(*) AS count FROM variables")[1,1]
  log_message(paste("Database contains", variable_count, "variables in the variables table"))
  
  if (variable_count < 250) {
    log_message(paste("WARNING: Expected 255 variables, but found only", variable_count))
  }
  
  # Verify data count
  data_count <- dbGetQuery(con, "SELECT COUNT(*) AS count FROM sdoh_data")[1,1]
  log_message(paste("Database contains", data_count, "total data points"))
  
  # Verify distinct variables in data
  distinct_vars <- dbGetQuery(con, "SELECT COUNT(DISTINCT variable_name) AS count FROM sdoh_data")[1,1]
  log_message(paste("Database contains", distinct_vars, "distinct variables with data"))
  
  if (distinct_vars < variable_count) {
    log_message(paste("WARNING:", variable_count - distinct_vars, "variables from variables table are not in sdoh_data"))
    
    # Get the missing variables
    missing_vars_query <- "
      SELECT v.variable_name
      FROM variables v
      LEFT JOIN (
        SELECT DISTINCT variable_name
        FROM sdoh_data
      ) d ON v.variable_name = d.variable_name
      WHERE d.variable_name IS NULL
    "
    missing_vars <- dbGetQuery(con, missing_vars_query)
    log_message("Missing variables:")
    print(missing_vars)
  }
  
  # Verify traffic safety variables
  traffic_vars_query <- "
    SELECT variable_name
    FROM sdoh_data
    WHERE variable_name LIKE '%fatality%' OR variable_name LIKE '%traffic%'
    GROUP BY variable_name
  "
  traffic_vars <- dbGetQuery(con, traffic_vars_query)
  
  if (nrow(traffic_vars) > 0) {
    log_message(paste("Found", nrow(traffic_vars), "traffic safety variables:"))
    for (var in traffic_vars$variable_name) {
      log_message(paste(" -", var))
    }
  } else {
    log_message("WARNING: No traffic safety variables found in database!")
  }
  
  # Verify data by domain
  domain_query <- "
    SELECT v.domain, COUNT(DISTINCT s.variable_name) as var_count
    FROM variables v
    JOIN sdoh_data s ON v.variable_name = s.variable_name
    GROUP BY v.domain
    ORDER BY var_count DESC
  "
  domains <- dbGetQuery(con, domain_query)
  
  log_message("Variables by domain:")
  for (i in 1:nrow(domains)) {
    log_message(sprintf(" - %-25s: %d variables", domains$domain[i], domains$var_count[i]))
  }
  
  # Close database connection
  dbDisconnect(con)
  
  return(TRUE)
}

verify_maps <- function(maps_dir) {
  log_message("Verifying maps...")
  
  if (!dir.exists(maps_dir)) {
    log_message("ERROR: Maps directory not found!")
    return(FALSE)
  }
  
  # Check by_variable directory
  var_dir <- file.path(maps_dir, "by_variable")
  if (!dir.exists(var_dir)) {
    log_message("ERROR: by_variable directory not found!")
    return(FALSE)
  }
  
  # Count PNG files in by_variable directory
  var_maps <- list.files(var_dir, pattern = "\\.png$")
  log_message(paste("Found", length(var_maps), "variable maps"))
  
  if (length(var_maps) < 250) {
    log_message(paste("WARNING: Expected approximately 255 variable maps, but found only", length(var_maps)))
  }
  
  # Check specifically for traffic safety maps
  traffic_maps <- var_maps[grep("fatality|traffic", var_maps, ignore.case = TRUE)]
  if (length(traffic_maps) > 0) {
    log_message(paste("Found", length(traffic_maps), "traffic safety maps"))
  } else {
    log_message("WARNING: No traffic safety maps found!")
  }
  
  return(TRUE)
}

verify_documentation <- function(docs_dir) {
  log_message("Verifying documentation...")
  
  if (!dir.exists(docs_dir)) {
    log_message("ERROR: Documentation directory not found!")
    return(FALSE)
  }
  
  # Check for key documentation files
  required_docs <- c(
    "README.md",
    "DATA_DICTIONARY.md",
    "data_sources/TRAFFIC_SAFETY_DATA.md"
  )
  
  for (doc in required_docs) {
    doc_path <- file.path(docs_dir, doc)
    if (!file.exists(doc_path)) {
      log_message(paste("WARNING: Documentation file not found:", doc))
    } else {
      log_message(paste("Found documentation file:", doc))
      
      # For traffic safety doc, check content
      if (doc == "data_sources/TRAFFIC_SAFETY_DATA.md") {
        content <- readLines(doc_path)
        fatality_mentions <- grep("fatality", content, ignore.case = TRUE)
        if (length(fatality_mentions) > 0) {
          log_message("Traffic safety documentation includes fatality data references")
        } else {
          log_message("WARNING: Traffic safety documentation may be incomplete (no fatality mentions)")
        }
      }
    }
  }
  
  return(TRUE)
}

# Main verification function
run_final_verification <- function(db_path = "output/us_county_sdoh_unified.duckdb", 
                                  maps_dir = "output/maps",
                                  docs_dir = "docs") {
  log_message("=================================================")
  log_message("RUNNING FINAL VERIFICATION OF SDOH PIPELINE OUTPUT")
  log_message("=================================================")
  
  # Verify database
  db_result <- verify_database(db_path)
  
  # Verify maps
  maps_result <- verify_maps(maps_dir)
  
  # Verify documentation
  docs_result <- verify_documentation(docs_dir)
  
  # Summary
  log_message("\n=================================================")
  log_message("VERIFICATION SUMMARY")
  log_message("=================================================")
  log_message(paste("Database Verification:", if(db_result) "PASSED" else "FAILED"))
  log_message(paste("Maps Verification:", if(maps_result) "PASSED" else "FAILED"))
  log_message(paste("Documentation Verification:", if(docs_result) "PASSED" else "FAILED"))
  
  overall_result <- db_result && maps_result && docs_result
  log_message(paste("OVERALL VERIFICATION:", if(overall_result) "PASSED" else "FAILED"))
  
  return(overall_result)
}

# Run verification if script is executed directly
args <- commandArgs(trailingOnly = TRUE)

# Parse arguments
db_path <- "output/us_county_sdoh_unified.duckdb"
maps_dir <- "output/maps"
docs_dir <- "docs"

# Override defaults with command line arguments if provided
if (length(args) >= 1) db_path <- args[1]
if (length(args) >= 2) maps_dir <- args[2]
if (length(args) >= 3) docs_dir <- args[3]

# Run verification
run_final_verification(db_path, maps_dir, docs_dir)