#!/usr/bin/env Rscript

# Verify All Variables
# This script verifies that all 255 variables in the database are populated 
# with real data, and reports on data quality and coverage.

# Load required packages
required_packages <- c("dplyr", "DBI", "duckdb", "tidyr")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE)
}

# Create a log function
log_message <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat("[", timestamp, "] [", level, "] ", message, "\n", sep = "")
}

log_message("STARTING COMPREHENSIVE VARIABLE VERIFICATION")

# Define database path
db_path <- "output/us_county_sdoh_unified.duckdb"

# Check if database exists
if (!file.exists(db_path)) {
  log_message(paste("Database not found at", db_path), "ERROR")
  stop("Database not found")
}

# Load crosswalk to get all variables
crosswalk_path <- "output/variable_crosswalk_consolidated.csv"
if (!file.exists(crosswalk_path)) {
  log_message(paste("Crosswalk not found at", crosswalk_path), "ERROR")
  stop("Crosswalk not found")
}

crosswalk <- read.csv(crosswalk_path, stringsAsFactors = FALSE)
log_message(paste("Loaded crosswalk with", nrow(crosswalk), "variables"))

# Try to connect to the database
tryCatch({
  # Connect to the database
  log_message("Connecting to database...")
  con <- dbConnect(duckdb(), dbdir = db_path)
  
  # Check if all variables exist in the variables table
  log_message("Checking if all variables exist in the database...")
  vars_in_db <- dbGetQuery(con, "SELECT variable_name FROM variables")$variable_name
  missing_vars <- setdiff(crosswalk$variable_name, vars_in_db)
  
  if (length(missing_vars) > 0) {
    log_message(paste("WARNING:", length(missing_vars), "variables missing from database:"), "WARN")
    for (var in missing_vars[1:min(length(missing_vars), 10)]) {
      log_message(paste(" -", var), "WARN")
    }
    if (length(missing_vars) > 10) {
      log_message(paste(" - ...and", length(missing_vars) - 10, "more"), "WARN")
    }
  } else {
    log_message("All variables exist in the variables table!")
  }
  
  # Get data quality breakdown for all variables
  log_message("Getting data quality breakdown for all variables...")
  
  quality_data <- dbGetQuery(con, "
    SELECT 
      variable_name, 
      data_quality, 
      COUNT(*) as count
    FROM sdoh_data 
    GROUP BY variable_name, data_quality
    ORDER BY variable_name, data_quality
  ")
  
  # Count variables with each data quality type
  quality_summary <- quality_data %>%
    group_by(data_quality) %>%
    summarize(
      variable_count = n_distinct(variable_name),
      total_rows = sum(count)
    )
  
  log_message("Data quality summary:")
  for (i in 1:nrow(quality_summary)) {
    log_message(paste("  -", quality_summary$data_quality[i], ":", 
                     quality_summary$variable_count[i], "variables,",
                     quality_summary$total_rows[i], "total rows"))
  }
  
  # Check for variables with only pending data
  pending_only <- dbGetQuery(con, "
    WITH var_quality_counts AS (
      SELECT 
        variable_name,
        SUM(CASE WHEN data_quality = 'pending' THEN 1 ELSE 0 END) as pending_count,
        COUNT(*) as total_count
      FROM sdoh_data
      GROUP BY variable_name
    )
    SELECT variable_name
    FROM var_quality_counts
    WHERE pending_count = total_count
  ")
  
  if (nrow(pending_only) > 0) {
    log_message(paste("WARNING:", nrow(pending_only), "variables have only pending data:"), "WARN")
    for (var in pending_only$variable_name[1:min(nrow(pending_only), 10)]) {
      log_message(paste(" -", var), "WARN")
    }
    if (nrow(pending_only) > 10) {
      log_message(paste(" - ...and", nrow(pending_only) - 10, "more"), "WARN")
    }
  } else {
    log_message("All variables have at least some direct data!")
  }
  
  # Calculate coverage metrics
  log_message("Calculating coverage metrics...")
  
  # Total expected combinations (counties × years × variables)
  total_counties <- dbGetQuery(con, "SELECT COUNT(*) as count FROM counties")$count
  years_range <- dbGetQuery(con, "SELECT MIN(year) as min_year, MAX(year) as max_year FROM sdoh_data")
  year_count <- years_range$max_year - years_range$min_year + 1
  total_variables <- nrow(crosswalk)
  
  # Maximum possible combinations
  max_combinations <- total_counties * year_count * total_variables
  
  # Actual non-NULL values
  actual_values <- dbGetQuery(con, "
    SELECT COUNT(*) as count
    FROM sdoh_data
    WHERE value IS NOT NULL
  ")$count
  
  # Calculate coverage percentage
  coverage_percentage <- actual_values * 100.0 / max_combinations
  
  log_message(paste("Data coverage metrics:"))
  log_message(paste("  - Counties:", total_counties))
  log_message(paste("  - Year range:", years_range$min_year, "to", years_range$max_year, 
                   "(", year_count, "years )"))
  log_message(paste("  - Variables:", total_variables))
  log_message(paste("  - Maximum possible combinations:", max_combinations))
  log_message(paste("  - Actual non-NULL values:", actual_values))
  log_message(paste("  - Overall coverage:", sprintf("%.2f%%", coverage_percentage)))
  
  # Check traffic safety variables specifically
  log_message("Checking traffic safety variables specifically...")
  
  traffic_safety_vars <- crosswalk %>%
    filter(domain == "Traffic Safety") %>%
    pull(variable_name)
  
  if (length(traffic_safety_vars) > 0) {
    # Get traffic safety data quality
    ts_quality <- dbGetQuery(con, paste0("
      SELECT 
        variable_name, 
        data_quality, 
        COUNT(*) as count
      FROM sdoh_data 
      WHERE variable_name IN ('", paste(traffic_safety_vars, collapse = "', '"), "')
      GROUP BY variable_name, data_quality
      ORDER BY variable_name, data_quality
    "))
    
    # Summarize traffic safety data quality
    ts_summary <- ts_quality %>%
      group_by(data_quality) %>%
      summarize(total_rows = sum(count))
    
    log_message("Traffic safety data quality summary:")
    for (i in 1:nrow(ts_summary)) {
      log_message(paste("  -", ts_summary$data_quality[i], ":", 
                       ts_summary$total_rows[i], "rows"))
    }
    
    # Get non-NULL traffic safety values
    ts_non_null <- dbGetQuery(con, paste0("
      SELECT COUNT(*) as count
      FROM sdoh_data
      WHERE variable_name IN ('", paste(traffic_safety_vars, collapse = "', '"), "')
      AND value IS NOT NULL
    "))$count
    
    # Get total traffic safety rows
    ts_total <- dbGetQuery(con, paste0("
      SELECT COUNT(*) as count
      FROM sdoh_data
      WHERE variable_name IN ('", paste(traffic_safety_vars, collapse = "', '"), "')
    "))$count
    
    log_message(paste("Traffic safety non-NULL values:", ts_non_null, "out of", ts_total,
                     sprintf("(%.2f%%)", ts_non_null * 100.0 / ts_total)))
  } else {
    log_message("No traffic safety variables found in crosswalk", "WARN")
  }
  
  # Close database connection
  dbDisconnect(con, shutdown = TRUE)
  
  log_message("COMPREHENSIVE VARIABLE VERIFICATION COMPLETE")
  
}, error = function(e) {
  log_message(paste("ERROR:", conditionMessage(e)), "ERROR")
  log_message("COMPREHENSIVE VARIABLE VERIFICATION FAILED")
})