#!/usr/bin/env Rscript

# module_database.r
# Database management module for the SDOH pipeline

# Load required packages
library(dplyr)
library(DBI)
library(duckdb)
library(tidyr)

#' Create or update the unified SDOH database
#'
#' This function creates a unified DuckDB database containing all
#' SDOH variables and their data, using a proper normalized schema.
#'
#' @param processed_data Combined dataset with all processed data
#' @param crosswalk The variable crosswalk table
#' @param db_path Path to save the DuckDB database
#' @param overwrite Whether to overwrite an existing database
#' @return TRUE if successful, FALSE otherwise
create_unified_database <- function(processed_data, 
                                  crosswalk, 
                                  db_path = "output/us_county_sdoh_unified.duckdb",
                                  overwrite = FALSE) {
  
  log_message("\nSTEP 4: CREATING UNIFIED DATABASE", 
             level = "INFO", show_console = TRUE)
  
  # Create the database directory if it doesn't exist
  db_dir <- dirname(db_path)
  if (!dir.exists(db_dir)) {
    dir.create(db_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Remove existing database if overwrite is TRUE
  if (file.exists(db_path) && overwrite) {
    file.remove(db_path)
    log_message(paste("Removed existing database:", db_path),
               level = "INFO", show_console = TRUE)
  }
  
  # Get the full path to the database
  unified_db_path <- db_path
  log_message(paste("Creating unified database at:", unified_db_path),
             level = "INFO", show_console = TRUE)
  
  # Try to connect to the database with retry logic
  con <- NULL
  max_attempts <- 3
  attempt <- 1
  
  while (attempt <= max_attempts && is.null(con)) {
    tryCatch({
      # Try to connect to the database
      log_message(paste("Connecting to database (attempt", attempt, "of", max_attempts, ")..."),
                 level = "INFO", show_console = TRUE)
      
      con <- dbConnect(duckdb::duckdb(), dbdir = unified_db_path)
      log_message("Successfully connected to database",
                 level = "INFO", show_console = TRUE)
    }, error = function(e) {
      log_message(paste("Database connection attempt", attempt, "failed:", conditionMessage(e)),
                 level = "WARN", show_console = TRUE)
      
      if (grepl("lock", conditionMessage(e), ignore.case = TRUE)) {
        # It's a lock issue - try a different path
        Sys.sleep(2)  # Wait 2 seconds
        new_path <- gsub("\\.duckdb$", paste0("_alt_", attempt, ".duckdb"), db_path)
        log_message(paste("Trying alternative database path:", new_path),
                   level = "INFO", show_console = TRUE)
        unified_db_path <<- new_path
      }
    })
    
    attempt <- attempt + 1
  }
  
  # If we couldn't connect to the database after all attempts, use in-memory
  if (is.null(con)) {
    log_message("ERROR: Failed to connect to database after multiple attempts. Using in-memory database.",
               level = "ERROR", show_console = TRUE)
    
    # Create an in-memory database as a last resort
    con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  }
  
  # 1. Create counties table
  log_message("Creating counties table...", level = "INFO", show_console = TRUE)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS counties (
      geoid VARCHAR PRIMARY KEY,
      name VARCHAR,
      state_fips VARCHAR,
      state_name VARCHAR
    )
  ")
  
  # Extract unique counties from processed data
  unique_counties <- processed_data %>%
    select(geoid, name, state_fips, state_name) %>%
    distinct()
  
  # Insert counties if they don't exist yet
  if (dbGetQuery(con, "SELECT COUNT(*) FROM counties")[1,1] == 0) {
    dbWriteTable(con, "counties", unique_counties, append = TRUE)
    log_message(paste("Added", nrow(unique_counties), "counties to database"),
               level = "INFO", show_console = TRUE)
  }
  
  # 2. Create variables table
  log_message("Creating variables table...", level = "INFO", show_console = TRUE)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS variables (
      variable_name VARCHAR PRIMARY KEY,
      domain VARCHAR,
      description VARCHAR,
      type VARCHAR,
      units VARCHAR,
      min_year INTEGER,
      max_year INTEGER,
      extended_only BOOLEAN
    )
  ")
  
  # Prepare variables for insertion
  variables_for_db <- crosswalk %>%
    select(variable_name, domain, description, type, units, min_year, max_year, extended_only)
  
  # Clear existing variables first
  dbExecute(con, "DELETE FROM variables")
  
  # Insert the variables
  dbWriteTable(con, "variables", variables_for_db, append = TRUE)
  log_message(paste("Added", nrow(variables_for_db), "variables to database"),
             level = "INFO", show_console = TRUE)
  
  # 3. Create main data table in normalized form (tall/long format)
  log_message("Creating main SDOH data table...", level = "INFO", show_console = TRUE)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS sdoh_data (
      geoid VARCHAR,
      year INTEGER,
      variable_name VARCHAR,
      value DOUBLE,
      data_quality VARCHAR,
      data_source VARCHAR,
      data_vintage VARCHAR,
      interpolation_method VARCHAR,
      ci_lower DOUBLE,
      ci_upper DOUBLE,
      confidence_level DOUBLE,
      last_updated TIMESTAMP,
      PRIMARY KEY (geoid, year, variable_name)
    )
  ")
  
  # Process the data for insertion (convert from wide to long format)
  # First, identify the columns that need to be pivoted (excluding metadata)
  metadata_cols <- c("geoid", "name", "state_fips", "state_name", "year")
  
  # Get all the variable names from the crosswalk
  var_names <- crosswalk$variable_name
  
  # Add interpolation and data quality flag columns if they exist
  flag_patterns <- c("_interpolated$", "_extended$", "data_quality_")
  flag_cols <- character(0)
  
  for (pattern in flag_patterns) {
    pattern_cols <- grep(pattern, names(processed_data), value = TRUE)
    flag_cols <- c(flag_cols, pattern_cols)
  }
  
  # Define columns to be pivoted (variable data columns)
  pivot_cols <- setdiff(names(processed_data), c(metadata_cols, flag_cols))
  pivot_cols <- intersect(pivot_cols, var_names)
  
  # Verify we have variable data to pivot
  if (length(pivot_cols) == 0) {
    stop("No variable data columns found in the processed data for pivoting")
  }
  
  log_message(paste("Converting", length(pivot_cols), "variables to long format..."),
             level = "INFO", show_console = TRUE)
  
  # Batch processing to avoid memory issues
  batch_size <- 50
  total_batches <- ceiling(length(pivot_cols) / batch_size)
  
  # Process in batches
  for (batch in 1:total_batches) {
    start_idx <- (batch - 1) * batch_size + 1
    end_idx <- min(batch * batch_size, length(pivot_cols))
    batch_cols <- pivot_cols[start_idx:end_idx]
    
    log_message(paste("Processing batch", batch, "of", total_batches, 
                     "(variables", start_idx, "to", end_idx, ")"),
               level = "INFO", show_console = TRUE)
    
    # Pivot the batch of variables to long format
    long_data <- processed_data %>%
      select(all_of(c(metadata_cols, batch_cols))) %>%
      pivot_longer(
        cols = all_of(batch_cols),
        names_to = "variable_name",
        values_to = "value"
      ) %>%
      filter(!is.na(value)) # Only keep non-NA values
    
    # Generate timestamp
    long_data$last_updated <- Sys.time()
    
    # Set other columns to NULL for now (will be updated later if flags exist)
    long_data$data_quality <- NA_character_
    long_data$data_source <- NA_character_
    long_data$data_vintage <- NA_character_
    long_data$interpolation_method <- NA_character_
    long_data$ci_lower <- NA_real_
    long_data$ci_upper <- NA_real_
    long_data$confidence_level <- NA_real_
    
    # Check for interpolation flags and add them
    for (var_name in batch_cols) {
      interp_col <- paste0(var_name, "_interpolated")
      if (interp_col %in% names(processed_data)) {
        var_rows <- long_data$variable_name == var_name
        
        # Join interpolation info
        interp_data <- processed_data %>%
          select(geoid, year, !!sym(interp_col)) %>%
          filter(!is.na(!!sym(interp_col)))
        
        if (nrow(interp_data) > 0) {
          # Create a lookup to efficiently update long_data
          interp_lookup <- interp_data %>%
            mutate(
              key = paste(geoid, year, sep = "_"),
              interp_value = !!sym(interp_col)
            )
          
          # Add a key to long_data for the lookup
          long_data <- long_data %>%
            mutate(key = ifelse(variable_name == var_name, 
                              paste(geoid, year, sep = "_"), NA))
          
          # Update interpolation method for matches
          for (i in 1:nrow(interp_lookup)) {
            current_key <- interp_lookup$key[i]
            current_value <- interp_lookup$interp_value[i]
            
            if (current_value) {
              # Update the interpolation method
              long_data$interpolation_method[long_data$key == current_key] <- "linear"
            }
          }
          
          # Remove the temporary key column
          long_data$key <- NULL
        }
      }
    }
    
    # Insert this batch into the database
    dbWriteTable(con, "sdoh_data", long_data, append = TRUE)
    log_message(paste("Inserted", nrow(long_data), "data points for batch", batch),
               level = "INFO", show_console = TRUE)
    
    # Clear the long_data to free memory
    rm(long_data)
    gc()
  }
  
  # Create indices for faster queries
  log_message("Creating database indices for faster queries...",
             level = "INFO", show_console = TRUE)
  
  # Create indices for most common query patterns
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_geoid ON sdoh_data(geoid)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_year ON sdoh_data(year)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_variable ON sdoh_data(variable_name)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_geoid_year ON sdoh_data(geoid, year)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_variable_year ON sdoh_data(variable_name, year)")
  
  # Create a view for easier querying with wide-format compatibility
  log_message("Creating convenient views for data access...",
             level = "INFO", show_console = TRUE)
  
  dbExecute(con, "
    CREATE OR REPLACE VIEW latest_data AS
    SELECT 
      c.geoid,
      c.name,
      c.state_fips,
      c.state_name,
      d.year,
      d.variable_name,
      d.value
    FROM counties c
    JOIN sdoh_data d ON c.geoid = d.geoid
    WHERE (d.geoid, d.variable_name, d.year) IN (
      SELECT geoid, variable_name, MAX(year) 
      FROM sdoh_data 
      GROUP BY geoid, variable_name
    )
  ")
  
  # Create a view for showing data coverage by variable and year
  dbExecute(con, "
    CREATE OR REPLACE VIEW data_coverage AS
    SELECT 
      variable_name,
      year,
      COUNT(*) as county_count,
      (SELECT COUNT(*) FROM counties) as total_counties,
      CAST(COUNT(*) AS FLOAT) / (SELECT COUNT(*) FROM counties) * 100 as coverage_percent
    FROM sdoh_data
    GROUP BY variable_name, year
    ORDER BY variable_name, year
  ")
  
  # Get database stats
  total_rows <- dbGetQuery(con, "SELECT COUNT(*) FROM sdoh_data")[1,1]
  county_count <- dbGetQuery(con, "SELECT COUNT(*) FROM counties")[1,1]
  variable_count <- dbGetQuery(con, "SELECT COUNT(*) FROM variables")[1,1]
  year_count <- dbGetQuery(con, "SELECT COUNT(DISTINCT year) FROM sdoh_data")[1,1]
  min_year <- dbGetQuery(con, "SELECT MIN(year) FROM sdoh_data")[1,1]
  max_year <- dbGetQuery(con, "SELECT MAX(year) FROM sdoh_data")[1,1]
  
  # Disconnect from the database
  dbDisconnect(con)
  
  # Log database creation summary
  log_message("\nUnified database created successfully:",
             level = "INFO", show_console = TRUE)
  log_message(paste(" - Database path:", unified_db_path),
             level = "INFO", show_console = TRUE)
  log_message(paste(" - Total data points:", total_rows),
             level = "INFO", show_console = TRUE)
  log_message(paste(" - Counties:", county_count),
             level = "INFO", show_console = TRUE)
  log_message(paste(" - Variables:", variable_count),
             level = "INFO", show_console = TRUE)
  log_message(paste(" - Year range:", min_year, "to", max_year, 
                   "(", year_count, "unique years)"),
             level = "INFO", show_console = TRUE)
  
  return(TRUE)
}

# Only run if executed directly (not sourced)
if (!exists("is_sourced") || !is_sourced()) {
  message("Database module cannot be run directly. Use the unified pipeline.")
}