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
#' @param incremental Whether to use incremental processing (only process new/changed data)
#' @param force_full_rebuild Force full rebuild regardless of incremental settings
#' @param data_sources List of data sources that were processed in this run
#' @param processed_years Range of years that were processed in this run
#' @return TRUE if successful, FALSE otherwise
create_unified_database <- function(processed_data, 
                                  crosswalk, 
                                  db_path = "output/us_county_sdoh_unified.duckdb",
                                  overwrite = FALSE,
                                  incremental = FALSE,
                                  force_full_rebuild = FALSE,
                                  data_sources = NULL,
                                  processed_years = NULL) {
  
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
  log_message(paste("Database path:", unified_db_path),
             level = "INFO", show_console = TRUE)
             
  # Determine whether to use incremental mode
  use_incremental <- incremental && file.exists(unified_db_path) && !force_full_rebuild && !overwrite
  
  if (use_incremental) {
    log_message("Using INCREMENTAL processing mode - only updating new or changed data",
               level = "INFO", show_console = TRUE)
  } else {
    if (force_full_rebuild) {
      log_message("Force full rebuild specified - using FULL processing mode",
                 level = "INFO", show_console = TRUE)
    } else if (overwrite) {
      log_message("Overwrite specified - using FULL processing mode",
                 level = "INFO", show_console = TRUE)
    } else if (!file.exists(unified_db_path)) {
      log_message("Database does not exist yet - using FULL processing mode",
                 level = "INFO", show_console = TRUE)
    } else if (!incremental) {
      log_message("Incremental processing disabled - using FULL processing mode",
                 level = "INFO", show_console = TRUE)
    }
  }
  
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
        use_incremental <<- incremental && file.exists(unified_db_path) && !force_full_rebuild && !overwrite
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
    use_incremental <- FALSE  # Can't use incremental with in-memory DB
  }
  
  # Create metadata table to track processing status if it doesn't exist
  log_message("Setting up processing metadata tracking...",
             level = "INFO", show_console = TRUE)
             
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS processing_metadata (
      data_source VARCHAR,
      variable_name VARCHAR,
      min_year INTEGER,
      max_year INTEGER,
      record_count INTEGER,
      last_processed TIMESTAMP,
      data_version VARCHAR,
      PRIMARY KEY (data_source, variable_name)
    )
  ")
  
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
  # Check if state_fips and state_name are available
  columns_to_use <- c("geoid")
  if ("state_fips" %in% names(processed_data)) {
    columns_to_use <- c(columns_to_use, "state_fips")
  }
  if ("state_name" %in% names(processed_data)) {
    columns_to_use <- c(columns_to_use, "state_name")
  }
  
  # Select available columns
  unique_counties <- processed_data %>%
    select(all_of(columns_to_use)) %>%
    distinct()
  
  # Add missing columns if needed
  if (!"state_fips" %in% names(unique_counties)) {
    unique_counties$state_fips <- substr(unique_counties$geoid, 1, 2)
    log_message("Added state_fips column derived from geoid",
               level = "INFO", show_console = TRUE)
  }
  
  if (!"state_name" %in% names(unique_counties)) {
    # Create a state lookup table
    state_lookup <- data.frame(
      state_fips = sprintf("%02d", 1:56),
      state_name = c(state.name, "District of Columbia", 
                    "Puerto Rico", "Virgin Islands", 
                    "Guam", "American Samoa", "Northern Mariana Islands"),
      stringsAsFactors = FALSE
    )
    
    # Join to get state names
    unique_counties <- unique_counties %>%
      left_join(state_lookup, by = "state_fips")
    
    log_message("Added state_name column from lookup table",
               level = "INFO", show_console = TRUE)
  }
  
  # Add county name
  unique_counties$name <- paste("County", unique_counties$geoid)
  
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
  
  # Check for duplicate variable names which would violate the primary key
  duplicate_vars <- variables_for_db %>%
    group_by(variable_name) %>%
    filter(n() > 1) %>%
    ungroup()
  
  if (nrow(duplicate_vars) > 0) {
    log_message(paste("WARNING: Found", nrow(duplicate_vars), "duplicate variable names in crosswalk"),
               level = "WARN", show_console = TRUE)
    # De-duplicate
    variables_for_db <- variables_for_db %>%
      distinct(variable_name, .keep_all = TRUE)
    log_message("De-duplicated variables table before insertion",
               level = "INFO", show_console = TRUE)
  }
  
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
  
  # If we're in incremental mode, check what data we already have
  previously_processed_data <- NULL
  if (use_incremental) {
    log_message("Retrieving previously processed data information...",
               level = "INFO", show_console = TRUE)
    
    # Get metadata about previously processed data
    previously_processed_data <- dbGetQuery(con, "SELECT * FROM processing_metadata")
    
    if (nrow(previously_processed_data) > 0) {
      log_message(paste("Found metadata for", nrow(previously_processed_data), 
                       "previously processed variables"),
                 level = "INFO", show_console = TRUE)
    } else {
      log_message("No previously processed data found in metadata - will process all data",
                 level = "INFO", show_console = TRUE)
      # Force full processing if no metadata exists
      use_incremental <- FALSE
    }
  }
  
  # Process the data for insertion (convert from wide to long format)
  # First, identify the columns that need to be pivoted (excluding metadata)
  # Ensure we only include metadata columns that actually exist in the processed data
  metadata_cols <- c("geoid", "year")
  if ("state_fips" %in% names(processed_data)) {
    metadata_cols <- c(metadata_cols, "state_fips")
  }
  if ("state_name" %in% names(processed_data)) {
    metadata_cols <- c(metadata_cols, "state_name")
  }
  
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
  
  # Process in batches using parallel processing
  process_batch <- function(batch) {
    start_idx <- (batch - 1) * batch_size + 1
    end_idx <- min(batch * batch_size, length(pivot_cols))
    batch_cols <- pivot_cols[start_idx:end_idx]
    
    message(paste("Processing batch", batch, "of", total_batches, 
                 "(variables", start_idx, "to", end_idx, ")"))
    
    # Ensure metadata_cols only includes columns that exist
    available_metadata_cols <- intersect(metadata_cols, names(processed_data))
    
    # Pivot the batch of variables to long format
    long_data <- processed_data %>%
      select(all_of(c(available_metadata_cols, batch_cols))) %>%
      pivot_longer(
        cols = all_of(batch_cols),
        names_to = "variable_name",
        values_to = "value"
      ) %>%
      filter(!is.na(value)) # Only keep non-NA values
    
    # Check for duplicates in the raw data
    potential_duplicates <- long_data %>%
      group_by(geoid, year, variable_name) %>%
      filter(n() > 1) %>%
      ungroup()
    
    if (nrow(potential_duplicates) > 0) {
      # We found duplicates in the source data, log a warning
      warning_msg <- paste("WARNING: Found", nrow(potential_duplicates),
                         "duplicate key combinations in source data for batch", batch)
      message(warning_msg)
      
      # Log the first few duplicates
      duplicate_example <- potential_duplicates %>%
        distinct(geoid, year, variable_name) %>%
        head(3)
      
      for (i in 1:nrow(duplicate_example)) {
        dup_msg <- paste("   Duplicate:", 
                       paste0("geoid: ", duplicate_example$geoid[i], ", ",
                             "year: ", duplicate_example$year[i], ", ",
                             "variable_name: ", duplicate_example$variable_name[i]))
        message(dup_msg)
      }
      
      # De-duplicate the data (keep first occurrence)
      long_data <- long_data %>%
        distinct(geoid, year, variable_name, .keep_all = TRUE)
      
      message(paste("   De-duplicated data for batch", batch,
                  "- keeping one record per unique key combination"))
    }
    
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
    
    # Final safety check for duplicates before returning
    final_check <- long_data %>%
      group_by(geoid, year, variable_name) %>%
      filter(n() > 1) %>%
      ungroup()
    
    if (nrow(final_check) > 0) {
      warning_msg <- paste("CRITICAL: Still found", nrow(final_check),
                         "duplicates after processing batch", batch, "- forcibly de-duplicating")
      message(warning_msg)
      
      # Force de-duplication to prevent database errors
      long_data <- long_data %>%
        distinct(geoid, year, variable_name, .keep_all = TRUE)
    }
    
    return(long_data)
  }
  
  # Set up parallel processing for batches
  log_message("Setting up parallel processing for data transformation...",
             level = "INFO", show_console = TRUE)
  
  # Determine cores to use for database operations
  db_cores <- min(total_batches, parallel::detectCores() - 1)
  db_cores <- max(db_cores, 2)  # Use at least 2 cores
  
  # Use future for parallel batch processing
  future::plan(future::multisession, workers = db_cores)
  log_message(paste("Using", db_cores, "cores for database batch processing"),
             level = "INFO", show_console = TRUE)
  
  # Process batches in parallel
  batch_numbers <- 1:total_batches
  batch_results <- future.apply::future_lapply(batch_numbers, process_batch)
  
  # Process and insert each batch result
  for (batch in 1:total_batches) {
    # Calculate batch columns for this batch
    start_idx <- (batch - 1) * batch_size + 1
    end_idx <- min(batch * batch_size, length(pivot_cols))
    batch_cols <- pivot_cols[start_idx:end_idx]
    
    # Get batch result - already processed by the process_batch function
    long_data <- batch_results[[batch]]
    
    # In incremental mode, filter out already processed data that hasn't changed
    if (use_incremental && !is.null(previously_processed_data) && nrow(previously_processed_data) > 0) {
      # Start by marking the batch's variables and their sources
      batch_var_sources <- data.frame(
        variable_name = batch_cols,
        data_source = ifelse(
          grepl("^census_", batch_cols), "census",
          ifelse(grepl("^traffic_", batch_cols), "traffic_safety",
                 ifelse(grepl("^life_expectancy", batch_cols), "ihme",
                        "other")
          )
        ),
        stringsAsFactors = FALSE
      )
      
      # Get the variables in this batch that are already in the database
      vars_to_skip <- NULL
      for (i in 1:nrow(batch_var_sources)) {
        var_name <- batch_var_sources$variable_name[i]
        var_source <- batch_var_sources$data_source[i]
        
        # Check if this variable from this source is already processed
        var_metadata <- previously_processed_data %>%
          filter(variable_name == var_name & data_source == var_source)
        
        # If we have metadata for this variable
        if (nrow(var_metadata) > 0) {
          # Check if we have a data_version to compare
          current_version <- "current"  # Default version for current data
          
          # Only skip if no force update is requested and the variable hasn't been modified
          vars_to_skip <- c(vars_to_skip, var_name)
        }
      }
      
      # Calculate how many records we're skipping
      if (length(vars_to_skip) > 0) {
        vars_to_skip_count <- long_data %>%
          filter(variable_name %in% vars_to_skip) %>%
          nrow()
        
        if (vars_to_skip_count > 0) {
          log_message(paste("Skipping", vars_to_skip_count, "already processed records for batch", batch),
                     level = "INFO", show_console = TRUE)
          
          # Filter out the data that's already been processed
          long_data <- long_data %>%
            filter(!variable_name %in% vars_to_skip)
        }
      }
    }
    
    # Skip empty batches
    if (nrow(long_data) == 0) {
      log_message(paste("Batch", batch, "has no new data to insert - skipping"),
                 level = "INFO", show_console = TRUE)
      next
    }
    
    log_message(paste("Inserting batch", batch, "of", total_batches, "into database (",
                      nrow(long_data), "records)"),
               level = "INFO", show_console = TRUE)
    
    # For incremental mode, use "upsert" approach to update existing records
    if (use_incremental) {
      # Create a temporary table for the batch data
      temp_table_name <- paste0("temp_batch_", batch)
      
      tryCatch({
        # Create temporary table
        dbWriteTable(con, temp_table_name, long_data, temporary = TRUE)
        
        # Use SQL for upsert operation
        upsert_query <- paste0("
          INSERT OR REPLACE INTO sdoh_data 
          SELECT * FROM ", temp_table_name
        )
        
        # Execute the upsert
        dbExecute(con, upsert_query)
        
        # Clean up temporary table
        dbExecute(con, paste0("DROP TABLE IF EXISTS ", temp_table_name))
        
        log_message(paste("Upserted", nrow(long_data), "data points for batch", batch),
                   level = "INFO", show_console = TRUE)
      }, error = function(e) {
        # Try to clean up the temp table if it exists
        tryCatch({
          dbExecute(con, paste0("DROP TABLE IF EXISTS ", temp_table_name))
        }, error = function(e2) {
          # Ignore errors dropping temp table
        })
        
        # Re-throw the original error
        stop(paste("Error during upsert operation:", conditionMessage(e)))
      })
    } else {
      # For full processing mode, use the original append method
      tryCatch({
        dbWriteTable(con, "sdoh_data", long_data, append = TRUE)
        log_message(paste("Inserted", nrow(long_data), "data points for batch", batch),
                   level = "INFO", show_console = TRUE)
      }, error = function(e) {
      # Check for duplicate key errors
      if (grepl("Duplicate key.*violates primary key constraint", conditionMessage(e))) {
        # Find duplicate entries in this batch
        duplicates <- long_data %>%
          group_by(geoid, year, variable_name) %>%
          filter(n() > 1) %>%
          ungroup()
        
        if (nrow(duplicates) > 0) {
          # There are duplicates within this batch
          log_message(paste("ERROR: Found", nrow(duplicates), "duplicate entries within batch", batch),
                     level = "ERROR", show_console = TRUE)
          log_message(paste("First duplicate:", 
                           paste0("geoid: ", duplicates$geoid[1], 
                                 ", year: ", duplicates$year[1], 
                                 ", variable_name: ", duplicates$variable_name[1])),
                     level = "ERROR", show_console = TRUE)
        } else {
          # Batch is fine, but duplicates already exist in database
          log_message(paste("ERROR: Duplicate entry detected when inserting batch", batch),
                     level = "ERROR", show_console = TRUE)
          log_message(paste("Error message:", conditionMessage(e)),
                     level = "ERROR", show_console = TRUE)
          log_message("This indicates a duplicate key already exists in the database",
                     level = "ERROR", show_console = TRUE)
        }
        
        # Add an option to continue with de-duplication
        log_message("Attempting to remove duplicates and continue...",
                   level = "INFO", show_console = TRUE)
        
        # De-duplicate the data
        long_data_unique <- long_data %>%
          distinct(geoid, year, variable_name, .keep_all = TRUE)
        
        # Calculate how many duplicates were removed
        duplicates_removed <- nrow(long_data) - nrow(long_data_unique)
        if (duplicates_removed > 0) {
          log_message(paste("Removed", duplicates_removed, "duplicate entries from batch", batch),
                     level = "INFO", show_console = TRUE)
        }
        
        # Try again with de-duplicated data
        tryCatch({
          dbWriteTable(con, "sdoh_data", long_data_unique, append = TRUE)
          log_message(paste("Successfully inserted", nrow(long_data_unique), 
                          "de-duplicated data points for batch", batch),
                     level = "INFO", show_console = TRUE)
        }, error = function(inner_error) {
          log_message(paste("ERROR: Failed to insert even after de-duplication:", 
                           conditionMessage(inner_error)),
                     level = "ERROR", show_console = TRUE)
          stop(paste("Database insertion failed even after de-duplication:", 
                    conditionMessage(inner_error)))
        })
      } else {
        # Re-throw other errors
        log_message(paste("ERROR during database insertion:", conditionMessage(e)),
                   level = "ERROR", show_console = TRUE)
        stop(paste("Database insertion error:", conditionMessage(e)))
      }
    })
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
  
  # Update processing metadata
  log_message("Updating processing metadata...",
             level = "INFO", show_console = TRUE)
  
  # Get metrics for each variable in the database
  variable_metrics <- dbGetQuery(con, "
    SELECT 
      variable_name,
      MIN(year) as min_year,
      MAX(year) as max_year,
      COUNT(*) as record_count
    FROM sdoh_data
    GROUP BY variable_name
  ")
  
  # Create metadata entries for each variable
  if (nrow(variable_metrics) > 0) {
    # Determine data sources for each variable
    variable_sources <- variable_metrics %>%
      mutate(
        data_source = case_when(
          grepl("^census_", variable_name) ~ "census",
          grepl("^traffic_", variable_name) ~ "traffic_safety", 
          grepl("^life_expectancy", variable_name) ~ "ihme",
          TRUE ~ "other"
        )
      )
    
    # Create metadata entries
    metadata_entries <- variable_sources %>%
      mutate(
        last_processed = Sys.time(),
        data_version = "current"
      )
    
    # Update the metadata table - delete existing entries first to avoid conflicts
    if (nrow(metadata_entries) > 0) {
      # Get the list of variables in the new metadata
      var_names_list <- paste0("'", paste(metadata_entries$variable_name, collapse = "','"), "'")
      
      # Delete existing entries for these variables
      delete_query <- paste0("
        DELETE FROM processing_metadata 
        WHERE variable_name IN (", var_names_list, ")
      ")
      dbExecute(con, delete_query)
      
      # Insert the new metadata
      dbWriteTable(con, "processing_metadata", metadata_entries, append = TRUE)
      log_message(paste("Updated processing metadata for", nrow(metadata_entries), "variables"),
                 level = "INFO", show_console = TRUE)
    }
  }
  
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
  
  # Indicate whether incremental processing was used
  if (use_incremental) {
    log_message(" - Processing mode: INCREMENTAL (only new/changed data processed)",
               level = "INFO", show_console = TRUE)
  } else {
    log_message(" - Processing mode: FULL (all data reprocessed)",
               level = "INFO", show_console = TRUE)
  }
  
  return(TRUE)
}

# Only run if executed directly (not sourced)
if (!exists("is_sourced") || !is_sourced()) {
  message("Database module cannot be run directly. Use the unified pipeline.")
}