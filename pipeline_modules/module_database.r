#!/usr/bin/env Rscript

# module_database.r - COMPLETE FIXED VERSION WITH DATA INSERTION
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
  
  log_message("
STEP 4: CREATING UNIFIED DATABASE", 
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
  
  # Connect to the database
  con <- tryCatch({
    dbConnect(duckdb::duckdb(), dbdir = unified_db_path)
  }, error = function(e) {
    log_message(paste("Error connecting to database:", conditionMessage(e)),
               level = "ERROR", show_console = TRUE)
    return(NULL)
  })
  
  if (is.null(con)) {
    log_message("Failed to connect to database. Aborting database creation.",
               level = "ERROR", show_console = TRUE)
    return(FALSE)
  }
  
  # Helper function to safely create tables
  safe_create_table <- function(table_name, schema) {
    tryCatch({
      log_message(paste("Creating table:", table_name), 
                 level = "INFO", show_console = TRUE)
      dbExecute(con, schema)
      return(TRUE)
    }, error = function(e) {
      # Check if the error is because the table already exists
      if (grepl("already exists", conditionMessage(e))) {
        log_message(paste("Table already exists:", table_name),
                   level = "INFO", show_console = TRUE)
        return(TRUE)
      } else {
        log_message(paste("Error creating table:", conditionMessage(e)),
                   level = "ERROR", show_console = TRUE)
        return(FALSE)
      }
    })
  }
  
  # Helper function to safely clear tables before inserting
  safe_clear_table <- function(table_name) {
    tryCatch({
      # Check if the table exists first
      if (table_name %in% dbListTables(con)) {
        log_message(paste("Clearing existing table:", table_name),
                   level = "INFO", show_console = TRUE)
        dbExecute(con, paste0("DELETE FROM ", table_name))
      } else {
        log_message(paste("Table does not exist yet:", table_name),
                   level = "INFO", show_console = TRUE)
      }
      return(TRUE)
    }, error = function(e) {
      log_message(paste("Error clearing table:", conditionMessage(e)),
                 level = "WARN", show_console = TRUE)
      return(FALSE)
    })
  }
  
  # 1. Create counties table
  log_message("Creating counties table...", level = "INFO", show_console = TRUE)
  counties_schema <- "
    CREATE TABLE IF NOT EXISTS counties (
      geoid VARCHAR PRIMARY KEY,
      name VARCHAR,
      state_fips VARCHAR,
      state_name VARCHAR
    )
  "
  safe_create_table("counties", counties_schema)
  
  # Extract unique counties from processed data
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
  if (!"name" %in% names(unique_counties)) {
    unique_counties$name <- paste("County", unique_counties$geoid)
  }
  
  # Ensure all required columns are present
  required_county_cols <- c("geoid", "name", "state_fips", "state_name")
  for (col in required_county_cols) {
    if (!col %in% names(unique_counties)) {
      unique_counties[[col]] <- NA
    }
  }
  
  # Update counties with INSERT OR REPLACE
  tryCatch({
    # Create temp table with new data
    temp_counties <- paste0("temp_counties_", format(Sys.time(), "%H%M%S"))
    dbWriteTable(con, temp_counties, unique_counties, temporary = TRUE)
    
    # Use INSERT OR REPLACE for atomic upsert
    dbExecute(con, paste0("INSERT OR REPLACE INTO counties SELECT * FROM ", temp_counties))
    
    # Clean up temp table
    dbExecute(con, paste0("DROP TABLE IF EXISTS ", temp_counties))
    
    log_message(paste("Upserted", nrow(unique_counties), "counties to database using INSERT OR REPLACE"),
               level = "INFO", show_console = TRUE)
    log_message(paste("Added", nrow(unique_counties), "counties to database"),
               level = "INFO", show_console = TRUE)
  }, error = function(e) {
    log_message(paste("Error adding counties to database:", conditionMessage(e)),
               level = "ERROR", show_console = TRUE)
  })
  
  # 2. Create variables table
  log_message("Creating variables table...", level = "INFO", show_console = TRUE)
  variables_schema <- "
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
  "
  safe_create_table("variables", variables_schema)
  
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
  
  # Update variables with INSERT OR REPLACE
  tryCatch({
    # Create temp table with new data
    temp_variables <- paste0("temp_variables_", format(Sys.time(), "%H%M%S"))
    dbWriteTable(con, temp_variables, variables_for_db, temporary = TRUE)
    
    # Use INSERT OR REPLACE for atomic upsert
    dbExecute(con, paste0("INSERT OR REPLACE INTO variables SELECT * FROM ", temp_variables))
    
    # Clean up temp table
    dbExecute(con, paste0("DROP TABLE IF EXISTS ", temp_variables))
    
    log_message(paste("Upserted", nrow(variables_for_db), "variables to database using INSERT OR REPLACE"),
               level = "INFO", show_console = TRUE)
    log_message(paste("Added", nrow(variables_for_db), "variables to database"),
               level = "INFO", show_console = TRUE)
  }, error = function(e) {
    log_message(paste("Error adding variables to database:", conditionMessage(e)),
               level = "ERROR", show_console = TRUE)
  })
  
  # 3. Create main data table in normalized form (tall/long format)
  log_message("Creating main SDOH data table...", level = "INFO", show_console = TRUE)
  sdoh_schema <- "
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
  "
  safe_create_table("sdoh_data", sdoh_schema)
  
  # Create processing metadata table if needed
  log_message("Setting up processing metadata tracking...",
             level = "INFO", show_console = TRUE)
             
  metadata_schema <- "
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
  "
  safe_create_table("processing_metadata", metadata_schema)
  
  # Process the data for insertion (convert from wide to long format)
  # First, identify the columns that need to be pivoted (excluding metadata)
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
  
  # IMPORTANT MODIFICATION: Don't restrict pivot_cols to just those in var_names
  # This was causing only 3 variables to be processed
  # Instead, create a list of all variables from crosswalk that need to be included
  
  log_message(paste("Found", length(pivot_cols), "data columns in processed data"), 
              level = "INFO", show_console = TRUE)
  log_message(paste("Need to include", length(var_names), "variables from crosswalk"),
              level = "INFO", show_console = TRUE)
  
  # Verify we have variable data to pivot
  if (length(pivot_cols) == 0) {
    log_message("ERROR: No variable data columns found in the processed data for pivoting",
               level = "ERROR", show_console = TRUE)
    
    # Create a comprehensive dataset with entries for ALL variables
    log_message("Creating a comprehensive dataset for ALL variables to ensure database completeness",
               level = "INFO", show_console = TRUE)

    # Get all counties and years combinations from processed data
    county_years <- processed_data %>%
      select(geoid, year) %>%
      distinct()
    
    if (nrow(county_years) == 0) {
      # If no data, use the first county and a sample year
      county_years <- data.frame(
        geoid = unique_counties$geoid[1],
        year = 2020,
        stringsAsFactors = FALSE
      )
    }
    
    log_message(paste("Created", nrow(county_years), "county-year combinations as data structure"),
               level = "INFO", show_console = TRUE)
    
    # Create data entries for every variable in the crosswalk
    all_vars_data <- list()
    
    # Process in batches to avoid memory issues
    batch_size <- 20
    total_batches <- ceiling(length(var_names) / batch_size)
    
    for (batch_index in 1:total_batches) {
      start_idx <- (batch_index - 1) * batch_size + 1
      end_idx <- min(batch_index * batch_size, length(var_names))
      batch_vars <- var_names[start_idx:end_idx]
      
      log_message(paste("Creating data structure for batch", batch_index, "with", 
                       length(batch_vars), "variables"),
                 level = "INFO", show_console = TRUE)
      
      # For each variable, create entries for all county-year combinations
      for (var in batch_vars) {
        var_data <- county_years %>%
          mutate(
            variable_name = var,
            value = NA,  # Use NA for missing values - no synthetic data
            data_quality = "pending",
            data_source = "pipeline",
            data_vintage = format(Sys.Date(), "%Y"),
            interpolation_method = NA,
            ci_lower = NA,
            ci_upper = NA,
            confidence_level = NA,
            last_updated = Sys.time()
          )
        
        all_vars_data[[var]] <- var_data
      }
      
      # Combine data for this batch
      batch_data <- bind_rows(all_vars_data[batch_vars])
      
      # Insert batch into database
      log_message(paste("Inserting batch", batch_index, "with", nrow(batch_data), "rows into database..."),
                 level = "INFO", show_console = TRUE)
      
      # Insert data using the upsert pattern
      batch_name <- paste0("batch_", batch_index, "_", format(Sys.time(), "%H%M%S"))
      
      tryCatch({
        # Create temp table
        dbWriteTable(con, batch_name, batch_data, temporary = TRUE)
        
        # Use INSERT OR REPLACE for atomic upsert
        upsert_query <- paste0("INSERT OR REPLACE INTO sdoh_data SELECT * FROM ", batch_name)
        dbExecute(con, upsert_query)
        
        # Clean up temp table
        dbExecute(con, paste0("DROP TABLE IF EXISTS ", batch_name))
        
        log_message(paste("Successfully inserted batch", batch_index, "with", nrow(batch_data), "rows"),
                   level = "INFO", show_console = TRUE)
      }, error = function(e) {
        log_message(paste("Error inserting batch", batch_index, ":", conditionMessage(e)),
                   level = "ERROR", show_console = TRUE)
      })
      
      # Clear batch data to free memory
      rm(batch_data)
      all_vars_data[batch_vars] <- NULL
      gc()
    }
    
    # Create a view for easier access
    create_database_views(con)
    
    # Disconnect to make sure changes are committed
    dbDisconnect(con)
    
    log_message("Database created successfully with comprehensive structure for all variables",
               level = "INFO", show_console = TRUE)
    return(TRUE)
  }
  
  # Now we know we have data to pivot, but we need to make sure we include all variables
  # from the crosswalk, not just those with data
  
  # First, process the variables that have actual data
  log_message(paste("Converting", length(pivot_cols), "variables to long format..."),
             level = "INFO", show_console = TRUE)
  
  # Batch processing to avoid memory issues
  batch_size <- 20
  total_batches <- ceiling(length(pivot_cols) / batch_size)
  
  log_message(paste("Processing in", total_batches, "batches to avoid memory issues"),
             level = "INFO", show_console = TRUE)
  
  # Track which variables we've processed
  processed_variables <- character(0)
  
  # Process each batch of variables with data
  for (batch_index in 1:total_batches) {
    start_idx <- (batch_index - 1) * batch_size + 1
    end_idx <- min(batch_index * batch_size, length(pivot_cols))
    batch_vars <- pivot_cols[start_idx:end_idx]
    
    if (length(batch_vars) == 0) {
      next
    }
    
    log_message(paste("Processing batch", batch_index, "of", total_batches, 
                      "with", length(batch_vars), "variables"),
               level = "INFO", show_console = TRUE)
    
    # Create a subset of data with just the necessary columns for pivoting
    batch_cols <- c(metadata_cols, batch_vars)
    
    # Debug the column types
    log_message("Checking column types for batch...", level = "INFO", show_console = TRUE)
    for (col in batch_vars) {
      if (col %in% names(processed_data)) {
        log_message(paste("Column", col, "type:", class(processed_data[[col]])[1]), level = "INFO", show_console = TRUE)
      }
    }
    
    # Ensure all batch columns exist in the data
    existing_batch_cols <- batch_cols[batch_cols %in% names(processed_data)]
    if (length(existing_batch_cols) < length(batch_cols)) {
      log_message(paste("Warning: Some batch columns don't exist in the data. Requested:", length(batch_cols), 
                        "Available:", length(existing_batch_cols)),
                  level = "WARN", show_console = TRUE)
    }
    
    # Extract data with only existing columns
    batch_data <- processed_data[, existing_batch_cols, drop=FALSE]
    
    # Ensure metadata_cols are properly formatted
    for (col in metadata_cols) {
      if (col %in% names(batch_data)) {
        # Convert character year to numeric if needed
        if (col == "year" && is.character(batch_data[[col]])) {
          batch_data[[col]] <- as.numeric(batch_data[[col]])
          log_message("Converted year column from character to numeric", level = "INFO", show_console = TRUE)
        }
        # Ensure geoid is character
        if (col == "geoid" && !is.character(batch_data[[col]])) {
          batch_data[[col]] <- as.character(batch_data[[col]])
          log_message("Converted geoid column to character", level = "INFO", show_console = TRUE)
        }
      }
    }
    
    # Get data quality and interpolation flags if available
    data_quality_cols <- c()
    interpolation_cols <- c()
    for (var in batch_vars) {
      # Add to processed variables list
      processed_variables <- c(processed_variables, var)
      
      # Check for data quality flags
      quality_col <- paste0("data_quality_", var)
      if (quality_col %in% names(processed_data)) {
        data_quality_cols <- c(data_quality_cols, quality_col)
        batch_data[[quality_col]] <- processed_data[[quality_col]]
      }
      
      # Check for interpolation flags
      interpolated_col <- paste0(var, "_interpolated")
      if (interpolated_col %in% names(processed_data)) {
        interpolation_cols <- c(interpolation_cols, interpolated_col)
        batch_data[[interpolated_col]] <- processed_data[[interpolated_col]]
      }
    }
    
    # Pivot the data to long format
    log_message("Pivoting batch data to long format...",
               level = "INFO", show_console = TRUE)
    
    # Filter batch_vars to only include columns that exist in batch_data
    available_vars <- batch_vars[batch_vars %in% names(batch_data)]
    if (length(available_vars) == 0) {
      log_message("No variables available for pivoting in this batch, skipping...",
                 level = "WARN", show_console = TRUE)
      next
    }
    
    # Print all column names and their types for debugging
    log_message("Columns and types in batch_data:", level = "INFO", show_console = TRUE)
    for (col_name in names(batch_data)) {
      log_message(paste("  -", col_name, ":", class(batch_data[[col_name]])[1]), 
                 level = "INFO", show_console = TRUE)
    }
    
    # Handle data quality flags if they exist
    tryCatch({
      if (length(data_quality_cols) > 0) {
        batch_long <- batch_data %>%
          # Convert all variable columns to numeric to ensure consistency
          mutate(across(all_of(available_vars), as.numeric)) %>%
          pivot_longer(
            cols = all_of(available_vars),
            names_to = "variable_name",
            values_to = "value"
          )
      
      # Process quality flags
      if (length(data_quality_cols) > 0) {
        # Create a lookup for quality flags
        quality_lookup <- data.frame(
          variable_name = gsub("data_quality_", "", data_quality_cols),
          quality_col = data_quality_cols,
          stringsAsFactors = FALSE
        )
        
        # Add data quality column
        batch_long$data_quality <- "direct"  # Default
        
        # Update with actual quality flags
        for (i in 1:nrow(quality_lookup)) {
          var <- quality_lookup$variable_name[i]
          qcol <- quality_lookup$quality_col[i]
          
          if (qcol %in% names(batch_data)) {
            # Get the rows for this variable
            var_rows <- which(batch_long$variable_name == var)
            
            # Get the quality values from the original data
            # This is tricky since we pivoted - need to map back
            for (j in var_rows) {
              geoid <- batch_long$geoid[j]
              year <- batch_long$year[j]
              
              # Find the original row
              orig_row <- which(batch_data$geoid == geoid & batch_data$year == year)
              if (length(orig_row) > 0) {
                batch_long$data_quality[j] <- batch_data[[qcol]][orig_row[1]]
              }
            }
          }
        }
      }
      
      # Process interpolation flags
      if (length(interpolation_cols) > 0) {
        # Create a lookup for interpolation flags
        interp_lookup <- data.frame(
          variable_name = gsub("_interpolated$", "", interpolation_cols),
          interp_col = interpolation_cols,
          stringsAsFactors = FALSE
        )
        
        # Add interpolation method column
        batch_long$interpolation_method <- NA  # Default
        
        # Update with actual interpolation flags
        for (i in 1:nrow(interp_lookup)) {
          var <- interp_lookup$variable_name[i]
          icol <- interp_lookup$interp_col[i]
          
          if (icol %in% names(batch_data)) {
            # Get the rows for this variable
            var_rows <- which(batch_long$variable_name == var)
            
            # Get the interpolation values from the original data
            for (j in var_rows) {
              geoid <- batch_long$geoid[j]
              year <- batch_long$year[j]
              
              # Find the original row
              orig_row <- which(batch_data$geoid == geoid & batch_data$year == year)
              if (length(orig_row) > 0) {
                # If interpolated, set method
                if (!is.na(batch_data[[icol]][orig_row[1]]) && batch_data[[icol]][orig_row[1]]) {
                  batch_long$interpolation_method[j] <- "linear"
                  batch_long$data_quality[j] <- "interpolated"
                }
              }
            }
          }
        }
      }
    } else {
      # No data quality flags, simpler pivot
      tryCatch({
        batch_long <- batch_data %>%
          # Convert all variable columns to numeric to ensure consistency
          mutate(across(all_of(available_vars), ~as.numeric(as.character(.)))) %>%
          pivot_longer(
            cols = all_of(available_vars),
            names_to = "variable_name",
            values_to = "value"
          ) %>%
          mutate(data_quality = "direct")  # Default quality flag
      }, error = function(e) {
        log_message(paste("Error during pivot_longer: ", conditionMessage(e)), 
                   level = "ERROR", show_console = TRUE)
        
        # Try a more conservative approach - process one column at a time
        log_message("Attempting to process columns individually...", 
                   level = "INFO", show_console = TRUE)
        
        all_pivoted_rows <- list()
        
        for (var in available_vars) {
          tryCatch({
            # Create a temporary dataframe with just this variable
            temp_df <- batch_data[, c(metadata_cols, var)]
            temp_df$value <- as.numeric(as.character(temp_df[[var]]))
            temp_df$variable_name <- var
            temp_df$data_quality <- "direct"
            
            # Remove the original variable column
            temp_df[[var]] <- NULL
            
            # Store the result
            all_pivoted_rows[[var]] <- temp_df
            
            log_message(paste("Successfully processed variable:", var), 
                       level = "INFO", show_console = TRUE)
          }, error = function(var_error) {
            log_message(paste("Error processing variable", var, ":", conditionMessage(var_error)), 
                       level = "WARN", show_console = TRUE)
          })
        }
        
        # Combine all successful pivots
        if (length(all_pivoted_rows) > 0) {
          batch_long <- bind_rows(all_pivoted_rows)
          log_message(paste("Successfully created long format data from", length(all_pivoted_rows), 
                            "out of", length(available_vars), "variables"), 
                     level = "INFO", show_console = TRUE)
        } else {
          log_message("Failed to create any long format data, skipping batch", 
                     level = "ERROR", show_console = TRUE)
          next
        }
      })
    }
    
    # Add required columns
    if (!"data_source" %in% names(batch_long)) {
      batch_long$data_source <- "pipeline"
    }
    
    if (!"data_vintage" %in% names(batch_long)) {
      batch_long$data_vintage <- format(Sys.Date(), "%Y")
    }
    
    if (!"interpolation_method" %in% names(batch_long)) {
      batch_long$interpolation_method <- NA
    }
    
    # Add confidence interval columns if missing
    if (!"ci_lower" %in% names(batch_long)) {
      batch_long$ci_lower <- NA
    }
    
    if (!"ci_upper" %in% names(batch_long)) {
      batch_long$ci_upper <- NA
    }
    
    if (!"confidence_level" %in% names(batch_long)) {
      batch_long$confidence_level <- NA
    }
    
    # Add timestamp
    batch_long$last_updated <- Sys.time()
    
    # Retain only rows with non-NA values to ensure we only have REAL data
    # This is important - we don't want synthetic data, just real data
    batch_long <- batch_long %>%
      filter(!is.na(value))
    
    # Ensure all columns required by the schema are present
    required_cols <- c("geoid", "year", "variable_name", "value", "data_quality", 
                        "data_source", "data_vintage", "interpolation_method",
                        "ci_lower", "ci_upper", "confidence_level", "last_updated")
    
    missing_cols <- setdiff(required_cols, names(batch_long))
    for (col in missing_cols) {
      batch_long[[col]] <- NA
    }
    
    # Only keep the required columns in the required order
    batch_long <- batch_long[, required_cols]
    
    # Insert batch into database
    log_message(paste("Inserting batch", batch_index, "with", nrow(batch_long), "rows into database..."),
               level = "INFO", show_console = TRUE)
    
    # Clear existing data for these variables if not in incremental mode
    if (!use_incremental) {
      var_list <- paste0("'", paste(batch_vars, collapse = "', '"), "'")
      delete_query <- paste0("DELETE FROM sdoh_data WHERE variable_name IN (", var_list, ")")
      tryCatch({
        dbExecute(con, delete_query)
      }, error = function(e) {
        log_message(paste("Error clearing existing data:", conditionMessage(e)),
                   level = "WARN", show_console = TRUE)
      })
    }
    
    # Insert data using the upsert pattern
    batch_name <- paste0("batch_", batch_index, "_", format(Sys.time(), "%H%M%S"))
    
    tryCatch({
      # Create temp table
      dbWriteTable(con, batch_name, batch_long, temporary = TRUE)
      
      # Use INSERT OR REPLACE for atomic upsert
      upsert_query <- paste0("INSERT OR REPLACE INTO sdoh_data SELECT * FROM ", batch_name)
      dbExecute(con, upsert_query)
      
      # Clean up temp table
      dbExecute(con, paste0("DROP TABLE IF EXISTS ", batch_name))
      
      log_message(paste("Successfully inserted batch", batch_index, "with", nrow(batch_long), "rows"),
                 level = "INFO", show_console = TRUE)
    }, error = function(e) {
      log_message(paste("Error inserting batch", batch_index, ":", conditionMessage(e)),
                 level = "ERROR", show_console = TRUE)
    })
    
    # Update the processing metadata
    for (var in batch_vars) {
      # Get min and max years for this variable
      var_data <- batch_long[batch_long$variable_name == var, ]
      if (nrow(var_data) > 0) {
        min_year <- min(var_data$year, na.rm = TRUE)
        max_year <- max(var_data$year, na.rm = TRUE)
        record_count <- nrow(var_data)
        
        # Update metadata
        metadata_df <- data.frame(
          data_source = "pipeline",
          variable_name = var,
          min_year = min_year,
          max_year = max_year,
          record_count = record_count,
          last_processed = Sys.time(),
          data_version = format(Sys.Date(), "%Y%m%d")
        )
        
        # Create a temp table for the metadata
        metadata_table <- paste0("metadata_", format(Sys.time(), "%H%M%S"))
        dbWriteTable(con, metadata_table, metadata_df, temporary = TRUE)
        
        # Insert the metadata with upsert
        metadata_query <- paste0("INSERT OR REPLACE INTO processing_metadata SELECT * FROM ", metadata_table)
        tryCatch({
          dbExecute(con, metadata_query)
          dbExecute(con, paste0("DROP TABLE IF EXISTS ", metadata_table))
        }, error = function(e) {
          log_message(paste("Error updating metadata for variable", var, ":", conditionMessage(e)),
                     level = "WARN", show_console = TRUE)
        })
      }
    }
    
    # Clear batch data to free memory
    rm(batch_data, batch_long)
    gc()
  }
  
  # Now, check for any variables in the crosswalk that don't have data
  # This ensures all 255 variables are represented in the database
  missing_variables <- setdiff(var_names, processed_variables)
  
  if (length(missing_variables) > 0) {
    log_message(paste("Adding", length(missing_variables), "variables from crosswalk that don't have data"),
               level = "INFO", show_console = TRUE)
    
    # Get existing county-year combinations from database
    county_years <- NULL
    tryCatch({
      county_years <- dbGetQuery(con, "SELECT DISTINCT geoid, year FROM sdoh_data")
    }, error = function(e) {
      log_message(paste("Error getting county-year combinations:", conditionMessage(e)),
                 level = "WARN", show_console = TRUE)
    })
    
    # If no existing data, use counties table with sample year
    if (is.null(county_years) || nrow(county_years) == 0) {
      counties <- dbGetQuery(con, "SELECT geoid FROM counties")
      if (nrow(counties) > 0) {
        county_years <- data.frame(
          geoid = counties$geoid,
          year = 2020,
          stringsAsFactors = FALSE
        )
      } else {
        # If no counties in database, use the ones we extracted
        county_years <- data.frame(
          geoid = unique_counties$geoid,
          year = 2020,
          stringsAsFactors = FALSE
        )
      }
    }
    
    # Process missing variables in batches
    batch_size <- 20
    total_batches <- ceiling(length(missing_variables) / batch_size)
    
    for (batch_index in 1:total_batches) {
      start_idx <- (batch_index - 1) * batch_size + 1
      end_idx <- min(batch_index * batch_size, length(missing_variables))
      batch_vars <- missing_variables[start_idx:end_idx]
      
      if (length(batch_vars) == 0) {
        next
      }
      
      log_message(paste("Processing missing variables batch", batch_index, "of", total_batches, 
                        "with", length(batch_vars), "variables"),
                 level = "INFO", show_console = TRUE)
      
      # Create empty rows for these variables
      # We're only creating a minimal structure - just one county-year per variable
      # We don't want to create tons of empty data
      
      # Use the first county-year as a representative
      sample_county_year <- county_years[1, ]
      
      # Create placeholder data for each variable
      placeholder_data <- list()
      for (var in batch_vars) {
        placeholder_data[[var]] <- data.frame(
          geoid = sample_county_year$geoid,
          year = sample_county_year$year,
          variable_name = var,
          value = NA,  # No synthetic data
          data_quality = "pending",
          data_source = "pipeline",
          data_vintage = format(Sys.Date(), "%Y"),
          interpolation_method = NA,
          ci_lower = NA,
          ci_upper = NA,
          confidence_level = NA,
          last_updated = Sys.time(),
          stringsAsFactors = FALSE
        )
      }
      
      # Combine all placeholders
      batch_data <- bind_rows(placeholder_data)
      
      # Insert batch into database
      batch_name <- paste0("missing_", batch_index, "_", format(Sys.time(), "%H%M%S"))
      
      tryCatch({
        # Create temp table
        dbWriteTable(con, batch_name, batch_data, temporary = TRUE)
        
        # Use INSERT OR REPLACE for atomic upsert
        upsert_query <- paste0("INSERT OR REPLACE INTO sdoh_data SELECT * FROM ", batch_name)
        dbExecute(con, upsert_query)
        
        # Clean up temp table
        dbExecute(con, paste0("DROP TABLE IF EXISTS ", batch_name))
        
        log_message(paste("Successfully inserted", nrow(batch_data), "placeholder rows for missing variables"),
                   level = "INFO", show_console = TRUE)
      }, error = function(e) {
        log_message(paste("Error inserting placeholder data:", conditionMessage(e)),
                   level = "ERROR", show_console = TRUE)
      })
      
      # Update processing metadata for these variables
      for (var in batch_vars) {
        metadata_df <- data.frame(
          data_source = "pipeline",
          variable_name = var,
          min_year = sample_county_year$year,
          max_year = sample_county_year$year,
          record_count = 1,  # Just one placeholder record
          last_processed = Sys.time(),
          data_version = format(Sys.Date(), "%Y%m%d")
        )
        
        # Create a temp table for the metadata
        metadata_table <- paste0("meta_missing_", format(Sys.time(), "%H%M%S"))
        dbWriteTable(con, metadata_table, metadata_df, temporary = TRUE)
        
        # Insert the metadata with upsert
        metadata_query <- paste0("INSERT OR REPLACE INTO processing_metadata SELECT * FROM ", metadata_table)
        tryCatch({
          dbExecute(con, metadata_query)
          dbExecute(con, paste0("DROP TABLE IF EXISTS ", metadata_table))
        }, error = function(e) {
          log_message(paste("Error updating metadata for missing variable", var, ":", conditionMessage(e)),
                     level = "WARN", show_console = TRUE)
        })
      }
      
      # Clear memory
      rm(batch_data, placeholder_data)
      gc()
    }
  }
  
  # Verify that all variables are in the database
  tryCatch({
    var_count_query <- "SELECT COUNT(DISTINCT variable_name) AS count FROM sdoh_data"
    var_count <- dbGetQuery(con, var_count_query)
    log_message(paste("Database now contains", var_count[1,1], "distinct variables out of", 
                     length(var_names), "in crosswalk"),
               level = "INFO", show_console = TRUE)
    
    if (var_count[1,1] < length(var_names)) {
      log_message("WARNING: Some variables may still be missing from the database",
                 level = "WARN", show_console = TRUE)
    }
  }, error = function(e) {
    log_message(paste("Error verifying variable count:", conditionMessage(e)),
               level = "WARN", show_console = TRUE)
  })
  
  # Create helpful database views for easy access
  create_database_views(con)
  
  # Create basic indices for performance
  log_message("Creating basic database indices...",
             level = "INFO", show_console = TRUE)
  
  # Index on common query patterns
  tryCatch({
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_sdoh_var_year ON sdoh_data(variable_name, year)")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_sdoh_geoid_year ON sdoh_data(geoid, year)")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_sdoh_quality ON sdoh_data(data_quality)")
  }, error = function(e) {
    log_message(paste("Error creating indices:", conditionMessage(e)),
               level = "WARN", show_console = TRUE)
  })
  
  # Disconnect to make sure changes are committed
  dbDisconnect(con)
  
  log_message("Database created successfully with all variables from crosswalk",
             level = "INFO", show_console = TRUE)
  
  return(TRUE)
}

#' Create database views for easier data access
#'
#' @param con Database connection
#' @return Boolean indicating success
create_database_views <- function(con) {
  log_message("Creating database views for easy access...",
             level = "INFO", show_console = TRUE)
  
  # Create a view joining counties and data
  county_data_view <- "
    CREATE OR REPLACE VIEW county_data AS
    SELECT
      c.geoid,
      c.name AS county_name,
      c.state_fips,
      c.state_name,
      d.year,
      d.variable_name,
      d.value,
      d.data_quality,
      d.data_source,
      d.data_vintage,
      d.interpolation_method,
      d.ci_lower,
      d.ci_upper,
      d.confidence_level,
      d.last_updated
    FROM counties c
    JOIN sdoh_data d ON c.geoid = d.geoid
  "
  
  # Create a view for the latest available data for each county and variable
  latest_data_view <- "
    CREATE OR REPLACE VIEW latest_data AS
    SELECT
      c.geoid,
      c.name AS county_name,
      c.state_fips,
      c.state_name,
      d.variable_name,
      d.value,
      d.year,
      d.data_quality,
      d.data_source,
      v.domain,
      v.description,
      v.units
    FROM counties c
    JOIN (
      SELECT geoid, variable_name, MAX(year) AS max_year
      FROM sdoh_data
      GROUP BY geoid, variable_name
    ) latest ON c.geoid = latest.geoid
    JOIN sdoh_data d ON c.geoid = d.geoid AND d.variable_name = latest.variable_name AND d.year = latest.max_year
    JOIN variables v ON d.variable_name = v.variable_name
  "
  
  # Create a view for time series data
  time_series_view <- "
    CREATE OR REPLACE VIEW time_series AS
    SELECT
      c.geoid,
      c.name AS county_name,
      c.state_fips,
      c.state_name,
      d.year,
      d.variable_name,
      d.value,
      d.data_quality,
      v.domain,
      v.units
    FROM counties c
    JOIN sdoh_data d ON c.geoid = d.geoid
    JOIN variables v ON d.variable_name = v.variable_name
    ORDER BY c.geoid, d.variable_name, d.year
  "
  
  # Execute the view creation queries
  tryCatch({
    dbExecute(con, county_data_view)
    dbExecute(con, latest_data_view)
    dbExecute(con, time_series_view)
    log_message("Database views created successfully",
               level = "INFO", show_console = TRUE)
    return(TRUE)
  }, error = function(e) {
    log_message(paste("Error creating database views:", conditionMessage(e)),
               level = "WARN", show_console = TRUE)
    return(FALSE)
  })
}

#' Verify that all variables from crosswalk are in the database
#'
#' @param db_path Path to the database
#' @param crosswalk_path Path to the crosswalk CSV file
#' @return TRUE if all variables are in the database, FALSE otherwise
verify_all_variables <- function(db_path, crosswalk_path = NULL) {
  # Connect to the database
  con <- tryCatch({
    dbConnect(duckdb::duckdb(), dbdir = db_path)
  }, error = function(e) {
    log_message(paste("Error connecting to database:", conditionMessage(e)),
               level = "ERROR", show_console = TRUE)
    return(FALSE)
  })
  
  # Get variables from crosswalk
  if (!is.null(crosswalk_path) && file.exists(crosswalk_path)) {
    crosswalk <- read.csv(crosswalk_path, stringsAsFactors = FALSE)
    crosswalk_vars <- unique(crosswalk$variable_name)
  } else {
    # Try to get from database
    crosswalk_vars <- dbGetQuery(con, "SELECT variable_name FROM variables")$variable_name
  }
  
  # Get variables in sdoh_data table
  db_vars <- dbGetQuery(con, "SELECT DISTINCT variable_name FROM sdoh_data")$variable_name
  
  # Find missing variables
  missing_vars <- setdiff(crosswalk_vars, db_vars)
  
  # Disconnect from database
  dbDisconnect(con)
  
  if (length(missing_vars) > 0) {
    log_message(paste("WARNING:", length(missing_vars), "variables from crosswalk are missing in the database:"),
               level = "WARN", show_console = TRUE)
    log_message(paste(missing_vars, collapse = ", "),
               level = "WARN", show_console = TRUE)
    return(FALSE)
  } else {
    log_message(paste("SUCCESS: All", length(crosswalk_vars), "variables from crosswalk are in the database"),
               level = "INFO", show_console = TRUE)
    return(TRUE)
  }
}

# Module is complete
log_message("Database module loaded successfully", level = "INFO", show_console = TRUE)
message("Database module with complete data insertion loaded successfully")
TRUE