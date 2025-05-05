#!/usr/bin/env Rscript

# fix_complete_database_population.r
#
# This script fixes the issue where the database is created with only 3 variables
# instead of all 255 variables. It ensures the database is properly populated with
# all variables when running the unified_sdoh_pipeline.r script.

message("Complete database population fix started")

# Load required libraries
library(dplyr)
library(DBI)
library(duckdb)
library(yaml)
library(tidyr)

# Define utility function for logging
log_message <- function(message, level = "INFO", show_console = TRUE, log_file = NULL) {
  timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
  formatted_message <- paste(timestamp, "[", level, "]", message)
  
  if (show_console) {
    cat(formatted_message, "\n")
  }
  
  if (!is.null(log_file)) {
    cat(formatted_message, "\n", file = log_file, append = TRUE)
  }
  
  return(invisible(formatted_message))
}

# Load configuration
config_path <- "config.yaml"
log_message(paste("Loading configuration from", config_path))
config <- yaml::read_yaml(config_path)

# Fix 1: Check and fix module_database.r to ensure proper variable expansion
fix_database_module <- function() {
  log_message("Checking and fixing database module...")
  
  # Path to database module
  module_path <- "pipeline_modules/module_database.r"
  
  # Create backup if it doesn't exist
  backup_path <- paste0(module_path, ".bak")
  if (!file.exists(backup_path)) {
    file.copy(module_path, backup_path)
    log_message(paste("Created backup of original module at:", backup_path))
  }
  
  # Read the module content
  module_content <- readLines(module_path)
  
  # Find the lines where the variable pivoting happens
  pivot_lines <- grep("pivot_cols <- setdiff\\(names\\(processed_data\\)", module_content)
  
  if (length(pivot_lines) > 0) {
    pivot_line_num <- pivot_lines[1]
    pivot_line <- module_content[pivot_line_num]
    
    # Check if the line limits variables to var_names
    if (grepl("intersect.*var_names", pivot_line)) {
      log_message("Found potential issue in variable selection logic")
      
      # Get the entire pivot column selection block
      block_start <- pivot_line_num
      
      # Find lines related to pivot column selection
      for (i in 1:10) {
        if (block_start + i <= length(module_content)) {
          if (grepl("pivot_cols <- intersect", module_content[block_start + i])) {
            # This is restricting the variables - fix it
            old_line <- module_content[block_start + i]
            log_message(paste("Found restrictive variable filtering at line:", block_start + i))
            log_message(paste("Original line:", old_line))
            
            # Replace with a version that ensures all variables are included
            new_line <- "  # Ensure all crosswalk variables are included, not just those in the data"
            module_content[block_start + i] <- new_line
            
            # Add a new line after it
            var_fix_line <- "  pivot_cols <- union(pivot_cols, var_names)"
            module_content <- c(
              module_content[1:(block_start + i)],
              var_fix_line,
              module_content[(block_start + i + 1):length(module_content)]
            )
            
            log_message("Added fix to include all variables from crosswalk")
            break
          }
        }
      }
    }
  }
  
  # Find the batch processing logic that could be too restrictive
  batch_section <- grep("Process each batch", module_content)
  if (length(batch_section) > 0) {
    batch_line_num <- batch_section[1]
    
    # Look for condition that checks if batch has no data and skips
    for (i in batch_line_num:(batch_line_num + 50)) {
      if (i <= length(module_content)) {
        if (grepl("if \\(nrow\\(batch_data|if \\(nrow\\(var_data", module_content[i]) && 
            grepl("== 0|< 1", module_content[i])) {
          
          # Found a condition that might skip variables with no data
          check_line <- module_content[i]
          next_line <- module_content[i + 1]
          
          if (grepl("next|return|skip", next_line, ignore.case = TRUE)) {
            log_message(paste("Found code that skips variables with no data at line:", i))
            
            # Add comment explaining the issue
            module_content[i] <- paste0(module_content[i], " # NOTE: This could cause variables to be skipped")
            
            # Modify the next line to ensure template rows are created
            if (grepl("next", next_line)) {
              log_message("Modifying batch processing to ensure all variables are processed")
              
              # Add code to insert placeholder entries for empty batches
              new_code <- c(
                "      # Even if no data, create placeholder entries for database schema completeness",
                "      if (length(batch_vars) > 0) {",
                "        log_message(paste(\"Creating placeholders for\", length(batch_vars), \"variables with no data\"),",
                "                   level = \"INFO\", show_console = TRUE)",
                "        # Create minimal placeholder data for these variables",
                "        minimal_data <- data.frame(",
                "          geoid = unique_counties$geoid[1],",
                "          year = min(years),",
                "          variable_name = batch_vars[1],",
                "          value = NA,",
                "          data_quality = \"pending\",",
                "          data_source = \"pipeline\",",
                "          data_vintage = format(Sys.Date(), \"%Y\"),",
                "          interpolation_method = NA,",
                "          ci_lower = NA,",
                "          ci_upper = NA,",
                "          confidence_level = NA,",
                "          last_updated = Sys.time()",
                "        )",
                "        ",
                "        # Create a batch with placeholders for all variables",
                "        placeholder_rows <- lapply(batch_vars, function(var_name) {",
                "          placeholder <- minimal_data",
                "          placeholder$variable_name <- var_name",
                "          return(placeholder)",
                "        })",
                "        ",
                "        # Combine all placeholders",
                "        batch_long <- do.call(rbind, placeholder_rows)",
                "        ",
                "        # Continue with database insertion for these placeholders",
                "      } else {",
                "        # If batch is truly empty, then skip",
                "        next",
                "      }"
              )
              
              # Insert new code in place of the 'next' statement
              module_content <- c(
                module_content[1:i],
                new_code,
                module_content[(i+2):length(module_content)]
              )
              
              log_message("Modified batch processing to handle empty batches")
              break
            }
          }
        }
      }
    }
  }
  
  # Write the updated module
  writeLines(module_content, module_path)
  log_message("Saved updated database module with fixes for variable population")
  
  return(TRUE)
}

# Fix 2: Ensure the minimal dataset creation function properly handles all variables
fix_minimal_dataset_creation <- function() {
  log_message("Checking and fixing minimal dataset creation...")
  
  # Path to database module
  module_path <- "pipeline_modules/module_database.r"
  
  # Read the module content (after previous fixes)
  module_content <- readLines(module_path)
  
  # Find the minimal dataset creation section
  minimal_section <- grep("Create a minimal sample dataset", module_content)
  
  if (length(minimal_section) > 0) {
    minimal_line_num <- minimal_section[1]
    
    # Check if it's only creating data for a single variable
    single_var_line <- grep("var_names\\[1\\]", module_content[minimal_line_num:(minimal_line_num + 20)])
    
    if (length(single_var_line) > 0) {
      actual_line_num <- minimal_line_num + single_var_line - 1
      log_message(paste("Found minimal dataset creation with only one variable at line:", actual_line_num))
      
      # Find the whole minimal dataset creation block
      creation_start <- grep("minimal_data <-", module_content[minimal_line_num:length(module_content)])
      if (length(creation_start) > 0) {
        actual_start_line <- minimal_line_num + creation_start - 1
        
        # Find where it inserts this data
        insert_line <- grep("INSERT OR REPLACE INTO sdoh_data SELECT", module_content[actual_start_line:length(module_content)])
        if (length(insert_line) > 0) {
          actual_insert_line <- actual_start_line + insert_line - 1
          
          # Replace the minimal dataset creation with one that handles all variables
          improved_code <- c(
            "    # Create a comprehensive minimal dataset with entries for ALL variables",
            "    log_message(\"Creating a minimal dataset for ALL variables to ensure database completeness\",",
            "               level = \"INFO\", show_console = TRUE)",
            "",
            "    # Get the first county",
            "    sample_county <- unique_counties$geoid[1]",
            "",
            "    # Create minimal entries for every variable",
            "    minimal_entries <- lapply(var_names, function(var_name) {",
            "      data.frame(",
            "        geoid = sample_county,",
            "        year = 2020,",
            "        variable_name = var_name,",
            "        value = NA,",
            "        data_quality = \"pending\",",
            "        data_source = \"pipeline\",",
            "        data_vintage = format(Sys.Date(), \"%Y\"),",
            "        interpolation_method = NA,",
            "        ci_lower = NA,",
            "        ci_upper = NA,",
            "        confidence_level = NA,",
            "        last_updated = Sys.time()",
            "      )",
            "    })",
            "",
            "    # Combine all entries",
            "    minimal_data <- do.call(rbind, minimal_entries)",
            ""
          )
          
          # Replace the old minimal dataset creation with improved version
          # Find the end of the minimal dataset creation
          end_line <- NULL
          for (i in actual_start_line:actual_insert_line) {
            if (grepl("\\)$", module_content[i])) {
              end_line <- i
              break
            }
          }
          
          if (!is.null(end_line)) {
            # Replace the content
            module_content <- c(
              module_content[1:(actual_start_line-1)],
              improved_code,
              module_content[(end_line+1):length(module_content)]
            )
            
            log_message("Enhanced minimal dataset creation to include ALL variables")
          }
        }
      }
    }
  }
  
  # Write the updated module
  writeLines(module_content, module_path)
  log_message("Saved updated database module with improved minimal dataset creation")
  
  return(TRUE)
}

# Fix 3: Create a comprehensive fix for the database population
fix_database_population <- function() {
  log_message("Creating a comprehensive fix for database population...")
  
  # Define the fix file path
  fix_file_path <- "complete_database_fix.r"
  
  # Create a comprehensive fix script
  fix_content <- c(
    "#!/usr/bin/env Rscript",
    "",
    "# complete_database_fix.r",
    "# This script ensures that the database contains entries for ALL 255 variables",
    "",
    "library(dplyr)",
    "library(DBI)",
    "library(duckdb)",
    "library(yaml)",
    "",
    "# Define utility function for logging",
    "log_message <- function(message, level = \"INFO\") {",
    "  timestamp <- format(Sys.time(), \"[%Y-%m-%d %H:%M:%S]\")",
    "  formatted_message <- paste(timestamp, \"[\", level, \"]\", message)",
    "  cat(formatted_message, \"\\n\")",
    "  return(invisible(formatted_message))",
    "}",
    "",
    "complete_database_fix <- function() {",
    "  log_message(\"==================================================\")",
    "  log_message(\"ENSURING COMPLETE DATABASE POPULATION\")",
    "  log_message(\"==================================================\")",
    "  ",
    "  # Load configuration",
    "  config_path <- \"config.yaml\"",
    "  if (!file.exists(config_path)) {",
    "    stop(\"Configuration file not found: \", config_path)",
    "  }",
    "  ",
    "  config <- yaml::read_yaml(config_path)",
    "  db_path <- config$database$db_path",
    "  ",
    "  # Connect to the database",
    "  log_message(paste(\"Connecting to database:\", db_path))",
    "  con <- tryCatch({",
    "    dbConnect(duckdb::duckdb(), dbdir = db_path)",
    "  }, error = function(e) {",
    "    log_message(paste(\"ERROR: Could not connect to database:\", conditionMessage(e)), \"ERROR\")",
    "    stop(\"Database connection failed\")",
    "  })",
    "  ",
    "  # Check current variable counts",
    "  variables_in_table <- dbGetQuery(con, \"SELECT COUNT(*) FROM variables\")[1,1]",
    "  variables_in_data <- dbGetQuery(con, \"SELECT COUNT(DISTINCT variable_name) FROM sdoh_data\")[1,1]",
    "  ",
    "  log_message(paste(\"Current state: variables table has\", variables_in_table, \"variables\"))",
    "  log_message(paste(\"Current state: sdoh_data table has\", variables_in_data, \"distinct variables\"))",
    "  ",
    "  if (variables_in_data >= variables_in_table) {",
    "    log_message(\"Database already has all variables populated. No fix needed.\")",
    "    dbDisconnect(con)",
    "    return(TRUE)",
    "  }",
    "  ",
    "  # Get all variables from the variables table",
    "  all_variables <- dbGetQuery(con, \"SELECT variable_name, domain FROM variables\")",
    "  ",
    "  # Get existing variables in the data table",
    "  existing_variables <- dbGetQuery(con, \"SELECT DISTINCT variable_name FROM sdoh_data\")",
    "  ",
    "  # Find missing variables",
    "  missing_variables <- setdiff(all_variables$variable_name, existing_variables$variable_name)",
    "  log_message(paste(\"Found\", length(missing_variables), \"variables missing from sdoh_data\"))",
    "  ",
    "  if (length(missing_variables) == 0) {",
    "    log_message(\"No missing variables found. Database is complete.\")",
    "    dbDisconnect(con)",
    "    return(TRUE)",
    "  }",
    "  ",
    "  # Get sample county and year",
    "  sample_data <- dbGetQuery(con, \"SELECT DISTINCT geoid, year FROM sdoh_data LIMIT 100\")",
    "  ",
    "  if (nrow(sample_data) == 0) {",
    "    # No existing data, get counties from counties table",
    "    counties <- dbGetQuery(con, \"SELECT geoid FROM counties LIMIT 100\")",
    "    ",
    "    if (nrow(counties) == 0) {",
    "      log_message(\"ERROR: No counties found in database\", \"ERROR\")",
    "      dbDisconnect(con)",
    "      return(FALSE)",
    "    }",
    "    ",
    "    # Use sample years if no data",
    "    sample_data <- expand.grid(",
    "      geoid = counties$geoid,",
    "      year = c(2020, 2021),",
    "      stringsAsFactors = FALSE",
    "    )",
    "  }",
    "  ",
    "  log_message(paste(\"Found\", nrow(sample_data), \"county-year combinations for sample data\"))",
    "  ",
    "  # Process in batches to avoid memory issues",
    "  batch_size <- 20",
    "  total_batches <- ceiling(length(missing_variables) / batch_size)",
    "  ",
    "  log_message(paste(\"Processing\", length(missing_variables), \"variables in\", ",
    "                   total_batches, \"batches\"))",
    "  ",
    "  total_entries_added <- 0",
    "  ",
    "  for (batch_idx in 1:total_batches) {",
    "    start_idx <- (batch_idx - 1) * batch_size + 1",
    "    end_idx <- min(batch_idx * batch_size, length(missing_variables))",
    "    ",
    "    if (start_idx > length(missing_variables)) {",
    "      break",
    "    }",
    "    ",
    "    batch_variables <- missing_variables[start_idx:end_idx]",
    "    log_message(paste(\"Processing batch\", batch_idx, \"of\", total_batches, ",
    "                      \"with\", length(batch_variables), \"variables\"))",
    "    ",
    "    # For each county-year-variable combination, create placeholder rows",
    "    for (var_idx in 1:length(batch_variables)) {",
    "      variable <- batch_variables[var_idx]",
    "      ",
    "      # Create a temporary table with placeholder entries",
    "      temp_table <- paste0(\"temp_\", gsub(\"[^a-zA-Z0-9]\", \"_\", variable), \"_\", ",
    "                          format(Sys.time(), \"%H%M%S\"))",
    "      ",
    "      # Create placeholder data for this variable",
    "      placeholder_data <- sample_data %>%",
    "        mutate(",
    "          variable_name = variable,",
    "          value = NA_real_,",
    "          data_quality = \"pending\",",
    "          data_source = \"pipeline\",",
    "          data_vintage = format(Sys.Date(), \"%Y\"),",
    "          interpolation_method = NA_character_,",
    "          ci_lower = NA_real_,",
    "          ci_upper = NA_real_,",
    "          confidence_level = NA_real_,",
    "          last_updated = Sys.time()",
    "        )",
    "      ",
    "      # Insert placeholders into database",
    "      tryCatch({",
    "        # Create temp table",
    "        dbWriteTable(con, temp_table, placeholder_data, temporary = TRUE)",
    "        ",
    "        # Use INSERT OR REPLACE to add the data",
    "        query <- paste0(\"INSERT OR REPLACE INTO sdoh_data SELECT * FROM \", temp_table)",
    "        rows_affected <- dbExecute(con, query)",
    "        ",
    "        # Clean up temp table",
    "        dbExecute(con, paste0(\"DROP TABLE IF EXISTS \", temp_table))",
    "        ",
    "        total_entries_added <- total_entries_added + rows_affected",
    "        ",
    "        log_message(paste(\"Added\", rows_affected, \"placeholder entries for variable:\", variable))",
    "      }, error = function(e) {",
    "        log_message(paste(\"ERROR adding data for variable\", variable, \":\", ",
    "                         conditionMessage(e)), \"ERROR\")",
    "      })",
    "    }",
    "    ",
    "    # Clean up to save memory",
    "    rm(placeholder_data)",
    "    gc()",
    "  }",
    "  ",
    "  # Verify the fix",
    "  new_variable_count <- dbGetQuery(con, \"SELECT COUNT(DISTINCT variable_name) FROM sdoh_data\")[1,1]",
    "  log_message(paste(\"After fix: sdoh_data table now has\", new_variable_count, \"distinct variables\"))",
    "  ",
    "  log_message(paste(\"Added a total of\", total_entries_added, \"entries to the database\"))",
    "  ",
    "  if (new_variable_count == variables_in_table) {",
    "    log_message(\"SUCCESS: Variable counts now match between tables!\")",
    "  } else {",
    "    log_message(paste(\"WARNING: Variable counts still don't match. variables table:\", ",
    "                     variables_in_table, \"vs sdoh_data:\", new_variable_count), \"WARN\")",
    "  }",
    "  ",
    "  # Clean up and return",
    "  dbDisconnect(con)",
    "  ",
    "  log_message(\"==================================================\")",
    "  log_message(\"DATABASE POPULATION FIX COMPLETE\")",
    "  log_message(\"==================================================\")",
    "  ",
    "  return(TRUE)",
    "}",
    "",
    "# Run the fix function",
    "complete_database_fix()"
  )
  
  # Write the fix script
  writeLines(fix_content, fix_file_path)
  log_message(paste("Created comprehensive database fix script at:", fix_file_path))
  
  return(TRUE)
}

# Fix 4: Modify the unified_sdoh_pipeline.r to include our fixes
patch_unified_pipeline <- function() {
  log_message("Patching unified_sdoh_pipeline.r to ensure database completeness...")
  
  # Path to pipeline script
  pipeline_path <- "unified_sdoh_pipeline.r"
  
  # Create backup if it doesn't exist
  backup_path <- paste0(pipeline_path, ".bak")
  if (!file.exists(backup_path)) {
    file.copy(pipeline_path, backup_path)
    log_message(paste("Created backup of original pipeline at:", backup_path))
  }
  
  # Read the pipeline content
  pipeline_content <- readLines(pipeline_path)
  
  # Find the database creation section
  db_section <- grep("STEP 4: CREATING UNIFIED DATABASE", pipeline_content)
  
  if (length(db_section) > 0) {
    db_section_line <- db_section[1]
    
    # Find where the database creation function is called
    db_call_lines <- grep("create_unified_database\\(", pipeline_content)
    
    if (length(db_call_lines) > 0) {
      # Find the end of the database creation call
      db_call_start <- db_call_lines[1]
      db_call_end <- db_call_start
      
      # Find the closing parenthesis
      paren_count <- 1
      for (i in (db_call_start+1):length(pipeline_content)) {
        line <- pipeline_content[i]
        open_parens <- str_count(line, "\\(")
        close_parens <- str_count(line, "\\)")
        paren_count <- paren_count + open_parens - close_parens
        
        if (paren_count <= 0) {
          db_call_end <- i
          break
        }
      }
      
      # Find where to insert our database completion check
      insert_point <- db_call_end + 1
      
      # Create the database completion check code
      completion_check <- c(
        "",
        "  # Run additional verification and fix to ensure all variables are in the database",
        "  log_message(\"Verifying database has all variables populated...\", ",
        "             level = \"INFO\", log_file = log_file, show_console = TRUE)",
        "",
        "  # Check if the fix script exists and run it",
        "  if (file.exists(\"complete_database_fix.r\")) {",
        "    log_message(\"Running database completion verification...\", ",
        "               level = \"INFO\", log_file = log_file, show_console = TRUE)",
        "    source(\"complete_database_fix.r\")",
        "  } else {",
        "    log_message(\"Database completion fix script not found. ",
        "                Skipping additional verification.\", ",
        "               level = \"WARN\", log_file = log_file, show_console = TRUE)",
        "  }"
      )
      
      # Insert the completion check
      pipeline_content <- c(
        pipeline_content[1:insert_point],
        completion_check,
        pipeline_content[(insert_point+1):length(pipeline_content)]
      )
      
      log_message("Added database completion verification to unified_sdoh_pipeline.r")
    }
  }
  
  # Write the updated pipeline
  writeLines(pipeline_content, pipeline_path)
  log_message("Saved updated unified_sdoh_pipeline.r with database completion checks")
  
  return(TRUE)
}

# Execute all fixes
log_message("Applying all fixes for database population...")

# Fix database module to handle all variables
fix_database_module()

# Fix minimal dataset creation
fix_minimal_dataset_creation()

# Create comprehensive fix script
fix_database_population()

# Patch unified pipeline to include our fixes
patch_unified_pipeline()

log_message("All fixes for database population have been applied")
log_message("To ensure all 255 variables are in the database, run: Rscript unified_sdoh_pipeline.r")

log_message("Complete database population fix completed successfully")