#!/usr/bin/env Rscript

# consolidate_crosswalks.r
# This script consolidates multiple crosswalk files into a single definitive file
# and updates the README to match the actual variable count.

library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# Define is_sourced function if it doesn't exist
if (!exists("is_sourced")) {
  is_sourced <- function() {
    # Check if the calling environment is the global environment
    # If it's not, the function is being sourced
    parent_env <- parent.frame()
    return(!identical(parent_env, .GlobalEnv))
  }
}

consolidate_crosswalks <- function() {
  cat("Consolidating SDOH variable crosswalk files...\n")
  
  # Step 1: Find all crosswalk files
  crosswalk_files <- c(
    "variable_crosswalk_extended.csv",
    "output/variable_crosswalk_extended.csv"
  )
  
  # Add more potential locations
  alt_locations <- c(
    "../variable_crosswalk_extended.csv",
    "R/variable_crosswalk_extended.csv",
    "R/output/variable_crosswalk_extended.csv"
  )
  
  crosswalk_files <- c(crosswalk_files, alt_locations)
  
  found_files <- crosswalk_files[sapply(crosswalk_files, file.exists)]
  
  if (length(found_files) == 0) {
    cat("ERROR: Could not find any crosswalk file. Please check the path.\n")
    return(FALSE)
  }
  
  # Report all found crosswalk files
  cat("Found", length(found_files), "crosswalk file(s):\n")
  for (i in seq_along(found_files)) {
    file_info <- file.info(found_files[i])
    file_size <- file_info$size / 1024  # KB
    file_mtime <- file_info$mtime
    cat(sprintf("  %d. %s (%.1f KB, last modified: %s)\n", 
                i, found_files[i], file_size, file_mtime))
  }
  
  # Print a warning about the category column if needed
  if (length(found_files) > 0) {
    sample_file <- found_files[1]
    sample_data <- read_csv(sample_file, n_max = 5, show_col_types = FALSE)
    if (!"category" %in% names(sample_data)) {
      cat("\nWARNING: No 'category' column found in the crosswalk file.\n")
      cat("This may affect the ability to organize variables by domain.\n")
      cat("Consider adding a 'category' column to properly classify variables.\n\n")
    }
  }
  
  # Step 2: Read all crosswalk files
  crosswalks <- list()
  for (i in seq_along(found_files)) {
    tryCatch({
      crosswalks[[i]] <- read_csv(found_files[i], show_col_types = FALSE)
      cat("Successfully read", found_files[i], "with", nrow(crosswalks[[i]]), "variables\n")
    }, error = function(e) {
      cat("Error reading", found_files[i], ":", conditionMessage(e), "\n")
    })
  }
  
  # Filter out any NULL entries from failed reads
  valid_idx <- which(!sapply(crosswalks, is.null))
  crosswalks <- crosswalks[valid_idx]
  valid_files <- found_files[valid_idx]
  
  if (length(crosswalks) == 0) {
    cat("ERROR: Could not read any crosswalk files\n")
    return(FALSE)
  }
  
  # Step 3: Merge all crosswalk files
  cat("\nMerging", length(crosswalks), "crosswalk files...\n")
  
  # Find the crosswalk with the most columns to use as the base
  col_counts <- sapply(crosswalks, ncol)
  base_idx <- which.max(col_counts)
  base_crosswalk <- crosswalks[[base_idx]]
  base_file <- valid_files[base_idx]
  
  cat("Using", base_file, "as the base with", ncol(base_crosswalk), "columns\n")
  
  # Get all unique variable names across all crosswalks
  all_variables <- lapply(crosswalks, function(df) df$variable_name)
  unique_variables <- unique(unlist(all_variables))
  
  cat("Found", length(unique_variables), "unique variables across all crosswalks\n")
  
  # Find variable counts in each crosswalk
  var_counts <- sapply(crosswalks, nrow)
  
  # Create a report of variables by crosswalk
  var_report <- data.frame(
    file = valid_files,
    variables = var_counts
  )
  
  cat("\nVariable counts by crosswalk file:\n")
  print(var_report)
  
  # Create a detailed report of variable presence
  var_presence <- data.frame(variable_name = unique_variables)
  
  for (i in seq_along(crosswalks)) {
    var_presence[[paste0("in_", i)]] <- var_presence$variable_name %in% crosswalks[[i]]$variable_name
  }
  
  # Count how many crosswalks each variable appears in
  var_presence$presence_count <- rowSums(var_presence[, -1])
  
  # Find variables that aren't in all crosswalks
  partial_vars <- var_presence %>%
    filter(presence_count < length(crosswalks) & presence_count > 0) %>%
    arrange(presence_count)
  
  if (nrow(partial_vars) > 0) {
    cat("\nFound", nrow(partial_vars), "variables that aren't in all crosswalks:\n")
    print(partial_vars)
  }
  
  # Step 4: Create consolidated crosswalk
  cat("\nCreating consolidated crosswalk...\n")
  
  # Use the crosswalk with the most rows as the starting point
  row_counts <- sapply(crosswalks, nrow)
  most_rows_idx <- which.max(row_counts)
  most_complete <- crosswalks[[most_rows_idx]]
  
  # Create a combined crosswalk by merging all crosswalks
  consolidated <- most_complete
  
  # For each crosswalk other than the most complete
  for (i in seq_along(crosswalks)) {
    if (i == most_rows_idx) next
    
    other <- crosswalks[[i]]
    
    # Find variables in the other crosswalk that aren't in consolidated
    new_vars <- setdiff(other$variable_name, consolidated$variable_name)
    
    if (length(new_vars) > 0) {
      cat("Adding", length(new_vars), "variables from", valid_files[i], "\n")
      
      # Add these variables to the consolidated crosswalk
      new_rows <- other %>% filter(variable_name %in% new_vars)
      
      # Ensure all required columns exist
      for (col in names(consolidated)) {
        if (!col %in% names(new_rows)) {
          new_rows[[col]] <- NA
        }
      }
      
      # Only keep columns that are in the consolidated crosswalk
      new_rows <- new_rows %>% select(all_of(intersect(names(new_rows), names(consolidated))))
      
      # Add the new rows
      consolidated <- bind_rows(consolidated, new_rows)
    }
    
    # Update any missing values in consolidated from other crosswalk
    common_vars <- intersect(consolidated$variable_name, other$variable_name)
    for (var in common_vars) {
      # Get the row for this variable from both crosswalks
      cons_row <- consolidated %>% filter(variable_name == var)
      other_row <- other %>% filter(variable_name == var)
      
      # For each column that exists in both
      common_cols <- intersect(names(cons_row), names(other_row))
      for (col in common_cols) {
        # If consolidated has NA and other doesn't, update
        if (is.na(cons_row[[col]]) && !is.na(other_row[[col]])) {
          consolidated[consolidated$variable_name == var, col] <- other_row[[col]]
        }
      }
    }
  }
  
  # Sort the variables by category and name if those columns exist
  if ("category" %in% names(consolidated) && "variable_name" %in% names(consolidated)) {
    consolidated <- consolidated %>%
      arrange(category, variable_name)
  } else if ("variable_name" %in% names(consolidated)) {
    consolidated <- consolidated %>%
      arrange(variable_name)
  }
  
  # Step 5: Create the output directory if it doesn't exist
  output_dir <- "output"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Created output directory:", output_dir, "\n")
  }
  
  # Step 6: Write the consolidated crosswalk
  consolidated_file <- file.path(output_dir, "variable_crosswalk_consolidated.csv")
  write_csv(consolidated, consolidated_file)
  cat("Wrote consolidated crosswalk with", nrow(consolidated), "variables to", consolidated_file, "\n")
  
  # Step 7: Create backup of the original files
  for (file in valid_files) {
    backup_file <- paste0(file, ".bak")
    file.copy(file, backup_file, overwrite = TRUE)
    cat("Created backup of", file, "at", backup_file, "\n")
  }
  
  # Step 8: Write the consolidated crosswalk to all original locations
  for (file in valid_files) {
    write_csv(consolidated, file)
    cat("Updated", file, "with the consolidated crosswalk\n")
  }
  
  # Step 9: Generate an extended data dictionary for variables
  dict_file <- file.path(output_dir, "extended_data_dictionary.csv")
  
  # Check which columns exist for the dictionary
  dict_cols <- c()
  if ("variable_name" %in% names(consolidated)) dict_cols <- c(dict_cols, "variable_name")
  if ("description" %in% names(consolidated)) dict_cols <- c(dict_cols, "description")
  if ("category" %in% names(consolidated)) dict_cols <- c(dict_cols, "category")
  if ("years_available" %in% names(consolidated)) dict_cols <- c(dict_cols, "years_available")
  
  # Create the dictionary with available columns
  if (length(dict_cols) > 0) {
    data_dict <- consolidated %>%
      select(all_of(dict_cols))
    
    # Rename columns if they exist
    if ("variable_name" %in% names(data_dict)) data_dict <- data_dict %>% rename(Variable = variable_name)
    if ("description" %in% names(data_dict)) data_dict <- data_dict %>% rename(Description = description)
    if ("category" %in% names(data_dict)) data_dict <- data_dict %>% rename(Category = category)
    if ("years_available" %in% names(data_dict)) data_dict <- data_dict %>% rename(`Years Available` = years_available)
  } else {
    # Create a minimal dictionary with just the variable names as the first column
    data_dict <- data.frame(
      Variable = if ("variable_name" %in% names(consolidated)) consolidated$variable_name else colnames(consolidated)[1]
    )
  }
  
  write_csv(data_dict, dict_file)
  cat("Wrote extended data dictionary to", dict_file, "\n")
  
  # Step 10: Update README if it exists
  readme_file <- "README.md"
  if (!file.exists(readme_file)) {
    readme_file <- "R/README.md"
  }
  
  if (file.exists(readme_file)) {
    cat("\nUpdating README.md with correct variable count...\n")
    
    # Read the README
    readme_content <- readLines(readme_file)
    
    # Look for the summary table with variable counts
    summary_table_start <- grep("\\| Domain \\| Number of Variables \\|", readme_content)
    if (length(summary_table_start) > 0) {
      # Find the end of the summary table
      summary_table_end <- 0
      for (i in (summary_table_start + 1):length(readme_content)) {
        if (!grepl("^\\|", readme_content[i]) || grepl("^\\| \\*\\*Total\\*\\*", readme_content[i])) {
          summary_table_end <- i
          break
        }
      }
      
      if (summary_table_end > 0) {
        # Extract the table rows
        table_rows <- readme_content[(summary_table_start + 1):(summary_table_end - 1)]
        
        # Create a mapping between README domains and crosswalk categories
        domain_mapping <- list(
          "Demographics & Population" = c("Demographics", "Demographic"),
          "Economic Factors" = c("Economic Factors", "Socioeconomic", "Economic"),
          "Education" = c("Education", "Educational Resources & Quality"),
          "Health Status" = c("Health Status", "Health Outcomes"),
          "Healthcare Access" = c("Healthcare Access", "Healthcare", "Health Access"),
          "Housing" = c("Housing"),
          "Environmental Health" = c("Environmental Health", "Environmental"),
          "Food Environment" = c("Food Environment & Access", "Food Environment"),
          "Transportation" = c("Transportation"),
          "Traffic Safety" = c("Traffic Safety"),
          "Social Cohesion" = c("Social Cohesion & Capital", "Social Factors", "Social"),
          "Crime & Safety" = c("Crime & Safety"),
          "Built Environment" = c("Built Environment"),
          "Disability" = c("Disability"),
          "Health Behaviors" = c("Health Behaviors"),
          "Race/Ethnicity" = c("Race/Ethnicity")
        )
        
        # Count variables by category
        category_counts <- consolidated %>%
          group_by(category) %>%
          summarise(count = n()) %>%
          arrange(desc(count))
        
        # Calculate counts for each domain
        domain_counts <- list()
        for (domain in names(domain_mapping)) {
          categories <- domain_mapping[[domain]]
          count <- sum(category_counts$count[category_counts$category %in% categories], na.rm = TRUE)
          domain_counts[[domain]] <- count
        }
        
        # Create updated table rows
        updated_rows <- c()
        for (row in table_rows) {
          # Check if this row contains a domain count
          updated <- FALSE
          for (domain in names(domain_mapping)) {
            if (grepl(paste0("\\| ", domain, " \\|"), row)) {
              # Extract the count for this domain
              count <- domain_counts[[domain]]
              if (!is.null(count) && count > 0) {
                # Replace the count in the row
                updated_row <- gsub("\\| \\d+ \\|", paste0("| ", count, " |"), row)
                updated_rows <- c(updated_rows, updated_row)
                updated <- TRUE
                break
              }
            }
          }
          
          # If not updated, keep the original row
          if (!updated) {
            updated_rows <- c(updated_rows, row)
          }
        }
        
        # Create the total row
        total_count <- nrow(consolidated)
        total_row <- paste0("| **Total** | **", total_count, "** | |")
        
        # Build the updated README
        updated_readme <- c(
          readme_content[1:summary_table_start],
          updated_rows,
          total_row,
          readme_content[(summary_table_end+1):length(readme_content)]
        )
        
        # Create a backup of the README
        backup_readme <- paste0(readme_file, ".bak")
        file.copy(readme_file, backup_readme, overwrite = TRUE)
        cat("Created backup of README at", backup_readme, "\n")
        
        # Write the updated README
        writeLines(updated_readme, readme_file)
        cat("Updated README with corrected variable counts\n")
      }
    }
  }
  
  # Step 11: Update the database metadata if it exists
  db_file <- "us_county_sdoh_data.duckdb"
  if (file.exists(db_file)) {
    cat("\nChecking database metadata...\n")
    
    tryCatch({
      library(DBI)
      library(duckdb)
      
      con <- dbConnect(duckdb(), db_file)
      
      # Check if the variable_crosswalk table exists
      tables <- dbListTables(con)
      if ("variable_crosswalk" %in% tables) {
        cat("Updating variable_crosswalk table in database...\n")
        
        # Delete existing table
        dbExecute(con, "DROP TABLE IF EXISTS variable_crosswalk")
        
        # Create a new table with consolidated data
        dbWriteTable(con, "variable_crosswalk", consolidated)
        cat("Successfully updated variable_crosswalk table in database\n")
      }
      
      # Check if the data_dictionary table exists
      if ("data_dictionary" %in% tables) {
        cat("Updating data_dictionary table in database...\n")
        
        # Delete existing table
        dbExecute(con, "DROP TABLE IF EXISTS data_dictionary")
        
        # Create a new data dictionary table with only columns that exist
        cols_to_select <- c()
        if ("variable_name" %in% names(consolidated)) cols_to_select <- c(cols_to_select, "std_name = variable_name")
        if ("description" %in% names(consolidated)) cols_to_select <- c(cols_to_select, "description")
        if ("category" %in% names(consolidated)) cols_to_select <- c(cols_to_select, "category")
        if ("preferred_source" %in% names(consolidated)) cols_to_select <- c(cols_to_select, "preferred_source")
        if ("years_available" %in% names(consolidated)) cols_to_select <- c(cols_to_select, "years_available")
        if ("interpolate_recommended" %in% names(consolidated)) cols_to_select <- c(cols_to_select, "interpolate_recommended")
        if ("extend_backwards" %in% names(consolidated)) cols_to_select <- c(cols_to_select, "extend_backwards")
        if ("extend_method" %in% names(consolidated)) cols_to_select <- c(cols_to_select, "extend_method")
        
        if (length(cols_to_select) > 0) {
          # Use parse_expr and eval_tidy to dynamically create the select statement
          select_expr <- rlang::parse_expr(paste0("select(", paste(cols_to_select, collapse=", "), ")"))
          dictionary <- rlang::eval_tidy(select_expr, consolidated)
          
          # Add computed columns
          dictionary <- dictionary %>%
            mutate(
              available_in_dataset = TRUE,
              column_in_db = if ("std_name" %in% names(dictionary)) {
                paste0(std_name, " (+ ", std_name, "_interpolated, ", std_name, "_extended flags)")
              } else {
                "column_name (with flags)"
              }
            )
        } else {
          # Create a minimal dictionary
          dictionary <- data.frame(
            std_name = if ("variable_name" %in% names(consolidated)) consolidated$variable_name else paste0("var_", seq_len(nrow(consolidated))),
            available_in_dataset = TRUE,
            column_in_db = "column_name (with flags)"
          )
        }
        
        dbWriteTable(con, "data_dictionary", dictionary)
        cat("Successfully updated data_dictionary table in database\n")
      }
      
      dbDisconnect(con)
    }, error = function(e) {
      cat("Error updating database metadata:", conditionMessage(e), "\n")
    })
  }
  
  # Step 12: Report
  cat("\n=================================================\n")
  cat("CROSSWALK CONSOLIDATION COMPLETE\n")
  cat("=================================================\n\n")
  
  cat("Successfully consolidated", length(crosswalks), "crosswalk files into a single definitive file.\n")
  cat("- Total variables in consolidated crosswalk:", nrow(consolidated), "\n")
  
  # Print category distribution if the category column exists
  if ("category" %in% names(consolidated)) {
    cat("- Variables by category:\n")
    print(consolidated %>% group_by(category) %>% summarise(count = n()) %>% arrange(desc(count)))
  } else {
    cat("- No category information available in the crosswalk.\n")
    cat("  Consider adding a 'category' column for better organization.\n")
  }
  
  cat("\nUpdated files:\n")
  cat("1. Created primary consolidated file:", consolidated_file, "\n")
  for (file in valid_files) {
    cat("2. Updated original location:", file, "\n")
  }
  cat("3. Created extended data dictionary:", dict_file, "\n")
  if (file.exists(readme_file)) {
    cat("4. Updated README with corrected variable counts:", readme_file, "\n")
  }
  
  cat("\nNext steps:\n")
  cat("1. Run the unified pipeline to ensure all data sources are properly loaded\n")
  cat("2. Verify the updated variable counts with verify_variables.r\n")
  
  return(TRUE)
}

# Execute the function if run directly
if (!is_sourced()) {
  consolidate_crosswalks()
}