#!/usr/bin/env Rscript

# fix_variable_processing.r
# This script attempts to fix issues with variable processing in the SDOH pipeline
# by ensuring all data modules are correctly integrated and variables are properly defined.

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

fix_variable_processing <- function() {
  cat("Starting SDOH pipeline variable processing fix...\n")
  
  # Step 1: Verify the current state of the pipeline
  cat("First, let's analyze the current state...\n")
  if (file.exists("verify_variables.r")) {
    source("verify_variables.r")
    initial_state <- verify_variables()
  } else {
    cat("WARNING: verify_variables.r not found, skipping verification\n")
  }
  
  # Step 2: Check if all data fetchers exist and are functional
  cat("\nVerifying all required data fetchers...\n")
  expected_fetchers <- c(
    "fetch_usda_food_atlas.r",
    "fetch_epa_data.r",
    "fetch_housing_data.r", 
    "fetch_healthcare_data.r",
    "fetch_transportation_data.r",
    "fetch_social_cohesion_data.r",
    "fetch_crime_data.r",
    "fetch_education_data.r",
    "fetch_economic_data.r",
    "fetch_built_environment_data.r",
    "fetch_climate_data.r",
    "fetch_substance_use_data.r",
    "fetch_digital_access_data.r",
    "fetch_traffic_safety_data.r",
    "fetch_county_data_final.r",
    "fetch_extended_data.r",
    "fetch_nhgis_data.r",
    "fetch_historical_data.r"
  )
  
  # Track which fetchers need to be fixed
  fetchers_to_fix <- c()
  
  for (fetcher in expected_fetchers) {
    if (!file.exists(fetcher)) {
      cat("Missing fetcher:", fetcher, "\n")
      fetchers_to_fix <- c(fetchers_to_fix, fetcher)
    } else {
      # Check if the file contains a function with the expected name
      fetcher_content <- readLines(fetcher)
      function_name <- gsub("\\.r$", "", fetcher)
      if (!any(grepl(paste0(function_name, "\\s*<-\\s*function"), fetcher_content))) {
        cat("Fetcher exists but doesn't contain expected function:", fetcher, "\n")
        fetchers_to_fix <- c(fetchers_to_fix, fetcher)
      }
    }
  }
  
  # Step 3: Fix the unified_sdoh_pipeline.r script to ensure all modules are loaded properly
  cat("\nChecking the unified_sdoh_pipeline.r script for proper module loading...\n")
  pipeline_file <- "unified_sdoh_pipeline.r"
  if (!file.exists(pipeline_file)) {
    cat("ERROR: Could not find unified_sdoh_pipeline.r\n")
    return(FALSE)
  }
  
  pipeline_content <- readLines(pipeline_file)
  
  # Check if all expected fetchers are included in the extended_fetchers array
  extended_fetchers_line <- grep("extended_fetchers\\s*<-\\s*c\\(", pipeline_content)
  if (length(extended_fetchers_line) == 0) {
    cat("ERROR: Could not find extended_fetchers array in pipeline script\n")
    return(FALSE)
  }
  
  # Find where the extended_fetchers array ends
  array_end_line <- 0
  for (i in extended_fetchers_line:length(pipeline_content)) {
    if (grepl("\\)", pipeline_content[i])) {
      array_end_line <- i
      break
    }
  }
  
  if (array_end_line == 0) {
    cat("ERROR: Could not find end of extended_fetchers array\n")
    return(FALSE)
  }
  
  # Extract the current array content
  array_content <- pipeline_content[(extended_fetchers_line + 1):(array_end_line - 1)]
  current_fetchers <- gsub("\\s*\"(.+)\"\\s*,?.*", "\\1", array_content)
  
  # Find missing fetchers in the array
  core_fetchers <- c(
    "fetch_usda_food_atlas.r",
    "fetch_epa_data.r",
    "fetch_housing_data.r", 
    "fetch_healthcare_data.r",
    "fetch_transportation_data.r",
    "fetch_social_cohesion_data.r",
    "fetch_crime_data.r",
    "fetch_education_data.r",
    "fetch_economic_data.r",
    "fetch_built_environment_data.r",
    "fetch_climate_data.r",
    "fetch_substance_use_data.r",
    "fetch_digital_access_data.r",
    "fetch_traffic_safety_data.r",
    "fetch_county_data_final.r"
  )
  
  missing_in_array <- setdiff(core_fetchers, current_fetchers)
  
  if (length(missing_in_array) > 0) {
    cat("The following fetchers are missing from the extended_fetchers array:\n")
    cat(paste("- ", missing_in_array, collapse = "\n"), "\n")
    
    # Update the pipeline script to include missing fetchers
    cat("\nUpdating the unified_sdoh_pipeline.r script...\n")
    
    # Create the updated array content
    updated_array <- c(
      array_content,
      paste0("  # Additional data sources", if (length(missing_in_array) > 0) "" else ""),
      paste0("  \"", missing_in_array, "\",")
    )
    
    # Replace the array in the pipeline content
    new_pipeline_content <- c(
      pipeline_content[1:extended_fetchers_line],
      updated_array,
      pipeline_content[array_end_line:length(pipeline_content)]
    )
    
    # Write the updated pipeline script
    backup_file <- paste0(pipeline_file, ".bak")
    file.copy(pipeline_file, backup_file, overwrite = TRUE)
    cat("Created backup of original pipeline script at:", backup_file, "\n")
    
    writeLines(new_pipeline_content, pipeline_file)
    cat("Updated the unified_sdoh_pipeline.r script to include all fetcher modules\n")
  } else {
    cat("All expected fetchers are already included in the extended_fetchers array\n")
  }
  
  # Step 4: Ensure the crosswalk file correctly represents all variables
  cat("\nChecking the crosswalk file for completeness...\n")
  crosswalk_file <- "variable_crosswalk_extended.csv"
  if (!file.exists(crosswalk_file)) {
    alt_locations <- c(
      "output/variable_crosswalk_extended.csv",
      "../variable_crosswalk_extended.csv",
      "R/variable_crosswalk_extended.csv"
    )
    
    for (loc in alt_locations) {
      if (file.exists(loc)) {
        crosswalk_file <- loc
        cat("Found crosswalk file at:", loc, "\n")
        break
      }
    }
  }
  
  if (!file.exists(crosswalk_file)) {
    cat("ERROR: Could not find variable_crosswalk_extended.csv. Cannot check crosswalk.\n")
  } else {
    # Read the crosswalk file
    crosswalk <- read_csv(crosswalk_file, show_col_types = FALSE)
    crosswalk_var_count <- nrow(crosswalk)
    
    # Check for README to verify expected count
    readme_file <- "README.md"
    if (!file.exists(readme_file)) {
      readme_file <- "R/README.md"
    }
    
    if (file.exists(readme_file)) {
      readme_content <- readLines(readme_file)
      total_lines <- grep("\\*\\*Total\\*\\*", readme_content)
      if (length(total_lines) > 0) {
        total_line <- readme_content[total_lines[1]]
        expected_count <- as.numeric(str_extract(total_line, "\\d+"))
        
        if (!is.na(expected_count) && expected_count > crosswalk_var_count) {
          cat("README mentions", expected_count, "variables but crosswalk only contains", crosswalk_var_count, "\n")
          cat("This suggests that more variables need to be added to the crosswalk.\n")
        }
      }
    }
  }
  
  # Step 5: Check and fix the process_extended_data.r script
  cat("\nChecking the process_extended_data.r script for filtering issues...\n")
  process_file <- "process_extended_data.r"
  if (!file.exists(process_file)) {
    cat("ERROR: Could not find process_extended_data.r\n")
  } else {
    # Read the processing script
    process_content <- readLines(process_file)
    
    # Look for any variables being filtered out
    filter_lines <- grep("available_vars|filter\\(.*variable|select\\(.*variable", process_content)
    if (length(filter_lines) > 0) {
      cat("Found potential variable filtering in the processing script at lines:", paste(filter_lines, collapse=", "), "\n")
      
      # Specifically check the available_vars definition
      available_vars_line <- grep("available_vars\\s*<-", process_content)
      if (length(available_vars_line) > 0) {
        cat("Checking available_vars definition at line", available_vars_line[1], "...\n")
        
        available_vars_def <- process_content[available_vars_line[1]]
        if (grepl("filter|select|subset", available_vars_def)) {
          cat("WARNING: available_vars is being filtered, which may cause variables to be dropped\n")
          cat("Definition:", available_vars_def, "\n")
        } else {
          cat("available_vars appears to be defined correctly\n")
        }
      }
    } else {
      cat("No explicit variable filtering found in the processing script\n")
    }
  }
  
  # Step 6: Generate a comprehensive fix report
  cat("\n=================================================\n")
  cat("VARIABLE PROCESSING FIX REPORT\n")
  cat("=================================================\n\n")
  
  if (length(missing_in_array) > 0) {
    cat("✅ FIXED: Added missing fetchers to the pipeline script\n")
    for (fetcher in missing_in_array) {
      cat("  - Added:", fetcher, "\n")
    }
  } else {
    cat("✓ All fetchers already included in pipeline\n")
  }
  
  if (length(fetchers_to_fix) > 0) {
    cat("\n⚠️ ACTION NEEDED: The following fetcher scripts need to be implemented or fixed:\n")
    for (fetcher in fetchers_to_fix) {
      cat("  - Fix or implement:", fetcher, "\n")
    }
  } else {
    cat("\n✓ All fetcher scripts are present and contain the expected functions\n")
  }
  
  # Final recommendations
  cat("\nFINAL RECOMMENDATIONS:\n")
  
  cat("1. Run the unified pipeline with the --force-update flag to ensure all data is refreshed\n")
  cat("2. Use the --verbose flag to see detailed information about each module's execution\n")
  cat("3. If any data sources require credentials, ensure they are properly configured\n")
  cat("4. After running the pipeline, run verify_variables.r again to check if all variables are now included\n")
  
  cat("\nCommand to run pipeline with all recommended options:\n")
  cat("Rscript unified_sdoh_pipeline.r --force-update --verbose --allow-interpolation\n\n")
  
  return(TRUE)
}

# Execute the function if run directly
if (!is_sourced()) {
  fix_variable_processing()
}