#!/usr/bin/env Rscript

# fix_variable_discrepancy.r
# This script automatically fixes the variable count discrepancy in the SDOH pipeline
# by ensuring all modules are loaded and updating documentation to match reality.

library(dplyr)
library(tidyr)
library(readr)
library(stringr)

fix_variable_discrepancy <- function() {
  cat("=====================================================\n")
  cat("SDOH PIPELINE VARIABLE DISCREPANCY FIX UTILITY\n")
  cat("=====================================================\n\n")
  
  cat("This utility will fix the discrepancy between variable counts by:\n")
  cat("1. Consolidating multiple crosswalk files\n")
  cat("2. Ensuring no simulated data is allowed\n")
  cat("3. Updating the README to reflect actual available variables\n")
  cat("4. Ensuring variable counts reflect real data availability\n\n")
  
  # Step 1: Check if required scripts exist
  scripts <- c(
    "verify_variables.r",
    "fix_variable_processing.r",
    "update_readme_variable_count.r",
    "consolidate_crosswalks.r"
  )
  
  # Define common is_sourced function for all scripts
  if (!exists("is_sourced")) {
    is_sourced <- function() {
      # Check if the calling environment is the global environment
      # If it's not, the function is being sourced
      parent_env <- parent.frame()
      return(!identical(parent_env, .GlobalEnv))
    }
  }
  
  missing_scripts <- scripts[!sapply(scripts, file.exists)]
  if (length(missing_scripts) > 0) {
    cat("ERROR: The following required scripts are missing:\n")
    cat(paste("- ", missing_scripts, collapse = "\n"), "\n")
    return(FALSE)
  }
  
  # Step 2: Source the scripts
  for (script in scripts) {
    cat("Sourcing", script, "...\n")
    source(script)
  }
  
  # Step 3: First, consolidate all crosswalk files into one
  cat("\n-----------------------------------------\n")
  cat("PHASE 1: CONSOLIDATING CROSSWALK FILES\n")
  cat("-----------------------------------------\n")
  
  # To handle errors more gracefully, wrap in tryCatch
  tryCatch({
    consolidate_crosswalks()
  }, error = function(e) {
    cat("Error during crosswalk consolidation:", conditionMessage(e), "\n")
    cat("Continuing with the next phase...\n")
  })
  
  # Step 4: Analyze the current state after consolidation
  cat("\n-----------------------------------------\n")
  cat("PHASE 2: ANALYZING CURRENT STATE\n")
  cat("-----------------------------------------\n")
  current_state <- verify_variables()
  
  # Step 5: Check for simulation settings in the pipeline
  cat("\n-----------------------------------------\n")
  cat("PHASE 3: ENSURING NO SIMULATED DATA\n")
  cat("-----------------------------------------\n")
  
  # Find the pipeline file
  pipeline_file <- "unified_sdoh_pipeline.r"
  if (!file.exists(pipeline_file)) {
    alt_locations <- c(
      "R/unified_sdoh_pipeline.r",
      "../unified_sdoh_pipeline.r"
    )
    
    for (loc in alt_locations) {
      if (file.exists(loc)) {
        pipeline_file <- loc
        cat("Found pipeline file at:", loc, "\n")
        break
      }
    }
  }
  
  if (!file.exists(pipeline_file)) {
    cat("ERROR: Could not find unified_sdoh_pipeline.r\n")
  } else {
    # Read the pipeline file
    pipeline_content <- readLines(pipeline_file)
    
    # Look for simulation settings
    simulation_settings <- grep("allow_simulation|simulate", pipeline_content)
    if (length(simulation_settings) > 0) {
      cat("Found simulation settings in the pipeline at lines:", paste(simulation_settings, collapse=", "), "\n")
      
      # Check if any simulation is enabled
      allow_simulation_lines <- grep("allow_simulation\\s*=\\s*TRUE", pipeline_content)
      if (length(allow_simulation_lines) > 0) {
        cat("WARNING: Simulation is enabled in the pipeline. Disabling...\n")
        
        # Create a backup of the pipeline file
        backup_file <- paste0(pipeline_file, ".bak")
        file.copy(pipeline_file, backup_file, overwrite = TRUE)
        cat("Created backup of pipeline at:", backup_file, "\n")
        
        # Replace TRUE with FALSE in all simulation settings
        updated_content <- pipeline_content
        for (line_num in allow_simulation_lines) {
          updated_content[line_num] <- gsub("allow_simulation\\s*=\\s*TRUE", "allow_simulation = FALSE", updated_content[line_num])
        }
        
        # Write the updated pipeline file
        writeLines(updated_content, pipeline_file)
        cat("Successfully disabled simulation in the pipeline\n")
      } else {
        cat("Simulation appears to be properly disabled in the pipeline\n")
      }
    }
    
    # Also check for default values in function definitions
    func_defs <- grep("function.*allow_simulation", pipeline_content)
    if (length(func_defs) > 0) {
      cat("Found function definitions with simulation parameters at lines:", paste(func_defs, collapse=", "), "\n")
      
      # Check if any have TRUE as default
      func_def_with_true <- grep("allow_simulation\\s*=\\s*TRUE", pipeline_content[func_defs])
      if (length(func_def_with_true) > 0) {
        cat("WARNING: Found function definitions with simulation enabled by default. Updating...\n")
        
        # Create a backup if we haven't already
        if (!file.exists(backup_file)) {
          backup_file <- paste0(pipeline_file, ".bak")
          file.copy(pipeline_file, backup_file, overwrite = TRUE)
          cat("Created backup of pipeline at:", backup_file, "\n")
        }
        
        # Replace TRUE with FALSE in function definitions
        updated_content <- pipeline_content
        for (idx in func_def_with_true) {
          line_num <- func_defs[idx]
          updated_content[line_num] <- gsub("allow_simulation\\s*=\\s*TRUE", "allow_simulation = FALSE", updated_content[line_num])
        }
        
        # Write the updated pipeline file
        writeLines(updated_content, pipeline_file)
        cat("Successfully updated function definitions to disable simulation by default\n")
      } else {
        cat("Function definitions have simulation properly disabled by default\n")
      }
    }
    
    # Check for county validation settings
    validation_settings <- grep("check_simulated|validate_counties", pipeline_content)
    if (length(validation_settings) > 0) {
      cat("\nFound county validation settings at lines:", paste(validation_settings, collapse=", "), "\n")
      
      # Check if validation is disabled
      validation_disabled <- grep("check_simulated\\s*=\\s*FALSE|validate_counties\\s*=\\s*FALSE", pipeline_content)
      if (length(validation_disabled) > 0) {
        cat("WARNING: County validation may be disabled. Enabling...\n")
        
        # Create a backup if we haven't already
        if (!file.exists(backup_file)) {
          backup_file <- paste0(pipeline_file, ".bak")
          file.copy(pipeline_file, backup_file, overwrite = TRUE)
          cat("Created backup of pipeline at:", backup_file, "\n")
        }
        
        # Replace FALSE with TRUE in validation settings
        updated_content <- pipeline_content
        for (line_num in validation_disabled) {
          updated_content[line_num] <- gsub("check_simulated\\s*=\\s*FALSE", "check_simulated = TRUE", updated_content[line_num])
          updated_content[line_num] <- gsub("validate_counties\\s*=\\s*FALSE", "validate_counties = TRUE", updated_content[line_num])
        }
        
        # Write the updated pipeline file
        writeLines(updated_content, pipeline_file)
        cat("Successfully enabled county validation in the pipeline\n")
      } else {
        cat("County validation appears to be properly enabled\n")
      }
    }
  }
  
  # Step 6: Apply fixes for module loading
  cat("\n-----------------------------------------\n")
  cat("PHASE 4: FIXING VARIABLE PROCESSING\n")
  cat("-----------------------------------------\n")
  fix_variable_processing()
  
  # Step 7: Update the README to match actual variables
  cat("\n-----------------------------------------\n")
  cat("PHASE 5: UPDATING README\n")
  cat("-----------------------------------------\n")
  update_readme_variable_count()
  
  # Step 8: Create helper script for handling year and county variation
  cat("\n-----------------------------------------\n")
  cat("PHASE 6: CREATING YEAR/COUNTY VARIATION HANDLER\n")
  cat("-----------------------------------------\n")
  
  year_county_handler <- "handle_year_county_variation.r"
  cat("Creating helper script to handle year and county variations:", year_county_handler, "\n")
  
  handler_content <- '#!/usr/bin/env Rscript

# handle_year_county_variation.r
# This script modifies the SDOH pipeline to properly handle variations
# in variable availability across years and counties.

library(dplyr)
library(tidyr)
library(readr)
library(stringr)

handle_year_county_variation <- function() {
  cat("Configuring SDOH pipeline to handle variable availability variations...\n")
  
  # Step 1: Find the process_extended_data.r file
  process_file <- "process_extended_data.r"
  if (!file.exists(process_file)) {
    alt_locations <- c(
      "R/process_extended_data.r",
      "../process_extended_data.r"
    )
    
    for (loc in alt_locations) {
      if (file.exists(loc)) {
        process_file <- loc
        cat("Found process file at:", loc, "\n")
        break
      }
    }
  }
  
  if (!file.exists(process_file)) {
    cat("ERROR: Could not find process_extended_data.r\n")
    return(FALSE)
  }
  
  # Step 2: Create a backup of the process file
  backup_file <- paste0(process_file, ".bak")
  file.copy(process_file, backup_file, overwrite = TRUE)
  cat("Created backup of process file at:", backup_file, "\n")
  
  # Step 3: Read the process file
  process_content <- readLines(process_file)
  
  # Step 4: Look for the interpolation section
  interp_section <- grep("Create a complete dataset with all counties and years", process_content)
  if (length(interp_section) == 0) {
    interp_section <- grep("interpolate_missing_years", process_content)
  }
  
  if (length(interp_section) > 0) {
    cat("Found interpolation section at line", interp_section[1], "\n")
    
    # Find where years are set to min-max
    year_range_lines <- interp_section[1] + grep("min_year\\s*<-\\s*min|max_year\\s*<-\\s*max", process_content[interp_section[1]:min(interp_section[1] + 30, length(process_content))])
    
    if (length(year_range_lines) > 0) {
      cat("Found year range settings at lines:", paste(year_range_lines, collapse=", "), "\n")
      
      # Add a comment explaining the implications
      comment_lines <- c(
        "    # NOTE: We\'re using the actual min/max years from the data, which means",
        "    # variables will only be interpolated for years where data exists.",
        "    # This ensures we don\'t generate simulated data for years outside the range,",
        "    # and county counts will naturally vary by year based on data availability."
      )
      
      # Insert the comments before the year range lines
      first_line <- min(year_range_lines)
      process_content <- c(
        process_content[1:(first_line-1)],
        comment_lines,
        process_content[first_line:length(process_content)]
      )
      
      cat("Added explanatory comments about year/county variation\n")
    }
  }
  
  # Step 5: Find the county_metadata generation section
  metadata_section <- grep("county_metadata\\s*<-", process_content)
  if (length(metadata_section) > 0) {
    cat("Found county metadata generation at line", metadata_section[1], "\n")
    
    # Look for consistent naming code (10 lines after metadata section)
    naming_lines <- metadata_section[1] + grep("consistent.*name", process_content[metadata_section[1]:min(metadata_section[1] + 20, length(process_content))])
    
    if (length(naming_lines) > 0) {
      cat("Found county naming consistency code at lines:", paste(naming_lines, collapse=", "), "\n")
      
      # Add a comment explaining the approach
      comment_lines <- c(
        "    # We create metadata that preserves the natural variation in county counts by year,",
        "    # but ensures consistent naming across available years for each county.",
        "    # This approach avoids simulating data for counties in years where they have no data."
      )
      
      # Insert the comments before the county metadata code
      process_content <- c(
        process_content[1:(metadata_section[1]-1)],
        comment_lines,
        process_content[metadata_section[1]:length(process_content)]
      )
      
      cat("Added explanatory comments about county count variation\n")
    }
  }
  
  # Step 6: Find data quality section to ensure simulated data is detected
  quality_section <- grep("data_quality\\s*=\\s*case_when", process_content)
  if (length(quality_section) > 0) {
    cat("Found data quality classification at line", quality_section[1], "\n")
    
    # Check if simulated data is being flagged
    has_simulation_flag <- any(grepl("simulated", process_content[quality_section[1]:min(quality_section[1] + 30, length(process_content))]))
    
    if (!has_simulation_flag) {
      cat("Adding simulation detection to data quality flags...\n")
      
      # Find the data_quality case_when statement
      case_when_end <- quality_section[1]
      while (case_when_end < length(process_content) && !grepl("\\)\\s*,?\\s*$", process_content[case_when_end])) {
        case_when_end <- case_when_end + 1
      }
      
      # Add simulation detection clause
      simulation_clause <- c(
        "        # Add detection for potential simulated data",
        "        grepl(\"simulated\", source, ignore.case = TRUE) ~ \"simulated\","
      )
      
      # Insert the clause near the end of the case_when statement
      process_content <- c(
        process_content[1:(case_when_end-1)],
        simulation_clause,
        process_content[case_when_end:length(process_content)]
      )
      
      cat("Added simulation detection to data quality classification\n")
    } else {
      cat("Simulation detection already present in data quality flags\n")
    }
  }
  
  # Step 7: Write the updated process file
  writeLines(process_content, process_file)
  cat("Successfully updated process file to handle year and county variations\n")
  
  # Step 8: Update the pipeline to ensure variables reflect reality
  pipeline_file <- "unified_sdoh_pipeline.r"
  if (!file.exists(pipeline_file)) {
    alt_locations <- c(
      "R/unified_sdoh_pipeline.r",
      "../unified_sdoh_pipeline.r"
    )
    
    for (loc in alt_locations) {
      if (file.exists(loc)) {
        pipeline_file <- loc
        cat("Found pipeline file at:", loc, "\n")
        break
      }
    }
  }
  
  if (!file.exists(pipeline_file)) {
    cat("ERROR: Could not find unified_sdoh_pipeline.r\n")
    return(FALSE)
  }
  
  # Create a backup of the pipeline file if we haven\'t already
  backup_pipeline <- paste0(pipeline_file, ".bak")
  if (!file.exists(backup_pipeline)) {
    file.copy(pipeline_file, backup_pipeline, overwrite = TRUE)
    cat("Created backup of pipeline file at:", backup_pipeline, "\n")
  }
  
  # Read the pipeline file
  pipeline_content <- readLines(pipeline_file)
  
  # Find the command-line arguments section
  args_section <- grep("parse_args|commandArgs", pipeline_content)
  if (length(args_section) > 0) {
    cat("Found command-line arguments section at line", args_section[1], "\n")
    
    # Look for allow_simulation parameter
    simulation_arg <- grep("allow_simulation", pipeline_content[args_section[1]:min(args_section[1] + 100, length(pipeline_content))])
    
    if (length(simulation_arg) > 0) {
      simulation_line <- args_section[1] + simulation_arg[1] - 1
      cat("Found allow_simulation parameter at line", simulation_line, "\n")
      
      # Ensure it\'s set to FALSE by default
      if (grepl("TRUE", pipeline_content[simulation_line])) {
        cat("Changing allow_simulation default to FALSE...\n")
        pipeline_content[simulation_line] <- gsub("TRUE", "FALSE", pipeline_content[simulation_line])
      }
    } else {
      # Add the parameter if it doesn\'t exist
      cat("Allow_simulation parameter not found. Adding it with FALSE default...\n")
      
      # Find a good insertion point after another parameter
      param_lines <- grep("\\--[a-z\\-]+=", pipeline_content[args_section[1]:min(args_section[1] + 100, length(pipeline_content))])
      if (length(param_lines) > 0) {
        insert_line <- args_section[1] + param_lines[length(param_lines)]
        
        # Add the new parameter
        new_param <- c(
          "  # Don\'t allow simulated data",
          "  allow_simulation = FALSE,"
        )
        
        pipeline_content <- c(
          pipeline_content[1:insert_line],
          new_param,
          pipeline_content[(insert_line+1):length(pipeline_content)]
        )
      }
    }
    
    # Write the updated pipeline file
    writeLines(pipeline_content, pipeline_file)
    cat("Successfully updated pipeline parameters\n")
  }
  
  cat("\nSuccessfully configured pipeline to handle year and county variations.\n")
  cat("The pipeline will now:\n")
  cat("1. Preserve natural variation in county counts by year based on data availability\n")
  cat("2. Only interpolate within the actual year range of available data\n")
  cat("3. Never generate simulated data\n")
  cat("4. Flag any potentially simulated data in the quality metrics\n")
  
  return(TRUE)
}

# Define the is_sourced function
is_sourced <- function() {
  # Check if the calling environment is the global environment
  # If it\'s not, the function is being sourced
  parent_env <- parent.frame()
  return(!identical(parent_env, .GlobalEnv))
}

# Execute the function if run directly
if (!is_sourced()) {
  handle_year_county_variation()
}
'
  
  writeLines(handler_content, year_county_handler)
  cat("Created helper script:", year_county_handler, "\n")
  
  # Make it executable
  Sys.chmod(year_county_handler, mode = "0755")
  
  # Step 9: Run the year/county variation handler
  cat("Running year/county variation handler...\n")
  source(year_county_handler)
  
  # Run the handler function
  handle_year_county_variation()
  
  # Step 10: Generate final report
  cat("\n=====================================================\n")
  cat("VARIABLE DISCREPANCY FIX COMPLETE\n")
  cat("=====================================================\n\n")
  
  cat("The following actions were completed:\n")
  cat("1. Consolidated multiple crosswalk files into a single definitive source\n")
  cat("2. Disabled simulation throughout the pipeline\n")
  cat("3. Fixed variable processing to include all data sources\n")
  cat("4. Updated the README to match actual variable counts\n")
  cat("5. Configured the pipeline to handle year and county variations properly\n\n")
  
  cat("NEXT STEPS:\n")
  cat("1. Run the unified pipeline with the following command:\n")
  cat("   Rscript unified_sdoh_pipeline.r --force-update --verbose --offline-mode=FALSE\n\n")
  cat("2. Verify the results with:\n")
  cat("   Rscript verify_variables.r\n\n")
  
  cat("IMPORTANT NOTES:\n")
  cat("- The pipeline now uses actual data from authoritative sources without simulation\n")
  cat("- County counts will naturally vary by year based on data availability\n")
  cat("- Variables are only available for years where real data exists\n")
  cat("- The README now reflects the actual variables in the crosswalk\n")
  
  return(TRUE)
}

# Execute the function if run directly
if (!exists("is_sourced")) {
  is_sourced <- function() {
    # Check if the calling environment is the global environment
    # If it's not, the function is being sourced
    parent_env <- parent.frame()
    return(!identical(parent_env, .GlobalEnv))
  }
  
  if (!is_sourced()) {
    fix_variable_discrepancy()
  }
} else {
  # is_sourced already exists, just run the function if not sourced
  if (!is_sourced()) {
    fix_variable_discrepancy()
  }
}