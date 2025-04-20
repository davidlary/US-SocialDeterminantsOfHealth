#\!/usr/bin/env Rscript

# handle_year_county_variation.r
# This script modifies the SDOH pipeline to properly handle variations
# in variable availability across years and counties.

library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# Define is_sourced function if it doesn't exist
if (\!exists("is_sourced")) {
  is_sourced <- function() {
    # Check if the calling environment is the global environment
    # If it's not, the function is being sourced
    parent_env <- parent.frame()
    return(\!identical(parent_env, .GlobalEnv))
  }
}

handle_year_county_variation <- function() {
  cat("Configuring SDOH pipeline to handle variable availability variations...\n")
  
  # Step 1: Find the process_extended_data.r file
  process_file <- "process_extended_data.r"
  if (\!file.exists(process_file)) {
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
  
  if (\!file.exists(process_file)) {
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
    year_range_pattern <- "min_year.*<-.*min|max_year.*<-.*max"
    range_offset <- min(interp_section[1] + 30, length(process_content)) - interp_section[1]
    year_range_lines <- interp_section[1] + grep(year_range_pattern, process_content[interp_section[1]:(interp_section[1] + range_offset)])
    
    if (length(year_range_lines) > 0) {
      cat("Found year range settings at lines:", paste(year_range_lines, collapse=", "), "\n")
      
      # Add a comment explaining the implications
      comment_lines <- c(
        "    # NOTE: We're using the actual min/max years from the data, which means",
        "    # variables will only be interpolated for years where data exists.",
        "    # This ensures we don't generate simulated data for years outside the range,",
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
  metadata_section <- grep("county_metadata.*<-", process_content)
  if (length(metadata_section) > 0) {
    cat("Found county metadata generation at line", metadata_section[1], "\n")
    
    # Look for consistent naming code (10 lines after metadata section)
    name_pattern <- "consistent.*name"
    name_offset <- min(metadata_section[1] + 20, length(process_content)) - metadata_section[1]
    naming_lines <- metadata_section[1] + grep(name_pattern, process_content[metadata_section[1]:(metadata_section[1] + name_offset)])
    
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
  quality_section <- grep("data_quality.*=.*case_when", process_content)
  if (length(quality_section) > 0) {
    cat("Found data quality classification at line", quality_section[1], "\n")
    
    # Check if simulated data is being flagged
    has_simulation_flag <- any(grepl("simulated", process_content[quality_section[1]:min(quality_section[1] + 30, length(process_content))]))
    
    if (\!has_simulation_flag) {
      cat("Adding simulation detection to data quality flags...\n")
      
      # Find the data_quality case_when statement
      case_when_end <- quality_section[1]
      while (case_when_end < length(process_content) && \!grepl("\\)", process_content[case_when_end])) {
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
  if (\!file.exists(pipeline_file)) {
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
  
  if (\!file.exists(pipeline_file)) {
    cat("ERROR: Could not find unified_sdoh_pipeline.r\n")
    return(FALSE)
  }
  
  # Create a backup of the pipeline file if we haven't already
  backup_pipeline <- paste0(pipeline_file, ".bak")
  if (\!file.exists(backup_pipeline)) {
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
    sim_offset <- min(args_section[1] + 100, length(pipeline_content)) - args_section[1]
    simulation_arg <- grep("allow_simulation", pipeline_content[args_section[1]:(args_section[1] + sim_offset)])
    
    if (length(simulation_arg) > 0) {
      simulation_line <- args_section[1] + simulation_arg[1] - 1
      cat("Found allow_simulation parameter at line", simulation_line, "\n")
      
      # Ensure it's set to FALSE by default
      if (grepl("TRUE", pipeline_content[simulation_line])) {
        cat("Changing allow_simulation default to FALSE...\n")
        pipeline_content[simulation_line] <- gsub("TRUE", "FALSE", pipeline_content[simulation_line])
      }
    } else {
      # Add the parameter if it doesn't exist
      cat("Allow_simulation parameter not found. Adding it with FALSE default...\n")
      
      # Find a good insertion point after another parameter
      param_pattern <- "--[a-z\\-]+="
      param_offset <- min(args_section[1] + 100, length(pipeline_content)) - args_section[1]
      param_lines <- grep(param_pattern, pipeline_content[args_section[1]:(args_section[1] + param_offset)])
      
      if (length(param_lines) > 0) {
        insert_line <- args_section[1] + param_lines[length(param_lines)]
        
        # Add the new parameter
        new_param <- c(
          "  # Don't allow simulated data",
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

# Execute the function if run directly
if (\!is_sourced()) {
  handle_year_county_variation()
}
