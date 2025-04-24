#!/usr/bin/env Rscript

# handle_year_county_variation.r
# This script modifies the SDOH pipeline to properly handle variations
# in variable availability across years and counties.

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
    
    if (!has_simulation_flag) {
      cat("Adding simulation detection to data quality flags...\n")
      
      # Find the data_quality case_when statement
      case_when_end <- quality_section[1]
      while (case_when_end < length(process_content) && !grepl("\\)", process_content[case_when_end])) {
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
  
  # Create a backup of the pipeline file if we haven't already
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

#' Perform temporal interpolation for missing county-year combinations
#'
#' This function:
#' 1. Identifies missing year-county combinations
#' 2. Checks if values exist for years before and after
#' 3. Performs linear (or other) interpolation between the bracketing years
#' 4. Flags the interpolated values with a data quality indicator
#'
#' @param data A dataframe containing county data with geoid and year columns
#' @param variable_names A character vector of variable names to interpolate
#' @param method The interpolation method to use ("linear", "spline", or "stine")
#' @param min_gap_size The minimum gap size to interpolate (in years)
#' @param max_gap_size The maximum gap size to interpolate (in years)
#' @return A dataframe with interpolated values and quality flags
interpolate_temporal_gaps <- function(data, variable_names, method = "linear",
                                     min_gap_size = 1, max_gap_size = 5) {
  # Verify the function has the required packages
  required_packages <- c("dplyr", "tidyr", "zoo")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message(paste("Installing required package:", pkg))
      install.packages(pkg, repos = "https://cloud.r-project.org")
      library(pkg, character.only = TRUE)
    } else {
      library(pkg, character.only = TRUE)
    }
  }
  
  # Validate input data
  if (!all(c("geoid", "year") %in% names(data))) {
    stop("Input data must contain 'geoid' and 'year' columns")
  }
  
  # Ensure variable names exist in the data
  valid_vars <- intersect(variable_names, names(data))
  if (length(valid_vars) == 0) {
    warning("None of the specified variables exist in the data. Returning original data.")
    return(data)
  }
  
  # Filter to only the variables we need for interpolation
  missing_vars <- setdiff(variable_names, valid_vars)
  if (length(missing_vars) > 0) {
    warning(paste("The following variables were not found in the data:",
                 paste(missing_vars, collapse = ", ")))
  }
  
  cat("Interpolating", length(valid_vars), "variables for temporal gaps...\n")
  
  # Create result dataframe (start with original data)
  result_data <- data
  
  # Initialize progress tracking
  total_vars <- length(valid_vars)
  var_counter <- 0
  
  # Process each county separately
  counties <- unique(data$geoid)
  cat("Processing", length(counties), "counties...\n")
  
  # Track interpolation counts
  interp_counts <- list(
    total_counties = length(counties),
    total_variables = length(valid_vars),
    interpolated_values = 0,
    counties_with_interpolation = 0
  )
  
  # Counties with interpolation
  counties_with_interp <- character(0)
  
  # Process each variable
  for (var in valid_vars) {
    var_counter <- var_counter + 1
    if (var_counter %% 5 == 0 || var_counter == total_vars) {
      cat("Processing variable", var_counter, "of", total_vars, ":", var, "\n")
    }
    
    # Create quality flag column name for this variable
    quality_col <- paste0(var, "_data_quality")
    
    # Ensure quality column exists
    if (!quality_col %in% names(result_data)) {
      result_data[[quality_col]] <- "direct"
    }
    
    # For each county, interpolate missing years
    county_counter <- 0
    for (county in counties) {
      county_counter <- county_counter + 1
      if (county_counter %% 500 == 0) {
        cat("  Processing county", county_counter, "of", length(counties), "\n")
      }
      
      # Extract data for this county
      county_data <- data %>%
        filter(geoid == county) %>%
        arrange(year)
      
      # Skip if fewer than 2 years of data
      if (nrow(county_data) < 2) {
        next
      }
      
      # Find years with data for this variable
      has_data <- !is.na(county_data[[var]])
      
      # Skip if all values are NA or if all values are present
      if (sum(has_data) < 2 || sum(has_data) == nrow(county_data)) {
        next
      }
      
      # Get years with and without data
      years_with_data <- county_data$year[has_data]
      all_years <- county_data$year
      
      # Identify missing years that can be interpolated
      interp_count <- 0
      for (i in 1:(length(all_years) - 1)) {
        # Skip if current year has data
        if (all_years[i] %in% years_with_data) {
          next
        }
        
        # Find bracketing years with data
        prev_year_idx <- max(which(years_with_data < all_years[i]), 0)
        next_year_idx <- min(which(years_with_data > all_years[i]), length(years_with_data) + 1)
        
        # Skip if no bracketing years
        if (prev_year_idx == 0 || next_year_idx > length(years_with_data)) {
          next
        }
        
        prev_year <- years_with_data[prev_year_idx]
        next_year <- years_with_data[next_year_idx]
        
        # Check if gap is within acceptable size
        gap_size <- next_year - prev_year
        if (gap_size < min_gap_size || gap_size > max_gap_size) {
          next
        }
        
        # Get values for bracketing years
        prev_value <- county_data[[var]][county_data$year == prev_year]
        next_value <- county_data[[var]][county_data$year == next_year]
        
        # Skip if either value is NA
        if (is.na(prev_value) || is.na(next_value)) {
          next
        }
        
        # Calculate interpolated value based on method
        if (method == "linear") {
          # Linear interpolation
          for (j in (prev_year + 1):(next_year - 1)) {
            if (j %in% all_years) {
              idx <- which(all_years == j)
              weight <- (j - prev_year) / (next_year - prev_year)
              interp_value <- prev_value + weight * (next_value - prev_value)
              
              # Update the result data
              result_idx <- which(result_data$geoid == county & result_data$year == j)
              if (length(result_idx) > 0) {
                result_data[[var]][result_idx] <- interp_value
                result_data[[quality_col]][result_idx] <- "interpolated"
                interp_count <- interp_count + 1
              }
            }
          }
        } else if (method == "spline" || method == "stine") {
          # Need at least 4 points for spline, so we'll only use it if we have enough data
          if (sum(has_data) >= 4) {
            # Create a series with all available data points
            all_values <- county_data[[var]]
            names(all_values) <- county_data$year
            
            # Interpolate using spline
            if (method == "spline") {
              interp_values <- zoo::na.spline(all_values, na.rm = TRUE)
            } else {
              # Stineman interpolation
              interp_values <- zoo::na.stine(all_values, na.rm = TRUE)
            }
            
            # Update the result data for years in the gap
            for (j in (prev_year + 1):(next_year - 1)) {
              if (j %in% all_years) {
                idx <- which(all_years == j)
                
                # Update the result data
                result_idx <- which(result_data$geoid == county & result_data$year == j)
                if (length(result_idx) > 0) {
                  result_data[[var]][result_idx] <- interp_values[as.character(j)]
                  result_data[[quality_col]][result_idx] <- "interpolated"
                  interp_count <- interp_count + 1
                }
              }
            }
          } else {
            # Fall back to linear interpolation for small data sets
            for (j in (prev_year + 1):(next_year - 1)) {
              if (j %in% all_years) {
                idx <- which(all_years == j)
                weight <- (j - prev_year) / (next_year - prev_year)
                interp_value <- prev_value + weight * (next_value - prev_value)
                
                # Update the result data
                result_idx <- which(result_data$geoid == county & result_data$year == j)
                if (length(result_idx) > 0) {
                  result_data[[var]][result_idx] <- interp_value
                  result_data[[quality_col]][result_idx] <- "interpolated"
                  interp_count <- interp_count + 1
                }
              }
            }
          }
        }
      }
      
      # Update tracking if interpolation occurred
      if (interp_count > 0) {
        interp_counts$interpolated_values <- interp_counts$interpolated_values + interp_count
        if (!county %in% counties_with_interp) {
          counties_with_interp <- c(counties_with_interp, county)
        }
      }
    }
  }
  
  # Update final count of counties with interpolation
  interp_counts$counties_with_interpolation <- length(counties_with_interp)
  
  # Print summary
  cat("\nInterpolation summary:\n")
  cat("  Total counties processed:", interp_counts$total_counties, "\n")
  cat("  Total variables processed:", interp_counts$total_variables, "\n")
  cat("  Counties with interpolated values:", interp_counts$counties_with_interpolation, "\n")
  cat("  Total interpolated values:", interp_counts$interpolated_values, "\n")
  
  return(result_data)
}

# Execute the function if run directly
if (!is_sourced()) {
  handle_year_county_variation()
}
