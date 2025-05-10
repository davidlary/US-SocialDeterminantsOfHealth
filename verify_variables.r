#!/usr/bin/env Rscript

# verify_variables.r
# This script analyzes the discrepancy between the number of variables defined in the
# crosswalk file versus what's actually being processed in the pipeline.

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

verify_variables <- function() {
  cat("Verifying variables in the SDOH pipeline...\n")
  
  # Step 1: Check all crosswalk files and identify the definitive one
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
    return(NULL)
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
  
  # Use the most recently modified crosswalk file by default
  file_mtimes <- sapply(found_files, function(f) file.info(f)$mtime)
  latest_file_idx <- which.max(file_mtimes)
  crosswalk_file <- found_files[latest_file_idx]
  
  cat("\nUsing the most recent crosswalk file:", crosswalk_file, "\n")
  
  # Step 2: Read the crosswalk file
  cat("Reading crosswalk file:", crosswalk_file, "...\n")
  crosswalk <- read_csv(crosswalk_file, show_col_types = FALSE)
  
  # Count the number of variables in the crosswalk
  var_count <- nrow(crosswalk)
  cat("Found", var_count, "variables defined in the crosswalk.\n")
  
  # Categorize variables by domain and years available if the category column exists
  if ("category" %in% names(crosswalk)) {
    domain_counts <- crosswalk %>%
      group_by(category) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count))
    
    cat("\nVariables by category:\n")
    print(domain_counts)
  } else {
    cat("\nNo category column found in the crosswalk\n")
  }
  
  # Analyze year availability if present in the crosswalk
  if ("years_available" %in% names(crosswalk)) {
    cat("\nAnalyzing year availability in the crosswalk...\n")
    
    # Extract year ranges from years_available column
    year_data <- crosswalk %>%
      filter(!is.na(years_available)) %>%
      mutate(
        start_year = as.numeric(str_extract(years_available, "^\\d{4}")),
        end_year = as.numeric(str_extract(years_available, "\\d{4}$")),
        span = end_year - start_year + 1
      )
    
    # Summarize year availability
    if (nrow(year_data) > 0) {
      cat("Year availability summary:\n")
      cat("- Earliest year in any variable:", min(year_data$start_year, na.rm = TRUE), "\n")
      cat("- Latest year in any variable:", max(year_data$end_year, na.rm = TRUE), "\n")
      cat("- Average year span per variable:", round(mean(year_data$span, na.rm = TRUE), 1), "years\n")
      
      # Count variables by decade
      cat("\nVariables available by decade:\n")
      
      # Create availability_decade column based on the first year in years_available
      decades <- c(seq(1970, 2020, by=10))
      decade_counts <- sapply(decades, function(decade) {
        sum(year_data$start_year <= decade & year_data$end_year >= decade, na.rm = TRUE)
      })
      
      decade_data <- data.frame(
        decade = paste0(decades, "s"),
        var_count = decade_counts
      )
      
      print(decade_data)
    }
  }
  
  # Step 3: Check which fetcher modules are available
  cat("\nChecking fetcher modules...\n")
  
  fetcher_modules <- c(
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
  
  # Check which fetcher modules are available
  module_status <- data.frame(
    module = fetcher_modules,
    exists = sapply(fetcher_modules, file.exists)
  )
  
  cat("Module availability:\n")
  print(module_status)
  
  missing_modules <- module_status$module[!module_status$exists]
  if (length(missing_modules) > 0) {
    cat("\nWARNING: The following modules are missing, which may cause variables to be missing:\n")
    cat(paste("- ", missing_modules, collapse = "\n"), "\n")
  }
  
  # Step 4: Load config from unified pipeline
  source_file <- "unified_sdoh_pipeline.r"
  if (!file.exists(source_file)) {
    source_file <- "R/unified_sdoh_pipeline.r"
    if (!file.exists(source_file)) {
      cat("ERROR: Could not find unified_sdoh_pipeline.r\n")
      return(NULL)
    }
  }
  
  # Read the file content to check for allow_simulation parameter
  source_content <- readLines(source_file)
  
  # Check if simulation is disabled
  simulation_settings <- grep("allow_simulation|simulate", source_content, value = TRUE)
  if (length(simulation_settings) > 0) {
    cat("\nSimulation settings in the pipeline:\n")
    cat(paste(simulation_settings, collapse = "\n"), "\n")
    
    if (any(grepl("allow_simulation\\s*=\\s*TRUE", simulation_settings))) {
      cat("WARNING: allow_simulation may be set to TRUE, which would allow simulated data\n")
    } else {
      cat("Simulation appears to be properly disabled\n")
    }
  }
  
  # Step 5: Read the README to find the expected variable count
  readme_file <- "README.md"
  if (!file.exists(readme_file)) {
    readme_file <- "R/README.md"
    if (!file.exists(readme_file)) {
      cat("WARNING: Could not find README.md for verification.\n")
      readme_content <- NULL
    } else {
      readme_content <- readLines(readme_file)
    }
  } else {
    readme_content <- readLines(readme_file)
  }
  
  # Look for variable count in README
  if (!is.null(readme_content)) {
    total_lines <- grep("\\*\\*Total\\*\\*", readme_content)
    if (length(total_lines) > 0) {
      total_line <- readme_content[total_lines[1]]
      expected_count <- as.numeric(str_extract(total_line, "\\d+"))
      cat("\nREADME mentions a total of", expected_count, "variables.\n")
      
      if (expected_count != var_count) {
        cat("DISCREPANCY: README mentions", expected_count, "variables but crosswalk contains", var_count, "variables.\n")
      }
    }
  }
  
  # Step 6: Check for extended data dictionary
  dict_file <- "output/extended_data_dictionary.csv"
  if (file.exists(dict_file)) {
    cat("\nReading extended data dictionary...\n")
    extended_dict <- read_csv(dict_file, show_col_types = FALSE)
    extended_count <- nrow(extended_dict)
    cat("Extended data dictionary contains", extended_count, "variables.\n")
    
    # Compare with crosswalk
    if (extended_count != var_count) {
      cat("DISCREPANCY: Extended dictionary has", extended_count, "variables but crosswalk contains", var_count, "variables.\n")
      
      # Find the extra variables
      dict_vars <- extended_dict$variable_name
      crosswalk_vars <- crosswalk$variable_name
      
      extra_in_dict <- setdiff(dict_vars, crosswalk_vars)
      if (length(extra_in_dict) > 0) {
        cat("Variables in dictionary but not in crosswalk:", length(extra_in_dict), "\n")
        if (length(extra_in_dict) < 20) {
          cat(paste("- ", extra_in_dict, collapse = "\n"), "\n")
        }
      }
      
      extra_in_crosswalk <- setdiff(crosswalk_vars, dict_vars)
      if (length(extra_in_crosswalk) > 0) {
        cat("Variables in crosswalk but not in dictionary:", length(extra_in_crosswalk), "\n")
        if (length(extra_in_crosswalk) < 20) {
          cat(paste("- ", extra_in_crosswalk, collapse = "\n"), "\n")
        }
      }
    }
  }
  
  # Step 7: Check database output if it exists
  db_file <- "us_county_sdoh_data.duckdb"
  if (file.exists(db_file)) {
    cat("\nAnalyzing database output...\n")
    
    # Try to connect to the database
    tryCatch({
      library(DBI)
      library(duckdb)
      
      con <- dbConnect(duckdb(), db_file)
      
      # Get the list of tables
      tables <- dbListTables(con)
      cat("Found", length(tables), "tables in the database.\n")
      
      # Check for county coverage by year
      if ("county_sdoh_data" %in% tables) {
        county_counts <- dbGetQuery(con, "
          SELECT year, COUNT(DISTINCT GEOID) as county_count
          FROM county_sdoh_data
          GROUP BY year
          ORDER BY year
        ")
        
        cat("\nCounty coverage by year in database:\n")
        print(county_counts)
        
        # Check if county counts vary by year
        if (length(unique(county_counts$county_count)) > 1) {
          cat("NOTE: County counts vary by year, which is expected due to data availability.\n")
          cat("Minimum counties:", min(county_counts$county_count), 
              "in year", county_counts$year[which.min(county_counts$county_count)], "\n")
          cat("Maximum counties:", max(county_counts$county_count), 
              "in year", county_counts$year[which.max(county_counts$county_count)], "\n")
        } else {
          cat("WARNING: County counts are identical across all years, which is unexpected.\n")
          cat("This might indicate that data was overly interpolated or simulated.\n")
        }
        
        # Get column names
        cols <- dbListFields(con, "county_sdoh_data")
        
        # Remove metadata and flag columns
        data_cols <- cols[!grepl("_interpolated$|_extended$|GEOID|NAME|year|source|data_|interpolation_|extension_", cols)]
        
        cat("\nFound", length(data_cols), "data variables in the database output.\n")
        
        # Compare with crosswalk
        crosswalk_vars <- crosswalk$variable_name
        missing_vars <- setdiff(crosswalk_vars, data_cols)
        
        if (length(missing_vars) > 0) {
          cat("Variables missing from database output:", length(missing_vars), "\n")
          
          # Group missing variables by category
          missing_by_category <- crosswalk %>%
            filter(variable_name %in% missing_vars) %>%
            group_by(category) %>%
            summarise(count = n(), .groups = "drop") %>%
            arrange(desc(count))
          
          cat("Missing variables by category:\n")
          print(missing_by_category)
          
          # Print the specific missing variables
          missing_vars_df <- crosswalk %>%
            filter(variable_name %in% missing_vars) %>%
            select(variable_name, category, description) %>%
            arrange(category, variable_name)
          
          cat("\nDetails of missing variables:\n")
          print(missing_vars_df)
        } else {
          cat("All variables from the crosswalk are present in the database output!\n")
        }
        
        # Check for variable availability by year to ensure there's no simulation
        cat("\nChecking for variable data completeness by year...\n")
        
        # Sample a few key variables to check data presence by year
        sample_vars <- c("total_population", "median_household_income", "poverty_rate", 
                         "obesity_pct", "unemployment_rate")
        sample_vars <- intersect(sample_vars, data_cols)
        
        if (length(sample_vars) > 0) {
          var_coverage <- lapply(sample_vars, function(var) {
            query <- paste0("
              SELECT year, COUNT(*) as total_counties, 
                     SUM(CASE WHEN ", var, " IS NOT NULL THEN 1 ELSE 0 END) as counties_with_data,
                     SUM(CASE WHEN ", var, "_interpolated = TRUE THEN 1 ELSE 0 END) as interpolated_counties
              FROM county_sdoh_data
              GROUP BY year
              ORDER BY year
            ")
            
            coverage_data <- dbGetQuery(con, query)
            coverage_data$variable <- var
            return(coverage_data)
          })
          
          var_coverage_df <- do.call(rbind, var_coverage)
          
          # Check for any years with 100% or near 100% interpolation
          suspicious_years <- var_coverage_df %>%
            filter(counties_with_data > 0) %>%
            mutate(interp_pct = interpolated_counties / counties_with_data * 100) %>%
            filter(interp_pct > 95)
          
          if (nrow(suspicious_years) > 0) {
            cat("\nWARNING: Found years with >95% interpolated data, which might indicate simulation:\n")
            print(suspicious_years %>% select(variable, year, counties_with_data, interpolated_counties, interp_pct))
          } else {
            cat("No evidence of excessive interpolation found in the sample variables.\n")
          }
        }
      }
      
      # Close the connection
      dbDisconnect(con)
    }, error = function(e) {
      cat("Error accessing database:", conditionMessage(e), "\n")
    })
  } else {
    cat("\nDatabase file not found. Run the pipeline first to generate output.\n")
  }
  
  # Step 8: Check all crosswalk files to suggest consolidation
  if (length(found_files) > 1) {
    cat("\n=================================================\n")
    cat("MULTIPLE CROSSWALK FILES DETECTED\n")
    cat("=================================================\n")
    cat("Found", length(found_files), "different crosswalk files:\n")
    
    # Compare the files
    crosswalk_contents <- lapply(found_files, function(f) {
      tryCatch({
        read_csv(f, show_col_types = FALSE)
      }, error = function(e) {
        cat("Error reading", f, ":", conditionMessage(e), "\n")
        return(NULL)
      })
    })
    
    # Remove any NULL entries from failed reads
    valid_idx <- which(!sapply(crosswalk_contents, is.null))
    crosswalk_contents <- crosswalk_contents[valid_idx]
    valid_files <- found_files[valid_idx]
    
    if (length(crosswalk_contents) > 1) {
      # Compare the row counts
      row_counts <- sapply(crosswalk_contents, nrow)
      
      # Create a comparison table
      comparison <- data.frame(
        file = valid_files,
        variables = row_counts,
        last_modified = sapply(valid_files, function(f) format(file.info(f)$mtime))
      )
      
      cat("\nCrosswalk file comparison:\n")
      print(comparison)
      
      # Suggest consolidation
      cat("\nRECOMMENDATION: Consolidate the crosswalk files to avoid confusion.\n")
      cat("The file with the most variables appears to be:", valid_files[which.max(row_counts)], "\n")
      cat("Run the consolidate_crosswalks.r script to merge all crosswalks into a single definitive file.\n")
    }
  }
  
  # Step 9: Generate report with recommendations
  cat("\n=================================================\n")
  cat("VARIABLE VERIFICATION REPORT\n")
  cat("=================================================\n\n")
  
  cat("1. Crosswalk contains", var_count, "variables\n")
  if (exists("expected_count") && !is.null(expected_count)) {
    cat("2. README mentions", expected_count, "variables\n")
  }
  if (exists("extended_count") && !is.null(extended_count)) {
    cat("3. Extended dictionary contains", extended_count, "variables\n")
  }
  if (exists("data_cols") && !is.null(data_cols)) {
    cat("4. Database output contains", length(data_cols), "variables\n")
  }
  
  cat("\nRECOMMENDATIONS:\n")
  
  if (length(missing_modules) > 0) {
    cat("- Fix or implement the missing modules:", paste(missing_modules, collapse=", "), "\n")
  }
  
  if (exists("missing_vars") && length(missing_vars) > 0) {
    cat("- ", length(missing_vars), "variables are missing from the output. Check the fetcher modules for these categories:\n")
    print(missing_by_category)
  }
  
  if (length(found_files) > 1) {
    cat("- Consolidate the multiple crosswalk files into a single source of truth\n")
  }
  
  if (any(grepl("allow_simulation\\s*=\\s*TRUE", simulation_settings))) {
    cat("- Disable simulation by setting allow_simulation=FALSE in the pipeline\n")
  }
  
  cat("\nTo fix the variable count discrepancy:\n")
  cat("1. Ensure all fetcher modules are properly implemented and loaded\n")
  cat("2. Check for API key and credential issues that might prevent data fetching\n")
  cat("3. Verify that process_extended_data.r correctly handles all variables\n")
  cat("4. Update the README to accurately reflect the current number of variables\n")
  cat("5. Ensure the variable count reflects actual available data, not theoretical variables\n")
  
  # Return a summary of the findings
  return(list(
    crosswalk_file = crosswalk_file,
    crosswalk_count = var_count,
    readme_count = if(exists("expected_count")) expected_count else NULL,
    dictionary_count = if(exists("extended_count")) extended_count else NULL,
    database_count = if(exists("data_cols")) length(data_cols) else NULL,
    missing_modules = missing_modules,
    missing_variables = if(exists("missing_vars")) missing_vars else NULL,
    multiple_crosswalks = if(length(found_files) > 1) found_files else NULL
  ))
}

# Execute the function if run directly
if (!is_sourced()) {
  verify_variables()
}