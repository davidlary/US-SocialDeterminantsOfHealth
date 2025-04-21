library(dplyr)
library(tidyr)
library(duckdb)
library(zoo)      # For time series interpolation
library(lubridate)
library(stringr)

#' Process and combine data from multiple sources into a harmonized dataset
#'
#' This function takes data from Census, IPUMS NHGIS, and CDC PLACES and combines
#' them into a coherent dataset with consistent variable names, standardized
#' geographic identifiers, and data quality flags.
#'
#' @param data_list List containing data frames from each source
#' @param crosswalk Extended variable crosswalk
#' @param interpolate Logical; whether to interpolate missing values
#' @param extend_health_data Logical; whether to extend health data backward
#' @param include_life_expectancy Logical; whether to include life expectancy data
#' @return None; data is saved to disk
process_extended_data <- function(data_list, crosswalk, 
                                 interpolate = TRUE, 
                                 extend_health_data = TRUE,
                                 include_life_expectancy = TRUE,
                                 check_simulated = TRUE) {
  
  # First validate that data_list is a proper list
  if (!is.list(data_list)) {
    cat("ERROR: data_list is not a list. Creating empty placeholder structure.\n")
    # Create a properly structured placeholder
    data_list <- list(
      census = list(),
      places = tibble(),
      nhgis = tibble(),
      life_expectancy = tibble()
    )
  }
  
  # Check data quality and mark missing data flags
  # This replaces the previous simulation check with a more general data quality assessment
  if (check_simulated) {
    # Look for data quality flags in each dataset
    data_quality_check <- list()
    
    # Safely iterate through data_list components
    for (src in names(data_list)) {
      tryCatch({
        if (is.data.frame(data_list[[src]])) {
          # Check for various data quality indicators
          quality_info <- list(
            # Check for "missing data" flags
            missing_data = "missing_data_flag" %in% names(data_list[[src]]) && 
                            any(data_list[[src]]$missing_data_flag, na.rm = TRUE),
            
            # Check data_quality field for values
            data_quality = if ("data_quality" %in% names(data_list[[src]])) {
              table(data_list[[src]]$data_quality, useNA = "ifany")
            } else {
              NULL
            },
            
            # Count of NA values in the dataset
            na_count = sum(sapply(data_list[[src]], function(x) sum(is.na(x)))),
            
            # Row count
            row_count = nrow(data_list[[src]])
          )
          
          data_quality_check[[src]] <- quality_info
          
          # Log data quality information
          if (quality_info$missing_data) {
            message(paste0(
              "NOTE: Missing data flags detected in ", src, " dataset.\n",
              "These rows will have NA values that can be interpolated if bracketed by real data."
            ))
          }
          
          # Log a data quality summary
          if (!is.null(quality_info$data_quality)) {
            message(paste0("Data quality summary for ", src, " dataset:"))
            print(quality_info$data_quality)
          }
          
          if (quality_info$na_count > 0) {
            na_pct <- round(100 * quality_info$na_count / (quality_info$row_count * ncol(data_list[[src]])), 1)
            message(paste0("Dataset has ", quality_info$na_count, " NA values (", na_pct, "% of all data points)"))
          }
        } else if (!is.null(data_list[[src]])) {
          # Data component exists but is not a data frame
          cat(paste0("WARNING: data_list$", src, " is not a data frame. Type: ", typeof(data_list[[src]]), "\n"))
        }
      }, error = function(e) {
        # Handle errors in quality checking
        cat(paste0("Error in quality check for ", src, ": ", conditionMessage(e), "\n"))
      })
    }
  }
  # Print the structure of the data list
  message("Processing data from multiple sources...")
  message("Structure of data_list:")
  # Use tryCatch to safely check the structure
  tryCatch({
    # Use capture.output to control how str() outputs
    structure_output <- capture.output(str(data_list, max.level = 1))
    cat(paste(structure_output, collapse = "\n"), "\n")
  }, error = function(e) {
    cat("Error checking data_list structure:", conditionMessage(e), "\n")
  })
  
  # Extract individual datasets - with robust error checking
  # Helper function to safely extract and validate dataset components
  safe_extract <- function(component_name, fallback = NULL, required = FALSE) {
    result <- tryCatch({
      if (is.list(data_list) && component_name %in% names(data_list)) {
        # Component exists, now validate its type
        component <- data_list[[component_name]]
        
        if (is.data.frame(component)) {
          # Valid dataframe
          return(component)
        } else if (is.list(component)) {
          # Is a list but not a dataframe - might be a valid nested structure
          # Return as is, will be handled by specific processing code
          return(component)
        } else {
          # Component exists but has invalid type
          cat(paste0("WARNING: data_list$", component_name, " has invalid type: ", 
                    typeof(component), ". Expected data frame or list.\n"))
          if (required) {
            cat(paste0("Creating empty placeholder for required component: ", component_name, "\n"))
            if (is.null(fallback)) {
              # Default fallback for different components
              if (component_name == "census") {
                return(list())  # Census data is often a nested list
              } else {
                return(tibble())  # Other components are usually data frames
              }
            } else {
              return(fallback)
            }
          } else {
            return(fallback)  # Use provided fallback
          }
        }
      } else {
        # Component doesn't exist
        if (required) {
          cat(paste0("WARNING: Required component '", component_name, 
                    "' not found in data_list. Creating empty placeholder.\n"))
          if (is.null(fallback)) {
            # Default fallback for different components
            if (component_name == "census") {
              return(list())  # Census data is often a nested list
            } else {
              return(tibble())  # Other components are usually data frames
            }
          } else {
            return(fallback)
          }
        } else {
          cat(paste0("NOTE: '", component_name, 
                    "' not found in data_list.", 
                    if(component_name == "historical") " This is normal if not using historical data." else "",
                    "\n"))
          return(fallback)
        }
      }
    }, error = function(e) {
      # Handle unexpected errors
      cat(paste0("ERROR extracting '", component_name, "' from data_list: ", conditionMessage(e), "\n"))
      if (required) {
        cat(paste0("Creating empty placeholder for required component: ", component_name, "\n"))
        if (is.null(fallback)) {
          # Default fallback for different components
          if (component_name == "census") {
            return(list())  # Census data is often a nested list
          } else {
            return(tibble())  # Other components are usually data frames
          }
        } else {
          return(fallback)
        }
      } else {
        return(fallback)
      }
    })
    
    return(result)
  }
  
  # Extract required components
  census_data <- safe_extract("census", list(), required = TRUE)
  places_data <- safe_extract("places", tibble())
  nhgis_data <- safe_extract("nhgis", tibble())
  life_expectancy_data <- safe_extract("life_expectancy", tibble())
  historical_data <- safe_extract("historical", tibble())
  
  # Process census data
  cat("Processing Census Bureau data...\n")
  
  # Verify the census_data structure before combining
  census_combined <- tryCatch({
    census_data_frames <- list()
    
    # Safely check and add each census component
    if (is.list(census_data)) {
      # Enhanced safety checks for each component
      # Helper function to safely extract census component
      extract_census_component <- function(component_name) {
        tryCatch({
          component <- census_data[[component_name]]
          if (!is.null(component) && is.data.frame(component) && nrow(component) > 0) {
            cat(paste0("Found valid census ", component_name, " data with ", nrow(component), " rows.\n"))
            return(component)
          } else if (!is.null(component) && !is.data.frame(component)) {
            cat(paste0("WARNING: census$", component_name, " exists but is not a data frame. Type: ", 
                      typeof(component), "\n"))
            return(NULL)
          } else {
            return(NULL)
          }
        }, error = function(e) {
          cat(paste0("ERROR extracting census ", component_name, " data: ", conditionMessage(e), "\n"))
          return(NULL)
        })
      }
      
      # Extract each component
      decennial_data <- extract_census_component("decennial")
      if (!is.null(decennial_data)) census_data_frames$decennial <- decennial_data
      
      pep_data <- extract_census_component("pep")
      if (!is.null(pep_data)) census_data_frames$pep <- pep_data
      
      acs_data <- extract_census_component("acs")
      if (!is.null(acs_data)) census_data_frames$acs <- acs_data
    } else {
      cat("WARNING: census_data is not a list. Cannot extract components.\n")
    }
    
    # Combine all available data frames
    if (length(census_data_frames) > 0) {
      # Additional safety check before binding
      valid_frames <- Filter(function(df) is.data.frame(df) && nrow(df) > 0, census_data_frames)
      
      if (length(valid_frames) > 0) {
        # Check if all data frames have compatible columns for binding
        col_check <- lapply(valid_frames, names)
        common_cols <- Reduce(intersect, col_check)
        
        if (length(common_cols) >= 4) {
          # We have enough common columns to safely bind
          cat(paste0("Binding ", length(valid_frames), " census data frames with ", 
                    length(common_cols), " common columns.\n"))
          bind_rows(valid_frames)
        } else {
          # Not enough common columns, use an alternative approach
          cat("WARNING: Census data frames have too few common columns for direct binding.\n")
          cat("Creating minimal compatible structure for binding.\n")
          
          # Create minimal compatible versions of each data frame
          minimal_frames <- lapply(names(valid_frames), function(source) {
            df <- valid_frames[[source]]
            # Extract minimal required columns and add placeholders if missing
            min_df <- tibble(
              GEOID = if ("GEOID" %in% names(df)) df$GEOID else NA_character_,
              NAME = if ("NAME" %in% names(df)) df$NAME else NA_character_,
              year = if ("year" %in% names(df)) df$year else NA_integer_,
              source = if ("source" %in% names(df)) df$source else source
            )
            # Add any available data columns
            for (col in setdiff(names(df), c("GEOID", "NAME", "year", "source"))) {
              min_df[[col]] <- df[[col]]
            }
            return(min_df)
          })
          
          # Bind the minimal compatible data frames
          bind_rows(minimal_frames)
        }
      } else {
        cat("WARNING: No valid census data frames found to combine.\n")
        tibble() # Return empty tibble if no data
      }
    } else {
      cat("WARNING: No valid census data frames found to combine.\n")
      tibble() # Return empty tibble if no data
    }
  }, error = function(e) {
    cat("ERROR binding census data frames:", conditionMessage(e), "\n")
    cat("Returning empty data frame instead.\n")
    tibble() # Return empty tibble in case of error
  })
  
  # Add data quality flags
  census_combined <- census_combined %>%
    mutate(
      data_quality = case_when(
        str_detect(source, "Decennial") ~ "direct",
        str_detect(source, "ACS") ~ "estimate",
        str_detect(source, "PEP") ~ "estimate",
        TRUE ~ "unknown"
      ),
      data_source = source,
      data_vintage = paste0(source, " ", year)
    )
  
  # Check if we have historical data
  have_historical_data <- !is.null(historical_data) && nrow(historical_data) > 0
  
  # Print historical data information if available
  if (have_historical_data) {
    cat("\nIncluded historical data:", nrow(historical_data), "records from years", 
        min(historical_data$year, na.rm = TRUE), "to", max(historical_data$year, na.rm = TRUE), "\n")
    
    # Verify SDOH parameter coverage for historical data
    if (!is.null(crosswalk) && nrow(crosswalk) > 0) {
      # Get all 37 SDOH parameters from crosswalk
      sdoh_params <- crosswalk %>%
        filter(category %in% c("Demographics", "Socioeconomic", "Education", 
                               "Housing", "Health Status", "Health Access", "Transportation")) %>%
        pull(std_name) %>%
        unique()
      
      cat("\nChecking coverage of", length(sdoh_params), "SDOH parameters in historical data...\n")
      
      # Check which SDOH parameters are in the historical data
      sdoh_coverage <- sapply(sdoh_params, function(param) {
        param %in% names(historical_data)
      })
      
      # Print coverage summary
      cat("SDOH parameter coverage in historical data:", 
          sum(sdoh_coverage), "of", length(sdoh_params), "parameters (", 
          round(sum(sdoh_coverage) / length(sdoh_params) * 100, 1), "%)\n")
      
      # List any missing parameters
      missing_params <- sdoh_params[!sdoh_coverage]
      if (length(missing_params) > 0) {
        cat("Warning: Missing SDOH parameters in historical data:", 
            paste(missing_params, collapse=", "), "\n")
        cat("These will be imputed during data processing.\n")
      }
    }
    
    # Ensure historical data has consistent column structure
    if (!"data_quality" %in% names(historical_data)) {
      historical_data$data_quality <- "historical"
    }
    if (!"data_source" %in% names(historical_data)) {
      historical_data$data_source <- "Historical"
    }
    if (!"data_vintage" %in% names(historical_data)) {
      historical_data$data_vintage <- paste0("Historical ", historical_data$year)
    }
    if (!"source" %in% names(historical_data)) {
      historical_data$source <- "Historical"
    }
  }
  
  # Combine all available data sources
  all_data_combined <- bind_rows(
    census_combined,
    if (!is.null(places_data) && nrow(places_data) > 0) places_data else NULL,
    if (!is.null(nhgis_data) && nrow(nhgis_data) > 0) nhgis_data else NULL,
    if (include_life_expectancy && !is.null(life_expectancy_data) && nrow(life_expectancy_data) > 0) life_expectancy_data else NULL,
    if (have_historical_data) historical_data else NULL
  )
  
  # Log information about the life expectancy data
  if (include_life_expectancy && !is.null(life_expectancy_data) && nrow(life_expectancy_data) > 0) {
    cat("\nIncluded", nrow(life_expectancy_data), "records of life expectancy data.\n")
    cat("Life expectancy data covers years:", paste(sort(unique(life_expectancy_data$year)), collapse=", "), "\n")
  } else if (include_life_expectancy) {
    cat("\nLife expectancy data was requested but no data was found or loaded.\n")
    cat("Please download IHME data manually as described in the documentation.\n")
  }
  
  # Get list of all standard variables in the crosswalk
  all_std_vars <- crosswalk$std_name
  
  # Check which variables are available
  available_vars <- names(all_data_combined)[names(all_data_combined) %in% all_std_vars]
  cat("\nFound", length(available_vars), "standardized variables in the combined dataset.\n")
  
  # Define ID columns and metadata columns
  id_cols <- c("GEOID", "NAME", "year", "source")
  meta_cols <- c("data_quality", "data_source", "data_vintage")
  
  # Check for missing years or counties
  year_county_coverage <- all_data_combined %>%
    group_by(year) %>%
    summarise(
      county_count = n_distinct(GEOID),
      .groups = "drop"
    )
  
  cat("\nYear coverage summary:\n")
  print(year_county_coverage)
  
  # Identify all unique counties
  all_counties <- all_data_combined %>%
    distinct(GEOID, NAME) %>%
    arrange(GEOID)
  
  cat("\nTotal unique counties:", nrow(all_counties), "\n")
  
  # Enhanced interpolation function with better handling of missing data and metadata
  interpolate_missing_years <- function(data, geoid_var, year_var, value_var, interpolate = TRUE) {
    if (!interpolate) return(data)
    
    # Create a complete year sequence for each GEOID, but only for years within the data range
    data_complete <- data %>%
      group_by(!!sym(geoid_var)) %>%
      complete(!!sym(year_var) := min(data[[year_var]], na.rm = TRUE):max(data[[year_var]], na.rm = TRUE)) %>%
      arrange(!!sym(geoid_var), !!sym(year_var))
    
    # Create the flag column names
    flag_colname <- paste0(value_var, "_interpolated")
    missing_data_flag_colname <- paste0(value_var, "_missing_data_interpolated")
    
    # Identify counties with missing data vs. sparse data points
    # Missing data: When a value is NA because no data was found at all (missing_data_flag is TRUE)
    # Sparse data: When a value is NA because it's between years (e.g., between census years)
    data_has_missing_data_flag <- "missing_data_flag" %in% names(data)
    
    # Always create a missing_data_flag column if it doesn't exist
    if (!data_has_missing_data_flag) {
      # Add the flag column to the complete data - assume FALSE for all rows
      data_complete$missing_data_flag <- FALSE
      data_has_missing_data_flag <- TRUE
      
      # If we're interpolating a county with no data for all years, consider it missing data
      no_data_counties <- data_complete %>%
        group_by(!!sym(geoid_var)) %>%
        summarise(all_missing = all(is.na(!!sym(value_var)))) %>%
        filter(all_missing) %>%
        pull(!!sym(geoid_var))
      
      if (length(no_data_counties) > 0) {
        # Mark these counties as having missing data
        data_complete <- data_complete %>%
          mutate(missing_data_flag = missing_data_flag | (!!sym(geoid_var) %in% no_data_counties))
      }
    }
    
    # Perform interpolation with detailed tracking of data quality
    data_interpolated <- data_complete %>%
      group_by(!!sym(geoid_var)) %>%
      mutate(
        # Record original state before interpolation for quality tracking
        original_value_was_na = is.na(!!sym(value_var)),
        original_source_was_na = is.na(source),
        
        # Copy or create the missing_data_flag
        missing_data_flag = if_else(is.na(missing_data_flag), FALSE, missing_data_flag),
        
        # Linearly interpolate missing values, using rule=1 to avoid extrapolation beyond data range
        !!value_var := na.approx(!!sym(value_var), na.rm = FALSE, rule = 1),
        
        # Flag interpolated values with detailed reasons
        !!flag_colname := original_value_was_na & !is.na(!!sym(value_var)),
        
        # Special flag for values interpolated from missing data points vs. just sparse data
        !!missing_data_flag_colname := !!sym(flag_colname) & missing_data_flag,
        
        # Set source and metadata for interpolated values
        source = if_else(original_source_was_na & !is.na(!!sym(value_var)), "Interpolated", source),
        
        # More descriptive data quality field that indicates if this was interpolated from missing or sparse data
        data_quality = case_when(
          # If it wasn't interpolated, keep original quality
          !original_value_was_na | is.na(!!sym(value_var)) ~ data_quality,
          # If interpolated from missing data vs sparse data
          !!sym(missing_data_flag_colname) ~ "interpolated_from_missing",
          !!sym(flag_colname) ~ "interpolated",
          TRUE ~ data_quality
        ),
        
        # Record the gap size for interpolated values (how many years between real data points)
        # This helps identify interpolations over very long periods
        interpolation_gap = if_else(
          !!sym(flag_colname),
          # Find distance to nearest non-NA values (before and after)
          {
            # Vector of all years with non-NA values for this county
            real_data_years <- !!sym(year_var)[!is.na(!!sym(value_var)) & !original_value_was_na]
            if(length(real_data_years) >= 2) {
              # For each interpolated point, find nearest year with real data before and after
              # Only do this calculation for interpolated points
              sapply(!!sym(year_var)[!!sym(flag_colname)], function(yr) {
                # Find nearest year before and after with real data
                before <- max(real_data_years[real_data_years < yr], na.rm = TRUE)
                after <- min(real_data_years[real_data_years > yr], na.rm = TRUE)
                # Calculate gap
                gap <- after - before
                # Return the gap
                return(gap)
              })
            } else {
              # Default to NA if we don't have enough real data points to calculate gap
              NA_integer_
            }
          },
          NA_integer_
        ),
        
        data_source = if_else(original_source_was_na & !is.na(!!sym(value_var)), 
                             if_else(!!sym(missing_data_flag_colname), 
                                    "Interpolation from Missing Data", 
                                    "Interpolation"), 
                             data_source),
                             
        data_vintage = if_else(original_source_was_na & !is.na(!!sym(value_var)), 
                              paste0("Interpolated ", !!sym(year_var), 
                                    if_else(!!sym(missing_data_flag_colname), 
                                            " (from missing data)", 
                                            "")), 
                              data_vintage),
        
        # Ensure NAME is consistent - use the first non-NA NAME
        NAME = first(NAME[!is.na(NAME)])
      ) %>%
      # Clean up temporary variables used for calculations
      select(-original_value_was_na, -original_source_was_na) %>%
      ungroup()
    
    return(data_interpolated)
  }
  
  # Improved function to extend health data backward in time
  extend_health_data_backward <- function(data, vars_to_extend, min_year = 2000) {
    # Filter to keep only health variables present in the data
    extend_vars <- intersect(vars_to_extend, names(data))
    
    if (length(extend_vars) == 0) {
      cat("No health variables found for extension.\n")
      return(data)
    }
    
    # Find earliest year with health data - for each variable separately
    earliest_years <- sapply(extend_vars, function(var) {
      data %>%
        filter(!is.na(!!sym(var))) %>%
        pull(year) %>%
        min(na.rm = TRUE)
    })
    
    # If all earliest years are already at or before min_year, no extension needed
    if (all(earliest_years <= min_year)) {
      cat("Health data already available from earliest required year - no extension needed.\n")
      return(data)
    }
    
    cat("Extending health variables backward:\n")
    for (var in extend_vars) {
      earliest_year <- earliest_years[var]
      if (earliest_year > min_year) {
        cat("  -", var, "from year", earliest_year, "back to", min_year, "\n")
      }
    }
    
    # Get all unique counties
    counties <- unique(data$GEOID)
    
    # Create a dataframe to hold all extended data
    extended_data <- tibble()
    
    # For each variable and county, extend data backward as needed
    for (var in extend_vars) {
      earliest_year <- earliest_years[var]
      
      # Skip if this variable doesn't need extension
      if (earliest_year <= min_year) next
      
      # Identify the years that need to be added for this variable
      missing_years <- min_year:(earliest_year - 1)
      
      for (county_id in counties) {
        # Get the earliest available data for this county and variable
        county_earliest <- data %>%
          filter(
            GEOID == county_id,
            year == earliest_year,
            !is.na(!!sym(var))
          )
        
        if (nrow(county_earliest) > 0) {
          # Create records for each missing year
          for (yr in missing_years) {
            # Copy the record with updated year and flags
            extended_row <- county_earliest %>%
              mutate(
                year = yr,
                data_quality = "extended",
                data_source = "Extended from CDC PLACES",
                data_vintage = paste0("Extended from ", data_vintage)
              )
            
            # Add flag for this extended variable
            extended_row[[paste0(var, "_extended")]] <- TRUE
            
            # Add to the extended data
            extended_data <- bind_rows(extended_data, extended_row)
          }
        }
      }
    }
    
    # Combine original data with extended data
    if (nrow(extended_data) > 0) {
      # Remove duplicate entries for the same GEOID and year
      extended_data <- extended_data %>%
        group_by(GEOID, year) %>%
        slice(1) %>%
        ungroup()
      
      # Ensure extension flags exist for all variables
      for (var in extend_vars) {
        if (!paste0(var, "_extended") %in% names(extended_data)) {
          extended_data[[paste0(var, "_extended")]] <- TRUE
        }
      }
      
      combined_data <- bind_rows(data, extended_data)
      cat("Added", nrow(extended_data), "extended health data records.\n")
      return(combined_data)
    } else {
      cat("No health data could be extended.\n")
      return(data)
    }
  }
  
  # Create a complete dataset with all counties and years
  if (interpolate) {
    cat("\nInterpolating missing data...\n")
    
    # Get min and max years
    min_year <- min(all_data_combined$year, na.rm = TRUE)
    max_year <- max(all_data_combined$year, na.rm = TRUE)
    
    # Get variables eligible for interpolation
    interpolation_vars <- tryCatch({
      # Try to get recommended variables from crosswalk
      vars <- crosswalk %>%
        filter(interpolate_recommended == TRUE) %>%
        pull(std_name)
      
      # If no recommended variables or empty result, use all numeric variables instead
      if (length(vars) == 0) {
        # Get all numeric columns as a fallback
        numeric_vars <- names(all_data_combined)[sapply(all_data_combined, is.numeric)]
        # Exclude metadata columns
        vars <- setdiff(numeric_vars, c("GEOID", "year", "interpolation_count", "extension_count"))
        cat("No variables with interpolate_recommended=TRUE found in crosswalk. Using all numeric variables instead.\n")
      }
      vars
    }, error = function(e) {
      # If error in crosswalk processing, use all numeric variables
      cat("Error getting interpolation variables from crosswalk:", conditionMessage(e), "\n")
      cat("Using all numeric variables instead.\n")
      numeric_vars <- names(all_data_combined)[sapply(all_data_combined, is.numeric)]
      setdiff(numeric_vars, c("GEOID", "year", "interpolation_count", "extension_count"))
    })
    
    # Ensure we only interpolate variables that exist in the data
    interpolation_vars <- intersect(interpolation_vars, available_vars)
    
    # Force interpolation on at least key variables
    key_vars <- c("total_population", "median_household_income", "poverty_rate", "uninsured_pct", "obesity_pct")
    additional_vars <- intersect(key_vars, available_vars)
    interpolation_vars <- unique(c(interpolation_vars, additional_vars))
    
    # Report interpolation plan
    cat("Interpolating", length(interpolation_vars), "variables across years", min_year, "to", max_year, "\n")
    if (length(interpolation_vars) == 0) {
      cat("WARNING: No variables available for interpolation! This will result in no interpolation being performed.\n")
      # Force at least some basic interpolation
      interpolation_vars <- intersect(c("total_population", "median_household_income"), names(all_data_combined))
      if (length(interpolation_vars) > 0) {
        cat("Forcing interpolation on basic variables:", paste(interpolation_vars, collapse=", "), "\n")
      }
    }
    
    # Initialize interpolated_data with original data
    interpolated_data <- all_data_combined
    
    # Add interpolation flag columns initialized to FALSE
    for (var in interpolation_vars) {
      interpolated_data[[paste0(var, "_interpolated")]] <- FALSE
    }
    
    # Process each variable independently with enhanced error handling
    cat("\nProgress: [")
    total_vars <- length(interpolation_vars)
    
    # Helper function to safely process a single variable
    safe_interpolate_variable <- function(var_name) {
      tryCatch({
        cat(".")  # Print a dot for each variable to show progress
        
        # Safely extract data for this variable
        var_data <- tryCatch({
          df <- all_data_combined %>%
            # Select only required columns to minimize errors
            select(GEOID, NAME, year, source, !!var_name)
          
          # Try to add quality columns if they exist
          if ("data_quality" %in% names(all_data_combined)) {
            df$data_quality <- all_data_combined$data_quality
          }
          if ("data_source" %in% names(all_data_combined)) {
            df$data_source <- all_data_combined$data_source
          }
          if ("data_vintage" %in% names(all_data_combined)) {
            df$data_vintage <- all_data_combined$data_vintage
          }
          
          # Filter to non-NA values
          df <- df %>% filter(!is.na(!!sym(var_name)))
          return(df)
        }, error = function(e) {
          cat("\nError extracting data for", var_name, ":", conditionMessage(e), "\n")
          return(NULL)
        })
        
        # Skip if no data for this variable or extraction failed
        if (is.null(var_data) || nrow(var_data) == 0) {
          cat("\nSkipping", var_name, "- no valid data found\n")
          return(NULL)
        }
        
        # Verify variable exists and has numeric data before interpolation
        if (!var_name %in% names(var_data)) {
          cat("\nSkipping", var_name, "- variable not found in extracted data\n")
          return(NULL)
        }
        
        # Check if data is numeric and can be interpolated
        if (!is.numeric(var_data[[var_name]])) {
          cat("\nSkipping", var_name, "- data is not numeric (type:", typeof(var_data[[var_name]]), ")\n")
          return(NULL)
        }
        
        # Ensure required columns exist before interpolation
        required_cols <- c("GEOID", "year")
        if (!all(required_cols %in% names(var_data))) {
          missing_cols <- setdiff(required_cols, names(var_data))
          cat("\nSkipping", var_name, "- missing required columns:", paste(missing_cols, collapse=", "), "\n")
          return(NULL)
        }
        
        # Interpolate with error handling
        var_interpolated <- tryCatch({
          interpolate_missing_years(var_data, "GEOID", "year", var_name)
        }, error = function(e) {
          cat("\nError interpolating", var_name, ":", conditionMessage(e), "\n")
          return(NULL)
        })
        
        if (is.null(var_interpolated)) {
          cat("\nInterpolation failed for", var_name, "\n")
          return(NULL)
        }
        
        # Check if any interpolation happened
        flag_col <- paste0(var_name, "_interpolated")
        
        # Verify flag column exists
        if (!flag_col %in% names(var_interpolated)) {
          cat("\nWarning: Interpolation flag column not found for", var_name, "\n")
          # Create the flag column if missing
          var_interpolated[[flag_col]] <- FALSE
        }
        
        interp_count <- sum(var_interpolated[[flag_col]], na.rm = TRUE)
        
        # Prepare results for joining - only include necessary columns
        # Start with minimal dataset
        join_data <- tryCatch({
          var_interpolated %>% 
            select(GEOID, year, !!sym(var_name))
        }, error = function(e) {
          cat("\nError preparing join data for", var_name, ":", conditionMessage(e), "\n")
          return(NULL)
        })
        
        if (is.null(join_data)) {
          return(NULL)
        }
        
        # Also include any special flags generated during interpolation
        missing_flag_col <- paste0(var_name, "_missing_data_interpolated")
        
        # Add interpolation flags safely
        if (missing_flag_col %in% names(var_interpolated)) {
          join_data[[missing_flag_col]] <- var_interpolated[[missing_flag_col]]
        }
        
        if (flag_col %in% names(var_interpolated)) {
          join_data[[flag_col]] <- var_interpolated[[flag_col]]
        }
            
        return(join_data)
      }, error = function(e) {
        cat("\nFatal error processing", var_name, ":", conditionMessage(e), "\n")
        return(NULL)
      })
    }
    
    # Process all variables
    for (i in seq_along(interpolation_vars)) {
      var <- interpolation_vars[i]
      if (i %% 10 == 0) cat(sprintf(" %d%%", round(i/total_vars*100)))  # Show percentage every 10 variables
      
      join_data <- safe_interpolate_variable(var)
      
      # Skip if interpolation failed
      if (is.null(join_data)) {
        next
      }
      
      # Update the dataset with interpolated values - with robust error handling
      flag_col <- paste0(var, "_interpolated")
      missing_flag_col <- paste0(var, "_missing_data_interpolated")
      
      tryCatch({
        # Update the dataset - using regular join to avoid relationship issues
        # First rename the original column to avoid conflicts
        if (var %in% names(interpolated_data)) {
          # Create a temporary name for the original column
          orig_col_name <- paste0(var, "_orig")
          
          # Create a copy with renamed column
          interpolated_data_temp <- interpolated_data
          names(interpolated_data_temp)[names(interpolated_data_temp) == var] <- orig_col_name
          
          # Perform the join
          interpolated_data <- left_join(interpolated_data_temp, join_data, by = c("GEOID", "year"))
          
          # Coalesce values - prefer non-NA values
          # This is safer than ifelse() which can cause type errors
          if (var %in% names(interpolated_data) && orig_col_name %in% names(interpolated_data)) {
            # Coalesce the value columns using dplyr::coalesce instead of ifelse
            interpolated_data[[var]] <- dplyr::coalesce(interpolated_data[[var]], interpolated_data[[orig_col_name]])
            
            # Remove the temporary original column
            interpolated_data[[orig_col_name]] <- NULL
          }
          
          # Set interpolation flags
          if (flag_col %in% names(join_data)) {
            if (flag_col %in% names(interpolated_data)) {
              # Update existing flag
              interpolated_data[[flag_col]] <- ifelse(
                is.na(interpolated_data[[flag_col]]),
                FALSE,
                interpolated_data[[flag_col]]
              )
              interpolated_data[[flag_col]] <- ifelse(
                !is.na(join_data[[flag_col]]) & join_data[[flag_col]],
                TRUE,
                interpolated_data[[flag_col]]
              )
            } else {
              # Add flag if it doesn't exist
              interpolated_data[[flag_col]] <- ifelse(
                !is.na(join_data[[flag_col]]) & join_data[[flag_col]],
                TRUE,
                FALSE
              )
            }
          }
          
          # Set missing data flag
          if (missing_flag_col %in% names(join_data)) {
            if (missing_flag_col %in% names(interpolated_data)) {
              # Update existing flag
              interpolated_data[[missing_flag_col]] <- ifelse(
                is.na(interpolated_data[[missing_flag_col]]),
                FALSE,
                interpolated_data[[missing_flag_col]]
              )
              interpolated_data[[missing_flag_col]] <- ifelse(
                !is.na(join_data[[missing_flag_col]]) & join_data[[missing_flag_col]],
                TRUE,
                interpolated_data[[missing_flag_col]]
              )
            } else {
              # Add flag if it doesn't exist
              interpolated_data[[missing_flag_col]] <- ifelse(
                !is.na(join_data[[missing_flag_col]]) & join_data[[missing_flag_col]],
                TRUE,
                FALSE
              )
            }
          }
        } else {
          # If variable doesn't exist in the dataset yet, simply add all columns from join_data
          for (col in names(join_data)) {
            interpolated_data[[col]] <- join_data[[col]]
          }
        }
      }, error = function(e) {
        cat("\nError updating interpolated values for", var, ":", conditionMessage(e), "\n")
        cat("Interpolation for this variable will be skipped.\n")
      })
    }
    cat(" 100%]\n")
    
    # Print enhanced summary after all variables are processed with details on missing data interpolation
    cat("\nInterpolation summary:\n")
    
    # Initialize counters for the summary
    total_interpolated <- 0
    total_missing_data_interpolated <- 0
    var_reports <- list()
    
    # Collect detailed interpolation statistics for each variable
    for (var in interpolation_vars) {
      # Standard interpolation flag
      flag_col <- paste0(var, "_interpolated")
      # Special flag for interpolation from missing data
      missing_flag_col <- paste0(var, "_missing_data_interpolated")
      
      # Count regular interpolations
      interp_count <- if (flag_col %in% names(interpolated_data)) {
        sum(interpolated_data[[flag_col]], na.rm = TRUE)
      } else 0
      
      # Count interpolations specifically from missing data
      missing_interp_count <- if (missing_flag_col %in% names(interpolated_data)) {
        sum(interpolated_data[[missing_flag_col]], na.rm = TRUE)
      } else 0
      
      # Calculate regular interpolations (not from missing data)
      regular_interp_count <- interp_count - missing_interp_count
      
      # Only report variables that had some interpolation
      if (interp_count > 0) {
        # Store report details for later sorting and display
        var_reports[[var]] <- list(
          var = var,
          total_interp = interp_count,
          missing_interp = missing_interp_count,
          regular_interp = regular_interp_count,
          pct_missing = if(interp_count > 0) round(missing_interp_count/interp_count*100, 1) else 0
        )
        
        # Update overall counters
        total_interpolated <- total_interpolated + interp_count
        total_missing_data_interpolated <- total_missing_data_interpolated + missing_interp_count
      }
    }
    
    # Sort variables by most interpolations first and display report
    sorted_vars <- names(var_reports)[order(sapply(var_reports, function(r) r$total_interp), decreasing = TRUE)]
    
    # Print the detailed report in a formatted table
    if (length(sorted_vars) > 0) {
      # Print header
      cat(sprintf("%-30s %10s %10s %10s %8s\n", "Variable", "Total", "Regular", "Missing", "% Missing"))
      cat(sprintf("%-30s %10s %10s %10s %8s\n", "--------", "-----", "-------", "-------", "---------"))
      
      # Print each variable's stats
      for (var in sorted_vars) {
        r <- var_reports[[var]]
        cat(sprintf("%-30s %10d %10d %10d %8.1f%%\n", 
                   r$var, r$total_interp, r$regular_interp, r$missing_interp, r$pct_missing))
      }
      
      # Print summary row
      total_pct_missing <- if(total_interpolated > 0) round(total_missing_data_interpolated/total_interpolated*100, 1) else 0
      cat(sprintf("%-30s %10s %10s %10s %8s\n", "--------", "-----", "-------", "-------", "---------"))
      cat(sprintf("%-30s %10d %10d %10d %8.1f%%\n", 
                 "TOTAL", total_interpolated, 
                 total_interpolated - total_missing_data_interpolated,
                 total_missing_data_interpolated, total_pct_missing))
    } else {
      cat("No interpolation was performed.\n")
    }
    
    all_data_processed <- interpolated_data
    
    # Create a variable indicating interpolation stats for each record
    all_data_processed <- all_data_processed %>%
      mutate(
        # Standard interpolation flags
        interpolation_used = rowSums(across(ends_with("_interpolated"), ~if_else(. == TRUE, 1, 0)), na.rm = TRUE) > 0,
        interpolation_count = rowSums(across(ends_with("_interpolated"), ~if_else(. == TRUE, 1, 0)), na.rm = TRUE),
        
        # Specific flags for missing data interpolation
        missing_data_interpolation_used = rowSums(across(ends_with("_missing_data_interpolated"), ~if_else(. == TRUE, 1, 0)), na.rm = TRUE) > 0,
        missing_data_interpolation_count = rowSums(across(ends_with("_missing_data_interpolated"), ~if_else(. == TRUE, 1, 0)), na.rm = TRUE)
      )
    
    # Final summary statistics
    total_rows <- nrow(all_data_processed)
    rows_with_interpolation <- sum(all_data_processed$interpolation_used, na.rm = TRUE)
    rows_with_missing_data_interpolation <- sum(all_data_processed$missing_data_interpolation_used, na.rm = TRUE)
    
    # If we have gaps calculated, report on them
    if ("interpolation_gap" %in% names(all_data_processed)) {
      max_gap <- max(all_data_processed$interpolation_gap, na.rm = TRUE)
      avg_gap <- mean(all_data_processed$interpolation_gap, na.rm = TRUE)
      median_gap <- median(all_data_processed$interpolation_gap, na.rm = TRUE)
      
      cat("\nInterpolation gap statistics:\n")
      cat("- Maximum gap between real data points:", max_gap, "years\n")
      cat("- Average gap:", round(avg_gap, 1), "years\n")
      cat("- Median gap:", median_gap, "years\n")
    }
    
    cat("\nRecord-level interpolation summary:\n")
    cat("- Total rows:", total_rows, "\n")
    cat("- Rows with any interpolated values:", rows_with_interpolation, 
        sprintf("(%.1f%%)", rows_with_interpolation/total_rows*100), "\n")
    cat("- Rows with interpolated values from missing data:", rows_with_missing_data_interpolation, 
        sprintf("(%.1f%%)", rows_with_missing_data_interpolation/total_rows*100), "\n")
    
    # Quality overview
    cat("\nData quality overview after interpolation:\n")
    data_quality_table <- table(all_data_processed$data_quality, useNA = "ifany")
    data_quality_pcts <- round(100 * data_quality_table / sum(data_quality_table), 1)
    
    for (qual in names(data_quality_table)) {
      cat(sprintf("- %-25s: %8d rows (%.1f%%)\n", 
                 qual, data_quality_table[qual], data_quality_pcts[qual]))
    }
    
  } else {
    all_data_processed <- all_data_combined
    cat("\nInterpolation skipped as requested.\n")
  }
  
  # Extend health data backward
  if (extend_health_data) {
    # Get health variables from the crosswalk that should be extended
    health_vars_to_extend <- crosswalk %>%
      filter(extend_backwards == TRUE & category %in% c("Health Status", "Health Behaviors", "Health Access")) %>%
      pull(std_name)
    
    # Only include variables that are actually present in the data
    health_vars_to_extend <- intersect(health_vars_to_extend, names(all_data_processed))
    
    if (length(health_vars_to_extend) > 0) {
      cat("\nExtending health variables backward in time...\n")
      cat("Processing: [")
      
      # Add a counter to show progress
      extension_counter <- 0
      extension_total <- length(health_vars_to_extend)
      
      # Show progress every 20%
      progress_markers <- round(seq(0, extension_total, length.out = 6)[-1])
      
      # Extend health data backward to the earliest year in the dataset
      min_year <- min(all_data_processed$year, na.rm = TRUE)
      
      # Start with progress indicator
      cat("...")
      all_data_processed <- extend_health_data_backward(all_data_processed, health_vars_to_extend, min_year)
      cat("...] 100% complete\n")
      
      cat("\nAdding extension flags for variables...\n")
      # Add extension flags if they don't already exist
      for (var in health_vars_to_extend) {
        if (!paste0(var, "_extended") %in% names(all_data_processed)) {
          all_data_processed[[paste0(var, "_extended")]] <- FALSE
        }
        # Print a dot for each variable to show progress
        cat(".")
      }
      cat(" Done!\n")
      
      # Update extension counts
      all_data_processed <- all_data_processed %>%
        mutate(
          extension_used = rowSums(across(ends_with("_extended"), ~if_else(. == TRUE, 1, 0)), na.rm = TRUE) > 0,
          extension_count = rowSums(across(ends_with("_extended"), ~if_else(. == TRUE, 1, 0)), na.rm = TRUE)
        )
      
      cat("Rows with extended health data:", sum(all_data_processed$extension_used, na.rm = TRUE), 
          "out of", nrow(all_data_processed), "total rows.\n")
    } else {
      cat("\nNo health variables found for extension.\n")
      # Add extension flags even if no extension was done
      all_data_processed <- all_data_processed %>%
        mutate(
          extension_used = FALSE,
          extension_count = 0
        )
    }
  } else {
    # Add extension flags even if no extension was requested
    all_data_processed <- all_data_processed %>%
      mutate(
        extension_used = FALSE,
        extension_count = 0
      )
    cat("\nHealth data extension skipped as requested.\n")
  }

  # Enhanced data quality tracking with specific handling of missing data flags
  all_data_processed <- all_data_processed %>%
    mutate(
      # Reconcile flags by checking all variables
      # Standard interpolation flag (whether sparse data or missing)
      interpolation_used = rowSums(across(ends_with("_interpolated"), ~if_else(. == TRUE, 1, 0)), na.rm = TRUE) > 0,
      interpolation_count = rowSums(across(ends_with("_interpolated"), ~if_else(. == TRUE, 1, 0)), na.rm = TRUE),
      
      # New flag specifically for interpolation from missing data (important distinction)
      missing_data_interpolation_used = rowSums(across(ends_with("_missing_data_interpolated"), ~if_else(. == TRUE, 1, 0)), na.rm = TRUE) > 0,
      missing_data_interpolation_count = rowSums(across(ends_with("_missing_data_interpolated"), ~if_else(. == TRUE, 1, 0)), na.rm = TRUE),
      
      # Extension flags
      extension_used = rowSums(across(ends_with("_extended"), ~if_else(. == TRUE, 1, 0)), na.rm = TRUE) > 0,
      extension_count = rowSums(across(ends_with("_extended"), ~if_else(. == TRUE, 1, 0)), na.rm = TRUE),
      
      # Calculate max interpolation gap (if present)
      max_interpolation_gap = if ("interpolation_gap" %in% names(.)) {
        max(interpolation_gap, na.rm = TRUE)
      } else {
        NA_integer_
      },
      
      # Improve data quality classification with greater detail
      data_quality = case_when(
        # Prioritize original data source quality indications when present
        !is.na(data_quality) & data_quality %in% c("direct", "estimate", "harmonized") & 
          !extension_used & !interpolation_used ~ data_quality,
          
        # Otherwise, base on interpolation/extension status with more detail
        data_quality == "interpolated_from_missing" ~ "interpolated_from_missing",
        extension_used ~ "extended",
        
        # Distinguish between interpolation sources
        missing_data_interpolation_used ~ "interpolated_from_missing", 
        interpolation_used ~ "interpolated",
        
        TRUE ~ coalesce(data_quality, "unknown")
      ),
      
      # Add data quality score (higher is better) with enhanced scoring for missing data
      data_quality_score = case_when(
        data_quality == "direct" ~ 5,
        data_quality == "estimate" ~ 4,
        data_quality == "harmonized" ~ 3,
        data_quality == "interpolated" ~ 2,  # Regular interpolation between existing data points
        data_quality == "interpolated_from_missing" ~ 1,  # Lower score for interpolation from missing data
        data_quality == "extended" ~ 0,
        TRUE ~ -1
      )
    )

  # Add county metadata
  cat("\nAdding county metadata...\n")
  
  # Get the latest county names and compute derived metrics
  county_metadata <- all_data_processed %>%
    group_by(GEOID) %>%
    # Get the most recent non-interpolated name 
    summarise(
      county_name = last(NAME[source != "Interpolated" & !is.na(NAME)]),
      first_year = min(year),
      last_year = max(year),
      years_available = n_distinct(year),
      percent_interpolated = mean(interpolation_used, na.rm = TRUE) * 100,
      percent_extended = mean(extension_used, na.rm = TRUE) * 100,
      .groups = "drop"
    )
  
  # Split county name into components
  county_metadata <- county_metadata %>%
    mutate(
      state_abbr = str_extract(county_name, ", [A-Z]{2}$") %>% str_replace(", ", ""),
      county_name_only = str_replace(county_name, ", [A-Z]{2}$", "")
    )
  
  # Store in database
  cat("\nStoring data in database...\n")
  tryCatch({
    cat("Connecting to database... ")
    con <- dbConnect(duckdb(), "us_county_sdoh_data.duckdb")
    cat("✓\n")
    
    cat("Sanitizing data for database storage... ")
    # Clean and sanitize data before writing to database
    all_data_processed <- all_data_processed %>%
      # Ensure all character columns are properly encoded
      mutate(across(where(is.character), ~iconv(., "UTF-8", "UTF-8", sub="")))
    
    # Also sanitize county metadata
    county_metadata <- county_metadata %>%
      mutate(across(where(is.character), ~iconv(., "UTF-8", "UTF-8", sub="")))
    cat("✓\n")
    
    # Save the combined dataset - this is the largest operation
    cat("Writing main dataset to database [")
    for (i in 1:10) {
      Sys.sleep(0.1)  # Add a small delay to show progress even if it's fast
      cat(".")
    }
    dbWriteTable(con, "county_sdoh_data", all_data_processed, overwrite = TRUE)
    cat("] ✓\n")
    
    cat("Writing county metadata... ")
    # Save the county metadata
    dbWriteTable(con, "county_metadata", county_metadata, overwrite = TRUE)
    cat("✓\n")
    
    cat("Writing variable crosswalk... ")
    # Save the variable crosswalk for reference
    dbWriteTable(con, "variable_crosswalk", crosswalk, overwrite = TRUE)
    cat("✓\n")
    
    # Create a data quality table with information about each variable and year
    cat("Generating data quality summaries... ")
    data_quality_summary <- all_data_processed %>%
      group_by(year, data_source) %>%
      summarise(
        record_count = n(),
        county_count = n_distinct(GEOID),
        across(all_of(available_vars), ~sum(!is.na(.), na.rm = TRUE), .names = "{.col}_count"),
        across(ends_with("_interpolated"), ~sum(., na.rm = TRUE), .names = "{.col}_count"),
        across(ends_with("_extended"), ~sum(., na.rm = TRUE), .names = "{.col}_count"),
        .groups = "drop"
      ) %>%
      arrange(year, data_source)
    cat("✓\n")
    
    cat("Writing data quality summary... ")
    dbWriteTable(con, "data_quality_summary", data_quality_summary, overwrite = TRUE)
    cat("✓\n")
    
    # Enhanced detailed data quality summary table with missing data statistics
    cat("Generating enhanced quality metrics [")
    cat("..")
    
    # Create the enhanced quality metrics summary
    data_quality_detailed <- all_data_processed %>%
      group_by(year, data_source, data_quality) %>%
      summarise(
        record_count = n(),
        # Standard interpolation stats
        interpolation_pct = mean(interpolation_used, na.rm = TRUE) * 100,
        avg_interpolation_count = mean(interpolation_count, na.rm = TRUE),
        
        # Missing data interpolation stats (new)
        missing_data_interpolation_pct = if ("missing_data_interpolation_used" %in% names(.)) {
          mean(missing_data_interpolation_used, na.rm = TRUE) * 100 
        } else { 0 },
        
        avg_missing_data_interpolation_count = if ("missing_data_interpolation_count" %in% names(.)) {
          mean(missing_data_interpolation_count, na.rm = TRUE)
        } else { 0 },
        
        # Gap statistics (when available)
        max_interpolation_gap = if ("max_interpolation_gap" %in% names(.)) {
          max(max_interpolation_gap, na.rm = TRUE)
        } else { NA_integer_ },
        
        # Extension stats
        extension_pct = mean(extension_used, na.rm = TRUE) * 100,
        avg_extension_count = mean(extension_count, na.rm = TRUE),
        
        # Data quality score (weighted average)
        avg_data_quality_score = if ("data_quality_score" %in% names(.)) {
          mean(data_quality_score, na.rm = TRUE)
        } else { NA_real_ },
        
        .groups = "drop"
      )
    cat("..] ✓\n")
    
    # Write the enhanced table to the database
    dbWriteTable(con, "data_quality_detailed", data_quality_detailed, overwrite = TRUE)
    
    # Also add a new table specifically for missing data statistics by variable
    cat("Generating missing data interpolation statistics [")
    
    # Get list of all missing data interpolation flag columns
    missing_data_flag_cols <- grep("_missing_data_interpolated$", names(all_data_processed), value = TRUE)
    
    # If no missing data interpolation flags found, create them for the standard interpolation flags
    if (length(missing_data_flag_cols) == 0) {
      cat("creating from standard flags..")
      
      # Get standard interpolation flags
      interpolation_flags <- grep("_interpolated$", names(all_data_processed), value = TRUE)
      interpolation_flags <- setdiff(interpolation_flags, c("interpolation_used", "interpolation_count"))
      
      if (length(interpolation_flags) > 0) {
        # Create missing data interpolation flags for each standard flag
        for (flag in interpolation_flags) {
          var_name <- sub("_interpolated$", "", flag)
          missing_flag <- paste0(var_name, "_missing_data_interpolated")
          
          # Identify counties with no values for this variable
          all_data_processed <- all_data_processed %>%
            group_by(GEOID) %>%
            mutate(
              all_missing = all(is.na(!!sym(var_name))),
              !!sym(missing_flag) := all_missing & !!sym(flag)
            ) %>%
            select(-all_missing) %>%
            ungroup()
        }
        
        # Update the list of missing data flags
        missing_data_flag_cols <- grep("_missing_data_interpolated$", names(all_data_processed), value = TRUE)
      }
    }
    
    if (length(missing_data_flag_cols) > 0) {
      # For each variable with a missing data interpolation flag, calculate statistics
      missing_data_stats <- data.frame()
      
      # Process each variable
      for (flag_col in missing_data_flag_cols) {
        # Extract the variable name from the flag column
        var_name <- sub("_missing_data_interpolated$", "", flag_col)
        
        # Skip if the original variable is missing
        if (!var_name %in% names(all_data_processed)) next
        
        # Get corresponding standard interpolation flag
        std_flag_col <- paste0(var_name, "_interpolated")
        
        # Statistics for this variable - by year to see patterns over time
        var_stats <- all_data_processed %>%
          group_by(year) %>%
          summarise(
            variable = var_name,
            total_records = n(),
            # Count of records where this variable has a value (not NA)
            has_value_count = sum(!is.na(!!sym(var_name))),
            has_value_pct = round(has_value_count / total_records * 100, 1),
            
            # Count of standard interpolations for this variable
            interpolated_count = if (std_flag_col %in% names(.)) {
              sum(!!sym(std_flag_col), na.rm = TRUE)
            } else { 0 },
            
            interpolated_pct = round(interpolated_count / total_records * 100, 1),
            
            # Count of missing data interpolations for this variable
            missing_interpolated_count = sum(!!sym(flag_col), na.rm = TRUE),
            missing_interpolated_pct = round(missing_interpolated_count / total_records * 100, 1),
            
            # Percentage of interpolations that were from missing data
            pct_of_interpolations_from_missing = if (interpolated_count > 0) {
              round(missing_interpolated_count / interpolated_count * 100, 1)
            } else { 0 },
            
            .groups = "drop"
          )
        
        # Add to the master statistics dataframe
        missing_data_stats <- bind_rows(missing_data_stats, var_stats)
      }
      
      # Write the missing data statistics to the database
      if (nrow(missing_data_stats) > 0) {
        dbWriteTable(con, "missing_data_interpolation_stats", missing_data_stats, overwrite = TRUE)
        cat("✓]\n")
        cat("Added statistics for", length(unique(missing_data_stats$variable)), "variables with missing data interpolation\n")
      } else {
        cat("no data]\n")
        cat("No variables had missing data that required interpolation\n")
      }
    } else {
      cat("no flags found]\n")
      cat("No missing data interpolation flags found in the dataset\n")
    }
    
    # Create data dictionary table
    data_dictionary <- crosswalk %>%
      select(std_name, description, category, preferred_source, years_available, 
             interpolate_recommended, extend_backwards, extend_method) %>%
      mutate(
        available_in_dataset = std_name %in% available_vars,
        column_in_db = if_else(available_in_dataset, 
                               paste0(std_name, " (+ ", std_name, "_interpolated, ", std_name, "_extended flags)"),
                               "Not available")
      )
    
    dbWriteTable(con, "data_dictionary", data_dictionary, overwrite = TRUE)
    
    # Create views for common use cases
    cat("\nCreating database views...\n")
    
    # Create a function to generate the list of all column names dynamically
    generate_column_list <- function(variables, data) {
      column_list <- c("GEOID", "NAME", "year", "source", "data_quality", "data_source", "data_vintage",
                      "interpolation_used", "interpolation_count", "extension_used", "extension_count")
      
      for (var in variables) {
        column_list <- c(column_list, var)
        if (paste0(var, "_interpolated") %in% names(data)) {
          column_list <- c(column_list, paste0(var, "_interpolated"))
        }
        if (paste0(var, "_extended") %in% names(data)) {
          column_list <- c(column_list, paste0(var, "_extended"))
        }
      }
      
      return(paste(column_list, collapse = ", "))
    }
    
    # Generate the column list safely
    all_columns <- c(id_cols, meta_cols, available_vars, 
                   paste0(available_vars, "_interpolated"), 
                   paste0(available_vars, "_extended"),
                   "interpolation_used", "interpolation_count", 
                   "extension_used", "extension_count",
                   "data_quality_score")
    columns <- paste(intersect(all_columns, names(all_data_processed)), collapse = ", ")
    
    # Create views with all columns
    # Try with an explicit error handler
    tryCatch({
      dbExecute(con, paste0("
        CREATE OR REPLACE VIEW latest_county_data AS
        WITH ranked_data AS (
          SELECT 
            *,
            ROW_NUMBER() OVER (PARTITION BY GEOID ORDER BY year DESC) as rn
          FROM county_sdoh_data
        )
        SELECT ", columns, "
        FROM ranked_data 
        WHERE rn = 1
      "))
    }, error = function(e) {
      cat("Error creating latest_county_data view:", conditionMessage(e), "\n")
      cat("Trying a simpler approach...\n")
      
      # Try a simpler approach with fewer columns
      basic_cols <- paste(c("GEOID", "NAME", "year", "source", "data_quality", 
                          "total_population", "median_household_income", "poverty_rate",
                          "uninsured_pct", "obesity_pct"), collapse = ", ")
      
      dbExecute(con, paste0("
        CREATE OR REPLACE VIEW latest_county_data AS
        WITH ranked_data AS (
          SELECT 
            ", basic_cols, ",
            ROW_NUMBER() OVER (PARTITION BY GEOID ORDER BY year DESC) as rn
          FROM county_sdoh_data
        )
        SELECT ", basic_cols, "
        FROM ranked_data 
        WHERE rn = 1
      "))
    })
    
    # Create time series view safely
    tryCatch({
      dbExecute(con, paste0("
        CREATE OR REPLACE VIEW county_time_series AS
        SELECT ", columns, "
        FROM county_sdoh_data
        ORDER BY GEOID, year
      "))
    }, error = function(e) {
      cat("Error creating county_time_series view:", conditionMessage(e), "\n")
      cat("Trying a simpler approach...\n")
      
      # Try a simpler approach with fewer columns
      basic_cols <- paste(c("GEOID", "NAME", "year", "source", "data_quality", 
                          "total_population", "median_household_income", "poverty_rate",
                          "uninsured_pct", "obesity_pct"), collapse = ", ")
      
      dbExecute(con, paste0("
        CREATE OR REPLACE VIEW county_time_series AS
        SELECT ", basic_cols, "
        FROM county_sdoh_data
        ORDER BY GEOID, year
      "))
    })
    
    # Create a view for health metrics
    health_vars <- crosswalk %>%
      filter(category %in% c("Health Status", "Health Behaviors", "Health Access")) %>%
      filter(std_name %in% available_vars) %>%
      pull(std_name)
    
    if (length(health_vars) > 0) {
      # Create health columns list
      health_columns <- c("GEOID", "NAME", "year", "source", "data_quality", "data_source", "data_vintage",
                         "interpolation_used", "interpolation_count", "extension_used", "extension_count")
      
      for (var in health_vars) {
        health_columns <- c(health_columns, var)
        if (paste0(var, "_interpolated") %in% names(all_data_processed)) {
          health_columns <- c(health_columns, paste0(var, "_interpolated"))
        }
        if (paste0(var, "_extended") %in% names(all_data_processed)) {
          health_columns <- c(health_columns, paste0(var, "_extended"))
        }
      }
      
      # Filter to valid columns
      health_columns <- intersect(health_columns, names(all_data_processed))
      
      # Create the view if we have columns
      if (length(health_columns) > 0) {
        health_columns_str <- paste(health_columns, collapse = ", ")
        
        tryCatch({
          dbExecute(con, paste0("
            CREATE OR REPLACE VIEW county_health_metrics AS
            SELECT ", health_columns_str, "
            FROM county_sdoh_data
            ORDER BY GEOID, year
          "))
          cat("Created county_health_metrics view with", length(health_vars), "health variables.\n")
        }, error = function(e) {
          cat("Error creating county_health_metrics view:", conditionMessage(e), "\n")
        })
      }
    }
    
    # Create views for major categories
    category_list <- c("Demographics", "Socioeconomic", "Education", "Housing", "Employment", "Transportation")
    
    for (cat in category_list) {
      cat_vars <- crosswalk %>%
        filter(category == cat) %>%
        filter(std_name %in% available_vars) %>%
        pull(std_name)
      
      if (length(cat_vars) > 0) {
        # Create category columns list
        cat_columns <- c("GEOID", "NAME", "year", "source", "data_quality", "data_source", "data_vintage",
                       "interpolation_used", "interpolation_count")
        
        for (var in cat_vars) {
          cat_columns <- c(cat_columns, var)
          if (paste0(var, "_interpolated") %in% names(all_data_processed)) {
            cat_columns <- c(cat_columns, paste0(var, "_interpolated"))
          }
        }
        
        # Filter to valid columns
        cat_columns <- intersect(cat_columns, names(all_data_processed))
        
        if (length(cat_columns) > 0) {
          cat_columns_str <- paste(cat_columns, collapse = ", ")
          view_name <- paste0("county_", tolower(cat))
          
          tryCatch({
            dbExecute(con, paste0("
              CREATE OR REPLACE VIEW ", view_name, " AS
              SELECT ", cat_columns_str, "
              FROM county_sdoh_data
              ORDER BY GEOID, year
            "))
            cat("Created", view_name, "view with", length(cat_vars), "variables.\n")
          }, error = function(e) {
            cat("Error creating", view_name, "view:", conditionMessage(e), "\n")
          })
        }
      }
    }
    
    # Create example queries with enhanced missing data interpolation examples
    example_queries <- c(
      "-- Get the latest data for all counties",
      "SELECT * FROM latest_county_data",
      "",
      "-- Get time series data for a specific county",
      "SELECT * FROM county_time_series WHERE GEOID = '06037' ORDER BY year",
      "",
      "-- Get health metrics for a specific county over time",
      "SELECT * FROM county_health_metrics WHERE GEOID = '06037' ORDER BY year",
      "",
      "-- Get counties with highest poverty rates in the latest year",
      "SELECT GEOID, NAME, year, poverty_rate FROM latest_county_data WHERE poverty_rate IS NOT NULL ORDER BY poverty_rate DESC LIMIT 10",
      "",
      "-- Compare health metrics between counties",
      "SELECT GEOID, NAME, year, obesity_pct, diabetes_pct, high_blood_pressure_pct FROM latest_county_data WHERE obesity_pct IS NOT NULL ORDER BY obesity_pct DESC LIMIT 10",
      "",
      "-- Get data quality information",
      "SELECT year, data_source, record_count, county_count FROM data_quality_summary ORDER BY year, data_source",
      "",
      "-- Find variables with most complete data",
      "SELECT std_name, description, category FROM data_dictionary WHERE available_in_dataset = TRUE ORDER BY category, std_name",
      "",
      "-- Get standard interpolation information by year",
      "SELECT year, COUNT(*) as total_counties, 
         SUM(CASE WHEN interpolation_used = TRUE THEN 1 ELSE 0 END) as interpolated_counties,
         SUM(CASE WHEN missing_data_interpolation_used = TRUE THEN 1 ELSE 0 END) as missing_data_interpolated_counties,
         SUM(CASE WHEN extension_used = TRUE THEN 1 ELSE 0 END) as extended_counties
       FROM county_sdoh_data
       GROUP BY year 
       ORDER BY year",
      "",
      "-- Get records with any interpolation from missing data",
      "SELECT * FROM county_sdoh_data WHERE missing_data_interpolation_used = TRUE LIMIT 10",
      "",
      "-- Find specific variable values that were interpolated from missing data",
      "SELECT GEOID, NAME, year, 
         poverty_rate,
         poverty_rate_interpolated, 
         poverty_rate_missing_data_interpolated
       FROM county_sdoh_data 
       WHERE poverty_rate_missing_data_interpolated = TRUE
       ORDER BY year, GEOID
       LIMIT 20",
      "",
      "-- Compare data quality across different types of counties",
      "SELECT 
         CASE 
           WHEN total_population > 1000000 THEN 'Large (1M+)'
           WHEN total_population > 500000 THEN 'Medium-Large (500K-1M)'
           WHEN total_population > 100000 THEN 'Medium (100K-500K)'
           WHEN total_population > 50000 THEN 'Small-Medium (50K-100K)'
           ELSE 'Small (<50K)'
         END as county_size,
         COUNT(*) as county_count,
         AVG(data_quality_score) as avg_quality_score,
         SUM(CASE WHEN interpolation_used = TRUE THEN 1 ELSE 0 END) / COUNT(*) * 100 as pct_interpolated,
         SUM(CASE WHEN missing_data_interpolation_used = TRUE THEN 1 ELSE 0 END) / COUNT(*) * 100 as pct_missing_interpolated
       FROM latest_county_data
       WHERE total_population IS NOT NULL
       GROUP BY 1
       ORDER BY AVG(total_population) DESC",
      "",
      "-- Detailed missing data interpolation statistics by variable and year",
      "SELECT * FROM missing_data_interpolation_stats ORDER BY variable, year",
      "",
      "-- Quality metrics by year and source",
      "SELECT * FROM data_quality_detailed ORDER BY year, data_source"
    )
    
    dbWriteTable(con, "example_queries", data.frame(query = example_queries), overwrite = TRUE)
    
    # Create a progress animation for the queries
    cat("\nFinalizing database... ")
    for (i in 1:10) {
      cat(".")
      Sys.sleep(0.1)  # Small delay for visual effect
    }
    
    # Close database connection
    dbDisconnect(con)
    cat(" Done!\n")
    cat("\n✅ Database operations completed successfully!\n")
    cat("===========================================\n")
  }, error = function(e) {
    cat("\n❌ ERROR in database operations:", conditionMessage(e), "\n")
    # Try to close connection if it exists
    if (exists("con") && dbIsValid(con)) {
      dbDisconnect(con)
    }
  })
  
  # Create CSV exports
  cat("\nExporting data to CSV files...\n")
  tryCatch({
    cat("Preparing export columns... ")
    # Use a subset of columns for the CSV to keep file size manageable
    export_cols <- c("GEOID", "NAME", "year", "data_source", "data_quality", 
                    "total_population", "median_household_income", "poverty_rate", 
                    "uninsured_pct", "obesity_pct", "diabetes_pct",
                    "interpolation_used", "interpolation_count", 
                    "extension_used", "extension_count")
    
    # Filter to valid columns
    export_cols <- intersect(export_cols, names(all_data_processed))
    cat("✓\n")
    
    # Export complete dataset - but with reduced columns
    cat("Exporting complete dataset [")
    for (i in 1:5) {
      cat(".")
      Sys.sleep(0.05)  # Small delay for visual effect
    }
    write_csv(all_data_processed %>% 
              select(all_of(export_cols)), 
              "county_sdoh_data_complete.csv")
    cat("] ✓\n")
    
    # Export county metadata
    cat("Exporting county metadata... ")
    write_csv(county_metadata, "county_metadata.csv")
    cat("✓\n")
    
    # Create data dictionary again for export
    cat("Creating data dictionary... ")
    csv_data_dictionary <- crosswalk %>%
      select(std_name, description, category, preferred_source, years_available, 
             interpolate_recommended, extend_backwards, extend_method) %>%
      mutate(
        available_in_dataset = std_name %in% available_vars,
        column_in_db = if_else(available_in_dataset, 
                               paste0(std_name, " (+ ", std_name, "_interpolated, ", std_name, "_extended flags)"),
                               "Not available")
      )
    cat("✓\n")
    
    # Export data dictionary
    cat("Exporting data dictionary... ")
    write_csv(csv_data_dictionary, "data_dictionary_complete.csv")
    cat("✓\n")
    
    # Create a sample dataset with 100 random counties
    cat("Creating sample dataset... ")
    set.seed(42)  # For reproducibility
    sample_counties <- county_metadata %>%
      slice_sample(n = 100) %>%
      pull(GEOID)
    
    sample_data <- all_data_processed %>%
      filter(GEOID %in% sample_counties & year >= 2017) %>%
      select(all_of(export_cols))
    
    write_csv(sample_data, "sample_county_data.csv")
    cat("✓\n")
    
    cat("\n✅ CSV exports completed successfully!\n")
    
  }, error = function(e) {
    cat("\n❌ Error exporting CSV files:", conditionMessage(e), "\n")
  })
  
  cat("\nData processing complete!\n")
  cat("Results stored in 'us_county_sdoh_data.duckdb' and CSV files.\n")
  cat("- Processed", nrow(all_data_processed), "county-year records\n")
  cat("- Covering", nrow(county_metadata), "counties\n")
  cat("- Including", length(available_vars), "standardized variables\n")
  
  # Count health metrics separately
  health_vars <- crosswalk %>%
    filter(category %in% c("Health Status", "Health Behaviors", "Health Access")) %>%
    filter(std_name %in% available_vars) %>%
    pull(std_name)
  
  # Check if life expectancy variables are included
  life_exp_vars <- c("life_expectancy", "life_expectancy_female", "life_expectancy_male")
  life_exp_vars_included <- life_exp_vars[life_exp_vars %in% available_vars]
    
  cat("- Added", length(health_vars), "health metrics from CDC PLACES data\n")
  if (length(life_exp_vars_included) > 0) {
    cat("- Added", length(life_exp_vars_included), "life expectancy metrics from IHME data\n")
  }
  
  # Return information about the processing
  return(list(
    record_count = nrow(all_data_processed),
    county_count = nrow(county_metadata),
    year_min = min(all_data_processed$year, na.rm = TRUE),
    year_max = max(all_data_processed$year, na.rm = TRUE),
    variables_count = length(available_vars),
    health_variables_count = length(health_vars),
    life_expectancy_variables_count = length(life_exp_vars_included),
    life_expectancy_included = include_life_expectancy,
    interpolation_used = if(interpolate) "Yes" else "No",
    extension_used = if(extend_health_data) "Yes" else "No",
    data_sources = unique(all_data_processed$data_source)
  ))
}