#!/usr/bin/env Rscript

# Extended Data Processing
# This script processes and integrates data from all sources in the extended SDOH pipeline

library(tidyverse)
library(lubridate)
library(zoo)  # For interpolation
library(DBI)
library(duckdb)
library(parallel)

#' Process extended SDOH data
#'
#' Integrates, cleans, and standardizes data from all sources in the extended pipeline,
#' handling missing data, interpolation, and data quality flagging.
#'
#' @param data_sources List of data sources from all fetchers
#' @param years Vector of years to process
#' @param skip_interpolation Whether to skip interpolation for missing years
#' @param original_db_path Path to the original SDOH database (optional)
#' @param verbose Whether to print verbose output
#' @param data_quality_flags List with standardized data quality flags
#' @return A processed data frame with all extended SDOH data
process_extended_data_v2 <- function(data_sources,
                                   years,
                                   skip_interpolation = FALSE,
                                   original_db_path = NULL,
                                   verbose = FALSE,
                                   data_quality_flags = list(
                                     direct = "direct",
                                     interpolated = "interpolated",
                                     extrapolated = "extrapolated",
                                     simulated = "simulated",
                                     missing = NA,
                                     imputed = "imputed"
                                   )) {
  # Helper function for clean output
  print_msg <- function(msg, detail_level = 1) {
    # If verbose is FALSE, only print messages with detail_level = 1
    # If verbose is TRUE, print all messages
    if (verbose || detail_level == 1) {
      # Check if being run interactively
      is_interactive_run <- !exists("is_sourced") || (is.logical(is_sourced) && !is_sourced)
      if (is_interactive_run) {
        message(msg)
      } else {
        cat(msg, "\n")
      }
    }
  }
  
  # Start timing
  start_time <- Sys.time()
  print_msg("Starting extended data processing...")
  
  # Get the number of data sources
  num_sources <- length(data_sources)
  print_msg(paste("Processing", num_sources, "data sources"))
  
  # Check if we have any data
  if (num_sources == 0) {
    print_msg("No data sources provided!")
    return(NULL)
  }
  
  # Get all source names
  source_names <- names(data_sources)
  print_msg(paste("Data sources:", paste(source_names, collapse=", ")), 2)
  
  # Count data rows in each source
  for (source_name in source_names) {
    source_data <- data_sources[[source_name]]
    # Check if source_data is a data frame (or tibble) and has rows
    if (!is.null(source_data) && is.data.frame(source_data) && nrow(source_data) > 0) {
      print_msg(paste(source_name, ":", nrow(source_data), "rows"), 2)
    } else if (!is.null(source_data) && is.list(source_data) && !is.data.frame(source_data)) {
      # Handle nested lists (like Census)
      print_msg(paste(source_name, ": Nested data structure"), 2)
      for (subname in names(source_data)) {
        subdata <- source_data[[subname]]
        if (is.data.frame(subdata) && nrow(subdata) > 0) {
          print_msg(paste("  -", subname, ":", nrow(subdata), "rows"), 2)
        } else {
          print_msg(paste("  -", subname, ": No data or empty dataframe"), 2)
        }
      }
    } else {
      print_msg(paste(source_name, ": No data or empty dataframe"), 2)
    }
  }
  
  # First, get a list of all counties to ensure complete coverage
  # Try several methods to get county list
  
  # 1. Try from the data sources
  all_counties <- NULL
  for (source_name in source_names) {
    source_data <- data_sources[[source_name]]
    
    # First handle direct data frames
    if (!is.null(source_data) && is.data.frame(source_data) && nrow(source_data) > 0 && "GEOID" %in% names(source_data)) {
      source_counties <- source_data %>%
        distinct(GEOID) %>%
        pull(GEOID)
      
      if (is.null(all_counties)) {
        all_counties <- source_counties
      } else {
        all_counties <- union(all_counties, source_counties)
      }
    }
    # Then handle nested lists (like Census)
    else if (!is.null(source_data) && is.list(source_data) && !is.data.frame(source_data)) {
      for (subname in names(source_data)) {
        subdata <- source_data[[subname]]
        if (is.data.frame(subdata) && nrow(subdata) > 0 && "GEOID" %in% names(subdata)) {
          source_counties <- subdata %>%
            distinct(GEOID) %>%
            pull(GEOID)
          
          if (is.null(all_counties)) {
            all_counties <- source_counties
          } else {
            all_counties <- union(all_counties, source_counties)
          }
        }
      }
    }
  }
  
  # 2. Try from the original database if provided
  if (is.null(all_counties) && !is.null(original_db_path) && file.exists(original_db_path)) {
    tryCatch({
      # Connect to the original database
      orig_con <- dbConnect(duckdb::duckdb(), dbdir = original_db_path)
      
      # Get counties
      if (dbExistsTable(orig_con, "counties")) {
        all_counties <- dbGetQuery(orig_con, "SELECT geoid FROM counties")$geoid
      }
      
      # Close connection
      dbDisconnect(orig_con)
    }, error = function(e) {
      print_msg(paste("Error getting counties from original database:", conditionMessage(e)))
    })
  }
  
  # 3. Use hard-coded list of counties if necessary
  if (is.null(all_counties)) {
    # Try to load county metadata file with default name
    county_metadata_file <- file.path(dirname(dirname(getwd())), "county_metadata.csv")
    if (file.exists(county_metadata_file)) {
      county_metadata <- read_csv(county_metadata_file, show_col_types = FALSE)
      if ("GEOID" %in% names(county_metadata)) {
        all_counties <- county_metadata$GEOID
      } else if ("geoid" %in% names(county_metadata)) {
        all_counties <- county_metadata$geoid
      } else if ("fips" %in% names(county_metadata)) {
        all_counties <- county_metadata$fips
      }
    }
    
    # If still null, use tidycensus if available
    if (is.null(all_counties)) {
      tryCatch({
        if (requireNamespace("tidycensus", quietly = TRUE)) {
          # Load county list from Census API
          if (Sys.getenv("CENSUS_API_KEY") != "") {
            counties <- tidycensus::get_decennial(
              geography = "county",
              variables = "P001001", # Total population
              year = 2020,
              geometry = FALSE
            )
            all_counties <- counties$GEOID
          }
        }
      }, error = function(e) {
        print_msg("Could not load tidycensus or get Census API data")
      })
    }
    
    # If still null, use a minimal county list
    if (is.null(all_counties)) {
      print_msg("WARNING: Using minimal county list as fallback!")
      # Create a very basic list with just a few counties
      all_counties <- c(
        "01001", "01003", "01005", "01007", "01009", "01011", "01013", "01015",
        "01017", "01019", "01021", "01023", "01025", "01027", "01029", "01031"
      )
    }
  }
  
  # Ensure all counties are formatted consistently (5-digit string with leading zeros)
  all_counties <- sprintf("%05d", as.numeric(all_counties))
  
  # Create the empty dataframe for all counties and years
  county_years <- expand.grid(
    GEOID = all_counties,
    year = years,
    stringsAsFactors = FALSE
  )
  
  print_msg(paste("Created base dataframe with", nrow(county_years), "county-years"))
  
  # List to store processed data by domain
  processed_domains <- list()
  
  # Process each data source
  for (source_name in source_names) {
    source_data <- data_sources[[source_name]]
    
    # Skip if NULL or empty
    if (is.null(source_data) || nrow(source_data) == 0) {
      print_msg(paste("Skipping empty source:", source_name))
      next
    }
    
    print_msg(paste("Processing source:", source_name))
    
    # Ensure GEOID is properly formatted
    if ("GEOID" %in% names(source_data)) {
      source_data$GEOID <- sprintf("%05d", as.numeric(source_data$GEOID))
    }
    
    # Get all variables from this source
    # Exclude flags and metadata columns
    var_cols <- grep("_data_quality$|_data_source$|_data_vintage$|GEOID|year", 
                   names(source_data), value = TRUE, invert = TRUE)
    
    print_msg(paste("Found", length(var_cols), "variables"), 2)
    
    # Merge with base dataframe to ensure complete county-year coverage
    merged_data <- county_years %>%
      left_join(source_data, by = c("GEOID", "year"))
    
    # Handle interpolation for each variable if needed
    if (!skip_interpolation) {
      print_msg("Performing interpolation for missing values...")
      
      # Define a function to interpolate a single variable - improved approach
      interpolate_variable <- function(var_name) {
        # Skip special columns like GEOID, year, NAME that don't need interpolation
        if (var_name %in% c("GEOID", "year", "NAME")) {
          # Return a dataframe with just the GEOID and year columns
          return(merged_data %>% select(GEOID, year))
        }
        
        # Check if the variable exists in the data
        if (!(var_name %in% names(merged_data))) {
          # Skip if variable doesn't exist
          if (verbose) {
            print_msg(paste("Skipping", var_name, "- not found in data"), 2)
          }
          return(merged_data %>% select(GEOID, year))
        }
        
        # Get quality flag column names
        quality_col <- paste0(var_name, "_data_quality")
        source_col <- paste0(var_name, "_data_source")
        vintage_col <- paste0(var_name, "_data_vintage")
        
        # Get metadata columns to include in result
        meta_cols <- c(quality_col, source_col, vintage_col)
        meta_cols <- meta_cols[meta_cols %in% names(merged_data)]
        
        # Create result dataframe with available columns
        result_df <- merged_data %>%
          select(GEOID, year, var_name, all_of(meta_cols)) %>%
          arrange(GEOID, year)
        
        # Create quality column if it doesn't exist
        if (!quality_col %in% names(result_df)) {
          if (verbose) {
            print_msg(paste("Creating quality flag column for", var_name), 2)
          }
          result_df[[quality_col]] <- ifelse(is.na(result_df[[var_name]]), 
                                          data_quality_flags$missing, 
                                          data_quality_flags$direct)
        }
        
        # Create source column if it doesn't exist
        if (!source_col %in% names(result_df)) {
          result_df[[source_col]] <- ifelse(is.na(result_df[[var_name]]), 
                                         "NOT_AVAILABLE", 
                                         source_name)
        }
        
        # Create vintage column if it doesn't exist
        if (!vintage_col %in% names(result_df)) {
          result_df[[vintage_col]] <- ifelse(is.na(result_df[[var_name]]), 
                                          NA_character_, 
                                          as.character(result_df$year))
        }
        
        if (verbose) {
          print_msg(paste("Interpolating", var_name, "for", length(unique(result_df$GEOID)), "counties"), 2)
        }
        
        # Process each GEOID separately for more consistent interpolation
        for (geoid in unique(result_df$GEOID)) {
          # Get data for this county
          county_indices <- which(result_df$GEOID == geoid)
          county_data <- result_df[county_indices, ]
          
          # Count values that are not NA
          non_na_count <- sum(!is.na(county_data[[var_name]]))
          
          # Skip if there's not enough data for interpolation
          if (non_na_count < 2) {
            if (verbose && non_na_count > 0) {
              print_msg(paste("Skipping interpolation for", var_name, "in county", geoid, 
                           "- only", non_na_count, "values available"), 3)
            }
            next
          }
          
          # Get years with data and valid range
          valid_indices <- which(!is.na(county_data[[var_name]]))
          data_years <- county_data$year[valid_indices]
          min_year <- min(data_years)
          max_year <- max(data_years)
          
          # Get data values for interpolation
          y_values <- county_data[[var_name]][valid_indices]
          
          # Use approx() for interpolation
          interpolated <- approx(data_years, y_values, 
                                xout = county_data$year, 
                                rule = 2)  # rule 2 = constant extrapolation
          
          # Find indices where there are NAs in original data but values in interpolated data
          na_indices <- which(is.na(county_data[[var_name]]) & !is.na(interpolated$y))
          
          if (length(na_indices) > 0) {
            if (verbose) {
              print_msg(paste("Interpolating", length(na_indices), "values for", var_name, 
                           "in county", geoid), 3)
            }
            
            # Update values and flags
            for (i in na_indices) {
              # Get the global index in result_df
              result_idx <- county_indices[i]
              
              # Set the interpolated value
              result_df[result_idx, var_name] <- interpolated$y[i]
              
              # Determine appropriate quality flag (interpolated vs extrapolated)
              current_year <- county_data$year[i]
              if (current_year < min_year || current_year > max_year) {
                # Extrapolated - outside the range of known data
                result_df[result_idx, quality_col] <- data_quality_flags$extrapolated
                result_df[result_idx, source_col] <- paste0("Extrapolated from ", source_name)
                
                # Identify which end it was extrapolated from
                if (current_year < min_year) {
                  result_df[result_idx, vintage_col] <- paste0("extrapolated_from_", min_year)
                } else {
                  result_df[result_idx, vintage_col] <- paste0("extrapolated_from_", max_year)
                }
              } else {
                # Interpolated - between known data points
                result_df[result_idx, quality_col] <- data_quality_flags$interpolated
                result_df[result_idx, source_col] <- paste0("Interpolated from ", source_name)
                
                # Find surrounding years for vintage information
                lower_year <- max(data_years[data_years <= current_year])
                upper_year <- min(data_years[data_years >= current_year])
                
                result_df[result_idx, vintage_col] <- paste0("interpolated_", lower_year, "_", upper_year)
              }
            }
          }
        }
        
        return(result_df)
      }
      
      # Process variables sequentially instead of in parallel due to data_quality_flags issue
      # This is more reliable even if slightly slower
      print_msg("Using sequential processing for interpolation")
      
      for (var_name in var_cols) {
        var_data <- interpolate_variable(var_name)
        
        # Join back to the main data
        if (!is.null(var_data)) {
          merged_data <- merged_data %>%
            select(-any_of(c(var_name, 
                           paste0(var_name, "_data_quality"),
                           paste0(var_name, "_data_source"),
                           paste0(var_name, "_data_vintage")))) %>%
            left_join(var_data, by = c("GEOID", "year"))
          
          if (verbose) {
            print_msg(paste("Processed variable:", var_name), 2)
          }
        }
      }
    }
    
    # Add to processed domains list
    processed_domains[[source_name]] <- merged_data
    
    print_msg(paste("Processed", source_name, "with", nrow(merged_data), "rows"))
  }
  
  # Combine all domains
  if (length(processed_domains) > 0) {
    print_msg("Combining all processed domains...")
    
    # Start with the base dataframe
    combined_data <- county_years
    
    # Add each domain
    for (domain_name in names(processed_domains)) {
      domain_data <- processed_domains[[domain_name]]
      
      # Get variable columns to join
      join_vars <- setdiff(names(domain_data), c("GEOID", "year"))
      
      # Join to combined data
      if (length(join_vars) > 0) {
        combined_data <- combined_data %>%
          left_join(
            domain_data %>% select(GEOID, year, all_of(join_vars)),
            by = c("GEOID", "year")
          )
      }
    }
    
    print_msg(paste("Created combined dataset with", nrow(combined_data), "rows and", 
                  ncol(combined_data), "columns"))
    
    # Integrate with original data if provided
    if (!is.null(original_db_path) && file.exists(original_db_path)) {
      print_msg("Integrating with original SDOH data...")
      
      tryCatch({
        # Connect to the original database
        orig_con <- dbConnect(duckdb::duckdb(), dbdir = original_db_path)
        
        # Get original data
        if (dbExistsTable(orig_con, "sdoh_data")) {
          # Query to get data for our years
          original_data_query <- paste0(
            "SELECT * FROM sdoh_data WHERE year IN (", 
            paste(years, collapse = ","), ")"
          )
          
          # This might be too much data for memory, so we'll get just the variable names
          variable_names <- dbGetQuery(orig_con, "SELECT DISTINCT variable_name FROM sdoh_data")$variable_name
          
          print_msg(paste("Found", length(variable_names), "variables in original data"), 2)
          
          # Process in chunks by variable
          chunk_size <- 10  # Process 10 variables at a time
          num_chunks <- ceiling(length(variable_names) / chunk_size)
          
          original_data_list <- list()
          
          for (i in 1:num_chunks) {
            start_idx <- (i - 1) * chunk_size + 1
            end_idx <- min(i * chunk_size, length(variable_names))
            chunk_vars <- variable_names[start_idx:end_idx]
            
            # Query for just these variables
            chunk_query <- paste0(
              "SELECT geoid, year, variable_name, value, data_quality, data_source, data_vintage FROM sdoh_data ",
              "WHERE year IN (", paste(years, collapse = ","), ") ",
              "AND variable_name IN ('", paste(chunk_vars, collapse = "','"), "')"
            )
            
            chunk_data <- dbGetQuery(orig_con, chunk_query)
            
            # Convert to wide format
            if (nrow(chunk_data) > 0) {
              # Pivot to make each variable a column
              wide_chunk <- chunk_data %>%
                pivot_wider(
                  id_cols = c("geoid", "year"),
                  names_from = "variable_name",
                  values_from = "value"
                )
              
              # Add quality flags
              for (var in chunk_vars) {
                var_quality <- chunk_data %>%
                  filter(variable_name == var) %>%
                  select(geoid, year, data_quality)
                
                if (nrow(var_quality) > 0) {
                  wide_chunk[[paste0(var, "_data_quality")]] <- 
                    var_quality$data_quality[match(paste(wide_chunk$geoid, wide_chunk$year),
                                                paste(var_quality$geoid, var_quality$year))]
                }
                
                var_source <- chunk_data %>%
                  filter(variable_name == var) %>%
                  select(geoid, year, data_source)
                
                if (nrow(var_source) > 0) {
                  wide_chunk[[paste0(var, "_data_source")]] <- 
                    var_source$data_source[match(paste(wide_chunk$geoid, wide_chunk$year),
                                               paste(var_source$geoid, var_source$year))]
                }
                
                var_vintage <- chunk_data %>%
                  filter(variable_name == var) %>%
                  select(geoid, year, data_vintage)
                
                if (nrow(var_vintage) > 0) {
                  wide_chunk[[paste0(var, "_data_vintage")]] <- 
                    var_vintage$data_vintage[match(paste(wide_chunk$geoid, wide_chunk$year),
                                                paste(var_vintage$geoid, var_vintage$year))]
                }
              }
              
              # Add to list
              original_data_list[[i]] <- wide_chunk
            }
            
            if (verbose) {
              print_msg(paste("Processed chunk", i, "of", num_chunks, 
                            "(", start_idx, "-", end_idx, ")"), 2)
            }
          }
          
          # Combine all chunks
          if (length(original_data_list) > 0) {
            # First, get all unique columns
            all_cols <- unique(unlist(lapply(original_data_list, names)))
            
            # Make sure each dataframe has all columns
            for (i in seq_along(original_data_list)) {
              missing_cols <- setdiff(all_cols, names(original_data_list[[i]]))
              for (col in missing_cols) {
                original_data_list[[i]][[col]] <- NA
              }
            }
            
            # Combine all chunks
            original_data <- bind_rows(original_data_list)
            
            # Rename columns to match our format
            names(original_data)[names(original_data) == "geoid"] <- "GEOID"
            
            # Make sure GEOID is in the right format
            original_data$GEOID <- sprintf("%05d", as.numeric(original_data$GEOID))
            
            print_msg(paste("Loaded", nrow(original_data), "rows from original data"), 2)
            
            # Merge with combined data
            if (nrow(original_data) > 0) {
              # Get columns that exist in original data but not in combined data
              orig_cols <- setdiff(names(original_data), names(combined_data))
              
              if (length(orig_cols) > 0) {
                # Join additional columns
                combined_data <- combined_data %>%
                  left_join(
                    original_data %>% select(GEOID, year, all_of(orig_cols)),
                    by = c("GEOID", "year")
                  )
                
                print_msg(paste("Added", length(orig_cols), "columns from original data"), 2)
              }
            }
          }
        }
        
        # Close connection
        dbDisconnect(orig_con)
      }, error = function(e) {
        print_msg(paste("Error integrating with original data:", conditionMessage(e)))
      })
    }
    
    # Final data validation and cleaning
    print_msg("Performing final data validation and cleaning...")
    
    # Get all data columns (not metadata)
    data_cols <- grep("_data_quality$|_data_source$|_data_vintage$", 
                    names(combined_data), value = TRUE, invert = TRUE)
    data_cols <- setdiff(data_cols, c("GEOID", "year"))
    
    # Check for implausible values and apply corrections
    for (col in data_cols) {
      # First, get the expected data type
      col_type <- class(combined_data[[col]])
      
      # Check if it's a numeric column
      if ("numeric" %in% col_type) {
        # Look for obvious outliers
        
        # For percentage variables (ends with _pct, _percent, _rate and typically 0-100)
        if (grepl("_pct$|_percent$|_rate$", col) && 
            !grepl("_ratio$|_index$|per_|_per_", col)) {
          
          # Get non-NA values
          values <- combined_data[[col]][!is.na(combined_data[[col]])]
          
          if (length(values) > 0) {
            # Check if this looks like a percentage (most values 0-100)
            if (median(values, na.rm = TRUE) < 100 && median(values, na.rm = TRUE) > 0) {
              # Flag extremely high values (>100 for percentages)
              extreme_high <- which(combined_data[[col]] > 100 & !is.na(combined_data[[col]]))
              
              if (length(extreme_high) > 0) {
                # Flag these in the quality column
                quality_col <- paste0(col, "_data_quality")
                if (quality_col %in% names(combined_data)) {
                  combined_data[[quality_col]][extreme_high] <- 
                    paste0(combined_data[[quality_col]][extreme_high], "_outlier_capped")
                }
                
                # Cap at 100
                combined_data[[col]][extreme_high] <- 100
                
                if (verbose) {
                  print_msg(paste("Capped", length(extreme_high), "values >100 for", col), 2)
                }
              }
              
              # Flag negative values for percentages
              negative <- which(combined_data[[col]] < 0 & !is.na(combined_data[[col]]))
              
              if (length(negative) > 0) {
                # Flag these in the quality column
                quality_col <- paste0(col, "_data_quality")
                if (quality_col %in% names(combined_data)) {
                  combined_data[[quality_col]][negative] <- 
                    paste0(combined_data[[quality_col]][negative], "_outlier_floored")
                }
                
                # Set to 0
                combined_data[[col]][negative] <- 0
                
                if (verbose) {
                  print_msg(paste("Floored", length(negative), "negative values for", col), 2)
                }
              }
            }
          }
        }
      }
    }
    
    # Calculate processing time
    end_time <- Sys.time()
    duration <- difftime(end_time, start_time, units = "mins")
    print_msg(paste("Completed extended data processing in", round(duration, 2), "minutes"))
    
    return(combined_data)
  } else {
    print_msg("No data to process!")
    return(NULL)
  }
}

# This lets the function be used when the script is sourced
is_sourced <- function() {
  return(!identical(environment(), globalenv()))
}

# If the script is run directly, test the function
if (!is_sourced()) {
  cat("Testing extended data processing...\n")
  
  # Create some test data sources
  test_food <- data.frame(
    GEOID = rep(c("01001", "01003"), each = 3),
    year = rep(2010:2012, 2),
    grocery_stores_per_1000 = c(0.5, 0.6, 0.7, 0.4, 0.5, 0.6),
    grocery_stores_per_1000_data_quality = rep("direct", 6),
    grocery_stores_per_1000_data_source = rep("USDA Food Environment Atlas", 6),
    grocery_stores_per_1000_data_vintage = rep("2010-2012", 6)
  )
  
  test_env <- data.frame(
    GEOID = rep(c("01001", "01003"), each = 3),
    year = rep(2010:2012, 2),
    air_quality_days_unhealthy = c(5, 6, 7, 8, 9, 10),
    air_quality_days_unhealthy_data_quality = rep("direct", 6),
    air_quality_days_unhealthy_data_source = rep("EPA Air Quality System", 6),
    air_quality_days_unhealthy_data_vintage = rep("2010-2012", 6)
  )
  
  # Put them in a list
  data_sources <- list(
    food = test_food,
    env = test_env
  )
  
  # Process the test data
  result <- process_extended_data_v2(
    data_sources = data_sources,
    years = 2010:2015,  # Include years not in the source to test interpolation
    skip_interpolation = FALSE,
    verbose = TRUE
  )
  
  cat("Test completed with", nrow(result), "rows and", ncol(result), "columns\n")
}