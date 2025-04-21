#!/usr/bin/env Rscript

# module_data_fetching.r
# Data fetching module for the SDOH pipeline

# Load required packages
library(dplyr)
library(readr)
library(httr)

# Make sure we have the log_message function
if (!exists("log_message")) {
  log_message <- function(message, level = "INFO", show_console = TRUE, log_file = NULL) {
    timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
    formatted_message <- paste(timestamp, "[", level, "]", message)
    
    if (show_console) {
      cat(formatted_message, "\n")
    }
    
    if (!is.null(log_file)) {
      if (!dir.exists(dirname(log_file)) && dirname(log_file) != ".") {
        dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
      }
      cat(formatted_message, "\n", file = log_file, append = TRUE)
    }
    
    return(formatted_message)
  }
}

# Helper function to handle log_message with different parameter sets
safe_log_message <- function(message, level = "INFO") {
  # Check if the log_message function has a show_console parameter
  if ("show_console" %in% names(formals(log_message))) {
    log_message(message, level = level, show_console = TRUE)
  } else {
    log_message(message, level = level)
  }
}

#' Fetch Census data for counties
#'
#' This function fetches data from the U.S. Census Bureau's API
#' for the specified years and variables.
#'
#' @param crosswalk Variable crosswalk containing Census variables
#' @param years Vector of years to fetch data for
#' @param refresh_cache Whether to refresh the cache
#' @param use_cache Whether to use cached data if available
#' @return A dataframe with Census data
get_census_data <- function(crosswalk, years, refresh_cache = FALSE, use_cache = TRUE) {
  # Define cache file path
  cache_dir <- "data/cache"
  cache_file <- file.path(cache_dir, "census_data.rds")
  
  # Check if cache exists and we can use it
  if (file.exists(cache_file) && use_cache && !refresh_cache) {
    safe_log_message("Loading Census data from cache...", level = "INFO")
    return(readRDS(cache_file))
  }
  
  # Check for pre-downloaded Census data files
  census_dirs <- c(
    "data/census_acs",
    "data/census_decennial",
    "data/census_pep",
    "data/cache/census"
  )
  
  # Look for CSV files with Census data
  census_files <- list()
  for (dir in census_dirs) {
    if (dir.exists(dir)) {
      # Look for CSV files with Census data
      files <- list.files(
        path = dir,
        pattern = "acs.*\\.csv$|dec.*\\.csv$|pep.*\\.csv$|census.*\\.csv$",
        full.names = TRUE,
        recursive = TRUE,
        ignore.case = TRUE
      )
      
      # Add to the list
      census_files <- c(census_files, files)
    }
  }
  
  # Check if we found any files
  if (length(census_files) == 0) {
    safe_log_message("ERROR: No Census data files found. Please download Census data.",
               level = "ERROR")
    safe_log_message("Required files should be in one of the following directories:",
               level = "ERROR")
    safe_log_message(paste(census_dirs, collapse = ", "),
               level = "ERROR")
    safe_log_message("File names should include 'acs', 'dec', or 'pep' with a CSV extension.",
               level = "ERROR")
    
    # Return empty dataframe with proper structure
    return(data.frame(
      geoid = character(0),
      name = character(0),
      state_fips = character(0),
      state_name = character(0),
      year = integer(0)
    ))
  }
  
  # Process files to create the combined dataset
  safe_log_message(paste("Found", length(census_files), "Census data files. Processing..."),
             level = "INFO")
  
  # Initialize list for each file's data
  file_data_list <- list()
  
  # Process each file
  for (file in census_files) {
    safe_log_message(paste("Processing Census file:", basename(file)),
               level = "INFO")
    
    # Extract year and type from filename
    filename <- basename(file)
    year_match <- regexpr("_[0-9]{4}", filename)
    
    # Figure out which type of Census data
    data_type <- if (grepl("acs", filename, ignore.case = TRUE)) {
      "ACS"
    } else if (grepl("dec", filename, ignore.case = TRUE)) {
      "Decennial"
    } else if (grepl("pep", filename, ignore.case = TRUE)) {
      "PEP"
    } else {
      "Unknown"
    }
    
    # Extract year if possible
    file_year <- if (year_match > 0) {
      as.numeric(substr(filename, year_match + 1, year_match + 4))
    } else {
      NA_integer_
    }
    
    # Only process if year is in the requested range
    if (!is.na(file_year) && file_year %in% years) {
      # Read the file
      file_data <- tryCatch({
        read.csv(file, stringsAsFactors = FALSE)
      }, error = function(e) {
        safe_log_message(paste("Error reading file:", e$message),
                   level = "ERROR")
        return(NULL)
      })
      
      # Process if we successfully read the file
      if (!is.null(file_data) && nrow(file_data) > 0) {
        # Ensure we have standard column names
        # Look for FIPS code
        if (!"geoid" %in% names(file_data)) {
          # Look for alternate column names
          fips_cols <- grep("fips|geoid|county_code|state_county", 
                           names(file_data), ignore.case = TRUE, value = TRUE)
          
          if (length(fips_cols) > 0) {
            # Rename the first match to geoid
            names(file_data)[names(file_data) == fips_cols[1]] <- "geoid"
          } else if ("state" %in% names(file_data) && "county" %in% names(file_data)) {
            # Construct FIPS from state and county
            file_data$geoid <- sprintf("%02d%03d", 
                                     as.numeric(file_data$state), 
                                     as.numeric(file_data$county))
          } else {
            # Can't determine FIPS code
            safe_log_message(paste("Cannot determine FIPS code in file:", filename),
                       level = "WARN")
            # Skip this file
            next
          }
        }
        
        # Ensure GEOID is standardized
        file_data$geoid <- sprintf("%05d", as.numeric(file_data$geoid))
        
        # Add year if missing
        if (!"year" %in% names(file_data)) {
          file_data$year <- file_year
        }
        
        # Add data source
        file_data$data_source <- paste("US Census Bureau", data_type)
        file_data$data_quality <- "direct"
        
        # Add to list
        file_data_list[[basename(file)]] <- file_data
      }
    }
  }
  
  # Combine all data
  if (length(file_data_list) == 0) {
    safe_log_message("ERROR: No valid Census data found for requested years.",
               level = "ERROR")
    
    # Return empty dataframe with proper structure
    return(data.frame(
      geoid = character(0),
      name = character(0),
      state_fips = character(0),
      state_name = character(0),
      year = integer(0)
    ))
  }
  
  # Combine all files
  combined_census_data <- bind_rows(file_data_list)
  
  # If we have Census data, save to cache
  if (nrow(combined_census_data) > 0) {
    safe_log_message(paste("Saving combined Census data with", 
                     nrow(combined_census_data), "rows to cache..."),
               level = "INFO")
    
    # Make sure cache directory exists
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Save to cache
    saveRDS(combined_census_data, cache_file)
  }
  
  return(combined_census_data)
}

#' Process and combine data from all sources
#'
#' This function processes and combines data from Census, NHGIS,
#' traffic safety, and other sources into a unified dataset.
#'
#' @param census_data Dataframe with Census data
#' @param nhgis_data Dataframe with NHGIS data (can be NULL)
#' @param years Vector of years to process
#' @param crosswalk Variable crosswalk
#' @return A processed dataset with all variables
get_processed_data <- function(census_data, nhgis_data, years, crosswalk) {
  log_message("Processing and combining data from all sources...",
             level = "INFO", show_console = TRUE)
  
  # Create a base dataset with county IDs and years
  if (is.null(census_data) || nrow(census_data) == 0) {
    log_message("WARNING: No Census data available. Creating base template only.",
               level = "WARN", show_console = TRUE)
    
    # Try to get county data from shapefiles or any other source
    counties <- NULL
    
    # Check in shapefiles directory
    shapefile_index_path <- "data/shapefiles/shapefile_index.csv"
    if (file.exists(shapefile_index_path)) {
      log_message("Trying to extract county information from shapefile index...",
                 level = "INFO", show_console = TRUE)
      shapefile_index <- read.csv(shapefile_index_path, stringsAsFactors = FALSE)
      
      if ("geoid" %in% names(shapefile_index) && "name" %in% names(shapefile_index)) {
        counties <- shapefile_index %>%
          select(geoid, name) %>%
          distinct()
        
        # Extract state FIPS from county FIPS
        counties$state_fips <- substr(counties$geoid, 1, 2)
        
        # Add state names
        # First create a state lookup
        state_lookup <- data.frame(
          state_fips = sprintf("%02d", 1:56),
          state_name = c(state.name, "District of Columbia", 
                        "Puerto Rico", "Virgin Islands", 
                        "Guam", "American Samoa", "Northern Mariana Islands"),
          stringsAsFactors = FALSE
        )
        
        # Join to get state names
        counties <- counties %>%
          left_join(state_lookup, by = "state_fips")
      }
    }
    
    # If still no county data, create minimal template
    if (is.null(counties) || nrow(counties) == 0) {
      log_message("WARNING: Could not find any county information. Creating minimal template.",
                 level = "WARN", show_console = TRUE)
      
      # Create empty dataframe
      counties <- data.frame(
        geoid = character(0),
        name = character(0),
        state_fips = character(0),
        state_name = character(0),
        stringsAsFactors = FALSE
      )
    }
    
    if (nrow(counties) > 0) {
      # Create a dataset with all years and counties
      years_df <- expand.grid(
        geoid = counties$geoid,
        year = years,
        stringsAsFactors = FALSE
      )
      
      # Merge counties info
      full_data <- merge(years_df, counties, by = "geoid")
      
      log_message(paste("Created base template with", nrow(full_data), 
                       "rows for", length(unique(counties$geoid)), 
                       "counties and", length(years), "years."),
                 level = "INFO", show_console = TRUE)
    } else {
      # No counties - create empty dataset with correct columns
      full_data <- data.frame(
        geoid = character(0),
        year = integer(0),
        name = character(0),
        state_fips = character(0),
        state_name = character(0),
        stringsAsFactors = FALSE
      )
      
      log_message("WARNING: Empty dataset created (no counties found).",
                 level = "WARN", show_console = TRUE)
    }
  } else {
    # Use the Census data as the base
    full_data <- census_data
    log_message(paste("Using Census data as base with", nrow(full_data), "rows."),
               level = "INFO", show_console = TRUE)
  }
  
  # First, try to get traffic safety data and add it to our dataset
  log_message("Adding traffic safety data...",
             level = "INFO", show_console = TRUE)
  
  if (exists("get_traffic_safety_data")) {
    # Fetch traffic safety data using the traffic safety module
    tryCatch({
      ts_data <- get_traffic_safety_data(years = years)
      
      # Check if ts_data has the required geoid and year columns for joining
      if (is.data.frame(ts_data) && nrow(ts_data) > 0 && 
          all(c("geoid", "year") %in% names(ts_data))) {
        # Get list of traffic safety variables
        ts_vars <- intersect(
          names(ts_data),
          crosswalk$variable_name[crosswalk$domain == "Traffic Safety"]
        )
        
        # If no exact matches found, try a looser match
        if (length(ts_vars) == 0) {
          ts_vars <- grep("fatalities|fatality_rate", names(ts_data), value = TRUE)
        }
        
        # If we have variables to add
        if (length(ts_vars) > 0) {
          log_message(paste("Found", length(ts_vars), "traffic safety variables to add"),
                     level = "INFO", show_console = TRUE)
          
          # Add data quality columns if they exist
          quality_vars <- character(0)
          for (var in ts_vars) {
            qual_col <- paste0(var, "_data_quality")
            if (qual_col %in% names(ts_data)) {
              quality_vars <- c(quality_vars, qual_col)
            }
          }
          
          # Prepare data for merge
          ts_merge_data <- ts_data[, c("geoid", "year", ts_vars, quality_vars)]
          
          # Merge with full_data
          full_data <- merge(full_data, ts_merge_data, 
                           by = c("geoid", "year"), 
                           all.x = TRUE)
          
          log_message(paste("Added traffic safety data to dataset."),
                     level = "INFO", show_console = TRUE)
        } else {
          log_message("No traffic safety variables found in data",
                     level = "WARN", show_console = TRUE)
        }
      } else {
        log_message("Traffic safety data doesn't have required columns for joining",
                   level = "WARN", show_console = TRUE)
      }
    }, error = function(e) {
      log_message(paste("Error adding traffic safety data:", conditionMessage(e)),
                 level = "ERROR", show_console = TRUE)
    })
  } else {
    log_message("Traffic safety data function not found - skipping",
               level = "WARN", show_console = TRUE)
  }
  
  # Add missing variables - BUT NOT WITH SIMULATED DATA
  log_message("Marking missing variables...",
             level = "INFO", show_console = TRUE)
  
  # Go through all variables in the crosswalk
  for (var in crosswalk$variable_name) {
    # Check if this variable exists in the data
    if (!var %in% names(full_data)) {
      # Add the column but set to NA (not simulated data)
      full_data[[var]] <- NA
      
      # Add data quality flag showing it's missing
      qual_col <- paste0(var, "_data_quality")
      full_data[[qual_col]] <- "missing"
      
      # Log that this variable is missing
      log_message(paste("Variable", var, "is not available in the dataset. Marked as missing."),
                 level = "INFO", show_console = TRUE)
    }
  }
  
  # Add IHME life expectancy data (using actual data files)
  log_message("Adding IHME life expectancy data...",
             level = "INFO", show_console = TRUE)
  
  ihme_vars <- crosswalk$variable_name[crosswalk$source == "IHME (Institute for Health Metrics and Evaluation)"]
  if (length(ihme_vars) > 0) {
    # Define the IHME data directory
    ihme_dir <- "data/ihme/CSV"
    
    # Check if the directory exists
    if (dir.exists(ihme_dir)) {
      # Get IHME CSV files
      ihme_files <- list.files(ihme_dir, pattern = "\\.CSV$", full.names = TRUE)
      
      if (length(ihme_files) > 0) {
        log_message(paste("Found", length(ihme_files), "IHME data files"),
                   level = "INFO", show_console = TRUE)
        
        # Create a dataframe to store all IHME life expectancy data
        ihme_data <- NULL
        
        # Process each file
        for (file in ihme_files) {
          tryCatch({
            # Extract file information
            filename <- basename(file)
            
            # Parse the filename to extract year, gender, and type
            # Format: IHME_USA_LE_COUNTY_RACE_ETHN_2000_2019_LT_YYYY_GENDER_YYYYMMDD.CSV
            year_match <- regexpr("_LT_(\\d{4})_", filename)
            gender_match <- regexpr("_(BOTH|MALE|FEMALE)_", filename)
            
            if (year_match > 0 && gender_match > 0) {
              year <- as.numeric(substr(filename, 
                                     year_match + 4, 
                                     year_match + 7))
              gender <- substr(filename, 
                            gender_match + 1, 
                            gender_match + nchar("BOTH") - 1)
              
              # Read the file
              log_message(paste("Reading IHME file for year", year, "and gender", gender),
                         level = "INFO", show_console = TRUE)
              
              file_data <- read.csv(file, stringsAsFactors = FALSE)
              
              # Check for different IHME CSV formats
              if ("location_id" %in% names(file_data) && "location_name" %in% names(file_data)) {
                # This is the standard format with location_id, val, etc.
                log_message(paste("Processing IHME file with standard format:", filename), 
                           level = "INFO", show_console = TRUE)
                
                # Extract race_ethnicity from race_name or race_id
                if ("race_name" %in% names(file_data)) {
                  # Map race_name to our standard codes
                  race_name_mapping <- list(
                    "Total" = "all",
                    "Latino" = "hispanic",
                    "White" = "nhw",
                    "Black" = "nhb",
                    "Asian" = "nhasian",
                    "AIAN" = "nhaian",
                    "NHPI" = "nhpi",
                    "API" = "nhasian", # API (Asian/Pacific Islander) in older IHME files
                    "Multiple races" = "multirace",
                    "Other" = "multirace"
                  )
                  
                  # Add race_ethnicity column based on race_name
                  file_data$race_ethnicity <- sapply(file_data$race_name, function(name) {
                    if (name %in% names(race_name_mapping)) {
                      return(race_name_mapping[[name]])
                    } else {
                      return("all")  # Default to "all" if not found
                    }
                  })
                } else {
                  # Default to "all" if we can't determine race
                  file_data$race_ethnicity <- "all"
                }
                
                # Rename columns to match our schema
                file_data <- file_data %>%
                  rename(
                    geoid = location_id,
                    county_name = location_name,
                    life_expectancy = val,
                    le_lower_ci = lower,
                    le_upper_ci = upper
                  )
                
                # Add year and gender
                file_data$year <- year
                file_data$gender <- gender
                
                # Combine with main IHME dataset
                if (is.null(ihme_data)) {
                  ihme_data <- file_data
                } else {
                  ihme_data <- rbind(ihme_data, file_data)
                }
              } else if ("Location" %in% names(file_data) && "LE_both" %in% names(file_data)) {
                # This is the legacy format with Location, LE_both, LE_race_* columns
                log_message(paste("Processing IHME file with legacy format:", filename), 
                           level = "INFO", show_console = TRUE)
                
                # Determine year from filename or assume latest (2019)
                year_from_filename <- as.numeric(gsub(".*_(\\d{4})\\.CSV$", "\\1", filename))
                if (is.na(year_from_filename)) {
                  year_from_filename <- 2019  # Default to 2019 if not in filename
                }
                
                # Process race-specific life expectancy data
                race_data_list <- list()
                
                # Add overall life expectancy data
                overall_data <- data.frame(
                  geoid = file_data$FIPS,
                  county_name = file_data$Location,
                  race_ethnicity = "all",
                  gender = "BOTH",
                  year = year_from_filename,
                  life_expectancy = file_data$LE_both,
                  le_lower_ci = file_data$LE_both - file_data$SD_both,
                  le_upper_ci = file_data$LE_both + file_data$SD_both,
                  stringsAsFactors = FALSE
                )
                
                # Add male life expectancy data
                male_data <- data.frame(
                  geoid = file_data$FIPS,
                  county_name = file_data$Location,
                  race_ethnicity = "all",
                  gender = "MALE",
                  year = year_from_filename,
                  life_expectancy = file_data$LE_male,
                  le_lower_ci = file_data$LE_male - file_data$SD_male,
                  le_upper_ci = file_data$LE_male + file_data$SD_male,
                  stringsAsFactors = FALSE
                )
                
                # Add female life expectancy data
                female_data <- data.frame(
                  geoid = file_data$FIPS,
                  county_name = file_data$Location,
                  race_ethnicity = "all",
                  gender = "FEMALE",
                  year = year_from_filename,
                  life_expectancy = file_data$LE_female,
                  le_lower_ci = file_data$LE_female - file_data$SD_female,
                  le_upper_ci = file_data$LE_female + file_data$SD_female,
                  stringsAsFactors = FALSE
                )
                
                # Combine all data
                legacy_data <- rbind(
                  overall_data,
                  male_data,
                  female_data
                )
                
                # For each race-specific column, create a separate entry
                race_columns <- grep("^LE_race_", names(file_data), value = TRUE)
                if (length(race_columns) > 0) {
                  log_message(paste("Found race-specific columns:", paste(race_columns, collapse = ", ")),
                             level = "INFO", show_console = TRUE)
                  
                  for (race_col in race_columns) {
                    # Extract the race name from the column name
                    race_name <- sub("^LE_race_", "", race_col)
                    
                    # Map the race name to our standard code
                    race_code <- switch(race_name,
                                      "white" = "nhw",
                                      "black" = "nhb",
                                      "hispanic" = "hispanic",
                                      "asian" = "nhasian",
                                      "aian" = "nhaian",
                                      "api" = "nhasian",
                                      "multirace" = "multirace",
                                      "all") # Default
                    
                    # Create data frame for this race
                    race_specific_data <- data.frame(
                      geoid = file_data$FIPS,
                      county_name = file_data$Location,
                      race_ethnicity = race_code,
                      gender = "BOTH",  # Race-specific data in legacy format is for both genders
                      year = year_from_filename,
                      life_expectancy = file_data[[race_col]],
                      le_lower_ci = NA,  # CIs not available in legacy format
                      le_upper_ci = NA,
                      stringsAsFactors = FALSE
                    )
                    
                    # Add to the combined dataset
                    legacy_data <- rbind(legacy_data, race_specific_data)
                  }
                }
                
                # Format the FIPS code to match our standard geoid format
                legacy_data$geoid <- sprintf("%05d", as.numeric(legacy_data$geoid))
                
                log_message(paste("Processed legacy format with", nrow(legacy_data), "rows for year", year_from_filename),
                           level = "INFO", show_console = TRUE)
                
                # Combine with main IHME dataset
                if (is.null(ihme_data)) {
                  ihme_data <- legacy_data
                } else {
                  ihme_data <- rbind(ihme_data, legacy_data)
                }
              } else {
                log_message(paste("IHME file", filename, "doesn't have expected columns"),
                           level = "WARN", show_console = TRUE)
                log_message(paste("Columns found:", paste(names(file_data), collapse = ", ")),
                           level = "INFO", show_console = TRUE)
              }
            }
          }, error = function(e) {
            log_message(paste("Error processing IHME file", basename(file), ":", conditionMessage(e)),
                       level = "ERROR", show_console = TRUE)
          })
        }
        
        # If we have IHME data, process it and add to full_data
        if (!is.null(ihme_data) && nrow(ihme_data) > 0) {
          log_message(paste("Successfully loaded", nrow(ihme_data), "rows of IHME life expectancy data"),
                     level = "INFO", show_console = TRUE)
          
          # Convert geoid to match the format in full_data
          ihme_data$geoid <- sprintf("%05d", as.numeric(ihme_data$geoid))
          
          # Process IHME life expectancy data with all race/ethnicity breakdowns
          log_message("Processing IHME life expectancy data with race/ethnicity breakdowns",
                      level = "INFO", show_console = TRUE)
                      
          # Create separate dataframes for each gender
          both_data <- ihme_data %>% filter(gender == "BOTH")
          male_data <- ihme_data %>% filter(gender == "MALE")
          female_data <- ihme_data %>% filter(gender == "FEMALE")
          
          # Create a base dataframe that will hold all life expectancy variables
          # Start with just geoid and year columns for all counties and years
          counties_years <- distinct(full_data, geoid, year)
          
          # Function to process data for a specific race/ethnicity
          process_race_data <- function(race_code, race_label) {
            log_message(paste("Processing", race_label, "life expectancy data"),
                        level = "INFO", show_console = TRUE)
            
            # Variables to create
            race_vars <- list()
            
            # Process overall (both genders) data
            race_both <- both_data %>% 
              filter(race_ethnicity == race_code) %>%
              select(geoid, year, life_expectancy, le_lower_ci, le_upper_ci)
            
            # Define variable name based on race
            if (race_code == "all") {
              var_name <- "life_expectancy"
              lower_name <- "le_lower_ci"
              upper_name <- "le_upper_ci"
            } else {
              var_name <- paste0("life_expectancy_", race_code)
              lower_name <- paste0("le_", race_code, "_lower_ci")
              upper_name <- paste0("le_", race_code, "_upper_ci")
            }
            
            # Rename columns
            names(race_both)[names(race_both) == "life_expectancy"] <- var_name
            names(race_both)[names(race_both) == "le_lower_ci"] <- lower_name
            names(race_both)[names(race_both) == "le_upper_ci"] <- upper_name
            
            # Process male data
            race_male <- male_data %>% 
              filter(race_ethnicity == race_code) %>%
              select(geoid, year, life_expectancy, le_lower_ci, le_upper_ci)
            
            # Define male variable names
            if (race_code == "all") {
              male_var_name <- "life_expectancy_male"
              male_lower_name <- "le_male_lower_ci"
              male_upper_name <- "le_male_upper_ci"
            } else {
              male_var_name <- paste0("life_expectancy_male_", race_code)
              male_lower_name <- paste0("le_male_", race_code, "_lower_ci")
              male_upper_name <- paste0("le_male_", race_code, "_upper_ci")
            }
            
            # Rename male columns
            names(race_male)[names(race_male) == "life_expectancy"] <- male_var_name
            names(race_male)[names(race_male) == "le_lower_ci"] <- male_lower_name
            names(race_male)[names(race_male) == "le_upper_ci"] <- male_upper_name
            
            # Process female data
            race_female <- female_data %>% 
              filter(race_ethnicity == race_code) %>%
              select(geoid, year, life_expectancy, le_lower_ci, le_upper_ci)
            
            # Define female variable names
            if (race_code == "all") {
              female_var_name <- "life_expectancy_female"
              female_lower_name <- "le_female_lower_ci"
              female_upper_name <- "le_female_upper_ci"
            } else {
              female_var_name <- paste0("life_expectancy_female_", race_code)
              female_lower_name <- paste0("le_female_", race_code, "_lower_ci")
              female_upper_name <- paste0("le_female_", race_code, "_upper_ci")
            }
            
            # Rename female columns
            names(race_female)[names(race_female) == "life_expectancy"] <- female_var_name
            names(race_female)[names(race_female) == "le_lower_ci"] <- female_lower_name
            names(race_female)[names(race_female) == "le_upper_ci"] <- female_upper_name
            
            # Merge all data for this race
            result <- merge(race_both, race_male, by = c("geoid", "year"), all = TRUE)
            result <- merge(result, race_female, by = c("geoid", "year"), all = TRUE)
            
            return(result)
          }
          
          # Race/ethnicity mapping between IHME codes and our variable names
          race_mapping <- list(
            "all" = "all",           # Overall
            "hispanic" = "hispanic", # Hispanic
            "nhw" = "nhw",           # Non-Hispanic White
            "nhb" = "nhb",           # Non-Hispanic Black
            "nham" = "nhaian",       # Non-Hispanic American Indian/Alaska Native
            "nha" = "nhasian",       # Non-Hispanic Asian
            "nhpi" = "nhpi",         # Non-Hispanic Pacific Islander
            "oth" = "multirace"      # Other/multiracial
          )
          
          # Process each race/ethnicity group and combine
          all_race_data <- NULL
          
          # Process overall (all races) data first
          all_race_result <- process_race_data("all", "All Races")
          
          # Start combined result with all races
          combined_result <- all_race_result
          
          # Process other race/ethnicity groups
          for (race_code in names(race_mapping)) {
            if (race_code != "all") {
              race_label <- race_mapping[[race_code]]
              race_result <- process_race_data(race_code, race_label)
              
              # Merge with combined result
              combined_result <- merge(combined_result, race_result, 
                                      by = c("geoid", "year"), 
                                      all = TRUE)
            }
          }
          
          # Join with full_data
          log_message("Merging all IHME life expectancy variables with main dataset",
                     level = "INFO", show_console = TRUE)
          
          # Merge by geoid and year
          full_data <- merge(full_data, combined_result, 
                           by = c("geoid", "year"), 
                           all.x = TRUE)
          
          # Add data quality flags for all IHME variables that are now in the dataset
          ihme_vars_in_data <- intersect(names(full_data), ihme_vars)
          for (var in ihme_vars_in_data) {
            quality_col <- paste0(var, "_data_quality")
            full_data[[quality_col]] <- "direct"
          }
          
          # Count how many IHME variables were successfully added
          num_ihme_vars_added <- length(ihme_vars_in_data)
          log_message(paste("Successfully added", num_ihme_vars_added, "IHME life expectancy variables to the dataset"),
                     level = "INFO", show_console = TRUE)
          
          log_message("Successfully added IHME life expectancy data to the dataset",
                     level = "INFO", show_console = TRUE)
          
          return(full_data)
        }
      } else {
        log_message("No IHME CSV files found in data/ihme/CSV",
                   level = "WARN", show_console = TRUE)
      }
    } else {
      log_message("IHME data directory not found at data/ihme/CSV",
                 level = "WARN", show_console = TRUE)
    }
    
    # NO SIMULATED DATA - If we couldn't load the actual IHME data, log an error
    log_message("ERROR: Could not load IHME life expectancy data files. These are required.",
               level = "ERROR", show_console = TRUE)
    log_message("Please ensure the IHME CSV files are present in the data/ihme/CSV directory.",
               level = "ERROR", show_console = TRUE)
    log_message("The pipeline requires actual data files - simulated data is not acceptable.",
               level = "ERROR", show_console = TRUE)
    
    # Set missing columns to NA with appropriate error flags
    for (var in ihme_vars) {
      if (!var %in% names(full_data)) {
        # Set to NA instead of simulated data
        full_data[[var]] <- NA
        
        # Add data quality flag
        quality_col <- paste0(var, "_data_quality")
        full_data[[quality_col]] <- "missing"
      }
    }
  }
  
  # Count available data by quality
  if (nrow(full_data) > 0) {
    # Count variables with direct, interpolated, and missing data
    qual_cols <- grep("_data_quality$", names(full_data), value = TRUE)
    
    if (length(qual_cols) > 0) {
      # Count number of variables by data quality
      quality_counts <- list(
        direct = 0,
        interpolated = 0,
        extrapolated = 0,
        missing = 0
      )
      
      for (col in qual_cols) {
        # Get variable name
        var_name <- sub("_data_quality$", "", col)
        
        # Count by quality type
        if (any(full_data[[col]] == "direct", na.rm = TRUE)) {
          quality_counts$direct <- quality_counts$direct + 1
        } else if (any(full_data[[col]] == "interpolated", na.rm = TRUE)) {
          quality_counts$interpolated <- quality_counts$interpolated + 1
        } else if (any(full_data[[col]] == "extrapolated", na.rm = TRUE)) {
          quality_counts$extrapolated <- quality_counts$extrapolated + 1
        } else if (any(full_data[[col]] == "missing", na.rm = TRUE) || 
                  all(is.na(full_data[[var_name]]))) {
          quality_counts$missing <- quality_counts$missing + 1
        }
      }
      
      log_message("Data quality summary:", level = "INFO", show_console = TRUE)
      log_message(paste(" - Direct data:", quality_counts$direct, "variables"),
                 level = "INFO", show_console = TRUE)
      log_message(paste(" - Interpolated data:", quality_counts$interpolated, "variables"),
                 level = "INFO", show_console = TRUE)
      log_message(paste(" - Extrapolated data:", quality_counts$extrapolated, "variables"),
                 level = "INFO", show_console = TRUE)
      log_message(paste(" - Missing data:", quality_counts$missing, "variables"),
                 level = "INFO", show_console = TRUE)
    }
  }
  
  log_message(paste("Processed data created with", nrow(full_data), "rows and", ncol(full_data), "columns"),
             level = "INFO", show_console = TRUE)
  
  # Count variables by domain
  domain_counts <- crosswalk %>%
    group_by(domain) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  
  log_message("Variable counts by domain:", level = "INFO", show_console = TRUE)
  for (i in 1:nrow(domain_counts)) {
    log_message(paste(" -", domain_counts$domain[i], ":", domain_counts$count[i]),
               level = "INFO", show_console = TRUE)
  }
  
  return(full_data)
}

# Only run if executed directly (not sourced)
if (!exists("is_sourced") || !is_sourced()) {
  message("Data fetching module cannot be run directly. Use the unified pipeline.")
}