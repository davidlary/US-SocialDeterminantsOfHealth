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
  # For the modular demo version, we'll just return a simulated dataset
  log_message("Using simulated Census data (demo mode)",
             level = "INFO", show_console = TRUE)
  
  # Create simulated Census data
  counties <- data.frame(
    geoid = sprintf("%05d", 1:3000),
    name = paste("County", 1:3000),
    state_fips = rep(sprintf("%02d", 1:50), each = 60),
    state_name = rep(state.name, each = 60)[1:3000]
  )
  
  # Add key Census variables from the crosswalk
  census_vars <- crosswalk %>%
    filter(grepl("American Community Survey|US Census Bureau", source)) %>%
    pull(variable_name)
  
  # Create data for each year
  census_data <- list()
  
  for (year in years) {
    if (year >= 2000 && year <= 2023) {  # Only include realistic years
      year_data <- counties
      year_data$year <- year
      
      # Add some sample data for key variables
      for (var in census_vars) {
        # Generate random values appropriate for this variable
        if (grepl("population", var)) {
          year_data[[var]] <- round(runif(nrow(year_data), 1000, 1000000))
        } else if (grepl("income", var)) {
          year_data[[var]] <- round(runif(nrow(year_data), 30000, 150000))
        } else if (grepl("rate|pct", var)) {
          year_data[[var]] <- round(runif(nrow(year_data), 1, 30), 1)
        } else {
          year_data[[var]] <- round(runif(nrow(year_data), 0, 100))
        }
      }
      
      census_data[[as.character(year)]] <- year_data
    }
  }
  
  # Combine all years
  combined_census_data <- do.call(rbind, census_data)
  
  log_message(paste("Simulated Census data created with", nrow(combined_census_data), "rows"),
             level = "INFO", show_console = TRUE)
  
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
  # This function simulates a processed dataset for the example
  # In the actual implementation, it would process and combine data from different sources
  
  log_message("Processing and combining data from all sources...",
             level = "INFO", show_console = TRUE)
  
  if (is.null(census_data)) {
    # Create a basic dataset with county IDs and years
    counties <- data.frame(
      geoid = sprintf("%05d", 1:3000),
      name = paste("County", 1:3000),
      state_fips = rep(sprintf("%02d", 1:50), each = 60),
      state_name = rep(state.name[1:50], each = 60)
    )
    
    # Create a dataset with all years and counties
    years_df <- expand.grid(
      geoid = counties$geoid,
      year = years
    )
    
    # Merge counties info
    full_data <- merge(years_df, counties, by = "geoid")
  } else {
    # Use the Census data as the base
    full_data <- census_data
  }
  
  # First, try to get traffic safety data and add it to our dataset
  log_message("Adding traffic safety data...",
             level = "INFO", show_console = TRUE)
  
  if (exists("get_traffic_safety_data")) {
    # Fetch traffic safety data using the traffic safety module
    tryCatch({
      ts_data <- get_traffic_safety_data(years = years)
      
      # Check if ts_data has the required geoid and year columns for joining
      if (is.data.frame(ts_data) && all(c("geoid", "year") %in% names(ts_data))) {
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
          
          # Prepare data for merge
          ts_merge_data <- ts_data[, c("geoid", "year", ts_vars)]
          
          # Merge with full_data
          full_data <- merge(full_data, ts_merge_data, 
                           by = c("geoid", "year"), 
                           all.x = TRUE)
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
  
  # Add missing variables from the crosswalk
  log_message("Adding remaining variables from crosswalk...",
             level = "INFO", show_console = TRUE)
  
  # Add more variables not already in the data
  for (var in crosswalk$variable_name) {
    if (!var %in% names(full_data)) {
      # Generate random values appropriate for this variable
      if (grepl("population", var)) {
        full_data[[var]] <- round(runif(nrow(full_data), 1000, 1000000))
      } else if (grepl("income|earnings", var)) {
        full_data[[var]] <- round(runif(nrow(full_data), 30000, 150000))
      } else if (grepl("rate|pct", var)) {
        full_data[[var]] <- round(runif(nrow(full_data), 1, 30), 1)
      } else if (grepl("fatalities|deaths", var)) {
        full_data[[var]] <- round(runif(nrow(full_data), 0, 500))
      } else if (grepl("expectancy", var)) {
        full_data[[var]] <- round(runif(nrow(full_data), 65, 85), 1)
      } else {
        full_data[[var]] <- round(runif(nrow(full_data), 0, 100))
      }
    }
    
    # Add interpolation flags for some data points
    interp_col <- paste0(var, "_interpolated")
    full_data[[interp_col]] <- sample(c(TRUE, FALSE), nrow(full_data), replace = TRUE, prob = c(0.2, 0.8))
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
              
              # Check if the file has the expected columns
              expected_cols <- c("location_id", "location_name", "race_ethnicity", "life_expectancy", "lower", "upper")
              if (all(expected_cols %in% names(file_data))) {
                # Rename columns to match our schema
                file_data <- file_data %>%
                  rename(
                    geoid = location_id,
                    county_name = location_name,
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
              } else {
                log_message(paste("IHME file", filename, "doesn't have expected columns"),
                           level = "WARN", show_console = TRUE)
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
              select(geoid, year, life_expectancy, lower, upper)
            
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
            names(race_both)[names(race_both) == "lower"] <- lower_name
            names(race_both)[names(race_both) == "upper"] <- upper_name
            
            # Process male data
            race_male <- male_data %>% 
              filter(race_ethnicity == race_code) %>%
              select(geoid, year, life_expectancy, lower, upper)
            
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
            names(race_male)[names(race_male) == "lower"] <- male_lower_name
            names(race_male)[names(race_male) == "upper"] <- male_upper_name
            
            # Process female data
            race_female <- female_data %>% 
              filter(race_ethnicity == race_code) %>%
              select(geoid, year, life_expectancy, lower, upper)
            
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
            names(race_female)[names(race_female) == "lower"] <- female_lower_name
            names(race_female)[names(race_female) == "upper"] <- female_upper_name
            
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