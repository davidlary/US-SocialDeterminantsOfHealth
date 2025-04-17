#!/usr/bin/env Rscript

# USDA Food Environment Atlas Data Fetcher
# This script handles retrieval of food environment data from USDA sources
# Enhanced version with multiple data sources and improved error handling

library(tidyverse)
library(httr)
library(readxl)
library(lubridate)
library(sf)
library(zoo) # For interpolation if needed

#' Fetch USDA Food Environment Atlas data
#'
#' Retrieves food environment data from the USDA Food Environment Atlas and
#' Food Access Research Atlas, which provides county-level data on food access,
#' food insecurity, food deserts, and related measures.
#'
#' This enhanced version includes:
#' - Multiple USDA data sources (Food Environment Atlas & Food Access Research Atlas)
#' - Improved error handling and offline fallbacks
#' - Comprehensive data validation
#' - Robust interpolation for missing years
#'
#' @param years Vector of years to include
#' @param cache_dir Directory to store cache files
#' @param refresh_cache Whether to refresh the cache
#' @param allow_simulation Whether to generate simulated data if real data not available
#' @param offline_mode If TRUE, will only use cached data without attempting downloads
#' @param allow_interpolation Whether to interpolate missing years
#' @param data_quality_flags List with standardized data quality flags
#' @return A data frame with food environment data for all requested years
fetch_usda_food_atlas <- function(years, 
                                 cache_dir = "data/cache", 
                                 refresh_cache = FALSE,
                                 allow_simulation = FALSE,
                                 allow_interpolation = TRUE,
                                 data_quality_flags = list(
                                   direct = "direct",
                                   interpolated = "interpolated",
                                   extrapolated = "extrapolated",
                                   simulated = "simulated",
                                   missing = NA,
                                   imputed = "imputed"
                                 ),
                                 offline_mode = FALSE) {
  # Helper function for clean output
  print_msg <- function(msg) {
    # Check if being run interactively
    is_interactive_run <- !exists("is_sourced") || (is.logical(is_sourced) && !is_sourced)
    if (is_interactive_run) {
      message(msg)
    } else {
      cat(msg, "\n")
    }
  }
  
  # Define cache file
  cache_file <- file.path(cache_dir, "usda_food_atlas_data.rds")
  
  # Use cache if available and not refreshing
  if (!refresh_cache && file.exists(cache_file)) {
    print_msg("Loading cached USDA Food Environment Atlas data...")
    food_atlas_data <- readRDS(cache_file)
    
    # Check if all requested years are in the cache
    cached_years <- unique(food_atlas_data$year)
    missing_years <- setdiff(years, cached_years)
    
    # Check for empty cache with just placeholder data
    if (nrow(food_atlas_data) <= 1 || 
        (is.data.frame(food_atlas_data) && "data_source" %in% names(food_atlas_data) && 
         any(grepl("SIMULATED", food_atlas_data$data_source)))) {
      print_msg("Cached Food Atlas data appears to be empty or a placeholder. Will process files again.")
      # Force refresh by continuing past this point
    } else if (length(missing_years) == 0) {
      print_msg("Using complete cached Food Atlas data.")
      return(food_atlas_data %>% filter(year %in% years))
    } else {
      print_msg(paste("Cache missing years:", paste(missing_years, collapse=", ")))
    }
  }
  
  # Make sure cache directory exists
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    print_msg(paste("Created cache directory at:", cache_dir))
  }
  
  # Make data directory if needed - ensure path is relative to current working directory
  # Rather than assuming a "data" directory, create a subdirectory in the cache_dir
  data_dir <- file.path(cache_dir, "usda")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
    print_msg(paste("Created USDA Food Atlas data directory at:", data_dir))
  }
  
  # Helper function to validate data files
  validate_data_file <- function(file_path, expected_type = "excel") {
    if (!file.exists(file_path)) return(FALSE)
    
    # Check file size (shouldn't be too small)
    file_size <- file.info(file_path)$size
    if (file_size < 1000) {
      print_msg(paste("Warning: File", file_path, "appears to be too small (", 
                      file_size, "bytes)"))
      return(FALSE)
    }
    
    # Try to read the file to validate it works
    if (expected_type == "excel") {
      tryCatch({
        # Just try to read sheet names to validate
        sheets <- readxl::excel_sheets(file_path)
        return(length(sheets) > 0)
      }, error = function(e) {
        print_msg(paste("Error validating Excel file:", conditionMessage(e)))
        return(FALSE)
      })
    } else if (expected_type == "csv") {
      tryCatch({
        # Try to read the first few rows
        headers <- readr::read_lines(file_path, n_max = 2)
        return(length(headers) > 0)
      }, error = function(e) {
        print_msg(paste("Error validating CSV file:", conditionMessage(e)))
        return(FALSE)
      })
    }
    
    return(TRUE)
  }
  
  # Safe download function with retries
  safe_download <- function(url, dest_file, description, retries = 3, wait_time = 2) {
    if (offline_mode) {
      print_msg("Skipping download in offline mode")
      return(file.exists(dest_file))
    }
    
    print_msg(paste("Downloading", description, "from:", url))
    
    for (attempt in 1:retries) {
      success <- tryCatch({
        # Set timeout to 60 seconds
        options(timeout = 60)
        
        # Try to download with httr for better control
        response <- httr::GET(url, httr::write_disk(dest_file, overwrite = TRUE),
                             httr::timeout(60))
        
        # Check status code
        if (httr::status_code(response) != 200) {
          print_msg(paste("HTTP error:", httr::status_code(response)))
          return(FALSE)
        }
        
        # Validate downloaded file
        file_valid <- validate_data_file(dest_file, 
                                       expected_type = tools::file_ext(dest_file))
        
        if (!file_valid) {
          print_msg("Downloaded file appears to be invalid")
          return(FALSE)
        }
        
        TRUE
      }, error = function(e) {
        print_msg(paste("Error on download attempt", attempt, ":", conditionMessage(e)))
        FALSE
      })
      
      if (success) {
        print_msg("Download successful!")
        return(TRUE)
      } else if (attempt < retries) {
        print_msg(paste("Retrying in", wait_time, "seconds... (Attempt", attempt, "of", retries, ")"))
        Sys.sleep(wait_time)
        # Increase wait time for next retry
        wait_time <- wait_time * 2
      }
    }
    
    print_msg(paste("Failed to download after", retries, "attempts"))
    
    # Check if we have an existing file to use
    if (file.exists(dest_file) && validate_data_file(dest_file)) {
      print_msg("Using existing file as fallback")
      return(TRUE)
    }
    
    return(FALSE)
  }
  
  # Food Environment Atlas data
  # Define direct download URLs for USDA Food Environment Atlas
  # Note: These URLs may change, check USDA site for updates
  food_env_base_url <- "https://www.ers.usda.gov/webdocs/DataFiles/80526/"
  food_env_file <- "FoodEnvironmentAtlas.xls"
  food_env_url <- paste0(food_env_base_url, food_env_file)
  
  # Define local file path for downloaded data
  food_env_local_file <- file.path(data_dir, food_env_file)
  
  # Check if file exists, download if it doesn't
  food_env_available <- FALSE
  if (!file.exists(food_env_local_file) || refresh_cache) {
    food_env_available <- safe_download(food_env_url, food_env_local_file, 
                                     "USDA Food Environment Atlas")
  } else {
    print_msg(paste("Using existing USDA Food Atlas file:", food_env_local_file))
    food_env_available <- validate_data_file(food_env_local_file)
  }
  
  # Food Access Research Atlas data (contains food desert information)
  # Define URL for Food Access Research Atlas
  food_access_base_url <- "https://www.ers.usda.gov/webdocs/DataFiles/80591/"
  food_access_file <- "FoodAccessResearchAtlasData.csv"
  food_access_url <- paste0(food_access_base_url, food_access_file)
  
  # Define local file path
  food_access_local_file <- file.path(data_dir, food_access_file)
  
  # Check if file exists, download if it doesn't
  food_access_available <- FALSE
  if (!file.exists(food_access_local_file) || refresh_cache) {
    food_access_available <- safe_download(food_access_url, food_access_local_file, 
                                        "USDA Food Access Research Atlas")
  } else {
    print_msg(paste("Using existing Food Access Research Atlas file:", food_access_local_file))
    food_access_available <- validate_data_file(food_access_local_file, "csv")
  }
  
  # Function to parse the Food Atlas Excel file
  parse_food_atlas <- function(file_path) {
    if (!file.exists(file_path)) {
      return(NULL)
    }
    
    print_msg("Reading USDA Food Environment Atlas data...")
    
    # The Food Atlas Excel file has multiple sheets - we need the county data
    # First check sheet names
    sheets <- excel_sheets(file_path)
    print_msg(paste("Found sheets:", paste(sheets, collapse=", ")))
    
    # Get the right sheet (typically "COUNTY" or "CountyData" or similar)
    county_sheet <- sheets[grep("COUNTY|County", sheets, ignore.case = TRUE)][1]
    if (is.na(county_sheet)) {
      # Fallback to first sheet if no obvious county sheet
      county_sheet <- sheets[1]
    }
    print_msg(paste("Using sheet:", county_sheet))
    
    # Read the Excel file
    raw_data <- tryCatch({
      readxl::read_excel(
        file_path, 
        sheet = county_sheet,
        col_types = "text"  # Read all as text initially
      )
    }, error = function(e) {
      print_msg(paste("Error reading Excel file:", conditionMessage(e)))
      return(NULL)
    })
    
    # Check if data reading was successful
    if (is.null(raw_data) || nrow(raw_data) == 0) {
      print_msg("Failed to read data from Excel file")
      return(NULL)
    }
    
    # Check column names
    print_msg(paste("Found columns:", paste(head(names(raw_data), 10), collapse=", "), "..."))
    
    # Identify key columns for county identification
    # Common column names in USDA datasets
    fips_col <- grep("FIPS|fips|^GEOID|^geoid", names(raw_data), value = TRUE)[1]
    county_col <- grep("County|county|COUNTY", names(raw_data), value = TRUE)[1]
    state_col <- grep("State|state|STATE", names(raw_data), value = TRUE)[1]
    
    # Ensure we found necessary columns
    if (is.na(fips_col)) {
      print_msg("Warning: No FIPS/GEOID column found - data might not link properly")
      # Try to construct FIPS from other columns if possible
      if (!is.na(state_col) && !is.na(county_col)) {
        print_msg("Attempting to create FIPS from state and county codes")
        # Check for state and county code columns
        state_code_col <- grep("State Code|state_code|STCNTY", names(raw_data), value = TRUE)[1]
        county_code_col <- grep("County Code|county_code", names(raw_data), value = TRUE)[1]
        
        if (!is.na(state_code_col) && !is.na(county_code_col)) {
          raw_data <- raw_data %>%
            mutate(GEOID = sprintf("%02d%03d", 
                                  as.numeric(get(state_code_col)), 
                                  as.numeric(get(county_code_col))))
          fips_col <- "GEOID"
        }
      }
    }
    
    # Rename key identifier columns to standard names
    if (!is.na(fips_col)) {
      raw_data <- raw_data %>% rename(GEOID = all_of(fips_col))
    } else {
      # If we still don't have FIPS, this data will be hard to use
      print_msg("Warning: Unable to identify or create FIPS codes")
      if (nrow(raw_data) < 4000) {  # Heuristic check - expecting county-level data
        print_msg("Data contains too few rows for county level, may not be county data")
      }
    }
    
    # Standardize the dataset format
    processed_data <- raw_data %>%
      mutate(
        # Ensure GEOID is properly formatted (5 digits with leading zeros)
        GEOID = if ("GEOID" %in% names(.)) {
          sprintf("%05d", as.numeric(GEOID))
        } else {
          NA_character_
        },
        
        # Create a NAME column if possible
        NAME = if (all(c(county_col, state_col) %in% names(.))) {
          paste0(!!sym(county_col), ", ", !!sym(state_col))
        } else if (!is.na(county_col)) {
          !!sym(county_col)
        } else {
          NA_character_
        }
      )
    
    # Try to extract the data year from the file
    # First look for year columns in the data
    year_col <- grep("^year$|^Year$|^YEAR$|^data_year$|^vintage$", 
                   names(processed_data), value = TRUE)[1]
    
    # If no obvious year column, try to extract from the file name or modification date
    if (is.na(year_col)) {
      # Try to extract year from filename (e.g., "FoodAtlas_2021.xls")
      filename_year <- as.numeric(regmatches(
        basename(file_path),
        regexpr("20[0-9]{2}", basename(file_path))
      ))
      
      file_year <- if (!is.na(filename_year)) {
        filename_year
      } else {
        # Fallback to file modification time
        as.numeric(format(file.mtime(file_path), "%Y"))
      }
    } else {
      # Use the first value in the year column as the data year
      file_year <- as.numeric(processed_data[[year_col]][1])
      # If that fails, use file modified date
      if (is.na(file_year)) {
        file_year <- as.numeric(format(file.mtime(file_path), "%Y"))
      }
    }
    
    # Expanded list of variables to extract from Food Environment Atlas
    food_variables <- c(
      # Store access variables
      "grocery_stores_per_1000" = "GROCPTH",
      "supercenters_per_1000" = "SUPSRPTH",
      "convenience_stores_per_1000" = "CONVSPTH",
      "specialized_food_stores_per_1000" = "SPECPTH",
      "snap_authorized_stores_per_1000" = "SNAPSPTH",
      "wic_authorized_stores_per_1000" = "WICSPTH",
      "farmers_markets_per_1000" = "FMRKTPTH",
      
      # Restaurant access variables
      "fast_food_restaurants_per_1000" = "FASTFDPTH",
      "full_service_restaurants_per_1000" = "FSRPTH",
      
      # Food access/insecurity variables
      "low_income_low_access_pct" = "PCT_LILATRACTS_1AND10",
      "low_income_low_access_child_pct" = "PCT_CHILD_LILATRACTS_1AND10",
      "low_income_low_access_seniors_pct" = "PCT_SENIORS_LILATRACTS_1AND10",
      "low_access_vehicle_pct" = "PCT_LACCESS_POP10",
      "low_income_pct" = "PCT_LACCESS_LOWI10",
      "snap_participation_rate" = "SNAPSPTH",
      "snap_benefits_redemption_per_capita" = "REDEMP_SNAPS",
      
      # Food security variables
      "food_insecurity_rate" = "FOODINSEC_13_15",
      "child_food_insecurity_rate" = "CHILDINSEC_13_15",
      
      # Food prices variables
      "price_index_fruits_vegetables" = "PRICE_FRUIT",
      "price_index_meat" = "PRICE_MEAT",
      "price_index_soda" = "PRICE_SODA",
      "price_index_milk" = "PRICE_MILK",
      
      # Food assistance variables
      "school_lunch_pct" = "PCT_FREE_LUNCH",
      "summer_food_program_pct" = "PCT_SFSP",
      
      # Health indicators
      "adult_obesity_pct" = "PCT_OBESE_ADULTS13",
      "adult_diabetes_pct" = "PCT_DIABETES_ADULTS13"
    )
    
    # Find which variables exist in the data
    available_vars <- intersect(unname(food_variables), names(processed_data))
    available_std_vars <- names(food_variables)[food_variables %in% available_vars]
    
    print_msg(paste("Found", length(available_vars), "of", length(food_variables), "target variables"))
    
    # For each variable, create a standardized column
    result <- processed_data %>%
      select(GEOID, NAME) %>%
      mutate(year = file_year) %>%
      as_tibble()
    
    # Add each found variable
    for (i in seq_along(available_vars)) {
      var_name <- available_std_vars[i]
      raw_name <- available_vars[i]
      
      # Add the variable, converting to numeric with error handling
      result <- result %>%
        mutate(!!var_name := suppressWarnings(as.numeric(processed_data[[raw_name]])))
      
      # Add data quality and source flags
      result <- result %>%
        mutate(
          !!paste0(var_name, "_data_quality") := data_quality_flags$direct,
          !!paste0(var_name, "_data_source") := "USDA Food Environment Atlas",
          !!paste0(var_name, "_data_vintage") := as.character(file_year)
        )
    }
    
    print_msg(paste("Processed", nrow(result), "county records with", 
                    ncol(result), "columns from Food Atlas data"))
    
    return(result)
  }
  
  # Function to parse the Food Access Research Atlas CSV file
  # This contains more detailed information about food deserts
  parse_food_access <- function(file_path) {
    if (!file.exists(file_path)) {
      return(NULL)
    }
    
    print_msg("Reading USDA Food Access Research Atlas data...")
    
    # Read the CSV file with error handling
    raw_data <- tryCatch({
      readr::read_csv(file_path, show_col_types = FALSE)
    }, error = function(e) {
      print_msg(paste("Error reading CSV file:", conditionMessage(e)))
      return(NULL)
    })
    
    # Check if data reading was successful
    if (is.null(raw_data) || nrow(raw_data) == 0) {
      print_msg("Failed to read data from CSV file")
      return(NULL)
    }
    
    # Check column names
    print_msg(paste("Found columns:", paste(head(names(raw_data), 10), collapse=", "), "..."))
    
    # Identify key columns for county identification
    # Food Access Research Atlas is typically at census tract level, so need to aggregate to county
    fips_col <- grep("FIPS|fips|^GEOID|^geoid", names(raw_data), value = TRUE)[1]
    
    # If no FIPS column, check for other identifiers
    if (is.na(fips_col)) {
      # Try looking for county and state identifiers
      county_col <- grep("County|county|COUNTY", names(raw_data), value = TRUE)[1]
      state_col <- grep("State|state|STATE", names(raw_data), value = TRUE)[1]
      
      if (!is.na(county_col) && !is.na(state_col)) {
        print_msg("Using county and state columns for identification")
      } else {
        # Check for CensusTract column which often contains FIPS info
        tract_col <- grep("CensusTract|TRACT|tract", names(raw_data), value = TRUE)[1]
        if (!is.na(tract_col)) {
          print_msg("Extracting county FIPS from census tract FIPS")
          raw_data <- raw_data %>%
            mutate(GEOID = substr(!!sym(tract_col), 1, 5))
          fips_col <- "GEOID"
        } else {
          print_msg("Warning: No county identifier columns found")
          return(NULL)
        }
      }
    } else {
      # Ensure FIPS column is named consistently
      raw_data <- raw_data %>% rename(GEOID = all_of(fips_col))
    }
    
    # Try to determine the data year
    # First look for year in column names
    year_col <- grep("^year$|^Year$|^YEAR$|^data_year$|^vintage$", 
                   names(raw_data), value = TRUE)[1]
    
    # If no year column, try to infer from file
    if (is.na(year_col)) {
      # Try to extract year from filename
      filename_year <- as.numeric(regmatches(
        basename(file_path),
        regexpr("20[0-9]{2}", basename(file_path))
      ))
      
      data_year <- if (!is.na(filename_year)) {
        filename_year
      } else {
        # Fallback to file modification time
        as.numeric(format(file.mtime(file_path), "%Y"))
      }
    } else {
      # Use the first value in the year column
      data_year <- as.numeric(raw_data[[year_col]][1])
      # If that fails, use file modified date
      if (is.na(data_year)) {
        data_year <- as.numeric(format(file.mtime(file_path), "%Y"))
      }
    }
    
    # Identify variables of interest from the Food Access Research Atlas
    # These variables include detailed food desert metrics at different distances and urban/rural splits
    food_desert_variables <- c(
      # Urban low-income & low-access (food desert) variables at different distances
      "urban_food_desert_pct_half_mile" = "lapophalf",
      "urban_food_desert_pct_1_mile" = "lapop1",
      "urban_food_desert_pct_10_miles" = "lapop10",
      "urban_food_desert_pct_20_miles" = "lapop20",
      
      # Rural low-income & low-access variables
      "rural_food_desert_pct_10_miles" = "lapop10",
      "rural_food_desert_pct_20_miles" = "lapop20",
      
      # Low income population in food deserts
      "low_income_food_desert_pct_half_mile" = "lalowihalf",
      "low_income_food_desert_pct_1_mile" = "lalowi1",
      "low_income_food_desert_pct_10_miles" = "lalowi10",
      "low_income_food_desert_pct_20_miles" = "lalowi20",
      
      # Children in food deserts
      "children_food_desert_pct_half_mile" = "lakidshalf",
      "children_food_desert_pct_1_mile" = "lakids1",
      "children_food_desert_pct_10_miles" = "lakids10",
      "children_food_desert_pct_20_miles" = "lakids20",
      
      # Seniors in food deserts
      "seniors_food_desert_pct_half_mile" = "laseniorshalf",
      "seniors_food_desert_pct_1_mile" = "laseniors1",
      "seniors_food_desert_pct_10_miles" = "laseniors10",
      "seniors_food_desert_pct_20_miles" = "laseniors20",
      
      # Housing units with no vehicle & low access
      "no_vehicle_food_desert_pct_half_mile" = "lahunvhalf",
      "no_vehicle_food_desert_pct_1_mile" = "lahunv1",
      "no_vehicle_food_desert_pct_10_miles" = "lahunv10",
      "no_vehicle_food_desert_pct_20_miles" = "lahunv20",
      
      # SNAP recipients in food deserts
      "snap_food_desert_pct_half_mile" = "lasnaphalf",
      "snap_food_desert_pct_1_mile" = "lasnap1",
      "snap_food_desert_pct_10_miles" = "lasnap10",
      "snap_food_desert_pct_20_miles" = "lasnap20"
    )
    
    # Determine if data is at tract or county level
    is_tract_level <- nrow(raw_data) > 3500 # Heuristic - US has ~3100 counties, ~74,000 tracts
    
    if (is_tract_level) {
      print_msg("Data appears to be at census tract level, aggregating to county level")
      
      # Ensure GEOID is just the county portion for aggregation
      if (nchar(raw_data$GEOID[1]) > 5) {
        raw_data <- raw_data %>%
          mutate(GEOID = substr(GEOID, 1, 5))
      }
      
      # Find which variables exist in the data
      available_vars <- intersect(unname(food_desert_variables), names(raw_data))
      
      # Aggregate to county level
      county_data <- raw_data %>%
        group_by(GEOID) %>%
        summarize(across(all_of(available_vars), 
                        ~ mean(.x, na.rm = TRUE)), 
                .groups = "drop")
    } else {
      print_msg("Data appears to already be at county level")
      
      # Standardize GEOID format (5 digits with leading zeros)
      county_data <- raw_data %>%
        mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
      
      # Find which variables exist
      available_vars <- intersect(unname(food_desert_variables), names(county_data))
    }
    
    # Map variables to standardized names
    available_std_vars <- names(food_desert_variables)[food_desert_variables %in% available_vars]
    
    print_msg(paste("Found", length(available_vars), "of", length(food_desert_variables), 
                  "food desert variables"))
    
    # Create result dataset
    result <- county_data %>%
      select(GEOID) %>%
      mutate(year = data_year) %>%
      as_tibble()
    
    # Add each found variable
    for (i in seq_along(available_vars)) {
      var_name <- available_std_vars[i]
      raw_name <- available_vars[i]
      
      # Add the variable, converting to numeric with error handling
      result <- result %>%
        mutate(!!var_name := suppressWarnings(as.numeric(county_data[[raw_name]])))
      
      # Add data quality and source flags
      result <- result %>%
        mutate(
          !!paste0(var_name, "_data_quality") := data_quality_flags$direct,
          !!paste0(var_name, "_data_source") := "USDA Food Access Research Atlas",
          !!paste0(var_name, "_data_vintage") := as.character(data_year)
        )
    }
    
    print_msg(paste("Processed", nrow(result), "county records with", 
                  ncol(result), "columns from Food Access Atlas data"))
    
    return(result)
  }
  
  # Process both data sources and combine results
  food_atlas_data <- NULL
  food_access_data <- NULL
  
  # Process Food Environment Atlas if available
  if (food_env_available) {
    food_atlas_data <- parse_food_atlas(food_env_local_file)
  } else {
    print_msg("Food Environment Atlas data not available")
  }
  
  # Process Food Access Research Atlas if available
  if (food_access_available) {
    food_access_data <- parse_food_access(food_access_local_file)
  } else {
    print_msg("Food Access Research Atlas data not available")
  }
  
  # Combine data from both sources if available
  combined_data <- NULL
  
  if (!is.null(food_atlas_data) && !is.null(food_access_data)) {
    print_msg("Combining data from both food environment and food access atlases")
    
    # Identify common columns for joining
    join_cols <- intersect(names(food_atlas_data), names(food_access_data))
    
    # Ensure we have at least GEOID and year for joining
    if (all(c("GEOID", "year") %in% join_cols)) {
      # Join the data
      combined_data <- full_join(food_atlas_data, food_access_data, by = join_cols)
      
      # Check for any duplicate columns that weren't in join_cols
      # This shouldn't happen with our standardized variable names, but checking just in case
      if (ncol(combined_data) < ncol(food_atlas_data) + ncol(food_access_data) - length(join_cols)) {
        print_msg("Warning: Some columns may have been dropped during data joining")
      }
    } else {
      print_msg("Cannot join datasets - missing common keys")
      # Use food_atlas_data as primary and append food_access_data where possible
      combined_data <- food_atlas_data
      
      # Try to match records from food_access_data
      if ("GEOID" %in% names(food_access_data) && "GEOID" %in% names(combined_data)) {
        print_msg("Appending food access data by GEOID matching")
        
        # For each county in combined_data, find matching record in food_access_data
        for (county_id in unique(combined_data$GEOID)) {
          # Find matching records
          access_records <- food_access_data %>% filter(GEOID == county_id)
          
          if (nrow(access_records) > 0) {
            # For each variable in access_records, add to combined_data if not already present
            access_vars <- setdiff(
              names(access_records),
              c("GEOID", "year", grep("_data_quality$|_data_source$|_data_vintage$", 
                                     names(access_records), value = TRUE))
            )
            
            for (var in access_vars) {
              if (!var %in% names(combined_data)) {
                # Get the value for this county
                val <- access_records[[var]][1]
                quality <- access_records[[paste0(var, "_data_quality")]][1]
                source <- access_records[[paste0(var, "_data_source")]][1]
                vintage <- access_records[[paste0(var, "_data_vintage")]][1]
                
                # Add to combined_data
                combined_data[[var]] <- ifelse(combined_data$GEOID == county_id, val, NA)
                combined_data[[paste0(var, "_data_quality")]] <- 
                  ifelse(combined_data$GEOID == county_id, quality, NA)
                combined_data[[paste0(var, "_data_source")]] <- 
                  ifelse(combined_data$GEOID == county_id, source, NA)
                combined_data[[paste0(var, "_data_vintage")]] <- 
                  ifelse(combined_data$GEOID == county_id, vintage, NA)
              }
            }
          }
        }
      }
    }
  } else if (!is.null(food_atlas_data)) {
    combined_data <- food_atlas_data
  } else if (!is.null(food_access_data)) {
    combined_data <- food_access_data
  } else {
    print_msg("No food environment data available from either source")
  }
  
  # Check if we have any data to work with
  if (!is.null(combined_data) && nrow(combined_data) > 0) {
    # Get available years in the data
    available_years <- unique(combined_data$year)
    print_msg(paste("Available years in data:", paste(available_years, collapse=", ")))
    
    # Check if we need to interpolate for missing years
    missing_years <- setdiff(years, available_years)
    
    if (length(missing_years) > 0 && allow_interpolation) {
      print_msg(paste("Need to interpolate/extrapolate for years:", paste(missing_years, collapse=", ")))
      
      # Enhanced interpolation/extrapolation
      # This will use actual interpolation between years rather than just copying
      
      # Identify all numeric variables to interpolate
      # Exclude identification columns and quality flags
      numeric_cols <- sapply(names(combined_data), function(col) {
        !col %in% c("GEOID", "NAME", "year") && 
          !grepl("_data_quality$|_data_source$|_data_vintage$", col) &&
          is.numeric(combined_data[[col]])
      })
      numeric_cols <- names(combined_data)[numeric_cols]
      
      # Create interpolated data for each missing year
      interp_data_list <- list()
      
      for (year in missing_years) {
        # Check if this is an interpolation or extrapolation
        is_interpolation <- year > min(available_years) && year < max(available_years)
        
        if (is_interpolation) {
          print_msg(paste("Interpolating data for", year))
          
          # Interpolate for each county
          county_list <- unique(combined_data$GEOID)
          
          interp_county_list <- list()
          
          for (county in county_list) {
            # Get data for this county
            county_data <- combined_data %>% filter(GEOID == county)
            
            # If we have at least 2 years for this county, interpolate
            if (nrow(county_data) >= 2) {
              # Create a new row for this county and year
              new_row <- county_data[1, c("GEOID", "NAME")] %>%
                mutate(year = year)
              
              # For each numeric column, interpolate
              for (col in numeric_cols) {
                # Extract existing data points
                x <- county_data$year
                y <- county_data[[col]]
                
                # Remove NA values
                valid <- !is.na(y)
                x_valid <- x[valid]
                y_valid <- y[valid]
                
                # If we have at least 2 valid points, interpolate
                if (length(x_valid) >= 2) {
                  # Use approx for linear interpolation
                  interp <- approx(x_valid, y_valid, xout = year)
                  new_row[[col]] <- interp$y
                  
                  # Add quality flags
                  new_row[[paste0(col, "_data_quality")]] <- data_quality_flags$interpolated
                  new_row[[paste0(col, "_data_source")]] <- 
                    county_data[[paste0(col, "_data_source")]][1]
                  new_row[[paste0(col, "_data_vintage")]] <- 
                    paste0("interpolated_", min(x_valid), "_", max(x_valid))
                } else if (length(x_valid) == 1) {
                  # Just one data point, use it directly
                  new_row[[col]] <- y_valid[1]
                  
                  # Add quality flags
                  new_row[[paste0(col, "_data_quality")]] <- data_quality_flags$extrapolated
                  new_row[[paste0(col, "_data_source")]] <- 
                    county_data[[paste0(col, "_data_source")]][1]
                  new_row[[paste0(col, "_data_vintage")]] <- 
                    paste0("extrapolated_from_", x_valid[1])
                }
              }
              
              interp_county_list[[county]] <- new_row
            }
          }
          
          # Combine all counties for this year
          if (length(interp_county_list) > 0) {
            interp_data_list[[as.character(year)]] <- bind_rows(interp_county_list)
          }
        } else {
          # Extrapolation - find closest year and adjust
          print_msg(paste("Extrapolating data for", year))
          
          closest_year <- available_years[which.min(abs(available_years - year))]
          
          # Get data for closest year
          year_data <- combined_data %>%
            filter(year == closest_year) %>%
            mutate(year = year)
          
          # Update quality flags for this data
          for (col in numeric_cols) {
            quality_col <- paste0(col, "_data_quality")
            vintage_col <- paste0(col, "_data_vintage")
            
            if (quality_col %in% names(year_data)) {
              year_data[[quality_col]] <- data_quality_flags$extrapolated
              year_data[[vintage_col]] <- paste0("extrapolated_from_", closest_year)
            }
          }
          
          interp_data_list[[as.character(year)]] <- year_data
        }
      }
      
      # Combine interpolated data with original data
      if (length(interp_data_list) > 0) {
        interp_data <- bind_rows(interp_data_list)
        combined_data <- bind_rows(combined_data, interp_data)
      }
    }
    
    # Filter to just the requested years
    combined_data <- combined_data %>%
      filter(year %in% years)
    
    # Sort by GEOID and year
    combined_data <- combined_data %>%
      arrange(GEOID, year)
    
    # Cache the result
    saveRDS(combined_data, cache_file)
    print_msg(paste("Cached combined food environment data to:", cache_file))
    
    return(combined_data)
  } else if (allow_simulation) {
    # If no real data and simulation allowed, create placeholder data
    print_msg("No USDA food environment data found. Creating simulated data...")
    
    # Get county list from built-in data or create basic list
    counties <- data.frame(
      GEOID = c("01001", "01003", "01005", "01007", "01009"), # Sample counties
      NAME = c("Autauga County, Alabama", "Baldwin County, Alabama", 
               "Barbour County, Alabama", "Bibb County, Alabama", 
               "Blount County, Alabama")
    )
    
    # Try to get a more comprehensive list if possible
    tryCatch({
      # Check for tidycensus
      if (requireNamespace("tidycensus", quietly = TRUE)) {
        library(tidycensus)
        
        # Try to get counties from Census API
        if (Sys.getenv("CENSUS_API_KEY") != "") {
          counties <- tidycensus::get_decennial(
            geography = "county",
            variables = "P001001", # Total population
            year = 2020,
            geometry = FALSE
          ) %>%
            select(GEOID, NAME) %>%
            distinct()
          
          print_msg(paste("Using", nrow(counties), "counties from Census API"))
        }
      }
    }, error = function(e) {
      print_msg("Using sample county list for simulation")
    })
    
    # Comprehensive list of food environment variables to simulate
    sim_variables <- c(
      # From Food Environment Atlas
      "grocery_stores_per_1000",
      "supercenters_per_1000",
      "convenience_stores_per_1000",
      "specialized_food_stores_per_1000",
      "snap_authorized_stores_per_1000",
      "wic_authorized_stores_per_1000",
      "farmers_markets_per_1000",
      "fast_food_restaurants_per_1000",
      "full_service_restaurants_per_1000",
      "low_income_low_access_pct",
      "low_income_low_access_child_pct",
      "low_income_low_access_seniors_pct",
      "low_access_vehicle_pct",
      "low_income_pct",
      "snap_participation_rate",
      "snap_benefits_redemption_per_capita",
      "food_insecurity_rate",
      "child_food_insecurity_rate",
      "price_index_fruits_vegetables",
      "price_index_meat",
      "price_index_soda",
      "price_index_milk",
      "school_lunch_pct",
      "summer_food_program_pct",
      "adult_obesity_pct",
      "adult_diabetes_pct",
      
      # From Food Access Research Atlas
      "urban_food_desert_pct_1_mile",
      "rural_food_desert_pct_10_miles",
      "low_income_food_desert_pct_1_mile",
      "children_food_desert_pct_1_mile",
      "seniors_food_desert_pct_1_mile",
      "no_vehicle_food_desert_pct_1_mile",
      "snap_food_desert_pct_1_mile"
    )
    
    # Create simulated data for each year
    sim_data_list <- list()
    for (year in years) {
      # Create base data frame with counties and year
      year_data <- counties %>%
        mutate(year = year)
      
      # Add simulated values for each variable
      for (var_name in sim_variables) {
        # Simulate values based on variable type with realistic ranges
        if (grepl("per_1000$", var_name)) {
          # Rates per 1000 - typically small positive numbers
          if (grepl("grocery|supercenters", var_name)) {
            # Grocery stores are less common
            year_data[[var_name]] <- runif(nrow(year_data), 0.05, 0.7)
          } else if (grepl("convenience", var_name)) {
            # Convenience stores are more common
            year_data[[var_name]] <- runif(nrow(year_data), 0.3, 1.5)
          } else if (grepl("farmers_markets", var_name)) {
            # Farmers markets are less common
            year_data[[var_name]] <- runif(nrow(year_data), 0.01, 0.2)
          } else if (grepl("fast_food", var_name)) {
            # Fast food restaurants are common
            year_data[[var_name]] <- runif(nrow(year_data), 0.5, 2.0)
          } else {
            # Other per 1000 variables
            year_data[[var_name]] <- runif(nrow(year_data), 0.1, 1.0)
          }
        } else if (grepl("per_capita$", var_name)) {
          # Per capita values - typically very small
          year_data[[var_name]] <- runif(nrow(year_data), 0.001, 0.1)
        } else if (grepl("_pct$|_rate$", var_name)) {
          # Percentages/rates - between 0 and 100
          if (grepl("obesity|diabetes", var_name)) {
            # Health conditions typically 10-40%
            year_data[[var_name]] <- runif(nrow(year_data), 10, 40)
          } else if (grepl("food_desert", var_name)) {
            # Food desert percentages typically 5-25%
            year_data[[var_name]] <- runif(nrow(year_data), 5, 25)
          } else if (grepl("food_insecurity", var_name)) {
            # Food insecurity typically 8-20%
            year_data[[var_name]] <- runif(nrow(year_data), 8, 20)
            
            # Child food insecurity typically higher
            if (grepl("child", var_name)) {
              year_data[[var_name]] <- year_data[[var_name]] * runif(nrow(year_data), 1.1, 1.5)
            }
          } else {
            # Other percentages
            year_data[[var_name]] <- runif(nrow(year_data), 0, 50)
          }
        } else if (grepl("price_index", var_name)) {
          # Price indices typically 80-120
          year_data[[var_name]] <- runif(nrow(year_data), 80, 120)
        } else {
          # Default - medium positive numbers
          year_data[[var_name]] <- runif(nrow(year_data), 0, 100)
        }
        
        # Add quality flags
        year_data[[paste0(var_name, "_data_quality")]] <- data_quality_flags$simulated
        year_data[[paste0(var_name, "_data_source")]] <- "SIMULATED Food Environment Data"
        year_data[[paste0(var_name, "_data_vintage")]] <- paste0("simulated_", year)
      }
      
      sim_data_list[[as.character(year)]] <- year_data
    }
    
    # Combine all years
    simulated_data <- bind_rows(sim_data_list)
    
    # Add trends over time for realistic simulation
    # Food insecurity decreasing slightly over time
    years_factor <- as.integer(factor(simulated_data$year, levels = sort(unique(simulated_data$year))))
    simulated_data$food_insecurity_rate <- simulated_data$food_insecurity_rate * (1 - 0.01 * (years_factor - 1))
    
    # Grocery stores slightly decreasing, convenience stores increasing
    simulated_data$grocery_stores_per_1000 <- simulated_data$grocery_stores_per_1000 * (1 - 0.02 * (years_factor - 1))
    simulated_data$convenience_stores_per_1000 <- simulated_data$convenience_stores_per_1000 * (1 + 0.02 * (years_factor - 1))
    
    # Fast food increasing
    simulated_data$fast_food_restaurants_per_1000 <- simulated_data$fast_food_restaurants_per_1000 * (1 + 0.03 * (years_factor - 1))
    
    # Farmers markets increasing (more rapidly in recent years)
    simulated_data$farmers_markets_per_1000 <- simulated_data$farmers_markets_per_1000 * (1 + 0.05 * (years_factor - 1))
    
    # Cache the simulated data
    saveRDS(simulated_data, cache_file)
    print_msg(paste("Cached simulated food environment data to:", cache_file))
    
    return(simulated_data)
  } else {
    # No data and simulation not allowed - create empty dataset with NAs
    print_msg("No USDA food environment data available. Creating empty dataset with NAs since simulation not allowed...")
    
    # Define the variables we would have included
    food_env_vars <- c(
      # From Food Environment Atlas
      "grocery_stores_per_1000",
      "supercenters_per_1000",
      "convenience_stores_per_1000",
      "specialized_food_stores_per_1000",
      "snap_authorized_stores_per_1000",
      "wic_authorized_stores_per_1000",
      "farmers_markets_per_1000",
      "fast_food_restaurants_per_1000",
      "full_service_restaurants_per_1000",
      "low_income_low_access_pct",
      "low_income_low_access_child_pct",
      "low_income_low_access_seniors_pct",
      "low_access_vehicle_pct",
      "low_income_pct",
      "snap_participation_rate",
      "snap_benefits_redemption_per_capita",
      "food_insecurity_rate",
      "child_food_insecurity_rate",
      "price_index_fruits_vegetables",
      "price_index_meat",
      "price_index_soda",
      "price_index_milk",
      "school_lunch_pct",
      "summer_food_program_pct",
      "adult_obesity_pct",
      "adult_diabetes_pct",
      
      # From Food Access Research Atlas
      "urban_food_desert_pct_1_mile",
      "rural_food_desert_pct_10_miles",
      "low_income_food_desert_pct_1_mile",
      "children_food_desert_pct_1_mile",
      "seniors_food_desert_pct_1_mile",
      "no_vehicle_food_desert_pct_1_mile",
      "snap_food_desert_pct_1_mile"
    )
    
    # Get county list from built-in data or create basic list
    counties <- data.frame(
      GEOID = c("01001", "01003", "01005", "01007", "01009"), # Sample counties
      NAME = c("Autauga County, Alabama", "Baldwin County, Alabama", 
               "Barbour County, Alabama", "Bibb County, Alabama", 
               "Blount County, Alabama")
    )
    
    # Try to get a more comprehensive list if possible
    tryCatch({
      # Check for tidycensus
      if (requireNamespace("tidycensus", quietly = TRUE)) {
        library(tidycensus)
        
        # Try to get counties from Census API
        if (Sys.getenv("CENSUS_API_KEY") != "") {
          counties <- tidycensus::get_decennial(
            geography = "county",
            variables = "P001001", # Total population
            year = 2020,
            geometry = FALSE
          ) %>%
            select(GEOID, NAME) %>%
            distinct()
          
          print_msg(paste("Using", nrow(counties), "counties from Census API"))
        }
      }
    }, error = function(e) {
      print_msg("Using sample county list for empty dataset")
    })
    
    # Create empty data for each year
    empty_data_list <- list()
    for (year in years) {
      # Create base data frame with counties and year
      year_data <- counties %>%
        mutate(year = year)
      
      # Add NA values for each variable
      for (var_name in food_env_vars) {
        year_data[[var_name]] <- NA_real_
        
        # Add quality flags
        year_data[[paste0(var_name, "_data_quality")]] <- data_quality_flags$missing
        year_data[[paste0(var_name, "_data_source")]] <- "NOT_AVAILABLE"
        year_data[[paste0(var_name, "_data_vintage")]] <- NA_character_
      }
      
      empty_data_list[[as.character(year)]] <- year_data
    }
    
    # Combine all years
    empty_data <- bind_rows(empty_data_list)
    
    # Cache the empty data
    saveRDS(empty_data, cache_file)
    print_msg(paste("Cached empty food environment data to:", cache_file))
    
    return(empty_data)
  }
}

# This lets the function be used when the script is sourced
is_sourced <- function() {
  return(!identical(environment(), globalenv()))
}

# If the script is run directly, test the function
if (!is_sourced()) {
  cat("Testing USDA Food Environment Atlas data fetcher...\n")
  
  # Get current year
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  
  # Test for last 5 years
  test_years <- (current_year-4):current_year
  
  # Define standardized data quality flags
  data_quality_flags <- list(
    direct = "direct",
    interpolated = "interpolated",
    extrapolated = "extrapolated",
    simulated = "simulated",
    missing = NA,
    imputed = "imputed"
  )
  
  # Test the function with various settings
  cat("\n----- TEST 1: With simulation allowed -----\n")
  result_sim <- fetch_usda_food_atlas(
    years = test_years,
    cache_dir = "data/cache",
    refresh_cache = FALSE,
    allow_simulation = TRUE,
    allow_interpolation = TRUE,
    data_quality_flags = data_quality_flags,
    offline_mode = FALSE
  )
  
  cat("Test 1 completed with", nrow(result_sim), "rows of data.\n")
  
  # Report data quality metrics
  if (!is.null(result_sim)) {
    cat("\nData quality metrics:\n")
    
    # Find all data quality columns
    quality_cols <- grep("_data_quality$", names(result_sim), value = TRUE)
    
    for (qcol in quality_cols[1:min(5, length(quality_cols))]) { # Limit to 5 variables to avoid excessive output
      # Get variable name without suffix
      var_name <- gsub("_data_quality$", "", qcol)
      
      # Count occurrences of each quality flag
      quality_counts <- table(result_sim[[qcol]], useNA = "ifany")
      
      cat(paste0("\n", var_name, ":\n"))
      for (flag in names(quality_counts)) {
        if (is.na(flag)) {
          cat("  Missing: ", quality_counts[[which(is.na(names(quality_counts)))]], "\n")
        } else {
          cat("  ", flag, ": ", quality_counts[[flag]], "\n")
        }
      }
    }
  }
  
  cat("\n----- TEST 2: No simulation, with interpolation -----\n")
  result_interp <- fetch_usda_food_atlas(
    years = test_years,
    cache_dir = "data/cache",
    refresh_cache = FALSE,
    allow_simulation = FALSE,
    allow_interpolation = TRUE,
    data_quality_flags = data_quality_flags,
    offline_mode = FALSE
  )
  
  cat("Test 2 completed with", nrow(result_interp), "rows of data.\n")
  
  cat("\n----- TEST 3: No simulation, no interpolation -----\n")
  result_none <- tryCatch({
    fetch_usda_food_atlas(
      years = test_years,
      cache_dir = "data/cache",
      refresh_cache = FALSE,
      allow_simulation = FALSE,
      allow_interpolation = FALSE,
      data_quality_flags = data_quality_flags,
      offline_mode = FALSE
    )
  }, error = function(e) {
    cat("Error as expected with no simulation and no interpolation:", conditionMessage(e), "\n")
    return(NULL)
  })
  
  if (!is.null(result_none)) {
    cat("Test 3 completed with", nrow(result_none), "rows of data.\n")
  }
  
  cat("\n----- TEST 4: Offline mode -----\n")
  result_offline <- fetch_usda_food_atlas(
    years = test_years,
    cache_dir = "data/cache",
    refresh_cache = FALSE,
    allow_simulation = TRUE,
    allow_interpolation = TRUE,
    data_quality_flags = data_quality_flags,
    offline_mode = TRUE
  )
  
  cat("Test 4 completed with", nrow(result_offline), "rows of data.\n")
}
