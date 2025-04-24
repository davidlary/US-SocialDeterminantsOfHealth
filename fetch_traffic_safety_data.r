#!/usr/bin/env Rscript

# Traffic Safety Data Fetcher for SDOH Pipeline
# This script retrieves traffic safety and accident data from multiple sources:
# 1. NHTSA's Fatality Analysis Reporting System (FARS)
# 2. CDC WONDER - Multiple Cause of Death database
# 
# For each data source, it performs:
# - Retrieval of raw data
# - Cleaning and standardization 
# - Geographic mapping to county FIPS codes
# - Interpolation for missing years (when allowed)

# Required packages
required_packages <- c(
  "tidyverse",
  "httr",
  "jsonlite",
  "readxl",
  "sf",
  "zoo",
  "tigris",
  "lubridate",
  "glue"
)

# Load required packages
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    message(paste("Required package", pkg, "is not installed."))
    message("Please run 'Rscript R/install_packages.r' first.")
    stop(paste("Missing required package:", pkg))
  }
}

# Also check for tidycensus which is helpful for fetching population data
has_tidycensus <- require("tidycensus", quietly = TRUE)

#' Fetch traffic safety data for specified years
#'
#' @param years Vector of years to fetch data for
#' @param cache_dir Directory to store cached data
#' @param refresh_cache Whether to refresh cached data
#' @param allow_interpolation Whether to allow interpolation for years with missing data
#' @param data_quality_flags List with flag values for different data quality types
#' @param offline_mode Whether to use only local data (no API/internet calls)
#'
#' @return A data frame with county-level traffic safety data
#' 
#' @export
fetch_traffic_safety_data <- function(years, 
                                  cache_dir = "data/cache", 
                                  refresh_cache = FALSE,
                                  allow_interpolation = TRUE,
                                  data_quality_flags = list(
                                    direct = "direct",
                                    interpolated = "interpolated",
                                    extrapolated = "extrapolated",
                                    missing = NA,
                                    imputed = "imputed"
                                  ),
                                  offline_mode = FALSE,
                                  parallel = TRUE,
                                  parallel_config = NULL,
                                  census_data = NULL) {
  
  # Create the cache directory if it doesn't exist
  traffic_cache_dir <- file.path(cache_dir, "traffic_safety")
  if (!dir.exists(traffic_cache_dir)) {
    dir.create(traffic_cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Setup parallel processing if enabled
  if (parallel) {
    # Use module_core.r's setup_parallel_processing if available
    if (exists("setup_parallel_processing")) {
      # Configure parallel processing with adaptive strategy
      if (is.null(parallel_config)) {
        parallel_config <- setup_parallel_processing(
          use_parallel = TRUE,
          num_cores = NULL,  # Auto-detect
          strategy = "auto", # Choose best strategy for platform
          memory_limit_gb = 8,
          chunk_size = 200
        )
      }
      message("Parallel processing enabled for traffic safety data")
    } else {
      # Basic parallel setup
      message("Using basic parallel processing setup for traffic safety data")
      if (!requireNamespace("future", quietly = TRUE)) {
        install.packages("future")
        library(future)
      }
      if (!requireNamespace("future.apply", quietly = TRUE)) {
        install.packages("future.apply")
        library(future.apply)
      }
      
      # Determine number of cores
      num_cores <- parallel::detectCores() - 1
      num_cores <- max(2, num_cores) # At least 2 cores
      
      # Choose strategy based on OS
      strategy <- if (.Platform$OS.type == "windows") {
        "multisession"
      } else {
        "multicore"
      }
      
      future::plan(strategy, workers = num_cores)
      options(future.globals.maxSize = 8 * 1024^3) # 8GB
      
      parallel_config <- list(
        enabled = TRUE,
        cores = num_cores,
        strategy = strategy,
        memory_limit_gb = 8,
        chunk_size = 200
      )
    }
  }
  
  # Default value for missing list elements
  `%||%` <- function(a, b) if (is.null(a)) b else a
  
  # Define the data quality flags from parameter or defaults
  direct_flag <- data_quality_flags$direct %||% "direct"
  interpolated_flag <- data_quality_flags$interpolated %||% "interpolated"
  extrapolated_flag <- data_quality_flags$extrapolated %||% "extrapolated" 
  missing_flag <- data_quality_flags$missing %||% NA
  imputed_flag <- data_quality_flags$imputed %||% "imputed"
  
  # Check if years are valid
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  years <- years[years <= current_year]
  if (length(years) == 0) {
    warning("No valid years specified. Using current year.")
    years <- current_year
  }
  
  # Sort years to ensure proper time series processing
  years <- sort(years)
  
  # Log the start of data fetching
  message(paste("Fetching traffic safety data for years", 
                min(years), "to", max(years)))
  
  # Create cache file paths for combined dataset
  cache_file <- file.path(traffic_cache_dir, 
                          paste0("traffic_safety_data_", 
                                 min(years), "_", max(years), ".rds"))
  
  # Check if combined cache file exists and is not being refreshed
  if (file.exists(cache_file) && !refresh_cache) {
    message("Loading traffic safety data from cache...")
    return(readRDS(cache_file))
  }
  
  # Initialize data frames for each source
  fars_data <- NULL
  cdc_data <- NULL
  
  # Fetch data from NHTSA's FARS if not in offline mode
  if (!offline_mode) {
    message("Fetching data from NHTSA FARS...")
    fars_data <- tryCatch({
      get_fars_data(years, traffic_cache_dir, refresh_cache)
    }, error = function(e) {
      warning(paste("Error fetching FARS data:", e$message))
      return(NULL)
    })
  } else {
    message("Offline mode: Attempting to load FARS data from cache...")
    fars_cache_file <- file.path(traffic_cache_dir, 
                                paste0("fars_data_", 
                                       min(years), "_", max(years), ".rds"))
    if (file.exists(fars_cache_file)) {
      fars_data <- readRDS(fars_cache_file)
    } else {
      warning("No cached FARS data available in offline mode.")
    }
  }
  
  # Fetch data from CDC WONDER if not in offline mode
  if (!offline_mode) {
    message("Fetching data from CDC WONDER...")
    cdc_data <- tryCatch({
      get_cdc_wonder_data(years, traffic_cache_dir, refresh_cache)
    }, error = function(e) {
      warning(paste("Error fetching CDC WONDER data:", e$message))
      return(NULL)
    })
  } else {
    message("Offline mode: Attempting to load CDC WONDER data from cache...")
    cdc_cache_file <- file.path(traffic_cache_dir, 
                               paste0("cdc_wonder_data_", 
                                      min(years), "_", max(years), ".rds"))
    if (file.exists(cdc_cache_file)) {
      cdc_data <- readRDS(cdc_cache_file)
    } else {
      warning("No cached CDC WONDER data available in offline mode.")
    }
  }
  
  # Get county template data frame
  county_template <- tryCatch({
    # Get counties from tigris for the most recent census
    counties <- tigris::counties(cb = TRUE, year = max(min(c(2020, current_year)), min(years)))
    
    # Extract required fields and keep GEOID as is (don't rename to fips)
    counties %>%
      sf::st_drop_geometry() %>%
      select(GEOID, NAME) %>%
      rename(county_name = NAME)  # Keep GEOID as GEOID
  }, error = function(e) {
    warning(paste("Error fetching county data from tigris:", e$message))
    # Return NULL if we couldn't get counties
    NULL
  })
  
  # If tigris failed, try to create a basic county list from FARS data
  if (is.null(county_template) && !is.null(fars_data)) {
    # Check if we need to rename fips to GEOID
    if ("fips" %in% names(fars_data) && !"GEOID" %in% names(fars_data)) {
      county_template <- fars_data %>%
        select(fips) %>%
        rename(GEOID = fips) %>%
        distinct() %>%
        mutate(county_name = NA_character_)
    } else {
      county_template <- fars_data %>%
        select(GEOID) %>%
        distinct() %>%
        mutate(county_name = NA_character_)
    }
  }
  
  # If we still don't have counties, use a minimal template
  if (is.null(county_template)) {
    # Create an empty template - will be populated as we process data
    county_template <- data.frame(
      GEOID = character(0),
      county_name = character(0)
    )
  }
  
  # Create a data frame with all counties and years
  all_counties_years <- tidyr::expand_grid(
    GEOID = unique(county_template$GEOID),
    year = years
  )
  
  # Add county names if available
  if (nrow(county_template) > 0) {
    all_counties_years <- all_counties_years %>%
      left_join(county_template, by = "GEOID")
  } else {
    all_counties_years$county_name <- NA_character_
  }
  
  # Add empty variables for traffic safety data
  all_counties_years <- all_counties_years %>%
    mutate(
      traffic_fatality_count = NA_real_,
      traffic_fatality_rate_per_100k = NA_real_,
      traffic_injury_count = NA_real_,
      traffic_injury_rate_per_100k = NA_real_,
      ped_bike_fatality_count = NA_real_,
      ped_bike_fatality_rate_per_100k = NA_real_,
      dui_fatality_count = NA_real_,
      dui_fatality_rate_per_100k = NA_real_,
      speeding_fatality_count = NA_real_,
      speeding_fatality_rate_per_100k = NA_real_
    )
  
  # Add data quality flag columns
  all_counties_years <- all_counties_years %>%
    mutate(
      traffic_fatality_count_data_quality = missing_flag,
      traffic_fatality_rate_per_100k_data_quality = missing_flag,
      traffic_injury_count_data_quality = missing_flag,
      traffic_injury_rate_per_100k_data_quality = missing_flag,
      ped_bike_fatality_count_data_quality = missing_flag,
      ped_bike_fatality_rate_per_100k_data_quality = missing_flag,
      dui_fatality_count_data_quality = missing_flag,
      dui_fatality_rate_per_100k_data_quality = missing_flag,
      speeding_fatality_count_data_quality = missing_flag,
      speeding_fatality_rate_per_100k_data_quality = missing_flag
    )
  
  # Start with the template
  combined_data <- all_counties_years
  
  # Fetch population data for rate calculations if not provided
  if (is.null(census_data)) {
    message("No population data provided. Attempting to fetch from Census...")
    
    # Try to load population data from cache
    pop_cache_file <- file.path(cache_dir, "population_data.rds")
    
    if (file.exists(pop_cache_file) && !refresh_cache) {
      message("Loading population data from cache...")
      population_data <- readRDS(pop_cache_file)
    } else if (!offline_mode && has_tidycensus) {
      # Fetch population data from Census API
      message("Fetching population data from Census API...")
      
      population_data <- tryCatch({
        # Check if we need a Census API key
        if (Sys.getenv("CENSUS_API_KEY") == "") {
          census_api_key <- read.table("census_api_key.txt", stringsAsFactors = FALSE)$V1
          Sys.setenv(CENSUS_API_KEY = census_api_key)
        }
        
        # Get population data for each year using parallel processing if enabled
        get_population_for_year <- function(year) {
          message(paste("Fetching population data for year:", year))
          yr_data <- NULL
          
          if (year >= 2010) {
            # For 2010 and later, use decennial census for 2010 and ACS for other years
            if (year == 2010) {
              yr_data <- tidycensus::get_decennial(
                geography = "county",
                variables = "P001001",  # Total population
                year = 2010,
                cache = TRUE
              )
            } else {
              # Use ACS 5-year estimates for non-decennial years
              acs_year <- min(year, current_year - 1)  # ACS data is typically a year behind
              yr_data <- tidycensus::get_acs(
                geography = "county",
                variables = "B01003_001",  # Total population
                year = acs_year,
                cache = TRUE
              )
            }
          } else if (year >= 2000) {
            # For 2000-2009, use 2000 decennial census and interpolate
            if (year == 2000) {
              yr_data <- tidycensus::get_decennial(
                geography = "county",
                variables = "P001001",  # Total population
                year = 2000,
                cache = TRUE
              )
            } else {
              # Interpolate between 2000 and 2010 censuses
              yr_2000 <- tidycensus::get_decennial(
                geography = "county",
                variables = "P001001",
                year = 2000,
                cache = TRUE
              ) %>% 
                rename(pop_2000 = value)
              
              yr_2010 <- tidycensus::get_decennial(
                geography = "county",
                variables = "P001001",
                year = 2010,
                cache = TRUE
              ) %>% 
                rename(pop_2010 = value)
              
              # Join 2000 and 2010 data using global_safe_merge if available
              if (exists("global_safe_merge")) {
                yr_data <- global_safe_merge(yr_2000, yr_2010, by_cols = "GEOID")
              } else {
                yr_data <- yr_2000 %>%
                  left_join(yr_2010, by = "GEOID", suffix = c("_2000", "_2010"))
              }
              
              # Linear interpolation between 2000 and 2010
              factor <- (year - 2000) / 10
              yr_data$value <- yr_data$pop_2000 + (yr_data$pop_2010 - yr_data$pop_2000) * factor
              yr_data$variable <- "B01003_001"
              yr_data$NAME <- yr_data$NAME_2000
              yr_data <- yr_data %>% select(GEOID, NAME, variable, value)
            }
          } else if (year >= 1990) {
            # For 1990-1999, use NHGIS data or extrapolate from 2000
            # Placeholder for demo - would use actual NHGIS data in production
            yr_2000 <- tidycensus::get_decennial(
              geography = "county",
              variables = "P001001",
              year = 2000,
              cache = TRUE
            )
            
            # Approximate extrapolation - in reality would use NHGIS data
            yr_data <- yr_2000
            factor <- 1 - (2000 - year) * 0.01  # Simple decay factor
            yr_data$value <- yr_data$value * factor
          } else if (year >= 1970) {
            # For 1970-1989, would use NHGIS data
            # Placeholder - would use actual NHGIS data in production
            yr_1990 <- tidycensus::get_decennial(
              geography = "county",
              variables = "P001001",
              year = 1990,
              cache = TRUE
            )
            
            # Approximate extrapolation - in reality would use NHGIS data
            yr_data <- yr_1990
            factor <- 1 - (1990 - year) * 0.01  # Simple decay factor
            yr_data$value <- yr_data$value * factor
          }
          
          if (!is.null(yr_data)) {
            yr_data$year <- year
            yr_data$data_quality <- ifelse(year %in% c(1970, 1980, 1990, 2000, 2010, 2020), 
                                          "direct", 
                                          "interpolated")
          }
          
          return(yr_data)
        }
        
        # Use parallel processing if enabled
        all_pop_data <- if (parallel && requireNamespace("future.apply", quietly = TRUE)) {
          message("Using parallel processing for population data fetching")
          
          # Setup progress tracking if available
          if (requireNamespace("progressr", quietly = TRUE)) {
            progressr::handlers(progressr::handler_progress())
            progressr::with_progress({
              p <- progressr::progressor(steps = length(years))
              
              future.apply::future_lapply(years, function(year) {
                result <- get_population_for_year(year)
                p(message = paste("Processed population data for year", year))
                return(result)
              })
            })
          } else {
            # No progress tracking
            future.apply::future_lapply(years, get_population_for_year)
          }
        } else {
          # Fallback to sequential processing
          message("Using sequential processing for population data fetching")
          lapply(years, get_population_for_year)
        }
        
        # Combine all years
        pop_data <- bind_rows(all_pop_data) %>%
          # Format for the pipeline
          select(GEOID, year, population = value, data_quality) %>%
          mutate(
            fips = GEOID,
            population = as.numeric(population)
          )
        
        # Save to cache
        saveRDS(pop_data, pop_cache_file)
        
        pop_data
      }, error = function(e) {
        warning(paste("Error fetching Census population data:", e$message))
        return(NULL)
      })
    } else {
      message("Cannot fetch population data - offline mode or missing tidycensus package")
      population_data <- NULL
    }
  } else {
    # Use the provided Census data
    population_data <- census_data
    
    # Ensure it has the columns we need
    if (is.data.frame(population_data) && all(c("fips", "year", "population") %in% names(population_data))) {
      message("Using provided population data")
    } else {
      message("Provided census_data does not have required columns (fips, year, population)")
      population_data <- NULL
    }
  }
  
  # Add FARS data if available
  if (!is.null(fars_data) && nrow(fars_data) > 0) {
    message("Integrating FARS data...")
    
    # Make sure column names match
    fars_data <- fars_data %>%
      rename_with(~tolower(gsub(" ", "_", .x)))
    
    # Ensure fips and year columns exist
    if (all(c("fips", "year") %in% names(fars_data))) {
      # Rename fips to GEOID for consistency
      fars_data <- fars_data %>%
        rename(GEOID = fips)
      
      # Prepare FARS variables for merging
      fars_for_merge <- fars_data %>%
        select(GEOID, year, 
               matches("traffic_fatality|ped_bike|dui|speeding")) %>%
        # Fill _data_quality columns if they don't exist
        mutate(across(matches("traffic_fatality|ped_bike|dui|speeding"), 
                     ~., 
                     .names = "{.col}_data_quality")) %>%
        mutate(across(ends_with("_data_quality"), 
                     ~ifelse(is.na(.x), direct_flag, .x)))
      
      # Merge with combined_data
      combined_data <- combined_data %>%
        left_join(fars_for_merge, by = c("GEOID", "year"), suffix = c("", "_fars"))
      
      # For each variable from FARS, update the corresponding variable in combined_data
      # giving preference to FARS data when available
      fars_vars <- grep("traffic_fatality|ped_bike|dui|speeding",
                       names(fars_for_merge), value = TRUE)
      fars_vars <- fars_vars[!grepl("_data_quality$", fars_vars)]
      
      for (var in fars_vars) {
        # Get the corresponding FARS column
        fars_col <- paste0(var, "_fars")
        qual_col <- paste0(var, "_data_quality")
        
        # Update the value if FARS data is available
        if (fars_col %in% names(combined_data)) {
          combined_data[[var]] <- ifelse(!is.na(combined_data[[fars_col]]), 
                                        combined_data[[fars_col]], 
                                        combined_data[[var]])
          
          # Update the quality flag
          combined_data[[qual_col]] <- ifelse(!is.na(combined_data[[fars_col]]),
                                             direct_flag,
                                             combined_data[[qual_col]])
          
          # Clean up the temporary column
          combined_data[[fars_col]] <- NULL
        }
      }
      
      # Clean up any remaining temporary columns
      temp_cols <- grep("_fars$", names(combined_data), value = TRUE)
      if (length(temp_cols) > 0) {
        combined_data <- combined_data %>%
          select(-all_of(temp_cols))
      }
    } else {
      warning("FARS data missing required 'fips' or 'year' columns. Skipping.")
    }
  }
  
  # Add CDC WONDER data if available
  if (!is.null(cdc_data) && nrow(cdc_data) > 0) {
    message("Integrating CDC WONDER data...")
    
    # Make sure column names match
    cdc_data <- cdc_data %>%
      rename_with(~tolower(gsub(" ", "_", .x)))
    
    # Ensure fips and year columns exist
    if (all(c("fips", "year") %in% names(cdc_data))) {
      # Rename fips to GEOID for consistency
      cdc_data <- cdc_data %>%
        rename(GEOID = fips)
      
      # Only use CDC data for variables not already populated from FARS
      # CDC generally provides mortality data, but may not have specific 
      # breakdowns like FARS does
      
      # Prepare CDC variables for merging
      cdc_for_merge <- cdc_data %>%
        select(GEOID, year, 
               matches("traffic|transport")) %>%
        # Fill _data_quality columns if they don't exist
        mutate(across(matches("traffic|transport"), 
                     ~., 
                     .names = "{.col}_data_quality")) %>%
        mutate(across(ends_with("_data_quality"), 
                     ~ifelse(is.na(.x), direct_flag, .x)))
      
      # Merge with combined_data
      combined_data <- combined_data %>%
        left_join(cdc_for_merge, by = c("GEOID", "year"), suffix = c("", "_cdc"))
      
      # For variables that might overlap with FARS but are missing in combined_data,
      # use the CDC data
      if ("traffic_fatality_count_cdc" %in% names(combined_data)) {
        combined_data <- combined_data %>%
          mutate(
            traffic_fatality_count = ifelse(
              is.na(traffic_fatality_count) & !is.na(traffic_fatality_count_cdc),
              traffic_fatality_count_cdc,
              traffic_fatality_count
            ),
            traffic_fatality_count_data_quality = as.character(ifelse(
              is.na(traffic_fatality_count_data_quality) & 
                !is.na(traffic_fatality_count_cdc),
              direct_flag,
              traffic_fatality_count_data_quality
            ))
          )
      }
      
      # Handle other CDC-specific variables that might not be in FARS
      # For example, CDC might have broader transport-related mortality
      if ("transport_mortality_count" %in% names(cdc_for_merge)) {
        # Add this as a new variable if it doesn't exist
        if (!"transport_mortality_count" %in% names(combined_data)) {
          combined_data$transport_mortality_count <- NA_real_
          combined_data$transport_mortality_count_data_quality <- as.character(missing_flag)
        }
        
        # Update with CDC data
        combined_data <- combined_data %>%
          mutate(
            transport_mortality_count = ifelse(
              !is.na(transport_mortality_count_cdc),
              transport_mortality_count_cdc,
              transport_mortality_count
            ),
            transport_mortality_count_data_quality = as.character(ifelse(
              !is.na(transport_mortality_count_cdc),
              direct_flag,
              transport_mortality_count_data_quality
            ))
          )
      }
      
      # Clean up temporary CDC columns
      temp_cols <- grep("_cdc$", names(combined_data), value = TRUE)
      if (length(temp_cols) > 0) {
        combined_data <- combined_data %>%
          select(-all_of(temp_cols))
      }
    } else {
      warning("CDC data missing required 'fips' or 'year' columns. Skipping.")
    }
  }
  
  # Standardize GEOID codes to ensure proper formatting
  combined_data <- combined_data %>%
    mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
  
  # Interpolate missing years if allowed
  if (allow_interpolation) {
    message("Performing interpolation for missing years...")
    
    # Define the variables to interpolate (excluding quality flags)
    vars_to_interpolate <- grep("count$|rate", names(combined_data), value = TRUE)
    vars_to_interpolate <- vars_to_interpolate[!grepl("_data_quality$", vars_to_interpolate)]
    
    # Save original values for comparing changes
    original_values <- combined_data
    
    # Interpolate each variable for each county
    combined_data <- combined_data %>%
      group_by(GEOID) %>%
      mutate(across(all_of(vars_to_interpolate), 
                   ~if(any(!is.na(.))) {
                     zoo::na.approx(.x, na.rm = FALSE)
                   } else {
                     .x
                   })) %>%
      ungroup()
    
    # Update data quality flags for interpolated values
    for (var in vars_to_interpolate) {
      quality_var <- paste0(var, "_data_quality")
      
      # Compare original and current values to identify interpolated ones
      combined_data[[quality_var]] <- ifelse(
        is.na(original_values[[var]]) & !is.na(combined_data[[var]]),
        interpolated_flag,
        combined_data[[quality_var]]
      )
    }
  }
  
  # Calculate rates using population data if available
  if (!is.null(population_data) && nrow(population_data) > 0) {
    message("Calculating rates using population data...")
    
    # Ensure population data has standardized FIPS codes and rename to GEOID
    population_data <- population_data %>%
      mutate(fips = sprintf("%05d", as.numeric(fips))) %>%
      rename(GEOID = fips)
    
    # Join with population data (keeping only required columns)
    combined_data <- combined_data %>%
      left_join(population_data %>% select(GEOID, year, population), 
               by = c("GEOID", "year"))
    
    # Calculate rates using actual population
    combined_data <- combined_data %>%
      mutate(
        # For each count variable, calculate or update the corresponding rate
        traffic_fatality_rate_per_100k = ifelse(
          !is.na(traffic_fatality_count) & !is.na(population) & population > 0,
          (traffic_fatality_count / population) * 100000,
          traffic_fatality_rate_per_100k
        ),
        traffic_injury_rate_per_100k = ifelse(
          !is.na(traffic_injury_count) & !is.na(population) & population > 0,
          (traffic_injury_count / population) * 100000,
          traffic_injury_rate_per_100k
        ),
        ped_bike_fatality_rate_per_100k = ifelse(
          !is.na(ped_bike_fatality_count) & !is.na(population) & population > 0,
          (ped_bike_fatality_count / population) * 100000,
          ped_bike_fatality_rate_per_100k
        ),
        dui_fatality_rate_per_100k = ifelse(
          !is.na(dui_fatality_count) & !is.na(population) & population > 0,
          (dui_fatality_count / population) * 100000,
          dui_fatality_rate_per_100k
        ),
        speeding_fatality_rate_per_100k = ifelse(
          !is.na(speeding_fatality_count) & !is.na(population) & population > 0,
          (speeding_fatality_count / population) * 100000,
          speeding_fatality_rate_per_100k
        )
      )
    
    # Update data quality flags for calculated rates
    combined_data <- combined_data %>%
      mutate(
        traffic_fatality_rate_per_100k_data_quality = ifelse(
          !is.na(traffic_fatality_count) & !is.na(population) & population > 0 & 
            (is.na(traffic_fatality_rate_per_100k_data_quality) || 
             traffic_fatality_rate_per_100k_data_quality == missing_flag),
          "calculated",
          traffic_fatality_rate_per_100k_data_quality
        ),
        traffic_injury_rate_per_100k_data_quality = ifelse(
          !is.na(traffic_injury_count) & !is.na(population) & population > 0 & 
            (is.na(traffic_injury_rate_per_100k_data_quality) || 
             traffic_injury_rate_per_100k_data_quality == missing_flag),
          "calculated",
          traffic_injury_rate_per_100k_data_quality
        ),
        ped_bike_fatality_rate_per_100k_data_quality = ifelse(
          !is.na(ped_bike_fatality_count) & !is.na(population) & population > 0 & 
            (is.na(ped_bike_fatality_rate_per_100k_data_quality) || 
             ped_bike_fatality_rate_per_100k_data_quality == missing_flag),
          "calculated",
          ped_bike_fatality_rate_per_100k_data_quality
        ),
        dui_fatality_rate_per_100k_data_quality = ifelse(
          !is.na(dui_fatality_count) & !is.na(population) & population > 0 & 
            (is.na(dui_fatality_rate_per_100k_data_quality) || 
             dui_fatality_rate_per_100k_data_quality == missing_flag),
          "calculated",
          dui_fatality_rate_per_100k_data_quality
        ),
        speeding_fatality_rate_per_100k_data_quality = ifelse(
          !is.na(speeding_fatality_count) & !is.na(population) & population > 0 & 
            (is.na(speeding_fatality_rate_per_100k_data_quality) || 
             speeding_fatality_rate_per_100k_data_quality == missing_flag),
          "calculated",
          speeding_fatality_rate_per_100k_data_quality
        )
      )
    
    message(paste("Calculated rates for", 
                  sum(!is.na(combined_data$traffic_fatality_rate_per_100k) & 
                        combined_data$traffic_fatality_rate_per_100k_data_quality == "calculated"), 
                  "counties using actual population data"))
  }
  
  # Ensure we have GEOID for compatibility with the SDOH pipeline
  combined_data <- combined_data %>%
    mutate(
      GEOID = fips,  # Add GEOID for compatibility with SDOH pipeline
      
      # Make sure all data quality flags are filled
      across(ends_with("_data_quality"), 
            ~ifelse(is.na(.x), missing_flag, .x))
    ) %>%
    # Use GEOID consistently instead of fips to fix column naming inconsistency with process_extended_data_v2
    rename_with(~gsub("^fips$", "GEOID", .), everything())
  
  # Save the combined dataset to cache
  message("Saving combined traffic safety data to cache...")
  saveRDS(combined_data, cache_file)
  
  # Return the final dataset
  return(combined_data)
}

#' Fetch data from NHTSA's Fatality Analysis Reporting System (FARS)
#'
#' @param years Vector of years to fetch data for
#' @param cache_dir Directory to store cached data
#' @param refresh_cache Whether to refresh cached data
#'
#' @return A data frame with FARS data
#' 
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate filter select rename
get_fars_data <- function(years, cache_dir, refresh_cache = FALSE) {
  # Define API endpoints and file paths
  fars_api_base <- "https://crashviewer.nhtsa.dot.gov/CrashAPI/"
  fars_data_url <- "https://www.nhtsa.gov/file-downloads?p=nhtsa/downloads/FARS/"
  
  # Alternative data sources
  fars_alt_sources <- list(
    # Official alternative direct URLs for FARS files
    nhtsa_ftp = "https://www.nhtsa.gov/content/nhtsa/downloads/", 
    nhtsa_ftp2 = "https://crashstats.nhtsa.dot.gov/Api/Public/ViewPublication/",
    # Data archive (for older data)
    fars_archive = "https://www.transportation.gov/data/safety/archive/FARS"
  )
  
  # Create cache file path
  fars_cache_file <- file.path(cache_dir, paste0("fars_data_", min(years), "_", max(years), ".rds"))
  
  # Check cache first
  if (file.exists(fars_cache_file) && !refresh_cache) {
    message("Loading FARS data from cache...")
    return(readRDS(fars_cache_file))
  }
  
  # Create cache directory for traffic safety
  traffic_safety_dir <- file.path(cache_dir, "traffic_safety")
  if (!dir.exists(traffic_safety_dir)) {
    dir.create(traffic_safety_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Check for predownloaded sample data
  fars_sample_path <- file.path(dirname(cache_dir), "traffic_safety/fars/FARS_2020_county.csv")
  if (file.exists(fars_sample_path)) {
    message("Found pre-downloaded sample FARS data file. Using this as seed data.")
    
    # Read the sample data
    sample_data <- read.csv(fars_sample_path, stringsAsFactors = FALSE)
    
    # Make sure the required 2020 data is present in the request
    if (2020 %in% years) {
      # Initialize fars_data with the sample data, but don't return immediately
      # This allows us to still try to fetch additional years
      fars_data <- sample_data
      
      # Save to cache directly for 2020
      fars_2020_cache <- file.path(traffic_safety_dir, "fars_2020.rds")
      saveRDS(sample_data, fars_2020_cache)
      message(paste("Cached 2020 FARS data with", nrow(sample_data), "records."))
      
      # Mark 2020 as already processed
      years <- years[years != 2020]
    }
  }
  
  # Check for pre-downloaded data in the data directory
  predownloaded_path <- file.path(dirname(cache_dir), "traffic_safety/fars")
  if (dir.exists(predownloaded_path)) {
    message("Checking for pre-downloaded FARS data...")
    
    # Look for year-specific data files
    year_files <- list()
    for (year in years) {
      # Check for CSV files first (preferred format)
      year_pattern <- paste0("fars.*", year, ".*\\.csv$|", year, ".*fars.*\\.csv$")
      year_files_csv <- list.files(
        path = predownloaded_path, 
        pattern = year_pattern, 
        recursive = TRUE, 
        ignore.case = TRUE,
        full.names = TRUE
      )
      
      # If no CSV, check for Excel files
      if (length(year_files_csv) == 0) {
        year_pattern <- paste0("fars.*", year, ".*\\.xlsx?$|", year, ".*fars.*\\.xlsx?$")
        year_files_xlsx <- list.files(
          path = predownloaded_path, 
          pattern = year_pattern, 
          recursive = TRUE, 
          ignore.case = TRUE,
          full.names = TRUE
        )
        
        if (length(year_files_xlsx) > 0) {
          year_files[[as.character(year)]] <- year_files_xlsx[1]
        }
      } else {
        year_files[[as.character(year)]] <- year_files_csv[1]
      }
    }
    
    # Process pre-downloaded files if found
    if (length(year_files) > 0) {
      message(paste("Found", length(year_files), "pre-downloaded FARS data files."))
      
      # Process each file
      year_data_list <- list()
      for (year in names(year_files)) {
        file_path <- year_files[[year]]
        message(paste("Processing pre-downloaded data for year", year, "from", basename(file_path)))
        
        # Read the file based on extension
        if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
          year_data <- tryCatch({
            read.csv(file_path, stringsAsFactors = FALSE)
          }, error = function(e) {
            message(paste("Error reading CSV:", e$message))
            return(NULL)
          })
        } else if (grepl("\\.xlsx?$", file_path, ignore.case = TRUE)) {
          year_data <- tryCatch({
            readxl::read_excel(file_path)
          }, error = function(e) {
            message(paste("Error reading Excel file:", e$message))
            return(NULL)
          })
        } else {
          year_data <- NULL
        }
        
        if (!is.null(year_data) && nrow(year_data) > 0) {
          # Process year data to match our expected output format
          # Look for standard FARS columns and rename as needed
          
          # Add year column if missing
          if (!"year" %in% names(year_data)) {
            year_data$year <- as.numeric(year)
          }
          
          # Standardize column names to lowercase
          year_data <- year_data %>%
            rename_with(~tolower(gsub(" ", "_", .x)))
          
          # Try to identify state and county columns to create FIPS
          if (all(c("state", "county") %in% names(year_data)) && !"fips" %in% names(year_data)) {
            year_data <- year_data %>%
              mutate(
                state = sprintf("%02d", as.numeric(state)),
                county = sprintf("%03d", as.numeric(county)),
                fips = paste0(state, county)
              )
          }
          
          # Add to data list
          year_data_list[[year]] <- year_data
        }
      }
      
      # Combine all year data
      if (length(year_data_list) > 0) {
        fars_data <- bind_rows(year_data_list)
        
        # Process and save to cache
        if (nrow(fars_data) > 0) {
          saveRDS(fars_data, fars_cache_file)
          message(paste("Saved processed FARS data from pre-downloaded files to cache with", nrow(fars_data), "records."))
          
          # Only return if we're not using other data sources (years is empty)
          if (length(years) == 0) {
            return(fars_data)
          }
          
          # Otherwise continue processing other years through API
        }
      }
    }
  }
  
  message("Fetching FARS data from NHTSA API...")
  
  # Initialize result data frame
  fars_data <- data.frame()
  
  # Fetch county-level summary data for each year
  for (year in years) {
    # Skip years before the FARS API has data (typically 1975+)
    if (year < 1975) {
      message(paste("Skipping", year, "- FARS data not available before 1975"))
      next
    }
    
    # Skip years in the future
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    if (year > current_year) {
      message(paste("Skipping", year, "- FARS data not available for future years"))
      next
    }
    
    # Check if we already have newer data that would include this year
    if (year > current_year - 2) {
      message(paste("FARS data for", year, "may be preliminary or not yet available"))
    }
    
    # Prepare cache file for this year
    year_cache_file <- file.path(cache_dir, paste0("fars_", year, ".rds"))
    
    # Check if year data is already in cache
    if (file.exists(year_cache_file) && !refresh_cache) {
      message(paste("Loading FARS data for", year, "from cache"))
      year_data <- readRDS(year_cache_file)
    } else {
      message(paste("Fetching FARS data for", year))
      
      # First try primary API
      api_data <- tryCatch({
        # Construct API endpoint for county-level data
        endpoint <- paste0(fars_api_base, "crashes/GetCrashesByLocation?year=", year, "&format=json")
        
        # Try to fetch data from API
        response <- httr::GET(endpoint, timeout(10))  # Add timeout to prevent hanging
        
        # Check if the request was successful
        if (httr::status_code(response) == 200) {
          # Parse the response content
          content <- httr::content(response, "text", encoding = "UTF-8")
          parsed <- jsonlite::fromJSON(content)
          
          # Extract the relevant data
          if (is.list(parsed) && !is.null(parsed$Results)) {
            # Process the results
            results_df <- as.data.frame(parsed$Results)
            
            # Add year column if not present
            if (!"year" %in% names(results_df)) {
              results_df$year <- year
            }
            
            return(results_df)
          }
        }
        # If we get here, the API request failed or returned unexpected format
        NULL
      }, error = function(e) {
        warning(paste("Primary API error for year", year, ":", e$message))
        NULL
      })
      
      # If primary API failed, try alternative APIs
      if (is.null(api_data)) {
        message(paste("Primary API failed for year", year, ". Trying alternative sources..."))
        
        # Try alternative endpoint format from NHTSA
        api_data <- tryCatch({
          # Try alternative NHTSA API endpoint (different format)
          alt_endpoint <- paste0("https://crashstats.nhtsa.dot.gov/Api/Public/GetCaseList?format=csv&year=", year)
          
          # Download to temporary file
          temp_file <- tempfile(fileext = ".csv")
          utils::download.file(alt_endpoint, temp_file, quiet = TRUE, mode = "wb")
          
          # Read the CSV file if it exists and has content
          if (file.exists(temp_file) && file.size(temp_file) > 100) {
            alt_data <- read.csv(temp_file, stringsAsFactors = FALSE)
            
            # Process the data to match expected format
            if (nrow(alt_data) > 0) {
              # Process to county level
              if (all(c("STATE", "COUNTY") %in% names(alt_data))) {
                county_data <- alt_data %>%
                  group_by(STATE, COUNTY) %>%
                  summarize(
                    traffic_fatality_count = n(),
                    .groups = "drop"
                  ) %>%
                  mutate(
                    fips = sprintf("%02d%03d", as.numeric(STATE), as.numeric(COUNTY)),
                    year = year
                  )
                return(county_data)
              }
            }
          }
          NULL
        }, error = function(e) {
          warning(paste("Alternative API error for year", year, ":", e$message))
          NULL
        }, finally = {
          # Clean up temporary file
          if (exists("temp_file") && file.exists(temp_file)) {
            file.remove(temp_file)
          }
        })
        
        # If still no data, try downloading from alternative FARS website
        if (is.null(api_data)) {
          # Try to download from NHTSA FTP site
          api_data <- tryCatch({
            # Construct URL for files (vary by year and format)
            alt_url <- if (year >= 2010) {
              paste0("https://www.nhtsa.gov/file-downloads/download?p=nhtsa/downloads/FARS/", 
                     year, "/National/FARS", year, "NationalCSV.zip")
            } else {
              paste0("https://www.nhtsa.gov/file-downloads/download?p=nhtsa/downloads/FARS/", 
                     year, "/Data/FARS", year, ".zip")
            }
            
            # Create temporary files
            temp_zip <- tempfile(fileext = ".zip")
            temp_dir <- tempdir()
            
            # Try to download the file
            utils::download.file(alt_url, temp_zip, mode = "wb", quiet = TRUE)
            
            # Extract the files
            utils::unzip(temp_zip, exdir = temp_dir)
            
            # Look for accident.csv or similar files
            accident_file <- list.files(temp_dir, pattern = "accident\\.csv$", 
                                      full.names = TRUE, recursive = TRUE)[1]
            
            # Process the accident data if found
            if (!is.na(accident_file) && file.exists(accident_file)) {
              accident_data <- read.csv(accident_file, stringsAsFactors = FALSE)
              
              # Process to county level
              if (all(c("STATE", "COUNTY") %in% names(accident_data))) {
                county_data <- accident_data %>%
                  group_by(STATE, COUNTY) %>%
                  summarize(
                    traffic_fatality_count = n(),
                    .groups = "drop"
                  ) %>%
                  mutate(
                    fips = sprintf("%02d%03d", as.numeric(STATE), as.numeric(COUNTY)),
                    year = year
                  )
                return(county_data)
              }
            }
            NULL
          }, error = function(e) {
            warning(paste("Error downloading FARS file for year", year, ":", e$message))
            NULL
          }, finally = {
            # Clean up temporary files
            if (exists("temp_zip") && file.exists(temp_zip)) {
              file.remove(temp_zip)
            }
          })
        }
      }
      
      # If API failed, try downloading the raw data files
      if (is.null(api_data)) {
        raw_data <- tryCatch({
          # Construct URL for the data file
          data_url <- paste0(fars_data_url, year, "/FARS", year, "NationalCSV.zip")
          
          # Create a temporary file to download to
          temp_zip <- tempfile(fileext = ".zip")
          
          # Try to download the file
          download_result <- tryCatch({
            utils::download.file(data_url, temp_zip, mode = "wb", quiet = TRUE)
            TRUE
          }, error = function(e) {
            warning(paste("Download failed:", e$message))
            FALSE
          })
          
          if (download_result) {
            # Extract the relevant files
            temp_dir <- tempdir()
            utils::unzip(temp_zip, exdir = temp_dir)
            
            # Look for accident.csv, person.csv, and vehicle.csv
            accident_file <- list.files(temp_dir, pattern = "accident\\.csv$", full.names = TRUE, recursive = TRUE)[1]
            person_file <- list.files(temp_dir, pattern = "person\\.csv$", full.names = TRUE, recursive = TRUE)[1]
            vehicle_file <- list.files(temp_dir, pattern = "vehicle\\.csv$", full.names = TRUE, recursive = TRUE)[1]
            
            if (!is.na(accident_file) && file.exists(accident_file)) {
              # Read the accident data
              accident_data <- read.csv(accident_file, stringsAsFactors = FALSE)
              
              # Process to county level
              county_data <- accident_data %>%
                group_by(STATE, COUNTY) %>%
                summarize(
                  traffic_fatality_count = n(),
                  speeding_related = sum(as.numeric(SPEEDREL) > 0, na.rm = TRUE),
                  .groups = "drop"
                ) %>%
                mutate(
                  fips = sprintf("%02d%03d", STATE, COUNTY),
                  year = year,
                  speeding_fatality_count = speeding_related
                ) %>%
                select(-speeding_related)
              
              # Add DUI information if available
              if (!is.na(person_file) && file.exists(person_file) &&
                  !is.na(vehicle_file) && file.exists(vehicle_file)) {
                
                # Read person and vehicle data
                person_data <- read.csv(person_file, stringsAsFactors = FALSE)
                vehicle_data <- read.csv(vehicle_file, stringsAsFactors = FALSE)
                
                # Get DUI counts by joining with vehicle data
                if ("DRINKING" %in% names(vehicle_data) || "DRUNK_DR" %in% names(vehicle_data)) {
                  # Determine which column to use
                  drunk_col <- if ("DRUNK_DR" %in% names(vehicle_data)) "DRUNK_DR" else "DRINKING"
                  
                  # Count DUI fatalities by county
                  dui_counts <- vehicle_data %>%
                    filter(get(drunk_col) == 1) %>%
                    left_join(accident_data %>% select(ST_CASE, STATE, COUNTY), by = "ST_CASE") %>%
                    group_by(STATE, COUNTY) %>%
                    summarize(
                      dui_fatality_count = n_distinct(ST_CASE),
                      .groups = "drop"
                    ) %>%
                    mutate(fips = sprintf("%02d%03d", STATE, COUNTY))
                  
                  # Add to county data
                  county_data <- county_data %>%
                    left_join(dui_counts %>% select(fips, dui_fatality_count), by = "fips")
                }
                
                # Get pedestrian/cyclist fatalities
                if ("PER_TYP" %in% names(person_data)) {
                  # Count pedestrian/cyclist fatalities by county
                  ped_bike_counts <- person_data %>%
                    filter(PER_TYP %in% c(1, 2, 3, 4, 5, 6, 7)) %>%  # Pedestrian and cyclist codes
                    left_join(accident_data %>% select(ST_CASE, STATE, COUNTY), by = "ST_CASE") %>%
                    group_by(STATE, COUNTY) %>%
                    summarize(
                      ped_bike_fatality_count = n_distinct(ST_CASE),
                      .groups = "drop"
                    ) %>%
                    mutate(fips = sprintf("%02d%03d", STATE, COUNTY))
                  
                  # Add to county data
                  county_data <- county_data %>%
                    left_join(ped_bike_counts %>% select(fips, ped_bike_fatality_count), by = "fips")
                }
              }
              
              return(county_data)
            }
          }
          NULL
        }, error = function(e) {
          warning(paste("Raw data processing error:", e$message))
          NULL
        }, finally = {
          # Clean up temporary files
          if (exists("temp_zip") && file.exists(temp_zip)) file.remove(temp_zip)
        })
        
        year_data <- raw_data
      } else {
        year_data <- api_data
      }
      
      # If we got data, save it to cache
      if (!is.null(year_data) && nrow(year_data) > 0) {
        saveRDS(year_data, year_cache_file)
      } else {
        warning(paste("No FARS data available for", year))
        year_data <- NULL
      }
    }
    
    # Append this year's data to the result
    if (!is.null(year_data) && nrow(year_data) > 0) {
      # Ensure year column exists
      if (!"year" %in% names(year_data)) {
        year_data$year <- year
      }
      
      fars_data <- bind_rows(fars_data, year_data)
    }
  }
  
  # If we couldn't get any data, return NULL
  if (nrow(fars_data) == 0) {
    warning("Could not retrieve FARS data for any of the requested years")
    return(NULL)
  }
  
  # Standardize column names
  fars_data <- fars_data %>%
    rename_with(~tolower(gsub(" ", "_", .x)))
  
  # Ensure we have a GEOID column (renamed from fips for consistency)
  if (!"GEOID" %in% names(fars_data)) {
    if ("fips" %in% names(fars_data)) {
      # If fips exists, rename it to GEOID
      fars_data <- fars_data %>%
        rename(GEOID = fips)
    } else if ("geoid" %in% names(fars_data)) {
      # If geoid exists, rename it to GEOID (standardize case)
      fars_data <- fars_data %>%
        rename(GEOID = geoid)
    } else if ("county_fips" %in% names(fars_data)) {
      # If county_fips exists, rename it to GEOID
      fars_data <- fars_data %>%
        rename(GEOID = county_fips)
    } else if (all(c("state", "county") %in% names(fars_data))) {
      # Create GEOID from state and county codes
      fars_data <- fars_data %>%
        mutate(
          state = sprintf("%02d", as.numeric(state)),
          county = sprintf("%03d", as.numeric(county)),
          GEOID = paste0(state, county)
        )
    }
  }
  
  # Standardize data types
  fars_data <- fars_data %>%
    mutate(
      GEOID = as.character(GEOID),
      year = as.numeric(year),
      # Ensure all numeric columns are properly typed
      across(matches("count|rate|number|total"), ~as.numeric(as.character(.x)))
    )
  
  # Filter to valid records
  fars_data <- fars_data %>%
    filter(year %in% years,
           !is.na(GEOID),
           nchar(GEOID) == 5)
  
  # Save the processed data to cache
  saveRDS(fars_data, fars_cache_file)
  
  return(fars_data)
}

#' Fetch transportation mortality data from CDC WONDER
#'
#' @param years Vector of years to fetch data for
#' @param cache_dir Directory to store cached data
#' @param refresh_cache Whether to refresh cached data
#'
#' @return A data frame with CDC WONDER data
#' 
#' @importFrom httr POST content
#' @importFrom dplyr mutate filter select rename
get_cdc_wonder_data <- function(years, cache_dir, refresh_cache = FALSE) {
  # Define cache file path
  cdc_cache_file <- file.path(cache_dir, paste0("cdc_wonder_data_", min(years), "_", max(years), ".rds"))
  
  # Check cache first
  if (file.exists(cdc_cache_file) && !refresh_cache) {
    message("Loading CDC WONDER data from cache...")
    return(readRDS(cdc_cache_file))
  }
  
  message("Fetching transportation mortality data from CDC WONDER...")
  
  # CDC WONDER API access requires formal permissions and is complex
  # For this implementation, we'll use CDC's publicly available datasets
  
  # Initialize data frame to store results
  cdc_data <- data.frame()
  
  # Define available CDC WONDER datasets by year range
  datasets <- list(
    "D76" = c(1999:2019),  # Multiple Cause of Death 1999-2019
    "D77" = c(2018:2022)   # Multiple Cause of Death 2018-2022
  )
  
  # Function to query CDC WONDER API (requires API access)
  query_cdc_wonder <- function(dataset, query_years) {
    # This is a simplified version - actual implementation would use CDC WONDER API
    # In a real implementation, you would:
    # 1. Build XML request following CDC WONDER specifications
    # 2. Send request via httr::POST
    # 3. Parse XML response
    # 4. Extract county-level transportation-related mortality data
    
    # For now, we'll look for locally cached data or downloaded data
    # that matches the CDC WONDER format
    
    message(paste("Looking for", dataset, "data for years", 
                  min(query_years), "to", max(query_years)))
    
    # Check for local CDC data files
    data_files <- NULL
    
    data_dirs <- c(
      "data/traffic_safety/cdc",
      "data/cdc_wonder",
      "data/cdc"
    )
    
    # Create pattern to match the specific dataset and years
    pattern <- paste0(dataset, ".*", paste0(query_years, collapse = "|"))
    
    for (dir in data_dirs) {
      if (dir.exists(dir)) {
        # Look for CSV, Excel, or RDS files
        files <- list.files(dir, pattern = pattern, 
                          full.names = TRUE, recursive = TRUE)
        
        if (length(files) > 0) {
          data_files <- files
          break
        }
      }
    }
    
    # If no specific dataset files, look for any CDC data
    if (is.null(data_files)) {
      for (dir in data_dirs) {
        if (dir.exists(dir)) {
          # Look for CDC files with transport-related names
          files <- list.files(dir, pattern = "transport|traffic|vehicle|mortality", 
                            full.names = TRUE, recursive = TRUE)
          
          if (length(files) > 0) {
            data_files <- files
            break
          }
        }
      }
    }
    
    # If we found files, try to read them
    if (!is.null(data_files) && length(data_files) > 0) {
      # Initialize results
      combined_data <- data.frame()
      
      for (file in data_files) {
        file_data <- NULL
        
        if (grepl("\\.csv$", file, ignore.case = TRUE)) {
          file_data <- tryCatch({
            read.csv(file, stringsAsFactors = FALSE)
          }, error = function(e) NULL)
        } else if (grepl("\\.xlsx?$", file, ignore.case = TRUE)) {
          file_data <- tryCatch({
            readxl::read_excel(file)
          }, error = function(e) NULL)
        } else if (grepl("\\.rds$", file, ignore.case = TRUE)) {
          file_data <- tryCatch({
            readRDS(file)
          }, error = function(e) NULL)
        }
        
        # If we got data, filter to transportation-related causes
        if (!is.null(file_data) && nrow(file_data) > 0) {
          # Check for ICD code columns
          icd_cols <- grep("icd|^cod|cause", names(file_data), value = TRUE, ignore.case = TRUE)
          
          if (length(icd_cols) > 0) {
            # Filter to transportation-related ICD codes (V01-V99)
            # This is a simplification - real implementation would be more precise
            for (icd_col in icd_cols) {
              if (any(grepl("^V[0-9]{2}", file_data[[icd_col]]))) {
                file_data <- file_data %>%
                  filter(grepl("^V[0-9]{2}", !!sym(icd_col)))
                break
              }
            }
          }
          
          # Check for year column and filter to requested years
          if ("year" %in% names(file_data)) {
            file_data <- file_data %>%
              filter(year %in% query_years)
          } else {
            # Try to extract year from file name
            year_match <- regexpr("[0-9]{4}", basename(file))
            if (year_match > 0) {
              extract_year <- as.numeric(
                substr(basename(file), year_match, year_match + 3)
              )
              
              if (!is.na(extract_year) && extract_year %in% query_years) {
                file_data$year <- extract_year
              }
            }
          }
          
          # Combine with results
          combined_data <- bind_rows(combined_data, file_data)
        }
      }
      
      return(combined_data)
    }
    
    # If no data found, return empty data frame
    return(data.frame())
  }
  
  # Process each year range with the appropriate dataset
  for (dataset_name in names(datasets)) {
    dataset_years <- datasets[[dataset_name]]
    overlap_years <- intersect(years, dataset_years)
    
    if (length(overlap_years) > 0) {
      # Query this dataset for the overlapping years
      dataset_data <- query_cdc_wonder(dataset_name, overlap_years)
      
      if (nrow(dataset_data) > 0) {
        cdc_data <- bind_rows(cdc_data, dataset_data)
      }
    }
  }
  
  # If we have any data, process it
  if (nrow(cdc_data) > 0) {
    # Standardize column names
    cdc_data <- cdc_data %>%
      rename_with(~tolower(gsub(" ", "_", .x)))
    
    # Ensure we have a GEOID column (renamed from fips for consistency)
    if (!"GEOID" %in% names(cdc_data)) {
      if ("fips" %in% names(cdc_data)) {
        # If fips exists, rename it to GEOID
        cdc_data <- cdc_data %>%
          rename(GEOID = fips)
      } else if ("county_code" %in% names(cdc_data)) {
        cdc_data <- cdc_data %>%
          rename(GEOID = county_code)
      } else if ("county_fips" %in% names(cdc_data)) {
        cdc_data <- cdc_data %>%
          rename(GEOID = county_fips)
      } else if ("geoid" %in% names(cdc_data)) {
        cdc_data <- cdc_data %>%
          rename(GEOID = geoid)
      } else if (all(c("state_code", "county_code") %in% names(cdc_data))) {
        cdc_data <- cdc_data %>%
          mutate(
            state_code = sprintf("%02d", as.numeric(state_code)),
            county_code = sprintf("%03d", as.numeric(county_code)),
            GEOID = paste0(state_code, county_code)
          )
      }
    }
    
    # Standardize column data types
    cdc_data <- cdc_data %>%
      mutate(
        GEOID = as.character(GEOID),
        year = as.numeric(year),
        # Ensure all numeric columns are properly typed
        across(matches("count|rate|number|total|deaths"), 
               ~as.numeric(as.character(.x)))
      )
    
    # Filter to valid records
    cdc_data <- cdc_data %>%
      filter(year %in% years,
             !is.na(GEOID),
             nchar(GEOID) == 5)
    
    # Rename deaths column to transport_mortality_count if present
    if ("deaths" %in% names(cdc_data) && !"transport_mortality_count" %in% names(cdc_data)) {
      cdc_data <- cdc_data %>%
        rename(transport_mortality_count = deaths)
    }
    
    # Check for crucial columns
    if (!"transport_mortality_count" %in% names(cdc_data) && 
        !"traffic_fatality_count" %in% names(cdc_data)) {
      warning("CDC data lacks mortality count columns. Data may be incomplete.")
    }
  } else {
    warning("No CDC WONDER data found for the requested years. Consider downloading CDC WONDER data manually.")
  }
  
  # Save processed data to cache
  saveRDS(cdc_data, cdc_cache_file)
  
  return(cdc_data)
}