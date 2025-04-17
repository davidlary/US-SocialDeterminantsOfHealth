#!/usr/bin/env Rscript

# Crime & Safety Data Fetcher
# This script handles retrieval of crime data from FBI Uniform Crime Reports
# and Bureau of Justice Statistics

library(tidyverse)
library(httr)
library(jsonlite)
library(readxl)
library(lubridate)
library(sf)
library(zoo) # For interpolation if needed

#' Fetch crime & safety data
#'
#' Retrieves crime data from FBI Uniform Crime Reports and Bureau of Justice Statistics
#' for crime rates and incarceration metrics at the county level.
#'
#' @param years Vector of years to include
#' @param cache_dir Directory to store cache files
#' @param refresh_cache Whether to refresh the cache
#' @param allow_simulation Whether to generate simulated data if real data not available
#' @param allow_interpolation Whether to interpolate missing values
#' @param data_quality_flags List of standardized data quality flags
#' @param offline_mode Whether to skip all downloads and use only cached data
#' @return A data frame with crime data for all requested years
fetch_crime_data <- function(years, 
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
  cache_file <- file.path(cache_dir, "crime_data.rds")
  
  # Use cache if available and not refreshing
  if (!refresh_cache && file.exists(cache_file)) {
    print_msg("Loading cached crime data...")
    crime_data <- readRDS(cache_file)
    
    # Check if all requested years are in the cache
    cached_years <- unique(crime_data$year)
    missing_years <- setdiff(years, cached_years)
    
    # Check for empty cache with just placeholder data
    if (nrow(crime_data) <= 1 || 
        (is.data.frame(crime_data) && "data_source" %in% names(crime_data) && 
         any(grepl("SIMULATED", crime_data$data_source)))) {
      print_msg("Cached crime data appears to be empty or a placeholder. Will process files again.")
      # Force refresh by continuing past this point
    } else if (length(missing_years) == 0) {
      print_msg("Using complete cached crime data.")
      return(crime_data %>% filter(year %in% years))
    } else {
      print_msg(paste("Cache missing years:", paste(missing_years, collapse=", ")))
    }
  }
  
  # Make sure cache directory exists
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    print_msg(paste("Created cache directory at:", cache_dir))
  }
  
  # Make data directory if needed
  data_dir <- "data/crime"
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
    print_msg(paste("Created crime data directory at:", data_dir))
  }
  
  # Helper function to safely download and read files
  safe_download <- function(url, destfile, description) {
    if (offline_mode) {
      print_msg(paste("Skipping download of", description, "(offline mode)"))
      return(file.exists(destfile))
    }
    
    print_msg(paste("Downloading", description, "from:", url))
    tryCatch({
      download.file(url, destfile, mode = "wb", quiet = TRUE)
      print_msg(paste("Successfully downloaded", description))
      return(TRUE)
    }, error = function(e) {
      print_msg(paste("Error downloading", description, ":", conditionMessage(e)))
      return(FALSE)
    })
  }
  
  # Function to get FBI UCR data
  get_ucr_data <- function() {
    # FBI UCR data is available from 2000 to 2021
    # The format changed in 2021 with the transition to NIBRS
    
    # Define variables we want to extract
    ucr_variables <- c(
      "violent_crime_rate" = "Violent crimes per 100,000 population",
      "property_crime_rate" = "Property crimes per 100,000 population",
      "homicide_rate" = "Homicides per 100,000 population"
    )
    
    # FBI UCR data list to store results
    ucr_data_list <- list()
    
    # Filter to years up to 2021
    ucr_years <- years[years >= 2000 & years <= 2021]
    
    for (year in ucr_years) {
      # Define file paths
      ucr_file <- file.path(data_dir, paste0("ucr_", year, ".csv"))
      
      # Check if we need to download
      need_download <- !file.exists(ucr_file) || refresh_cache
      
      if (need_download) {
        # FBI UCR URL
        # For 2021+, use crime-data-explorer API
        # For earlier years, use archived data
        if (year >= 2021) {
          ucr_url <- paste0(
            "https://crime-data-explorer.fr.cloud.gov/api/summarized/agencies/counties/",
            year, 
            "/offenses"
          )
        } else {
          ucr_url <- paste0(
            "https://s3-us-gov-west-1.amazonaws.com/cg-d4b776d0-d898-4153-90c8-8336f86bdfec/",
            year, 
            "/county_crime.csv"
          )
        }
        
        # Try to download
        if (!safe_download(ucr_url, ucr_file, paste("FBI UCR data for", year))) {
          print_msg(paste("Could not download FBI UCR data for", year))
          next
        }
      } else {
        print_msg(paste("Using existing FBI UCR file for", year))
      }
      
      # Process the data if file exists
      if (file.exists(ucr_file)) {
        print_msg(paste("Reading FBI UCR data for", year))
        
        # Read the file
        tryCatch({
          ucr_data <- read_csv(ucr_file, show_col_types = FALSE)
          
          # Get column names
          print_msg(paste("UCR data has", ncol(ucr_data), "columns and", nrow(ucr_data), "rows"))
          
          # Check for FIPS/GEOID column
          geoid_col <- grep("FIPS|fips|geoid|GEOID|county.*code|COUNTY.*CODE", 
                           names(ucr_data), value = TRUE)[1]
          
          if (is.na(geoid_col)) {
            print_msg("Could not identify GEOID column in UCR data")
            next
          }
          
          # Rename and format GEOID
          ucr_data <- ucr_data %>%
            rename(GEOID = all_of(geoid_col)) %>%
            mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
          
          # Find columns for our variables of interest
          
          # Violent crime rate
          violent_col <- grep("violent.*rate|violent.*per|violent.*100", 
                             names(ucr_data), value = TRUE)[1]
          
          # Property crime rate
          property_col <- grep("property.*rate|property.*per|property.*100", 
                              names(ucr_data), value = TRUE)[1]
          
          # Homicide rate
          homicide_col <- grep("homicide.*rate|murder.*rate|homicide.*per|murder.*per", 
                              names(ucr_data), value = TRUE)[1]
          
          print_msg(paste("Found columns: Violent crime:", !is.na(violent_col),
                        "Property crime:", !is.na(property_col),
                        "Homicide:", !is.na(homicide_col)))
          
          # Create data frame for this year
          year_data <- data.frame(
            GEOID = ucr_data$GEOID,
            year = year
          )
          
          # Add violent crime rate if available
          if (!is.na(violent_col)) {
            year_data$violent_crime_rate <- ucr_data[[violent_col]]
            year_data$violent_crime_rate_data_quality <- data_quality_flags$direct
            year_data$violent_crime_rate_data_source <- "FBI Uniform Crime Reports"
            year_data$violent_crime_rate_data_vintage <- as.character(year)
          }
          
          # Add property crime rate if available
          if (!is.na(property_col)) {
            year_data$property_crime_rate <- ucr_data[[property_col]]
            year_data$property_crime_rate_data_quality <- data_quality_flags$direct
            year_data$property_crime_rate_data_source <- "FBI Uniform Crime Reports"
            year_data$property_crime_rate_data_vintage <- as.character(year)
          }
          
          # Add homicide rate if available
          if (!is.na(homicide_col)) {
            year_data$homicide_rate <- ucr_data[[homicide_col]]
            year_data$homicide_rate_data_quality <- data_quality_flags$direct
            year_data$homicide_rate_data_source <- "FBI Uniform Crime Reports"
            year_data$homicide_rate_data_vintage <- as.character(year)
          }
          
          # Add to list
          ucr_data_list[[as.character(year)]] <- year_data
          
          print_msg(paste("Processed FBI UCR data for", year))
        }, error = function(e) {
          print_msg(paste("Error reading FBI UCR data for", year, ":", conditionMessage(e)))
        })
      }
    }
    
    # Combine all years
    if (length(ucr_data_list) > 0) {
      combined_ucr <- bind_rows(ucr_data_list)
      print_msg(paste("Combined FBI UCR data with", nrow(combined_ucr), "rows"))
      return(combined_ucr)
    } else {
      print_msg("No FBI UCR data processed successfully")
      return(NULL)
    }
  }
  
  # Function to get Bureau of Justice Statistics data
  get_bjs_data <- function() {
    # BJS has county-level jail incarceration data
    
    # Define variables we want to extract
    bjs_variables <- c(
      "jail_incarceration_rate" = "County jail inmates per 100,000 population",
      "pretrial_detention_rate" = "Pretrial detainees per 100,000 population"
    )
    
    # BJS data list to store results
    bjs_data_list <- list()
    
    # Filter to years up to 2020
    bjs_years <- years[years >= 2000 & years <= 2020]
    
    for (year in bjs_years) {
      # Define file paths
      bjs_file <- file.path(data_dir, paste0("bjs_jail_", year, ".csv"))
      
      # Check if we need to download
      need_download <- !file.exists(bjs_file) || refresh_cache
      
      if (need_download) {
        # BJS URL - placeholder, real URLs would depend on specific BJS data structure
        bjs_url <- paste0(
          "https://bjs.ojp.gov/content/pub/data/jail/county_jail_", 
          year, 
          ".csv"
        )
        
        # Try to download
        if (!safe_download(bjs_url, bjs_file, paste("BJS jail data for", year))) {
          print_msg(paste("Could not download BJS jail data for", year))
          # BJS data typically requires manual download from their site
          next
        }
      } else {
        print_msg(paste("Using existing BJS jail file for", year))
      }
      
      # Process the data if file exists
      if (file.exists(bjs_file)) {
        print_msg(paste("Reading BJS jail data for", year))
        
        # Read the file
        tryCatch({
          bjs_data <- read_csv(bjs_file, show_col_types = FALSE)
          
          # Get column names
          print_msg(paste("BJS data has", ncol(bjs_data), "columns and", nrow(bjs_data), "rows"))
          
          # Check for FIPS/GEOID column
          geoid_col <- grep("FIPS|fips|geoid|GEOID|county.*code|COUNTY.*CODE", 
                           names(bjs_data), value = TRUE)[1]
          
          if (is.na(geoid_col)) {
            print_msg("Could not identify GEOID column in BJS data")
            next
          }
          
          # Rename and format GEOID
          bjs_data <- bjs_data %>%
            rename(GEOID = all_of(geoid_col)) %>%
            mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
          
          # Find columns for our variables of interest
          
          # Jail incarceration rate
          jail_col <- grep("jail.*rate|incarceration.*rate|jail.*per|incarceration.*per", 
                          names(bjs_data), value = TRUE)[1]
          
          # Pretrial detention rate
          pretrial_col <- grep("pretrial.*rate|pretrial.*per", 
                              names(bjs_data), value = TRUE)[1]
          
          print_msg(paste("Found columns: Jail incarceration:", !is.na(jail_col),
                        "Pretrial detention:", !is.na(pretrial_col)))
          
          # Create data frame for this year
          year_data <- data.frame(
            GEOID = bjs_data$GEOID,
            year = year
          )
          
          # Add jail incarceration rate if available
          if (!is.na(jail_col)) {
            year_data$jail_incarceration_rate <- bjs_data[[jail_col]]
            year_data$jail_incarceration_rate_data_quality <- data_quality_flags$direct
            year_data$jail_incarceration_rate_data_source <- "Bureau of Justice Statistics"
            year_data$jail_incarceration_rate_data_vintage <- as.character(year)
          }
          
          # Add pretrial detention rate if available
          if (!is.na(pretrial_col)) {
            year_data$pretrial_detention_rate <- bjs_data[[pretrial_col]]
            year_data$pretrial_detention_rate_data_quality <- data_quality_flags$direct
            year_data$pretrial_detention_rate_data_source <- "Bureau of Justice Statistics"
            year_data$pretrial_detention_rate_data_vintage <- as.character(year)
          }
          
          # Add to list
          bjs_data_list[[as.character(year)]] <- year_data
          
          print_msg(paste("Processed BJS jail data for", year))
        }, error = function(e) {
          print_msg(paste("Error reading BJS jail data for", year, ":", conditionMessage(e)))
        })
      }
    }
    
    # Combine all years
    if (length(bjs_data_list) > 0) {
      combined_bjs <- bind_rows(bjs_data_list)
      print_msg(paste("Combined BJS data with", nrow(combined_bjs), "rows"))
      return(combined_bjs)
    } else {
      print_msg("No BJS data processed successfully")
      return(NULL)
    }
  }
  
  # Get data from different sources
  ucr_data <- get_ucr_data()
  bjs_data <- get_bjs_data()
  
  # Combine all data sources
  crime_data_list <- list()
  
  if (!is.null(ucr_data) && nrow(ucr_data) > 0) {
    crime_data_list[["ucr"]] <- ucr_data
  }
  
  if (!is.null(bjs_data) && nrow(bjs_data) > 0) {
    crime_data_list[["bjs"]] <- bjs_data
  }
  
  # Process if we have data
  if (length(crime_data_list) > 0) {
    # Combine all sources and handle overlapping columns
    # Start with first dataset
    combined_crime_data <- crime_data_list[[1]]
    
    # Add each additional dataset
    if (length(crime_data_list) > 1) {
      for (i in 2:length(crime_data_list)) {
        next_data <- crime_data_list[[i]]
        
        # Identify common columns for joining
        join_cols <- intersect(names(combined_crime_data), names(next_data))
        
        # Ensure we have at least GEOID and year for joining
        if (all(c("GEOID", "year") %in% join_cols)) {
          # Find columns that might overlap (excluding join columns and flags)
          overlap_cols <- setdiff(
            intersect(names(combined_crime_data), names(next_data)),
            c(join_cols, grep("_data_quality$|_data_source$|_data_vintage$", 
                             names(next_data), value = TRUE))
          )
          
          if (length(overlap_cols) > 0) {
            # For overlapping columns, prefer data from the current dataset
            # Remove those columns from the combined data before joining
            combined_crime_data <- combined_crime_data %>%
              select(-all_of(overlap_cols))
          }
          
          # Join with the next dataset
          combined_crime_data <- full_join(
            combined_crime_data, 
            next_data, 
            by = join_cols
          )
        } else {
          print_msg("Warning: Cannot join datasets - missing common keys")
        }
      }
    }
    
    # Check if we need to handle missing years
    available_years <- unique(combined_crime_data$year)
    missing_years <- setdiff(years, available_years)
    
    if (length(missing_years) > 0 && allow_interpolation) {
      print_msg(paste("Interpolating data for missing years:", paste(missing_years, collapse=", ")))
      
      # Get all variables except join columns and metadata columns
      measure_cols <- setdiff(
        names(combined_crime_data),
        c("GEOID", "year", grep("_data_quality$|_data_source$|_data_vintage$", 
                               names(combined_crime_data), value = TRUE))
      )
      
      # Process each county separately for interpolation
      counties <- unique(combined_crime_data$GEOID)
      
      # List to store interpolated data
      interp_data_list <- list()
      
      for (county in counties) {
        # Get data for this county
        county_data <- combined_crime_data %>%
          filter(GEOID == county) %>%
          arrange(year)
        
        # Create grid with all years for this county
        county_grid <- data.frame(
          GEOID = county,
          year = years,
          stringsAsFactors = FALSE
        )
        
        # Process each measure column
        for (col in measure_cols) {
          # Get values for this column
          x_values <- county_data$year
          y_values <- county_data[[col]]
          
          # Identify valid data points (not NA)
          valid_indices <- !is.na(y_values)
          
          # Skip if no valid data
          if (sum(valid_indices) < 2) {
            next
          }
          
          x_valid <- x_values[valid_indices]
          y_valid <- y_values[valid_indices]
          
          # Use approx for interpolation
          interp_result <- approx(x_valid, y_valid, xout = county_grid$year, rule = 1)
          county_grid[[col]] <- interp_result$y
          
          # Set quality flags based on interpolation status
          quality_col <- paste0(col, "_data_quality")
          source_col <- paste0(col, "_data_source")
          vintage_col <- paste0(col, "_data_vintage")
          
          # Get original quality flags to preserve direct data
          if (quality_col %in% names(county_data)) {
            original_quality <- county_data[[quality_col]]
            original_years <- county_data$year
            
            # Create initial quality flags
            county_grid[[quality_col]] <- NA
            
            # For each year in our grid
            for (i in 1:nrow(county_grid)) {
              grid_year <- county_grid$year[i]
              
              # If this is an original data year, preserve its quality flag
              if (grid_year %in% original_years) {
                idx <- which(original_years == grid_year)
                county_grid[[quality_col]][i] <- original_quality[idx]
              } else if (grid_year > max(x_valid) || grid_year < min(x_valid)) {
                # Extrapolation
                county_grid[[quality_col]][i] <- data_quality_flags$extrapolated
              } else {
                # Interpolation
                county_grid[[quality_col]][i] <- data_quality_flags$interpolated
              }
            }
            
            # Also copy source and vintage
            if (source_col %in% names(county_data)) {
              source_values <- county_data[[source_col]]
              county_grid[[source_col]] <- source_values[1]  # Use first source
            }
            
            if (vintage_col %in% names(county_data)) {
              county_grid[[vintage_col]] <- paste0("derived_from_", 
                                                 paste(sort(unique(x_valid)), collapse="_"))
            }
          }
        }
        
        # Add to list
        interp_data_list[[county]] <- county_grid
      }
      
      # Combine all counties
      if (length(interp_data_list) > 0) {
        interp_data <- bind_rows(interp_data_list)
        
        # Add any missing metadata columns with NA values
        meta_cols <- grep("_data_quality$|_data_source$|_data_vintage$", 
                         names(combined_crime_data), value = TRUE)
        
        for (col in meta_cols) {
          if (!(col %in% names(interp_data))) {
            interp_data[[col]] <- NA
          }
        }
        
        # Use the interpolated data instead
        combined_crime_data <- interp_data
      }
    } else if (length(missing_years) > 0) {
      print_msg("Missing years but interpolation not allowed. Years will remain missing.")
    }
    
    # Filter to just the requested years
    combined_crime_data <- combined_crime_data %>%
      filter(year %in% years)
    
    # Sort data
    combined_crime_data <- combined_crime_data %>%
      arrange(GEOID, year)
    
    # Cache the processed data
    saveRDS(combined_crime_data, cache_file)
    print_msg(paste("Cached crime data to:", cache_file))
    
    return(combined_crime_data)
  } else if (allow_simulation) {
    # Create simulated data
    print_msg("No crime data found. Creating simulated data...")
    
    # Crime variables to simulate
    crime_vars <- c(
      "violent_crime_rate" = "Violent crimes per 100,000 population",
      "property_crime_rate" = "Property crimes per 100,000 population",
      "homicide_rate" = "Homicides per 100,000 population",
      "jail_incarceration_rate" = "County jail inmates per 100,000 population",
      "pretrial_detention_rate" = "Pretrial detainees per 100,000 population"
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
      print_msg("Using sample county list for simulation")
    })
    
    # Create simulated data for each year
    sim_data_list <- list()
    for (year in years) {
      # Create base data frame with counties and year
      year_data <- counties %>%
        mutate(year = year)
      
      # Add simulated values for each variable
      for (var_name in names(crime_vars)) {
        if (var_name == "violent_crime_rate") {
          # Typically 100-1000 per 100,000
          year_data[[var_name]] <- runif(nrow(year_data), 100, 1000)
        } else if (var_name == "property_crime_rate") {
          # Typically 1000-4000 per 100,000
          year_data[[var_name]] <- runif(nrow(year_data), 1000, 4000)
        } else if (var_name == "homicide_rate") {
          # Typically 1-20 per 100,000
          year_data[[var_name]] <- runif(nrow(year_data), 1, 20)
        } else if (var_name == "jail_incarceration_rate") {
          # Typically 100-500 per 100,000
          year_data[[var_name]] <- runif(nrow(year_data), 100, 500)
        } else if (var_name == "pretrial_detention_rate") {
          # Typically 50-300 per 100,000
          year_data[[var_name]] <- runif(nrow(year_data), 50, 300)
        } else {
          # Default - 0-100 range
          year_data[[var_name]] <- runif(nrow(year_data), 0, 100)
        }
        
        # Add quality flags
        year_data[[paste0(var_name, "_data_quality")]] <- data_quality_flags$simulated
        year_data[[paste0(var_name, "_data_source")]] <- "SIMULATED Crime Data"
        year_data[[paste0(var_name, "_data_vintage")]] <- paste0("simulated_", year)
      }
      
      sim_data_list[[as.character(year)]] <- year_data
    }
    
    # Combine all years
    simulated_data <- bind_rows(sim_data_list)
    
    # Cache the simulated data
    saveRDS(simulated_data, cache_file)
    print_msg(paste("Cached simulated crime data to:", cache_file))
    
    return(simulated_data)
  } else {
    # No data and simulation not allowed - create empty dataset with NAs
    print_msg("No crime data available and simulation not allowed. Creating empty dataset with NAs.")
    
    # Crime variables to include
    crime_vars <- c(
      "violent_crime_rate",
      "property_crime_rate",
      "homicide_rate",
      "jail_incarceration_rate",
      "pretrial_detention_rate"
    )
    
    # Get county list using get_county_list() or fallback to sample counties
    # Try to get a comprehensive list if possible
    counties <- NULL
    tryCatch({
      # Check if we're running in a pipeline environment with get_county_list
      if (exists("get_county_list", mode="function")) {
        counties <- get_county_list()
        print_msg(paste("Using", nrow(counties), "counties from get_county_list function"))
      }
    }, error = function(e) {
      print_msg("Error getting county list from function")
    })
    
    # Fallback if counties is still NULL
    if (is.null(counties)) {
      # Sample counties
      counties <- data.frame(
        GEOID = c("01001", "01003", "01005", "01007", "01009"), # Sample counties
        NAME = c("Autauga County, Alabama", "Baldwin County, Alabama", 
                "Barbour County, Alabama", "Bibb County, Alabama", 
                "Blount County, Alabama")
      )
      print_msg("Using sample county list for empty dataset")
    }
    
    # Create grid with all counties and years
    grid <- expand.grid(
      GEOID = counties$GEOID,
      year = years,
      stringsAsFactors = FALSE
    )
    
    # Add NAME column if available
    if ("NAME" %in% names(counties)) {
      grid$NAME <- counties$NAME[match(grid$GEOID, counties$GEOID)]
    }
    
    # Add empty variable columns with NAs
    for (var in crime_vars) {
      grid[[var]] <- NA_real_
      grid[[paste0(var, "_data_quality")]] <- data_quality_flags$missing
      grid[[paste0(var, "_data_source")]] <- "NOT_AVAILABLE"
      grid[[paste0(var, "_data_vintage")]] <- NA_character_
    }
    
    crime_data <- as_tibble(grid)
    print_msg(paste("Created empty crime dataset with", nrow(crime_data), "rows"))
    
    # Cache the empty data
    saveRDS(crime_data, cache_file)
    print_msg(paste("Cached empty crime data to:", cache_file))
    
    return(crime_data)
  }
}

# This lets the function be used when the script is sourced
is_sourced <- function() {
  return(!identical(environment(), globalenv()))
}

# If the script is run directly, test the function
if (!is_sourced()) {
  cat("Testing crime data fetcher...\n")
  
  # Get current year
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  
  # Test for last 5 years
  test_years <- (current_year-4):current_year
  
  # Test the function
  result <- fetch_crime_data(
    years = test_years,
    cache_dir = "data/cache",
    refresh_cache = FALSE,
    allow_simulation = TRUE,
    allow_interpolation = TRUE,
    data_quality_flags = list(
      direct = "direct",
      interpolated = "interpolated",
      extrapolated = "extrapolated",
      simulated = "simulated",
      missing = NA,
      imputed = "imputed"
    )
  )
  
  # Report data quality metrics
  cat("Data quality summary:\n")
  quality_cols <- grep("_data_quality$", names(result), value = TRUE)
  for (col in quality_cols) {
    var_name <- gsub("_data_quality$", "", col)
    quality_counts <- table(result[[col]], useNA = "always")
    cat(paste(" -", var_name, ":", paste(names(quality_counts), quality_counts, sep="=", collapse=", "), "\n"))
  }
  
  cat("Test completed with", nrow(result), "rows of data.\n")
}
