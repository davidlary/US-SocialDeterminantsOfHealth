#!/usr/bin/env Rscript

# Transportation Data Fetcher
# This script handles retrieval of transportation data from NHTS, All Transit Database, and ACS

library(tidyverse)
library(httr)
library(jsonlite)
library(readxl)
library(lubridate)
library(sf)
library(zoo) # For interpolation if needed

#' Fetch transportation data
#'
#' Retrieves transportation data from National Household Travel Survey,
#' All Transit Database, and American Community Survey for transportation metrics at the county level.
#'
#' @param years Vector of years to include
#' @param cache_dir Directory to store cache files
#' @param refresh_cache Whether to refresh the cache
#' @param allow_simulation Whether to generate simulated data if real data not available
#' @param allow_interpolation Whether to interpolate missing values
#' @param data_quality_flags List of standardized data quality flags
#' @param offline_mode Whether to skip all downloads and use only cached data
#' @return A data frame with transportation data for all requested years
fetch_transportation_data <- function(years, 
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
  cache_file <- file.path(cache_dir, "transportation_data.rds")
  
  # Use cache if available and not refreshing
  if (!refresh_cache && file.exists(cache_file)) {
    print_msg("Loading cached transportation data...")
    transportation_data <- readRDS(cache_file)
    
    # Check if all requested years are in the cache
    cached_years <- unique(transportation_data$year)
    missing_years <- setdiff(years, cached_years)
    
    # Check for empty cache with just placeholder data
    if (nrow(transportation_data) <= 1 || 
        (is.data.frame(transportation_data) && "data_source" %in% names(transportation_data) && 
         any(grepl("SIMULATED", transportation_data$data_source)))) {
      print_msg("Cached transportation data appears to be empty or a placeholder. Will process files again.")
      # Force refresh by continuing past this point
    } else if (length(missing_years) == 0) {
      print_msg("Using complete cached transportation data.")
      return(transportation_data %>% filter(year %in% years))
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
  data_dir <- "data/transportation"
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
    print_msg(paste("Created transportation data directory at:", data_dir))
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
  
  # Function to get National Household Travel Survey data
  get_nhts_data <- function() {
    # NHTS is conducted periodically (2001, 2009, 2017)
    # We need to map these to the years in our range
    
    # Define the NHTS survey years
    nhts_years <- c(2001, 2009, 2017)
    
    # Map requested years to nearest NHTS survey
    year_mapping <- sapply(years, function(y) {
      nhts_years[which.min(abs(nhts_years - y))]
    })
    
    # Unique NHTS years needed
    unique_nhts_years <- unique(year_mapping)
    
    # NHTS data list to store results
    nhts_data_list <- list()
    
    # Process each NHTS survey year
    for (nhts_year in unique_nhts_years) {
      # Define file paths
      nhts_file <- file.path(data_dir, paste0("nhts_", nhts_year, ".csv"))
      
      # Check if we need to download
      need_download <- !file.exists(nhts_file) || refresh_cache
      
      if (need_download) {
        # NHTS data requires registration and download from their website
        # URLs change for each survey, so we'll provide placeholder URL structure
        nhts_url <- paste0(
          "https://nhts.ornl.gov/assets/", 
          nhts_year, 
          "/download/CountyLevel.csv"
        )
        
        # Try to download
        if (!safe_download(nhts_url, nhts_file, paste("NHTS data for", nhts_year))) {
          print_msg(paste("Could not download NHTS data for", nhts_year))
          # NHTS data typically requires registration and manual download
          # A real implementation would need to handle this differently
          next
        }
      } else {
        print_msg(paste("Using existing NHTS file for", nhts_year))
      }
      
      # Process the data if file exists
      if (file.exists(nhts_file)) {
        print_msg(paste("Reading NHTS data for", nhts_year))
        
        # Read the file
        tryCatch({
          nhts_data <- read_csv(nhts_file, show_col_types = FALSE)
          
          # Get column names
          print_msg(paste("NHTS data has", ncol(nhts_data), "columns and", nrow(nhts_data), "rows"))
          
          # Check for FIPS/GEOID column
          geoid_col <- grep("FIPS|fips|geoid|GEOID|county.*code|COUNTY.*CODE", names(nhts_data), value = TRUE)[1]
          
          if (is.na(geoid_col)) {
            print_msg("Could not identify GEOID column in NHTS data")
            next
          }
          
          # Rename and format GEOID
          nhts_data <- nhts_data %>%
            rename(GEOID = all_of(geoid_col)) %>%
            mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
          
          # Find columns for our variables of interest
          
          # Vehicle miles traveled
          vmt_col <- grep("vmt|VMT|vehicle.*mile|VEHICLE.*MILE", names(nhts_data), value = TRUE)[1]
          
          # Transportation cost burden
          cost_col <- grep("cost|Cost|COST|expense|Expense|EXPENSE", names(nhts_data), value = TRUE)[1]
          
          print_msg(paste("Found columns: VMT:", !is.na(vmt_col),
                        "Cost burden:", !is.na(cost_col)))
          
          # Create data frame for this survey year
          nhts_year_data <- data.frame(
            GEOID = nhts_data$GEOID
          )
          
          # Add vehicle miles traveled if available
          if (!is.na(vmt_col)) {
            nhts_year_data$vehicle_miles_traveled_per_capita <- nhts_data[[vmt_col]]
          }
          
          # Add transportation cost burden if available
          if (!is.na(cost_col)) {
            nhts_year_data$transportation_cost_burden_pct <- nhts_data[[cost_col]]
          }
          
          # Map this NHTS data to all corresponding years
          for (year in years[year_mapping == nhts_year]) {
            year_data <- nhts_year_data %>%
              mutate(year = year)
            
            # Add quality flags based on whether this is the exact NHTS year or interpolated
            quality_level <- if (year == nhts_year) data_quality_flags$direct else data_quality_flags$interpolated
            
            # Add quality flags for all variables
            if (!is.na(vmt_col)) {
              year_data$vehicle_miles_traveled_per_capita_data_quality <- quality_level
              year_data$vehicle_miles_traveled_per_capita_data_source <- "National Household Travel Survey"
              year_data$vehicle_miles_traveled_per_capita_data_vintage <- as.character(nhts_year)
            }
            
            if (!is.na(cost_col)) {
              year_data$transportation_cost_burden_pct_data_quality <- quality_level
              year_data$transportation_cost_burden_pct_data_source <- "National Household Travel Survey"
              year_data$transportation_cost_burden_pct_data_vintage <- as.character(nhts_year)
            }
            
            # Add to list
            nhts_data_list[[as.character(year)]] <- year_data
          }
          
          print_msg(paste("Processed NHTS data for", nhts_year))
        }, error = function(e) {
          print_msg(paste("Error reading NHTS data for", nhts_year, ":", conditionMessage(e)))
        })
      }
    }
    
    # Combine all years
    if (length(nhts_data_list) > 0) {
      combined_nhts <- bind_rows(nhts_data_list)
      print_msg(paste("Combined NHTS data with", nrow(combined_nhts), "rows"))
      return(combined_nhts)
    } else {
      print_msg("No NHTS data processed successfully")
      return(NULL)
    }
  }
  
  # Function to get All Transit Database data
  get_transit_data <- function() {
    # All Transit Database data is available from 2012 onwards
    # We'll try to get data for each year in the requested range
    
    # Define variables we want to extract
    transit_variables <- c(
      "transit_connectivity_index" = "Measure of transit connectivity",
      "transit_access_jobs" = "Number of jobs accessible by transit within 30 minutes",
      "transit_performance_index" = "Composite measure of transit performance"
    )
    
    # Transit data list to store results
    transit_data_list <- list()
    
    # Limit to years 2012 and later
    transit_years <- years[years >= 2012]
    
    for (year in transit_years) {
      # Skip future years
      if (year > as.integer(format(Sys.Date(), "%Y"))) {
        next
      }
      
      # Define file paths
      transit_file <- file.path(data_dir, paste0("transit_", year, ".csv"))
      
      # Check if we need to download
      need_download <- !file.exists(transit_file) || refresh_cache
      
      if (need_download) {
        # All Transit URL - placeholder structure
        transit_url <- paste0(
          "https://alltransit.cnt.org/data/download/counties_", 
          year, 
          ".csv"
        )
        
        # Try to download
        if (!safe_download(transit_url, transit_file, paste("All Transit data for", year))) {
          print_msg(paste("Could not download All Transit data for", year))
          # All Transit data might require registration
          next
        }
      } else {
        print_msg(paste("Using existing All Transit file for", year))
      }
      
      # Process the data if file exists
      if (file.exists(transit_file)) {
        print_msg(paste("Reading All Transit data for", year))
        
        # Read the file
        tryCatch({
          transit_data <- read_csv(transit_file, show_col_types = FALSE)
          
          # Get column names
          print_msg(paste("Transit data has", ncol(transit_data), "columns and", nrow(transit_data), "rows"))
          
          # Check for GEOID column
          geoid_col <- grep("FIPS|fips|geoid|GEOID|county.*code|COUNTY.*CODE", names(transit_data), value = TRUE)[1]
          
          if (is.na(geoid_col)) {
            print_msg("Could not identify GEOID column in Transit data")
            next
          }
          
          # Rename and format GEOID
          transit_data <- transit_data %>%
            rename(GEOID = all_of(geoid_col)) %>%
            mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
          
          # Find columns for our variables of interest
          
          # Connectivity index
          connectivity_col <- grep("connect|Connect|CONNECT", names(transit_data), value = TRUE)[1]
          
          # Jobs access
          jobs_col <- grep("jobs|Jobs|JOBS|employment|Employment", names(transit_data), value = TRUE)[1]
          
          # Performance index
          performance_col <- grep("perform|Perform|PERFORM", names(transit_data), value = TRUE)[1]
          
          print_msg(paste("Found columns: Connectivity:", !is.na(connectivity_col),
                        "Jobs access:", !is.na(jobs_col),
                        "Performance:", !is.na(performance_col)))
          
          # Create data frame for this year
          year_data <- data.frame(
            GEOID = transit_data$GEOID,
            year = year
          )
          
          # Add connectivity index if available
          if (!is.na(connectivity_col)) {
            year_data$transit_connectivity_index <- transit_data[[connectivity_col]]
            year_data$transit_connectivity_index_data_quality <- data_quality_flags$direct
            year_data$transit_connectivity_index_data_source <- "All Transit Database"
            year_data$transit_connectivity_index_data_vintage <- as.character(year)
          }
          
          # Add jobs access if available
          if (!is.na(jobs_col)) {
            year_data$transit_access_jobs <- transit_data[[jobs_col]]
            year_data$transit_access_jobs_data_quality <- data_quality_flags$direct
            year_data$transit_access_jobs_data_source <- "All Transit Database"
            year_data$transit_access_jobs_data_vintage <- as.character(year)
          }
          
          # Add performance index if available
          if (!is.na(performance_col)) {
            year_data$transit_performance_index <- transit_data[[performance_col]]
            year_data$transit_performance_index_data_quality <- data_quality_flags$direct
            year_data$transit_performance_index_data_source <- "All Transit Database"
            year_data$transit_performance_index_data_vintage <- as.character(year)
          }
          
          # Add to list
          transit_data_list[[as.character(year)]] <- year_data
          
          print_msg(paste("Processed All Transit data for", year))
        }, error = function(e) {
          print_msg(paste("Error reading All Transit data for", year, ":", conditionMessage(e)))
        })
      }
    }
    
    # Combine all years
    if (length(transit_data_list) > 0) {
      combined_transit <- bind_rows(transit_data_list)
      print_msg(paste("Combined All Transit data with", nrow(combined_transit), "rows"))
      return(combined_transit)
    } else {
      print_msg("No All Transit data processed successfully")
      return(NULL)
    }
  }
  
  # Function to get ACS transportation data
  get_acs_data <- function() {
    # ACS has data on zero-vehicle households and commute metrics
    # Available from 2005 onwards
    
    # Define variables we want to extract
    acs_variables <- c(
      "zero_vehicle_households_pct" = "Percentage of households with no vehicles",
      "public_transit_trips_per_capita" = "Public transit trips per capita"
    )
    
    # ACS data list to store results
    acs_data_list <- list()
    
    # Limit to years 2005 and later
    acs_years <- years[years >= 2005]
    
    for (year in acs_years) {
      # Skip future years
      if (year > as.integer(format(Sys.Date(), "%Y"))) {
        next
      }
      
      # Define file paths
      acs_file <- file.path(data_dir, paste0("acs_transportation_", year, ".csv"))
      
      # Check if we need to download
      need_download <- !file.exists(acs_file) || refresh_cache
      
      if (need_download) {
        # In a real implementation, we would use Census API
        # This would require a Census API key and proper queries
        print_msg(paste("ACS transportation data file not found for", year))
        # No automatic download option for ACS without API key
        next
      } else {
        print_msg(paste("Using existing ACS transportation file for", year))
      }
      
      # Process the data if file exists
      if (file.exists(acs_file)) {
        print_msg(paste("Reading ACS transportation data for", year))
        
        # Read the file
        tryCatch({
          acs_data <- read_csv(acs_file, show_col_types = FALSE)
          
          # Get column names
          print_msg(paste("ACS data has", ncol(acs_data), "columns and", nrow(acs_data), "rows"))
          
          # Check for GEOID column
          geoid_col <- grep("GEOID|geoid|fips|FIPS", names(acs_data), value = TRUE)[1]
          
          if (is.na(geoid_col)) {
            print_msg("Could not identify GEOID column in ACS data")
            next
          }
          
          # Rename and format GEOID
          acs_data <- acs_data %>%
            rename(GEOID = all_of(geoid_col)) %>%
            mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
          
          # Find columns for our variables of interest
          
          # Zero-vehicle households
          zero_vehicle_col <- grep("no.*vehicle|zero.*vehicle|without.*vehicle", 
                                  names(acs_data), value = TRUE)[1]
          
          # Public transit trips
          transit_trips_col <- grep("transit.*trip|public.*transport.*trip", 
                                   names(acs_data), value = TRUE)[1]
          
          print_msg(paste("Found columns: Zero-vehicle households:", !is.na(zero_vehicle_col),
                        "Transit trips:", !is.na(transit_trips_col)))
          
          # Create data frame for this year
          year_data <- data.frame(
            GEOID = acs_data$GEOID,
            year = year
          )
          
          # Add zero-vehicle households if available
          if (!is.na(zero_vehicle_col)) {
            year_data$zero_vehicle_households_pct <- acs_data[[zero_vehicle_col]]
            year_data$zero_vehicle_households_pct_data_quality <- data_quality_flags$direct
            year_data$zero_vehicle_households_pct_data_source <- "American Community Survey"
            year_data$zero_vehicle_households_pct_data_vintage <- as.character(year)
          }
          
          # Add public transit trips if available
          if (!is.na(transit_trips_col)) {
            year_data$public_transit_trips_per_capita <- acs_data[[transit_trips_col]]
            year_data$public_transit_trips_per_capita_data_quality <- data_quality_flags$direct
            year_data$public_transit_trips_per_capita_data_source <- "American Community Survey"
            year_data$public_transit_trips_per_capita_data_vintage <- as.character(year)
          }
          
          # Add to list
          acs_data_list[[as.character(year)]] <- year_data
          
          print_msg(paste("Processed ACS transportation data for", year))
        }, error = function(e) {
          print_msg(paste("Error reading ACS transportation data for", year, ":", conditionMessage(e)))
        })
      }
    }
    
    # Combine all years
    if (length(acs_data_list) > 0) {
      combined_acs <- bind_rows(acs_data_list)
      print_msg(paste("Combined ACS transportation data with", nrow(combined_acs), "rows"))
      return(combined_acs)
    } else {
      print_msg("No ACS transportation data processed successfully")
      return(NULL)
    }
  }
  
  # Get data from different transportation sources
  nhts_data <- get_nhts_data()
  transit_data <- get_transit_data()
  acs_data <- get_acs_data()
  
  # Combine all data sources
  transportation_data_list <- list()
  
  if (!is.null(nhts_data) && nrow(nhts_data) > 0) {
    transportation_data_list[["nhts"]] <- nhts_data
  }
  
  if (!is.null(transit_data) && nrow(transit_data) > 0) {
    transportation_data_list[["transit"]] <- transit_data
  }
  
  if (!is.null(acs_data) && nrow(acs_data) > 0) {
    transportation_data_list[["acs"]] <- acs_data
  }
  
  # Process if we have data
  if (length(transportation_data_list) > 0) {
    # Combine all sources and handle overlapping columns
    # Start with first dataset
    combined_transportation_data <- transportation_data_list[[1]]
    
    # Add each additional dataset
    if (length(transportation_data_list) > 1) {
      for (i in 2:length(transportation_data_list)) {
        next_data <- transportation_data_list[[i]]
        
        # Identify common columns for joining
        join_cols <- intersect(names(combined_transportation_data), names(next_data))
        
        # Ensure we have at least GEOID and year for joining
        if (all(c("GEOID", "year") %in% join_cols)) {
          # Find columns that might overlap (excluding join columns and flags)
          overlap_cols <- setdiff(
            intersect(names(combined_transportation_data), names(next_data)),
            c(join_cols, grep("_data_quality$|_data_source$|_data_vintage$", 
                             names(next_data), value = TRUE))
          )
          
          if (length(overlap_cols) > 0) {
            # For overlapping columns, prefer data from the current dataset
            # Remove those columns from the combined data before joining
            combined_transportation_data <- combined_transportation_data %>%
              select(-all_of(overlap_cols))
          }
          
          # Join with the next dataset
          combined_transportation_data <- full_join(
            combined_transportation_data, 
            next_data, 
            by = join_cols
          )
        } else {
          print_msg("Warning: Cannot join datasets - missing common keys")
        }
      }
    }
    
    # Check if we need to handle missing years
    available_years <- unique(combined_transportation_data$year)
    missing_years <- setdiff(years, available_years)
    
    if (length(missing_years) > 0 && allow_interpolation) {
      print_msg(paste("Interpolating data for missing years:", paste(missing_years, collapse=", ")))
      
      # Get all variables except join columns and metadata columns
      measure_cols <- setdiff(
        names(combined_transportation_data),
        c("GEOID", "year", grep("_data_quality$|_data_source$|_data_vintage$", 
                               names(combined_transportation_data), value = TRUE))
      )
      
      # Process each county separately for interpolation
      counties <- unique(combined_transportation_data$GEOID)
      
      # List to store interpolated data
      interp_data_list <- list()
      
      for (county in counties) {
        # Get data for this county
        county_data <- combined_transportation_data %>%
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
                         names(combined_transportation_data), value = TRUE)
        
        for (col in meta_cols) {
          if (!(col %in% names(interp_data))) {
            interp_data[[col]] <- NA
          }
        }
        
        # Use the interpolated data instead
        combined_transportation_data <- interp_data
      }
    } else if (length(missing_years) > 0) {
      print_msg("Missing years but interpolation not allowed. Years will remain missing.")
    }
    
    # Filter to just the requested years
    combined_transportation_data <- combined_transportation_data %>%
      filter(year %in% years)
    
    # Sort data
    combined_transportation_data <- combined_transportation_data %>%
      arrange(GEOID, year)
    
    # Cache the processed data
    saveRDS(combined_transportation_data, cache_file)
    print_msg(paste("Cached transportation data to:", cache_file))
    
    return(combined_transportation_data)
  } else if (allow_simulation) {
    # Create simulated data
    print_msg("No transportation data found. Creating simulated data...")
    
    # Transportation variables to simulate
    transportation_vars <- c(
      "vehicle_miles_traveled_per_capita" = "Annual vehicle miles traveled per capita",
      "transportation_cost_burden_pct" = "Transportation costs as percentage of household income",
      "zero_vehicle_households_pct" = "Percentage of households with no vehicles",
      "public_transit_trips_per_capita" = "Public transit trips per capita",
      "transit_connectivity_index" = "Measure of transit connectivity",
      "transit_access_jobs" = "Number of jobs accessible by transit within 30 minutes",
      "transit_performance_index" = "Composite measure of transit performance"
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
      for (var_name in names(transportation_vars)) {
        if (var_name == "vehicle_miles_traveled_per_capita") {
          # Typically 8,000-15,000 miles per year
          year_data[[var_name]] <- runif(nrow(year_data), 8000, 15000)
        } else if (var_name == "transportation_cost_burden_pct") {
          # Typically 10-25% of income
          year_data[[var_name]] <- runif(nrow(year_data), 10, 25)
        } else if (var_name == "zero_vehicle_households_pct") {
          # Typically 2-20% depending on urban/rural
          year_data[[var_name]] <- runif(nrow(year_data), 2, 20)
        } else if (var_name == "public_transit_trips_per_capita") {
          # Typically 0-100 trips per year, higher in urban areas
          year_data[[var_name]] <- runif(nrow(year_data), 0, 100)
        } else if (var_name == "transit_connectivity_index") {
          # Typically 0-10 scale
          year_data[[var_name]] <- runif(nrow(year_data), 0, 10)
        } else if (var_name == "transit_access_jobs") {
          # Typically 0-500,000 jobs
          year_data[[var_name]] <- runif(nrow(year_data), 0, 500000)
        } else if (var_name == "transit_performance_index") {
          # Typically 0-100 scale
          year_data[[var_name]] <- runif(nrow(year_data), 0, 100)
        } else {
          # Default - 0-100 range
          year_data[[var_name]] <- runif(nrow(year_data), 0, 100)
        }
        
        # Add quality flags
        year_data[[paste0(var_name, "_data_quality")]] <- data_quality_flags$simulated
        year_data[[paste0(var_name, "_data_source")]] <- "SIMULATED Transportation Data"
        year_data[[paste0(var_name, "_data_vintage")]] <- paste0("simulated_", year)
      }
      
      sim_data_list[[as.character(year)]] <- year_data
    }
    
    # Combine all years
    simulated_data <- bind_rows(sim_data_list)
    
    # Cache the simulated data
    saveRDS(simulated_data, cache_file)
    print_msg(paste("Cached simulated transportation data to:", cache_file))
    
    return(simulated_data)
  } else {
    # No data and simulation not allowed - create empty dataset with NAs
    print_msg("No transportation data available and simulation not allowed. Creating empty dataset with NAs.")
    
    # Get variable list for transportation variables
    transportation_vars <- c(
      "vehicle_miles_traveled_per_capita",
      "transportation_cost_burden_pct",
      "zero_vehicle_households_pct",
      "public_transit_trips_per_capita",
      "transit_connectivity_index",
      "transit_access_jobs",
      "transit_performance_index"
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
    
    # Add NAME column
    grid$NAME <- counties$NAME[match(grid$GEOID, counties$GEOID)]
    
    # Add empty variable columns with NAs
    for (var in transportation_vars) {
      grid[[var]] <- NA_real_
      grid[[paste0(var, "_data_quality")]] <- data_quality_flags$missing
      grid[[paste0(var, "_data_source")]] <- "NOT_AVAILABLE"
      grid[[paste0(var, "_data_vintage")]] <- NA_character_
    }
    
    transportation_data <- as_tibble(grid)
    print_msg(paste("Created empty transportation dataset with", nrow(transportation_data), "rows"))
    
    # Cache the empty data
    saveRDS(transportation_data, cache_file)
    print_msg(paste("Cached empty transportation data to:", cache_file))
    
    return(transportation_data)
  }
}

# This lets the function be used when the script is sourced
is_sourced <- function() {
  return(!identical(environment(), globalenv()))
}

# If the script is run directly, test the function
if (!is_sourced()) {
  cat("Testing transportation data fetcher...\n")
  
  # Get current year
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  
  # Test for last 5 years
  test_years <- (current_year-4):current_year
  
  # Test the function
  result <- fetch_transportation_data(
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