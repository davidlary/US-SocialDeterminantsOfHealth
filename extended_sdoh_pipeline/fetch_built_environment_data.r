#!/usr/bin/env Rscript

# Built Environment Data Fetcher
# This script handles retrieval of built environment data from EPA Smart Location Database
# and Trust for Public Land ParkScore

library(tidyverse)
library(httr)
library(jsonlite)
library(readxl)
library(lubridate)
library(sf)
library(zoo) # For interpolation if needed

#' Fetch built environment data
#'
#' Retrieves built environment data from EPA Smart Location Database and
#' Trust for Public Land ParkScore for walkability and park access metrics at the county level.
#'
#' @param years Vector of years to include
#' @param cache_dir Directory to store cache files
#' @param refresh_cache Whether to refresh the cache
#' @param allow_simulation Whether to generate simulated data if real data not available
#' @param allow_interpolation Whether to interpolate missing years
#' @param data_quality_flags List with standardized data quality flags
#' @param offline_mode Whether to operate in offline mode (no downloads)
#' @return A data frame with built environment data for all requested years
fetch_built_environment_data <- function(years, 
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
    is_interactive_run <- !exists("is_sourced") || (exists("is_sourced") && !is_sourced())
    if (is_interactive_run) {
      message(msg)
    } else {
      cat(msg, "\n")
    }
  }
  
  # Define cache file
  cache_file <- file.path(cache_dir, "built_environment_data.rds")
  
  # Use cache if available and not refreshing
  if (!refresh_cache && file.exists(cache_file)) {
    print_msg("Loading cached built environment data...")
    built_env_data <- readRDS(cache_file)
    
    # Check if all requested years are in the cache
    cached_years <- unique(built_env_data$year)
    missing_years <- setdiff(years, cached_years)
    
    # Check for empty cache with just placeholder data
    if (nrow(built_env_data) <= 1 || 
        (is.data.frame(built_env_data) && "data_source" %in% names(built_env_data) && 
         any(grepl("SIMULATED", built_env_data$data_source)))) {
      print_msg("Cached built environment data appears to be empty or a placeholder. Will process files again.")
      # Force refresh by continuing past this point
    } else if (length(missing_years) == 0) {
      print_msg("Using complete cached built environment data.")
      return(built_env_data %>% filter(year %in% years))
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
  data_dir <- "data/built_environment"
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
    print_msg(paste("Created built environment data directory at:", data_dir))
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
  
  # Function to get EPA Smart Location Database data
  get_sld_data <- function() {
    # EPA SLD has data for 2010, 2013, and 2021
    
    # Define SLD years available
    sld_years <- c(2010, 2013, 2021)
    
    # Define variables we want to extract
    sld_variables <- c(
      "walkability_index" = "County-level walkability score",
      "land_use_diversity" = "Mix of land uses (entropy index)",
      "street_intersection_density" = "Number of intersections per square mile",
      "employment_access_index" = "Access to employment centers",
      "transit_service_density" = "Transit routes and stops per square mile",
      "housing_density" = "Housing units per acre of developed land"
    )
    
    # Map requested years to nearest SLD year
    year_mapping <- sapply(years, function(y) {
      sld_years[which.min(abs(sld_years - y))]
    })
    
    # Unique SLD years needed
    unique_sld_years <- unique(year_mapping)
    
    # SLD data list to store results
    sld_data_list <- list()
    
    # Process each SLD version
    for (sld_year in unique_sld_years) {
      # Define file paths
      sld_file <- file.path(data_dir, paste0("epa_sld_", sld_year, ".csv"))
      
      # Check if we need to download
      need_download <- !file.exists(sld_file) || refresh_cache
      
      if (need_download) {
        # EPA SLD URL (placeholder - actual URL would be from their site)
        sld_url <- paste0(
          "https://www.epa.gov/sites/default/files/smart_location_db_", 
          sld_year, 
          "_county.csv"
        )
        
        # Try to download
        if (!safe_download(sld_url, sld_file, paste("EPA SLD data for", sld_year))) {
          print_msg(paste("Could not download EPA SLD data for", sld_year))
          next
        }
      } else {
        print_msg(paste("Using existing EPA SLD file for", sld_year))
      }
      
      # Process the data if file exists
      if (file.exists(sld_file)) {
        print_msg(paste("Reading EPA SLD data for", sld_year))
        
        # Read the file
        tryCatch({
          sld_data <- read_csv(sld_file, show_col_types = FALSE)
          
          # Get column names
          print_msg(paste("SLD data has", ncol(sld_data), "columns and", nrow(sld_data), "rows"))
          
          # Check for FIPS/GEOID column
          geoid_col <- grep("FIPS|fips|geoid|GEOID|county.*id|COUNTY.*ID", 
                           names(sld_data), value = TRUE)[1]
          
          if (is.na(geoid_col)) {
            print_msg("Could not identify GEOID column in SLD data")
            next
          }
          
          # Rename and format GEOID
          sld_data <- sld_data %>%
            rename(GEOID = all_of(geoid_col)) %>%
            mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
          
          # Find columns for our variables of interest
          
          # Walkability index
          walkability_col <- grep("walk.*index|walk.*score", 
                                 names(sld_data), value = TRUE)[1]
          
          # Land use diversity
          diversity_col <- grep("divers.*index|land.*mix|entropy", 
                               names(sld_data), value = TRUE)[1]
          
          # Street intersection density
          intersection_col <- grep("intersect.*dens|node.*dens", 
                                  names(sld_data), value = TRUE)[1]
          
          # Employment access
          employment_col <- grep("emp.*access|job.*access", 
                                names(sld_data), value = TRUE)[1]
          
          # Transit service density
          transit_col <- grep("transit.*dens|transit.*service", 
                             names(sld_data), value = TRUE)[1]
          
          # Housing density
          housing_col <- grep("housing.*dens|house.*dens|dwelling.*dens", 
                             names(sld_data), value = TRUE)[1]
          
          print_msg(paste("Found columns: Walkability:", !is.na(walkability_col),
                        "Land use diversity:", !is.na(diversity_col),
                        "Intersection density:", !is.na(intersection_col),
                        "Employment access:", !is.na(employment_col),
                        "Transit density:", !is.na(transit_col),
                        "Housing density:", !is.na(housing_col)))
          
          # Map this SLD data to all corresponding years
          for (year in years[year_mapping == sld_year]) {
            year_data <- data.frame(
              GEOID = sld_data$GEOID,
              year = year
            )
            
            # Add quality flags based on whether this is the exact SLD year or interpolated
            quality_level <- if (year == sld_year) {
              data_quality_flags$direct
            } else if (allow_interpolation) {
              data_quality_flags$interpolated
            } else {
              data_quality_flags$missing
            }
            
            # Add walkability index if available
            if (!is.na(walkability_col) && (quality_level != data_quality_flags$missing)) {
              year_data$walkability_index <- sld_data[[walkability_col]]
              year_data$walkability_index_data_quality <- quality_level
              year_data$walkability_index_data_source <- "EPA Smart Location Database"
              year_data$walkability_index_data_vintage <- as.character(sld_year)
            } else if (!is.na(walkability_col)) {
              year_data$walkability_index <- NA_real_
              year_data$walkability_index_data_quality <- data_quality_flags$missing
              year_data$walkability_index_data_source <- "NOT_AVAILABLE"
              year_data$walkability_index_data_vintage <- NA_character_
            }
            
            # Add land use diversity if available
            if (!is.na(diversity_col) && (quality_level != data_quality_flags$missing)) {
              year_data$land_use_diversity <- sld_data[[diversity_col]]
              year_data$land_use_diversity_data_quality <- quality_level
              year_data$land_use_diversity_data_source <- "EPA Smart Location Database"
              year_data$land_use_diversity_data_vintage <- as.character(sld_year)
            } else if (!is.na(diversity_col)) {
              year_data$land_use_diversity <- NA_real_
              year_data$land_use_diversity_data_quality <- data_quality_flags$missing
              year_data$land_use_diversity_data_source <- "NOT_AVAILABLE"
              year_data$land_use_diversity_data_vintage <- NA_character_
            }
            
            # Add street intersection density if available
            if (!is.na(intersection_col) && (quality_level != data_quality_flags$missing)) {
              year_data$street_intersection_density <- sld_data[[intersection_col]]
              year_data$street_intersection_density_data_quality <- quality_level
              year_data$street_intersection_density_data_source <- "EPA Smart Location Database"
              year_data$street_intersection_density_data_vintage <- as.character(sld_year)
            } else if (!is.na(intersection_col)) {
              year_data$street_intersection_density <- NA_real_
              year_data$street_intersection_density_data_quality <- data_quality_flags$missing
              year_data$street_intersection_density_data_source <- "NOT_AVAILABLE"
              year_data$street_intersection_density_data_vintage <- NA_character_
            }
            
            # Add employment access if available
            if (!is.na(employment_col) && (quality_level != data_quality_flags$missing)) {
              year_data$employment_access_index <- sld_data[[employment_col]]
              year_data$employment_access_index_data_quality <- quality_level
              year_data$employment_access_index_data_source <- "EPA Smart Location Database"
              year_data$employment_access_index_data_vintage <- as.character(sld_year)
            } else if (!is.na(employment_col)) {
              year_data$employment_access_index <- NA_real_
              year_data$employment_access_index_data_quality <- data_quality_flags$missing
              year_data$employment_access_index_data_source <- "NOT_AVAILABLE"
              year_data$employment_access_index_data_vintage <- NA_character_
            }
            
            # Add transit service density if available
            if (!is.na(transit_col) && (quality_level != data_quality_flags$missing)) {
              year_data$transit_service_density <- sld_data[[transit_col]]
              year_data$transit_service_density_data_quality <- quality_level
              year_data$transit_service_density_data_source <- "EPA Smart Location Database"
              year_data$transit_service_density_data_vintage <- as.character(sld_year)
            } else if (!is.na(transit_col)) {
              year_data$transit_service_density <- NA_real_
              year_data$transit_service_density_data_quality <- data_quality_flags$missing
              year_data$transit_service_density_data_source <- "NOT_AVAILABLE"
              year_data$transit_service_density_data_vintage <- NA_character_
            }
            
            # Add housing density if available
            if (!is.na(housing_col) && (quality_level != data_quality_flags$missing)) {
              year_data$housing_density <- sld_data[[housing_col]]
              year_data$housing_density_data_quality <- quality_level
              year_data$housing_density_data_source <- "EPA Smart Location Database"
              year_data$housing_density_data_vintage <- as.character(sld_year)
            } else if (!is.na(housing_col)) {
              year_data$housing_density <- NA_real_
              year_data$housing_density_data_quality <- data_quality_flags$missing
              year_data$housing_density_data_source <- "NOT_AVAILABLE"
              year_data$housing_density_data_vintage <- NA_character_
            }
            
            # Add to list
            sld_data_list[[as.character(year)]] <- year_data
          }
          
          print_msg(paste("Processed EPA SLD data for", sld_year))
        }, error = function(e) {
          print_msg(paste("Error reading EPA SLD data for", sld_year, ":", conditionMessage(e)))
        })
      }
    }
    
    # Combine all years
    if (length(sld_data_list) > 0) {
      combined_sld <- bind_rows(sld_data_list)
      print_msg(paste("Combined EPA SLD data with", nrow(combined_sld), "rows"))
      return(combined_sld)
    } else {
      print_msg("No EPA SLD data processed successfully")
      return(NULL)
    }
  }
  
  # Function to get Trust for Public Land ParkScore data
  get_parkscore_data <- function() {
    # ParkScore has data from 2012 onwards
    
    # Define variables we want to extract
    park_variables <- c(
      "park_access_pct" = "Percentage of residents living within 10-minute walk of a park",
      "park_acres_per_1000" = "Park acres per 1,000 residents",
      "park_spending_per_capita" = "Park system spending per resident",
      "playgrounds_per_10000" = "Playgrounds per 10,000 residents"
    )
    
    # ParkScore data list to store results
    park_data_list <- list()
    
    # Limit to years 2012 and later
    park_years <- years[years >= 2012]
    
    for (year in park_years) {
      # Skip future years
      if (year > as.integer(format(Sys.Date(), "%Y"))) {
        next
      }
      
      # Define file paths
      park_file <- file.path(data_dir, paste0("parkscore_", year, ".csv"))
      
      # Check if we need to download
      need_download <- !file.exists(park_file) || refresh_cache
      
      if (need_download) {
        # ParkScore URL (placeholder - actual URL would be from their site)
        park_url <- paste0(
          "https://www.tpl.org/sites/default/files/parkscore_", 
          year, 
          "_county.csv"
        )
        
        # Try to download
        if (!safe_download(park_url, park_file, paste("ParkScore data for", year))) {
          print_msg(paste("Could not download ParkScore data for", year))
          next
        }
      } else {
        print_msg(paste("Using existing ParkScore file for", year))
      }
      
      # Process the data if file exists
      if (file.exists(park_file)) {
        print_msg(paste("Reading ParkScore data for", year))
        
        # Read the file
        tryCatch({
          park_data <- read_csv(park_file, show_col_types = FALSE)
          
          # Get column names
          print_msg(paste("ParkScore data has", ncol(park_data), "columns and", nrow(park_data), "rows"))
          
          # Check for FIPS/GEOID column
          geoid_col <- grep("FIPS|fips|geoid|GEOID|county.*id|COUNTY.*ID", 
                           names(park_data), value = TRUE)[1]
          
          if (is.na(geoid_col)) {
            print_msg("Could not identify GEOID column in ParkScore data")
            next
          }
          
          # Rename and format GEOID
          park_data <- park_data %>%
            rename(GEOID = all_of(geoid_col)) %>%
            mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
          
          # Find columns for our variables of interest
          
          # Park access
          access_col <- grep("access|walk.*time|proximity", 
                            names(park_data), value = TRUE)[1]
          
          # Park acres
          acres_col <- grep("acres|acreage|area", 
                           names(park_data), value = TRUE)[1]
          
          # Park spending
          spending_col <- grep("spend|expenditure|budget", 
                              names(park_data), value = TRUE)[1]
          
          # Playgrounds
          playground_col <- grep("playground|play.*ground|recreation", 
                                names(park_data), value = TRUE)[1]
          
          print_msg(paste("Found columns: Park access:", !is.na(access_col),
                        "Park acres:", !is.na(acres_col),
                        "Park spending:", !is.na(spending_col),
                        "Playgrounds:", !is.na(playground_col)))
          
          # Create data frame for this year
          year_data <- data.frame(
            GEOID = park_data$GEOID,
            year = year
          )
          
          # Add park access if available
          if (!is.na(access_col)) {
            year_data$park_access_pct <- park_data[[access_col]]
            year_data$park_access_pct_data_quality <- data_quality_flags$direct
            year_data$park_access_pct_data_source <- "Trust for Public Land ParkScore"
            year_data$park_access_pct_data_vintage <- as.character(year)
          }
          
          # Add park acres if available
          if (!is.na(acres_col)) {
            year_data$park_acres_per_1000 <- park_data[[acres_col]]
            year_data$park_acres_per_1000_data_quality <- data_quality_flags$direct
            year_data$park_acres_per_1000_data_source <- "Trust for Public Land ParkScore"
            year_data$park_acres_per_1000_data_vintage <- as.character(year)
          }
          
          # Add park spending if available
          if (!is.na(spending_col)) {
            year_data$park_spending_per_capita <- park_data[[spending_col]]
            year_data$park_spending_per_capita_data_quality <- data_quality_flags$direct
            year_data$park_spending_per_capita_data_source <- "Trust for Public Land ParkScore"
            year_data$park_spending_per_capita_data_vintage <- as.character(year)
          }
          
          # Add playgrounds if available
          if (!is.na(playground_col)) {
            year_data$playgrounds_per_10000 <- park_data[[playground_col]]
            year_data$playgrounds_per_10000_data_quality <- data_quality_flags$direct
            year_data$playgrounds_per_10000_data_source <- "Trust for Public Land ParkScore"
            year_data$playgrounds_per_10000_data_vintage <- as.character(year)
          }
          
          # Add to list
          park_data_list[[as.character(year)]] <- year_data
          
          print_msg(paste("Processed ParkScore data for", year))
        }, error = function(e) {
          print_msg(paste("Error reading ParkScore data for", year, ":", conditionMessage(e)))
        })
      }
    }
    
    # Combine all years
    if (length(park_data_list) > 0) {
      combined_park <- bind_rows(park_data_list)
      print_msg(paste("Combined ParkScore data with", nrow(combined_park), "rows"))
      return(combined_park)
    } else {
      print_msg("No ParkScore data processed successfully")
      return(NULL)
    }
  }
  
  # Get data from different sources
  sld_data <- get_sld_data()
  park_data <- get_parkscore_data()
  
  # Combine all data sources
  built_env_data_list <- list()
  
  if (!is.null(sld_data) && nrow(sld_data) > 0) {
    built_env_data_list[["sld"]] <- sld_data
  }
  
  if (!is.null(park_data) && nrow(park_data) > 0) {
    built_env_data_list[["park"]] <- park_data
  }
  
  # Process if we have data
  if (length(built_env_data_list) > 0) {
    # Combine all sources and handle overlapping columns
    # Start with first dataset
    combined_built_env_data <- built_env_data_list[[1]]
    
    # Add each additional dataset
    if (length(built_env_data_list) > 1) {
      for (i in 2:length(built_env_data_list)) {
        next_data <- built_env_data_list[[i]]
        
        # Identify common columns for joining
        join_cols <- intersect(names(combined_built_env_data), names(next_data))
        
        # Ensure we have at least GEOID and year for joining
        if (all(c("GEOID", "year") %in% join_cols)) {
          # Find columns that might overlap (excluding join columns and flags)
          overlap_cols <- setdiff(
            intersect(names(combined_built_env_data), names(next_data)),
            c(join_cols, grep("_data_quality$|_data_source$|_data_vintage$", 
                             names(next_data), value = TRUE))
          )
          
          if (length(overlap_cols) > 0) {
            # For overlapping columns, prefer data from the current dataset
            # Remove those columns from the combined data before joining
            combined_built_env_data <- combined_built_env_data %>%
              select(-all_of(overlap_cols))
          }
          
          # Join with the next dataset
          combined_built_env_data <- full_join(
            combined_built_env_data, 
            next_data, 
            by = join_cols
          )
        } else {
          print_msg("Warning: Cannot join datasets - missing common keys")
        }
      }
    }
    
    # Check if we need to handle missing years
    available_years <- unique(combined_built_env_data$year)
    missing_years <- setdiff(years, available_years)
    
    if (length(missing_years) > 0 && allow_interpolation) {
      print_msg(paste("Need to handle missing years:", paste(missing_years, collapse=", ")))
      
      # Get unique county GEOIDs
      geoids <- unique(combined_built_env_data$GEOID)
      
      # For missing years, properly interpolate data
      missing_data_list <- list()
      
      # Identify variables to interpolate (exclude metadata columns)
      measure_cols <- setdiff(
        names(combined_built_env_data),
        c("GEOID", "year", grep("_data_quality$|_data_source$|_data_vintage$", 
                               names(combined_built_env_data), value = TRUE))
      )
      
      for (current_geoid in geoids) {
        # Get data for this county
        county_data <- combined_built_env_data %>%
          filter(GEOID == current_geoid) %>%
          arrange(year)
        
        # Create a grid with all required years for this county
        county_grid <- data.frame(
          GEOID = current_geoid,
          year = sort(unique(c(county_data$year, missing_years)))
        )
        
        # For each measure column, interpolate values
        for (col in measure_cols) {
          # Get existing values for this column
          existing_values <- county_data[[col]]
          existing_years <- county_data$year
          
          # Skip if all NA
          if (all(is.na(existing_values))) {
            county_grid[[col]] <- NA_real_
            next
          }
          
          # Remove NA values for interpolation
          valid_idx <- !is.na(existing_values)
          y_valid <- existing_values[valid_idx]
          x_valid <- existing_years[valid_idx]
          
          if (length(y_valid) >= 2) {
            # Use approx for interpolation
            interp_result <- approx(x_valid, y_valid, xout = county_grid$year, rule = 1)
            county_grid[[col]] <- interp_result$y
            
            # Add quality flags
            quality_col <- paste0(col, "_data_quality")
            source_col <- paste0(col, "_data_source")
            vintage_col <- paste0(col, "_data_vintage")
            
            # Create quality flags
            county_grid[[quality_col]] <- NA
            county_grid[[source_col]] <- NA
            county_grid[[vintage_col]] <- NA
            
            # For each year in the grid, determine if it's direct, interpolated, or extrapolated
            for (i in 1:nrow(county_grid)) {
              current_year <- county_grid$year[i]
              
              if (current_year %in% existing_years) {
                # This is direct data, get original quality flag
                original_idx <- which(county_data$year == current_year)
                county_grid[i, quality_col] <- county_data[original_idx, quality_col]
                county_grid[i, source_col] <- county_data[original_idx, source_col]
                county_grid[i, vintage_col] <- county_data[original_idx, vintage_col]
              } else if (current_year > max(x_valid) || current_year < min(x_valid)) {
                # This is extrapolated data
                county_grid[i, quality_col] <- data_quality_flags$extrapolated
                county_grid[i, source_col] <- paste0("Extrapolated from ", 
                                                   ifelse(current_year < min(x_valid), min(x_valid), max(x_valid)))
                county_grid[i, vintage_col] <- paste0("extrapolated_from_", 
                                                    ifelse(current_year < min(x_valid), min(x_valid), max(x_valid)))
              } else {
                # This is interpolated data
                county_grid[i, quality_col] <- data_quality_flags$interpolated
                
                # Find surrounding years
                lower_year <- max(x_valid[x_valid <= current_year])
                upper_year <- min(x_valid[x_valid >= current_year])
                
                county_grid[i, source_col] <- paste0("Interpolated between ", lower_year, " and ", upper_year)
                county_grid[i, vintage_col] <- paste0("interpolated_", lower_year, "_", upper_year)
              }
            }
          } else if (length(y_valid) == 1) {
            # Only one value available, use it for all years
            county_grid[[col]] <- y_valid[1]
            
            # Add quality flags
            quality_col <- paste0(col, "_data_quality")
            source_col <- paste0(col, "_data_source")
            vintage_col <- paste0(col, "_data_vintage")
            
            # For each year, set flag
            for (i in 1:nrow(county_grid)) {
              current_year <- county_grid$year[i]
              
              if (current_year == x_valid) {
                # Direct value
                original_idx <- which(county_data$year == current_year)
                county_grid[i, quality_col] <- county_data[original_idx, quality_col]
                county_grid[i, source_col] <- county_data[original_idx, source_col]
                county_grid[i, vintage_col] <- county_data[original_idx, vintage_col]
              } else {
                # Extended value
                county_grid[i, quality_col] <- data_quality_flags$extrapolated
                county_grid[i, source_col] <- paste0("Extended from ", x_valid)
                county_grid[i, vintage_col] <- paste0("extended_from_", x_valid)
              }
            }
          }
        }
        
        # Extract only missing years data
        missing_county_data <- county_grid %>%
          filter(year %in% missing_years)
        
        # Add to list
        for (missing_year in missing_years) {
          year_data <- missing_county_data %>%
            filter(year == missing_year)
          
          if (nrow(year_data) > 0) {
            if (!as.character(missing_year) %in% names(missing_data_list)) {
              missing_data_list[[as.character(missing_year)]] <- year_data
            } else {
              missing_data_list[[as.character(missing_year)]] <- bind_rows(
                missing_data_list[[as.character(missing_year)]],
                year_data
              )
            }
          }
        }
      }
      
      # Combine with original data
      if (length(missing_data_list) > 0) {
        missing_data <- bind_rows(missing_data_list)
        combined_built_env_data <- bind_rows(combined_built_env_data, missing_data)
      }
    }
    
    # Filter to just the requested years
    combined_built_env_data <- combined_built_env_data %>%
      filter(year %in% years)
    
    # Sort data
    combined_built_env_data <- combined_built_env_data %>%
      arrange(GEOID, year)
    
    # Cache the processed data
    saveRDS(combined_built_env_data, cache_file)
    print_msg(paste("Cached built environment data to:", cache_file))
    
    return(combined_built_env_data)
  } else if (allow_simulation) {
    # Create simulated data
    print_msg("No built environment data found. Creating simulated data...")
    
    # Built environment variables to simulate
    built_env_vars <- c(
      "walkability_index" = "County-level walkability score",
      "land_use_diversity" = "Mix of land uses (entropy index)",
      "street_intersection_density" = "Number of intersections per square mile",
      "employment_access_index" = "Access to employment centers",
      "transit_service_density" = "Transit routes and stops per square mile",
      "housing_density" = "Housing units per acre of developed land",
      "park_access_pct" = "Percentage of residents living within 10-minute walk of a park",
      "park_acres_per_1000" = "Park acres per 1,000 residents",
      "park_spending_per_capita" = "Park system spending per resident",
      "playgrounds_per_10000" = "Playgrounds per 10,000 residents"
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
      for (var_name in names(built_env_vars)) {
        if (var_name == "walkability_index") {
          # Typically 0-100 scale
          year_data[[var_name]] <- runif(nrow(year_data), 0, 100)
        } else if (var_name == "land_use_diversity") {
          # Typically 0-1 scale (entropy index)
          year_data[[var_name]] <- runif(nrow(year_data), 0, 1)
        } else if (var_name == "street_intersection_density") {
          # Typically 50-500 intersections per square mile
          year_data[[var_name]] <- runif(nrow(year_data), 50, 500)
        } else if (var_name == "employment_access_index") {
          # Typically 0-10 scale
          year_data[[var_name]] <- runif(nrow(year_data), 0, 10)
        } else if (var_name == "transit_service_density") {
          # Typically 0-50 routes/stops per square mile
          year_data[[var_name]] <- runif(nrow(year_data), 0, 50)
        } else if (var_name == "housing_density") {
          # Typically 0.5-20 units per acre
          year_data[[var_name]] <- runif(nrow(year_data), 0.5, 20)
        } else if (var_name == "park_access_pct") {
          # Typically 20-90%
          year_data[[var_name]] <- runif(nrow(year_data), 20, 90)
        } else if (var_name == "park_acres_per_1000") {
          # Typically 5-50 acres per 1,000 residents
          year_data[[var_name]] <- runif(nrow(year_data), 5, 50)
        } else if (var_name == "park_spending_per_capita") {
          # Typically $20-$200 per resident
          year_data[[var_name]] <- runif(nrow(year_data), 20, 200)
        } else if (var_name == "playgrounds_per_10000") {
          # Typically 1-10 playgrounds per 10,000 residents
          year_data[[var_name]] <- runif(nrow(year_data), 1, 10)
        } else {
          # Default - 0-100 range
          year_data[[var_name]] <- runif(nrow(year_data), 0, 100)
        }
        
        # Add quality flags
        year_data[[paste0(var_name, "_data_quality")]] <- data_quality_flags$simulated
        year_data[[paste0(var_name, "_data_source")]] <- "SIMULATED Built Environment Data"
        year_data[[paste0(var_name, "_data_vintage")]] <- paste0("simulated_", year)
      }
      
      sim_data_list[[as.character(year)]] <- year_data
    }
    
    # Combine all years
    simulated_data <- bind_rows(sim_data_list)
    
    # Cache the simulated data
    saveRDS(simulated_data, cache_file)
    print_msg(paste("Cached simulated built environment data to:", cache_file))
    
    return(simulated_data)
  } else {
    # No data and simulation not allowed - create empty dataset with NAs
    print_msg("No built environment data available. Creating empty dataset with NAs since simulation not allowed...")
    
    # Built environment variables we would have included
    built_env_vars <- c(
      "walkability_index",
      "land_use_diversity",
      "street_intersection_density",
      "employment_access_index",
      "transit_service_density",
      "housing_density",
      "park_access_pct",
      "park_acres_per_1000",
      "park_spending_per_capita",
      "playgrounds_per_10000"
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
      for (var_name in built_env_vars) {
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
    print_msg(paste("Cached empty built environment data to:", cache_file))
    
    return(empty_data)
  }
}

# This lets the function be used when the script is sourced
is_sourced <- function() {
  return(!identical(environment(), globalenv()))
}

# If the script is run directly, test the function
if (!is_sourced()) {
  cat("Testing built environment data fetcher...\n")
  
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
  result_sim <- fetch_built_environment_data(
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
    
    for (qcol in quality_cols) {
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
  result_interp <- fetch_built_environment_data(
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
    fetch_built_environment_data(
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
  result_offline <- fetch_built_environment_data(
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