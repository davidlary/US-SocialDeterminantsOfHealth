#!/usr/bin/env Rscript

# Housing Data Fetcher
# This script handles retrieval of housing data from HUD CHAS, Eviction Lab, and other sources

library(tidyverse)
library(httr)
library(jsonlite)
library(readxl)
library(lubridate)
library(sf)
library(zoo) # For interpolation if needed

#' Fetch housing data
#'
#' Retrieves housing data from HUD CHAS, Eviction Lab, and Federal Reserve HMDA
#' for cost burden, evictions, and mortgage characteristics at the county level.
#'
#' @param years Vector of years to include
#' @param cache_dir Directory to store cache files
#' @param refresh_cache Whether to refresh the cache
#' @param allow_simulation Whether to generate simulated data if real data not available
#' @param allow_interpolation Whether to interpolate missing years from available data
#' @param data_quality_flags List of flags for data quality tracking
#' @param offline_mode If TRUE, will only use cached data without attempting downloads
#' @return A data frame with housing data for all requested years
fetch_housing_data <- function(years, 
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
  cache_file <- file.path(cache_dir, "housing_data.rds")
  
  # Use cache if available and not refreshing
  if (!refresh_cache && file.exists(cache_file)) {
    print_msg("Loading cached housing data...")
    housing_data <- readRDS(cache_file)
    
    # Check if all requested years are in the cache
    cached_years <- unique(housing_data$year)
    missing_years <- setdiff(years, cached_years)
    
    # Check for empty cache with just placeholder data
    if (nrow(housing_data) <= 1 || 
        (is.data.frame(housing_data) && "data_source" %in% names(housing_data) && 
         any(grepl("SIMULATED", housing_data$data_source)))) {
      print_msg("Cached housing data appears to be empty or a placeholder. Will process files again.")
      # Force refresh by continuing past this point
    } else if (length(missing_years) == 0) {
      print_msg("Using complete cached housing data.")
      return(housing_data %>% filter(year %in% years))
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
  data_dir <- "data/housing"
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
    print_msg(paste("Created housing data directory at:", data_dir))
  }
  
  # Helper function to safely download and read files
  safe_download <- function(url, destfile, description) {
    # Skip download if in offline mode
    if (offline_mode) {
      print_msg(paste("Skipping download of", description, "(offline mode)"))
      return(FALSE)
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
  
  # Function to get HUD CHAS data
  get_chas_data <- function() {
    # HUD CHAS data is available for years from around 2006 to present
    # Each release covers a 5-year ACS period (e.g., 2013-2017)
    # We need to map these to single years for our dataset
    
    # Define the variables we want from CHAS
    chas_variables <- c(
      "severely_cost_burdened_owners_pct" = "% Owner HH with severe cost burden (>50% of income)",
      "severely_cost_burdened_renters_pct" = "% Renter HH with severe cost burden (>50% of income)",
      "low_income_renters_affordable_units_ratio" = "Ratio of affordable units to low-income renters",
      "housing_problems_pct" = "% HH with at least one housing problem",
      "overcrowded_housing_pct" = "% Occupied units with > 1 person per room"
    )
    
    # Map CHAS 5-year periods to single years in our dataset
    # We'll use the last year of each period as the reference
    chas_periods <- list(
      "2006-2010" = 2010,
      "2007-2011" = 2011,
      "2008-2012" = 2012,
      "2009-2013" = 2013,
      "2010-2014" = 2014,
      "2011-2015" = 2015,
      "2012-2016" = 2016,
      "2013-2017" = 2017,
      "2014-2018" = 2018,
      "2015-2019" = 2019,
      "2016-2020" = 2020
    )
    
    # Find which periods we need to cover our requested years
    target_periods <- names(chas_periods)[chas_periods %in% years]
    
    # CHAS data list to store results
    chas_data_list <- list()
    
    # Process each period
    for (period in target_periods) {
      # Define local file paths
      chas_file <- file.path(data_dir, paste0("chas_", period, ".csv"))
      
      # Check if we need to download
      need_download <- !file.exists(chas_file) || refresh_cache
      
      if (need_download) {
        # HUD CHAS URLs follow a pattern but it may change
        # Here's a common pattern for recent years
        period_no_dash <- gsub("-", "", period)
        chas_url <- paste0(
          "https://www.huduser.gov/portal/datasets/cp/",
          period_no_dash,
          "/CHAS_", period_no_dash, "_County.csv"
        )
        
        # Try to download
        if (!safe_download(chas_url, chas_file, paste("CHAS data for", period))) {
          # Try alternative URL pattern
          alt_url <- paste0(
            "https://www.huduser.gov/portal/datasets/cp/",
            "CHAS_", period_no_dash, "_County.zip"
          )
          
          # Download to temporary zip file
          temp_zip <- file.path(data_dir, paste0("chas_", period, ".zip"))
          if (safe_download(alt_url, temp_zip, paste("CHAS data for", period, "(zip)"))) {
            # Try to unzip
            tryCatch({
              unzip(temp_zip, exdir = file.path(data_dir, "chas_temp"))
              
              # Find CSV file
              csv_files <- list.files(file.path(data_dir, "chas_temp"), 
                                     pattern = "\\.csv$", 
                                     full.names = TRUE)
              
              if (length(csv_files) > 0) {
                # Use the first CSV file
                file.copy(csv_files[1], chas_file, overwrite = TRUE)
                print_msg(paste("Extracted CHAS data for", period))
                
                # Clean up temp files
                unlink(temp_zip)
                unlink(file.path(data_dir, "chas_temp"), recursive = TRUE)
              } else {
                print_msg("No CSV files found in CHAS zip file")
              }
            }, error = function(e) {
              print_msg(paste("Error extracting CHAS data:", conditionMessage(e)))
            })
          } else {
            print_msg(paste("Could not download CHAS data for", period))
          }
        }
      } else {
        print_msg(paste("Using existing CHAS file for", period))
      }
      
      # Process the data if file exists
      if (file.exists(chas_file)) {
        print_msg(paste("Reading CHAS data for", period))
        
        # Read the file
        tryCatch({
          chas_data <- read_csv(chas_file, show_col_types = FALSE)
          
          # Get column names
          print_msg(paste("CHAS data has", ncol(chas_data), "columns and", nrow(chas_data), "rows"))
          
          # Check if this looks like CHAS data
          # CHAS typically has county information in specific columns
          county_col <- grep("county|County", names(chas_data), value = TRUE)[1]
          state_col <- grep("state|State", names(chas_data), value = TRUE)[1]
          
          # We also need to check for GEOID
          geoid_col <- grep("GEOID|geoid|fips|FIPS", names(chas_data), value = TRUE)[1]
          
          if (is.na(geoid_col) && (!is.na(county_col) && !is.na(state_col))) {
            print_msg("CHAS data uses county/state names, not GEOID - may need mapping")
            # Ideally we would map county/state names to GEOID
            # But for this example, we'll skip this complex task
            next
          } else if (is.na(geoid_col)) {
            print_msg("Could not identify GEOID or county/state columns in CHAS data")
            next
          }
          
          # Rename the GEOID column
          chas_data <- chas_data %>%
            rename(GEOID = all_of(geoid_col)) %>%
            # Ensure GEOID is properly formatted (5 digits with leading zeros)
            mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
          
          # Now find columns for our variables of interest
          # This is challenging because CHAS has many columns with complex naming
          
          # Find columns for severe cost burden (owners)
          owner_burden_cols <- grep("owner|Owner|OWNER", names(chas_data), value = TRUE)
          owner_burden_cols <- intersect(
            owner_burden_cols,
            grep("burden|Burden|BURDEN|cost|Cost|COST", names(chas_data), value = TRUE)
          )
          owner_burden_cols <- intersect(
            owner_burden_cols,
            grep("severe|Severe|SEVERE|50|gt50", names(chas_data), value = TRUE)
          )
          
          # Find columns for severe cost burden (renters)
          renter_burden_cols <- grep("renter|Renter|RENTER", names(chas_data), value = TRUE)
          renter_burden_cols <- intersect(
            renter_burden_cols,
            grep("burden|Burden|BURDEN|cost|Cost|COST", names(chas_data), value = TRUE)
          )
          renter_burden_cols <- intersect(
            renter_burden_cols,
            grep("severe|Severe|SEVERE|50|gt50", names(chas_data), value = TRUE)
          )
          
          # Find columns for affordable units ratio
          affordable_cols <- grep("afford|Afford|AFFORD", names(chas_data), value = TRUE)
          affordable_cols <- intersect(
            affordable_cols,
            grep("ratio|Ratio|RATIO", names(chas_data), value = TRUE)
          )
          
          # Find columns for housing problems
          problems_cols <- grep("problem|Problem|PROBLEM", names(chas_data), value = TRUE)
          
          # Find columns for overcrowding
          overcrowded_cols <- grep("crowd|Crowd|CROWD|person|Person|PERSON", names(chas_data), value = TRUE)
          overcrowded_cols <- intersect(
            overcrowded_cols,
            grep("room|Room|ROOM", names(chas_data), value = TRUE)
          )
          
          print_msg(paste("Found columns: Owner burden:", length(owner_burden_cols),
                        "Renter burden:", length(renter_burden_cols),
                        "Affordable ratio:", length(affordable_cols),
                        "Problems:", length(problems_cols),
                        "Overcrowded:", length(overcrowded_cols)))
          
          # Create data frame for this period's year
          year_data <- data.frame(
            GEOID = chas_data$GEOID,
            year = chas_periods[[period]]
          )
          
          # Add owner cost burden if available
          if (length(owner_burden_cols) > 0) {
            # Use first matching column
            owner_burden_col <- owner_burden_cols[1]
            
            # Get total households for percentage calculation
            total_owner_households_col <- grep("owner|Owner|OWNER", names(chas_data), value = TRUE)
            total_owner_households_col <- intersect(
              total_owner_households_col,
              grep("total|Total|TOTAL", names(chas_data), value = TRUE)
            )
            
            if (length(total_owner_households_col) > 0) {
              # Calculate percentage
              year_data$severely_cost_burdened_owners_pct <- 
                100 * as.numeric(chas_data[[owner_burden_col]]) / 
                as.numeric(chas_data[[total_owner_households_col[1]]])
              
              year_data$severely_cost_burdened_owners_pct_data_quality <- data_quality_flags$direct
              year_data$severely_cost_burdened_owners_pct_data_source <- "HUD CHAS"
              year_data$severely_cost_burdened_owners_pct_data_vintage <- as.character(period)
            }
          }
          
          # Add renter cost burden if available
          if (length(renter_burden_cols) > 0) {
            # Use first matching column
            renter_burden_col <- renter_burden_cols[1]
            
            # Get total households for percentage calculation
            total_renter_households_col <- grep("renter|Renter|RENTER", names(chas_data), value = TRUE)
            total_renter_households_col <- intersect(
              total_renter_households_col,
              grep("total|Total|TOTAL", names(chas_data), value = TRUE)
            )
            
            if (length(total_renter_households_col) > 0) {
              # Calculate percentage
              year_data$severely_cost_burdened_renters_pct <- 
                100 * as.numeric(chas_data[[renter_burden_col]]) / 
                as.numeric(chas_data[[total_renter_households_col[1]]])
              
              year_data$severely_cost_burdened_renters_pct_data_quality <- data_quality_flags$direct
              year_data$severely_cost_burdened_renters_pct_data_source <- "HUD CHAS"
              year_data$severely_cost_burdened_renters_pct_data_vintage <- as.character(period)
            }
          }
          
          # Add affordable units ratio if available
          if (length(affordable_cols) > 0) {
            # Use first matching column
            affordable_col <- affordable_cols[1]
            
            year_data$low_income_renters_affordable_units_ratio <- 
              as.numeric(chas_data[[affordable_col]])
            
            year_data$low_income_renters_affordable_units_ratio_data_quality <- data_quality_flags$direct
            year_data$low_income_renters_affordable_units_ratio_data_source <- "HUD CHAS"
            year_data$low_income_renters_affordable_units_ratio_data_vintage <- as.character(period)
          }
          
          # Add housing problems if available
          if (length(problems_cols) > 0) {
            # Use first matching column
            problems_col <- problems_cols[1]
            
            # Get total households for percentage calculation
            total_households_col <- grep("total|Total|TOTAL", names(chas_data), value = TRUE)
            
            if (length(total_households_col) > 0) {
              # Calculate percentage
              year_data$housing_problems_pct <- 
                100 * as.numeric(chas_data[[problems_col]]) / 
                as.numeric(chas_data[[total_households_col[1]]])
              
              year_data$housing_problems_pct_data_quality <- data_quality_flags$direct
              year_data$housing_problems_pct_data_source <- "HUD CHAS"
              year_data$housing_problems_pct_data_vintage <- as.character(period)
            }
          }
          
          # Add overcrowding if available
          if (length(overcrowded_cols) > 0) {
            # Use first matching column
            overcrowded_col <- overcrowded_cols[1]
            
            # Get total households for percentage calculation
            total_households_col <- grep("total|Total|TOTAL", names(chas_data), value = TRUE)
            
            if (length(total_households_col) > 0) {
              # Calculate percentage
              year_data$overcrowded_housing_pct <- 
                100 * as.numeric(chas_data[[overcrowded_col]]) / 
                as.numeric(chas_data[[total_households_col[1]]])
              
              year_data$overcrowded_housing_pct_data_quality <- data_quality_flags$direct
              year_data$overcrowded_housing_pct_data_source <- "HUD CHAS"
              year_data$overcrowded_housing_pct_data_vintage <- as.character(period)
            }
          }
          
          # Add to list
          chas_data_list[[period]] <- year_data
          
          print_msg(paste("Processed CHAS data for", period))
        }, error = function(e) {
          print_msg(paste("Error reading CHAS data for", period, ":", conditionMessage(e)))
        })
      }
    }
    
    # Combine all periods
    if (length(chas_data_list) > 0) {
      combined_chas <- bind_rows(chas_data_list)
      print_msg(paste("Combined CHAS data with", nrow(combined_chas), "rows"))
      return(combined_chas)
    } else {
      print_msg("No CHAS data processed successfully")
      return(NULL)
    }
  }
  
  # Function to get Eviction Lab data
  get_eviction_data <- function() {
    # Eviction Lab data is available for years 2000-2018
    # We'll try to get data for each year in the requested range
    
    # Define variables we want to extract
    eviction_variables <- c(
      "eviction_rate" = "Evictions per 100 renter homes",
      "eviction_filing_rate" = "Eviction filings per 100 renter homes",
      "rent_burden_pct" = "Percentage of income spent on rent (median)"
    )
    
    # Eviction Lab data can be downloaded as a single file with multiple years
    eviction_file <- file.path(data_dir, "eviction_counties.csv")
    
    # Check if we need to download
    need_download <- !file.exists(eviction_file) || refresh_cache
    
    if (need_download) {
      # Eviction Lab URL for county-level data
      eviction_url <- "https://eviction-lab-data-downloads.s3.amazonaws.com/full-datasets/counties.csv"
      
      # Try to download
      if (!safe_download(eviction_url, eviction_file, "Eviction Lab county data")) {
        print_msg("Could not download Eviction Lab data")
      }
    } else {
      print_msg("Using existing Eviction Lab file")
    }
    
    # Process the data if file exists
    if (file.exists(eviction_file)) {
      print_msg("Reading Eviction Lab data")
      
      # Read the file
      tryCatch({
        # Eviction Lab files can be large, read with optimizations
        eviction_data <- read_csv(
          eviction_file, 
          show_col_types = FALSE,
          guess_max = 10000
        )
        
        # Get column names
        print_msg(paste("Eviction Lab data has", ncol(eviction_data), "columns and", nrow(eviction_data), "rows"))
        
        # Check if this looks like Eviction Lab data
        # Should have columns for year, GEOID, and eviction-related metrics
        if (!all(c("year", "GEOID") %in% names(eviction_data))) {
          # Try alternate column names
          geoid_col <- grep("FIPS|fips|geoid", names(eviction_data), value = TRUE)[1]
          year_col <- grep("^year$|^yr$|^Year$", names(eviction_data), value = TRUE)[1]
          
          if (is.na(geoid_col) || is.na(year_col)) {
            print_msg("Could not identify GEOID or year columns in Eviction Lab data")
            return(NULL)
          }
          
          # Rename columns
          eviction_data <- eviction_data %>%
            rename(
              GEOID = all_of(geoid_col),
              year = all_of(year_col)
            )
        }
        
        # Ensure GEOID is properly formatted
        eviction_data <- eviction_data %>%
          mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
        
        # Find columns for our variables of interest
        
        # Eviction rate
        eviction_rate_col <- grep("^eviction_rate$|^er$|^eviction-rate$", 
                                 names(eviction_data), value = TRUE)[1]
        
        # Eviction filing rate
        filing_rate_col <- grep("^eviction_filing_rate$|^efr$|^filing-rate$", 
                               names(eviction_data), value = TRUE)[1]
        
        # Rent burden
        rent_burden_col <- grep("^rent_burden$|^rb$|^rent-burden$|^rent_to_income$", 
                               names(eviction_data), value = TRUE)[1]
        
        print_msg(paste("Found columns: Eviction rate:", !is.na(eviction_rate_col),
                      "Filing rate:", !is.na(filing_rate_col),
                      "Rent burden:", !is.na(rent_burden_col)))
        
        # Extract and process the requested years
        eviction_data <- eviction_data %>%
          filter(year %in% years)
        
        # Create our standardized dataset
        result_data <- eviction_data %>%
          select(GEOID, year)
        
        # Add eviction rate if available
        if (!is.na(eviction_rate_col)) {
          result_data$eviction_rate <- eviction_data[[eviction_rate_col]]
          result_data$eviction_rate_data_quality <- data_quality_flags$direct
          result_data$eviction_rate_data_source <- "Eviction Lab"
          result_data$eviction_rate_data_vintage <- as.character(eviction_data$year)
        }
        
        # Add filing rate if available
        if (!is.na(filing_rate_col)) {
          result_data$eviction_filing_rate <- eviction_data[[filing_rate_col]]
          result_data$eviction_filing_rate_data_quality <- data_quality_flags$direct
          result_data$eviction_filing_rate_data_source <- "Eviction Lab"
          result_data$eviction_filing_rate_data_vintage <- as.character(eviction_data$year)
        }
        
        # Add rent burden if available
        if (!is.na(rent_burden_col)) {
          result_data$rent_burden_pct <- eviction_data[[rent_burden_col]]
          result_data$rent_burden_pct_data_quality <- data_quality_flags$direct
          result_data$rent_burden_pct_data_source <- "Eviction Lab"
          result_data$rent_burden_pct_data_vintage <- as.character(eviction_data$year)
        }
        
        print_msg(paste("Processed Eviction Lab data with", nrow(result_data), "rows"))
        return(result_data)
      }, error = function(e) {
        print_msg(paste("Error reading Eviction Lab data:", conditionMessage(e)))
        return(NULL)
      })
    } else {
      print_msg("Eviction Lab data file not found")
      return(NULL)
    }
  }
  
  # Function to get Federal Reserve HMDA data
  get_hmda_data <- function() {
    # HMDA data is available from 2007 onwards
    # We'll try to get data for each year in the requested range
    
    # Define variables we want to extract
    hmda_variables <- c(
      "mortgage_denial_rate" = "Percentage of mortgage applications denied",
      "high_cost_loans_pct" = "Percentage of loans that are high-cost",
      "foreclosure_rate" = "Foreclosures per 1,000 housing units"
    )
    
    # HMDA data list to store results
    hmda_data_list <- list()
    
    # Limit to years 2007 and later
    hmda_years <- years[years >= 2007]
    
    for (year in hmda_years) {
      # Skip future years
      if (year > as.integer(format(Sys.Date(), "%Y"))) {
        next
      }
      
      # Define file paths
      hmda_file <- file.path(data_dir, paste0("hmda_", year, ".csv"))
      
      # Check if we need to download
      need_download <- !file.exists(hmda_file) || refresh_cache
      
      if (need_download) {
        # HMDA URLs can vary by year and source
        # Here we use a simplified URL pattern (real implementation would need actual URLs)
        hmda_url <- paste0(
          "https://www.ffiec.gov/hmda/data/countyfiles/", 
          year, 
          "/hmda_county_", 
          year, 
          ".zip"
        )
        
        # Try to download - note this is a placeholder URL
        if (!safe_download(hmda_url, hmda_file, paste("HMDA data for", year))) {
          print_msg(paste("Could not download HMDA data for", year))
          # HMDA data is complex to access via direct download
          # In a real implementation, you would need to navigate the FFIEC site
          # or use their API if available
          next
        }
      } else {
        print_msg(paste("Using existing HMDA file for", year))
      }
      
      # Process the data if file exists - this is simplified for example purposes
      if (file.exists(hmda_file)) {
        print_msg(paste("Reading HMDA data for", year))
        
        # Read the file
        tryCatch({
          hmda_data <- read_csv(hmda_file, show_col_types = FALSE)
          
          # Get column names
          print_msg(paste("HMDA data has", ncol(hmda_data), "columns and", nrow(hmda_data), "rows"))
          
          # Check for GEOID column
          geoid_col <- grep("FIPS|fips|geoid|GEOID", names(hmda_data), value = TRUE)[1]
          
          if (is.na(geoid_col)) {
            print_msg("Could not identify GEOID column in HMDA data")
            next
          }
          
          # Rename and format GEOID
          hmda_data <- hmda_data %>%
            rename(GEOID = all_of(geoid_col)) %>%
            mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
          
          # Find columns for our variables
          
          # Denial rate
          denial_col <- grep("denial|Denial|denied|Denied", 
                            names(hmda_data), value = TRUE)[1]
          
          # High-cost loans
          highcost_col <- grep("high.*cost|high.*price|rate.*spread|subprime", 
                              names(hmda_data), value = TRUE)[1]
          
          # Foreclosure
          foreclosure_col <- grep("foreclos|Foreclos", 
                                 names(hmda_data), value = TRUE)[1]
          
          print_msg(paste("Found columns: Denial rate:", !is.na(denial_col),
                        "High-cost loans:", !is.na(highcost_col),
                        "Foreclosures:", !is.na(foreclosure_col)))
          
          # Create data frame for this year
          year_data <- data.frame(
            GEOID = hmda_data$GEOID,
            year = year
          )
          
          # Add mortgage denial rate if available
          if (!is.na(denial_col)) {
            year_data$mortgage_denial_rate <- hmda_data[[denial_col]]
            year_data$mortgage_denial_rate_data_quality <- data_quality_flags$direct
            year_data$mortgage_denial_rate_data_source <- "Federal Reserve HMDA"
            year_data$mortgage_denial_rate_data_vintage <- as.character(year)
          }
          
          # Add high-cost loans if available
          if (!is.na(highcost_col)) {
            year_data$high_cost_loans_pct <- hmda_data[[highcost_col]]
            year_data$high_cost_loans_pct_data_quality <- data_quality_flags$direct
            year_data$high_cost_loans_pct_data_source <- "Federal Reserve HMDA"
            year_data$high_cost_loans_pct_data_vintage <- as.character(year)
          }
          
          # Add foreclosure rate if available
          if (!is.na(foreclosure_col)) {
            year_data$foreclosure_rate <- hmda_data[[foreclosure_col]]
            year_data$foreclosure_rate_data_quality <- data_quality_flags$direct
            year_data$foreclosure_rate_data_source <- "Federal Reserve HMDA"
            year_data$foreclosure_rate_data_vintage <- as.character(year)
          }
          
          # Add to list
          hmda_data_list[[as.character(year)]] <- year_data
          
          print_msg(paste("Processed HMDA data for", year))
        }, error = function(e) {
          print_msg(paste("Error reading HMDA data for", year, ":", conditionMessage(e)))
        })
      }
    }
    
    # Combine all years
    if (length(hmda_data_list) > 0) {
      combined_hmda <- bind_rows(hmda_data_list)
      print_msg(paste("Combined HMDA data with", nrow(combined_hmda), "rows"))
      return(combined_hmda)
    } else {
      print_msg("No HMDA data processed successfully")
      return(NULL)
    }
  }
  
  # Get data from different housing sources
  chas_data <- get_chas_data()
  eviction_data <- get_eviction_data()
  hmda_data <- get_hmda_data()
  
  # Combine all data sources
  housing_data_list <- list()
  
  if (!is.null(chas_data) && nrow(chas_data) > 0) {
    housing_data_list[["chas"]] <- chas_data
  }
  
  if (!is.null(eviction_data) && nrow(eviction_data) > 0) {
    housing_data_list[["eviction"]] <- eviction_data
  }
  
  if (!is.null(hmda_data) && nrow(hmda_data) > 0) {
    housing_data_list[["hmda"]] <- hmda_data
  }
  
  # Process if we have data
  if (length(housing_data_list) > 0) {
    # Combine all sources and handle overlapping columns
    # Start with first dataset
    combined_housing_data <- housing_data_list[[1]]
    
    # Add each additional dataset
    if (length(housing_data_list) > 1) {
      for (i in 2:length(housing_data_list)) {
        next_data <- housing_data_list[[i]]
        
        # Identify common columns for joining
        join_cols <- intersect(names(combined_housing_data), names(next_data))
        
        # Ensure we have at least GEOID and year for joining
        if (all(c("GEOID", "year") %in% join_cols)) {
          # Find columns that might overlap (excluding join columns and flags)
          overlap_cols <- setdiff(
            intersect(names(combined_housing_data), names(next_data)),
            c(join_cols, grep("_data_quality$|_data_source$|_data_vintage$", 
                             names(next_data), value = TRUE))
          )
          
          if (length(overlap_cols) > 0) {
            # For overlapping columns, prefer data from the current dataset
            # Remove those columns from the combined data before joining
            combined_housing_data <- combined_housing_data %>%
              select(-all_of(overlap_cols))
          }
          
          # Join with the next dataset
          combined_housing_data <- full_join(
            combined_housing_data, 
            next_data, 
            by = join_cols
          )
        } else {
          print_msg("Warning: Cannot join datasets - missing common keys")
        }
      }
    }
    
    # Check if we need to handle missing years
    available_years <- unique(combined_housing_data$year)
    missing_years <- setdiff(years, available_years)
    
    if (length(missing_years) > 0) {
      print_msg(paste("Need to handle missing years:", paste(missing_years, collapse=", ")))
      
      # For missing years, duplicate data from closest available year
      missing_data_list <- list()
      
      for (missing_year in missing_years) {
        # Find closest available year
        closest_year <- available_years[which.min(abs(available_years - missing_year))]
        
        # Get data for closest year
        closest_data <- combined_housing_data %>%
          filter(year == closest_year)
        
        # Update year and quality flags
        closest_data$year <- missing_year
        
        # Update flags for all variables
        measure_cols <- setdiff(
          names(closest_data),
          c("GEOID", "year", grep("_data_quality$|_data_source$|_data_vintage$", 
                                 names(closest_data), value = TRUE))
        )
        
        for (col in measure_cols) {
          quality_col <- paste0(col, "_data_quality")
          source_col <- paste0(col, "_data_source")
          vintage_col <- paste0(col, "_data_vintage")
          
          if (quality_col %in% names(closest_data)) {
            closest_data[[quality_col]] <- 
              if (!allow_interpolation) {
                data_quality_flags$missing # No interpolation allowed
              } else if (missing_year > max(available_years) || missing_year < min(available_years)) {
                data_quality_flags$extrapolated # Extrapolation
              } else {
                data_quality_flags$interpolated # Interpolation
              }
          }
          
          if (vintage_col %in% names(closest_data)) {
            closest_data[[vintage_col]] <- paste0("derived_from_", closest_year)
          }
        }
        
        # Add to list
        missing_data_list[[as.character(missing_year)]] <- closest_data
      }
      
      # Combine with original data
      if (length(missing_data_list) > 0) {
        missing_data <- bind_rows(missing_data_list)
        combined_housing_data <- bind_rows(combined_housing_data, missing_data)
      }
    }
    
    # Filter to just the requested years
    combined_housing_data <- combined_housing_data %>%
      filter(year %in% years)
    
    # Sort data
    combined_housing_data <- combined_housing_data %>%
      arrange(GEOID, year)
    
    # Cache the processed data
    saveRDS(combined_housing_data, cache_file)
    print_msg(paste("Cached housing data to:", cache_file))
    
    return(combined_housing_data)
  } else if (allow_simulation) {
    # Create simulated data
    print_msg("No housing data found. Creating simulated data...")
    
    # Housing variables to simulate
    housing_vars <- c(
      "severely_cost_burdened_owners_pct" = "Percentage of owner households spending >50% of income on housing",
      "severely_cost_burdened_renters_pct" = "Percentage of renter households spending >50% of income on housing",
      "low_income_renters_affordable_units_ratio" = "Ratio of affordable units to low-income renters",
      "housing_problems_pct" = "Percentage of households with at least one housing problem",
      "overcrowded_housing_pct" = "Percentage of housing units with >1 person per room",
      "eviction_rate" = "Number of evictions per 100 renter homes",
      "eviction_filing_rate" = "Number of eviction filings per 100 renter homes",
      "rent_burden_pct" = "Percentage of income spent on rent (median)",
      "mortgage_denial_rate" = "Percentage of mortgage applications denied",
      "high_cost_loans_pct" = "Percentage of loans that are high-cost",
      "foreclosure_rate" = "Foreclosures per 1,000 housing units"
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
      for (var_name in names(housing_vars)) {
        if (var_name == "severely_cost_burdened_owners_pct") {
          # Typically 10-25%
          year_data[[var_name]] <- runif(nrow(year_data), 10, 25)
        } else if (var_name == "severely_cost_burdened_renters_pct") {
          # Typically 20-40%
          year_data[[var_name]] <- runif(nrow(year_data), 20, 40)
        } else if (var_name == "low_income_renters_affordable_units_ratio") {
          # Typically 0.4-1.2
          year_data[[var_name]] <- runif(nrow(year_data), 0.4, 1.2)
        } else if (var_name == "housing_problems_pct") {
          # Typically 20-50%
          year_data[[var_name]] <- runif(nrow(year_data), 20, 50)
        } else if (var_name == "overcrowded_housing_pct") {
          # Typically 1-10%
          year_data[[var_name]] <- runif(nrow(year_data), 1, 10)
        } else if (var_name == "eviction_rate") {
          # Typically 1-8 per 100 renter homes
          year_data[[var_name]] <- runif(nrow(year_data), 1, 8)
        } else if (var_name == "eviction_filing_rate") {
          # Typically 3-15 per 100 renter homes
          year_data[[var_name]] <- runif(nrow(year_data), 3, 15)
        } else if (var_name == "rent_burden_pct") {
          # Typically 25-40%
          year_data[[var_name]] <- runif(nrow(year_data), 25, 40)
        } else if (var_name == "mortgage_denial_rate") {
          # Typically 10-30%
          year_data[[var_name]] <- runif(nrow(year_data), 10, 30)
        } else if (var_name == "high_cost_loans_pct") {
          # Typically 5-20%
          year_data[[var_name]] <- runif(nrow(year_data), 5, 20)
        } else if (var_name == "foreclosure_rate") {
          # Typically 1-10 per 1,000 housing units
          year_data[[var_name]] <- runif(nrow(year_data), 1, 10)
        } else {
          # Default - 0-100 range
          year_data[[var_name]] <- runif(nrow(year_data), 0, 100)
        }
        
        # Add quality flags
        year_data[[paste0(var_name, "_data_quality")]] <- data_quality_flags$simulated
        year_data[[paste0(var_name, "_data_source")]] <- "SIMULATED Housing Data"
        year_data[[paste0(var_name, "_data_vintage")]] <- paste0("simulated_", year)
      }
      
      sim_data_list[[as.character(year)]] <- year_data
    }
    
    # Combine all years
    simulated_data <- bind_rows(sim_data_list)
    
    # Cache the simulated data
    saveRDS(simulated_data, cache_file)
    print_msg(paste("Cached simulated housing data to:", cache_file))
    
    return(simulated_data)
  } else {
    # No data and simulation not allowed - create empty dataset with NAs
    print_msg("No housing data available and simulation not allowed. Creating empty dataset with NAs.")
    
    # Get variable list for housing data
    housing_vars <- c(
      "severely_cost_burdened_owners_pct",
      "severely_cost_burdened_renters_pct",
      "low_income_renters_affordable_units_ratio",
      "housing_problems_pct",
      "overcrowded_housing_pct",
      "eviction_rate",
      "eviction_filing_rate",
      "rent_burden_pct",
      "mortgage_denial_rate",
      "high_cost_loans_pct",
      "foreclosure_rate"
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
    for (var in housing_vars) {
      grid[[var]] <- NA_real_
      grid[[paste0(var, "_data_quality")]] <- data_quality_flags$missing
      grid[[paste0(var, "_data_source")]] <- "NOT_AVAILABLE"
      grid[[paste0(var, "_data_vintage")]] <- NA_character_
    }
    
    housing_data <- as_tibble(grid)
    print_msg(paste("Created empty housing dataset with", nrow(housing_data), "rows"))
    
    # Cache the empty data
    saveRDS(housing_data, cache_file)
    print_msg(paste("Cached empty housing data to:", cache_file))
    
    return(housing_data)
  }
}

# This lets the function be used when the script is sourced
is_sourced <- function() {
  return(!identical(environment(), globalenv()))
}

# If the script is run directly, test the function
if (!is_sourced()) {
  cat("Testing housing data fetcher...\n")
  
  # Get current year
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  
  # Test for last 5 years
  test_years <- (current_year-4):current_year
  
  # Test the function
  result <- fetch_housing_data(
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
  
  cat("Test completed with", nrow(result), "rows of data.\n")
  
  # Report data quality metrics
  cat("Data quality summary:\n")
  quality_cols <- grep("_data_quality$", names(result), value = TRUE)
  for (col in quality_cols) {
    var_name <- gsub("_data_quality$", "", col)
    quality_counts <- table(result[[col]], useNA = "always")
    cat(paste(" -", var_name, ":", paste(names(quality_counts), quality_counts, sep="=", collapse=", "), "\n"))
  }
}