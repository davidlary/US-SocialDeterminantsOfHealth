#!/usr/bin/env Rscript

# Economic Data Fetcher
# This script handles retrieval of economic data from USDA Economic Research Service, 
# Bureau of Labor Statistics, and Opportunity Insights sources

library(tidyverse)
library(httr)
library(jsonlite)
library(readxl)
library(lubridate)
library(sf)
library(zoo) # For interpolation if needed

#' Fetch economic data
#'
#' Retrieves economic data from BLS, USDA Economic Research Service, and Opportunity Insights
#' for employment, economic typology, and economic mobility at the county level.
#'
#' @param years Vector of years to include
#' @param cache_dir Directory to store cache files
#' @param refresh_cache Whether to refresh the cache
#' @param allow_simulation Whether to generate simulated data if real data not available
#' @param allow_interpolation Whether to interpolate missing values
#' @param data_quality_flags List of standardized data quality flags
#' @param offline_mode Whether to skip all downloads and use only cached data
#' @return A data frame with economic data for all requested years
fetch_economic_data <- function(years, 
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
  cache_file <- file.path(cache_dir, "economic_data.rds")
  
  # Use cache if available and not refreshing
  if (!refresh_cache && file.exists(cache_file)) {
    print_msg("Loading cached economic data...")
    economic_data <- readRDS(cache_file)
    
    # Check if all requested years are in the cache
    cached_years <- unique(economic_data$year)
    missing_years <- setdiff(years, cached_years)
    
    # Check for empty cache with just placeholder data
    if (nrow(economic_data) <= 1 || 
        (is.data.frame(economic_data) && "data_source" %in% names(economic_data) && 
         any(grepl("SIMULATED", economic_data$data_source)))) {
      print_msg("Cached economic data appears to be empty or a placeholder. Will process files again.")
      # Force refresh by continuing past this point
    } else if (length(missing_years) == 0) {
      print_msg("Using complete cached economic data.")
      return(economic_data %>% filter(year %in% years))
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
  data_dir <- "data/economic"
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
    print_msg(paste("Created economic data directory at:", data_dir))
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
  
  # Function to get USDA Economic Research Service data
  get_ers_data <- function() {
    # USDA ERS has data on county typology and economic indicators
    
    # Define the variables we want from ERS
    ers_variables <- c(
      "employment_volatility_index" = "Index of employment stability/volatility",
      "economic_typology" = "County economic typology",
      "persistent_poverty_county" = "Flag for counties with persistent poverty",
      "persistent_child_poverty_county" = "Flag for counties with persistent child poverty"
    )
    
    # County Typology Codes
    typology_file <- file.path(data_dir, "county_typology.csv")
    
    # Check if we need to download
    need_download <- !file.exists(typology_file) || refresh_cache
    
    if (need_download) {
      # USDA ERS county typology URL
      typology_url <- "https://www.ers.usda.gov/webdocs/DataFiles/48652/2015CountyTypologyCodes.csv"
      
      # Try to download
      if (!safe_download(typology_url, typology_file, "USDA ERS County Typology")) {
        print_msg("Could not download USDA ERS County Typology data")
      }
    } else {
      print_msg("Using existing USDA ERS County Typology file")
    }
    
    # Process the typology data if file exists
    typology_data <- NULL
    if (file.exists(typology_file)) {
      print_msg("Reading USDA ERS County Typology data")
      
      # Read the file
      tryCatch({
        typology_data <- read_csv(typology_file, show_col_types = FALSE)
        
        # Get column names
        print_msg(paste("Typology data has", ncol(typology_data), "columns and", nrow(typology_data), "rows"))
        
        # Check if this looks like typology data
        # Should have FIPS code and typology information
        fips_col <- grep("FIPS|fips|geoid|GEOID", names(typology_data), value = TRUE)[1]
        
        if (is.na(fips_col)) {
          print_msg("Could not identify FIPS column in typology data")
        } else {
          # Rename to GEOID
          typology_data <- typology_data %>%
            rename(GEOID = all_of(fips_col)) %>%
            # Ensure GEOID is properly formatted (5 digits with leading zeros)
            mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
          
          # Check for columns containing typology information
          typology_col <- grep("type|Type|TYPE|typology|Typology", names(typology_data), value = TRUE)[1]
          poverty_col <- grep("poverty|Poverty|POVERTY", names(typology_data), value = TRUE)[1]
          child_poverty_col <- grep("child.*poverty|Child.*Poverty", names(typology_data), value = TRUE)[1]
          
          print_msg(paste("Found columns: Typology:", !is.na(typology_col),
                        "Poverty:", !is.na(poverty_col),
                        "Child Poverty:", !is.na(child_poverty_col)))
        }
      }, error = function(e) {
        print_msg(paste("Error reading typology data:", conditionMessage(e)))
      })
    }
    
    # Employment data
    # For employment volatility and other metrics
    emp_file <- file.path(data_dir, "county_employment.csv")
    
    # Check if we need to download
    need_download <- !file.exists(emp_file) || refresh_cache
    
    if (need_download) {
      # USDA ERS unemployment data URL
      emp_url <- "https://www.ers.usda.gov/webdocs/DataFiles/48747/Unemployment.csv"
      
      # Try to download
      if (!safe_download(emp_url, emp_file, "USDA ERS Unemployment Data")) {
        print_msg("Could not download USDA ERS Unemployment data")
      }
    } else {
      print_msg("Using existing USDA ERS Unemployment file")
    }
    
    # Process the employment data if file exists
    emp_data <- NULL
    if (file.exists(emp_file)) {
      print_msg("Reading USDA ERS Employment data")
      
      # Read the file
      tryCatch({
        emp_data <- read_csv(emp_file, show_col_types = FALSE)
        
        # Get column names
        print_msg(paste("Employment data has", ncol(emp_data), "columns and", nrow(emp_data), "rows"))
        
        # Check if this looks like employment data
        # Should have FIPS code and year information
        fips_col <- grep("FIPS|fips|geoid|GEOID", names(emp_data), value = TRUE)[1]
        year_col <- grep("^year$|^yr$|^Year$", names(emp_data), value = TRUE)[1]
        
        if (is.na(fips_col)) {
          print_msg("Could not identify FIPS column in employment data")
        } else if (is.na(year_col)) {
          print_msg("Could not identify year column in employment data")
        } else {
          # Rename to GEOID and year
          emp_data <- emp_data %>%
            rename(
              GEOID = all_of(fips_col),
              year = all_of(year_col)
            ) %>%
            # Ensure GEOID is properly formatted (5 digits with leading zeros)
            mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
          
          # Filter to requested years
          emp_data <- emp_data %>%
            filter(year %in% years)
          
          # Check for volatility-related columns
          volatility_col <- grep("volatility|Volatility|stability|Stability", names(emp_data), value = TRUE)[1]
          
          print_msg(paste("Found employment volatility column:", !is.na(volatility_col)))
        }
      }, error = function(e) {
        print_msg(paste("Error reading employment data:", conditionMessage(e)))
      })
    }
    
    # Combine typology and employment data
    combined_ers_data <- NULL
    
    # Process typology data which applies to all years
    if (!is.null(typology_data)) {
      # Identify columns we'll use
      typology_col <- grep("type|Type|TYPE|typology|Typology", names(typology_data), value = TRUE)[1]
      poverty_col <- grep("poverty|Poverty|POVERTY", names(typology_data), value = TRUE)[1]
      child_poverty_col <- grep("child.*poverty|Child.*Poverty", names(typology_data), value = TRUE)[1]
      
      # Create data for each year
      ers_data_by_year <- list()
      
      for (year in years) {
        # Create data frame for this year
        year_data <- data.frame(
          GEOID = typology_data$GEOID,
          year = year
        )
        
        # Add economic typology if available
        if (!is.na(typology_col)) {
          year_data$economic_typology <- typology_data[[typology_col]]
          year_data$economic_typology_data_quality <- data_quality_flags$direct
          year_data$economic_typology_data_source <- "USDA ERS County Typology"
          year_data$economic_typology_data_vintage <- "2015" # Base year for typology
        }
        
        # Add persistent poverty if available
        if (!is.na(poverty_col)) {
          year_data$persistent_poverty_county <- typology_data[[poverty_col]]
          year_data$persistent_poverty_county_data_quality <- data_quality_flags$direct
          year_data$persistent_poverty_county_data_source <- "USDA ERS County Typology"
          year_data$persistent_poverty_county_data_vintage <- "2015" # Base year
        }
        
        # Add persistent child poverty if available
        if (!is.na(child_poverty_col)) {
          year_data$persistent_child_poverty_county <- typology_data[[child_poverty_col]]
          year_data$persistent_child_poverty_county_data_quality <- data_quality_flags$direct
          year_data$persistent_child_poverty_county_data_source <- "USDA ERS County Typology"
          year_data$persistent_child_poverty_county_data_vintage <- "2015" # Base year
        }
        
        ers_data_by_year[[as.character(year)]] <- year_data
      }
      
      # Combine all years
      if (length(ers_data_by_year) > 0) {
        combined_ers_data <- bind_rows(ers_data_by_year)
      }
    }
    
    # Add employment data if available
    if (!is.null(emp_data) && !is.null(combined_ers_data)) {
      # Find employment volatility column
      volatility_col <- grep("volatility|Volatility|stability|Stability", names(emp_data), value = TRUE)[1]
      
      if (!is.na(volatility_col)) {
        # Create a temporary data frame with just volatility
        emp_volatility <- emp_data %>%
          select(GEOID, year, volatility = all_of(volatility_col))
        
        # Join with combined data
        combined_ers_data <- left_join(
          combined_ers_data,
          emp_volatility,
          by = c("GEOID", "year")
        )
        
        # Rename and add metadata
        combined_ers_data <- combined_ers_data %>%
          rename(employment_volatility_index = volatility) %>%
          mutate(
            employment_volatility_index_data_quality = data_quality_flags$direct,
            employment_volatility_index_data_source = "USDA ERS",
            employment_volatility_index_data_vintage = as.character(year)
          )
      }
    } else if (!is.null(emp_data) && is.null(combined_ers_data)) {
      # If we only have employment data, start with that
      volatility_col <- grep("volatility|Volatility|stability|Stability", names(emp_data), value = TRUE)[1]
      
      if (!is.na(volatility_col)) {
        combined_ers_data <- emp_data %>%
          rename(employment_volatility_index = all_of(volatility_col)) %>%
          mutate(
            employment_volatility_index_data_quality = data_quality_flags$direct,
            employment_volatility_index_data_source = "USDA ERS",
            employment_volatility_index_data_vintage = as.character(year)
          )
      }
    }
    
    return(combined_ers_data)
  }
  
  # Function to get Bureau of Labor Statistics data
  get_bls_data <- function() {
    # BLS LAUS (Local Area Unemployment Statistics) for job growth and unemployment
    
    # Define the variables we want from BLS
    bls_variables <- c(
      "job_growth_rate" = "Annual job growth rate"
    )
    
    # BLS LAUS data list to store results
    bls_data_list <- list()
    
    # Process each year
    for (year in years) {
      # Skip future years
      if (year > as.integer(format(Sys.Date(), "%Y"))) {
        next
      }
      
      # Define file paths
      bls_file <- file.path(data_dir, paste0("bls_laus_", year, ".csv"))
      
      # Check if we need to download
      need_download <- !file.exists(bls_file) || refresh_cache
      
      if (need_download) {
        # BLS LAUS URL
        # Note: BLS data requires API key for bulk downloads, this is simplified
        # Real implementation would use BLS API with proper key
        bls_url <- paste0(
          "https://download.bls.gov/pub/time.series/la/la.data.", year, ".csv"
        )
        
        # Try to download
        if (!safe_download(bls_url, bls_file, paste("BLS LAUS data for", year))) {
          print_msg(paste("Could not download BLS LAUS data for", year))
          next
        }
      } else {
        print_msg(paste("Using existing BLS LAUS file for", year))
      }
      
      # Process the data if file exists
      if (file.exists(bls_file)) {
        print_msg(paste("Reading BLS LAUS data for", year))
        
        # Read the file
        tryCatch({
          # BLS files can be large, use optimizations
          bls_data <- read_csv(
            bls_file, 
            show_col_types = FALSE,
            guess_max = 10000
          )
          
          # Get column names
          print_msg(paste("BLS data has", ncol(bls_data), "columns and", nrow(bls_data), "rows"))
          
          # BLS LAUS data is complex and needs special processing
          # This is a simplified example of how it might work
          # A full implementation would require careful parsing of BLS series codes
          
          # Check if this has county-level data
          # BLS uses series IDs which include geographic codes
          series_col <- grep("series|Series|SERIES", names(bls_data), value = TRUE)[1]
          
          if (is.na(series_col)) {
            print_msg("Could not identify series column in BLS data")
            next
          }
          
          # Filter to county-level series (usually starts with LAU)
          county_series <- bls_data %>%
            filter(grepl("^LAU", .data[[series_col]]))
          
          if (nrow(county_series) == 0) {
            print_msg("No county-level series found in BLS data")
            next
          }
          
          # Extract FIPS codes from series IDs
          # This is highly dependent on BLS series ID structure
          # A real implementation would need specific logic
          
          # For this example, create a simplified dataset
          year_data <- data.frame(
            GEOID = rep(NA, nrow(county_series)),
            year = year
          )
          
          # Add job growth rate if available
          growth_col <- grep("growth|Growth|change|Change", names(county_series), value = TRUE)[1]
          
          if (!is.na(growth_col)) {
            year_data$job_growth_rate <- county_series[[growth_col]]
            year_data$job_growth_rate_data_quality <- data_quality_flags$direct
            year_data$job_growth_rate_data_source <- "Bureau of Labor Statistics"
            year_data$job_growth_rate_data_vintage <- as.character(year)
          }
          
          # Add to list
          bls_data_list[[as.character(year)]] <- year_data
          
          print_msg(paste("Processed BLS LAUS data for", year))
        }, error = function(e) {
          print_msg(paste("Error reading BLS LAUS data for", year, ":", conditionMessage(e)))
        })
      }
    }
    
    # Combine all years
    if (length(bls_data_list) > 0) {
      combined_bls <- bind_rows(bls_data_list)
      print_msg(paste("Combined BLS data with", nrow(combined_bls), "rows"))
      return(combined_bls)
    } else {
      print_msg("No BLS data processed successfully")
      return(NULL)
    }
  }
  
  # Function to get Opportunity Insights data
  get_opportunity_data <- function() {
    # Opportunity Insights data on economic mobility
    
    # Define the variables we want
    opportunity_variables <- c(
      "income_mobility_index" = "Measure of intergenerational economic mobility",
      "absolute_upward_mobility" = "Expected income rank for children from low-income families",
      "mean_commute_distance" = "Average commute distance",
      "job_density_index" = "Number of jobs within typical commute distance"
    )
    
    # Opportunity Atlas data file
    opportunity_file <- file.path(data_dir, "opportunity_atlas.csv")
    
    # Check if we need to download
    need_download <- !file.exists(opportunity_file) || refresh_cache
    
    if (need_download) {
      # Opportunity Insights URL
      opportunity_url <- "https://opportunityinsights.org/wp-content/uploads/2018/10/county_outcomes.csv"
      
      # Try to download
      if (!safe_download(opportunity_url, opportunity_file, "Opportunity Insights data")) {
        print_msg("Could not download Opportunity Insights data")
      }
    } else {
      print_msg("Using existing Opportunity Insights file")
    }
    
    # Process the data if file exists
    opportunity_data <- NULL
    if (file.exists(opportunity_file)) {
      print_msg("Reading Opportunity Insights data")
      
      # Read the file
      tryCatch({
        opportunity_data <- read_csv(opportunity_file, show_col_types = FALSE)
        
        # Get column names
        print_msg(paste("Opportunity data has", ncol(opportunity_data), "columns and", nrow(opportunity_data), "rows"))
        
        # Check for county identifier
        county_col <- grep("county|County|COUNTY", names(opportunity_data), value = TRUE)[1]
        state_col <- grep("state|State|STATE", names(opportunity_data), value = TRUE)[1]
        fips_col <- grep("FIPS|fips|geoid|GEOID", names(opportunity_data), value = TRUE)[1]
        
        if (is.na(fips_col) && (!is.na(county_col) && !is.na(state_col))) {
          print_msg("Opportunity data uses county/state names, not FIPS - may need mapping")
          # Ideally we would map county/state names to GEOID
          # But for this example, we'll skip this complex task
          return(NULL)
        } else if (is.na(fips_col)) {
          print_msg("Could not identify FIPS or county/state columns in Opportunity data")
          return(NULL)
        }
        
        # Rename and format GEOID
        opportunity_data <- opportunity_data %>%
          rename(GEOID = all_of(fips_col)) %>%
          mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
        
        # Find columns for our variables of interest
        
        # Economic mobility
        mobility_col <- grep("mobility|Mobility|econ.*mobility", names(opportunity_data), value = TRUE)[1]
        
        # Absolute upward mobility
        upward_col <- grep("upward|Upward|absolute.*mobility", names(opportunity_data), value = TRUE)[1]
        
        # Commute distance
        commute_col <- grep("commute.*dist|commute.*time|dist.*commute", names(opportunity_data), value = TRUE)[1]
        
        # Job density
        job_density_col <- grep("job.*density|density.*job|employment.*density", names(opportunity_data), value = TRUE)[1]
        
        print_msg(paste("Found columns: Mobility:", !is.na(mobility_col),
                      "Upward mobility:", !is.na(upward_col),
                      "Commute distance:", !is.na(commute_col),
                      "Job density:", !is.na(job_density_col)))
        
        # Opportunity Insights data applies to specific years
        # We'll assign it to all requested years
        opportunity_data_by_year <- list()
        
        for (year in years) {
          # Only include years in the Opportunity Insights range (2000-2018)
          if (year < 2000 || year > 2018) {
            next
          }
          
          # Create data frame for this year
          year_data <- data.frame(
            GEOID = opportunity_data$GEOID,
            year = year
          )
          
          # Add income mobility if available
          if (!is.na(mobility_col)) {
            year_data$income_mobility_index <- opportunity_data[[mobility_col]]
            year_data$income_mobility_index_data_quality <- data_quality_flags$direct
            year_data$income_mobility_index_data_source <- "Opportunity Insights"
            year_data$income_mobility_index_data_vintage <- "2018" # Base year for dataset
          }
          
          # Add upward mobility if available
          if (!is.na(upward_col)) {
            year_data$absolute_upward_mobility <- opportunity_data[[upward_col]]
            year_data$absolute_upward_mobility_data_quality <- data_quality_flags$direct
            year_data$absolute_upward_mobility_data_source <- "Opportunity Insights"
            year_data$absolute_upward_mobility_data_vintage <- "2018" # Base year for dataset
          }
          
          # Add commute distance if available
          if (!is.na(commute_col)) {
            year_data$mean_commute_distance <- opportunity_data[[commute_col]]
            year_data$mean_commute_distance_data_quality <- data_quality_flags$direct
            year_data$mean_commute_distance_data_source <- "Opportunity Insights"
            year_data$mean_commute_distance_data_vintage <- "2018" # Base year for dataset
          }
          
          # Add job density if available
          if (!is.na(job_density_col)) {
            year_data$job_density_index <- opportunity_data[[job_density_col]]
            year_data$job_density_index_data_quality <- data_quality_flags$direct
            year_data$job_density_index_data_source <- "Opportunity Insights"
            year_data$job_density_index_data_vintage <- "2018" # Base year for dataset
          }
          
          opportunity_data_by_year[[as.character(year)]] <- year_data
        }
        
        # Combine all years
        if (length(opportunity_data_by_year) > 0) {
          opportunity_data <- bind_rows(opportunity_data_by_year)
          print_msg(paste("Processed Opportunity Insights data with", nrow(opportunity_data), "rows"))
        } else {
          print_msg("No years in range for Opportunity Insights data")
          opportunity_data <- NULL
        }
      }, error = function(e) {
        print_msg(paste("Error reading Opportunity Insights data:", conditionMessage(e)))
        opportunity_data <- NULL
      })
    }
    
    return(opportunity_data)
  }
  
  # Function to get income inequality data from ACS (American Community Survey)
  get_acs_inequality_data <- function() {
    # ACS has data on income inequality ratios
    
    # Define the variables we want from ACS
    acs_variables <- c(
      "income_inequality_ratio" = "Ratio of income at 80th percentile to income at 20th percentile"
    )
    
    # ACS data list to store results
    acs_data_list <- list()
    
    # Process each year
    for (year in years) {
      # Skip years before ACS started (2005+) and future years
      if (year < 2005 || year > as.integer(format(Sys.Date(), "%Y"))) {
        next
      }
      
      # Define file paths
      acs_file <- file.path(data_dir, paste0("acs_inequality_", year, ".csv"))
      
      # Check if we need to download
      need_download <- !file.exists(acs_file) || refresh_cache
      
      if (need_download) {
        # Census API would be used in a real implementation
        # This would require a Census API key and proper queries
        # For this example, we'll simulate the data structure
        
        print_msg(paste("ACS inequality data file not found for", year))
        # No automatic download option for ACS without API key
        next
      } else {
        print_msg(paste("Using existing ACS inequality file for", year))
      }
      
      # Process the data if file exists
      if (file.exists(acs_file)) {
        print_msg(paste("Reading ACS inequality data for", year))
        
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
          
          # Look for inequality ratio column
          inequality_col <- grep("inequality|Inequality|gini|Gini|ratio|Ratio", names(acs_data), value = TRUE)[1]
          
          if (is.na(inequality_col)) {
            print_msg("Could not identify inequality column in ACS data")
            next
          }
          
          # Create data for this year
          year_data <- data.frame(
            GEOID = acs_data$GEOID,
            year = year,
            income_inequality_ratio = acs_data[[inequality_col]],
            income_inequality_ratio_data_quality = data_quality_flags$direct,
            income_inequality_ratio_data_source = "American Community Survey",
            income_inequality_ratio_data_vintage = as.character(year)
          )
          
          # Add to list
          acs_data_list[[as.character(year)]] <- year_data
          
          print_msg(paste("Processed ACS inequality data for", year))
        }, error = function(e) {
          print_msg(paste("Error reading ACS inequality data for", year, ":", conditionMessage(e)))
        })
      }
    }
    
    # Combine all years
    if (length(acs_data_list) > 0) {
      combined_acs <- bind_rows(acs_data_list)
      print_msg(paste("Combined ACS inequality data with", nrow(combined_acs), "rows"))
      return(combined_acs)
    } else {
      print_msg("No ACS inequality data processed successfully")
      return(NULL)
    }
  }
  
  # Get data from different sources
  ers_data <- get_ers_data()
  bls_data <- get_bls_data()
  opportunity_data <- get_opportunity_data()
  acs_inequality_data <- get_acs_inequality_data()
  
  # Combine all data sources
  economic_data_list <- list()
  
  if (!is.null(ers_data) && nrow(ers_data) > 0) {
    economic_data_list[["ers"]] <- ers_data
  }
  
  if (!is.null(bls_data) && nrow(bls_data) > 0) {
    economic_data_list[["bls"]] <- bls_data
  }
  
  if (!is.null(opportunity_data) && nrow(opportunity_data) > 0) {
    economic_data_list[["opportunity"]] <- opportunity_data
  }
  
  if (!is.null(acs_inequality_data) && nrow(acs_inequality_data) > 0) {
    economic_data_list[["acs"]] <- acs_inequality_data
  }
  
  # Process if we have data
  if (length(economic_data_list) > 0) {
    # Combine all sources and handle overlapping columns
    # Start with first dataset
    combined_economic_data <- economic_data_list[[1]]
    
    # Add each additional dataset
    if (length(economic_data_list) > 1) {
      for (i in 2:length(economic_data_list)) {
        next_data <- economic_data_list[[i]]
        
        # Identify common columns for joining
        join_cols <- intersect(names(combined_economic_data), names(next_data))
        
        # Ensure we have at least GEOID and year for joining
        if (all(c("GEOID", "year") %in% join_cols)) {
          # Find columns that might overlap (excluding join columns and flags)
          overlap_cols <- setdiff(
            intersect(names(combined_economic_data), names(next_data)),
            c(join_cols, grep("_data_quality$|_data_source$|_data_vintage$", 
                             names(next_data), value = TRUE))
          )
          
          if (length(overlap_cols) > 0) {
            # For overlapping columns, prefer data from the current dataset
            # Remove those columns from the combined data before joining
            combined_economic_data <- combined_economic_data %>%
              select(-all_of(overlap_cols))
          }
          
          # Join with the next dataset
          combined_economic_data <- full_join(
            combined_economic_data, 
            next_data, 
            by = join_cols
          )
        } else {
          print_msg("Warning: Cannot join datasets - missing common keys")
        }
      }
    }
    
    # Check if we need to handle missing years
    available_years <- unique(combined_economic_data$year)
    missing_years <- setdiff(years, available_years)
    
    if (length(missing_years) > 0 && allow_interpolation) {
      print_msg(paste("Interpolating data for missing years:", paste(missing_years, collapse=", ")))
      
      # Get all variables except join columns and metadata columns
      measure_cols <- setdiff(
        names(combined_economic_data),
        c("GEOID", "year", grep("_data_quality$|_data_source$|_data_vintage$", 
                               names(combined_economic_data), value = TRUE))
      )
      
      # Process each county separately for interpolation
      counties <- unique(combined_economic_data$GEOID)
      
      # List to store interpolated data
      interp_data_list <- list()
      
      for (county in counties) {
        # Get data for this county
        county_data <- combined_economic_data %>%
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
          # Skip categorical variables which shouldn't be interpolated
          if (col == "economic_typology" || 
              col == "persistent_poverty_county" || 
              col == "persistent_child_poverty_county") {
            # Instead, find the value from the nearest year
            if (col %in% names(county_data)) {
              # For each year in our grid
              for (i in 1:nrow(county_grid)) {
                grid_year <- county_grid$year[i]
                
                # Try to find this year directly
                if (grid_year %in% county_data$year) {
                  year_idx <- which(county_data$year == grid_year)
                  county_grid[[col]][i] <- county_data[[col]][year_idx]
                  
                  # Also copy the metadata
                  quality_col <- paste0(col, "_data_quality")
                  source_col <- paste0(col, "_data_source")
                  vintage_col <- paste0(col, "_data_vintage")
                  
                  if (quality_col %in% names(county_data)) {
                    county_grid[[quality_col]][i] <- county_data[[quality_col]][year_idx]
                  }
                  
                  if (source_col %in% names(county_data)) {
                    county_grid[[source_col]][i] <- county_data[[source_col]][year_idx]
                  }
                  
                  if (vintage_col %in% names(county_data)) {
                    county_grid[[vintage_col]][i] <- county_data[[vintage_col]][year_idx]
                  }
                } else {
                  # Find nearest year
                  nearest_year <- county_data$year[which.min(abs(county_data$year - grid_year))]
                  nearest_idx <- which(county_data$year == nearest_year)
                  
                  # Copy the value
                  county_grid[[col]][i] <- county_data[[col]][nearest_idx]
                  
                  # Set appropriate quality flags
                  quality_col <- paste0(col, "_data_quality")
                  source_col <- paste0(col, "_data_source")
                  vintage_col <- paste0(col, "_data_vintage")
                  
                  if (quality_col %in% names(county_data)) {
                    county_grid[[quality_col]][i] <- 
                      if (grid_year > max(county_data$year) || grid_year < min(county_data$year)) {
                        data_quality_flags$extrapolated # Extrapolation
                      } else {
                        data_quality_flags$interpolated # Interpolation
                      }
                  }
                  
                  if (source_col %in% names(county_data)) {
                    county_grid[[source_col]][i] <- county_data[[source_col]][nearest_idx]
                  }
                  
                  if (vintage_col %in% names(county_data)) {
                    county_grid[[vintage_col]][i] <- paste0("derived_from_", nearest_year)
                  }
                }
              }
            }
            next
          }
          
          # For numeric columns, use linear interpolation
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
                         names(combined_economic_data), value = TRUE)
        
        for (col in meta_cols) {
          if (!(col %in% names(interp_data))) {
            interp_data[[col]] <- NA
          }
        }
        
        # Use the interpolated data instead
        combined_economic_data <- interp_data
      }
    } else if (length(missing_years) > 0) {
      print_msg("Missing years but interpolation not allowed. Years will remain missing.")
    }
    
    # Filter to just the requested years
    combined_economic_data <- combined_economic_data %>%
      filter(year %in% years)
    
    # Sort data
    combined_economic_data <- combined_economic_data %>%
      arrange(GEOID, year)
    
    # Cache the processed data
    saveRDS(combined_economic_data, cache_file)
    print_msg(paste("Cached economic data to:", cache_file))
    
    return(combined_economic_data)
  } else if (allow_simulation) {
    # Create simulated data
    print_msg("No economic data found. Creating simulated data...")
    
    # Economic variables to simulate
    economic_vars <- c(
      "employment_volatility_index" = "Index of employment stability/volatility",
      "job_growth_rate" = "Annual job growth rate",
      "income_inequality_ratio" = "Ratio of income at 80th percentile to income at 20th percentile",
      "economic_typology" = "County economic typology",
      "persistent_poverty_county" = "Flag for counties with persistent poverty",
      "persistent_child_poverty_county" = "Flag for counties with persistent child poverty",
      "economic_distress_index" = "Composite index of economic distress",
      "income_mobility_index" = "Measure of intergenerational economic mobility",
      "absolute_upward_mobility" = "Expected income rank for children from low-income families",
      "mean_commute_distance" = "Average commute distance",
      "job_density_index" = "Number of jobs within typical commute distance"
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
      for (var_name in names(economic_vars)) {
        if (var_name == "employment_volatility_index") {
          # Typically 0-10 scale
          year_data[[var_name]] <- runif(nrow(year_data), 0, 10)
        } else if (var_name == "job_growth_rate") {
          # Typically -5% to +10%
          year_data[[var_name]] <- runif(nrow(year_data), -5, 10)
        } else if (var_name == "income_inequality_ratio") {
          # Typically 3-8 range
          year_data[[var_name]] <- runif(nrow(year_data), 3, 8)
        } else if (var_name == "economic_typology") {
          # Categorical: farming, manufacturing, etc.
          types <- c("Farming", "Manufacturing", "Mining", "Government", "Recreation", "Nonspecialized")
          year_data[[var_name]] <- sample(types, nrow(year_data), replace = TRUE)
        } else if (var_name == "persistent_poverty_county" || var_name == "persistent_child_poverty_county") {
          # Binary: 0/1
          year_data[[var_name]] <- sample(c(0, 1), nrow(year_data), replace = TRUE, prob = c(0.85, 0.15))
        } else if (var_name == "economic_distress_index") {
          # Typically 0-100 scale
          year_data[[var_name]] <- runif(nrow(year_data), 0, 100)
        } else if (var_name == "income_mobility_index") {
          # Typically 0-100 scale
          year_data[[var_name]] <- runif(nrow(year_data), 20, 80)
        } else if (var_name == "absolute_upward_mobility") {
          # Typically 30-60 range (percentile)
          year_data[[var_name]] <- runif(nrow(year_data), 30, 60)
        } else if (var_name == "mean_commute_distance") {
          # Typically 5-30 miles
          year_data[[var_name]] <- runif(nrow(year_data), 5, 30)
        } else if (var_name == "job_density_index") {
          # Typically wide range, e.g., 0-5000
          year_data[[var_name]] <- runif(nrow(year_data), 0, 5000)
        } else {
          # Default - 0-100 range
          year_data[[var_name]] <- runif(nrow(year_data), 0, 100)
        }
        
        # Add quality flags
        year_data[[paste0(var_name, "_data_quality")]] <- data_quality_flags$simulated
        year_data[[paste0(var_name, "_data_source")]] <- "SIMULATED Economic Data"
        year_data[[paste0(var_name, "_data_vintage")]] <- paste0("simulated_", year)
      }
      
      sim_data_list[[as.character(year)]] <- year_data
    }
    
    # Combine all years
    simulated_data <- bind_rows(sim_data_list)
    
    # Cache the simulated data
    saveRDS(simulated_data, cache_file)
    print_msg(paste("Cached simulated economic data to:", cache_file))
    
    return(simulated_data)
  } else {
    # No data and simulation not allowed - create empty dataset with NAs
    print_msg("No economic data available and simulation not allowed. Creating empty dataset with NAs.")
    
    # Economic variables to include
    economic_vars <- c(
      "employment_volatility_index",
      "job_growth_rate",
      "income_inequality_ratio",
      "economic_typology",
      "persistent_poverty_county",
      "persistent_child_poverty_county",
      "economic_distress_index",
      "income_mobility_index",
      "absolute_upward_mobility",
      "mean_commute_distance",
      "job_density_index"
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
    for (var in economic_vars) {
      grid[[var]] <- NA_real_
      grid[[paste0(var, "_data_quality")]] <- data_quality_flags$missing
      grid[[paste0(var, "_data_source")]] <- "NOT_AVAILABLE"
      grid[[paste0(var, "_data_vintage")]] <- NA_character_
    }
    
    economic_data <- as_tibble(grid)
    print_msg(paste("Created empty economic dataset with", nrow(economic_data), "rows"))
    
    # Cache the empty data
    saveRDS(economic_data, cache_file)
    print_msg(paste("Cached empty economic data to:", cache_file))
    
    return(economic_data)
  }
}

# This lets the function be used when the script is sourced
is_sourced <- function() {
  return(!identical(environment(), globalenv()))
}

# If the script is run directly, test the function
if (!is_sourced()) {
  cat("Testing economic data fetcher...\n")
  
  # Get current year
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  
  # Test for last 5 years
  test_years <- (current_year-4):current_year
  
  # Test the function
  result <- fetch_economic_data(
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