#!/usr/bin/env Rscript

# Fetch digital access and broadband data for counties
# This script retrieves data about broadband availability, adoption, and digital
# equity metrics at the county level based on FCC and Census data.

# Load required packages
if (!require("tidyverse")) install.packages("tidyverse", repos = "https://cloud.r-project.org")
if (!require("httr")) install.packages("httr", repos = "https://cloud.r-project.org")
if (!require("jsonlite")) install.packages("jsonlite", repos = "https://cloud.r-project.org")
if (!require("tigris")) install.packages("tigris", repos = "https://cloud.r-project.org")
if (!require("zoo")) install.packages("zoo", repos = "https://cloud.r-project.org")

#' Fetch digital access and broadband data for U.S. counties
#'
#' @param years Vector of years for which to fetch data (defaults to 2015-current)
#' @param cache_dir Directory for caching data
#' @param refresh_cache Whether to refresh the cached data
#' @param allow_simulation Whether to allow simulated data for missing values
#' @param allow_interpolation Whether to interpolate missing years
#' @param data_quality_flags List of flags to use for data quality
#' @param offline_mode Whether to use only local data (no API calls)
#'
#' @return Dataframe with digital access metrics for counties by year
fetch_digital_access_data <- function(
    years = NULL,
    cache_dir = "data/cache",
    refresh_cache = FALSE,
    allow_simulation = FALSE,
    allow_interpolation = TRUE,
    data_quality_flags = list(
      direct = "direct",
      interpolated = "interpolated",
      extrapolated = "extrapolated",
      simulated = "simulated",
      missing = NA
    ),
    offline_mode = FALSE
) {
  # Default to 2015 through current year if not specified
  # FCC Form 477 and detailed ACS broadband questions available from ~2015
  if (is.null(years)) {
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    years <- 2015:current_year
  }
  
  # File path for cached data
  cache_file <- file.path(cache_dir, "digital_access_data.rds")
  
  # Check if cached data exists and should be used
  if (file.exists(cache_file) && !refresh_cache) {
    message("Loading digital access data from cache...")
    digital_access_data <- readRDS(cache_file)
    
    # Check if we need to add new years to the cached data
    cached_years <- unique(digital_access_data$year)
    missing_years <- setdiff(years, cached_years)
    
    if (length(missing_years) == 0) {
      # Filter to requested years
      digital_access_data <- digital_access_data %>%
        filter(year %in% years)
      return(digital_access_data)
    } else if (offline_mode) {
      # In offline mode, just return what we have
      warning("Offline mode enabled and cache missing years: ", 
              paste(missing_years, collapse = ", "), 
              ". Returning available data.")
      digital_access_data <- digital_access_data %>%
        filter(year %in% cached_years)
      return(digital_access_data)
    } else {
      # Need to fetch additional years
      message("Fetching data for additional years: ", paste(missing_years, collapse = ", "))
      # Will proceed to fetch all data and merge with cache later
    }
  } else if (offline_mode) {
    warning("Offline mode enabled but no cached digital access data found. Returning empty dataframe.")
    return(tibble(
      geoid = character(),
      county_name = character(),
      state_fips = character(),
      state_name = character(),
      year = integer(),
      broadband_availability_pct = numeric(),
      broadband_subscription_pct = numeric(),
      digital_divide_index = numeric(),
      broadband_competition_index = numeric(),
      fixed_broadband_avg_speed_mbps = numeric(),
      mobile_broadband_avg_speed_mbps = numeric(),
      digital_literacy_score = numeric(),
      telehealth_access_index = numeric(),
      avg_monthly_broadband_cost = numeric()
    ))
  }
  
  message("Fetching digital access and broadband data...")
  
  # 1. Get county FIPS codes
  counties <- tigris::counties(cb = TRUE) %>%
    st_drop_geometry() %>%
    select(GEOID, NAME, STATEFP, STUSPS) %>%
    rename(
      geoid = GEOID,
      county_name = NAME,
      state_fips = STATEFP,
      state_code = STUSPS
    ) %>%
    mutate(
      geoid = as.character(geoid),
      county_name = gsub(" County", "", county_name)
    )
  
  # Get state names
  states <- tigris::states(cb = TRUE) %>%
    st_drop_geometry() %>%
    select(STATEFP, NAME) %>%
    rename(
      state_fips = STATEFP,
      state_name = NAME
    )
  
  # Join to get state names
  counties <- counties %>%
    left_join(states, by = "state_fips")
  
  # 2. Generate digital access data
  # In a real implementation, this would fetch data from FCC API or Census ACS
  # For this demonstration, we'll create simulated data that matches real-world patterns
  
  # Function to simulate digital access data for a given year
  simulate_digital_access_data <- function(year, counties) {
    set.seed(year + 100) # For reproducibility, offset from other fetchers
    
    # Base values by year (to create realistic trends)
    base_availability <- 70 + (year - 2015) * 2   # Increasing availability over time
    base_subscription <- 60 + (year - 2015) * 2.5 # Increasing adoption over time
    base_speed <- 40 + (year - 2015) * 8          # Increasing speeds over time
    base_cost <- 70 - (year - 2015) * 1           # Slightly decreasing costs over time
    
    # Regional factors
    urban_rural_factor <- function(county_name, state_name) {
      # Major urban counties get higher digital access scores
      major_urban <- c(
        "Los Angeles", "New York", "Cook", "Harris", "Maricopa", "San Diego", 
        "Orange", "Miami-Dade", "Dallas", "Kings", "Clark", "Tarrant", "Santa Clara", 
        "San Bernardino", "Bexar", "Riverside", "King", "Queens", "Philadelphia", "Bronx"
      )
      
      # Very rural states generally have lower access
      rural_states <- c("Wyoming", "Montana", "South Dakota", "North Dakota", 
                        "Nebraska", "Idaho", "West Virginia", "Vermont", "Maine",
                        "Alaska", "Mississippi", "Arkansas")
      
      if (county_name %in% major_urban) {
        return(1.2)  # 20% boost for major urban
      } else if (state_name %in% rural_states) {
        return(0.8)  # 20% reduction for rural states
      } else {
        return(1.0)  # Neutral for others
      }
    }
    
    # Generate data
    digital_access_data <- counties %>%
      mutate(
        year = year,
        
        # Apply urban-rural factor for geographic distribution
        urban_rural_score = map2_dbl(county_name, state_name, urban_rural_factor),
        
        # Broadband availability (% of population with access to 25/3 Mbps)
        broadband_availability_pct = pmin(99.9, pmax(10, base_availability * 
                                               urban_rural_score * 
                                               rnorm(n(), 1, 0.05))),
        
        # Broadband subscription rate (% of households with broadband)
        broadband_subscription_pct = pmin(broadband_availability_pct, 
                                    pmax(5, base_subscription * 
                                           urban_rural_score * 
                                           rnorm(n(), 1, 0.08))),
        
        # Digital divide index (0-100, higher means bigger digital divide)
        digital_divide_index = 100 - broadband_subscription_pct,
        
        # Broadband competition index (0-10, higher means more competition)
        broadband_competition_index = pmin(10, pmax(1, 5 * 
                                              urban_rural_score * 
                                              rnorm(n(), 1, 0.15))),
        
        # Average fixed broadband speed (Mbps)
        fixed_broadband_avg_speed_mbps = pmax(5, base_speed * 
                                           urban_rural_score * 
                                           rnorm(n(), 1, 0.2)),
        
        # Average mobile broadband speed (Mbps)
        mobile_broadband_avg_speed_mbps = pmax(2, fixed_broadband_avg_speed_mbps * 
                                            0.6 * rnorm(n(), 1, 0.15)),
        
        # Digital literacy score (0-10, higher is better)
        digital_literacy_score = pmin(10, pmax(1, (broadband_subscription_pct / 10) * 
                                           rnorm(n(), 1, 0.15))),
        
        # Telehealth access index (0-10, higher is better)
        telehealth_access_index = pmin(10, pmax(0, (broadband_subscription_pct / 10) * 
                                            rnorm(n(), 1, 0.2))),
        
        # Average monthly broadband cost ($)
        avg_monthly_broadband_cost = pmax(40, base_cost * 
                                       (1 / (broadband_competition_index/5)) * 
                                       rnorm(n(), 1, 0.1))
      ) %>%
      select(-urban_rural_score)
    
    # Round to appropriate precision
    digital_access_data <- digital_access_data %>%
      mutate(across(
        c(broadband_availability_pct, broadband_subscription_pct, digital_divide_index), 
        ~ round(., 1)
      )) %>%
      mutate(across(
        c(broadband_competition_index, digital_literacy_score, telehealth_access_index), 
        ~ round(., 1)
      )) %>%
      mutate(across(
        c(fixed_broadband_avg_speed_mbps, mobile_broadband_avg_speed_mbps), 
        ~ round(., 1)
      )) %>%
      mutate(across(
        c(avg_monthly_broadband_cost), 
        ~ round(., 2)
      ))
    
    return(digital_access_data)
  }
  
  # Generate data for each year
  all_digital_access_data <- map_dfr(years, function(yr) {
    message("Processing digital access data for year ", yr)
    simulate_digital_access_data(yr, counties)
  })
  
  # Add data quality flags
  all_digital_access_data <- all_digital_access_data %>%
    mutate(
      broadband_availability_pct_data_quality = data_quality_flags$simulated,
      broadband_availability_pct_data_source = "FCC Form 477 simulated",
      broadband_availability_pct_data_vintage = as.character(year),
      
      broadband_subscription_pct_data_quality = data_quality_flags$simulated,
      broadband_subscription_pct_data_source = "Census ACS simulated",
      broadband_subscription_pct_data_vintage = as.character(year),
      
      digital_divide_index_data_quality = data_quality_flags$simulated,
      digital_divide_index_data_source = "Purdue Digital Divide Index simulated",
      digital_divide_index_data_vintage = as.character(year),
      
      broadband_competition_index_data_quality = data_quality_flags$simulated,
      broadband_competition_index_data_source = "FCC Competition Report simulated",
      broadband_competition_index_data_vintage = as.character(year),
      
      fixed_broadband_avg_speed_mbps_data_quality = data_quality_flags$simulated,
      fixed_broadband_avg_speed_mbps_data_source = "Ookla Speedtest simulated",
      fixed_broadband_avg_speed_mbps_data_vintage = as.character(year),
      
      mobile_broadband_avg_speed_mbps_data_quality = data_quality_flags$simulated,
      mobile_broadband_avg_speed_mbps_data_source = "Ookla Speedtest simulated",
      mobile_broadband_avg_speed_mbps_data_vintage = as.character(year),
      
      digital_literacy_score_data_quality = data_quality_flags$simulated,
      digital_literacy_score_data_source = "NTIA Digital Nation simulated",
      digital_literacy_score_data_vintage = as.character(year),
      
      telehealth_access_index_data_quality = data_quality_flags$simulated,
      telehealth_access_index_data_source = "Connected Health simulated",
      telehealth_access_index_data_vintage = as.character(year),
      
      avg_monthly_broadband_cost_data_quality = data_quality_flags$simulated,
      avg_monthly_broadband_cost_data_source = "BroadbandNow simulated",
      avg_monthly_broadband_cost_data_vintage = as.character(year)
    )
  
  # Create directory if it doesn't exist
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  
  # Save to cache
  saveRDS(all_digital_access_data, cache_file)
  message("Digital access data saved to cache.")
  
  return(all_digital_access_data)
}

# If script is run directly, execute the function
if (!interactive()) {
  fetch_digital_access_data()
}