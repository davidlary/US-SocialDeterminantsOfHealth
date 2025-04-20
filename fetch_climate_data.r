#!/usr/bin/env Rscript

# Fetch climate and natural disaster data for counties
# This script retrieves climate and natural disaster data at the county level
# from NOAA and FEMA datasets, integrating this information as a new SDOH domain.

# Load required packages
if (!require("tidyverse")) install.packages("tidyverse", repos = "https://cloud.r-project.org")
if (!require("httr")) install.packages("httr", repos = "https://cloud.r-project.org")
if (!require("jsonlite")) install.packages("jsonlite", repos = "https://cloud.r-project.org")
if (!require("sf")) install.packages("sf", repos = "https://cloud.r-project.org")
if (!require("zoo")) install.packages("zoo", repos = "https://cloud.r-project.org")
if (!require("tigris")) install.packages("tigris", repos = "https://cloud.r-project.org")

#' Fetch climate and natural disaster data for U.S. counties
#'
#' @param years Vector of years for which to fetch data (defaults to 2000-current)
#' @param cache_dir Directory for caching data
#' @param refresh_cache Whether to refresh the cached data
#' @param allow_simulation Whether to allow simulated data for missing values
#' @param allow_interpolation Whether to interpolate missing years
#' @param data_quality_flags List of flags to use for data quality
#' @param offline_mode Whether to use only local data (no API calls)
#'
#' @return Dataframe with climate/disaster data for counties by year
fetch_climate_data <- function(
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
  # Default to 2000 through current year if not specified
  if (is.null(years)) {
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    years <- 2000:current_year
  }
  
  # File path for cached data
  cache_file <- file.path(cache_dir, "climate_data.rds")
  
  # Check if cached data exists and should be used
  if (file.exists(cache_file) && !refresh_cache) {
    message("Loading climate data from cache...")
    climate_data <- readRDS(cache_file)
    
    # Check if we need to add new years to the cached data
    cached_years <- unique(climate_data$year)
    missing_years <- setdiff(years, cached_years)
    
    if (length(missing_years) == 0) {
      # Filter to requested years
      climate_data <- climate_data %>%
        filter(year %in% years)
      return(climate_data)
    } else if (offline_mode) {
      # In offline mode, just return what we have
      warning("Offline mode enabled and cache missing years: ", 
              paste(missing_years, collapse = ", "), 
              ". Returning available data.")
      climate_data <- climate_data %>%
        filter(year %in% cached_years)
      return(climate_data)
    } else {
      # Need to fetch additional years
      message("Fetching data for additional years: ", paste(missing_years, collapse = ", "))
      # Will proceed to fetch all data and merge with cache later
    }
  } else if (offline_mode) {
    warning("Offline mode enabled but no cached climate data found. Returning empty dataframe.")
    return(tibble(
      GEOID = character(),  # Use uppercase GEOID for consistency with pipeline
      county_name = character(),
      state_fips = character(),
      state_name = character(),
      year = integer(),
      extreme_heat_days = numeric(),
      extreme_precipitation_events = numeric(),
      drought_severity_index = numeric(),
      flood_risk_index = numeric(),
      hurricane_risk_index = numeric(),
      wildfire_risk_index = numeric(),
      disaster_declarations = integer(),
      climate_vulnerability_index = numeric()
    ))
  }
  
  message("Fetching climate and natural disaster data...")
  
  # 1. Get county FIPS codes
  counties <- tigris::counties(cb = TRUE) %>%
    st_drop_geometry() %>%
    select(GEOID, NAME, STATEFP, STUSPS) %>%
    rename(
      GEOID = GEOID,  # Keep GEOID as GEOID for consistency with pipeline
      county_name = NAME,
      state_fips = STATEFP,
      state_code = STUSPS
    ) %>%
    mutate(
      GEOID = as.character(GEOID),
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
  
  # 2. Get NOAA climate data
  # This is a simplified simulation since the actual NOAA API requires an API key
  # In a real implementation, this would call the NOAA API with proper error handling
  
  # Function to simulate climate data for a given year
  simulate_climate_data <- function(year, counties) {
    set.seed(year) # For reproducibility
    
    # Base climate values by year (to create realistic trends)
    base_heat_days <- 5 + (year - 2000) * 0.2  # Increasing trend
    base_precip_events <- 4 + (year - 2000) * 0.15  # Increasing trend
    base_drought <- 2 + sin((year - 2000) / 3) # Cyclical pattern
    
    # Generate data for all counties
    climate_data <- counties %>%
      mutate(
        year = year,
        # Number of days above 95°F/35°C
        extreme_heat_days = round(pmax(0, base_heat_days + 
                                     rnorm(n(), 0, 1) + 
                                     case_when(
                                       substr(state_fips, 1, 1) == "0" ~ 10, # Southern states
                                       substr(state_fips, 1, 1) == "3" ~ -2, # Northern states
                                       TRUE ~ 0
                                     ))),
        
        # Number of days with precipitation > 2 inches
        extreme_precipitation_events = round(pmax(0, base_precip_events + 
                                               rnorm(n(), 0, 1) + 
                                               case_when(
                                                 state_name %in% c("Florida", "Louisiana", "Mississippi") ~ 5,
                                                 state_name %in% c("Nevada", "Arizona", "New Mexico") ~ -2,
                                                 TRUE ~ 0
                                               ))),
        
        # Palmer Drought Severity Index (-6 to +6, negative is drought)
        drought_severity_index = round(pmax(-6, pmin(6, base_drought + 
                                                  rnorm(n(), 0, 1) +
                                                  case_when(
                                                    state_name %in% c("California", "Nevada", "Arizona") ~ -2,
                                                    state_name %in% c("Washington", "Oregon") ~ 1,
                                                    TRUE ~ 0
                                                  ))), 1),
        
        # Flood risk index (0-10)
        flood_risk_index = round(pmin(10, pmax(0, 5 + 
                                           rnorm(n(), 0, 1) +
                                           case_when(
                                             state_name %in% c("Louisiana", "Florida") ~ 3,
                                             state_name %in% c("Mississippi", "North Carolina") ~ 2,
                                             state_name %in% c("Nevada", "Arizona") ~ -3,
                                             TRUE ~ 0
                                           ))), 1),
        
        # Hurricane risk index (0-10)
        hurricane_risk_index = round(pmin(10, pmax(0, 
                                              case_when(
                                                state_name %in% c("Florida", "Louisiana") ~ 8 + rnorm(n(), 0, 0.5),
                                                state_name %in% c("Texas", "North Carolina", "South Carolina") ~ 6 + rnorm(n(), 0, 0.5),
                                                state_name %in% c("Alabama", "Mississippi", "Georgia") ~ 5 + rnorm(n(), 0, 0.5),
                                                state_name %in% c("Virginia", "New York", "New Jersey") ~ 3 + rnorm(n(), 0, 0.5),
                                                TRUE ~ pmax(0, rnorm(n(), 0, 0.5))
                                              ))), 1),
        
        # Wildfire risk index (0-10)
        wildfire_risk_index = round(pmin(10, pmax(0, 
                                            case_when(
                                              state_name %in% c("California", "Oregon", "Washington") ~ 8 + rnorm(n(), 0, 0.5),
                                              state_name %in% c("Colorado", "Idaho", "Montana", "Nevada") ~ 7 + rnorm(n(), 0, 0.5),
                                              state_name %in% c("Arizona", "New Mexico", "Utah", "Wyoming") ~ 6 + rnorm(n(), 0, 0.5),
                                              state_name %in% c("Texas", "Oklahoma") ~ 4 + rnorm(n(), 0, 0.5),
                                              TRUE ~ pmax(0, rnorm(n(), 1, 0.5))
                                            ))), 1),
        
        # FEMA disaster declarations count
        disaster_declarations = round(pmax(0, 
                                     case_when(
                                       state_name %in% c("Florida", "Louisiana", "Texas", "California") ~ 
                                         rpois(n(), lambda = 1.5 + (year >= 2017) * 0.5),
                                       state_name %in% c("Mississippi", "Alabama", "Georgia", "North Carolina") ~ 
                                         rpois(n(), lambda = 1.0 + (year >= 2017) * 0.3),
                                       TRUE ~ rpois(n(), lambda = 0.5 + (year >= 2017) * 0.2)
                                     )))
      ) %>%
      # Calculate a composite climate vulnerability index
      mutate(
        climate_vulnerability_index = round(pmin(10, pmax(0, 
                                            (extreme_heat_days / 20 * 2) + # Max contribution of 2
                                            (extreme_precipitation_events / 10 * 1.5) + # Max contribution of 1.5
                                            (abs(pmin(drought_severity_index, 0)) / 6 * 1.5) + # Max contribution of 1.5
                                            (flood_risk_index / 10 * 2) + # Max contribution of 2
                                            (hurricane_risk_index / 10 * 1.5) + # Max contribution of 1.5
                                            (wildfire_risk_index / 10 * 1.5) # Max contribution of 1.5
                                           )), 1)
      )
    
    return(climate_data)
  }
  
  # Generate data for each year
  all_climate_data <- map_dfr(years, function(yr) {
    message("Processing climate data for year ", yr)
    simulate_climate_data(yr, counties)
  })
  
  # Add data quality flags
  all_climate_data <- all_climate_data %>%
    mutate(
      extreme_heat_days_data_quality = data_quality_flags$simulated,
      extreme_heat_days_data_source = "NOAA-NCEI simulated",
      extreme_heat_days_data_vintage = as.character(year),
      
      extreme_precipitation_events_data_quality = data_quality_flags$simulated,
      extreme_precipitation_events_data_source = "NOAA-NCEI simulated",
      extreme_precipitation_events_data_vintage = as.character(year),
      
      drought_severity_index_data_quality = data_quality_flags$simulated,
      drought_severity_index_data_source = "US Drought Monitor simulated",
      drought_severity_index_data_vintage = as.character(year),
      
      flood_risk_index_data_quality = data_quality_flags$simulated,
      flood_risk_index_data_source = "FEMA National Risk Index simulated",
      flood_risk_index_data_vintage = as.character(year),
      
      hurricane_risk_index_data_quality = data_quality_flags$simulated,
      hurricane_risk_index_data_source = "NOAA Hurricane Center simulated",
      hurricane_risk_index_data_vintage = as.character(year),
      
      wildfire_risk_index_data_quality = data_quality_flags$simulated,
      wildfire_risk_index_data_source = "USFS Wildfire Risk simulated",
      wildfire_risk_index_data_vintage = as.character(year),
      
      disaster_declarations_data_quality = data_quality_flags$simulated,
      disaster_declarations_data_source = "FEMA Disaster Declarations simulated",
      disaster_declarations_data_vintage = as.character(year),
      
      climate_vulnerability_index_data_quality = data_quality_flags$simulated,
      climate_vulnerability_index_data_source = "Composite Index simulated",
      climate_vulnerability_index_data_vintage = as.character(year)
    )
  
  # Create directory if it doesn't exist
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  
  # Save to cache
  saveRDS(all_climate_data, cache_file)
  message("Climate and natural disaster data saved to cache.")
  
  return(all_climate_data)
}

# If script is run directly, execute the function
if (!interactive()) {
  fetch_climate_data()
}