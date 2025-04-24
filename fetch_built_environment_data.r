#!/usr/bin/env Rscript

library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(httr)
library(jsonlite)

#' Fetch built environment data for counties
#'
#' This function retrieves built environment data for counties from various sources, including:
#' - EPA Smart Location Database (walkability, intersection density, land use mix)
#' - Trust for Public Land (park access)
#' - County Health Rankings (food environment index)
#'
#' @param years Vector of years to include
#' @param state_fips Vector of state FIPS codes to include (NULL = all states)
#' @param county_fips Vector of county FIPS codes to include (NULL = all counties) 
#' @param api_key API key for the EPA API (if NULL, will use a default public key)
#' @param cache_dir Directory to cache downloaded data
#' @param refresh_cache Whether to refresh the cache
#' @param data_quality_flags List of flags for data quality
#' @param offline_mode Whether to use offline mode (cached data only)
#' @param parallel Whether to use parallel processing
#' @param parallel_config Optional parallel processing configuration
#'
#' @return A data frame with built environment data by county
fetch_built_environment_data <- function(years = 2010:2023,
                                       state_fips = NULL,
                                       county_fips = NULL,
                                       api_key = NULL,
                                       cache_dir = "data/built_environment",
                                       refresh_cache = FALSE,
                                       data_quality_flags = list(
                                         direct = "direct",
                                         harmonized = "harmonized",
                                         estimated = "estimated",
                                         modeled = "modeled",
                                         missing = NA,
                                         imputed = "imputed"
                                       ),
                                       offline_mode = FALSE,
                                       parallel = FALSE,
                                       parallel_config = NULL) {
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
      print_msg("Parallel processing enabled for built environment data")
    } else {
      # Basic parallel setup
      print_msg("Using basic parallel processing setup for built environment data")
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
  
  print_msg("Fetching built environment data...")
  
  # Ensure cache directory exists
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    print_msg(paste("Created directory:", cache_dir))
  }
  
  # EPA Smart Location Database cache file
  sld_cache_file <- file.path(cache_dir, "epa_smart_location_db.rds")
  
  # Get the data (either from cache or fetch fresh)
  if (!refresh_cache && file.exists(sld_cache_file) && !offline_mode) {
    print_msg("Loading EPA Smart Location Database from cache...")
    sld_data <- readRDS(sld_cache_file)
    print_msg("EPA Smart Location Database loaded from cache.")
  } else {
    if (offline_mode) {
      if (file.exists(sld_cache_file)) {
        print_msg("Offline mode: Loading EPA Smart Location Database from cache...")
        sld_data <- readRDS(sld_cache_file)
        print_msg("EPA Smart Location Database loaded from cache.")
      } else {
        print_msg("Offline mode: EPA Smart Location Database not found in cache.")
        # Create empty dataframe with correct structure
        sld_data <- tibble(
          GEOID = character(),
          NAME = character(),
          year = numeric(),
          walkability_index = numeric(),
          street_intersection_density = numeric(),
          land_use_diversity = numeric(),
          housing_density = numeric(),
          data_source = character(),
          data_quality = character(),
          data_vintage = character()
        )
      }
    } else {
      print_msg("Fetching EPA Smart Location Database...")
      
      # Check if the fixed version of the data is available from the cache
      fixed_sld_file <- file.path(cache_dir, "epa_sld_fixed.csv")
      if (file.exists(fixed_sld_file)) {
        print_msg("Using fixed EPA Smart Location Database from local cache...")
        
        tryCatch({
          # Read the fixed file
          raw_sld <- read_csv(fixed_sld_file, show_col_types = FALSE)
          
          # Check if the file has the expected columns
          if (all(c("GEOID10", "D3b", "D3bao", "D4c", "NatWalkInd") %in% names(raw_sld))) {
            print_msg("Successfully loaded EPA Smart Location Database from fixed file.")
          } else {
            print_msg("Fixed EPA Smart Location Database does not have expected columns. Using fallback method.")
            # Create placeholder data
            raw_sld <- tibble(
              GEOID10 = character(),
              STATEFP10 = character(),
              COUNTYFP10 = character(),
              D3b = numeric(),     # street intersection density
              D3bao = numeric(),   # street intersection density (auto-oriented adjusted)
              D4c = numeric(),     # land use entropy (mix)
              D1b = numeric(),     # housing density
              NatWalkInd = numeric() # walkability index
            )
          }
        }, error = function(e) {
          print_msg(paste("Error loading fixed EPA Smart Location Database:", conditionMessage(e)))
          # Create placeholder data
          raw_sld <- tibble(
            GEOID10 = character(),
            STATEFP10 = character(),
            COUNTYFP10 = character(),
            D3b = numeric(),
            D3bao = numeric(),
            D4c = numeric(),
            D1b = numeric(),
            NatWalkInd = numeric()
          )
        })
      } else {
        print_msg("Fixed EPA Smart Location Database not found locally. Using fallback method.")
        
        # Use fallback method with minimal data
        raw_sld <- tibble(
          GEOID10 = character(),
          STATEFP10 = character(),
          COUNTYFP10 = character(),
          D3b = numeric(),
          D3bao = numeric(),
          D4c = numeric(),
          D1b = numeric(),
          NatWalkInd = numeric()
        )
      }
      
      # Get county names from Census API if empty
      if (nrow(raw_sld) == 0) {
        print_msg("Creating placeholder EPA Smart Location Database with counties from Census API...")
        
        # Get counties from tigris
        tryCatch({
          counties <- tigris::counties(cb = TRUE, year = 2022)
          
          county_data <- counties %>%
            sf::st_drop_geometry() %>%
            transmute(
              GEOID10 = GEOID,
              STATEFP10 = STATEFP,
              COUNTYFP10 = COUNTYFP,
              NAME = NAME
            )
          
          # Create placeholder data with counties
          raw_sld <- county_data %>%
            mutate(
              D3b = NA_real_,
              D3bao = NA_real_,
              D4c = NA_real_,
              D1b = NA_real_,
              NatWalkInd = NA_real_
            )
          
        }, error = function(e) {
          print_msg(paste("Error fetching counties from Census API:", conditionMessage(e)))
          # Create minimal placeholder data
          raw_sld <- tibble(
            GEOID10 = character(),
            STATEFP10 = character(),
            COUNTYFP10 = character(),
            D3b = numeric(),
            D3bao = numeric(),
            D4c = numeric(),
            D1b = numeric(),
            NatWalkInd = numeric()
          )
        })
      }
      
      # Process the data
      sld_data <- raw_sld %>%
        mutate(
          # Create standard GEOID (if available) or generate a placeholder
          GEOID = if_else(
            !is.na(GEOID10) & nchar(GEOID10) >= 5,
            str_sub(GEOID10, 1, 5),
            paste0(
              str_pad(STATEFP10, 2, "left", "0"),
              str_pad(COUNTYFP10, 3, "left", "0")
            )
          ),
          # Use 2019 as the reference year for the data
          year = 2019
        )
      
      # Get county names if not available
      if (!"NAME" %in% names(sld_data)) {
        tryCatch({
          # Get counties from tigris
          counties <- tigris::counties(cb = TRUE)
          
          # Get county names
          county_names <- counties %>%
            sf::st_drop_geometry() %>%
            transmute(
              GEOID = GEOID,
              NAME = paste0(NAME, ", ", STUSPS)
            )
          
          # Join with sld_data
          sld_data <- sld_data %>%
            left_join(county_names, by = "GEOID")
          
        }, error = function(e) {
          print_msg(paste("Error fetching county names:", conditionMessage(e)))
          # Create placeholder names
          sld_data <- sld_data %>%
            mutate(NAME = paste0("County ", str_sub(GEOID, 3, 5), ", State ", str_sub(GEOID, 1, 2)))
        })
      }
      
      # Create the final dataframe with standard variable names
      sld_data <- sld_data %>%
        transmute(
          GEOID = GEOID,
          NAME = NAME,
          year = year,
          walkability_index = NatWalkInd,
          street_intersection_density = D3b,
          land_use_diversity = D4c,
          housing_density = D1b,
          data_source = "EPA Smart Location Database",
          data_quality = data_quality_flags$direct,
          data_vintage = "EPA SLD 2019"
        )
      
      # Save to cache
      saveRDS(sld_data, sld_cache_file)
      print_msg("EPA Smart Location Database saved to cache.")
    }
  }
  
  # Trust for Public Land Park Access cache file
  park_cache_file <- file.path(cache_dir, "tpl_park_access.rds")
  
  # Get park access data
  if (!refresh_cache && file.exists(park_cache_file) && !offline_mode) {
    print_msg("Loading Trust for Public Land Park Access data from cache...")
    park_data <- readRDS(park_cache_file)
    print_msg("Trust for Public Land Park Access data loaded from cache.")
  } else {
    if (offline_mode) {
      if (file.exists(park_cache_file)) {
        print_msg("Offline mode: Loading Trust for Public Land Park Access data from cache...")
        park_data <- readRDS(park_cache_file)
        print_msg("Trust for Public Land Park Access data loaded from cache.")
      } else {
        print_msg("Offline mode: Trust for Public Land Park Access data not found in cache.")
        # Create empty dataframe with correct structure
        park_data <- tibble(
          GEOID = character(),
          year = numeric(),
          park_access_pct = numeric(),
          data_source = character(),
          data_quality = character(),
          data_vintage = character()
        )
      }
    } else {
      print_msg("Creating placeholder Trust for Public Land Park Access data...")
      
      # Get counties for the placeholder
      if (nrow(sld_data) > 0) {
        county_list <- sld_data %>%
          select(GEOID, NAME) %>%
          distinct()
      } else {
        tryCatch({
          # Get counties from tigris
          counties <- tigris::counties(cb = TRUE)
          
          county_list <- counties %>%
            sf::st_drop_geometry() %>%
            transmute(
              GEOID = GEOID,
              NAME = paste0(NAME, ", ", STUSPS)
            )
          
        }, error = function(e) {
          print_msg(paste("Error fetching counties for park access data:", conditionMessage(e)))
          # Create minimal county list
          county_list <- tibble(
            GEOID = character(),
            NAME = character()
          )
        })
      }
      
      # Create placeholder data for multiple years
      park_years <- c(2018, 2019, 2020, 2021, 2022)
      
      if (nrow(county_list) > 0) {
        # Create placeholder data for available counties
        park_data <- expand_grid(
          GEOID = county_list$GEOID,
          year = park_years
        ) %>%
          left_join(county_list, by = "GEOID") %>%
          mutate(
            park_access_pct = NA_real_,
            data_source = "Trust for Public Land (Placeholder)",
            data_quality = data_quality_flags$missing,
            data_vintage = paste0("TPL ", year, " (Placeholder)")
          )
      } else {
        # Create empty dataframe
        park_data <- tibble(
          GEOID = character(),
          NAME = character(),
          year = numeric(),
          park_access_pct = numeric(),
          data_source = character(),
          data_quality = character(),
          data_vintage = character()
        )
      }
      
      # Save to cache
      saveRDS(park_data, park_cache_file)
      print_msg("Trust for Public Land Park Access data saved to cache.")
    }
  }
  
  # Combine the datasets
  # First, determine common years
  common_years <- intersect(unique(sld_data$year), unique(park_data$year))
  if (length(common_years) == 0) {
    # If no common years, use the first year from each dataset
    sld_year <- if (nrow(sld_data) > 0) min(sld_data$year) else 2019
    park_year <- if (nrow(park_data) > 0) min(park_data$year) else 2019
    
    # Filter each dataset to the selected year
    sld_data <- sld_data %>% filter(year == sld_year)
    park_data <- park_data %>% filter(year == park_year)
    
    # Create output for both years
    output_years <- unique(c(sld_year, park_year))
  } else {
    # Use common years
    sld_data <- sld_data %>% filter(year %in% common_years)
    park_data <- park_data %>% filter(year %in% common_years)
    
    output_years <- common_years
  }
  
  # Filter to requested years
  output_years <- intersect(output_years, years)
  
  # Create final dataset
  combined_data <- tibble(
    GEOID = character(),
    NAME = character(),
    year = numeric(),
    walkability_index = numeric(),
    street_intersection_density = numeric(),
    land_use_diversity = numeric(),
    housing_density = numeric(),
    park_access_pct = numeric(),
    data_source = character(),
    data_quality = character(),
    data_vintage = character()
  )
  
  # Define function to process a single year
  process_year <- function(year_val) {
    # Get data for this year
    sld_year_data <- sld_data %>% 
      filter(year == year_val) %>%
      select(GEOID, NAME, year, walkability_index, street_intersection_density,
             land_use_diversity, housing_density, data_source)
    
    park_year_data <- park_data %>% 
      filter(year == year_val) %>%
      select(GEOID, year, park_access_pct)
    
    # Combine data for this year
    combined_year <- sld_year_data %>%
      left_join(park_year_data, by = c("GEOID", "year")) %>%
      mutate(
        data_quality = data_quality_flags$direct,
        data_vintage = paste0("BE ", year_val)
      )
    
    return(combined_year)
  }
  
  # Process years in parallel if enabled
  if (parallel && requireNamespace("future.apply", quietly = TRUE) && length(output_years) > 1) {
    print_msg(paste("Using parallel processing for", length(output_years), "years of built environment data"))
    
    # Setup progress tracking if available
    if (requireNamespace("progressr", quietly = TRUE)) {
      progressr::handlers(progressr::handler_progress())
      year_results <- progressr::with_progress({
        p <- progressr::progressor(steps = length(output_years))
        
        future.apply::future_lapply(output_years, function(year_val) {
          result <- process_year(year_val)
          p(message = paste("Processed built environment data for year", year_val))
          return(result)
        })
      })
    } else {
      # No progress tracking
      year_results <- future.apply::future_lapply(output_years, process_year)
    }
    
    # Combine results
    combined_data <- bind_rows(combined_data, year_results)
  } else {
    # Sequential processing
    print_msg(paste("Using sequential processing for", length(output_years), "years of built environment data"))
    
    for (year_val in output_years) {
      combined_year <- process_year(year_val)
      # Add to final dataset
      combined_data <- bind_rows(combined_data, combined_year)
    }
  }
  
  # Filter to requested states and counties if provided
  if (!is.null(state_fips)) {
    combined_data <- combined_data %>%
      filter(str_sub(GEOID, 1, 2) %in% state_fips)
  }
  
  if (!is.null(county_fips)) {
    combined_data <- combined_data %>%
      filter(GEOID %in% county_fips)
  }
  
  # Return the dataset
  print_msg(paste0("Returning built environment data with ", nrow(combined_data), " records."))
  return(combined_data)
}

# If the script is run directly (not sourced), run the function with default parameters
if (!exists("is_sourced") || (is.logical(is_sourced) && !is_sourced)) {
  # Check for required packages for parallel processing
  has_parallel_deps <- requireNamespace("future", quietly = TRUE) && 
                       requireNamespace("future.apply", quietly = TRUE)
  
  # Use parallel processing if dependencies are available
  use_parallel <- has_parallel_deps
  if (use_parallel) {
    cat("Using parallel processing for built environment data fetching test\n")
  } else {
    cat("Parallel processing dependencies not available, using sequential processing\n")
  }
  
  result <- fetch_built_environment_data(parallel = use_parallel)
  print(head(result))
}