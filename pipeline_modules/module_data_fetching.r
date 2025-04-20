#!/usr/bin/env Rscript

# module_data_fetching.r
# Data fetching module for the SDOH pipeline

# Load required packages
library(dplyr)
library(readr)
library(httr)

# Make sure we have the log_message function
if (!exists("log_message")) {
  log_message <- function(message, level = "INFO", show_console = TRUE, log_file = NULL) {
    timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
    formatted_message <- paste(timestamp, "[", level, "]", message)
    
    if (show_console) {
      cat(formatted_message, "\n")
    }
    
    if (!is.null(log_file)) {
      if (!dir.exists(dirname(log_file)) && dirname(log_file) != ".") {
        dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
      }
      cat(formatted_message, "\n", file = log_file, append = TRUE)
    }
    
    return(formatted_message)
  }
}

#' Fetch Census data for counties
#'
#' This function fetches data from the U.S. Census Bureau's API
#' for the specified years and variables.
#'
#' @param crosswalk Variable crosswalk containing Census variables
#' @param years Vector of years to fetch data for
#' @param refresh_cache Whether to refresh the cache
#' @param use_cache Whether to use cached data if available
#' @return A dataframe with Census data
get_census_data <- function(crosswalk, years, refresh_cache = FALSE, use_cache = TRUE) {
  # For the modular demo version, we'll just return a simulated dataset
  log_message("Using simulated Census data (demo mode)",
             level = "INFO", show_console = TRUE)
  
  # Create simulated Census data
  counties <- data.frame(
    geoid = sprintf("%05d", 1:3000),
    name = paste("County", 1:3000),
    state_fips = rep(sprintf("%02d", 1:50), each = 60),
    state_name = rep(state.name, each = 60)[1:3000]
  )
  
  # Add key Census variables from the crosswalk
  census_vars <- crosswalk %>%
    filter(grepl("American Community Survey|US Census Bureau", source)) %>%
    pull(variable_name)
  
  # Create data for each year
  census_data <- list()
  
  for (year in years) {
    if (year >= 2000 && year <= 2023) {  # Only include realistic years
      year_data <- counties
      year_data$year <- year
      
      # Add some sample data for key variables
      for (var in census_vars) {
        # Generate random values appropriate for this variable
        if (grepl("population", var)) {
          year_data[[var]] <- round(runif(nrow(year_data), 1000, 1000000))
        } else if (grepl("income", var)) {
          year_data[[var]] <- round(runif(nrow(year_data), 30000, 150000))
        } else if (grepl("rate|pct", var)) {
          year_data[[var]] <- round(runif(nrow(year_data), 1, 30), 1)
        } else {
          year_data[[var]] <- round(runif(nrow(year_data), 0, 100))
        }
      }
      
      census_data[[as.character(year)]] <- year_data
    }
  }
  
  # Combine all years
  combined_census_data <- do.call(rbind, census_data)
  
  log_message(paste("Simulated Census data created with", nrow(combined_census_data), "rows"),
             level = "INFO", show_console = TRUE)
  
  return(combined_census_data)
}

#' Process and combine data from all sources
#'
#' This function processes and combines data from Census, NHGIS,
#' and other sources into a unified dataset.
#'
#' @param census_data Dataframe with Census data
#' @param nhgis_data Dataframe with NHGIS data (can be NULL)
#' @param years Vector of years to process
#' @param crosswalk Variable crosswalk
#' @return A processed dataset with all variables
get_processed_data <- function(census_data, nhgis_data, years, crosswalk) {
  # This function simulates a processed dataset for the example
  # In the actual implementation, it would process and combine data from different sources
  
  if (is.null(census_data)) {
    # Create a basic dataset with county IDs and years
    counties <- data.frame(
      geoid = sprintf("%05d", 1:3000),
      name = paste("County", 1:3000),
      state_fips = rep(sprintf("%02d", 1:50), each = 60),
      state_name = rep(state.name[1:50], each = 60)
    )
    
    # Create a dataset with all years and counties
    years_df <- expand.grid(
      geoid = counties$geoid,
      year = years
    )
    
    # Merge counties info
    full_data <- merge(years_df, counties, by = "geoid")
  } else {
    # Use the Census data as the base
    full_data <- census_data
  }
  
  # Add more variables not already in the Census data
  for (var in crosswalk$variable_name) {
    if (!var %in% names(full_data)) {
      # Generate random values appropriate for this variable
      if (grepl("population", var)) {
        full_data[[var]] <- round(runif(nrow(full_data), 1000, 1000000))
      } else if (grepl("income|earnings", var)) {
        full_data[[var]] <- round(runif(nrow(full_data), 30000, 150000))
      } else if (grepl("rate|pct", var)) {
        full_data[[var]] <- round(runif(nrow(full_data), 1, 30), 1)
      } else if (grepl("fatalities|deaths", var)) {
        full_data[[var]] <- round(runif(nrow(full_data), 0, 500))
      } else if (grepl("expectancy", var)) {
        full_data[[var]] <- round(runif(nrow(full_data), 65, 85), 1)
      } else {
        full_data[[var]] <- round(runif(nrow(full_data), 0, 100))
      }
    }
    
    # Add interpolation flags for some data points
    interp_col <- paste0(var, "_interpolated")
    full_data[[interp_col]] <- sample(c(TRUE, FALSE), nrow(full_data), replace = TRUE, prob = c(0.2, 0.8))
  }
  
  log_message(paste("Processed data created with", nrow(full_data), "rows and", ncol(full_data), "columns"),
             level = "INFO", show_console = TRUE)
  
  return(full_data)
}

# Only run if executed directly (not sourced)
if (!exists("is_sourced") || !is_sourced()) {
  message("Data fetching module cannot be run directly. Use the unified pipeline.")
}