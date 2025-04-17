#!/usr/bin/env Rscript

# Generate county maps for visualization of SDOH variables
# This script creates choropleth maps for various social determinants of health

library(tidyverse)
library(sf)
library(tigris)
library(ggplot2)
library(DBI)
library(duckdb)

# Check if viridis is installed, and install if not
tryCatch({
  if (!requireNamespace("viridis", quietly = TRUE)) {
    cat("Installing viridis package...\n")
    install.packages("viridis", repos = "https://cloud.r-project.org")
  }
  library(viridis)
}, error = function(e) {
  cat("Error loading viridis package:", conditionMessage(e), "\n")
  cat("Maps will have basic coloring instead of viridis palettes.\n")
})

# Source the shapefile utilities
source("utilities/fetch_county_shapefiles.r")

#' Generate choropleth maps for selected SDOH variables
#'
#' This function creates county-level choropleth maps for visualization of 
#' social determinants of health variables across different years.
#'
#' @param database_path Path to the DuckDB database
#' @param years Vector of years for which to create maps
#' @param variables Vector of variables to map
#' @param output_dir Directory to save the maps
#' @param shapefile_dir Directory where shapefiles are stored
#' @return A list of ggplot objects with the generated maps
generate_county_maps <- function(database_path = "us_county_sdoh_data.duckdb",
                              years = c(1970, 1980, 1990, 2000, 2010, 2020),
                              variables = c("poverty_rate", "median_household_income", "unemployment_rate", 
                                           "uninsured_pct", "obesity_pct", "life_expectancy"),
                              output_dir = "output/maps",
                              shapefile_dir = "data/shapefiles") {
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    cat("Created output directory:", output_dir, "\n")
  }
  
  # Load shapefiles for all years
  shapefile_years <- c(1990, 2000, 2010, 2020)
  cat("Loading county shapefiles for years:", paste(shapefile_years, collapse = ", "), "\n")
  
  county_shapefiles <- fetch_county_shapefiles(years = shapefile_years, 
                                            shapefile_dir = shapefile_dir)
  
  # Connect to the database
  cat("Connecting to database:", database_path, "\n")
  con <- dbConnect(duckdb(), database_path)
  
  # Function to assign shapefile by year
  get_shapefile_for_year <- function(year) {
    if (year <= 1995) {
      return(county_shapefiles[["1990"]])
    } else if (year <= 2005) {
      return(county_shapefiles[["2000"]])
    } else if (year <= 2015) {
      return(county_shapefiles[["2010"]])
    } else {
      return(county_shapefiles[["2020"]])
    }
  }
  
  # List to store all maps
  all_maps <- list()
  
  # Generate maps for each variable and year
  for (variable in variables) {
    cat("Processing variable:", variable, "\n")
    
    # Get metadata for variable
    variable_metadata <- tryCatch({
      dbGetQuery(con, sprintf("SELECT * FROM data_dictionary WHERE std_name = '%s'", variable))
    }, error = function(e) {
      cat("Error getting metadata for variable", variable, ":", conditionMessage(e), "\n")
      return(data.frame(
        std_name = variable,
        description = variable,
        category = "Unknown",
        units = "Unknown"
      ))
    })
    
    # Determine variable description and title - with improved checking
    var_title <- if (nrow(variable_metadata) > 0 && 
                     "description" %in% names(variable_metadata) && 
                     !is.null(variable_metadata$description[1]) && 
                     !is.na(variable_metadata$description[1])) {
      variable_metadata$description[1]
    } else {
      # Make a readable title from variable name
      gsub("_", " ", variable)
    }
    
    # Determine units - with improved checking
    var_units <- if (nrow(variable_metadata) > 0 && 
                     "units" %in% names(variable_metadata) && 
                     !is.null(variable_metadata$units[1]) && 
                     !is.na(variable_metadata$units[1])) {
      variable_metadata$units[1]
    } else {
      if (grepl("pct|rate", variable)) "%" else ""
    }
    
    # Get color scale - with fallback if viridis not available
    if (requireNamespace("viridis", quietly = TRUE)) {
      # When viridis is available, use its specialized palettes
      if (grepl("income|value|expectancy", variable)) {
        # Higher values are better - use viridis
        color_scale <- scale_fill_viridis_c(option = "viridis", na.value = "gray90", 
                                          name = paste0(var_title, " (", var_units, ")"))
      } else if (grepl("poverty|uninsured|unemploy|obesity", variable)) {
        # Lower values are better - use reversed viridis
        color_scale <- scale_fill_viridis_c(option = "viridis", direction = -1, na.value = "gray90",
                                          name = paste0(var_title, " (", var_units, ")"))
      } else {
        # Neutral - use magma
        color_scale <- scale_fill_viridis_c(option = "magma", na.value = "gray90",
                                          name = paste0(var_title, " (", var_units, ")"))
      }
    } else {
      # Fallback to basic ggplot2 color scales if viridis not available
      if (grepl("poverty|uninsured|unemploy|obesity", variable)) {
        # For "bad" metrics, use a red scale
        color_scale <- scale_fill_gradient(low = "yellow", high = "red", na.value = "gray90",
                                         name = paste0(var_title, " (", var_units, ")"))
      } else {
        # For regular or "good" metrics, use a blue scale
        color_scale <- scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "gray90",
                                         name = paste0(var_title, " (", var_units, ")"))
      }
    }
    
    # Generate maps for each year
    for (year in years) {
      cat("  Generating map for year:", year, "\n")
      
      # Get data for this year - try multiple tables to handle emergency database
      year_data <- tryCatch({
        # First try the interpolated table (from emergency database)
        result <- dbGetQuery(con, sprintf("
          SELECT GEOID, NAME, %s 
          FROM county_interpolated 
          WHERE year = %d
        ", variable, year))
        
        if (nrow(result) == 0) {
          # If no results, try the standard table
          result <- dbGetQuery(con, sprintf("
            SELECT GEOID, NAME, %s 
            FROM county_sdoh_data 
            WHERE year = %d
          ", variable, year))
        }
        
        # If we have results, return them
        if (nrow(result) > 0) {
          return(result)
        }
        
        # Final fallback - try the county_time_series_interpolated view
        dbGetQuery(con, sprintf("
          SELECT GEOID, NAME, %s 
          FROM county_time_series_interpolated 
          WHERE year = %d
        ", variable, year))
      }, error = function(e) {
        # Try fallback query if first attempt fails
        tryCatch({
          cat("Trying fallback query for year", year, "...\n")
          # Try the county_time_series view
          dbGetQuery(con, sprintf("
            SELECT GEOID, NAME, %s 
            FROM county_time_series
            WHERE year = %d
          ", variable, year))
        }, error = function(e2) {
          cat("Error getting data for year", year, ":", conditionMessage(e2), "\n")
          return(NULL)
        })
      })
      
      if (is.null(year_data) || nrow(year_data) == 0) {
        cat("  No data available for year", year, "\n")
        next
      }
      
      # Get the appropriate shapefile
      county_sf <- get_shapefile_for_year(year)
      
      if (is.null(county_sf)) {
        cat("  No shapefile available for year", year, "\n")
        next
      }
      
      # Print sample of both for debugging
      cat("  Shapefile GEOID column name check:", paste(names(county_sf)[grep("GEOID", names(county_sf), ignore.case=TRUE)], collapse=", "), "\n")
      cat("  Data GEOID column name check:", paste(names(year_data)[grep("GEOID", names(year_data), ignore.case=TRUE)], collapse=", "), "\n")
      
      # Make sure we have matching column names for joining
      if ("GEOID" %in% names(county_sf)) {
        county_sf$GEOID <- as.character(county_sf$GEOID)
      } else if ("GEOID10" %in% names(county_sf)) {
        county_sf$GEOID <- as.character(county_sf$GEOID10)
      } else if ("GEOID20" %in% names(county_sf)) {
        county_sf$GEOID <- as.character(county_sf$GEOID20)
      } else if ("FIPS" %in% names(county_sf)) {
        county_sf$GEOID <- as.character(county_sf$FIPS)
      }
      
      # Make sure data GEOID is character
      if ("GEOID" %in% names(year_data)) {
        year_data$GEOID <- as.character(year_data$GEOID)
      }
      
      # Join data with shapefile
      map_data <- tryCatch({
        county_sf %>%
          left_join(year_data, by = "GEOID")
      }, error = function(e) {
        cat("  Error joining data with shapefile:", conditionMessage(e), "\n")
        cat("  Attempting fallback join method...\n")
        
        # Alternative join method
        tryCatch({
          # Create a simple data frame from the sf object
          county_df <- as.data.frame(county_sf)
          # Join with the year data
          joined <- merge(county_df, year_data, by = "GEOID", all.x = TRUE)
          # Convert back to sf
          st_as_sf(joined)
        }, error = function(e2) {
          cat("  Fallback join method also failed:", conditionMessage(e2), "\n")
          NULL
        })
      })
      
      # Skip if join failed
      if (is.null(map_data)) {
        cat("  Skipping map for year", year, "due to join failure\n")
        next
      }
      
      # Create the map with US continental boundaries
      map <- ggplot(map_data) +
        geom_sf(aes(fill = !!sym(variable)), color = NA) +
        color_scale +
        # Set map boundaries to show just the continental USA
        coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) +
        theme_minimal() +
        labs(
          title = paste0(var_title, " by County (", year, ")"),
          subtitle = "Data source: Social Determinants of Health County Dataset",
          caption = "Source: IPUMS NHGIS, U.S. Census Bureau, CDC PLACES, and SEER"
        ) +
        theme(
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(size = 9),
          plot.caption = element_text(size = 8),
          legend.position = "bottom",
          panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
          panel.background = element_rect(fill = "azure")
        )
      
      # Store the map
      map_key <- paste0(variable, "_", year)
      all_maps[[map_key]] <- map
      
      # Create variable subdirectory if it doesn't exist
      var_dir <- file.path(output_dir, variable)
      if (!dir.exists(var_dir)) {
        dir.create(var_dir, recursive = TRUE, showWarnings = FALSE)
      }
      
      # Save the map to file in the variable subdirectory with variable name included
      output_file <- file.path(var_dir, paste0(variable, "_", year, ".png"))
      ggsave(output_file, map, width = 10, height = 7, dpi = 150)
      cat("  Saved map to:", output_file, "\n")
    }
  }
  
  # Clean up
  dbDisconnect(con)
  
  # Return the map list
  return(all_maps)
}

# If this script is run directly, generate maps for key variables
if (!interactive()) {
  # Define parameters based on command-line arguments or defaults
  args <- commandArgs(trailingOnly = TRUE)
  
  # Default parameters
  database_path <- "us_county_sdoh_data.duckdb"
  years <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 
             2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)
  
  # Generate maps for all SDOH variables in our crosswalk
  # Read the variable crosswalk to get all available variables
  crosswalk_file <- "variable_crosswalk_extended.csv"
  if (file.exists(crosswalk_file)) {
    # Read the crosswalk file
    crosswalk <- read.csv(crosswalk_file, stringsAsFactors = FALSE)
    
    # Extract all variable names from the first column
    variables <- crosswalk$std_name
    
    # Remove any empty or NA variables
    variables <- variables[!is.na(variables) & variables != ""]
    
    cat("Extracted", length(variables), "variables from crosswalk\n")
  } else {
    # Fallback to a curated list if crosswalk not available
    cat("Variable crosswalk file not found. Using default variable list.\n")
    variables <- c(
      # Economic variables
      "poverty_rate", 
      "median_household_income",
      "median_earnings",
      
      # Housing variables  
      "median_home_value",
      "median_gross_rent",
      "homeownership_rate",
      "vacant_housing_rate",
      
      # Employment variables
      "unemployment_rate",
      "labor_force_participation",
      
      # Health variables
      "life_expectancy",
      "obesity_pct",
      "diabetes_pct",
      
      # Transportation variables
      "mean_commute_time"
    )
  }
  
  cat("Mapping key SDOH variables:", paste(variables, collapse=", "), "\n")
  
  # Parse arguments if present
  if (length(args) > 0) {
    # TODO: Implement argument parsing
  }
  
  # Generate maps
  cat("Generating county maps for years:", paste(years, collapse = ", "), "\n")
  cat("Variables:", paste(variables, collapse = ", "), "\n")
  
  maps <- generate_county_maps(
    database_path = database_path,
    years = years,
    variables = variables
  )
  
  cat("Map generation complete. Maps saved to output/maps directory.\n")
}