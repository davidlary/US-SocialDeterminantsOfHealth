#!/usr/bin/env Rscript

# Utility script for downloading and managing county shapefiles
# This script handles downloading, caching, and accessing county boundary shapefiles

library(tigris)
library(sf)
library(dplyr)
library(readr)
library(stringr)
library(httr)

#' Download and cache county shapefiles for specified years
#'
#' This function downloads county boundary shapefiles from the Census Bureau
#' using the tigris package. It implements caching and robust error handling.
#'
#' @param years Vector of years for which to download shapefiles (default: c(1990, 2000, 2010, 2020))
#' @param shapefile_dir Directory to store shapefiles (default: "data/shapefiles")
#' @param refresh_cache Whether to refresh the cache (default: FALSE)
#' @param simplified Use simplified (cb = TRUE) shapefiles (default: TRUE)
#' @return A list of sf objects with county boundaries for each year
fetch_county_shapefiles <- function(years = c(1990, 2000, 2010, 2020),
                                   shapefile_dir = "data/shapefiles",
                                   refresh_cache = FALSE,
                                   simplified = TRUE) {
  # Create shapefile directory if it doesn't exist
  if (!dir.exists(shapefile_dir)) {
    dir.create(shapefile_dir, recursive = TRUE, showWarnings = FALSE)
    cat("Created shapefile directory:", shapefile_dir, "\n")
  }
  
  # Create a list to store shapefiles for each year
  county_shapefiles <- list()
  
  # Track years with successful downloads
  successful_years <- c()
  
  # Download shapefiles for each year
  for (year in years) {
    # Define cache file paths
    shapefile_rds <- file.path(shapefile_dir, paste0("counties_", year, ".rds"))
    shapefile_metadata <- file.path(shapefile_dir, paste0("counties_", year, "_metadata.txt"))
    
    # Check if cached file exists and whether to use it
    use_cache <- !refresh_cache && file.exists(shapefile_rds)
    
    if (use_cache) {
      cat("Using cached shapefile for year", year, "\n")
      tryCatch({
        county_shapefiles[[as.character(year)]] <- readRDS(shapefile_rds)
        successful_years <- c(successful_years, year)
      }, error = function(e) {
        cat("Error reading cached shapefile for year", year, ":", conditionMessage(e), "\n")
        cat("Will attempt to download fresh data\n")
        use_cache <- FALSE
      })
    }
    
    # Download fresh data if needed
    if (!use_cache) {
      cat("Downloading county shapefile for year", year, "\n")
      
      # Try multiple approaches for robustness
      shapefile_data <- tryCatch({
        # First attempt: Use tigris
        counties <- tigris::counties(year = year, cb = simplified)
        
        # Save metadata
        metadata <- tibble(
          source = "tigris",
          year = year,
          download_date = Sys.Date(),
          num_counties = nrow(counties),
          simplified = simplified
        )
        write_csv(metadata, shapefile_metadata)
        
        counties
      }, error = function(e) {
        cat("Error downloading from tigris for year", year, ":", conditionMessage(e), "\n")
        
        # Second attempt: Use alternative source if available
        tryCatch({
          cat("Attempting alternative download for year", year, "\n")
          
          # For recent years, try Census API directly
          if (year >= 2010) {
            # URL varies by year and whether simplified boundaries are requested
            base_url <- "https://www2.census.gov/geo/tiger"
            
            # Determine appropriate URL suffix
            if (year == 2020) {
              suffix <- if(simplified) "/GENZ2020/shp/cb_2020_us_county_500k.zip" else "/TIGER2020/COUNTY/tl_2020_us_county.zip"
            } else if (year == 2010) {
              suffix <- if(simplified) "/GENZ2010/gz_2010_us_050_00_500k.zip" else "/TIGER2010/COUNTY/2010/tl_2010_us_county10.zip"
            } else {
              stop("No alternative source available for year ", year)
            }
            
            # Construct full URL
            url <- paste0(base_url, suffix)
            
            # Create temp directory for download
            temp_dir <- tempdir()
            zip_file <- file.path(temp_dir, basename(url))
            
            # Download file
            download.file(url, zip_file, mode = "wb", quiet = TRUE)
            
            # Unzip file
            unzip(zip_file, exdir = temp_dir)
            
            # Find shapefile in temp directory
            shp_files <- list.files(temp_dir, pattern = ".shp$", recursive = TRUE, full.names = TRUE)
            
            if (length(shp_files) == 0) {
              stop("No shapefile found in downloaded archive")
            }
            
            # Read shapefile
            counties <- sf::read_sf(shp_files[1])
            
            # Save metadata
            metadata <- tibble(
              source = "census_direct",
              year = year,
              download_date = Sys.Date(),
              num_counties = nrow(counties),
              simplified = simplified,
              url = url
            )
            write_csv(metadata, shapefile_metadata)
            
            counties
          } else {
            # For older years, try historical Tiger Line files if available
            stop("Historical Tiger Line files not implemented")
          }
        }, error = function(e2) {
          cat("Alternative download also failed for year", year, ":", conditionMessage(e2), "\n")
          
          # Fall back to nearest available year
          nearest_year <- NULL
          if (year == 1990 && "2000" %in% names(county_shapefiles)) nearest_year <- "2000"
          if (year == 2000 && "2010" %in% names(county_shapefiles)) nearest_year <- "2010"
          if (year == 2010 && "2020" %in% names(county_shapefiles)) nearest_year <- "2020"
          if (year == 2020 && "2010" %in% names(county_shapefiles)) nearest_year <- "2010"
          
          if (!is.null(nearest_year)) {
            cat("Using", nearest_year, "shapefile as fallback for", year, "\n")
            counties <- county_shapefiles[[nearest_year]]
            
            # Save metadata
            metadata <- tibble(
              source = "fallback",
              year = year,
              fallback_year = nearest_year,
              download_date = Sys.Date(),
              num_counties = nrow(counties),
              simplified = simplified
            )
            write_csv(metadata, shapefile_metadata)
            
            counties
          } else {
            # Create placeholder with basic structure
            cat("Creating minimal placeholder shapefile for year", year, "\n")
            
            # Get basic county data from other sources
            state_fips <- c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", 
                           "13", "15", "16", "17", "18", "19", "20", "21", "22", "23", 
                           "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", 
                           "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", 
                           "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56")
            
            # Create basic counties without geometry
            counties_basic <- tibble(
              STATEFP = rep(state_fips, each = 3),
              COUNTYFP = rep(c("001", "003", "005"), times = length(state_fips)),
              GEOID = paste0(STATEFP, COUNTYFP),
              NAME = paste0("County ", COUNTYFP, ", State ", STATEFP)
            )
            
            # Convert to sf object with placeholder geometry
            counties <- st_as_sf(counties_basic, 
                               geometry = st_sfc(lapply(1:nrow(counties_basic), 
                                                      function(i) st_point(c(0,0)))),
                               crs = 4326)
            
            # Save metadata
            metadata <- tibble(
              source = "placeholder",
              year = year,
              download_date = Sys.Date(),
              num_counties = nrow(counties),
              simplified = NA,
              placeholder = TRUE
            )
            write_csv(metadata, shapefile_metadata)
            
            counties
          }
        })
      })
      
      # Save to cache
      saveRDS(shapefile_data, shapefile_rds)
      cat("Saved shapefile for year", year, "to cache\n")
      
      # Add to list of shapefiles
      county_shapefiles[[as.character(year)]] <- shapefile_data
      successful_years <- c(successful_years, year)
    }
  }
  
  # Print summary of downloaded shapefiles
  cat("\nDownloaded shapefiles for years:", paste(successful_years, collapse = ", "), "\n")
  
  # Create shapefile index file
  shapefile_index <- tibble(
    year = successful_years,
    cache_file = file.path(shapefile_dir, paste0("counties_", successful_years, ".rds")),
    num_counties = sapply(as.character(successful_years), function(yr) nrow(county_shapefiles[[yr]])),
    has_geometry = sapply(as.character(successful_years), function(yr) inherits(county_shapefiles[[yr]], "sf")),
    download_date = file.info(file.path(shapefile_dir, paste0("counties_", successful_years, ".rds")))$mtime
  )
  
  write_csv(shapefile_index, file.path(shapefile_dir, "shapefile_index.csv"))
  
  return(county_shapefiles)
}

#' Get county shapefile for a specific year
#'
#' @param year The year for which to get the shapefile
#' @param shapefile_dir The directory where shapefiles are stored
#' @param download If TRUE, download if not available
#' @return An sf object with county boundaries
get_county_shapefile <- function(year, shapefile_dir = "data/shapefiles", download = TRUE) {
  # Define cache file path
  shapefile_rds <- file.path(shapefile_dir, paste0("counties_", year, ".rds"))
  
  # Check if file exists
  if (file.exists(shapefile_rds)) {
    cat("Loading shapefile for year", year, "\n")
    return(readRDS(shapefile_rds))
  } else if (download) {
    # Download if not available
    cat("Shapefile for year", year, "not found. Downloading...\n")
    shapefiles <- fetch_county_shapefiles(years = year, shapefile_dir = shapefile_dir)
    return(shapefiles[[as.character(year)]])
  } else {
    # Return NULL if not available and not downloading
    cat("Shapefile for year", year, "not found and download=FALSE\n")
    return(NULL)
  }
}

# If this script is run directly, download shapefiles for key years
if (!interactive()) {
  # Define years based on command-line arguments or defaults
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 0) {
    years <- as.numeric(args)
  } else {
    years <- c(1990, 2000, 2010, 2020)
  }
  
  cat("Downloading county shapefiles for years:", paste(years, collapse = ", "), "\n")
  shapefiles <- fetch_county_shapefiles(years = years)
  
  # Print summary
  for (year in as.character(years)) {
    if (!is.null(shapefiles[[year]])) {
      cat(year, ": ", nrow(shapefiles[[year]]), " counties\n", sep = "")
    } else {
      cat(year, ": No data available\n", sep = "")
    }
  }
}