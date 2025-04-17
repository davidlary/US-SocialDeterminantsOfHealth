#!/usr/bin/env Rscript

# Education Data Fetcher
# This script handles retrieval of education data from National Center for Education Statistics
# and Stanford Education Data Archive

library(tidyverse)
library(httr)
library(jsonlite)
library(readxl)
library(lubridate)
library(sf)
library(zoo) # For interpolation if needed

#' Fetch educational resources data
#'
#' Retrieves education data from National Center for Education Statistics and
#' Stanford Education Data Archive for educational quality metrics at the county level.
#'
#' @param years Vector of years to include
#' @param cache_dir Directory to store cache files
#' @param refresh_cache Whether to refresh the cache
#' @param allow_simulation Whether to generate simulated data if real data not available
#' @param allow_interpolation Whether to interpolate missing values
#' @param data_quality_flags List of standardized data quality flags
#' @param offline_mode Whether to skip all downloads and use only cached data
#' @return A data frame with education data for all requested years
fetch_education_data <- function(years, 
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
  cache_file <- file.path(cache_dir, "education_data.rds")
  
  # Use cache if available and not refreshing
  if (!refresh_cache && file.exists(cache_file)) {
    print_msg("Loading cached education data...")
    education_data <- readRDS(cache_file)
    
    # Check if all requested years are in the cache
    cached_years <- unique(education_data$year)
    missing_years <- setdiff(years, cached_years)
    
    # Check for empty cache with just placeholder data
    if (nrow(education_data) <= 1 || 
        (is.data.frame(education_data) && "data_source" %in% names(education_data) && 
         any(grepl("SIMULATED", education_data$data_source)))) {
      print_msg("Cached education data appears to be empty or a placeholder. Will process files again.")
      # Force refresh by continuing past this point
    } else if (length(missing_years) == 0) {
      print_msg("Using complete cached education data.")
      return(education_data %>% filter(year %in% years))
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
  data_dir <- "data/education"
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
    print_msg(paste("Created education data directory at:", data_dir))
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
  
  # Function to get NCES data
  get_nces_data <- function() {
    # NCES has county-level education data
    
    # Define variables we want to extract
    nces_variables <- c(
      "student_teacher_ratio" = "Student-to-teacher ratio in public schools",
      "per_pupil_expenditure" = "Per-pupil expenditure in public schools",
      "high_school_graduation_rate" = "Four-year high school graduation rate",
      "preschool_enrollment_rate" = "Percentage of 3-4 year-olds enrolled in preschool",
      "school_funding_equity" = "Ratio of funding in high-poverty vs. low-poverty districts"
    )
    
    # NCES data list to store results
    nces_data_list <- list()
    
    # Process each year
    for (year in years) {
      # Skip future years
      if (year > as.integer(format(Sys.Date(), "%Y"))) {
        next
      }
      
      # Define file paths
      nces_file <- file.path(data_dir, paste0("nces_", year, ".csv"))
      
      # Check if we need to download
      need_download <- !file.exists(nces_file) || refresh_cache
      
      if (need_download) {
        # NCES URL (placeholder - actual URLs would depend on specific NCES data structure)
        nces_url <- paste0(
          "https://nces.ed.gov/programs/edge/data/county_", 
          year, 
          ".csv"
        )
        
        # Try to download
        if (!safe_download(nces_url, nces_file, paste("NCES data for", year))) {
          print_msg(paste("Could not download NCES data for", year))
          # NCES data typically requires navigation through their site
          next
        }
      } else {
        print_msg(paste("Using existing NCES file for", year))
      }
      
      # Process the data if file exists
      if (file.exists(nces_file)) {
        print_msg(paste("Reading NCES data for", year))
        
        # Read the file
        tryCatch({
          nces_data <- read_csv(nces_file, show_col_types = FALSE)
          
          # Get column names
          print_msg(paste("NCES data has", ncol(nces_data), "columns and", nrow(nces_data), "rows"))
          
          # Check for FIPS/GEOID column
          geoid_col <- grep("FIPS|fips|geoid|GEOID|county.*code|COUNTY.*CODE", 
                           names(nces_data), value = TRUE)[1]
          
          if (is.na(geoid_col)) {
            print_msg("Could not identify GEOID column in NCES data")
            next
          }
          
          # Rename and format GEOID
          nces_data <- nces_data %>%
            rename(GEOID = all_of(geoid_col)) %>%
            mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
          
          # Find columns for our variables of interest
          
          # Student-teacher ratio
          ratio_col <- grep("student.*teacher|pupil.*teacher|student.*ratio", 
                           names(nces_data), value = TRUE)[1]
          
          # Per-pupil expenditure
          expenditure_col <- grep("expenditure|spending|cost.*pupil|per.*pupil", 
                                 names(nces_data), value = TRUE)[1]
          
          # Graduation rate
          graduation_col <- grep("grad.*rate|graduation|complete", 
                                names(nces_data), value = TRUE)[1]
          
          # Preschool enrollment
          preschool_col <- grep("preschool|pre.*school|pre.*k|prek", 
                               names(nces_data), value = TRUE)[1]
          
          # Funding equity
          equity_col <- grep("equity|equal|funding.*ratio", 
                            names(nces_data), value = TRUE)[1]
          
          print_msg(paste("Found columns: Student-teacher ratio:", !is.na(ratio_col),
                        "Per-pupil expenditure:", !is.na(expenditure_col),
                        "Graduation rate:", !is.na(graduation_col),
                        "Preschool enrollment:", !is.na(preschool_col),
                        "Funding equity:", !is.na(equity_col)))
          
          # Create data frame for this year
          year_data <- data.frame(
            GEOID = nces_data$GEOID,
            year = year
          )
          
          # Add student-teacher ratio if available
          if (!is.na(ratio_col)) {
            year_data$student_teacher_ratio <- nces_data[[ratio_col]]
            year_data$student_teacher_ratio_data_quality <- data_quality_flags$direct
            year_data$student_teacher_ratio_data_source <- "National Center for Education Statistics"
            year_data$student_teacher_ratio_data_vintage <- as.character(year)
          }
          
          # Add per-pupil expenditure if available
          if (!is.na(expenditure_col)) {
            year_data$per_pupil_expenditure <- nces_data[[expenditure_col]]
            year_data$per_pupil_expenditure_data_quality <- data_quality_flags$direct
            year_data$per_pupil_expenditure_data_source <- "National Center for Education Statistics"
            year_data$per_pupil_expenditure_data_vintage <- as.character(year)
          }
          
          # Add graduation rate if available
          if (!is.na(graduation_col)) {
            year_data$high_school_graduation_rate <- nces_data[[graduation_col]]
            year_data$high_school_graduation_rate_data_quality <- data_quality_flags$direct
            year_data$high_school_graduation_rate_data_source <- "National Center for Education Statistics"
            year_data$high_school_graduation_rate_data_vintage <- as.character(year)
          }
          
          # Add preschool enrollment if available
          if (!is.na(preschool_col)) {
            year_data$preschool_enrollment_rate <- nces_data[[preschool_col]]
            year_data$preschool_enrollment_rate_data_quality <- data_quality_flags$direct
            year_data$preschool_enrollment_rate_data_source <- "National Center for Education Statistics"
            year_data$preschool_enrollment_rate_data_vintage <- as.character(year)
          }
          
          # Add funding equity if available
          if (!is.na(equity_col)) {
            year_data$school_funding_equity <- nces_data[[equity_col]]
            year_data$school_funding_equity_data_quality <- data_quality_flags$direct
            year_data$school_funding_equity_data_source <- "National Center for Education Statistics"
            year_data$school_funding_equity_data_vintage <- as.character(year)
          }
          
          # Add to list
          nces_data_list[[as.character(year)]] <- year_data
          
          print_msg(paste("Processed NCES data for", year))
        }, error = function(e) {
          print_msg(paste("Error reading NCES data for", year, ":", conditionMessage(e)))
        })
      }
    }
    
    # Combine all years
    if (length(nces_data_list) > 0) {
      combined_nces <- bind_rows(nces_data_list)
      print_msg(paste("Combined NCES data with", nrow(combined_nces), "rows"))
      return(combined_nces)
    } else {
      print_msg("No NCES data processed successfully")
      return(NULL)
    }
  }
  
  # Function to get Stanford Education Data Archive data
  get_seda_data <- function() {
    # SEDA has county-level education achievement data from 2009-2018
    
    # Define variables we want to extract
    seda_variables <- c(
      "reading_achievement_gap" = "Achievement gap in reading scores by race/ethnicity",
      "math_achievement_gap" = "Achievement gap in math scores by race/ethnicity",
      "educational_opportunity_index" = "Measure of educational opportunity"
    )
    
    # SEDA data file
    seda_file <- file.path(data_dir, "seda_county.csv")
    
    # Check if we need to download
    need_download <- !file.exists(seda_file) || refresh_cache
    
    if (need_download) {
      # SEDA URL (placeholder - actual URL would be from their site)
      seda_url <- "https://edopportunity.org/get-download/seda_county_pool_3.0.csv"
      
      # Try to download
      if (!safe_download(seda_url, seda_file, "Stanford Education Data Archive data")) {
        print_msg("Could not download SEDA data")
        return(NULL)
      }
    } else {
      print_msg("Using existing SEDA file")
    }
    
    # Process the data if file exists
    seda_data <- NULL
    if (file.exists(seda_file)) {
      print_msg("Reading SEDA data")
      
      # Read the file
      tryCatch({
        seda_data <- read_csv(seda_file, show_col_types = FALSE)
        
        # Get column names
        print_msg(paste("SEDA data has", ncol(seda_data), "columns and", nrow(seda_data), "rows"))
        
        # Check for FIPS/GEOID column
        geoid_col <- grep("FIPS|fips|geoid|GEOID|county.*id", names(seda_data), value = TRUE)[1]
        
        if (is.na(geoid_col)) {
          print_msg("Could not identify GEOID column in SEDA data")
          return(NULL)
        }
        
        # Check for year column
        year_col <- grep("year|Year|YEAR|cohort|Cohort", names(seda_data), value = TRUE)[1]
        
        if (is.na(year_col)) {
          print_msg("Could not identify year column in SEDA data")
          return(NULL)
        }
        
        # Rename and format GEOID
        seda_data <- seda_data %>%
          rename(
            GEOID = all_of(geoid_col),
            year = all_of(year_col)
          ) %>%
          mutate(
            GEOID = sprintf("%05d", as.numeric(GEOID)),
            year = as.numeric(year)
          )
        
        # Filter to requested years
        seda_data <- seda_data %>%
          filter(year %in% years)
        
        # Find columns for our variables of interest
        
        # Reading achievement gap
        reading_gap_col <- grep("reading.*gap|ela.*gap|read.*gap", 
                               names(seda_data), value = TRUE)[1]
        
        # Math achievement gap
        math_gap_col <- grep("math.*gap|math.*gap", 
                            names(seda_data), value = TRUE)[1]
        
        # Educational opportunity
        opportunity_col <- grep("opportunity|opport.*index", 
                               names(seda_data), value = TRUE)[1]
        
        print_msg(paste("Found columns: Reading gap:", !is.na(reading_gap_col),
                      "Math gap:", !is.na(math_gap_col),
                      "Opportunity index:", !is.na(opportunity_col)))
        
        # Create data frame with selected columns
        seda_processed <- seda_data %>%
          select(GEOID, year)
        
        # Add reading achievement gap if available
        if (!is.na(reading_gap_col)) {
          seda_processed$reading_achievement_gap <- seda_data[[reading_gap_col]]
          seda_processed$reading_achievement_gap_data_quality <- data_quality_flags$direct
          seda_processed$reading_achievement_gap_data_source <- "Stanford Education Data Archive"
          seda_processed$reading_achievement_gap_data_vintage <- as.character(seda_data$year)
        }
        
        # Add math achievement gap if available
        if (!is.na(math_gap_col)) {
          seda_processed$math_achievement_gap <- seda_data[[math_gap_col]]
          seda_processed$math_achievement_gap_data_quality <- data_quality_flags$direct
          seda_processed$math_achievement_gap_data_source <- "Stanford Education Data Archive"
          seda_processed$math_achievement_gap_data_vintage <- as.character(seda_data$year)
        }
        
        # Add educational opportunity if available
        if (!is.na(opportunity_col)) {
          seda_processed$educational_opportunity_index <- seda_data[[opportunity_col]]
          seda_processed$educational_opportunity_index_data_quality <- data_quality_flags$direct
          seda_processed$educational_opportunity_index_data_source <- "Stanford Education Data Archive"
          seda_processed$educational_opportunity_index_data_vintage <- as.character(seda_data$year)
        }
        
        print_msg(paste("Processed SEDA data with", nrow(seda_processed), "rows"))
        return(seda_processed)
      }, error = function(e) {
        print_msg(paste("Error reading SEDA data:", conditionMessage(e)))
        return(NULL)
      })
    }
    
    return(NULL)
  }
  
  # Get data from different sources
  nces_data <- get_nces_data()
  seda_data <- get_seda_data()
  
  # Combine all data sources
  education_data_list <- list()
  
  if (!is.null(nces_data) && nrow(nces_data) > 0) {
    education_data_list[["nces"]] <- nces_data
  }
  
  if (!is.null(seda_data) && nrow(seda_data) > 0) {
    education_data_list[["seda"]] <- seda_data
  }
  
  # Process if we have data
  if (length(education_data_list) > 0) {
    # Combine all sources and handle overlapping columns
    # Start with first dataset
    combined_education_data <- education_data_list[[1]]
    
    # Add each additional dataset
    if (length(education_data_list) > 1) {
      for (i in 2:length(education_data_list)) {
        next_data <- education_data_list[[i]]
        
        # Identify common columns for joining
        join_cols <- intersect(names(combined_education_data), names(next_data))
        
        # Ensure we have at least GEOID and year for joining
        if (all(c("GEOID", "year") %in% join_cols)) {
          # Find columns that might overlap (excluding join columns and flags)
          overlap_cols <- setdiff(
            intersect(names(combined_education_data), names(next_data)),
            c(join_cols, grep("_data_quality$|_data_source$|_data_vintage$", 
                             names(next_data), value = TRUE))
          )
          
          if (length(overlap_cols) > 0) {
            # For overlapping columns, prefer data from the current dataset
            # Remove those columns from the combined data before joining
            combined_education_data <- combined_education_data %>%
              select(-all_of(overlap_cols))
          }
          
          # Join with the next dataset
          combined_education_data <- full_join(
            combined_education_data, 
            next_data, 
            by = join_cols
          )
        } else {
          print_msg("Warning: Cannot join datasets - missing common keys")
        }
      }
    }
    
    # Check if we need to handle missing years
    available_years <- unique(combined_education_data$year)
    missing_years <- setdiff(years, available_years)
    
    if (length(missing_years) > 0 && allow_interpolation) {
      print_msg(paste("Interpolating data for missing years:", paste(missing_years, collapse=", ")))
      
      # Get all variables except join columns and metadata columns
      measure_cols <- setdiff(
        names(combined_education_data),
        c("GEOID", "year", grep("_data_quality$|_data_source$|_data_vintage$", 
                               names(combined_education_data), value = TRUE))
      )
      
      # Process each county separately for interpolation
      counties <- unique(combined_education_data$GEOID)
      
      # List to store interpolated data
      interp_data_list <- list()
      
      for (county in counties) {
        # Get data for this county
        county_data <- combined_education_data %>%
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
                         names(combined_education_data), value = TRUE)
        
        for (col in meta_cols) {
          if (!(col %in% names(interp_data))) {
            interp_data[[col]] <- NA
          }
        }
        
        # Use the interpolated data instead
        combined_education_data <- interp_data
      }
    } else if (length(missing_years) > 0) {
      print_msg("Missing years but interpolation not allowed. Years will remain missing.")
    }
    
    # Filter to just the requested years
    combined_education_data <- combined_education_data %>%
      filter(year %in% years)
    
    # Sort data
    combined_education_data <- combined_education_data %>%
      arrange(GEOID, year)
    
    # Cache the processed data
    saveRDS(combined_education_data, cache_file)
    print_msg(paste("Cached education data to:", cache_file))
    
    return(combined_education_data)
  } else if (allow_simulation) {
    # Create simulated data
    print_msg("No education data found. Creating simulated data...")
    
    # Education variables to simulate
    education_vars <- c(
      "student_teacher_ratio" = "Student-to-teacher ratio in public schools",
      "per_pupil_expenditure" = "Per-pupil expenditure in public schools",
      "high_school_graduation_rate" = "Four-year high school graduation rate",
      "preschool_enrollment_rate" = "Percentage of 3-4 year-olds enrolled in preschool",
      "school_funding_equity" = "Ratio of funding in high-poverty vs. low-poverty districts",
      "reading_achievement_gap" = "Achievement gap in reading scores by race/ethnicity",
      "math_achievement_gap" = "Achievement gap in math scores by race/ethnicity",
      "educational_opportunity_index" = "Measure of educational opportunity"
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
      for (var_name in names(education_vars)) {
        if (var_name == "student_teacher_ratio") {
          # Typically 12-25 students per teacher
          year_data[[var_name]] <- runif(nrow(year_data), 12, 25)
        } else if (var_name == "per_pupil_expenditure") {
          # Typically $8,000-$25,000 per student
          year_data[[var_name]] <- runif(nrow(year_data), 8000, 25000)
        } else if (var_name == "high_school_graduation_rate") {
          # Typically 70-95%
          year_data[[var_name]] <- runif(nrow(year_data), 70, 95)
        } else if (var_name == "preschool_enrollment_rate") {
          # Typically 30-70%
          year_data[[var_name]] <- runif(nrow(year_data), 30, 70)
        } else if (var_name == "school_funding_equity") {
          # Typically 0.6-1.2 (values < 1 indicate inequity)
          year_data[[var_name]] <- runif(nrow(year_data), 0.6, 1.2)
        } else if (var_name == "reading_achievement_gap" || var_name == "math_achievement_gap") {
          # Typically 0.2-1.0 standard deviations
          year_data[[var_name]] <- runif(nrow(year_data), 0.2, 1.0)
        } else if (var_name == "educational_opportunity_index") {
          # Typically 0-10 scale
          year_data[[var_name]] <- runif(nrow(year_data), 0, 10)
        } else {
          # Default - 0-100 range
          year_data[[var_name]] <- runif(nrow(year_data), 0, 100)
        }
        
        # Add quality flags
        year_data[[paste0(var_name, "_data_quality")]] <- data_quality_flags$simulated
        year_data[[paste0(var_name, "_data_source")]] <- "SIMULATED Education Data"
        year_data[[paste0(var_name, "_data_vintage")]] <- paste0("simulated_", year)
      }
      
      sim_data_list[[as.character(year)]] <- year_data
    }
    
    # Combine all years
    simulated_data <- bind_rows(sim_data_list)
    
    # Cache the simulated data
    saveRDS(simulated_data, cache_file)
    print_msg(paste("Cached simulated education data to:", cache_file))
    
    return(simulated_data)
  } else {
    # No data and simulation not allowed - create empty dataset with NAs
    print_msg("No education data available and simulation not allowed. Creating empty dataset with NAs.")
    
    # Education variables to include
    education_vars <- c(
      "student_teacher_ratio",
      "per_pupil_expenditure",
      "high_school_graduation_rate",
      "preschool_enrollment_rate",
      "school_funding_equity",
      "reading_achievement_gap",
      "math_achievement_gap",
      "educational_opportunity_index"
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
    for (var in education_vars) {
      grid[[var]] <- NA_real_
      grid[[paste0(var, "_data_quality")]] <- data_quality_flags$missing
      grid[[paste0(var, "_data_source")]] <- "NOT_AVAILABLE"
      grid[[paste0(var, "_data_vintage")]] <- NA_character_
    }
    
    education_data <- as_tibble(grid)
    print_msg(paste("Created empty education dataset with", nrow(education_data), "rows"))
    
    # Cache the empty data
    saveRDS(education_data, cache_file)
    print_msg(paste("Cached empty education data to:", cache_file))
    
    return(education_data)
  }
}

# This lets the function be used when the script is sourced
is_sourced <- function() {
  return(!identical(environment(), globalenv()))
}

# If the script is run directly, test the function
if (!is_sourced()) {
  cat("Testing education data fetcher...\n")
  
  # Get current year
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  
  # Test for last 5 years
  test_years <- (current_year-4):current_year
  
  # Test the function
  result <- fetch_education_data(
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
