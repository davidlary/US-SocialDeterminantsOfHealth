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
#' @param allow_interpolation Whether to interpolate missing values
#' @param data_quality_flags List of standardized data quality flags
#' @param offline_mode Whether to skip all downloads and use only cached data
#' @return A data frame with education data for all requested years
fetch_education_data <- function(years, 
                               cache_dir = "data/cache", 
                               refresh_cache = FALSE,
                               allow_interpolation = TRUE,
                               data_quality_flags = list(
                                 direct = "direct",
                                 interpolated = "interpolated",
                                 extrapolated = "extrapolated",
                                 missing = NA,
                                 imputed = "imputed"
                               ),
                               offline_mode = FALSE,
                               parallel = TRUE,
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
        (is.data.frame(education_data) && 
         any(sapply(names(education_data), function(col) {
           if (grepl("_data_source$", col)) {
             return(any(grepl("SIMULATED|NO_DATA_AVAILABLE", education_data[[col]])))
           }
           return(FALSE)
         })))) {
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
      print_msg("Parallel processing enabled for education data with adaptive strategy")
    } else {
      # Basic parallel setup
      print_msg("Using basic parallel processing setup for education data")
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
  
  # Make data directory if needed
  data_dir <- "data/education"
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
    print_msg(paste("Created education data directory at:", data_dir))
  }
  
  # Function to find local education data files
  find_local_education_files <- function() {
    # List of directories to check
    education_dirs <- c(
      "data/education",
      "data/cache/education",
      "data/education_data",
      "data/nces",
      "data/seda"
    )
    
    # List of possible file extensions
    file_exts <- c("\\.csv$", "\\.xlsx$", "\\.xls$", "\\.txt$", "\\.rds$")
    
    # Search for files
    all_files <- c()
    for (dir in education_dirs) {
      if (dir.exists(dir)) {
        for (ext in file_exts) {
          files <- list.files(dir, pattern = ext, full.names = TRUE, recursive = TRUE)
          all_files <- c(all_files, files)
        }
      }
    }
    
    # Filter for different types of education data
    education_files <- list(
      nces = grep("nces|education|school|student|teacher|enrollment|graduation|expenditure", 
                  all_files, value = TRUE, ignore.case = TRUE),
      seda = grep("seda|stanford|achievement|opportunity|education.*data.*archive", 
                  all_files, value = TRUE, ignore.case = TRUE)
    )
    
    return(education_files)
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
    
    # Find local NCES files
    local_files <- find_local_education_files()
    nces_files <- local_files$nces
    
    print_msg(paste("Found", length(nces_files), "potential NCES data files"))
    
    # NCES data list to store results
    nces_data_list <- list()
    
    # First, check if we have files with year in the name
    year_specific_files <- list()
    for (year in years) {
      # Skip future years
      if (year > as.integer(format(Sys.Date(), "%Y"))) {
        next
      }
      
      # Look for files with this year in the name
      year_files <- grep(paste0("_", year, "\\.|_", year, "$"), nces_files, value = TRUE)
      if (length(year_files) > 0) {
        year_specific_files[[as.character(year)]] <- year_files[1]  # Use the first match if multiple
        print_msg(paste("Found NCES file for year", year, ":", year_files[1]))
      }
    }
    
    # Process each year
    for (year in years) {
      # Skip future years
      if (year > as.integer(format(Sys.Date(), "%Y"))) {
        next
      }
      
      # Define file paths - check if we have a year-specific file first
      if (as.character(year) %in% names(year_specific_files)) {
        nces_file <- year_specific_files[[as.character(year)]]
        print_msg(paste("Using year-specific NCES file for", year, ":", nces_file))
      } else {
        # If no year-specific file, use the default path for potential downloads
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
            
            # Try to use the most recent file if we couldn't download
            if (length(nces_files) > 0) {
              # Sort files by modification time (newest first)
              file_info <- file.info(nces_files)
              file_info$path <- rownames(file_info)
              file_info <- file_info[order(file_info$mtime, decreasing = TRUE), ]
              
              # Use the newest file
              nces_file <- file_info$path[1]
              print_msg(paste("Using most recent NCES file instead:", nces_file))
            } else {
              # No files available
              next
            }
          }
        } else {
          print_msg(paste("Using existing NCES file for", year, ":", nces_file))
        }
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
    
    # Find local SEDA files
    local_files <- find_local_education_files()
    seda_files <- local_files$seda
    
    print_msg(paste("Found", length(seda_files), "potential SEDA data files"))
    
    # Use the most recent SEDA file if available
    seda_file <- NULL
    if (length(seda_files) > 0) {
      # Sort files by modification time (newest first)
      file_info <- file.info(seda_files)
      file_info$path <- rownames(file_info)
      file_info <- file_info[order(file_info$mtime, decreasing = TRUE), ]
      
      # Use the newest file
      seda_file <- file_info$path[1]
      print_msg(paste("Using most recent SEDA file:", seda_file))
    } else {
      # Default SEDA data file if we need to download
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
  
  # Get data from different sources - use parallel processing if enabled
  if (parallel && requireNamespace("future.apply", quietly = TRUE)) {
    print_msg("Using parallel processing to fetch education data from multiple sources")
    
    # Define the data sources to fetch
    data_sources <- c("nces", "seda")
    
    # Create a function to process one data source
    process_data_source <- function(source) {
      print_msg(paste("Processing education data source:", source))
      
      if (source == "nces") {
        return(get_nces_data())
      } else if (source == "seda") {
        return(get_seda_data())
      } else {
        return(NULL)
      }
    }
    
    # Set up progress reporting if available
    if (requireNamespace("progressr", quietly = TRUE)) {
      # Create a progress handler
      progressr::handlers(progressr::handler_progress())
      
      # Process with progress tracking
      results <- progressr::with_progress({
        p <- progressr::progressor(steps = length(data_sources))
        
        future.apply::future_lapply(data_sources, function(source) {
          result <- process_data_source(source)
          p(message = paste("Processed education data source:", source))
          return(list(source = source, data = result))
        })
      })
    } else {
      # Process without progress tracking
      results <- future.apply::future_lapply(data_sources, function(source) {
        result <- process_data_source(source)
        return(list(source = source, data = result))
      })
    }
    
    # Extract results into their respective variables
    nces_data <- NULL
    seda_data <- NULL
    
    for (result in results) {
      if (result$source == "nces") {
        nces_data <- result$data
      } else if (result$source == "seda") {
        seda_data <- result$data
      }
    }
  } else {
    # Sequential processing
    print_msg("Using sequential processing to fetch education data from multiple sources")
    nces_data <- get_nces_data()
    seda_data <- get_seda_data()
  }
  
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
  } else {
    # No data available - create empty dataset with proper structure
    print_msg("No education data available. Creating empty dataset with proper structure.")
    
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
      grid[[paste0(var, "_data_source")]] <- "NO_DATA_AVAILABLE"
      grid[[paste0(var, "_data_vintage")]] <- NA_character_
    }
    
    education_data <- as_tibble(grid)
    print_msg(paste("Created empty education dataset with", nrow(education_data), "rows"))
    
    # Provide clear error message about missing data
    print_msg("ERROR: No education data files found. Please download education data.")
    print_msg("Required files should be placed in: data/education/")
    print_msg("File formats needed:")
    print_msg("1. NCES data (National Center for Education Statistics): CSV files with county-level education metrics")
    print_msg("   - Expected columns: FIPS/GEOID, student-teacher ratio, per-pupil expenditure, graduation rates")
    print_msg("   - Files should be named with year pattern (e.g., nces_2020.csv)")
    print_msg("2. SEDA data (Stanford Education Data Archive): CSV files with achievement metrics")
    print_msg("   - Expected columns: FIPS/GEOID, year, reading/math achievement gaps, opportunity indices")
    print_msg("   - Files typically named seda_county.csv or similar")
    print_msg("Alternative locations checked:")
    for (dir in c("data/education", "data/cache/education", "data/nces", "data/seda")) {
      print_msg(paste("  -", dir))
    }
    
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
  
  # Test the function with parallel processing
  result <- fetch_education_data(
    years = test_years,
    cache_dir = "data/cache",
    refresh_cache = FALSE,
    allow_interpolation = TRUE,
    data_quality_flags = list(
      direct = "direct",
      interpolated = "interpolated",
      extrapolated = "extrapolated",
      missing = NA,
      imputed = "imputed"
    ),
    parallel = TRUE
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
