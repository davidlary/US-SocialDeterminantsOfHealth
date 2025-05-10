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

# Helper function to handle log_message with different parameter sets
safe_log_message <- function(message, level = "INFO") {
  # Check if the log_message function has a show_console parameter
  if ("show_console" %in% names(formals(log_message))) {
    log_message(message, level = level, show_console = TRUE)
  } else {
    log_message(message, level = level)
  }
}

# Function to process a single IHME file - defined at module level to reduce memory overhead
# This will be used by the parallel processing but not captured in the closure
process_ihme_file <- function(file) {
  tryCatch({
    # Extract file information
    filename <- basename(file)
    
    # Parse the filename to extract year, gender, and type
    # Try different patterns for filenames
    
    # Pattern 1: Standard LT format - IHME_USA_LE_COUNTY_RACE_ETHN_2000_2019_LT_YYYY_GENDER_YYYYMMDD.CSV
    # Pattern 2: Standard MX format - IHME_USA_LE_COUNTY_RACE_ETHN_2000_2019_MX_YYYY_GENDER_YYYYMMDD.CSV
    # Pattern 3: Simple format - IHME_USA_LE_COUNTY_BOTH_2019.CSV
    # Pattern 4: Simple format - IHME_USA_LE_COUNTY_YYYY.CSV
    
    # Try to extract year and gender
    year <- NULL
    gender <- NULL
    
    # Pattern 1: LT format
    if (is.null(year) && grepl("_LT_(\\d{4})_", filename)) {
      year <- as.numeric(gsub(".*_LT_(\\d{4})_.*", "\\1", filename))
      if (grepl("_(BOTH|MALE|FEMALE)_", filename)) {
        gender <- gsub(".*_(BOTH|MALE|FEMALE)_.*", "\\1", filename)
      }
    }
    
    # Pattern 2: MX format
    if (is.null(year) && grepl("_MX_(\\d{4})_", filename)) {
      year <- as.numeric(gsub(".*_MX_(\\d{4})_.*", "\\1", filename))
      if (grepl("_(BOTH|MALE|FEMALE)_", filename)) {
        gender <- gsub(".*_(BOTH|MALE|FEMALE)_.*", "\\1", filename)
      }
    }
    
    # Pattern 3: Simple format with gender
    if (is.null(year) && grepl("_(BOTH|MALE|FEMALE)_(\\d{4})\\.CSV$", filename, ignore.case = TRUE)) {
      year <- as.numeric(gsub(".*_(BOTH|MALE|FEMALE)_(\\d{4})\\.CSV$", "\\2", filename, ignore.case = TRUE))
      gender <- gsub(".*_(BOTH|MALE|FEMALE)_(\\d{4})\\.CSV$", "\\1", filename, ignore.case = TRUE)
    }
    
    # Pattern 4: Simple format with year only
    if (is.null(year) && grepl("_(\\d{4})\\.CSV$", filename, ignore.case = TRUE)) {
      year <- as.numeric(gsub(".*_(\\d{4})\\.CSV$", "\\1", filename, ignore.case = TRUE))
      gender <- "BOTH"  # Default to BOTH if no gender specified
    }
    
    # If we couldn't parse the year and gender, return NULL
    if (is.null(year) || is.null(gender)) {
      message(paste("Could not parse year and gender from filename:", filename))
      return(NULL)
    }
    
    # Read the file
    message(paste("Reading IHME file for year", year, "and gender", gender))
    file_data <- read.csv(file, stringsAsFactors = FALSE)
    
    # Process the file based on its format
    if ("location_id" %in% names(file_data) && "location_name" %in% names(file_data)) {
      # This is the standard format with location_id, val, etc.
      message(paste("Processing IHME file with standard format:", filename))
      
      # Extract race_ethnicity from race_name or race_id
      if ("race_name" %in% names(file_data)) {
        # Map race_name to our standard codes
        race_name_mapping <- list(
          "Total" = "all",
          "Latino" = "hispanic",
          "White" = "nhw",
          "Black" = "nhb",
          "Asian" = "nhasian",
          "AIAN" = "nhaian",
          "NHPI" = "nhpi",
          "API" = "nhasian", # API (Asian/Pacific Islander) in older IHME files
          "Multiple races" = "multirace",
          "Other" = "multirace"
        )
        
        # Add race_ethnicity column based on race_name
        file_data$race_ethnicity <- sapply(file_data$race_name, function(name) {
          if (name %in% names(race_name_mapping)) {
            return(race_name_mapping[[name]])
          } else {
            return("all")  # Default to "all" if not found
          }
        })
      } else {
        # Default to "all" if we can't determine race
        file_data$race_ethnicity <- "all"
      }
      
      # Rename columns to match our schema (safely handle potential missing columns)
      rename_map <- list(
        location_id = "geoid",
        location_name = "county_name",
        val = "life_expectancy",
        lower = "le_lower_ci",
        upper = "le_upper_ci"
      )
      
      # Only rename columns that exist
      cols_to_rename <- names(rename_map)[names(rename_map) %in% names(file_data)]
      
      if (length(cols_to_rename) > 0) {
        # Create the rename mapping
        for (old_col in cols_to_rename) {
          new_col <- rename_map[[old_col]]
          names(file_data)[names(file_data) == old_col] <- new_col
          message(paste("Renamed column", old_col, "to", new_col))
        }
      } else {
        message("WARNING: No columns could be renamed - missing expected columns")
      }
      
      # Add year and gender
      file_data$year <- year
      file_data$gender <- gender
      
      return(file_data)
      
    } else if ("Location" %in% names(file_data) && "LE_both" %in% names(file_data)) {
      # This is the legacy format with Location, LE_both, LE_race_* columns
      message(paste("Processing IHME file with legacy format:", filename))
      
      # Process race-specific life expectancy data
      race_data_list <- list()
      
      # Add overall life expectancy data
      overall_data <- data.frame(
        geoid = file_data$FIPS,
        county_name = file_data$Location,
        race_ethnicity = "all",
        gender = "BOTH",
        year = year,
        life_expectancy = file_data$LE_both,
        le_lower_ci = file_data$LE_both - file_data$SD_both,
        le_upper_ci = file_data$LE_both + file_data$SD_both,
        stringsAsFactors = FALSE
      )
      
      # Add male life expectancy data
      male_data <- data.frame(
        geoid = file_data$FIPS,
        county_name = file_data$Location,
        race_ethnicity = "all",
        gender = "MALE",
        year = year,
        life_expectancy = file_data$LE_male,
        le_lower_ci = file_data$LE_male - file_data$SD_male,
        le_upper_ci = file_data$LE_male + file_data$SD_male,
        stringsAsFactors = FALSE
      )
      
      # Add female life expectancy data
      female_data <- data.frame(
        geoid = file_data$FIPS,
        county_name = file_data$Location,
        race_ethnicity = "all",
        gender = "FEMALE",
        year = year,
        life_expectancy = file_data$LE_female,
        le_lower_ci = file_data$LE_female - file_data$SD_female,
        le_upper_ci = file_data$LE_female + file_data$SD_female,
        stringsAsFactors = FALSE
      )
      
      # Combine all data
      legacy_data <- rbind(
        overall_data,
        male_data,
        female_data
      )
      
      # For each race-specific column, create a separate entry
      race_columns <- grep("^LE_race_", names(file_data), value = TRUE)
      if (length(race_columns) > 0) {
        message(paste("Found race-specific columns:", paste(race_columns, collapse = ", ")))
        
        for (race_col in race_columns) {
          # Extract the race name from the column name
          race_name <- sub("^LE_race_", "", race_col)
          
          # Map the race name to our standard code
          race_code <- switch(race_name,
                            "white" = "nhw",
                            "black" = "nhb", 
                            "hispanic" = "hispanic",
                            "asian" = "nhasian",
                            "aian" = "nhaian",
                            "api" = "nhasian",
                            "multirace" = "multirace",
                            "all") # Default
          
          # Create data frame for this race
          race_specific_data <- data.frame(
            geoid = file_data$FIPS,
            county_name = file_data$Location,
            race_ethnicity = race_code,
            gender = "BOTH",  # Race-specific data in legacy format is for both genders
            year = year,
            life_expectancy = file_data[[race_col]],
            le_lower_ci = NA,  # CIs not available in legacy format
            le_upper_ci = NA,
            stringsAsFactors = FALSE
          )
          
          # Add to the combined dataset
          legacy_data <- rbind(legacy_data, race_specific_data)
        }
      }
      
      # Format the FIPS code to match our standard geoid format
      legacy_data$geoid <- sprintf("%05d", as.numeric(legacy_data$geoid))
      
      message(paste("Processed legacy format with", nrow(legacy_data), "rows for year", year))
      
      return(legacy_data)
    } else {
      message(paste("IHME file", filename, "doesn't have expected columns"))
      message(paste("Columns found:", paste(names(file_data), collapse = ", ")))
      return(NULL)
    }
    
  }, error = function(e) {
    message(paste("Error processing IHME file", basename(file), ":", conditionMessage(e)))
    return(NULL)
  })
}

# Optimized ultra-safe merging function that avoids using merge() but with parallel batch processing
# Implements a manual join approach to avoid bus errors and memory issues
# This global function makes ALL merges in the pipeline safe
global_safe_merge <- function(df1, df2, by_cols, all.x = TRUE, all.y = FALSE, sort = FALSE, 
                              use_parallel = TRUE, batch_size = 2000) {
  # Install and load required packages for parallel processing if needed
  if (use_parallel) {
    if (!requireNamespace("future", quietly = TRUE)) {
      safe_log_message("Installing 'future' package for parallel merge processing...", level = "INFO")
      install.packages("future")
      library(future)
    }
    
    if (!requireNamespace("future.apply", quietly = TRUE)) {
      safe_log_message("Installing 'future.apply' package for parallel merge processing...", level = "INFO")
      install.packages("future.apply")
      library(future.apply)
    }
    
    if (requireNamespace("future", quietly = TRUE) && !isNamespaceLoaded("future")) {
      library(future)
    }
    
    if (requireNamespace("future.apply", quietly = TRUE) && !isNamespaceLoaded("future.apply")) {
      library(future.apply)
    }
  }
  
  # Check if future and future.apply are available for parallel processing
  can_use_parallel <- use_parallel && 
                      requireNamespace("future", quietly = TRUE) && 
                      requireNamespace("future.apply", quietly = TRUE)
  
  # Handle parameters to match merge() function
  all_arg <- if (all.x && all.y) TRUE else if (all.x) TRUE else if (all.y) TRUE else FALSE
  
  # Check if both dataframes have data
  if (nrow(df1) == 0) return(df2)
  if (nrow(df2) == 0) return(df1)
  
  # Force garbage collection before the operation
  gc()
  
  # Create a new dataframe to store the result
  if (can_use_parallel) {
    safe_log_message("Performing parallel ultra-safe manual dataframe join...", level = "INFO")
  } else {
    safe_log_message("Performing sequential ultra-safe manual dataframe join...", level = "INFO")
  }
  
  # Ensure by columns have consistent types
  for (col in by_cols) {
    if (col %in% names(df1) && col %in% names(df2)) {
      # Convert to character to ensure type consistency
      df1[[col]] <- as.character(df1[[col]])
      df2[[col]] <- as.character(df2[[col]])
    }
  }
  
  # Create a unique key for each row based on by_cols - optimized version
  create_key <- function(df, by_cols) {
    # Pre-allocate vector for keys
    keys <- character(nrow(df))
    
    # Use vectorized operations where possible
    if (length(by_cols) == 1) {
      # Fast path for single key column
      keys <- as.character(df[[by_cols]])
    } else {
      # Combine values to create unique keys
      for (i in 1:nrow(df)) {
        # Create key from all by columns with a separator
        key_parts <- sapply(by_cols, function(col) as.character(df[i, col]))
        keys[i] <- paste(key_parts, collapse = "__|__")
      }
    }
    
    return(keys)
  }
  
  # Create keys for both dataframes
  safe_log_message("Creating join keys...", level = "INFO")
  keys1 <- create_key(df1, by_cols)
  keys2 <- create_key(df2, by_cols)
  
  # Add keys to dataframes
  df1$manual_join_key <- keys1
  df2$manual_join_key <- keys2
  
  # Get all unique keys based on merge type
  safe_log_message("Identifying unique keys for join...", level = "INFO")
  all_keys <- if (all.x && all.y) {
    unique(c(keys1, keys2))  # Full outer join (all=TRUE)
  } else if (all.x) {
    unique(c(keys1))         # Left join
  } else if (all.y) {
    unique(c(keys2))         # Right join
  } else {
    unique(intersect(keys1, keys2))  # Inner join
  }
  
  # Create column sets (excluding join columns)
  cols1 <- setdiff(names(df1), c(by_cols, "manual_join_key"))
  cols2 <- setdiff(names(df2), c(by_cols, "manual_join_key"))
  
  # Check for column name conflicts
  conflicts <- intersect(cols1, cols2)
  if (length(conflicts) > 0) {
    safe_log_message(paste("Column name conflicts detected:", paste(conflicts, collapse=", ")),
                   level = "WARN")
    # Rename conflicting columns in df2
    for (col in conflicts) {
      new_name <- paste0(col, "_df2")
      names(df2)[names(df2) == col] <- new_name
      cols2[cols2 == col] <- new_name
    }
  }
  
  # Initialize result columns
  result_cols <- c(by_cols, cols1, cols2)
  
  # Log info about join operation
  safe_log_message(paste("Processing", length(all_keys), "unique keys for manual join..."),
                 level = "INFO")
  
  # Determine batch processing approach - use larger batch size with parallel
  if (can_use_parallel) {
    # Use a larger batch size for parallel processing
    batch_size <- max(batch_size, 2000)
  } else {
    # Use a smaller batch size for sequential processing to avoid memory issues
    batch_size <- min(batch_size, 1000)
  }
  
  # Calculate number of batches
  num_batches <- ceiling(length(all_keys) / batch_size)
  
  # Function to process a batch of keys
  process_batch <- function(batch_idx) {
    # Calculate batch indices
    start_idx <- (batch_idx - 1) * batch_size + 1
    end_idx <- min(batch_idx * batch_size, length(all_keys))
    batch_keys <- all_keys[start_idx:end_idx]
    
    # Create a list to store results for this batch
    batch_results <- vector("list", length(batch_keys))
    
    # Process each key in this batch
    for (i in 1:length(batch_keys)) {
      key <- batch_keys[i]
      
      # Find matching rows
      match1 <- which(df1$manual_join_key == key)
      match2 <- which(df2$manual_join_key == key)
      
      # Process based on matches
      if (length(match1) > 0 && length(match2) > 0) {
        # Both dataframes have matching rows
        row1 <- df1[match1[1], ]
        row2 <- df2[match2[1], ]
        
        # Create new row with values from both
        new_row <- list()
        
        # Add join columns
        for (col in by_cols) {
          new_row[[col]] <- row1[[col]]
        }
        
        # Add df1 columns
        for (col in cols1) {
          new_row[[col]] <- row1[[col]]
        }
        
        # Add df2 columns
        for (col in cols2) {
          new_row[[col]] <- row2[[col]]
        }
        
        batch_results[[i]] <- new_row
        
      } else if (length(match1) > 0 && all.x) {
        # Only df1 has matching row and we want all rows from df1
        row1 <- df1[match1[1], ]
        
        # Create new row with values from df1 only
        new_row <- list()
        
        # Add join columns
        for (col in by_cols) {
          new_row[[col]] <- row1[[col]]
        }
        
        # Add df1 columns
        for (col in cols1) {
          new_row[[col]] <- row1[[col]]
        }
        
        # Add NA for df2 columns
        for (col in cols2) {
          new_row[[col]] <- NA
        }
        
        batch_results[[i]] <- new_row
        
      } else if (length(match2) > 0 && all.y) {
        # Only df2 has matching row and we want all rows from df2
        row2 <- df2[match2[1], ]
        
        # Create new row with values from df2 only
        new_row <- list()
        
        # Add join columns
        for (col in by_cols) {
          new_row[[col]] <- row2[[col]]
        }
        
        # Add NA for df1 columns
        for (col in cols1) {
          new_row[[col]] <- NA
        }
        
        # Add df2 columns
        for (col in cols2) {
          new_row[[col]] <- row2[[col]]
        }
        
        batch_results[[i]] <- new_row
      }
    }
    
    # Return results with batch position information
    return(list(
      start_idx = start_idx,
      end_idx = end_idx,
      results = batch_results
    ))
  }
  
  # Process batches with progress reporting
  if (can_use_parallel) {
    # Set up parallel processing
    num_cores <- parallel::detectCores() - 1
    num_cores <- max(2, num_cores) # Use at least 2 cores
    
    # Strategy based on OS
    strategy <- if (.Platform$OS.type == "windows") {
      "multisession"
    } else {
      "multicore" 
    }
    
    # Plan execution
    future::plan(strategy, workers = num_cores)
    
    # Set memory limit (4GB) - more conservative to avoid global size errors
    options(future.globals.maxSize = 4 * 1024^3)
    
    safe_log_message(paste("Using parallel processing with", num_cores, "cores for manual join"),
                   level = "INFO")
    
    # Process batches in parallel with progress tracking if available
    if (requireNamespace("progressr", quietly = TRUE)) {
      # Setup progress handler
      progressr::handlers(progressr::handler_progress())
      
      # Process with progress
      batch_results <- progressr::with_progress({
        p <- progressr::progressor(steps = num_batches)
        
        future.apply::future_lapply(1:num_batches, function(batch) {
          result <- process_batch(batch)
          p(message = paste("Processed batch", batch, "of", num_batches))
          return(result)
        })
      })
    } else {
      # Process without progress tracking
      batch_results <- future.apply::future_lapply(1:num_batches, process_batch)
    }
  } else {
    # Sequential processing
    safe_log_message("Using sequential processing for manual join", level = "INFO")
    
    batch_results <- list()
    for (batch in 1:num_batches) {
      safe_log_message(paste("Processing batch", batch, "of", num_batches),
                     level = "INFO")
      batch_results[[batch]] <- process_batch(batch)
      
      # Force garbage collection after each batch
      if (batch %% 10 == 0) gc()
    }
  }
  
  # Combine batch results
  safe_log_message("Combining batch results...", level = "INFO")
  
  # Combine all rows from all batches
  all_rows <- vector("list", length(all_keys))
  
  for (batch_result in batch_results) {
    start_idx <- batch_result$start_idx
    results <- batch_result$results
    
    # Add rows to the combined results list at the correct positions
    for (i in 1:length(results)) {
      idx <- start_idx + i - 1
      all_rows[[idx]] <- results[[i]]
    }
  }
  
  # Filter out NULL rows 
  valid_rows <- which(!sapply(all_rows, is.null))
  if (length(valid_rows) == 0) {
    safe_log_message("No valid rows found in manual join result", level = "ERROR")
    # Return the larger dataframe as fallback
    if (nrow(df1) >= nrow(df2)) return(df1) else return(df2)
  }
  
  # Create the result dataframe efficiently
  safe_log_message("Creating result dataframe from processed rows...", level = "INFO")
  
  # Create a new data frame with the appropriate columns
  result <- as.data.frame(matrix(NA, nrow = length(valid_rows), 
                             ncol = length(result_cols),
                             dimnames = list(NULL, result_cols)))
  
  # Fill in the data
  for (i in seq_along(valid_rows)) {
    row_idx <- valid_rows[i]
    row_data <- all_rows[[row_idx]]
    
    for (col in result_cols) {
      if (col %in% names(row_data)) {
        result[i, col] <- row_data[[col]]
      }
    }
  }
  
  # Clean up temporary columns
  df1$manual_join_key <- NULL
  df2$manual_join_key <- NULL
  
  # Sort if requested
  if (sort) {
    result <- result[order(result[[by_cols[1]]]), ]
  }
  
  # Force garbage collection
  rm(df1, df2, all_rows, batch_results)
  gc()
  
  safe_log_message(paste("Manual join completed successfully with", 
                      nrow(result), "rows and", ncol(result), "columns"),
                 level = "INFO")
  
  return(result)
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
  # Define cache file path
  cache_dir <- "data/cache"
  cache_file <- file.path(cache_dir, "census_data.rds")
  
  # Check if cache exists and we can use it
  if (file.exists(cache_file) && use_cache && !refresh_cache) {
    safe_log_message("Loading Census data from cache...", level = "INFO")
    return(readRDS(cache_file))
  }
  
  # Check for pre-downloaded Census data files
  census_dirs <- c(
    "data/census_acs",
    "data/census_decennial",
    "data/census_pep",
    "data/cache/census"
  )
  
  # Look for CSV files with Census data
  census_files <- list()
  for (dir in census_dirs) {
    if (dir.exists(dir)) {
      # Look for CSV files with Census data
      files <- list.files(
        path = dir,
        pattern = "acs.*\\.csv$|dec.*\\.csv$|pep.*\\.csv$|census.*\\.csv$",
        full.names = TRUE,
        recursive = TRUE,
        ignore.case = TRUE
      )
      
      # Add to the list
      census_files <- c(census_files, files)
    }
  }
  
  # Check if we found any files
  if (length(census_files) == 0) {
    safe_log_message("ERROR: No Census data files found. Please download Census data.",
               level = "ERROR")
    safe_log_message("Required files should be in one of the following directories:",
               level = "ERROR")
    safe_log_message(paste(census_dirs, collapse = ", "),
               level = "ERROR")
    safe_log_message("File names should include 'acs', 'dec', or 'pep' with a CSV extension.",
               level = "ERROR")
    
    # Return empty dataframe with proper structure
    return(data.frame(
      geoid = character(0),
      name = character(0),
      state_fips = character(0),
      state_name = character(0),
      year = integer(0)
    ))
  }
  
  # Process files to create the combined dataset
  safe_log_message(paste("Found", length(census_files), "Census data files. Processing..."),
             level = "INFO")
  
  # Initialize list for each file's data
  file_data_list <- list()
  
  # Process each file
  for (file in census_files) {
    safe_log_message(paste("Processing Census file:", basename(file)),
               level = "INFO")
    
    # Extract year and type from filename
    filename <- basename(file)
    year_match <- regexpr("_[0-9]{4}", filename)
    
    # Figure out which type of Census data
    data_type <- if (grepl("acs", filename, ignore.case = TRUE)) {
      "ACS"
    } else if (grepl("dec", filename, ignore.case = TRUE)) {
      "Decennial"
    } else if (grepl("pep", filename, ignore.case = TRUE)) {
      "PEP"
    } else {
      "Unknown"
    }
    
    # Extract year if possible
    file_year <- if (year_match > 0) {
      as.numeric(substr(filename, year_match + 1, year_match + 4))
    } else {
      NA_integer_
    }
    
    # Only process if year is in the requested range
    if (!is.na(file_year) && file_year %in% years) {
      # Read the file
      file_data <- tryCatch({
        read.csv(file, stringsAsFactors = FALSE)
      }, error = function(e) {
        safe_log_message(paste("Error reading file:", e$message),
                   level = "ERROR")
        return(NULL)
      })
      
      # Process if we successfully read the file
      if (!is.null(file_data) && nrow(file_data) > 0) {
        # Ensure we have standard column names
        # Look for FIPS code
        if (!"geoid" %in% names(file_data)) {
          # Look for alternate column names
          fips_cols <- grep("fips|geoid|county_code|state_county", 
                           names(file_data), ignore.case = TRUE, value = TRUE)
          
          if (length(fips_cols) > 0) {
            # Rename the first match to geoid
            names(file_data)[names(file_data) == fips_cols[1]] <- "geoid"
          } else if ("state" %in% names(file_data) && "county" %in% names(file_data)) {
            # Construct FIPS from state and county
            file_data$geoid <- sprintf("%02d%03d", 
                                     as.numeric(file_data$state), 
                                     as.numeric(file_data$county))
          } else {
            # Can't determine FIPS code
            safe_log_message(paste("Cannot determine FIPS code in file:", filename),
                       level = "WARN")
            # Skip this file
            next
          }
        }
        
        # Ensure GEOID is standardized
        file_data$geoid <- sprintf("%05d", as.numeric(file_data$geoid))
        
        # Add year if missing
        if (!"year" %in% names(file_data)) {
          file_data$year <- file_year
        }
        
        # Add data source
        file_data$data_source <- paste("US Census Bureau", data_type)
        file_data$data_quality <- "direct"
        
        # Add to list
        file_data_list[[basename(file)]] <- file_data
      }
    }
  }
  
  # Combine all data
  if (length(file_data_list) == 0) {
    safe_log_message("ERROR: No valid Census data found for requested years.",
               level = "ERROR")
    
    # Return empty dataframe with proper structure
    return(data.frame(
      geoid = character(0),
      name = character(0),
      state_fips = character(0),
      state_name = character(0),
      year = integer(0)
    ))
  }
  
  # Combine all files
  combined_census_data <- bind_rows(file_data_list)
  
  # If we have Census data, save to cache
  if (nrow(combined_census_data) > 0) {
    safe_log_message(paste("Saving combined Census data with", 
                     nrow(combined_census_data), "rows to cache..."),
               level = "INFO")
    
    # Make sure cache directory exists
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Save to cache
    saveRDS(combined_census_data, cache_file)
  }
  
  return(combined_census_data)
}

#' Process and combine data from all sources
#'
#' This function processes and combines data from Census, NHGIS,
#' traffic safety, and other sources into a unified dataset.
#'
#' @param census_data Dataframe with Census data
#' @param nhgis_data Dataframe with NHGIS data (can be NULL)
#' @param years Vector of years to process
#' @param crosswalk Variable crosswalk
#' @param economic_data Economic factors data
#' @param education_data Education data
#' @param cdc_places_data CDC PLACES health data
#' @param life_expectancy_data IHME life expectancy data
#' @param healthcare_data Healthcare access data
#' @param housing_data Housing data
#' @param environmental_data Environmental factors data
#' @param smart_location_data EPA smart location data
#' @param food_data Food environment data
#' @param transportation_data Transportation data
#' @param traffic_safety_data Traffic safety data
#' @param social_cohesion_data Social cohesion data
#' @param crime_data Crime and safety data
#' @param park_access_data Park and recreation access data
#' @param climate_data Climate and weather data
#' @return A processed dataset with all variables
get_processed_data <- function(census_data, nhgis_data, years, crosswalk,
                              economic_data = NULL, education_data = NULL,
                              cdc_places_data = NULL, life_expectancy_data = NULL,
                              healthcare_data = NULL, housing_data = NULL,
                              environmental_data = NULL, smart_location_data = NULL,
                              food_data = NULL, transportation_data = NULL,
                              traffic_safety_data = NULL, social_cohesion_data = NULL,
                              crime_data = NULL, park_access_data = NULL,
                              climate_data = NULL) {
  log_message("Processing and combining data from all sources...",
             level = "INFO", show_console = TRUE)
  
  # Create a base dataset with county IDs and years
  if (is.null(census_data) || nrow(census_data) == 0) {
    log_message("WARNING: No Census data available. Creating base template only.",
               level = "WARN", show_console = TRUE)
    
    # Try to get county data from shapefiles or any other source
    counties <- NULL
    
    # Check in shapefiles directory
    shapefile_index_path <- "data/shapefiles/shapefile_index.csv"
    if (file.exists(shapefile_index_path)) {
      log_message("Trying to extract county information from shapefile index...",
                 level = "INFO", show_console = TRUE)
      shapefile_index <- read.csv(shapefile_index_path, stringsAsFactors = FALSE)
      
      if ("geoid" %in% names(shapefile_index) && "name" %in% names(shapefile_index)) {
        counties <- shapefile_index %>%
          select(geoid, name) %>%
          distinct()
        
        # Extract state FIPS from county FIPS
        counties$state_fips <- substr(counties$geoid, 1, 2)
        
        # Add state names
        # First create a state lookup
        state_lookup <- data.frame(
          state_fips = sprintf("%02d", 1:56),
          state_name = c(state.name, "District of Columbia", 
                        "Puerto Rico", "Virgin Islands", 
                        "Guam", "American Samoa", "Northern Mariana Islands"),
          stringsAsFactors = FALSE
        )
        
        # Join to get state names
        counties <- counties %>%
          left_join(state_lookup, by = "state_fips")
      }
    }
    
    # If still no county data, create minimal template
    if (is.null(counties) || nrow(counties) == 0) {
      log_message("WARNING: Could not find any county information. Creating minimal template.",
                 level = "WARN", show_console = TRUE)
      
      # Create empty dataframe
      counties <- data.frame(
        geoid = character(0),
        name = character(0),
        state_fips = character(0),
        state_name = character(0),
        stringsAsFactors = FALSE
      )
    }
    
    if (nrow(counties) > 0) {
      # Create a dataset with all years and counties
      years_df <- expand.grid(
        geoid = counties$geoid,
        year = years,
        stringsAsFactors = FALSE
      )
      
      # Merge counties info using the ultra-safe merge
      full_data <- global_safe_merge(years_df, counties, by_cols = "geoid")
      
      log_message(paste("Created base template with", nrow(full_data), 
                       "rows for", length(unique(counties$geoid)), 
                       "counties and", length(years), "years."),
                 level = "INFO", show_console = TRUE)
    } else {
      # No counties - create empty dataset with correct columns
      full_data <- data.frame(
        geoid = character(0),
        year = integer(0),
        name = character(0),
        state_fips = character(0),
        state_name = character(0),
        stringsAsFactors = FALSE
      )
      
      log_message("WARNING: Empty dataset created (no counties found).",
                 level = "WARN", show_console = TRUE)
    }
  } else {
    # Use the Census data as the base
    full_data <- census_data
    log_message(paste("Using Census data as base with", nrow(full_data), "rows."),
               level = "INFO", show_console = TRUE)
  }
  
  # First, try to get traffic safety data and add it to our dataset
  log_message("Adding traffic safety data...",
             level = "INFO", show_console = TRUE)
  
  if (exists("get_traffic_safety_data")) {
    # Fetch traffic safety data using the traffic safety module
    tryCatch({
      ts_data <- get_traffic_safety_data(years = years)
      
      # Check if ts_data has the required geoid and year columns for joining
      if (is.data.frame(ts_data) && nrow(ts_data) > 0 && 
          all(c("geoid", "year") %in% names(ts_data))) {
        # Get list of traffic safety variables
        ts_vars <- intersect(
          names(ts_data),
          crosswalk$variable_name[crosswalk$domain == "Traffic Safety"]
        )
        
        # If no exact matches found, try a looser match
        if (length(ts_vars) == 0) {
          ts_vars <- grep("fatalities|fatality_rate", names(ts_data), value = TRUE)
        }
        
        # If we have variables to add
        if (length(ts_vars) > 0) {
          log_message(paste("Found", length(ts_vars), "traffic safety variables to add"),
                     level = "INFO", show_console = TRUE)
          
          # Add data quality columns if they exist
          quality_vars <- character(0)
          for (var in ts_vars) {
            qual_col <- paste0(var, "_data_quality")
            if (qual_col %in% names(ts_data)) {
              quality_vars <- c(quality_vars, qual_col)
            }
          }
          
          # Prepare data for merge
          ts_merge_data <- ts_data[, c("geoid", "year", ts_vars, quality_vars)]
          
          # Merge with full_data using the ultra-safe merge
          full_data <- global_safe_merge(full_data, ts_merge_data, 
                                      by_cols = c("geoid", "year"), 
                                      all.x = TRUE)
          
          log_message(paste("Added traffic safety data to dataset."),
                     level = "INFO", show_console = TRUE)
        } else {
          log_message("No traffic safety variables found in data",
                     level = "WARN", show_console = TRUE)
        }
      } else {
        log_message("Traffic safety data doesn't have required columns for joining",
                   level = "WARN", show_console = TRUE)
      }
    }, error = function(e) {
      log_message(paste("Error adding traffic safety data:", conditionMessage(e)),
                 level = "ERROR", show_console = TRUE)
    })
  } else {
    log_message("Traffic safety data function not found - skipping",
               level = "WARN", show_console = TRUE)
  }
  
  # Merge in all domain data
  merge_domain_data <- function(full_data, domain_data, domain_name) {
    if (!is.null(domain_data) && is.data.frame(domain_data) && nrow(domain_data) > 0) {
      log_message(paste("Merging", domain_name, "data with", nrow(domain_data), "rows"),
                 level = "INFO", show_console = TRUE)
      
      # Check for required join columns
      if (all(c("geoid", "year") %in% names(domain_data))) {
        # Get domain variables from crosswalk if available
        domain_vars <- intersect(names(domain_data), crosswalk$variable_name)
        
        # If no exact matches, try to find any useful variables
        if (length(domain_vars) == 0) {
          # Exclude common columns for merging
          exclude_cols <- c("geoid", "year", "state_fips", "state_name", "county_name")
          domain_vars <- setdiff(names(domain_data), exclude_cols)
        }
        
        if (length(domain_vars) > 0) {
          log_message(paste("Found", length(domain_vars), domain_name, "variables to add"),
                     level = "INFO", show_console = TRUE)
          
          # Add data quality columns if they exist
          quality_vars <- character(0)
          for (var in domain_vars) {
            qual_col <- paste0(var, "_data_quality")
            if (qual_col %in% names(domain_data)) {
              quality_vars <- c(quality_vars, qual_col)
            }
          }
          
          # Check for and handle potential merge columns
          merge_cols <- intersect(c("geoid", "year"), names(domain_data))
          if (length(merge_cols) < 2) {
            log_message(paste("WARNING:", domain_name, "data doesn't have both geoid and year. Using available columns."),
                       level = "WARN", show_console = TRUE)
          }
          
          # Prepare data for merge  
          domain_merge_data <- domain_data[, c(merge_cols, domain_vars, quality_vars)]
          
          # Merge with full_data using the ultra-safe merge
          full_data <- global_safe_merge(full_data, domain_merge_data, 
                                      by_cols = merge_cols, 
                                      all.x = TRUE)
          
          log_message(paste("Added", domain_name, "data to dataset."),
                     level = "INFO", show_console = TRUE)
          
          # Add data quality flags for any added variables without existing quality flags
          for (var in domain_vars) {
            qual_col <- paste0(var, "_data_quality")
            if (var %in% names(full_data) && !qual_col %in% names(full_data)) {
              full_data[[qual_col]] <- "direct"
            }
          }
        } else {
          log_message(paste("No variables found in", domain_name, "data"),
                     level = "WARN", show_console = TRUE)
        }
      } else {
        log_message(paste(domain_name, "data doesn't have required columns for joining"),
                   level = "WARN", show_console = TRUE)
      }
    }
    
    return(full_data)
  }
  
  # Process each domain in turn
  log_message("Merging all domain data...", level = "INFO", show_console = TRUE)
  
  # 2. Economic Factors
  full_data <- merge_domain_data(full_data, economic_data, "economic factors")
  
  # 3. Education
  full_data <- merge_domain_data(full_data, education_data, "education")
  
  # 4. Health Status - CDC PLACES
  full_data <- merge_domain_data(full_data, cdc_places_data, "CDC PLACES health")
  
  # 5. IHME Life Expectancy - process after other domains
  
  # 6. Healthcare Access
  full_data <- merge_domain_data(full_data, healthcare_data, "healthcare access")
  
  # 7. Housing
  full_data <- merge_domain_data(full_data, housing_data, "housing")
  
  # 8. Environmental Factors
  full_data <- merge_domain_data(full_data, environmental_data, "environmental factors")
  full_data <- merge_domain_data(full_data, smart_location_data, "smart location")
  
  # 9. Food Environment
  full_data <- merge_domain_data(full_data, food_data, "food environment")
  
  # 10. Transportation
  full_data <- merge_domain_data(full_data, transportation_data, "transportation")
  
  # 11. Traffic Safety
  full_data <- merge_domain_data(full_data, traffic_safety_data, "traffic safety")
  
  # 12. Social Cohesion
  full_data <- merge_domain_data(full_data, social_cohesion_data, "social cohesion")
  
  # 13. Crime and Safety
  full_data <- merge_domain_data(full_data, crime_data, "crime and safety")
  
  # 14. Built Environment
  full_data <- merge_domain_data(full_data, park_access_data, "built environment")
  
  # 15. Climate & Weather
  full_data <- merge_domain_data(full_data, climate_data, "climate and weather")
  
  # Add missing variables - BUT NOT WITH SIMULATED DATA
  log_message("Checking for missing variables...",
             level = "INFO", show_console = TRUE)
  
  # Go through all variables in the crosswalk
  for (var in crosswalk$variable_name) {
    # Check if this variable exists in the data
    if (!var %in% names(full_data)) {
      # Add the column but set to NA (not simulated data)
      full_data[[var]] <- NA
      
      # Add data quality flag showing it's missing
      qual_col <- paste0(var, "_data_quality")
      full_data[[qual_col]] <- "missing"
      
      # Log that this variable is missing
      log_message(paste("Variable", var, "is not available in the dataset. Marked as missing."),
                 level = "INFO", show_console = TRUE)
    }
  }
  
  # Add IHME life expectancy data (using actual data files)
  log_message("Adding IHME life expectancy data...",
             level = "INFO", show_console = TRUE)
  
  ihme_vars <- crosswalk$variable_name[crosswalk$source == "IHME (Institute for Health Metrics and Evaluation)"]
  if (length(ihme_vars) > 0) {
    # Define the IHME data directory
    ihme_dir <- "data/ihme/CSV"
    
    # Check if the directory exists
    if (dir.exists(ihme_dir)) {
      # Get IHME CSV files
      ihme_files <- list.files(ihme_dir, pattern = "\\.CSV$", full.names = TRUE)
      
      if (length(ihme_files) > 0) {
        log_message(paste("Found", length(ihme_files), "IHME data files"),
                   level = "INFO", show_console = TRUE)
        
        # Ensure all required packages are installed and loaded
        required_packages <- c("future", "future.apply", "furrr", "progressr", "parallel")
        for (pkg in required_packages) {
          if (!requireNamespace(pkg, quietly = TRUE)) {
            log_message(paste("Installing required package:", pkg), 
                       level = "INFO", show_console = TRUE)
            install.packages(pkg)
            library(pkg, character.only = TRUE)
          }
        }
        
        # Make sure key packages are loaded
        if (requireNamespace("future", quietly = TRUE) && 
            !isNamespaceLoaded("future")) {
          library(future)
        }
        
        if (requireNamespace("future.apply", quietly = TRUE) && 
            !isNamespaceLoaded("future.apply")) {
          library(future.apply)
        }
        
        if (requireNamespace("progressr", quietly = TRUE) && 
            !isNamespaceLoaded("progressr")) {
          library(progressr)
        }
        
        # This function is moved outside the closure to reduce memory footprint
        # We'll use a simple wrapper function inside the parallel processing section
        
        # Process IHME files in parallel using the parallel map function
        log_message("Processing IHME files in parallel...",
                   level = "INFO", show_console = TRUE)
        
        # Set up parallel processing
        cores_to_use <- min(length(ihme_files), parallel::detectCores() - 1)
        cores_to_use <- max(cores_to_use, 2)  # Use at least 2 cores
        
        # Increase memory limit for future package
        options(future.globals.maxSize = 4 * 1024^3)  # 4 GB - more conservative to avoid memory errors
        
        # Use future package for parallel processing with chunking
        future::plan(future::multisession, workers = cores_to_use)
        log_message(paste("Using", cores_to_use, "cores for parallel IHME file processing"),
                   level = "INFO", show_console = TRUE)
        
        # Process files in smaller chunks to avoid memory issues
        chunk_size <- 20  # Process 20 files at a time
        num_chunks <- ceiling(length(ihme_files) / chunk_size)
        log_message(paste("Processing", length(ihme_files), "files in", num_chunks, "chunks of", chunk_size, "files each"),
                   level = "INFO", show_console = TRUE)
        
        # Process files using enhanced chunked parallel processing with progress tracking
        log_message("Using enhanced parallel processing with progress tracking for IHME files",
                   level = "INFO", show_console = TRUE)
        
        # Create a wrapper function that handles both processing and progress updates
        process_chunked_ihme_files <- function() {
          # Create a progress bar if progressr is available
          if (requireNamespace("progressr", quietly = TRUE)) {
            with_progress <- progressr::with_progress
            p <- progressr::progressor(steps = length(ihme_files))
          } else {
            # No progress reporting available, create a dummy function
            with_progress <- function(expr) expr
            p <- function(...) NULL
          }
          
          # Process with progress tracking
          with_progress({
            # Process files in chunks
            ihme_results <- list()
            for (chunk_idx in 1:num_chunks) {
                chunk_start <- (chunk_idx - 1) * chunk_size + 1
                chunk_end <- min(chunk_idx * chunk_size, length(ihme_files))
                log_message(paste("Processing chunk", chunk_idx, "of", num_chunks, "(files", chunk_start, "to", chunk_end, ")"),
                           level = "INFO", show_console = TRUE)
                
                # Get files for this chunk
                chunk_files <- ihme_files[chunk_start:chunk_end]
                
                # Process this chunk with enhanced error handling and fallbacks
                chunk_results <- tryCatch({
                    # Use future_lapply with retry capabilities
                    if (requireNamespace("future.apply", quietly = TRUE)) {
                        # Define a minimal wrapper that updates progress
                        # Using a small function with minimal environment capture
                        minimal_process_function <- function(file) {
                            # Use the globally defined process_ihme_file function 
                            # to avoid capturing the entire environment
                            result <- process_ihme_file(file)
                            p(message = paste("Processed", basename(file)))
                            return(result)
                        }
                        
                        # Process with progress updates
                        future.apply::future_lapply(
                            chunk_files, 
                            minimal_process_function, 
                            future.scheduling = TRUE,
                            future.chunk.size = min(5, length(chunk_files)),
                            future.packages = c("dplyr", "readr") # Specify packages to reduce globals
                        )
                    } else {
                        # Fallback to basic parallel processing
                        parallel::mclapply(chunk_files, function(file) {
                            result <- process_ihme_file(file)
                            p(message = paste("Processed", basename(file)))
                            return(result)
                        }, mc.cores = cores_to_use)
                    }
                }, error = function(e) {
                    log_message(paste("Error in parallel processing chunk", chunk_idx, ":", conditionMessage(e)),
                               level = "ERROR", show_console = TRUE)
                    log_message("Falling back to sequential processing for this chunk",
                               level = "WARN", show_console = TRUE)
                    
                    # Fall back to sequential processing with progress
                    lapply(chunk_files, function(file) {
                        result <- process_ihme_file(file)
                        p(message = paste("Processed", basename(file)))
                        return(result)
                    })
                })
                
                # Add results to main list - use a safer approach
                ihme_results <- append(ihme_results, chunk_results)
                
                # Force garbage collection to free memory
                log_message(paste("Completed chunk", chunk_idx, "of", num_chunks, "- running garbage collection"),
                           level = "INFO", show_console = TRUE)
                gc(full = TRUE)
                
                # Log memory usage if available
                if (requireNamespace("pryr", quietly = TRUE)) {
                    tryCatch({
                        mem_used <- pryr::mem_used() / 1024^2  # MB
                        log_message(paste("Current memory usage:", round(mem_used, 1), "MB"),
                                   level = "INFO", show_console = TRUE)
                    }, error = function(e) {
                        # Silently ignore memory reporting errors
                    })
                }
            }
            
            return(ihme_results)
          })
        }
        
        # Execute the chunked processing function
        ihme_results <- process_chunked_ihme_files()
        
        # Filter out NULL results and bind rows
        log_message("Combining all IHME file results...",
                   level = "INFO", show_console = TRUE)
        valid_results <- ihme_results[!sapply(ihme_results, is.null)]
        
        # Check if we have any valid results
        if (length(valid_results) == 0) {
            log_message("ERROR: No valid IHME data was processed. Check file formats and paths.",
                       level = "ERROR", show_console = TRUE)
            ihme_data <- NULL
        } else {
            # Log the number of valid results
            log_message(paste("Found", length(valid_results), "valid IHME file results"),
                       level = "INFO", show_console = TRUE)
            
            # Check column consistency before combining
            all_columns <- lapply(valid_results, names)
            unique_column_sets <- unique(lapply(all_columns, function(cols) paste(sort(cols), collapse=",")))
            
            if (length(unique_column_sets) > 1) {
                log_message("WARNING: Found different column sets in IHME files. Standardizing before combining.",
                           level = "WARN", show_console = TRUE)
                
                # Identify all unique columns across all results
                all_unique_columns <- unique(unlist(all_columns))
                required_columns <- c("geoid", "county_name", "year", "gender", "race_ethnicity", 
                                     "life_expectancy", "le_lower_ci", "le_upper_ci")
                
                # Ensure required columns are present
                missing_required <- setdiff(required_columns, all_unique_columns)
                if (length(missing_required) > 0) {
                    log_message(paste("ERROR: Missing required columns in IHME data:", 
                                     paste(missing_required, collapse=", ")),
                               level = "ERROR", show_console = TRUE)
                    
                    # Try to rename possible matching columns if they exist
                    if ("val" %in% all_unique_columns && "life_expectancy" %in% missing_required) {
                        log_message("Attempting to rename 'val' to 'life_expectancy'",
                                   level = "INFO", show_console = TRUE)
                        for (i in seq_along(valid_results)) {
                            if ("val" %in% names(valid_results[[i]])) {
                                names(valid_results[[i]])[names(valid_results[[i]]) == "val"] <- "life_expectancy"
                            }
                        }
                    }
                    
                    if ("lower" %in% all_unique_columns && "le_lower_ci" %in% missing_required) {
                        log_message("Attempting to rename 'lower' to 'le_lower_ci'",
                                   level = "INFO", show_console = TRUE)
                        for (i in seq_along(valid_results)) {
                            if ("lower" %in% names(valid_results[[i]])) {
                                names(valid_results[[i]])[names(valid_results[[i]]) == "lower"] <- "le_lower_ci"
                            }
                        }
                    }
                    
                    if ("upper" %in% all_unique_columns && "le_upper_ci" %in% missing_required) {
                        log_message("Attempting to rename 'upper' to 'le_upper_ci'",
                                   level = "INFO", show_console = TRUE)
                        for (i in seq_along(valid_results)) {
                            if ("upper" %in% names(valid_results[[i]])) {
                                names(valid_results[[i]])[names(valid_results[[i]]) == "upper"] <- "le_upper_ci"
                            }
                        }
                    }
                }
                
                # Standardize all dataframes to have the same columns
                standardized_results <- lapply(valid_results, function(df) {
                    # Add missing columns with NA values
                    for (col in all_unique_columns) {
                        if (!(col %in% names(df))) {
                            df[[col]] <- NA
                        }
                    }
                    return(df)
                })
                
                # Now combine the standardized results
                log_message("Combining standardized IHME file results",
                           level = "INFO", show_console = TRUE)
                ihme_data <- do.call(rbind, standardized_results)
            } else {
                # Columns are consistent, combine directly
                log_message(paste("All", length(valid_results), "IHME files have consistent columns. Combining directly."),
                           level = "INFO", show_console = TRUE)
                ihme_data <- do.call(rbind, valid_results)
            }
        }
        
        # If we have IHME data, process it and add to full_data
        if (!is.null(ihme_data) && nrow(ihme_data) > 0) {
          log_message(paste("Successfully loaded", nrow(ihme_data), "rows of IHME life expectancy data"),
                     level = "INFO", show_console = TRUE)
          
          # Convert geoid to match the format in full_data
          ihme_data$geoid <- sprintf("%05d", as.numeric(ihme_data$geoid))
          
          # Process IHME life expectancy data with all race/ethnicity breakdowns
          log_message("Processing IHME life expectancy data with race/ethnicity breakdowns",
                      level = "INFO", show_console = TRUE)
                      
          # Create separate dataframes for each gender
          both_data <- ihme_data %>% filter(gender == "BOTH")
          male_data <- ihme_data %>% filter(gender == "MALE")
          female_data <- ihme_data %>% filter(gender == "FEMALE")
          
          # Create a base dataframe that will hold all life expectancy variables
          # Start with just geoid and year columns for all counties and years
          counties_years <- distinct(full_data, geoid, year)
          
          # Function to process data for a specific race/ethnicity
          process_race_data <- function(race_code, race_label) {
            log_message(paste("Processing", race_label, "life expectancy data"),
                        level = "INFO", show_console = TRUE)
            
            # Variables to create
            race_vars <- list()
            
            # Safely process data for each gender and handle potential memory/data issues
            tryCatch({
              # Process overall (both genders) data - ensure data exists and is properly formatted
              race_both <- tryCatch({
                df <- both_data %>% 
                  filter(race_ethnicity == race_code) %>%
                  select(geoid, year, life_expectancy, le_lower_ci, le_upper_ci)
                
                # Standardize geoid format
                df$geoid <- as.character(df$geoid)
                df$year <- as.integer(df$year)
                
                # Return the filtered data
                df
              }, error = function(e) {
                log_message(paste("Error processing 'both' gender data for", race_label, ":", conditionMessage(e)),
                           level = "ERROR", show_console = TRUE)
                # Return a minimal dataframe with required columns
                data.frame(geoid = character(0), year = integer(0), stringsAsFactors = FALSE)
              })
              
              # Define variable name based on race
              if (race_code == "all") {
                var_name <- "life_expectancy"
                lower_name <- "le_lower_ci"
                upper_name <- "le_upper_ci"
              } else {
                var_name <- paste0("life_expectancy_", race_code)
                lower_name <- paste0("le_", race_code, "_lower_ci")
                upper_name <- paste0("le_", race_code, "_upper_ci")
              }
              
              # Rename columns only if data exists
              if (nrow(race_both) > 0) {
                # Rename columns
                if ("life_expectancy" %in% names(race_both)) 
                  names(race_both)[names(race_both) == "life_expectancy"] <- var_name
                if ("le_lower_ci" %in% names(race_both)) 
                  names(race_both)[names(race_both) == "le_lower_ci"] <- lower_name
                if ("le_upper_ci" %in% names(race_both)) 
                  names(race_both)[names(race_both) == "le_upper_ci"] <- upper_name
              }
              
              # Process male data with error handling
              race_male <- tryCatch({
                df <- male_data %>% 
                  filter(race_ethnicity == race_code) %>%
                  select(geoid, year, life_expectancy, le_lower_ci, le_upper_ci)
                
                # Standardize geoid format
                df$geoid <- as.character(df$geoid)
                df$year <- as.integer(df$year)
                
                # Return the filtered data
                df
              }, error = function(e) {
                log_message(paste("Error processing male data for", race_label, ":", conditionMessage(e)),
                           level = "ERROR", show_console = TRUE)
                # Return a minimal dataframe with required columns
                data.frame(geoid = character(0), year = integer(0), stringsAsFactors = FALSE)
              })
              
              # Define male variable names
              if (race_code == "all") {
                male_var_name <- "life_expectancy_male"
                male_lower_name <- "le_male_lower_ci"
                male_upper_name <- "le_male_upper_ci"
              } else {
                male_var_name <- paste0("life_expectancy_male_", race_code)
                male_lower_name <- paste0("le_male_", race_code, "_lower_ci")
                male_upper_name <- paste0("le_male_", race_code, "_upper_ci")
              }
              
              # Rename male columns if data exists
              if (nrow(race_male) > 0) {
                if ("life_expectancy" %in% names(race_male)) 
                  names(race_male)[names(race_male) == "life_expectancy"] <- male_var_name
                if ("le_lower_ci" %in% names(race_male)) 
                  names(race_male)[names(race_male) == "le_lower_ci"] <- male_lower_name
                if ("le_upper_ci" %in% names(race_male)) 
                  names(race_male)[names(race_male) == "le_upper_ci"] <- male_upper_name
              }
              
              # Process female data with error handling
              race_female <- tryCatch({
                df <- female_data %>% 
                  filter(race_ethnicity == race_code) %>%
                  select(geoid, year, life_expectancy, le_lower_ci, le_upper_ci)
                
                # Standardize geoid format
                df$geoid <- as.character(df$geoid)
                df$year <- as.integer(df$year)
                
                # Return the filtered data
                df
              }, error = function(e) {
                log_message(paste("Error processing female data for", race_label, ":", conditionMessage(e)),
                           level = "ERROR", show_console = TRUE)
                # Return a minimal dataframe with required columns
                data.frame(geoid = character(0), year = integer(0), stringsAsFactors = FALSE)
              })
              
              # Define female variable names
              if (race_code == "all") {
                female_var_name <- "life_expectancy_female"
                female_lower_name <- "le_female_lower_ci"
                female_upper_name <- "le_female_upper_ci"
              } else {
                female_var_name <- paste0("life_expectancy_female_", race_code)
                female_lower_name <- paste0("le_female_", race_code, "_lower_ci")
                female_upper_name <- paste0("le_female_", race_code, "_upper_ci")
              }
              
              # Rename female columns if data exists
              if (nrow(race_female) > 0) {
                if ("life_expectancy" %in% names(race_female)) 
                  names(race_female)[names(race_female) == "life_expectancy"] <- female_var_name
                if ("le_lower_ci" %in% names(race_female)) 
                  names(race_female)[names(race_female) == "le_lower_ci"] <- female_lower_name
                if ("le_upper_ci" %in% names(race_female)) 
                  names(race_female)[names(race_female) == "le_upper_ci"] <- female_upper_name
              }
              
              # Use the global safe merge implementation for local usage
              safe_merge <- function(df1, df2, by_cols, all_arg = TRUE) {
                # Map parameters to the global function
                all.x <- all_arg
                all.y <- all_arg
                
                # Call the global function that implements the ultra-safe merge strategy
                global_safe_merge(df1, df2, by_cols = by_cols, all.x = all.x, all.y = all.y, sort = FALSE)
              }
              
              # Merge all data for this race safely
              result <- data.frame(geoid = character(0), year = integer(0), stringsAsFactors = FALSE)
              
              # First merge both and male data
              if (nrow(race_both) > 0 || nrow(race_male) > 0) {
                result <- safe_merge(race_both, race_male, by = c("geoid", "year"))
              }
              
              # Then merge with female data
              if (nrow(result) > 0 || nrow(race_female) > 0) {
                result <- safe_merge(result, race_female, by = c("geoid", "year"))
              }
              
              return(result)
              
            }, error = function(e) {
              log_message(paste("Error in process_race_data for", race_label, ":", conditionMessage(e)),
                         level = "ERROR", show_console = TRUE)
              # Return an empty dataframe with the right structure
              return(data.frame(geoid = character(0), year = integer(0), stringsAsFactors = FALSE))
            })
          }
          
          # Race/ethnicity mapping between IHME codes and our variable names
          race_mapping <- list(
            "all" = "all",           # Overall
            "hispanic" = "hispanic", # Hispanic
            "nhw" = "nhw",           # Non-Hispanic White
            "nhb" = "nhb",           # Non-Hispanic Black
            "nham" = "nhaian",       # Non-Hispanic American Indian/Alaska Native
            "nha" = "nhasian",       # Non-Hispanic Asian
            "nhpi" = "nhpi",         # Non-Hispanic Pacific Islander
            "oth" = "multirace"      # Other/multiracial
          )
          
          # First check if we'd be dealing with a very large dataset
          # Estimate the dataset size to decide on processing approach
          # Use object.size for more accurate estimation, falling back to rough calculation if not available
          if (requireNamespace("utils", quietly = TRUE)) {
            # More accurate size estimation
            try({
              ihme_data_size <- utils::object.size(ihme_data)
              estimated_size_gb <- as.numeric(ihme_data_size) / (1024^3)  # convert to GB
            }, silent = TRUE)
          }
          
          # If the try block failed, use rough estimate based on rows and columns
          if (!exists("estimated_size_gb")) {
            estimated_size_gb <- nrow(ihme_data) * ncol(ihme_data) * 8 / (1024^3)  # rough estimate in GB
          }
          
          log_message(paste("Estimated IHME dataset size:", round(estimated_size_gb, 2), "GB"),
                     level = "INFO", show_console = TRUE)
          
          # For large datasets > 2GB or very large raw table (>5M rows), use sequential processing
          force_sequential <- (estimated_size_gb > 2) || (nrow(ihme_data) > 5000000)
          
          # Install and load required parallel libraries if not already available
          if (!requireNamespace("future", quietly = TRUE)) {
            log_message("Installing 'future' package for parallel processing...", 
                       level = "INFO", show_console = TRUE)
            install.packages("future")
            library(future)
          }
          
          if (!requireNamespace("future.apply", quietly = TRUE)) {
            log_message("Installing 'future.apply' package for parallel processing...", 
                       level = "INFO", show_console = TRUE)
            install.packages("future.apply")
            library(future.apply)
          }
          
          if (!requireNamespace("progressr", quietly = TRUE)) {
            log_message("Installing 'progressr' package for progress tracking...", 
                       level = "INFO", show_console = TRUE)
            install.packages("progressr")
            library(progressr)
          }
          
          # Check if we can use parallel processing for race/ethnicity data
          can_use_parallel <- requireNamespace("future", quietly = TRUE) && 
                              requireNamespace("future.apply", quietly = TRUE) &&
                              !force_sequential
          
          if (can_use_parallel) {
            log_message("Using chunked parallel processing for race/ethnicity groups",
                       level = "INFO", show_console = TRUE)
            
            # Set up processing parameters
            race_codes <- names(race_mapping)
            race_labels <- unlist(race_mapping)
            
            # Set up parallel processing
            num_cores <- parallel::detectCores() - 1
            num_cores <- max(2, min(num_cores, 4)) # Limit to 4 cores to reduce memory pressure
            
            # Strategy based on OS
            strategy <- if (.Platform$OS.type == "windows") {
              "multisession"
            } else {
              "multicore"
            }
            
            log_message(paste("Using", strategy, "strategy with", num_cores, "cores for race processing"),
                       level = "INFO", show_console = TRUE)
            
            # Plan execution
            future::plan(strategy, workers = num_cores)
            
            # Set memory limit (4GB) - more conservative to avoid memory errors
            options(future.globals.maxSize = 4 * 1024^3)
            
            # Process races in smaller groups to manage memory
            chunk_size <- 1 # Process 1 race at a time to limit memory usage
            num_chunks <- ceiling(length(race_codes) / chunk_size)
            
            log_message(paste("Processing", length(race_codes), "race groups in", 
                             num_chunks, "chunks of", chunk_size, "race each"),
                       level = "INFO", show_console = TRUE)
            
            # Initialize the combined result
            combined_result <- data.frame(
              geoid = character(0),
              year = integer(0),
              stringsAsFactors = FALSE
            )
            
            # Process race chunks
            for (chunk_idx in 1:num_chunks) {
              # Calculate chunk indices
              start_idx <- (chunk_idx - 1) * chunk_size + 1
              end_idx <- min(chunk_idx * chunk_size, length(race_codes))
              chunk_codes <- race_codes[start_idx:end_idx]
              chunk_labels <- race_labels[start_idx:end_idx]
              
              log_message(paste("Processing chunk", chunk_idx, "of", num_chunks, 
                               "(races", start_idx, "to", end_idx, ")"),
                         level = "INFO", show_console = TRUE)
              
              # Try parallel processing but fall back to sequential if needed
              race_results <- NULL
              parallel_success <- FALSE
              
              tryCatch({
                # Try to process in parallel first
                # Use a more efficient approach with localized variables to reduce globals size
                all_args <- lapply(1:length(chunk_codes), function(i) {
                  list(
                    race_code = chunk_codes[i],
                    race_label = chunk_labels[i]
                  )
                })
                
                # Use a minimal function that only captures what it needs
                minimal_process_function <- function(args) {
                  race_code <- args$race_code
                  race_label <- args$race_label
                  
                  # Call the processing function
                  result <- process_race_data(race_code, race_label)
                  
                  # Return only necessary data
                  list(
                    race_code = race_code,
                    race_label = race_label,
                    result = result
                  )
                }
                
                # Wrap in future_lapply with minimal environment capture
                race_results <- future.apply::future_lapply(
                  all_args, 
                  minimal_process_function,
                  future.packages = c("dplyr", "tidyr")
                )
                parallel_success <- TRUE
              }, error = function(e) {
                log_message(paste("Parallel processing failed:", conditionMessage(e)),
                           level = "WARN", show_console = TRUE)
                log_message("Falling back to sequential processing for this chunk",
                           level = "WARN", show_console = TRUE)
              })
              
              # If parallel processing failed, try sequential
              if (!parallel_success) {
                # Process sequentially
                race_results <- list()
                for (i in 1:length(chunk_codes)) {
                  race_code <- chunk_codes[i]
                  race_label <- chunk_labels[i]
                  
                  log_message(paste("Processing", race_label, "sequentially"),
                             level = "INFO", show_console = TRUE)
                  
                  # Process this race
                  result <- process_race_data(race_code, race_label)
                  
                  # Add to results
                  race_results[[i]] <- list(
                    race_code = race_code,
                    race_label = race_label, 
                    result = result
                  )
                }
              }
              
              # Combine results from this chunk
              log_message("Combining results from current chunk...",
                         level = "INFO", show_console = TRUE)
              
              # Process each result
              for (race_result in race_results) {
                race_code <- race_result$race_code
                race_label <- race_result$race_label
                result_df <- race_result$result
                
                # Add data to combined result if we have rows
                if (!is.null(result_df) && nrow(result_df) > 0) {
                  log_message(paste("Adding", nrow(result_df), "rows of data for", race_label),
                             level = "INFO", show_console = TRUE)
                  
                  # Safely combine using global_safe_merge
                  if (nrow(combined_result) == 0) {
                    combined_result <- result_df
                  } else {
                    combined_result <- global_safe_merge(
                      combined_result, result_df, 
                      by_cols = c("geoid", "year"),
                      all.x = TRUE, all.y = TRUE,
                      use_parallel = TRUE,
                      batch_size = 2000
                    )
                  }
                } else {
                  log_message(paste("No data available for", race_label),
                             level = "WARN", show_console = TRUE)
                }
              }
              
              # Force garbage collection after each chunk
              log_message("Running garbage collection after chunk processing...",
                         level = "INFO", show_console = TRUE)
              gc(full = TRUE)
            }
          } else {
            # Fall back to sequential processing if parallel libraries aren't available
            log_message("Falling back to sequential processing for race/ethnicity data (parallel libraries not available)...",
                       level = "INFO", show_console = TRUE)
            
            # Set up processing parameters
            race_codes <- names(race_mapping)
            race_labels <- unlist(race_mapping)
            
            # Increase memory garbage collection frequency
            gc_limit <- 2  # Run garbage collection every 2 races
            
            # Process one race/ethnicity at a time to minimize memory usage
            log_message("Processing each race/ethnicity group sequentially",
                       level = "INFO", show_console = TRUE)
                     
            # Initialize with empty dataframe with correct structure
            combined_result <- data.frame(
              geoid = character(0),
              year = integer(0),
              stringsAsFactors = FALSE
            )
            
            # Process one race at a time and combine incrementally
            for (i in seq_along(race_codes)) {
              log_message(paste("Processing race/ethnicity group", i, "of", length(race_codes), ":", race_labels[i]),
                         level = "INFO", show_console = TRUE)
              
              # Process this race
              race_result <- process_race_data(race_codes[i], race_labels[i])
              
              # Add to combined result
              if (nrow(race_result) > 0) {
                log_message(paste("Adding", nrow(race_result), "rows of data for", race_labels[i]),
                           level = "INFO", show_console = TRUE)
                
                # Combine using global_safe_merge
                if (nrow(combined_result) == 0) {
                  combined_result <- race_result
                } else {
                  combined_result <- global_safe_merge(
                    combined_result, race_result, 
                    by_cols = c("geoid", "year"),
                    all.x = TRUE, all.y = TRUE,
                    use_parallel = FALSE
                  )
                }
              } else {
                log_message(paste("No data available for", race_labels[i]),
                           level = "WARN", show_console = TRUE)
              }
              
              # Clean up to free memory
              rm(race_result)
              
              # Run garbage collection periodically
              if (i %% gc_limit == 0) {
                log_message("Running garbage collection to free memory...",
                           level = "INFO", show_console = TRUE)
                gc()
              }
            }
          }
          
          # Final garbage collection
          gc()
          
          # Join with full_data (optimized chunked approach with parallel processing)
          log_message("Merging all IHME life expectancy variables with main dataset",
                     level = "INFO", show_console = TRUE)
          
          # Get unique counties
          unique_counties <- unique(full_data$geoid)
          county_count <- length(unique_counties)
          
          # Ensure required libraries are available
          if (!requireNamespace("future", quietly = TRUE)) {
            log_message("Installing 'future' package for parallel county processing...", 
                       level = "INFO", show_console = TRUE)
            install.packages("future")
            library(future)
          }
          
          if (!requireNamespace("future.apply", quietly = TRUE)) {
            log_message("Installing 'future.apply' package for parallel county processing...", 
                       level = "INFO", show_console = TRUE)
            install.packages("future.apply")
            library(future.apply)
          }
          
          if (!requireNamespace("progressr", quietly = TRUE)) {
            log_message("Installing 'progressr' package for progress tracking...", 
                       level = "INFO", show_console = TRUE)
            install.packages("progressr")
            library(progressr)
          }
          
          # Check if we can use parallel processing
          can_use_parallel <- requireNamespace("future", quietly = TRUE) && 
                              requireNamespace("future.apply", quietly = TRUE)
          
          # Determine chunk size (adaptive based on dataset size and processing mode)
          # For large datasets, use smaller chunks
          if (can_use_parallel) {
            # Adaptive chunk size based on data size
            if (estimated_size_gb > 1) {
              chunk_size <- 250  # Smaller chunks for very large datasets
              log_message("Using parallel processing with small chunk size for large county dataset",
                         level = "INFO", show_console = TRUE)
            } else {
              chunk_size <- 500  # Medium chunks for parallel with moderate data size
              log_message("Using parallel processing with medium chunk size for county merging",
                         level = "INFO", show_console = TRUE)
            }
          } else {
            # Sequential processing always uses smaller chunks
            chunk_size <- 200  # Smaller chunks for sequential mode
            log_message("Using sequential processing with small chunks for county merging",
                       level = "INFO", show_console = TRUE)
          }
          
          num_chunks <- ceiling(county_count / chunk_size)
          
          log_message(paste("Processing", county_count, "counties in", num_chunks, "chunks"),
                     level = "INFO", show_console = TRUE)
          
          # Create a result dataframe
          result_data <- full_data
          
          # Define a function to process a chunk of counties
          process_county_chunk <- function(chunk_idx) {
            # Determine counties for this chunk
            start_idx <- (chunk_idx - 1) * chunk_size + 1
            end_idx <- min(chunk_idx * chunk_size, county_count)
            chunk_counties <- unique_counties[start_idx:end_idx]
            
            # Filter data for this chunk
            chunk_data <- full_data[full_data$geoid %in% chunk_counties, ]
            chunk_result <- combined_result[combined_result$geoid %in% chunk_counties, ]
            
            # Merge chunk data safely
            if (nrow(chunk_result) > 0) {
              merged_chunk <- tryCatch({
                # Use the parallel-optimized global_safe_merge function
                global_safe_merge(
                  chunk_data, chunk_result, 
                  by_cols = c("geoid", "year"),
                  all.x = TRUE, all.y = TRUE,
                  use_parallel = can_use_parallel,
                  batch_size = if(can_use_parallel) 2000 else 1000
                )
              }, error = function(e) {
                # Log error but keep processing
                message(paste("Error merging chunk", chunk_idx, ":", conditionMessage(e)))
                # Return unmerged chunk as fallback
                chunk_data
              })
              
              # Return merged chunk with metadata
              return(list(
                chunk_idx = chunk_idx,
                counties = chunk_counties,
                merged_data = merged_chunk
              ))
            } else {
              # No IHME data for these counties
              return(list(
                chunk_idx = chunk_idx,
                counties = chunk_counties,
                merged_data = chunk_data
              ))
            }
          }
          
          # Process chunks (parallel or sequential)
          chunk_results <- NULL
          
          if (can_use_parallel) {
            # Set up parallel processing
            num_cores <- parallel::detectCores() - 1
            num_cores <- max(2, min(num_cores, 4)) # Limit to 4 cores for memory reasons
            
            # Strategy based on OS
            strategy <- if (.Platform$OS.type == "windows") {
              "multisession"
            } else {
              "multicore"
            }
            
            log_message(paste("Using", strategy, "strategy with", num_cores, "cores for county chunk processing"),
                       level = "INFO", show_console = TRUE)
            
            # Plan execution
            future::plan(strategy, workers = num_cores)
            
            # Set memory limit (4GB) - more conservative to avoid memory errors
            options(future.globals.maxSize = 4 * 1024^3)
            
            # Try parallel processing but fall back to sequential if needed
            chunk_results <- NULL
            parallel_success <- FALSE
            
            tryCatch({
              # Process in parallel with progress tracking if available
              if (requireNamespace("progressr", quietly = TRUE)) {
                # Setup progress handler
                progressr::handlers(progressr::handler_progress())
                
                # Process with progress - using minimal environment capture
                chunk_results <- progressr::with_progress({
                  p <- progressr::progressor(steps = num_chunks)
                  
                  # Create a minimal version of process_county_chunk that avoids capturing
                  # the entire environment, which can cause memory issues
                  minimal_chunk_process <- function(idx) {
                    # Process this chunk of counties
                    result <- process_county_chunk(idx)
                    # Update progress
                    p(message = paste("Processed county chunk", idx, "of", num_chunks))
                    # Return result
                    return(result)
                  }
                  
                  # Use future_lapply with explicit packages to minimize globals
                  future.apply::future_lapply(
                    1:num_chunks, 
                    minimal_chunk_process,
                    future.packages = c("dplyr", "tidyr")
                  )
                })
              } else {
                # Process without progress tracking - also using minimal environment capture
                # Create a minimal wrapper for better memory efficiency
                minimal_processor <- function(idx) {
                  process_county_chunk(idx)
                }
                
                # Use future_lapply with explicit packages
                chunk_results <- future.apply::future_lapply(
                  1:num_chunks, 
                  minimal_processor,
                  future.packages = c("dplyr", "tidyr")
                )
              }
              parallel_success <- TRUE
            }, error = function(e) {
              log_message(paste("Parallel processing failed:", conditionMessage(e)),
                         level = "WARN", show_console = TRUE)
              log_message("Falling back to sequential processing for county chunks",
                         level = "WARN", show_console = TRUE)
            })
            
            # If parallel processing failed, fall back to sequential
            if (!parallel_success) {
              log_message("Using sequential processing for county chunks after parallel failure",
                         level = "INFO", show_console = TRUE)
              
              chunk_results <- list()
              for (chunk_idx in 1:num_chunks) {
                log_message(paste("Processing chunk", chunk_idx, "of", num_chunks),
                           level = "INFO", show_console = TRUE)
                
                chunk_results[[chunk_idx]] <- process_county_chunk(chunk_idx)
                
                # Force garbage collection every 3 chunks
                if (chunk_idx %% 3 == 0) {
                  log_message("Running garbage collection...", level = "INFO", show_console = TRUE)
                  gc(full = TRUE)
                }
              }
            }
          } else {
            # Sequential processing from the start
            log_message("Using sequential processing for county chunks",
                       level = "INFO", show_console = TRUE)
            
            chunk_results <- list()
            for (chunk_idx in 1:num_chunks) {
              log_message(paste("Processing chunk", chunk_idx, "of", num_chunks),
                         level = "INFO", show_console = TRUE)
              
              chunk_results[[chunk_idx]] <- process_county_chunk(chunk_idx)
              
              # Force garbage collection every 3 chunks
              if (chunk_idx %% 3 == 0) {
                log_message("Running garbage collection...", level = "INFO", show_console = TRUE)
                gc(full = TRUE)
              }
            }
          }
          
          # Update the result dataframe with the processed chunks
          log_message("Updating main dataset with processed chunks...",
                     level = "INFO", show_console = TRUE)
          
          for (chunk_result in chunk_results) {
            chunk_counties <- chunk_result$counties
            merged_chunk <- chunk_result$merged_data
            
            # Update the result dataset
            result_idx <- which(result_data$geoid %in% chunk_counties)
            if (length(result_idx) > 0 && nrow(merged_chunk) == length(result_idx)) {
              result_data[result_idx, ] <- merged_chunk
            } else {
              log_message(paste("Warning: Row count mismatch in chunk", chunk_result$chunk_idx,
                               "- expected", length(result_idx), "got", nrow(merged_chunk)),
                         level = "WARN", show_console = TRUE)
            }
            
            # Clear merged chunk to free memory
            rm(merged_chunk)
          }
          
          # Force final garbage collection
          rm(chunk_results)
          gc(full = TRUE)
          
          full_data <- result_data
          rm(result_data)
          gc()
          
          # Add data quality flags for all IHME variables that are now in the dataset
          ihme_vars_in_data <- intersect(names(full_data), ihme_vars)
          for (var in ihme_vars_in_data) {
            quality_col <- paste0(var, "_data_quality")
            full_data[[quality_col]] <- "direct"
          }
          
          # Count how many IHME variables were successfully added
          num_ihme_vars_added <- length(ihme_vars_in_data)
          log_message(paste("Successfully added", num_ihme_vars_added, "IHME life expectancy variables to the dataset"),
                     level = "INFO", show_console = TRUE)
          
          log_message("Successfully added IHME life expectancy data to the dataset",
                     level = "INFO", show_console = TRUE)
          
          return(full_data)
        }
      } else {
        log_message("No IHME CSV files found in data/ihme/CSV",
                   level = "WARN", show_console = TRUE)
      }
    } else {
      log_message("IHME data directory not found at data/ihme/CSV",
                 level = "WARN", show_console = TRUE)
    }
    
    # NO SIMULATED DATA - If we couldn't load the actual IHME data, log an error
    log_message("ERROR: Could not load IHME life expectancy data files. These are required.",
               level = "ERROR", show_console = TRUE)
    log_message("Please ensure the IHME CSV files are present in the data/ihme/CSV directory.",
               level = "ERROR", show_console = TRUE)
    log_message("The pipeline requires actual data files - simulated data is not acceptable.",
               level = "ERROR", show_console = TRUE)
    
    # Set missing columns to NA with appropriate error flags
    for (var in ihme_vars) {
      if (!var %in% names(full_data)) {
        # Set to NA instead of simulated data
        full_data[[var]] <- NA
        
        # Add data quality flag
        quality_col <- paste0(var, "_data_quality")
        full_data[[quality_col]] <- "missing"
      }
    }
  }
  
  # Count available data by quality
  if (nrow(full_data) > 0) {
    # Count variables with direct, interpolated, and missing data
    qual_cols <- grep("_data_quality$", names(full_data), value = TRUE)
    
    if (length(qual_cols) > 0) {
      # Count number of variables by data quality
      quality_counts <- list(
        direct = 0,
        interpolated = 0,
        extrapolated = 0,
        missing = 0
      )
      
      for (col in qual_cols) {
        # Get variable name
        var_name <- sub("_data_quality$", "", col)
        
        # Count by quality type
        if (any(full_data[[col]] == "direct", na.rm = TRUE)) {
          quality_counts$direct <- quality_counts$direct + 1
        } else if (any(full_data[[col]] == "interpolated", na.rm = TRUE)) {
          quality_counts$interpolated <- quality_counts$interpolated + 1
        } else if (any(full_data[[col]] == "extrapolated", na.rm = TRUE)) {
          quality_counts$extrapolated <- quality_counts$extrapolated + 1
        } else if (any(full_data[[col]] == "missing", na.rm = TRUE) || 
                  all(is.na(full_data[[var_name]]))) {
          quality_counts$missing <- quality_counts$missing + 1
        }
      }
      
      log_message("Data quality summary:", level = "INFO", show_console = TRUE)
      log_message(paste(" - Direct data:", quality_counts$direct, "variables"),
                 level = "INFO", show_console = TRUE)
      log_message(paste(" - Interpolated data:", quality_counts$interpolated, "variables"),
                 level = "INFO", show_console = TRUE)
      log_message(paste(" - Extrapolated data:", quality_counts$extrapolated, "variables"),
                 level = "INFO", show_console = TRUE)
      log_message(paste(" - Missing data:", quality_counts$missing, "variables"),
                 level = "INFO", show_console = TRUE)
    }
  }
  
  log_message(paste("Processed data created with", nrow(full_data), "rows and", ncol(full_data), "columns"),
             level = "INFO", show_console = TRUE)
  
  # Count variables by domain
  domain_counts <- crosswalk %>%
    group_by(domain) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  
  log_message("Variable counts by domain:", level = "INFO", show_console = TRUE)
  for (i in 1:nrow(domain_counts)) {
    log_message(paste(" -", domain_counts$domain[i], ":", domain_counts$count[i]),
               level = "INFO", show_console = TRUE)
  }
  
  # Apply temporal interpolation if the function is available
  if (exists("interpolate_temporal_gaps") || file.exists("handle_year_county_variation.r")) {
    log_message("Applying temporal interpolation for missing year-county combinations...",
               level = "INFO", show_console = TRUE)
               
    # Source the interpolation function if needed
    if (!exists("interpolate_temporal_gaps") && file.exists("handle_year_county_variation.r")) {
      log_message("Loading temporal interpolation function...",
                 level = "INFO", show_console = TRUE)
      source("handle_year_county_variation.r")
    }
    
    if (exists("interpolate_temporal_gaps")) {
      # Get variable names from the crosswalk to interpolate
      # Exclude special variables and metadata
      vars_to_interpolate <- crosswalk$variable_name[
        !crosswalk$variable_name %in% c("geoid", "year", "name", "state_fips", "state_name")
      ]
      
      log_message(paste("Interpolating", length(vars_to_interpolate), "variables across counties and years..."),
                 level = "INFO", show_console = TRUE)
      
      # Apply the interpolation function
      full_data <- interpolate_temporal_gaps(
        data = full_data,
        variable_names = vars_to_interpolate,
        method = "linear",
        min_gap_size = 1,
        max_gap_size = 5
      )
      
      log_message("Temporal interpolation completed successfully.",
                 level = "INFO", show_console = TRUE)
    } else {
      log_message("Temporal interpolation function not available. Skipping interpolation.",
                 level = "WARN", show_console = TRUE)
    }
  }
  
  return(full_data)
}

# Only run if executed directly (not sourced)
if (!exists("is_sourced") || !is_sourced()) {
  message("Data fetching module cannot be run directly. Use the unified pipeline.")
}