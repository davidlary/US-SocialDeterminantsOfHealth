#!/usr/bin/env Rscript

# test_ihme_processing.r
# Test script to verify IHME data processing

# Load required libraries
library(yaml)
library(dplyr)
library(readr)

# Load configuration
source("pipeline_modules/module_core.r")
config <- load_config("config.yaml")

# Set up logging
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

# Test IHME data processing
log_message("Testing IHME data processing", level = "INFO")

# Define the IHME data directory
ihme_dir <- file.path(config$directories$data_dir, "ihme", "CSV")

# Check if the directory exists
if (!dir.exists(ihme_dir)) {
  log_message("ERROR: IHME data directory not found", level = "ERROR")
  quit(status = 1)
}

# Get IHME CSV files
ihme_files <- list.files(ihme_dir, pattern = "\\.CSV$", full.names = TRUE)

if (length(ihme_files) == 0) {
  log_message("ERROR: No IHME CSV files found", level = "ERROR")
  quit(status = 1)
}

log_message(paste("Found", length(ihme_files), "IHME data files"), level = "INFO")

# Create a dataframe to store all IHME life expectancy data
ihme_data <- NULL

# Process each file to detect format and contents
for (file in ihme_files[1:3]) { # Process first 3 files as a sample
  tryCatch({
    # Extract file information
    filename <- basename(file)
    log_message(paste("Processing file:", filename), level = "INFO")
    
    # Read the file headers to understand its format
    file_data <- read.csv(file, nrows = 1, stringsAsFactors = FALSE)
    
    # Display the column names
    log_message(paste("Column names:", paste(names(file_data), collapse = ", ")), level = "INFO")
    
    # Check if it's the old or new format
    if ("Location" %in% names(file_data) && "LE_both" %in% names(file_data)) {
      log_message("Detected legacy format with Location/LE_both columns", level = "INFO")
      # This is the old format
      file_data <- read.csv(file, stringsAsFactors = FALSE)
      
      # Show a sample of race columns
      race_cols <- grep("LE_race_", names(file_data), value = TRUE)
      log_message(paste("Race columns:", paste(race_cols, collapse = ", ")), level = "INFO")
      
      # Create a sample transformation
      sample_data <- file_data[1:5, ]
      log_message("Sample data from legacy format:", level = "INFO")
      print(head(sample_data, 3))
      
      # Demonstrate how we'd transform this format
      log_message("Transforming legacy format to standard format...", level = "INFO")
      
      # Create a long-format data frame for race-specific life expectancy
      transformed_data <- data.frame(
        geoid = rep(file_data$FIPS, 5),
        county_name = rep(file_data$Location, 5),
        year = rep(2019, nrow(file_data) * 5) # Assuming 2019 from filename
      )
      
      # Add overall life expectancy data
      overall_data <- data.frame(
        geoid = file_data$FIPS,
        county_name = file_data$Location,
        race_ethnicity = "all",
        gender = "BOTH",
        year = 2019,
        life_expectancy = file_data$LE_both,
        le_lower_ci = file_data$LE_both - file_data$SD_both,
        le_upper_ci = file_data$LE_both + file_data$SD_both
      )
      
      # Add male life expectancy data
      male_data <- data.frame(
        geoid = file_data$FIPS,
        county_name = file_data$Location,
        race_ethnicity = "all",
        gender = "MALE",
        year = 2019,
        life_expectancy = file_data$LE_male,
        le_lower_ci = file_data$LE_male - file_data$SD_male,
        le_upper_ci = file_data$LE_male + file_data$SD_male
      )
      
      # Add female life expectancy data
      female_data <- data.frame(
        geoid = file_data$FIPS,
        county_name = file_data$Location,
        race_ethnicity = "all",
        gender = "FEMALE",
        year = 2019,
        life_expectancy = file_data$LE_female,
        le_lower_ci = file_data$LE_female - file_data$SD_female,
        le_upper_ci = file_data$LE_female + file_data$SD_female
      )
      
      # Add race-specific data (if available)
      race_data_list <- list()
      
      # For each race_column, create a separate dataframe
      if ("LE_race_white" %in% names(file_data)) {
        white_data <- data.frame(
          geoid = file_data$FIPS,
          county_name = file_data$Location,
          race_ethnicity = "nhw",
          gender = "BOTH",
          year = 2019,
          life_expectancy = file_data$LE_race_white,
          # Confidence intervals not available in this format
          le_lower_ci = NA,
          le_upper_ci = NA
        )
        race_data_list[["white"]] <- white_data
      }
      
      if ("LE_race_black" %in% names(file_data)) {
        black_data <- data.frame(
          geoid = file_data$FIPS,
          county_name = file_data$Location,
          race_ethnicity = "nhb",
          gender = "BOTH",
          year = 2019,
          life_expectancy = file_data$LE_race_black,
          le_lower_ci = NA,
          le_upper_ci = NA
        )
        race_data_list[["black"]] <- black_data
      }
      
      if ("LE_race_hispanic" %in% names(file_data)) {
        hispanic_data <- data.frame(
          geoid = file_data$FIPS,
          county_name = file_data$Location,
          race_ethnicity = "hispanic",
          gender = "BOTH",
          year = 2019,
          life_expectancy = file_data$LE_race_hispanic,
          le_lower_ci = NA,
          le_upper_ci = NA
        )
        race_data_list[["hispanic"]] <- hispanic_data
      }
      
      if ("LE_race_asian" %in% names(file_data)) {
        asian_data <- data.frame(
          geoid = file_data$FIPS,
          county_name = file_data$Location,
          race_ethnicity = "nhasian",
          gender = "BOTH",
          year = 2019,
          life_expectancy = file_data$LE_race_asian,
          le_lower_ci = NA,
          le_upper_ci = NA
        )
        race_data_list[["asian"]] <- asian_data
      }
      
      # Combine all data
      combined_data <- rbind(
        overall_data,
        male_data,
        female_data
      )
      
      # Add race-specific data if available
      for (race_data in race_data_list) {
        combined_data <- rbind(combined_data, race_data)
      }
      
      # Show sample of transformed data
      log_message("Sample of transformed legacy data:", level = "INFO")
      print(head(combined_data, 3))
      
    } else if ("location_id" %in% names(file_data) && "race_name" %in% names(file_data)) {
      log_message("Detected standard format with location_id/race_name columns", level = "INFO")
      # This is the new format
      file_data <- read.csv(file, nrows = 100, stringsAsFactors = FALSE)
      
      # Show unique race_name values
      race_names <- unique(file_data$race_name)
      log_message(paste("Unique race_name values:", paste(race_names, collapse = ", ")), level = "INFO")
      
      # Parse the filename to extract year and gender
      year_match <- regexpr("_LT_(\\d{4})_", filename)
      gender_match <- regexpr("_(BOTH|MALE|FEMALE)_", filename)
      
      if (year_match > 0 && gender_match > 0) {
        year <- as.numeric(substr(filename, 
                                  year_match + 4, 
                                  year_match + 7))
        gender <- substr(filename, 
                      gender_match + 1, 
                      gender_match + nchar("BOTH") - 1)
        
        log_message(paste("Extracted year:", year, "and gender:", gender), level = "INFO")
        
        # Map race_name to our standard codes
        race_name_mapping <- list(
          "Total" = "all",
          "Latino" = "hispanic",
          "White" = "nhw",
          "Black" = "nhb",
          "Asian" = "nhasian",
          "AIAN" = "nhaian",
          "NHPI" = "nhpi",
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
        
        # Show race mappings
        for (race_name in race_names) {
          if (race_name %in% names(race_name_mapping)) {
            log_message(paste("Race mapping:", race_name, "->", race_name_mapping[[race_name]]), level = "INFO")
          } else {
            log_message(paste("Race not mapped:", race_name), level = "WARN")
          }
        }
        
        # Rename columns to match our schema
        file_data <- file_data %>%
          rename(
            geoid = location_id,
            county_name = location_name,
            life_expectancy = val,
            le_lower_ci = lower,
            le_upper_ci = upper
          )
        
        # Add year and gender
        file_data$year <- year
        file_data$gender <- gender
        
        # Show sample of transformed data
        log_message("Sample of transformed data:", level = "INFO")
        sample_cols <- c("geoid", "county_name", "race_ethnicity", "gender", "year", 
                         "life_expectancy", "le_lower_ci", "le_upper_ci")
        print(head(file_data[, sample_cols], 3))
      } else {
        log_message(paste("Couldn't extract year/gender from filename:", filename), level = "WARN")
      }
    } else {
      log_message(paste("Unknown file format for:", filename), level = "WARN")
      log_message(paste("Columns:", paste(names(file_data), collapse = ", ")), level = "INFO")
    }
  }, error = function(e) {
    log_message(paste("Error processing file", basename(file), ":", conditionMessage(e)),
               level = "ERROR")
  })
}

# Test race_name mapping logic
log_message("Testing race_name mapping logic:", level = "INFO")
test_race_names <- c("Total", "Latino", "White", "Black", "AIAN", "Asian", "NHPI", "Multiple races", "Other", "Unknown")
race_name_mapping <- list(
  "Total" = "all",
  "Latino" = "hispanic",
  "White" = "nhw",
  "Black" = "nhb",
  "Asian" = "nhasian",
  "AIAN" = "nhaian",
  "NHPI" = "nhpi",
  "Multiple races" = "multirace",
  "Other" = "multirace"
)

for (race_name in test_race_names) {
  if (race_name %in% names(race_name_mapping)) {
    log_message(paste("Race mapping:", race_name, "->", race_name_mapping[[race_name]]), level = "INFO")
  } else {
    log_message(paste("Race not mapped:", race_name, "-> default: all"), level = "WARN")
  }
}

log_message("IHME data processing test completed", level = "INFO")