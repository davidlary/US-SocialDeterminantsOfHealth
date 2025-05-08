#!/usr/bin/env Rscript

# generate_conus_maps.r
# This script generates maps for the Continental United States (CONUS)
# for each variable and year in the SDOH dataset.

# Function to check and install required packages
install_required_packages <- function(packages) {
  new_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
  if (length(new_packages) > 0) {
    cat("Installing required packages:", paste(new_packages, collapse = ", "), "\n")
    install.packages(new_packages)
  }
}

# List of required packages
required_packages <- c(
  "dplyr", "ggplot2", "sf", "DBI", "duckdb", 
  "tidyr", "readr", "stringr", "RColorBrewer", 
  "viridis", "gridExtra"
)

# Install any missing packages
install_required_packages(required_packages)

# Load required packages
library(dplyr)
library(ggplot2)
library(sf)
library(DBI)
library(duckdb)
library(tidyr)
library(readr)
library(stringr)
library(RColorBrewer)
library(viridis)

# Define is_sourced function if it doesn't exist
if (!exists("is_sourced")) {
  is_sourced <- function() {
    # Check if the calling environment is the global environment
    # If it's not, the function is being sourced
    parent_env <- parent.frame()
    return(!identical(parent_env, .GlobalEnv))
  }
}

#' Generate CONUS maps for all variables and years
#'
#' @param output_dir Directory to store output maps
#' @param db_path Path to the DuckDB database with SDOH data
#' @param shapefile_path Path to county shapefile
#' @param years Vector of years to generate maps for (NULL for all years)
#' @param variables Vector of variables to map (NULL for all variables)
#' @param conus_only Whether to limit maps to continental US (excluding AK, HI, territories)
#' @param parallel Whether to use parallel processing
#' @param cores Number of cores to use for parallel processing (default: 2)
#' @param overwrite Whether to overwrite existing map files
#' @return TRUE if successful, FALSE otherwise
generate_conus_maps <- function(output_dir = "output/maps",
                               db_path = "output/us_county_sdoh_unified.duckdb",
                               shapefile_path = NULL,
                               years = NULL,
                               variables = NULL, 
                               conus_only = TRUE,
                               parallel = FALSE,
                               cores = 2,
                               overwrite = FALSE) {
  
  cat("=================================================\n")
  cat("GENERATING CONUS MAPS FOR SDOH VARIABLES\n")
  cat("=================================================\n\n")
  
  # Step 1: Set up output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Created output directory:", output_dir, "\n")
  }
  
  # Check for subdirectories and create if needed
  year_dir <- file.path(output_dir, "by_year")
  variable_dir <- file.path(output_dir, "by_variable")
  combined_dir <- file.path(output_dir, "combined")
  
  for (dir in c(year_dir, variable_dir, combined_dir)) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      cat("Created directory:", dir, "\n")
    }
  }
  
  # Step 2: Connect to database
  cat("Connecting to database:", db_path, "...\n")
  
  # Try to find the database file if it doesn't exist
  if (!file.exists(db_path)) {
    potential_db_paths <- c(
      "us_county_sdoh_unified.duckdb",
      "output/us_county_sdoh_unified.duckdb",
      "us_county_sdoh_data.duckdb",
      "output/us_county_sdoh_data.duckdb"
    )
    
    for (potential_path in potential_db_paths) {
      if (file.exists(potential_path)) {
        cat("Database not found at", db_path, "but found at", potential_path, "\n")
        db_path <- potential_path
        break
      }
    }
  }
  
  tryCatch({
    con <- dbConnect(duckdb(), db_path)
    cat("Successfully connected to database\n")
  }, error = function(e) {
    cat("ERROR: Could not connect to database:", conditionMessage(e), "\n")
    cat("Please make sure the database file exists and the pipeline has been run.\n")
    return(FALSE)
  })
  
  # Check if database connection was successful
  if (!exists("con")) {
    return(FALSE)
  }
  
  # Step 3: Get available data years and variables 
  cat("Getting available years and variables...\n")
  tryCatch({
    # Handle two possible database structures:
    # 1. Wide format (old): Variables as columns (county_sdoh_data, county_time_series, etc.)
    # 2. Normalized format (new): Data in sdoh_data table with (geoid, year, variable_name, value) format
    
    data_table_found <- FALSE
    
    # Check first for the newer normalized structure (sdoh_data table)
    if (dbExistsTable(con, "sdoh_data") && dbExistsTable(con, "variables")) {
      cat("Found normalized database structure with sdoh_data table\n")
      
      # Get available years
      available_years <- dbGetQuery(con, "SELECT DISTINCT year FROM sdoh_data ORDER BY year")$year
      
      if (length(available_years) > 0) {
        cat("Found", length(available_years), "years in the database, from", 
            min(available_years), "to", max(available_years), "\n")
        
        # Get available variables from the variables table
        data_cols <- dbGetQuery(con, "SELECT variable_name FROM variables")$variable_name
        
        if (length(data_cols) > 0) {
          cat("Found", length(data_cols), "variables in the database\n")
          data_table_found <- TRUE
          
          # Use normalized table format for future queries
          use_normalized_format <- TRUE
        }
      }
    }
    
    # If normalized structure not found, try the older wide format tables
    if (!data_table_found) {
      tables_to_try <- c("county_sdoh_data", "county_time_series", "county_interpolated")
      
      for (table in tables_to_try) {
        if (dbExistsTable(con, table)) {
          cat("Found data in legacy table format:", table, "\n")
          
          # Get all available years from this table
          available_years <- dbGetQuery(con, sprintf("SELECT DISTINCT year FROM %s ORDER BY year", table))$year
          
          if (length(available_years) > 0) {
            cat("Found", length(available_years), "years in the database, from", 
                min(available_years), "to", max(available_years), "\n")
            
            # Get all available variables (excluding metadata and flags)
            colnames <- dbListFields(con, table)
            data_cols <- colnames[!grepl("_interpolated$|_extended$|GEOID|NAME|name|geoid|year|source|data_|interpolation_|extension_", colnames)]
            
            if (length(data_cols) > 0) {
              cat("Found", length(data_cols), "variables in the database\n")
              data_table_found <- TRUE
              
              # Use wide format for future queries
              use_normalized_format <- FALSE
              wide_format_table <- table
              break
            }
          }
        }
      }
    }
    
    if (!data_table_found) {
      cat("ERROR: No suitable data tables found in database.\n")
      
      # Check if tables exist but are empty
      all_tables <- dbListTables(con)
      cat("Available tables in database:", paste(all_tables, collapse=", "), "\n")
      
      # Check specifically for sdoh_data table
      if ("sdoh_data" %in% all_tables) {
        row_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM sdoh_data")[1,1]
        if (row_count == 0) {
          cat("The sdoh_data table exists but is empty. The database needs to be populated with data.\n")
          cat("Please run the full pipeline to ensure data is loaded into the database.\n")
        } else {
          # Check variable coverage
          var_count <- dbGetQuery(con, "SELECT COUNT(DISTINCT variable_name) as count FROM sdoh_data")[1,1]
          cat("The sdoh_data table has", row_count, "rows and", var_count, "variables.\n")
          
          # Print a sample of variable names for debugging
          sample_vars <- dbGetQuery(con, "SELECT DISTINCT variable_name FROM sdoh_data LIMIT 10")
          cat("Sample variables:", paste(sample_vars$variable_name, collapse=", "), "\n")
          
          # Check if specifically traffic safety variables exist
          if (var_count > 0) {
            traffic_vars <- dbGetQuery(con, "SELECT DISTINCT variable_name FROM sdoh_data WHERE variable_name LIKE '%traffic%' OR variable_name LIKE '%fatality%'")
            if (nrow(traffic_vars) > 0) {
              cat("Found", nrow(traffic_vars), "traffic safety related variables:", 
                  paste(traffic_vars$variable_name, collapse=", "), "\n")
            } else {
              cat("No traffic safety variables found in the database.\n")
            }
          }
        }
      } else if ("county_sdoh_data" %in% all_tables) {
        # Check the old format table
        cols <- dbListFields(con, "county_sdoh_data")
        col_count <- length(cols)
        traffic_cols <- grep("traffic|fatality", cols, value=TRUE)
        
        cat("The county_sdoh_data table exists with", col_count, "columns.\n")
        if (length(traffic_cols) > 0) {
          cat("Found", length(traffic_cols), "traffic safety related columns:", 
              paste(traffic_cols, collapse=", "), "\n")
        } else {
          cat("No traffic safety columns found in the county_sdoh_data table.\n")
        }
        
        # Check if the table has data
        row_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM county_sdoh_data")[1,1]
        if (row_count == 0) {
          cat("The county_sdoh_data table is empty. The database needs to be populated with data.\n")
        } else {
          cat("The county_sdoh_data table has", row_count, "rows.\n")
          
          # Try to diagnose why we can't find the data
          cat("Attempting to diagnose data access issues...\n")
          
          # Check for geoid and year columns
          if ("geoid" %in% cols || "GEOID" %in% cols) {
            geoid_col <- if ("GEOID" %in% cols) "GEOID" else "geoid"
            if ("year" %in% cols) {
              # Get year range
              year_range <- dbGetQuery(con, sprintf("SELECT MIN(year) as min_year, MAX(year) as max_year FROM county_sdoh_data"))
              cat("Year range in database:", year_range$min_year, "to", year_range$max_year, "\n")
              
              # Try to get some sample data
              sample_data <- dbGetQuery(con, sprintf("SELECT %s, year FROM county_sdoh_data LIMIT 5", geoid_col))
              cat("Sample data from county_sdoh_data:\n")
              print(sample_data)
            }
          }
        }
      } else {
        cat("Neither sdoh_data nor county_sdoh_data tables exist in the database.\n")
        cat("Please run the full pipeline to create and populate the database tables.\n")
      }
      
      dbDisconnect(con)
      return(FALSE)
    }
    
    # If specific years were requested, filter to those years
    if (!is.null(years) && length(years) > 0) {
      available_years <- intersect(years, available_years)
      if (length(available_years) == 0) {
        cat("ERROR: None of the requested years are available in the database.\n")
        dbDisconnect(con)
        return(FALSE)
      }
      cat("Filtered to", length(available_years), "requested years\n")
    }
    
    # If specific variables were requested, filter to those variables
    if (!is.null(variables) && length(variables) > 0) {
      data_cols <- intersect(variables, data_cols)
      if (length(data_cols) == 0) {
        cat("ERROR: None of the requested variables are available in the database.\n")
        dbDisconnect(con)
        return(FALSE)
      }
      cat("Filtered to", length(data_cols), "requested variables\n")
    }
  }, error = function(e) {
    cat("ERROR: Failed to get available years and variables:", conditionMessage(e), "\n")
    dbDisconnect(con)
    return(FALSE)
  })
  
  # Step 4: Load county shapefile
  cat("Loading county shapefile...\n")
  county_sf <- NULL
  
  # Try to find the shapefile if not provided
  if (is.null(shapefile_path)) {
    # First check for shapefile cache from utilities/fetch_county_shapefiles.r
    if (file.exists("data/shapefiles/counties_2020.rds")) {
      tryCatch({
        county_sf <- readRDS("data/shapefiles/counties_2020.rds")
        cat("Loaded 2020 county shapefile from RDS cache\n")
      }, error = function(e) {
        cat("Error loading shapefile from RDS cache:", conditionMessage(e), "\n")
      })
    }
    
    # If that didn't work, check common locations for county shapefile
    if (is.null(county_sf)) {
      potential_paths <- c(
        "data/shapefiles/counties.shp",
        "data/shapefiles/us_counties.shp",
        "data/shapefiles/county/counties.shp",
        "../data/shapefiles/counties.shp",
        "R/data/shapefiles/counties.shp"
      )
      
      for (path in potential_paths) {
        if (file.exists(path)) {
          shapefile_path <- path
          cat("Found county shapefile at:", path, "\n")
          break
        }
      }
    }
    
    # If still not found, try to download the shapefile using tigris
    if (is.null(county_sf) && is.null(shapefile_path)) {
      cat("County shapefile not found in common locations. Attempting to download using tigris...\n")
      tryCatch({
        # Check if tigris is installed
        if (!requireNamespace("tigris", quietly = TRUE)) {
          # Try to install tigris
          install.packages("tigris", repos = "https://cloud.r-project.org")
          library(tigris)
        } else {
          library(tigris)
        }
        
        # Download county shapefile
        options(tigris_use_cache = TRUE)
        county_sf <- tigris::counties(cb = TRUE, year = 2020)
        cat("Successfully downloaded county shapefile using tigris\n")
      }, error = function(e) {
        cat("ERROR: Failed to download county shapefile:", conditionMessage(e), "\n")
        cat("Please provide a valid shapefile path.\n")
        return(NULL)
      })
    }
  }
  
  # If we still don't have the county_sf object, try to load from the shapefile path
  if (is.null(county_sf) && !is.null(shapefile_path)) {
    tryCatch({
      county_sf <- sf::read_sf(shapefile_path)
      cat("Successfully loaded county shapefile from:", shapefile_path, "\n")
    }, error = function(e) {
      cat("ERROR: Failed to load county shapefile:", conditionMessage(e), "\n")
      cat("Please provide a valid shapefile path.\n")
      return(NULL)
    })
  }
  
  # Check if county_sf is still NULL
  if (is.null(county_sf)) {
    cat("ERROR: Could not load or download county shapefile.\n")
    dbDisconnect(con)
    return(FALSE)
  }
  
  # Ensure county_sf has a GEOID column for joining
  if (!"GEOID" %in% names(county_sf)) {
    # Try to create GEOID from other columns
    if (all(c("STATEFP", "COUNTYFP") %in% names(county_sf))) {
      county_sf$GEOID <- paste0(county_sf$STATEFP, county_sf$COUNTYFP)
      cat("Created GEOID column from STATEFP and COUNTYFP\n")
    } else if ("FIPS" %in% names(county_sf)) {
      county_sf$GEOID <- county_sf$FIPS
      cat("Using FIPS column as GEOID\n")
    } else if ("GEOID10" %in% names(county_sf)) {
      county_sf$GEOID <- county_sf$GEOID10
      cat("Using GEOID10 column as GEOID\n")
    } else if ("GEOID20" %in% names(county_sf)) {
      county_sf$GEOID <- county_sf$GEOID20
      cat("Using GEOID20 column as GEOID\n")
    } else {
      cat("ERROR: County shapefile does not have a GEOID column for joining.\n")
      dbDisconnect(con)
      return(FALSE)
    }
  }
  
  # Filter to CONUS if requested
  if (conus_only) {
    # FIPS codes for Alaska (02), Hawaii (15), Puerto Rico (72), and other territories
    non_conus_states <- c("02", "15", "72", "60", "66", "69", "78")
    
    # Extract state FIPS from GEOID (first 2 digits)
    if (nchar(county_sf$GEOID[1]) >= 2) {
      state_fips <- substr(county_sf$GEOID, 1, 2)
    } else if ("STATEFP" %in% names(county_sf)) {
      state_fips <- county_sf$STATEFP
    } else {
      cat("WARNING: Cannot determine state FIPS from GEOID. Using all counties.\n")
      state_fips <- rep("", nrow(county_sf))
    }
    
    # Filter to CONUS counties
    conus_counties <- !state_fips %in% non_conus_states
    county_sf <- county_sf[conus_counties, ]
    cat("Filtered to", nrow(county_sf), "counties in the Continental US (CONUS)\n")
  }
  
  # Step 5: Get variable metadata for better map titles and color schemes
  cat("Getting variable metadata...\n")
  variable_metadata <- NULL
  
  # Try all possible metadata sources in order of preference
  metadata_tried <- 0
  
  # 1. First try data_dictionary table in database
  if ("data_dictionary" %in% dbListTables(con)) {
    metadata_tried <- metadata_tried + 1
    tryCatch({
      # Get metadata from data_dictionary table
      variable_metadata <- dbGetQuery(con, "SELECT std_name, description, category FROM data_dictionary")
      cat("Found metadata for", nrow(variable_metadata), "variables in data_dictionary table\n")
    }, error = function(e) {
      cat("WARNING: Failed to get variable metadata from data_dictionary table:", conditionMessage(e), "\n")
      variable_metadata <- NULL
    })
  }
  
  # 2. Try extended_data_dictionary.csv in output directory
  if (is.null(variable_metadata)) {
    metadata_tried <- metadata_tried + 1
    extended_dict_file <- "output/extended_data_dictionary.csv"
    if (file.exists(extended_dict_file)) {
      tryCatch({
        variable_metadata <- read_csv(extended_dict_file, show_col_types = FALSE) %>%
          select(std_name = name, description, category)
        cat("Found metadata for", nrow(variable_metadata), "variables in extended_data_dictionary.csv\n")
      }, error = function(e) {
        cat("WARNING: Failed to get variable metadata from extended_data_dictionary.csv:", conditionMessage(e), "\n")
        variable_metadata <- NULL
      })
    }
  }
  
  # 3. Try variable_crosswalk_extended.csv
  if (is.null(variable_metadata)) {
    metadata_tried <- metadata_tried + 1
    crosswalk_files <- c(
      "variable_crosswalk_extended.csv",
      "output/variable_crosswalk_extended.csv"
    )
    
    for (file in crosswalk_files) {
      if (file.exists(file)) {
        tryCatch({
          crosswalk <- read_csv(file, show_col_types = FALSE)
          
          # Try to determine the column names
          name_col <- NULL
          desc_col <- NULL
          cat_col <- NULL
          
          # Check for various potential column names
          for (potential_name in c("std_name", "variable_name", "name", "variable")) {
            if (potential_name %in% names(crosswalk)) {
              name_col <- potential_name
              break
            }
          }
          
          for (potential_desc in c("description", "desc", "variable_description")) {
            if (potential_desc %in% names(crosswalk)) {
              desc_col <- potential_desc
              break
            }
          }
          
          for (potential_cat in c("category", "type", "domain", "variable_category")) {
            if (potential_cat %in% names(crosswalk)) {
              cat_col <- potential_cat
              break
            }
          }
          
          if (!is.null(name_col)) {
            if (!is.null(desc_col) && !is.null(cat_col)) {
              variable_metadata <- crosswalk %>%
                select(std_name = name_col, description = desc_col, category = cat_col)
            } else if (!is.null(desc_col)) {
              variable_metadata <- crosswalk %>%
                select(std_name = name_col, description = desc_col) %>%
                mutate(category = "Unknown")
            } else {
              variable_metadata <- crosswalk %>%
                select(std_name = name_col) %>%
                mutate(description = crosswalk[[name_col]], category = "Unknown")
            }
            
            cat("Found metadata for", nrow(variable_metadata), "variables in", file, "\n")
            break
          }
        }, error = function(e) {
          cat("WARNING: Failed to get variable metadata from", file, ":", conditionMessage(e), "\n")
        })
      }
      
      if (!is.null(variable_metadata)) {
        break
      }
    }
  }
  
  # 4. Create a minimal metadata table if still not available
  if (is.null(variable_metadata)) {
    variable_metadata <- data.frame(
      std_name = data_cols,
      description = gsub("_", " ", tools::toTitleCase(data_cols)),
      category = "Unknown"
    )
    cat("Created minimal metadata for", length(data_cols), "variables (no metadata source found after", metadata_tried, "attempts)\n")
  }
  
  # Step 6: Set up color schemes by variable category
  category_palettes <- list(
    "Demographics" = "YlOrBr",
    "Demographic" = "YlOrBr",
    "Race/Ethnicity" = "YlOrBr",
    "Economics" = "Greens",
    "Economic Factors" = "Greens",
    "Socioeconomic" = "Greens",
    "Economic" = "Greens",
    "Education" = "Blues",
    "Educational Resources & Quality" = "Blues",
    "Health Status" = "Reds",
    "Health Outcomes" = "Reds",
    "Healthcare Access" = "Purples",
    "Healthcare" = "Purples",
    "Health Access" = "Purples",
    "Housing" = "YlGnBu",
    "Environmental Health" = "BuGn",
    "Environmental" = "BuGn",
    "Food Environment" = "YlGn",
    "Food Environment & Access" = "YlGn",
    "Transportation" = "GnBu",
    "Traffic Safety" = "OrRd",
    "Social Cohesion" = "PuBu",
    "Social Cohesion & Capital" = "PuBu",
    "Social Factors" = "PuBu",
    "Social" = "PuBu",
    "Crime & Safety" = "RdPu",
    "Built Environment" = "BuPu",
    "Disability" = "PuRd",
    "Health Behaviors" = "YlOrRd"
  )
  
  # Default palette for unknown categories
  default_palette <- "viridis"
  
  # Step 7: Generate maps for each year and variable
  cat("\nGenerating maps for", length(available_years), "years and", length(data_cols), "variables...\n")
  cat("This will create", length(available_years) * length(data_cols), "maps\n")
  
  # Set up progress tracking
  total_maps <- length(available_years) * length(data_cols)
  maps_created <- 0
  start_time <- Sys.time()
  
  # Function to create a single map
  create_map <- function(year, variable) {
    # Generate filenames
    year_filename <- file.path(year_dir, sprintf("%d_%s.png", year, variable))
    variable_filename <- file.path(variable_dir, sprintf("%s_%d.png", variable, year))
    
    # Skip if files already exist and overwrite is FALSE
    if (!overwrite && file.exists(year_filename) && file.exists(variable_filename)) {
      return(c(year_filename, variable_filename))
    }
    
    # Get variable metadata
    var_meta <- variable_metadata %>%
      filter(std_name == variable)
    
    var_description <- if (nrow(var_meta) > 0 && !is.na(var_meta$description[1])) {
      var_meta$description[1]
    } else {
      gsub("_", " ", tools::toTitleCase(variable))
    }
    
    var_category <- if (nrow(var_meta) > 0 && !is.na(var_meta$category[1])) {
      var_meta$category[1]
    } else {
      "Unknown"
    }
    
    # Choose palette based on category
    palette_name <- if (var_category %in% names(category_palettes)) {
      category_palettes[[var_category]]
    } else {
      default_palette
    }
    
    # Fetch data for this year and variable - handling both database formats
    var_data <- NULL
    
    if (exists("use_normalized_format") && use_normalized_format) {
      # For normalized format (sdoh_data table)
      query <- sprintf("
        SELECT 
          c.geoid, 
          d.value as %s
        FROM counties c
        JOIN sdoh_data d ON c.geoid = d.geoid
        WHERE d.year = %d
        AND d.variable_name = '%s'
      ", variable, year, variable)
      
      var_data <- tryCatch({
        result <- dbGetQuery(con, query)
        
        # Rename the column to match the variable name
        names(result)[names(result) == "value"] <- variable
        
        # Add GEOID column for compatibility with shapefile join
        result$GEOID <- result$geoid
        
        result
      }, error = function(e) {
        cat(sprintf("ERROR: Failed to get data for %s in %d from normalized table: %s\n", 
                   variable, year, conditionMessage(e)))
        return(NULL)
      })
    } else {
      # For wide format (legacy tables)
      query_table <- ""
      for (table in tables_to_try) {
        if (dbExistsTable(con, table)) {
          # Check if this table has this variable
          cols <- dbListFields(con, table)
          if (variable %in% cols) {
            query_table <- table
            break
          }
        }
      }
      
      if (query_table == "") {
        cat(sprintf("WARNING: No table found containing variable %s\n", variable))
        return(NULL)
      }
      
      # Check if GEOID or geoid is used in this table
      cols <- dbListFields(con, query_table)
      geoid_col <- if ("GEOID" %in% cols) "GEOID" else "geoid"
      
      # Fetch data for this year and variable
      query <- sprintf("
        SELECT 
          %s, 
          %s
        FROM %s
        WHERE year = %d
      ", geoid_col, variable, query_table, year)
      
      var_data <- tryCatch({
        result <- dbGetQuery(con, query)
        
        # Add GEOID column for compatibility with shapefile join
        if (geoid_col == "geoid") {
          result$GEOID <- result$geoid
        }
        
        # Convert GEOID to character to match shapefile
        result$GEOID <- as.character(result$GEOID)
        
        result
      }, error = function(e) {
        cat(sprintf("ERROR: Failed to get data for %s in %d from wide table: %s\n", 
                   variable, year, conditionMessage(e)))
        return(NULL)
      })
    }
    
    if (is.null(var_data) || nrow(var_data) == 0) {
      cat(sprintf("WARNING: No data available for %s in %d\n", variable, year))
      return(NULL)
    }
    
    # Ensure GEOID is character for joining
    if ("GEOID" %in% names(county_sf)) {
      county_sf$GEOID <- as.character(county_sf$GEOID)
    }
    
    # Join data with shapefile
    map_data <- county_sf %>%
      left_join(var_data, by = "GEOID")
    
    # Handle NA values in the variable
    non_na_count <- sum(!is.na(map_data[[variable]]))
    total_count <- nrow(map_data)
    coverage_pct <- round(100 * non_na_count / total_count, 1)
    
    # Skip if less than 1% of counties have data
    if (coverage_pct < 1) {
      cat(sprintf("WARNING: Only %.1f%% coverage for %s in %d. Skipping map.\n", 
                 coverage_pct, variable, year))
      return(NULL)
    }
    
    # Determine if the variable is a percentage
    is_percentage <- grepl("pct|percent|rate", variable, ignore.case = TRUE) || 
                    grepl("percentage|rate|ratio", var_description, ignore.case = TRUE)
    
    # Set map title and legend title
    map_title <- sprintf("%s (%d)", var_description, year)
    legend_title <- if (is_percentage) {
      "Percent"
    } else if (grepl("count|number", variable, ignore.case = TRUE) || 
              grepl("count|number", var_description, ignore.case = TRUE)) {
      "Count"
    } else if (grepl("ratio", variable, ignore.case = TRUE) || 
              grepl("ratio", var_description, ignore.case = TRUE)) {
      "Ratio"
    } else if (grepl("index", variable, ignore.case = TRUE) || 
              grepl("index", var_description, ignore.case = TRUE)) {
      "Index"
    } else if (grepl("income|earning|dollar|money|cost", variable, ignore.case = TRUE) || 
              grepl("income|earning|dollar|money|cost", var_description, ignore.case = TRUE)) {
      "Dollars"
    } else {
      "Value"
    }
    
    # Set up the color scale based on the variable's properties
    if (palette_name == "viridis") {
      fill_scale <- scale_fill_viridis_c(
        name = legend_title,
        na.value = "grey90",
        option = "viridis"
      )
    } else {
      fill_scale <- scale_fill_distiller(
        name = legend_title,
        palette = palette_name,
        direction = 1,
        na.value = "grey90"
      )
    }
    
    # Check for interpolation column
    has_interpolation <- paste0(variable, "_interpolated") %in% names(map_data)
    
    if (has_interpolation) {
      # Create a column to indicate interpolated values
      map_data$data_type <- ifelse(
        is.na(map_data[[paste0(variable, "_interpolated")]]) | !map_data[[paste0(variable, "_interpolated")]],
        "Actual", "Interpolated"
      )
      
      # Create the map with interpolation indication
      p <- ggplot(map_data) +
        geom_sf(aes(fill = .data[[variable]], alpha = data_type), color = "white", size = 0.1) +
        fill_scale +
        scale_alpha_manual(values = c("Actual" = 1, "Interpolated" = 0.5), name = "Data Type") +
        labs(
          title = map_title,
          subtitle = sprintf("Data available for %d of %d counties (%.1f%%)", 
                          non_na_count, total_count, coverage_pct),
          caption = sprintf("Source: US Social Determinants of Health Dataset %d", year)
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 10),
          plot.caption = element_text(size = 8),
          legend.position = "bottom",
          legend.box = "vertical",
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()
        )
    } else {
      # Create a simpler map without interpolation indication
      p <- ggplot(map_data) +
        geom_sf(aes(fill = .data[[variable]]), color = "white", size = 0.1) +
        fill_scale +
        labs(
          title = map_title,
          subtitle = sprintf("Data available for %d of %d counties (%.1f%%)", 
                          non_na_count, total_count, coverage_pct),
          caption = sprintf("Source: US Social Determinants of Health Dataset %d", year)
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 10),
          plot.caption = element_text(size = 8),
          legend.position = "bottom",
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()
        )
    }
    
    # Save the maps
    ggsave(year_filename, p, width = 10, height = 7, dpi = 150)
    ggsave(variable_filename, p, width = 10, height = 7, dpi = 150)
    
    return(c(year_filename, variable_filename))
  }
  
  # Process maps in parallel or sequentially
  if (parallel && requireNamespace("parallel", quietly = TRUE)) {
    cat("Using parallel processing with", cores, "cores...\n")
    
    # Create a cluster
    cl <- parallel::makeCluster(cores)
    
    # First, verify all required objects exist
    required_objects <- c("county_sf", "variable_metadata", "category_palettes", "default_palette", 
                         "year_dir", "variable_dir", "combined_dir", "overwrite", 
                         "tables_to_try", "db_path")
    
    # Check for any missing objects
    missing_objects <- required_objects[!sapply(required_objects, exists)]
    if (length(missing_objects) > 0) {
      cat("ERROR: The following required objects are missing:", 
          paste(missing_objects, collapse=", "), "\n")
      parallel::stopCluster(cl)
      return(FALSE)
    }
    
    # Create a function to load the shapefile (to prevent 'county_sf' not found error)
    if (is.null(county_sf)) {
      cat("ERROR: county_sf is NULL. Cannot proceed with map generation.\n")
      parallel::stopCluster(cl)
      return(FALSE)
    }
    
    # Save county_sf to a temporary file for workers to load
    temp_shapefile <- tempfile(fileext = ".rds")
    saveRDS(county_sf, temp_shapefile)
    cat("Saved shapefile to temporary file for worker processes:", temp_shapefile, "\n")
    
    # Export necessary variables to the cluster
    parallel::clusterExport(cl, c(
      "variable_metadata", "category_palettes", "default_palette",
      "year_dir", "variable_dir", "combined_dir", "overwrite", "tables_to_try",
      "db_path", "temp_shapefile"
    ))
    
    # Export necessary functions and load libraries in each worker
    parallel::clusterEvalQ(cl, {
      library(dplyr)
      library(ggplot2)
      library(sf)
      library(tidyr)
      library(stringr)
      library(DBI)
      library(duckdb)
      
      # Create database connection for each worker
      con <- dbConnect(duckdb(), db_path)
      
      # Load shapefile from temporary file
      county_sf <- readRDS(temp_shapefile)
    })
    
    # Generate map task combinations
    map_tasks <- expand.grid(year = available_years, variable = data_cols, stringsAsFactors = FALSE)
    
    # Process maps in parallel
    results <- parallel::parLapply(cl, seq_len(nrow(map_tasks)), function(i) {
      year <- map_tasks$year[i]
      variable <- map_tasks$variable[i]
      create_map(year, variable)
    })
    
    # Close connections and stop cluster
    parallel::clusterEvalQ(cl, {
      dbDisconnect(con)
    })
    parallel::stopCluster(cl)
    
    # Clean up temporary file
    if (file.exists(temp_shapefile)) {
      file.remove(temp_shapefile)
    }
    
    # Count successful maps
    maps_created <- sum(!sapply(results, is.null))
    
  } else {
    # Process maps sequentially
    results <- list()
    for (year in available_years) {
      for (variable in data_cols) {
        maps_created <- maps_created + 1
        
        # Show progress
        if (maps_created %% 10 == 0 || maps_created == total_maps) {
          pct_complete <- 100 * maps_created / total_maps
          elapsed <- difftime(Sys.time(), start_time, units = "mins")
          estimated_total <- elapsed * total_maps / maps_created
          estimated_remaining <- estimated_total - elapsed
          
          cat(sprintf("\rProgress: %.1f%% complete (%d/%d maps). Est. time remaining: %.1f minutes.      ", 
                     pct_complete, maps_created, total_maps, as.numeric(estimated_remaining)))
          flush.console()
        }
        
        # Create the map
        result <- create_map(year, variable)
        if (!is.null(result)) {
          results[[length(results) + 1]] <- result
        }
      }
    }
    cat("\n")  # New line after progress updates
  }
  
  # Step 8: Generate yearly combined maps (one image with many variables for each year)
  cat("\nGenerating combined yearly maps...\n")
  for (year in available_years) {
    # Pick 9 important variables from different categories to show on the combined map
    important_vars <- c(
      "total_population", "median_household_income", "poverty_rate", 
      "unemployment_rate", "median_age", "bachelors_or_higher_pct", 
      "uninsured_pct", "obesity_pct", "air_pollution_pm25"
    )
    
    # Filter to variables that actually exist in the data
    available_important_vars <- intersect(important_vars, data_cols)
    
    # If we have less than 4 important variables, just pick the first 9 available
    if (length(available_important_vars) < 4) {
      available_important_vars <- head(data_cols, 9)
    }
    
    # Limit to at most 9 variables for the grid
    display_vars <- head(available_important_vars, 9)
    
    if (length(display_vars) == 0) {
      cat(sprintf("WARNING: No variables available for combined map in %d\n", year))
      next
    }
    
    # Create a combined map filename
    combined_filename <- file.path(combined_dir, sprintf("combined_%d.png", year))
    
    # Skip if file already exists and overwrite is FALSE
    if (!overwrite && file.exists(combined_filename)) {
      next
    }
    
    # Fetch data for all variables in this year - support both database formats
    combined_data <- NULL
    
    if (exists("use_normalized_format") && use_normalized_format) {
      # For normalized format (sdoh_data table)
      # We'll get the data for each variable separately and then join them together
      
      # Start with the county IDs
      combined_data <- dbGetQuery(con, "SELECT geoid FROM counties")
      combined_data$GEOID <- combined_data$geoid  # Add uppercase version for joining
      
      # For each variable, get the data and add it as a column
      for (variable in display_vars) {
        query <- sprintf("
          SELECT 
            c.geoid, 
            d.value as %s
          FROM counties c
          LEFT JOIN sdoh_data d ON c.geoid = d.geoid AND d.year = %d AND d.variable_name = '%s'
        ", variable, year, variable)
        
        var_data <- tryCatch({
          result <- dbGetQuery(con, query)
          # Rename the column to match the variable name if needed
          if ("value" %in% names(result)) {
            names(result)[names(result) == "value"] <- variable
          }
          result
        }, error = function(e) {
          cat(sprintf("ERROR: Failed to get data for %s in %d from normalized table: %s\n", 
                     variable, year, conditionMessage(e)))
          return(NULL)
        })
        
        if (!is.null(var_data) && nrow(var_data) > 0) {
          # Join with the existing data
          combined_data <- combined_data %>%
            left_join(var_data %>% select(geoid, !!sym(variable)), by = "geoid")
        }
      }
    } else {
      # For wide format (legacy tables)
      query_table <- tables_to_try[1]  # Default to first table
      for (table in tables_to_try) {
        if (dbExistsTable(con, table)) {
          # Check if table has year column
          year_count <- dbGetQuery(con, sprintf("SELECT COUNT(*) FROM %s WHERE year = %d", table, year))
          if (year_count[1,1] > 0) {
            query_table <- table
            break
          }
        }
      }
      
      # Check if GEOID or geoid is used in this table
      cols <- dbListFields(con, query_table)
      geoid_col <- if ("GEOID" %in% cols) "GEOID" else "geoid"
      
      # Fetch data for all variables in this year
      query <- sprintf("
        SELECT 
          %s, 
          %s
        FROM %s
        WHERE year = %d
      ", geoid_col, paste(display_vars, collapse = ", "), query_table, year)
      
      combined_data <- tryCatch({
        result <- dbGetQuery(con, query)
        
        # Add GEOID column for compatibility with shapefile join
        if (geoid_col == "geoid") {
          result$GEOID <- result$geoid
        }
        
        # Convert GEOID to character for joining
        result$GEOID <- as.character(result$GEOID)
        
        result
      }, error = function(e) {
        cat(sprintf("ERROR: Failed to get combined data for %d from wide table: %s\n", 
                   year, conditionMessage(e)))
        return(NULL)
      })
    }
    
    if (is.null(combined_data) || nrow(combined_data) == 0) {
      cat(sprintf("WARNING: No data available for combined map in %d\n", year))
      next
    }
    
    # Join data with shapefile
    map_data <- county_sf %>%
      left_join(combined_data, by = "GEOID")
    
    # Create a list to hold individual plots
    plots <- list()
    
    # Generate a plot for each variable
    for (i in seq_along(display_vars)) {
      variable <- display_vars[i]
      
      # Get variable metadata
      var_meta <- variable_metadata %>%
        filter(std_name == variable)
      
      var_description <- if (nrow(var_meta) > 0 && !is.na(var_meta$description[1])) {
        var_meta$description[1]
      } else {
        gsub("_", " ", tools::toTitleCase(variable))
      }
      
      var_category <- if (nrow(var_meta) > 0 && !is.na(var_meta$category[1])) {
        var_meta$category[1]
      } else {
        "Unknown"
      }
      
      # Choose palette based on category
      palette_name <- if (var_category %in% names(category_palettes)) {
        category_palettes[[var_category]]
      } else {
        default_palette
      }
      
      # Set up the color scale based on the variable's properties
      if (palette_name == "viridis") {
        fill_scale <- scale_fill_viridis_c(
          name = variable,
          na.value = "grey90",
          option = "viridis"
        )
      } else {
        fill_scale <- scale_fill_distiller(
          name = variable,
          palette = palette_name,
          direction = 1,
          na.value = "grey90"
        )
      }
      
      # Create the small map
      p <- ggplot(map_data) +
        geom_sf(aes(fill = .data[[variable]]), color = NA) +
        fill_scale +
        labs(title = var_description) +
        theme_void() +
        theme(
          plot.title = element_text(size = 8),
          legend.position = "none"
        )
      
      plots[[i]] <- p
    }
    
    # Calculate the grid dimensions
    n_plots <- length(plots)
    n_rows <- ceiling(sqrt(n_plots))
    n_cols <- ceiling(n_plots / n_rows)
    
    # Set up the combined plot layout
    if (requireNamespace("gridExtra", quietly = TRUE)) {
      library(gridExtra)
      
      # Add empty plots if needed to fill the grid
      while (length(plots) < n_rows * n_cols) {
        plots[[length(plots) + 1]] <- ggplot() + theme_void()
      }
      
      # Create the combined plot
      combined_plot <- gridExtra::arrangeGrob(
        grobs = plots,
        ncol = n_cols,
        top = grid::textGrob(
          sprintf("US Social Determinants of Health Overview - %d", year),
          gp = grid::gpar(fontsize = 16, fontface = "bold")
        )
      )
      
      # Save the combined plot
      ggsave(combined_filename, combined_plot, width = 15, height = 12, dpi = 150)
      cat(sprintf("Created combined map for %d with %d variables\n", year, n_plots))
    } else {
      cat("WARNING: gridExtra package not available. Cannot create combined map.\n")
    }
  }
  
  # Step 9: Create time series maps for each variable (showing changes over time)
  cat("\nGenerating time series maps for each variable...\n")
  for (variable in data_cols) {
    # Pick a subset of years to display (first, middle, and last)
    if (length(available_years) <= 6) {
      display_years <- available_years
    } else {
      step <- floor(length(available_years) / 5)
      display_years <- available_years[seq(1, length(available_years), by = step)]
      
      # Always include the first and last year
      if (!display_years[1] == available_years[1]) {
        display_years <- c(available_years[1], display_years)
      }
      if (!display_years[length(display_years)] == available_years[length(available_years)]) {
        display_years <- c(display_years, available_years[length(available_years)])
      }
      
      # Limit to 6 years
      display_years <- head(display_years, 6)
    }
    
    # Create a time series map filename
    timeseries_filename <- file.path(combined_dir, sprintf("timeseries_%s.png", variable))
    
    # Skip if file already exists and overwrite is FALSE
    if (!overwrite && file.exists(timeseries_filename)) {
      next
    }
    
    # Fetch data for all years for this variable - handle both database formats
    timeseries_data <- NULL
    
    if (exists("use_normalized_format") && use_normalized_format) {
      # For normalized format (sdoh_data table)
      query <- sprintf("
        SELECT 
          c.geoid, 
          d.year,
          d.value as %s
        FROM counties c
        JOIN sdoh_data d ON c.geoid = d.geoid
        WHERE d.year IN (%s)
        AND d.variable_name = '%s'
      ", variable, paste(display_years, collapse = ", "), variable)
      
      timeseries_data <- tryCatch({
        result <- dbGetQuery(con, query)
        
        # Rename the column to match the variable name
        names(result)[names(result) == "value"] <- variable
        
        # Add GEOID column for compatibility with shapefile join
        result$GEOID <- result$geoid
        
        result
      }, error = function(e) {
        cat(sprintf("ERROR: Failed to get time series data for %s from normalized table: %s\n", 
                   variable, conditionMessage(e)))
        return(NULL)
      })
    } else {
      # For wide format (legacy tables)
      query_table <- tables_to_try[1]  # Default to first table
      for (table in tables_to_try) {
        if (dbExistsTable(con, table)) {
          # Check if table has this variable
          cols <- dbListFields(con, table)
          if (variable %in% cols) {
            query_table <- table
            break
          }
        }
      }
      
      # Check if GEOID or geoid is used in this table
      cols <- dbListFields(con, query_table)
      geoid_col <- if ("GEOID" %in% cols) "GEOID" else "geoid"
      
      # Fetch data for all years for this variable
      query <- sprintf("
        SELECT 
          %s, 
          year,
          %s
        FROM %s
        WHERE year IN (%s)
      ", geoid_col, variable, query_table, paste(display_years, collapse = ", "))
      
      timeseries_data <- tryCatch({
        result <- dbGetQuery(con, query)
        
        # Add GEOID column for compatibility with shapefile join
        if (geoid_col == "geoid") {
          result$GEOID <- result$geoid
        }
        
        # Convert GEOID to character for joining
        result$GEOID <- as.character(result$GEOID)
        
        result
      }, error = function(e) {
        cat(sprintf("ERROR: Failed to get time series data for %s from wide table: %s\n", 
                   variable, conditionMessage(e)))
        return(NULL)
      })
    }
    
    if (is.null(timeseries_data) || nrow(timeseries_data) == 0) {
      cat(sprintf("WARNING: No time series data available for %s\n", variable))
      next
    }
    
    # Get variable metadata
    var_meta <- variable_metadata %>%
      filter(std_name == variable)
    
    var_description <- if (nrow(var_meta) > 0 && !is.na(var_meta$description[1])) {
      var_meta$description[1]
    } else {
      gsub("_", " ", tools::toTitleCase(variable))
    }
    
    var_category <- if (nrow(var_meta) > 0 && !is.na(var_meta$category[1])) {
      var_meta$category[1]
    } else {
      "Unknown"
    }
    
    # Choose palette based on category
    palette_name <- if (var_category %in% names(category_palettes)) {
      category_palettes[[var_category]]
    } else {
      default_palette
    }
    
    # Create a list to hold individual plots
    plots <- list()
    
    # Generate a plot for each year
    for (i in seq_along(display_years)) {
      year <- display_years[i]
      
      # Filter data for this year
      year_data <- timeseries_data %>%
        filter(year == !!year)
      
      # Join data with shapefile
      map_data <- county_sf %>%
        left_join(year_data, by = "GEOID")
      
      # Handle NA values in the variable
      non_na_count <- sum(!is.na(map_data[[variable]]))
      total_count <- nrow(map_data)
      coverage_pct <- round(100 * non_na_count / total_count, 1)
      
      # Set up the color scale based on the variable's properties
      if (palette_name == "viridis") {
        fill_scale <- scale_fill_viridis_c(
          name = variable,
          na.value = "grey90",
          option = "viridis"
        )
      } else {
        fill_scale <- scale_fill_distiller(
          name = variable,
          palette = palette_name,
          direction = 1,
          na.value = "grey90"
        )
      }
      
      # Create the small map
      p <- ggplot(map_data) +
        geom_sf(aes(fill = .data[[variable]]), color = NA) +
        fill_scale +
        labs(title = as.character(year)) +
        theme_void() +
        theme(
          plot.title = element_text(size = 10),
          legend.position = "none"
        )
      
      plots[[i]] <- p
    }
    
    # Calculate the grid dimensions
    n_plots <- length(plots)
    n_rows <- 2  # Fix to 2 rows
    n_cols <- ceiling(n_plots / n_rows)
    
    # Set up the combined plot layout
    if (requireNamespace("gridExtra", quietly = TRUE)) {
      library(gridExtra)
      
      # Add empty plots if needed to fill the grid
      while (length(plots) < n_rows * n_cols) {
        plots[[length(plots) + 1]] <- ggplot() + theme_void()
      }
      
      # Create the combined plot
      timeseries_plot <- gridExtra::arrangeGrob(
        grobs = plots,
        ncol = n_cols,
        top = grid::textGrob(
          sprintf("%s Over Time", var_description),
          gp = grid::gpar(fontsize = 16, fontface = "bold")
        )
      )
      
      # Save the combined plot
      ggsave(timeseries_filename, timeseries_plot, width = 15, height = 8, dpi = 150)
      cat(sprintf("Created time series map for %s with %d years\n", variable, n_plots))
    } else {
      cat("WARNING: gridExtra package not available. Cannot create time series map.\n")
    }
  }
  
  # Step 10: Generate a README file for the maps
  readme_content <- "# US Social Determinants of Health Maps\n\n"
  readme_content <- paste0(readme_content, "This directory contains maps for the US Social Determinants of Health dataset.\n\n")
  
  readme_content <- paste0(readme_content, "## Map Organization\n\n")
  readme_content <- paste0(readme_content, "- **by_year/**: Maps organized by year, with filenames like `YEAR_VARIABLE.png`\n")
  readme_content <- paste0(readme_content, "- **by_variable/**: Maps organized by variable, with filenames like `VARIABLE_YEAR.png`\n")
  readme_content <- paste0(readme_content, "- **combined/**: Combined maps with multiple variables for each year (`combined_YEAR.png`) and time series maps for each variable (`timeseries_VARIABLE.png`)\n\n")
  
  readme_content <- paste0(readme_content, "## Map Coverage\n\n")
  readme_content <- paste0(readme_content, sprintf("- **Time Period**: %d to %d\n", min(available_years), max(available_years)))
  readme_content <- paste0(readme_content, sprintf("- **Variables**: %d variables mapped across various domains\n", length(data_cols)))
  readme_content <- paste0(readme_content, sprintf("- **Geography**: Maps show the Continental United States (CONUS), excluding Alaska, Hawaii, and territories\n\n"))
  
  readme_content <- paste0(readme_content, "## Variable Domains\n\n")
  
  # Add variable counts by category
  if (!is.null(variable_metadata) && "category" %in% names(variable_metadata)) {
    category_table <- variable_metadata %>%
      filter(std_name %in% data_cols) %>%
      group_by(category) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
    readme_content <- paste0(readme_content, "| Domain | Variables |\n|--------|----------|\n")
    for (i in 1:nrow(category_table)) {
      readme_content <- paste0(readme_content, 
                              sprintf("| %s | %d |\n", 
                                     category_table$category[i], 
                                     category_table$count[i]))
    }
  }
  
  # Write the README file
  writeLines(readme_content, file.path(output_dir, "README.md"))
  cat("Created README file for maps\n")
  
  # Step 11: Close database connection
  dbDisconnect(con)
  
  # Step 12: Final report
  cat("\n=================================================\n")
  cat("MAP GENERATION COMPLETE\n")
  cat("=================================================\n\n")
  
  cat("Successfully generated maps for the US Social Determinants of Health dataset.\n")
  cat("Maps are organized in the following directories:\n")
  cat("- ", year_dir, "\n")
  cat("- ", variable_dir, "\n")
  cat("- ", combined_dir, "\n\n")
  
  elapsed <- difftime(Sys.time(), start_time, units = "mins")
  cat(sprintf("Time taken: %.1f minutes\n", as.numeric(elapsed)))
  cat(sprintf("Average time per map: %.2f seconds\n", as.numeric(elapsed) * 60 / maps_created))
  cat(sprintf("Total maps created: %d\n", maps_created))
  
  return(TRUE)
}

# Add this map generation step to the pipeline
integrate_map_generation <- function() {
  cat("Integrating map generation into the SDOH pipeline...\n")
  
  # Find the unified pipeline script
  pipeline_file <- "unified_sdoh_pipeline.r"
  if (!file.exists(pipeline_file)) {
    alt_locations <- c(
      "R/unified_sdoh_pipeline.r",
      "../unified_sdoh_pipeline.r"
    )
    
    for (loc in alt_locations) {
      if (file.exists(loc)) {
        pipeline_file <- loc
        cat("Found pipeline file at:", loc, "\n")
        break
      }
    }
  }
  
  if (!file.exists(pipeline_file)) {
    cat("ERROR: Could not find unified_sdoh_pipeline.r\n")
    return(FALSE)
  }
  
  # Create a backup of the pipeline file
  backup_file <- paste0(pipeline_file, ".bak")
  file.copy(pipeline_file, backup_file, overwrite = TRUE)
  cat("Created backup of pipeline at:", backup_file, "\n")
  
  # Read the pipeline file
  pipeline_content <- readLines(pipeline_file)
  
  # Look for the end of the pipeline where we should add map generation
  end_pipeline <- grep("cat\\(\"Pipeline completed successfully", pipeline_content)
  
  if (length(end_pipeline) == 0) {
    end_pipeline <- grep("Pipeline\\s*completed|completed\\s*successfully", pipeline_content)
  }
  
  if (length(end_pipeline) == 0) {
    # Try to find a good insertion point near the end of the file
    end_pipeline <- length(pipeline_content) - 10
    cat("WARNING: Could not find specific pipeline completion message. Adding map generation to near the end of the file.\n")
  } else {
    cat("Found pipeline completion at line", end_pipeline[1], "\n")
  }
  
  # Create the map generation code to insert
  map_code <- c(
    "",
    "# ---- Step: Generate CONUS Maps ----",
    "log_message(\"STEP: GENERATING CONUS MAPS FOR ALL VARIABLES\", ",
    "            level = \"INFO\", show_console = TRUE)",
    "",
    "# Source the map generation script",
    "source(file.path(root_dir, \"generate_conus_maps.r\"))",
    "",
    "# Generate maps for all variables and years",
    "map_result <- tryCatch({",
    "  generate_conus_maps(",
    "    output_dir = file.path(output_dir, \"maps\"),",
    "    db_path = file.path(output_dir, \"us_county_sdoh_data.duckdb\"),",
    "    conus_only = TRUE,",
    "    parallel = FALSE",
    "  )",
    "  TRUE",
    "}, error = function(e) {",
    "  log_message(paste(\"ERROR: Map generation failed:\", conditionMessage(e)), ",
    "              level = \"ERROR\", show_console = TRUE)",
    "  FALSE",
    "})",
    "",
    "if (map_result) {",
    "  log_message(\"Maps successfully generated\", level = \"INFO\", show_console = TRUE)",
    "} else {",
    "  log_message(\"Map generation encountered errors\", level = \"WARN\", show_console = TRUE)",
    "}",
    ""
  )
  
  # Insert the map generation code before the pipeline completion message
  updated_pipeline <- c(
    pipeline_content[1:(end_pipeline[1] - 1)],
    map_code,
    pipeline_content[end_pipeline[1]:length(pipeline_content)]
  )
  
  # Write the updated pipeline file
  writeLines(updated_pipeline, pipeline_file)
  cat("Successfully added map generation to pipeline\n")
  
  # Update generate_county_maps.r if it exists
  county_maps_file <- "generate_county_maps.r"
  if (file.exists(county_maps_file)) {
    cat("Updating existing generate_county_maps.r to use new functionality...\n")
    
    # Create a backup
    file.copy(county_maps_file, paste0(county_maps_file, ".bak"), overwrite = TRUE)
    
    # Add a note to the original file to use the new function
    wrapper_code <- c(
      "#!/usr/bin/env Rscript",
      "",
      "# NOTE: This script is now a wrapper for the enhanced generate_conus_maps.r script",
      "# It provides backwards compatibility but uses the improved functionality",
      "",
      "source(\"generate_conus_maps.r\")",
      "",
      "# Call the new function with parameters that match the old behavior",
      "generate_conus_maps(",
      "  output_dir = \"output/maps\",",
      "  db_path = \"us_county_sdoh_data.duckdb\",",
      "  conus_only = TRUE",
      ")"
    )
    
    writeLines(wrapper_code, county_maps_file)
    cat("Updated generate_county_maps.r to use new functionality\n")
  }
  
  cat("\nMap generation integration complete!\n")
  cat("The pipeline will now automatically generate maps for all variables and years.\n")
  cat("Maps will be saved to the output/maps directory.\n")
  
  return(TRUE)
}

# Define is_sourced function if it doesn't exist
if (!exists("is_sourced")) {
  is_sourced <- function() {
    # Check if the calling environment is the global environment
    # If it's not, the function is being sourced
    parent_env <- parent.frame()
    return(!identical(parent_env, .GlobalEnv))
  }
}

# Execute the function if run directly (not sourced)
if (!is_sourced()) {
  # Check for command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) > 0) {
    # Parse simple arguments
    db_path <- "us_county_sdoh_data.duckdb"  # default
    parallel <- FALSE
    
    # Check for database path argument
    if ("--db" %in% args) {
      db_index <- which(args == "--db") + 1
      if (db_index <= length(args)) {
        db_path <- args[db_index]
      }
    }
    
    # Check for parallel flag
    parallel_cores <- 2
    if ("--parallel" %in% args) {
      parallel <- TRUE
      cores_index <- which(args == "--parallel") + 1
      if (cores_index <= length(args) && grepl("^\\d+$", args[cores_index])) {
        parallel_cores <- as.integer(args[cores_index])
      }
    }
    
    # Run with command line arguments
    generate_conus_maps(
      db_path = db_path,
      parallel = parallel,
      cores = parallel_cores
    )
  } else {
    # Run the integration function if no specific arguments
    integrate_map_generation()
  }
}