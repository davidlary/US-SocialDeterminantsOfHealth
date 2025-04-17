#!/usr/bin/env Rscript

# This is a fixed version of fetch_county_data_final.r

library(tidyverse)
library(tidycensus)
library(duckdb)
library(httr)
library(parallel)
library(foreach)
library(doParallel)
library(future)
library(future.apply)
library(progressr)

fetch_county_data <- function(crosswalk, parallel = TRUE, num_cores = NULL) {
  # Check if parallel backend is already set up
  if (parallel && !inherits(future::plan(), "sequential")) {
    # A parallel backend is already registered from the parent function
    using_parallel <- TRUE
    cat("Using existing parallel backend for Census data fetching\n")
  } else if (parallel) {
    # Set up parallel backend for this function
    using_parallel <- TRUE
    if (is.null(num_cores)) {
      num_cores <- max(1, parallel::detectCores() - 1)
    }
    cat("Setting up parallel processing with", num_cores, "cores for Census data fetching\n")
    doParallel::registerDoParallel(cores = num_cores)
  } else {
    using_parallel <- FALSE
    cat("Using sequential processing for Census data fetching\n")
  }
  
  safe_get <- function(func, variable, year, context, ...) {
    max_attempts <- 3
    for (attempt in 1:max_attempts) {
      cat("Fetching ", context, " for year ", year, ", variable ", ifelse(is.null(variable), "NULL", variable), ", attempt ", attempt, "...\n")
      result <- tryCatch(
        func(variables = variable, year = year, ...) %>% suppressWarnings(),
        error = function(e) {
          message("Attempt ", attempt, " failed: ", e$message)
          if (attempt < max_attempts) Sys.sleep(10)
          return(NULL)
        }
      )
      if (!is.null(result)) return(result)
    }
    warning("Skipping variable ", variable, " for year ", year, " after ", max_attempts, " attempts.")
    return(NULL)
  }

  # Decennial Census
  cat("Starting Decennial fetch...\n")
  decennial_years <- c(1980, 1990, 2000, 2010, 2020)
  decennial_vars <- crosswalk %>%
    select(variable_name, matches("dec_\\d{4}_var")) %>%
    pivot_longer(-variable_name, names_to = "source", values_to = "variable") %>%
    filter(!is.na(variable)) %>%
    mutate(year = as.numeric(str_extract(source, "\\d{4}")))
  
  # Debug - print the variables being fetched
  cat("Variables to fetch by decennial year:\n")
  print(decennial_vars)
  
  # Check for pre-2000 variables - these need special handling
  historical_years <- c(1980, 1990)
  has_historical_vars <- any(decennial_vars$year %in% historical_years)
  
  if (has_historical_vars) {
    cat("\nDetected variables for historical Census years (1980, 1990).\n")
    cat("These will be handled by fetch_historical_data.r instead of direct API access.\n")
    cat("Filtering to only 2000+ variables for direct Census API access.\n\n")
    
    # Filter out historical years from direct API access
    decennial_vars <- decennial_vars %>%
      filter(!year %in% historical_years)
    
    # Remove historical years from direct fetch
    decennial_years <- setdiff(decennial_years, historical_years)
  }
  
  # Use parallel processing if enabled
  if (using_parallel) {
    # Use with_progress for safe progress handling
    decennial_results <- progressr::with_progress({
      # Create a progress bar for Decennial years
      p_dec <- progressr::progressor(along = decennial_years)
      
      # Process each year in parallel
      future.apply::future_lapply(decennial_years, function(year) {
        p_dec(sprintf("Decennial %d", year))
      
        vars <- decennial_vars %>% filter(year == !!year) %>% pull(variable)
        cat("Fetching Decennial variables for year ", year, ": ", paste(vars, collapse = ", "), "\n")
        if (length(vars) == 0) {
          warning("No variables to fetch for Decennial year ", year)
          return(tibble(year = integer(), GEOID = character(), NAME = character(), variable = character(), value = numeric(), source = character(), variable_name = character()))
        }
        
        # For 2020, we need to use "dhc" (Demographic and Housing Characteristics) instead of "pl" for some variables
        # This is a fallback if the pl fails
        dataset <- if(year == 2020) "pl" else "sf1"
        
        # Map variables back to their standardized names for later use
        var_std_map <- decennial_vars %>% 
          filter(year == !!year) %>% 
          select(variable, variable_name) %>%
          deframe()
        
        map_df(vars, function(var) {
          result <- safe_get(
            get_decennial, 
            variable = var, 
            year = year, 
            context = "Decennial", 
            geography = "county", 
            cache_table = TRUE,
            sumfile = dataset
          )
          
          # If 2020 and pl fails, try dhc
          if (is.null(result) && year == 2020) {
            cat("Retrying with DHC dataset for 2020...\n")
            result <- safe_get(
              get_decennial, 
              variable = var, 
              year = year, 
              context = "Decennial DHC", 
              geography = "county", 
              cache_table = TRUE,
              sumfile = "dhc" # Try the DHC summary file instead
            )
          }
          
          if (!is.null(result)) {
            result %>%
              mutate(
                year = year, 
                source = paste("Decennial", year), 
                GEOID = str_pad(GEOID, 5, "left", "0"),
                variable_name = var_std_map[variable] # Add standardized name directly
              ) %>%
              select(year, GEOID, NAME, variable, value, source, variable_name)
          } else {
            tibble(year = integer(), GEOID = character(), NAME = character(), variable = character(), value = numeric(), source = character(), variable_name = character())
          }
        })
      }, future.packages = c("tidyverse", "dplyr", "tidycensus"))
    })
    
    # Combine results from all years
    decennial_data <- bind_rows(decennial_results)
  } else {
    # Sequential processing (original code)
    decennial_data <- map_df(decennial_years, function(year) {
      vars <- decennial_vars %>% filter(year == !!year) %>% pull(variable)
      cat("Fetching Decennial variables for year ", year, ": ", paste(vars, collapse = ", "), "\n")
      if (length(vars) == 0) {
        warning("No variables to fetch for Decennial year ", year)
        return(tibble(year = integer(), GEOID = character(), NAME = character(), variable = character(), value = numeric(), source = character(), variable_name = character()))
      }
      
      # For 2020, we need to use "dhc" (Demographic and Housing Characteristics) instead of "pl" for some variables
      # This is a fallback if the pl fails
      dataset <- if(year == 2020) "pl" else "sf1"
      
      # Map variables back to their standardized names for later use
      var_std_map <- decennial_vars %>% 
        filter(year == !!year) %>% 
        select(variable, std_name) %>%
        deframe()
      
      map_df(vars, function(var) {
        result <- safe_get(
          get_decennial, 
          variable = var, 
          year = year, 
          context = "Decennial", 
          geography = "county", 
          cache_table = TRUE,
          sumfile = dataset
        )
        
        # If 2020 and pl fails, try dhc
        if (is.null(result) && year == 2020) {
          cat("Retrying with DHC dataset for 2020...\n")
          result <- safe_get(
            get_decennial, 
            variable = var, 
            year = year, 
            context = "Decennial DHC", 
            geography = "county", 
            cache_table = TRUE,
            sumfile = "dhc" # Try the DHC summary file instead
          )
        }
        
        if (!is.null(result)) {
          result %>%
            mutate(
              year = year, 
              source = paste("Decennial", year), 
              GEOID = str_pad(GEOID, 5, "left", "0"),
              variable_name = var_std_map[variable] # Add standardized name directly
            ) %>%
            select(year, GEOID, NAME, variable, value, source, variable_name)
        } else {
          tibble(year = integer(), GEOID = character(), NAME = character(), variable = character(), value = numeric(), source = character(), variable_name = character())
        }
      })
    })
  }
  
  # Pivot wider to create a clean dataset with standardized column names
  decennial_wide <- decennial_data %>%
    select(-variable) %>%  # Remove the original variable name
    pivot_wider(
      id_cols = c(year, GEOID, NAME, source),
      names_from = variable_name,
      values_from = value
    )
  
  cat("Completed Decennial fetch with ", nrow(decennial_wide), " rows.\n")
  
  # PEP - IMPROVED VERSION WITH CENSUS API UPDATES
  cat("Starting PEP fetch...\n")
  
  # Define multiple years to try but skip 2020 initially due to known issues
  # Include both year + vintage for post-2020 data
  pep_approaches <- list(
    # New PEP API approach with year + vintage
    list(year = 2023, vintage = 2023, type = "api"),  # Added 2023 data
    list(year = 2022, vintage = 2022, type = "api"),  # Added 2022 data
    list(year = 2021, vintage = 2021, type = "api"),
    list(year = 2019, vintage = 2019, type = "api"),
    list(year = 2018, vintage = 2018, type = "api"),
    
    # Alternative API endpoints for older vintages
    list(year = 2019, vintage = 2019, endpoint = "https://api.census.gov/data/2019/pep/charagegroups", type = "api_alt"),
    list(year = 2018, vintage = 2018, endpoint = "https://api.census.gov/data/2018/pep/charagegroups", type = "api_alt"),
    
    # Direct file downloads as last resort - with updated URLs
    list(year = 2023, file = "https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/counties/totals/co-est2023-alldata.csv", type = "file"),  # Added 2023 data
    list(year = 2022, file = "https://www2.census.gov/programs-surveys/popest/datasets/2020-2022/counties/totals/co-est2022-alldata.csv", type = "file"),
    list(year = 2021, file = "https://www2.census.gov/programs-surveys/popest/datasets/2020-2021/counties/totals/co-est2021-alldata.csv", type = "file"),
    list(year = 2019, file = "https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv", type = "file"),
    
    # Try the FTP server as well
    list(year = 2023, file = "ftp://ftp2.census.gov/programs-surveys/popest/datasets/2020-2023/counties/totals/co-est2023-alldata.csv", type = "file"),
    list(year = 2022, file = "ftp://ftp2.census.gov/programs-surveys/popest/datasets/2020-2022/counties/totals/co-est2022-alldata.csv", type = "file"),
    list(year = 2019, file = "ftp://ftp2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv", type = "file"),
    
    # Another URL format variation
    list(year = 2023, file = "https://www2.census.gov/programs-surveys/popest/tables/2020-2023/counties/totals/co-est2023-alldata.csv", type = "file"),
    list(year = 2022, file = "https://www2.census.gov/programs-surveys/popest/tables/2020-2022/counties/totals/co-est2022-alldata.csv", type = "file"),
    list(year = 2019, file = "https://www2.census.gov/programs-surveys/popest/tables/2010-2019/counties/totals/co-est2019-alldata.csv", type = "file")
  )
  
  # Try to fetch PEP data from any available source
  pep_wide <- NULL
  
  # Process each approach
  for (approach in pep_approaches) {
    # Skip if we already have data
    if (!is.null(pep_wide) && nrow(pep_wide) > 0) break
    
    if (approach$type == "api") {
      # New API approach with vintage parameter for post-2020 data
      year <- approach$year
      vintage <- approach$vintage
      
      cat(sprintf("Trying PEP data via tidycensus API for year %d (vintage %d)...\n", year, vintage))
      
      # Set longer timeout
      options(timeout = 180) # 3 minutes timeout
      
      # Try with explicit vintage and year
      result <- tryCatch({
        # Try the newer approach with vintage parameter
        if (vintage >= 2020) {
          # For post-2020 data, use both vintage and year
          get_estimates(
            geography = "county",
            product = "population",
            year = year,
            vintage = vintage,
            cache_table = TRUE
          )
        } else {
          # For pre-2020 data, use the original approach
          get_estimates(
            geography = "county",
            product = "population",
            year = year,
            cache_table = TRUE
          )
        }
      }, error = function(e) {
        cat("PEP fetch with vintage parameter failed:", conditionMessage(e), "\n")
        NULL
      })
      
      # If first attempt failed, try with variable = "POP"
      if (is.null(result)) {
        Sys.sleep(3) # Short pause before retry
        
        cat("Trying basic variables approach...\n")
        result <- tryCatch({
          if (vintage >= 2020) {
            get_estimates(
              geography = "county",
              variables = "POP",
              year = year,
              vintage = vintage,
              cache_table = TRUE
            )
          } else {
            get_estimates(
              geography = "county",
              variables = "POP",
              year = year,
              cache_table = TRUE
            )
          }
        }, error = function(e) {
          cat("Basic PEP variables approach failed:", conditionMessage(e), "\n")
          NULL
        })
      }
      
      # Process results if successful
      if (!is.null(result) && nrow(result) > 0) {
        cat("Successfully retrieved data. Processing...\n")
        
        # Try to determine the structure of the result
        cat("Result structure:", paste(names(result), collapse=", "), "\n")
        
        # Look for population column with more flexibility
        pop_col <- NULL
        
        # Check for different column patterns
        if ("value" %in% names(result)) {
          # New API format - the value column has the population
          pop_col <- "value"
        } else {
          # Try exact matches for population columns
          exact_matches <- c("POP", "POPESTIMATE", "POPEST", "POPESTIMATE2023", "POPESTIMATE2022", "POPESTIMATE2021", 
                            "POPESTIMATE2020", "POPESTIMATE2019", "POPESTIMATE2018")
          for (col in exact_matches) {
            if (col %in% names(result)) {
              pop_col <- col
              break
            }
          }
          
          # If exact matches fail, try pattern matching
          if (is.null(pop_col)) {
            # Try columns containing population-related terms
            pattern_matches <- grep("^POP|POPEST|ESTIMATE|TOT_POP|CENSUS", names(result), value = TRUE)
            if (length(pattern_matches) > 0) {
              # Use the first match or prefer columns with specific patterns
              pop_matches <- grep(paste0("POP$|POP", year, "$|POPEST$|POPESTIMATE$|POPESTIMATE", year, "$"), 
                                 pattern_matches, value = TRUE)
              if (length(pop_matches) > 0) {
                pop_col <- pop_matches[1]
              } else {
                pop_col <- pattern_matches[1]
              }
            }
          }
        }
        
        if (is.null(pop_col)) {
          cat("No population column found in result. Available columns:", 
              paste(names(result), collapse=", "), "\n")
        } else {
          cat("Using column", pop_col, "for population data\n")
          
          # Different processing based on API response format
          if ("value" %in% names(result) && "variable" %in% names(result)) {
            # New format with variable/value structure
            pep_wide <- result %>%
              filter(variable == pop_col) %>%
              select(GEOID, NAME, value) %>%
              mutate(
                year = year,
                source = "PEP",
                GEOID = str_pad(GEOID, 5, "left", "0"),
                total_population = as.numeric(value)
              ) %>%
              select(year, GEOID, NAME, source, total_population)
          } else {
            # Traditional format with population in a column
            pep_wide <- result %>%
              select(GEOID, NAME, !!pop_col) %>%
              mutate(
                year = year,
                source = "PEP",
                GEOID = str_pad(GEOID, 5, "left", "0"),
                total_population = as.numeric(!!sym(pop_col))
              ) %>%
              select(year, GEOID, NAME, source, total_population)
          }
          
          cat("Successfully extracted PEP data with", nrow(pep_wide), "rows\n")
        }
      } else {
        cat("API approach failed or returned empty result.\n")
      }
    } else if (approach$type == "api_alt") {
      # Try alternative API endpoint
      cat("Trying alternative API endpoint:", approach$endpoint, "\n")
      
      # Set longer timeout
      options(timeout = 180)
      
      # Try to fetch directly from alternative endpoint
      year <- approach$year
      endpoint <- approach$endpoint
      
      result <- tryCatch({
        api_data <- GET(paste0(endpoint, "?get=POP&for=county:*&key=", Sys.getenv("CENSUS_API_KEY")))
        
        if (http_status(api_data)$category == "Success") {
          content <- content(api_data, "text")
          jsonlite::fromJSON(content)
        } else {
          NULL
        }
      }, error = function(e) {
        cat("Alternative API endpoint failed:", conditionMessage(e), "\n")
        NULL
      })
      
      if (!is.null(result) && is.list(result) && length(result) > 0) {
        cat("Received data from alternative endpoint. Processing...\n")
        
        # Process based on expected structure
        tryCatch({
          # Typically JSON responses have column names in first row
          col_names <- result[[1]]
          data_rows <- result[-1]
          
          # Convert to data frame
          df <- as.data.frame(do.call(rbind, data_rows))
          names(df) <- col_names
          
          # Find population column (usually named "POP")
          pop_col <- grep("^POP", names(df), value = TRUE)
          
          if (length(pop_col) > 0) {
            # Extract state and county columns
            state_col <- grep("^state$|STATE", names(df), ignore.case = TRUE, value = TRUE)[1]
            county_col <- grep("^county$|COUNTY", names(df), ignore.case = TRUE, value = TRUE)[1]
            
            # Create dataset
            pep_wide <- df %>%
              mutate(
                GEOID = paste0(
                  str_pad(!!sym(state_col), 2, "left", "0"),
                  str_pad(!!sym(county_col), 3, "left", "0")
                ),
                total_population = as.numeric(!!sym(pop_col[1])),
                year = year,
                source = "PEP",
                NAME = paste("County", GEOID)  # We don't have names from this source
              ) %>%
              select(year, GEOID, NAME, source, total_population)
            
            cat("Successfully created dataset from alternative API with", nrow(pep_wide), "rows\n")
          } else {
            cat("No population column found in alternative API response.\n")
          }
        }, error = function(e) {
          cat("Error processing alternative API response:", conditionMessage(e), "\n")
        })
      } else {
        cat("Alternative API endpoint returned no usable data.\n")
      }
    } else if (approach$type == "file") {
      # Try direct file download
      year <- approach$year
      file_url <- approach$file
      temp_file <- tempfile(fileext = ".csv")
      
      cat("Trying to download PEP data from:", file_url, "\n")
      
      # Try to download with additional headers to avoid 403
      download_success <- tryCatch({
        # Try with custom headers
        GET(file_url, 
            write_disk(temp_file, overwrite = TRUE),
            add_headers(
              'User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36',
              'Accept' = 'text/html,application/xhtml+xml,application/xml',
              'Accept-Language' = 'en-US,en;q=0.9',
              'Referer' = 'https://www.census.gov/'
            ),
            timeout(60))
        
        # Check if file exists and has content
        file.exists(temp_file) && file.info(temp_file)$size > 0
      }, error = function(e) {
        cat("Download failed:", conditionMessage(e), "\n")
        FALSE
      })
      
      if (download_success) {
        cat("Download successful! Processing file...\n")
        
        # Try to read and process the file
        tryCatch({
          # Read the CSV
          pep_data <- read_csv(temp_file, show_col_types = FALSE)
          
          if (nrow(pep_data) > 0) {
            cat("CSV file loaded with", nrow(pep_data), "rows,", ncol(pep_data), "columns\n")
            cat("Column names:", paste(head(names(pep_data), 10), collapse=", "), "...\n")
            
            # Identify key columns
            geoid_col <- NULL
            state_col <- NULL
            county_col <- NULL
            name_col <- NULL
            pop_col <- NULL
            
            # Look for columns containing geographic identifiers
            possible_geo_cols <- c(
              # Direct GEOID possibilities
              grep("^GEOID|FIPS|STATE_COUNTY", names(pep_data), ignore.case = TRUE, value = TRUE),
              # State FIPS
              grep("^STATE$|STATEFP|STATE_FIPS", names(pep_data), ignore.case = TRUE, value = TRUE),
              # County FIPS
              grep("^COUNTY$|COUNTYFP|COUNTY_FIPS", names(pep_data), ignore.case = TRUE, value = TRUE)
            )
            
            if (length(possible_geo_cols) >= 2) {
              # If we have at least 2 columns, they're likely state and county codes
              state_col <- possible_geo_cols[1]
              county_col <- possible_geo_cols[2]
            } else if (length(possible_geo_cols) == 1) {
              # If only one, it might be a combined GEOID
              geoid_col <- possible_geo_cols[1]
            }
            
            # Look for county name
            name_cols <- grep("^NAME|COUNTY_NAME|CTYNAME", names(pep_data), ignore.case = TRUE, value = TRUE)
            if (length(name_cols) > 0) name_col <- name_cols[1]
            
            # Look for population columns
            # First try year-specific columns
            year_specific_pop_cols <- grep(paste0("POPESTIMATE", year, "$|POP", year, "$"), 
                                          names(pep_data), value = TRUE)
            
            # Then try general population columns
            general_pop_cols <- grep("^POPESTIMATE$|^POP$|^POPULATION$", names(pep_data), value = TRUE)
            
            # Combine and prioritize year-specific
            pop_cols <- c(year_specific_pop_cols, general_pop_cols)
            
            if (length(pop_cols) > 0) {
              pop_col <- pop_cols[1]
              cat("Using population column:", pop_col, "\n")
              
              # Create dataset
              if (!is.null(geoid_col)) {
                # Using direct GEOID
                cat("Using direct GEOID column:", geoid_col, "\n")
                
                pep_wide <- pep_data %>%
                  select(!!geoid_col, !!pop_col) %>%
                  rename(GEOID = !!geoid_col) %>%
                  mutate(
                    GEOID = str_pad(as.character(GEOID), 5, "left", "0"),
                    NAME = if (!is.null(name_col) && name_col %in% names(pep_data)) 
                             pep_data[[name_col]] else paste("County", GEOID),
                    year = year,
                    source = "PEP",
                    total_population = as.numeric(!!sym(pop_col))
                  ) %>%
                  select(year, GEOID, NAME, source, total_population)
              } else if (!is.null(state_col) && !is.null(county_col)) {
                # Combine state and county codes
                cat("Combining state column", state_col, "and county column", county_col, "\n")
                
                pep_wide <- pep_data %>%
                  select(!!state_col, !!county_col, !!pop_col) %>%
                  mutate(
                    GEOID = str_pad(paste0(
                      str_pad(as.character(!!sym(state_col)), 2, "left", "0"),
                      str_pad(as.character(!!sym(county_col)), 3, "left", "0")
                    ), 5, "left", "0"),
                    NAME = if (!is.null(name_col) && name_col %in% names(pep_data)) 
                             pep_data[[name_col]] else paste("County", GEOID),
                    year = year,
                    source = "PEP",
                    total_population = as.numeric(!!sym(pop_col))
                  ) %>%
                  select(year, GEOID, NAME, source, total_population)
              } else {
                cat("Could not identify required geographic identifier columns.\n")
              }
              
              if (exists("pep_wide") && !is.null(pep_wide) && nrow(pep_wide) > 0) {
                cat("Successfully created PEP dataset with", nrow(pep_wide), "rows\n")
              } else {
                cat("Failed to create PEP dataset.\n")
              }
            } else {
              cat("No population column found in the file.\n")
            }
          } else {
            cat("CSV file is empty.\n")
          }
        }, error = function(e) {
          cat("Error processing CSV file:", conditionMessage(e), "\n")
        })
      } else {
        cat("File download failed or file is empty.\n")
      }
      
      # Clean up temp file
      if (file.exists(temp_file)) file.remove(temp_file)
    }
  }
  
  # If all methods failed, create an empty dataset
  if (is.null(pep_wide) || nrow(pep_wide) == 0) {
    pep_wide <- tibble(
      year = integer(),
      GEOID = character(),
      NAME = character(),
      source = character(),
      total_population = numeric()
    )
    cat("All PEP data retrieval methods failed. Created empty placeholder.\n")
  }
  
  cat("Completed PEP fetch with ", nrow(pep_wide), " rows.\n")
  
  # ACS - IMPROVED VERSION WITH 2022 SUPPORT
  cat("Starting ACS fetch...\n")
  acs_vars <- crosswalk %>% 
    filter(!is.na(acs_var)) %>% 
    select(variable_name, acs_var) %>%
    deframe()  # Convert to a named vector for mapping
  
  cat("ACS variables: ", paste(names(acs_vars), "=", acs_vars, collapse = ", "), "\n")
  
  # Update years to include all available years (1-year and 5-year data)
  acs_years <- 2005:2023
  
  # Set up parallel processing for years if parallel is enabled
  if (using_parallel) {
    cat("Using parallel processing for ACS data fetching\n")
    
    # Use with_progress for safe progress handling
    years_results <- progressr::with_progress({
      # Create a progress bar
      p <- progressr::progressor(along = acs_years)
      
      # Use future.apply for parallel processing of years
      # Get data for each year in parallel
      future.apply::future_lapply(acs_years, function(year) {
        p(sprintf("Processing ACS year %d", year))
      
        cat("Fetching ACS variables for year ", year, "\n")
        acs_var_list <- acs_vars
        
        if (length(acs_var_list) == 0) {
          warning("No variables to fetch for ACS year ", year)
          return(tibble(year = integer(), GEOID = character(), NAME = character(), variable = character(), value = numeric(), source = character(), variable_name = character()))
        }
        
        # Further parallelize variable fetching within each year if we have many variables
        if (length(acs_var_list) > 10 && inherits(future::plan(), "multisession")) {
          # Create variable/name pairs for parallel processing
          var_pairs <- tibble(
            var_name = names(acs_var_list),
            var_code = unname(acs_var_list)
          )
          
          # Use with_progress for nested parallel processing
          result_data <- progressr::with_progress({
            # Use parallel processing for variables too
            pv <- progressr::progressor(along = names(acs_var_list))
            
            # Use future.apply for variables too
            future.apply::future_lapply(1:nrow(var_pairs), function(i) {
              var_name <- var_pairs$var_name[i]
              var_code <- var_pairs$var_code[i]
              
              pv(sprintf("Var %s", var_code))
            
              # Try to fetch this variable
              cat("Fetching ACS variable", var_code, "for", var_name, "\n")
              
              # Add multiple retries and better error handling
              result <- NULL
              retry_count <- 0
              max_retries <- 3
              
              while(is.null(result) && retry_count < max_retries) {
                retry_count <- retry_count + 1
                
                result <- tryCatch({
                  # Try to fetch with increased timeout
                  options(timeout = 120)
                  
                  # Use withCallingHandlers to capture warnings
                  withCallingHandlers({
                    # For 2022, we'll try both ACS5 and ACS1 if needed
                    if (year == 2022) {
                      # Try ACS5 first
                      acs_result <- tryCatch({
                        get_acs(
                          variables = var_code,
                          year = year,
                          geography = "county",
                          survey = "acs5",
                          cache_table = TRUE
                        )
                      }, error = function(e) {
                        cat("ACS5 fetch failed for 2022, trying ACS1: ", conditionMessage(e), "\n")
                        # Try ACS1 as fallback
                        tryCatch({
                          get_acs(
                            variables = var_code,
                            year = year,
                            geography = "county",
                            survey = "acs1",  # Try 1-year estimates as fallback
                            cache_table = TRUE
                          )
                        }, error = function(e2) {
                          cat("ACS1 fetch also failed for 2022: ", conditionMessage(e2), "\n")
                          NULL
                        })
                      })
                      acs_result
                    } else {
                      # For other years, use ACS5 as before
                      get_acs(
                        variables = var_code,
                        year = year,
                        geography = "county",
                        survey = "acs5",
                        cache_table = TRUE
                      )
                    }
                  }, warning = function(w) {
                    cat("Warning in ACS fetch for", var_name, ":", conditionMessage(w), "\n")
                  })
                }, error = function(e) {
                  cat("Error in ACS fetch for", var_name, "attempt", retry_count, ":", conditionMessage(e), "\n")
                  
                  # If we have more retries, wait a bit longer each time
                  if (retry_count < max_retries) {
                    wait_time <- 5 * retry_count
                    cat("Waiting", wait_time, "seconds before retry...\n")
                    Sys.sleep(wait_time)
                  }
                  return(NULL)
                })
              }
              
              if (!is.null(result)) {
                # Process successful result
                result %>%
                  mutate(
                    year = year,
                    source = "ACS 5-Year",
                    GEOID = str_pad(GEOID, 5, "left", "0"),
                    variable_name = var_name
                  ) %>%
                  select(year, GEOID, NAME, variable, value = estimate, source, variable_name)
              } else {
                # Return empty tibble for unsuccessful fetches
                cat("Failed to fetch ACS variable", var_code, "for", var_name, "after", max_retries, "attempts\n")
                tibble(year = integer(), GEOID = character(), NAME = character(), 
                       variable = character(), value = numeric(), source = character(), 
                       variable_name = character())
              }
            }, future.packages = c("tidyverse", "dplyr", "tidycensus"))
          })
          
          # Combine all variable results
          bind_rows(result_data)
        } else {
          # Sequential processing for variables
          map_df(names(acs_var_list), function(std_var_name) {
            var_code <- acs_var_list[[std_var_name]]
            
            # Try to fetch this variable
            cat("Fetching ACS variable", var_code, "for", std_var_name, "\n")
            
            # Add multiple retries and better error handling
            result <- NULL
            retry_count <- 0
            max_retries <- 3
            
            while(is.null(result) && retry_count < max_retries) {
              retry_count <- retry_count + 1
              
              result <- tryCatch({
                # Try to fetch with increased timeout
                options(timeout = 120)
                
                # Use withCallingHandlers to capture warnings
                withCallingHandlers({
                  # For 2022, we'll try both ACS5 and ACS1 if needed
                  if (year == 2022) {
                    # Try ACS5 first
                    acs_result <- tryCatch({
                      get_acs(
                        variables = var_code,
                        year = year,
                        geography = "county",
                        survey = "acs5",
                        cache_table = TRUE
                      )
                    }, error = function(e) {
                      cat("ACS5 fetch failed for 2022, trying ACS1: ", conditionMessage(e), "\n")
                      # Try ACS1 as fallback
                      tryCatch({
                        get_acs(
                          variables = var_code,
                          year = year,
                          geography = "county",
                          survey = "acs1",  # Try 1-year estimates as fallback
                          cache_table = TRUE
                        )
                      }, error = function(e2) {
                        cat("ACS1 fetch also failed for 2022: ", conditionMessage(e2), "\n")
                        NULL
                      })
                    })
                    acs_result
                  } else {
                    # For other years, use ACS5 as before
                    get_acs(
                      variables = var_code,
                      year = year,
                      geography = "county",
                      survey = "acs5",
                      cache_table = TRUE
                    )
                  }
                }, warning = function(w) {
                  cat("Warning in ACS fetch for", std_var_name, ":", conditionMessage(w), "\n")
                })
              }, error = function(e) {
                cat("Error in ACS fetch for", std_var_name, "attempt", retry_count, ":", conditionMessage(e), "\n")
                
                # If we have more retries, wait a bit longer each time
                if (retry_count < max_retries) {
                  wait_time <- 5 * retry_count
                  cat("Waiting", wait_time, "seconds before retry...\n")
                  Sys.sleep(wait_time)
                }
                return(NULL)
              })
            }
            
            if (!is.null(result)) {
              # Process successful result
              result %>%
                mutate(
                  year = year,
                  source = "ACS 5-Year",
                  GEOID = str_pad(GEOID, 5, "left", "0"),
                  variable_name = std_var_name
                ) %>%
                select(year, GEOID, NAME, variable, value = estimate, source, variable_name)
            } else {
              # Return empty tibble for unsuccessful fetches
              cat("Failed to fetch ACS variable", var_code, "for", std_var_name, "after", max_retries, "attempts\n")
              tibble(year = integer(), GEOID = character(), NAME = character(), 
                     variable = character(), value = numeric(), source = character(), 
                     variable_name = character())
            }
          })
        }
      }, future.packages = c("tidyverse", "dplyr", "tidycensus"))
    })
    
    # Combine results from all years
    cat("Combining parallel results from all years...\n")
    acs_data <- bind_rows(years_results)
  } else {
    # Sequential processing
    acs_data <- map_df(acs_years, function(year) {
      cat("Fetching ACS variables for year ", year, "\n")
      acs_var_list <- acs_vars
      
      if (length(acs_var_list) == 0) {
        warning("No variables to fetch for ACS year ", year)
        return(tibble(year = integer(), GEOID = character(), NAME = character(), variable = character(), value = numeric(), source = character(), variable_name = character()))
      }
    
      # Fetch variables one by one with improved error handling
      result_list <- map_df(names(acs_var_list), function(std_var_name) {
        var_code <- acs_var_list[[std_var_name]]
        
        # Try to fetch this variable
        cat("Fetching ACS variable", var_code, "for", std_var_name, "\n")
        
        # Add multiple retries and better error handling
        result <- NULL
        retry_count <- 0
        max_retries <- 3
        
        while(is.null(result) && retry_count < max_retries) {
          retry_count <- retry_count + 1
          
          result <- tryCatch({
            # Try to fetch with increased timeout
            options(timeout = 120)
            
            # Use withCallingHandlers to capture warnings
            withCallingHandlers({
              # For 2022, we'll try both ACS5 and ACS1 if needed
              if (year == 2022) {
                # Try ACS5 first
                acs_result <- tryCatch({
                  get_acs(
                    variables = var_code,
                    year = year,
                    geography = "county",
                    survey = "acs5",
                    cache_table = TRUE
                  )
                }, error = function(e) {
                  cat("ACS5 fetch failed for 2022, trying ACS1: ", conditionMessage(e), "\n")
                  # Try ACS1 as fallback
                  tryCatch({
                    get_acs(
                      variables = var_code,
                      year = year,
                      geography = "county",
                      survey = "acs1",  # Try 1-year estimates as fallback
                      cache_table = TRUE
                    )
                  }, error = function(e2) {
                    cat("ACS1 fetch also failed for 2022: ", conditionMessage(e2), "\n")
                    NULL
                  })
                })
                acs_result
              } else {
                # For other years, use ACS5 as before
                get_acs(
                  variables = var_code,
                  year = year,
                  geography = "county",
                  survey = "acs5",
                  cache_table = TRUE
                )
              }
            }, warning = function(w) {
              cat("Warning in ACS fetch for", std_var_name, ":", conditionMessage(w), "\n")
            })
          }, error = function(e) {
            cat("Error in ACS fetch for", std_var_name, "attempt", retry_count, ":", conditionMessage(e), "\n")
            
            # If we have more retries, wait a bit longer each time
            if (retry_count < max_retries) {
              wait_time <- 5 * retry_count
              cat("Waiting", wait_time, "seconds before retry...\n")
              Sys.sleep(wait_time)
            }
            return(NULL)
          })
        }
        
        if (!is.null(result)) {
          # Process successful result
          result %>%
            mutate(
              year = year,
              source = "ACS 5-Year",
              GEOID = str_pad(GEOID, 5, "left", "0"),
              variable_name = std_var_name
            ) %>%
            select(year, GEOID, NAME, variable, value = estimate, source, variable_name)
        } else {
          # Return empty tibble for unsuccessful fetches
          cat("Failed to fetch ACS variable", var_code, "for", std_var_name, "after", max_retries, "attempts\n")
          tibble(year = integer(), GEOID = character(), NAME = character(), 
                 variable = character(), value = numeric(), source = character(), 
                 variable_name = character())
        }
      })
      
      return(result_list)
    })
  }
  
  # Add specific handling for common error patterns in 2022 data
  if (any(acs_data$year == 2022)) {
    # Check for any anomalies in the 2022 data
    problematic_2022_data <- acs_data %>% 
      filter(year == 2022, is.na(value) | is.infinite(value))
    
    if (nrow(problematic_2022_data) > 0) {
      cat("Found", nrow(problematic_2022_data), "problematic entries in 2022 ACS data\n")
      
      # Try to fix by using most recent available data for those entries
      for (i in 1:nrow(problematic_2022_data)) {
        problem_row <- problematic_2022_data[i, ]
        cat("Attempting to fix problematic 2022 data for GEOID:", problem_row$GEOID, 
            "variable:", problem_row$variable, "\n")
        
        # Find most recent available data for this GEOID and variable
        replacement_data <- acs_data %>% 
          filter(GEOID == problem_row$GEOID, 
                 variable_name == problem_row$variable_name,
                 year < 2022,
                 !is.na(value), 
                 !is.infinite(value)) %>%
          arrange(desc(year)) %>%
          slice(1)
        
        if (nrow(replacement_data) > 0) {
          cat("Found replacement data from year", replacement_data$year, "\n")
          
          # Update the problematic entry with most recent available data
          acs_data <- acs_data %>%
            mutate(
              value = ifelse(
                year == 2022 & GEOID == problem_row$GEOID & variable_name == problem_row$variable_name,
                replacement_data$value,
                value
              ),
              source = ifelse(
                year == 2022 & GEOID == problem_row$GEOID & variable_name == problem_row$variable_name,
                paste("ACS (backfilled from", replacement_data$year, ")"),
                source
              )
            )
        } else {
          cat("No suitable replacement data found for GEOID:", problem_row$GEOID, 
              "variable:", problem_row$variable, "\n")
        }
      }
    }
  }
  
  # Pivot wider for ACS data
  acs_wide <- acs_data %>%
    select(-variable) %>%
    pivot_wider(
      id_cols = c(year, GEOID, NAME, source),
      names_from = variable_name,
      values_from = value
    )
  
  cat("Completed ACS fetch with ", nrow(acs_wide), " rows.\n")
  
  # Additional checks for 2022 ACS data quality and completeness
  if (any(acs_wide$year == 2022)) {
    acs_2022 <- acs_wide %>% filter(year == 2022)
    cat("Successfully retrieved 2022 ACS data for", nrow(acs_2022), "counties\n")
    
    # Check for completeness of data
    missing_vars_2022 <- acs_2022 %>%
      summarise(across(-c(year, GEOID, NAME, source), ~sum(is.na(.)))) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
      filter(missing_count > 0)
    
    if (nrow(missing_vars_2022) > 0) {
      cat("Warning: Some 2022 ACS variables have missing values:\n")
      print(missing_vars_2022)
      
      # Attempt to impute from previous years where reasonable
      # This is a simple approach - in production you might want more sophisticated imputation
      for (var_name in missing_vars_2022$variable) {
        cat("Attempting to impute missing 2022 values for", var_name, "\n")
        
        # Find counties with missing values for this variable
        missing_counties <- acs_2022 %>%
          filter(is.na(!!sym(var_name))) %>%
          pull(GEOID)
        
        for (county_id in missing_counties) {
          # Find most recent available data for this county and variable
          prior_value <- acs_wide %>% 
            filter(
              GEOID == county_id,
              year < 2022,
              !is.na(!!sym(var_name))
            ) %>%
            arrange(desc(year)) %>%
            slice(1) %>%
            pull(!!sym(var_name))
          
          if (length(prior_value) > 0 && !is.na(prior_value[1])) {
            # Use the most recent prior value as an imputation
            row_idx <- which(acs_wide$year == 2022 & acs_wide$GEOID == county_id)
            if (length(row_idx) > 0) {
              acs_wide[row_idx, var_name] <- prior_value[1]
              acs_wide[row_idx, "source"] <- "ACS (imputed)"
              cat("Imputed", var_name, "for county", county_id, "with value from previous years\n")
            }
          }
        }
      }
    } else {
      cat("2022 ACS data is complete with no missing values!\n")
    }
    
    # Introduce specific checks for 2022 ACS data quality compared to previous years
    cat("Performing quality checks on 2022 ACS data...\n")
    
    # For each variable, check for outliers compared to previous years
    # This helps identify potential data quality issues specific to 2022
    acs_vars_to_check <- setdiff(names(acs_2022), c("year", "GEOID", "NAME", "source"))
    
    for (var_name in acs_vars_to_check) {
      # Skip variables that are already entirely NA or don't exist
      if (!var_name %in% names(acs_2022) || all(is.na(acs_2022[[var_name]]))) {
        next
      }
      
      cat("Checking variable", var_name, "for outliers...\n")
      
      # Wrap in tryCatch to handle any errors
      acs_compare <- tryCatch({
        # Compare 2022 values with 2021 values to identify significant changes
        acs_wide %>%
          filter(year %in% c(2021, 2022)) %>%
          select(year, GEOID, NAME, !!sym(var_name)) %>%
          pivot_wider(
            id_cols = c(GEOID, NAME),
            names_from = year,
            values_from = !!sym(var_name),
            names_prefix = "year_"
          ) %>%
          filter(!is.na(year_2021), !is.na(year_2022)) %>%
          # First check if values can be converted to numeric
          mutate(
            # Try to convert to numeric, treating errors as NA
            year_2021_num = tryCatch(as.numeric(as.character(year_2021)), error = function(e) NA_real_),
            year_2022_num = tryCatch(as.numeric(as.character(year_2022)), error = function(e) NA_real_),
            # Filter out rows where conversion failed
            valid_numeric = !is.na(year_2021_num) & !is.na(year_2022_num) & 
                            year_2021_num != 0 & !is.infinite(year_2021_num) & 
                            !is.infinite(year_2022_num)
          ) %>%
          filter(valid_numeric) %>%
          # Now calculate percentage change only on valid numeric rows
          mutate(
            pct_change = (year_2022_num - year_2021_num) / year_2021_num * 100,
            abs_pct_change = abs(pct_change)
          ) %>%
          select(-year_2021_num, -year_2022_num, -valid_numeric)
      }, error = function(e) {
        cat("Error processing variable", var_name, ":", conditionMessage(e), "\n")
        # Return empty tibble with the expected structure
        tibble(
          GEOID = character(),
          NAME = character(),
          year_2021 = numeric(),
          year_2022 = numeric(),
          pct_change = numeric(),
          abs_pct_change = numeric()
        )
      })
      
      # Check if acs_compare has data before processing
      if (nrow(acs_compare) > 0) {
        # Identify potential outliers (counties with very large changes)
        # Using a simple threshold approach for demonstration
        outliers <- acs_compare %>%
          filter(abs_pct_change > 50) %>%  # Counties with >50% change
          arrange(desc(abs_pct_change))
        
        if (nrow(outliers) > 0) {
          cat("Found", nrow(outliers), "potential outliers in 2022 data for", var_name, "\n")
          
          # For each outlier, consider applying a correction
          for (i in 1:min(nrow(outliers), 10)) {  # Limit to 10 most extreme cases
            outlier_county <- outliers$GEOID[i]
            outlier_name <- outliers$NAME[i]
            outlier_change <- outliers$pct_change[i]
            
            cat("County", outlier_name, "shows", round(outlier_change, 1), 
                "% change for", var_name, "between 2021 and 2022\n")
            
            # For extreme cases, consider replacing with a trend-based value
            if (abs(outlier_change) > 100) {  # Very extreme changes
              cat("Attempting to correct extreme value for", outlier_name, "\n")
              
              # Get time series for this county
              county_ts <- acs_wide %>%
                filter(GEOID == outlier_county, year >= 2018, year <= 2021) %>%
                arrange(year) %>%
                select(year, !!sym(var_name))
              
              # If we have enough data points, try to correct
              if (nrow(county_ts) >= 3 && !any(is.na(county_ts[[var_name]]))) {
                # Add safe conversion to numeric
                county_values <- county_ts[[var_name]]
                # Convert to numeric if it's not already
                if (!is.numeric(county_values)) {
                  county_values <- as.numeric(as.character(county_values))
                }
                
                # Ensure we have valid numeric values
                if (!any(is.na(county_values))) {
                  # Simple trend estimation - mean annual change over previous years
                  annual_changes <- diff(county_values)
                  avg_annual_change <- mean(annual_changes)
                  
                  # Predict 2022 value based on 2021 plus average annual change
                  predicted_2022 <- county_values[length(county_values)] + avg_annual_change
                  
                  # Replace the extreme value
                  row_idx <- which(acs_wide$year == 2022 & acs_wide$GEOID == outlier_county)
                  if (length(row_idx) > 0) {
                    # Get the actual value safely using double brackets to extract just the value
                    current_val <- tryCatch({
                      as.character(acs_wide[row_idx, var_name][[1]])
                    }, error = function(e) {
                      "unknown value"
                    })
                    cat("Correcting extreme value from", current_val, "to", predicted_2022, "\n")
                    acs_wide[row_idx, var_name] <- predicted_2022
                    acs_wide[row_idx, "source"] <- "ACS (trend-adjusted)"
                  }
                }
              }
            }
          }
        }
      } else {
        cat("No extreme outliers found for", var_name, "\n")
      }
    }
  }
  
  # Special handling for 2022 ACS dataset validation
  if (any(acs_wide$year == 2022)) {
    # Check if we have a reasonable number of counties for 2022
    expected_county_count <- acs_wide %>% 
      filter(year == 2021) %>% 
      nrow()
    
    actual_county_count <- acs_wide %>% 
      filter(year == 2022) %>% 
      nrow()
    
    if (actual_county_count < (expected_county_count * 0.9)) {
      # We're missing more than 10% of counties compared to 2021
      cat("Warning: 2022 ACS data includes only", actual_county_count, "counties,", 
          "compared to", expected_county_count, "in 2021\n")
      
      # Find which counties are missing in 2022
      missing_counties <- acs_wide %>%
        filter(year == 2021) %>%
        anti_join(acs_wide %>% filter(year == 2022), by = "GEOID") %>%
        select(GEOID, NAME)
      
      if (nrow(missing_counties) > 0) {
        cat("Missing", nrow(missing_counties), "counties in 2022 data. First few:",
            paste(head(missing_counties$NAME, 5), collapse = ", "), "...\n")
        
        # For demonstration, fill in missing counties with 2021 data
        missing_data <- acs_wide %>%
          filter(year == 2021, GEOID %in% missing_counties$GEOID) %>%
          mutate(
            year = 2022,
            source = "ACS (copied from 2021)"
          )
        
        if (nrow(missing_data) > 0) {
          cat("Filling in", nrow(missing_data), "missing counties with 2021 data\n")
          acs_wide <- bind_rows(acs_wide, missing_data)
        }
      }
    }
    
    # Final validation of 2022 ACS dataset
    acs_2022_final <- acs_wide %>% filter(year == 2022)
    cat("Final 2022 ACS dataset contains", nrow(acs_2022_final), "counties\n")
    
    # Report on imputation and corrections made
    data_sources <- table(acs_2022_final$source)
    cat("2022 ACS data sources:\n")
    print(data_sources)
  }
  
  cat("Finished all fetches.\n")
  return(list(decennial = decennial_wide, pep = pep_wide, acs = acs_wide))
}