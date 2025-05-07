#!/usr/bin/env Rscript

#' Complete Traffic Safety Testing Suite
#' 
#' This script provides a comprehensive testing suite for all traffic safety components:
#' 1. Data fetching and ingestion from FARS and CDC sources
#' 2. Data validation and quality checking
#' 3. Database integration and storage
#' 4. Temporal interpolation and forecasting
#' 5. Geospatial analysis and mapping
#' 6. Full pipeline integration
#' 7. Dashboard and visualization components 
#'
#' Run this script to verify the complete implementation before deployment.

# Initialize ----
cat("\n========================================================================\n")
cat("TRAFFIC SAFETY COMPLETE TESTING SUITE")
cat("\n========================================================================\n\n")

# Set working directory to project root if run from command line
script_path <- commandArgs(trailingOnly = FALSE)
script_path <- sub("--file=", "", script_path[grep("--file=", script_path)])
if (length(script_path) > 0) {
  script_dir <- dirname(script_path)
  setwd(script_dir)
  cat("Working directory set to:", getwd(), "\n\n")
}

# Create test directory structure
test_dir <- "output/test_traffic_safety_complete"
test_cache_dir <- file.path(test_dir, "cache")
test_log_file <- file.path(test_dir, "test_log.txt")
test_results_dir <- file.path(test_dir, "results")

# Create directories if they don't exist
for (dir_path in c(test_dir, test_cache_dir, test_results_dir)) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    cat("Created directory:", dir_path, "\n")
  }
}

# Initialize log file
cat("", file = test_log_file, append = FALSE)

# Test configuration
test_config <- list(
  years = 2018:2020,  # Limited year range for quicker tests
  test_county = "06037",  # Los Angeles County
  db_path = file.path(test_dir, "traffic_safety_test.duckdb"),
  timeout = 300,  # Default timeout in seconds (5 minutes)
  check_all_counties = FALSE,  # Set to TRUE for exhaustive testing (slow)
  generate_maps = TRUE,
  run_dashboard = FALSE  # Set to TRUE to test dashboard launch (blocks execution)
)

# Logging function
log_test <- function(message, level = "INFO", show_time = TRUE) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  prefix <- if (show_time) paste0("[", timestamp, "] [", level, "] ") else paste0("[", level, "] ")
  formatted_message <- paste0(prefix, message)
  
  # Write to log file
  cat(formatted_message, "\n", file = test_log_file, append = TRUE)
  
  # Apply color to console output based on level
  color <- switch(level,
                  "INFO" = "\033[0m",      # default
                  "SUCCESS" = "\033[32m",  # green
                  "WARN" = "\033[33m",     # yellow
                  "ERROR" = "\033[31m",    # red
                  "\033[0m")               # default fallback
  
  cat(color, formatted_message, "\033[0m", "\n", sep = "")
  
  # Force output to display immediately
  flush.console()
}

# Test timing function
run_timed_test <- function(name, test_func) {
  separator <- paste(rep("-", 80), collapse = "")
  cat("\n", separator, "\n", sep = "")
  log_test(paste("STARTING TEST:", name))
  
  start_time <- Sys.time()
  
  result <- tryCatch({
    # Execute the test function
    test_result <- test_func()
    
    # Calculate elapsed time
    end_time <- Sys.time()
    elapsed <- difftime(end_time, start_time, units = "secs")
    
    # Determine status
    if (is.list(test_result) && "status" %in% names(test_result)) {
      status <- test_result$status
    } else {
      status <- !is.null(test_result)
    }
    
    if (status) {
      log_test(paste("TEST PASSED:", name, "in", round(elapsed, 2), "seconds"), "SUCCESS")
    } else {
      message <- if (is.list(test_result) && "message" %in% names(test_result)) test_result$message else "Test failed with no message"
      log_test(paste("TEST FAILED:", name, "in", round(elapsed, 2), "seconds"), "ERROR")
      log_test(paste("Failure reason:", message), "ERROR")
    }
    
    # Return complete result with timing
    list(
      name = name,
      status = status,
      time = as.numeric(elapsed),
      results = test_result
    )
  }, error = function(e) {
    # Handle errors
    end_time <- Sys.time()
    elapsed <- difftime(end_time, start_time, units = "secs")
    
    log_test(paste("TEST ERROR:", name, "in", round(elapsed, 2), "seconds"), "ERROR")
    log_test(paste("Error:", conditionMessage(e)), "ERROR")
    
    # Return error result
    list(
      name = name,
      status = FALSE,
      time = as.numeric(elapsed),
      results = list(
        status = FALSE,
        message = conditionMessage(e),
        error = e
      )
    )
  })
  
  cat(separator, "\n", sep = "")
  
  return(result)
}

# Track all results
all_results <- list()

# Test 1: Module Loading ----
test_module_loading <- function() {
  log_test("Testing traffic safety module loading")
  
  # List of all traffic safety related modules
  traffic_safety_modules <- c(
    "traffic_safety_integration.r",
    "traffic_safety_validation.r",
    "traffic_safety_cache.r",
    "traffic_safety_forecasting.r",
    "traffic_safety_geospatial.r",
    "traffic_safety_dashboard.r",
    "traffic_safety_api_tests.r"
  )
  
  # Track loaded modules
  loaded_modules <- list()
  integration_loaded <- FALSE
  
  # Try to load each module
  for (module in traffic_safety_modules) {
    module_exists <- file.exists(module)
    
    if (module_exists) {
      result <- tryCatch({
        source(module)
        log_test(paste("Successfully loaded module:", module), "SUCCESS")
        loaded_modules[[module]] <- TRUE
        
        if (module == "traffic_safety_integration.r") {
          integration_loaded <- TRUE
        }
        
        TRUE
      }, error = function(e) {
        log_test(paste("Error loading module:", module, "-", conditionMessage(e)), "ERROR")
        loaded_modules[[module]] <- FALSE
        FALSE
      })
    } else {
      log_test(paste("Module not found:", module), "WARN")
      loaded_modules[[module]] <- FALSE
    }
  }
  
  # Count loaded modules
  num_loaded <- sum(unlist(loaded_modules))
  num_critical <- sum(loaded_modules[["traffic_safety_integration.r"]] == TRUE)
  
  # Check for integration module and key functions
  if (integration_loaded) {
    log_test("Main integration module loaded successfully", "SUCCESS")
    
    # Check for key functions
    key_functions <- c(
      "get_traffic_safety_data",
      "get_traffic_safety_variable_names",
      "validate_traffic_safety_data",
      "process_traffic_safety_data"
    )
    
    function_exists <- sapply(key_functions, exists)
    
    for (i in seq_along(key_functions)) {
      status <- if (function_exists[i]) "EXISTS" else "MISSING"
      level <- if (function_exists[i]) "SUCCESS" else "WARN"
      log_test(paste("Function check:", key_functions[i], "-", status), level)
    }
    
    # Check if all required functions exist
    critical_functions_exist <- all(function_exists[1:2])  # First two are critical
  } else {
    log_test("Critical traffic_safety_integration.r module not loaded", "ERROR")
    critical_functions_exist <- FALSE
  }
  
  # Overall status
  test_passed <- integration_loaded && (num_loaded >= 1) && critical_functions_exist
  
  # Return results
  return(list(
    status = test_passed,
    message = if (test_passed) 
      paste("Successfully loaded", num_loaded, "traffic safety modules")
    else 
      "Failed to load all required traffic safety modules",
    details = list(
      modules_loaded = loaded_modules,
      integration_loaded = integration_loaded,
      num_loaded = num_loaded
    )
  ))
}

# Run module loading test
all_results$module_loading <- run_timed_test("Module Loading", test_module_loading)

# Continuing only if integration module loaded
if (!all_results$module_loading$status) {
  log_test("Critical integration module loading failed. Cannot continue tests.", "ERROR")
  quit(status = 1)
}

# Test 2: Data Fetching and Processing ----
test_data_fetching <- function() {
  log_test("Testing traffic safety data fetching and processing")
  
  # Check if required function exists
  if (!exists("get_traffic_safety_data")) {
    return(list(
      status = FALSE,
      message = "get_traffic_safety_data function not found",
      details = NULL
    ))
  }
  
  # Test data fetching
  log_test(paste("Fetching traffic safety data for years:", paste(test_config$years, collapse = ", ")))
  
  traffic_data <- tryCatch({
    get_traffic_safety_data(
      years = test_config$years,
      refresh = TRUE
    )
  }, error = function(e) {
    log_test(paste("Error fetching traffic safety data:", conditionMessage(e)), "ERROR")
    NULL
  })
  
  if (is.null(traffic_data) || nrow(traffic_data) == 0) {
    return(list(
      status = FALSE,
      message = "Failed to retrieve traffic safety data",
      details = NULL
    ))
  }
  
  # Log data retrieval success
  log_test(paste("Successfully retrieved", nrow(traffic_data), "traffic safety data records"), "SUCCESS")
  log_test(paste("Data covers", length(unique(traffic_data$geoid)), "counties across", 
                 length(unique(traffic_data$year)), "years"), "INFO")
  
  # Check for required columns
  required_variables <- c("geoid", "year", "traffic_fatalities", "traffic_fatality_rate")
  missing_variables <- setdiff(required_variables, names(traffic_data))
  
  if (length(missing_variables) > 0) {
    return(list(
      status = FALSE,
      message = paste("Missing required variables in traffic safety data:", 
                     paste(missing_variables, collapse = ", ")),
      details = list(available_variables = names(traffic_data))
    ))
  }
  
  # Check variable coverage
  all_variables <- get_traffic_safety_variable_names()
  found_variables <- intersect(all_variables, names(traffic_data))
  
  log_test(paste("Data includes", length(found_variables), "of", length(all_variables), 
                "expected traffic safety variables"), 
          if(length(found_variables) == length(all_variables)) "SUCCESS" else "WARN")
  
  # Check for test county
  test_county_data <- traffic_data[traffic_data$geoid == test_config$test_county, ]
  
  if (nrow(test_county_data) == 0) {
    log_test(paste("Test county", test_config$test_county, "not found in data"), "WARN")
  } else {
    log_test(paste("Test county data found with", nrow(test_county_data), "records"), "SUCCESS")
  }
  
  # Save data for other tests
  saveRDS(traffic_data, file.path(test_cache_dir, "traffic_data.rds"))
  
  # Return success
  return(list(
    status = TRUE,
    message = paste("Successfully fetched and processed traffic safety data for", 
                   length(unique(traffic_data$year)), "years"),
    details = list(
      rows = nrow(traffic_data),
      counties = length(unique(traffic_data$geoid)),
      years = sort(unique(traffic_data$year)),
      variables = found_variables
    )
  ))
}

# Run data fetching test
all_results$data_fetching <- run_timed_test("Data Fetching", test_data_fetching)

# Continue only if data fetching succeeded
if (!all_results$data_fetching$status) {
  log_test("Data fetching failed. Cannot continue tests.", "ERROR")
  quit(status = 1)
}

# Test 3: Data Validation ----
test_data_validation <- function() {
  log_test("Testing traffic safety data validation")
  
  # Load test data
  traffic_data_path <- file.path(test_cache_dir, "traffic_data.rds")
  
  if (!file.exists(traffic_data_path)) {
    return(list(
      status = FALSE,
      message = "Traffic safety data not found. Run data fetching test first.",
      details = NULL
    ))
  }
  
  traffic_data <- readRDS(traffic_data_path)
  
  # Check if validation function exists
  if (!exists("validate_traffic_safety_data")) {
    log_test("validate_traffic_safety_data function not found", "WARN")
    
    # Manual basic validation
    log_test("Performing basic manual validation instead", "INFO")
    
    # Check for missing values in key columns
    missing_counts <- sapply(traffic_data[c("geoid", "year", "traffic_fatalities")], 
                            function(x) sum(is.na(x)))
    
    all_valid <- all(missing_counts == 0)
    
    if (all_valid) {
      log_test("Basic validation passed - no missing values in key columns", "SUCCESS")
    } else {
      log_test("Basic validation failed - found missing values in key columns", "ERROR")
      for (col in names(missing_counts)) {
        if (missing_counts[col] > 0) {
          log_test(paste(col, "has", missing_counts[col], "missing values"), "ERROR")
        }
      }
    }
    
    return(list(
      status = all_valid,
      message = if (all_valid) "Basic validation passed" else "Basic validation failed",
      details = list(missing_counts = missing_counts)
    ))
  }
  
  # Run validation
  log_test("Running comprehensive data validation...")
  
  validation_result <- tryCatch({
    validate_traffic_safety_data(
      traffic_data,
      auto_fix = TRUE,
      verbose = TRUE
    )
  }, error = function(e) {
    log_test(paste("Error in validation:", conditionMessage(e)), "ERROR")
    NULL
  })
  
  if (is.null(validation_result)) {
    return(list(
      status = FALSE,
      message = "Data validation function failed",
      details = NULL
    ))
  }
  
  # Check validation results
  if (validation_result$valid) {
    log_test("Validation PASSED: All criteria met or issues fixed", "SUCCESS")
  } else {
    log_test(paste("Validation FAILED:", length(validation_result$issues), "issues found"), "ERROR")
    
    # Log issues
    for (i in seq_along(validation_result$issues)) {
      log_test(paste("Issue", i, ":", validation_result$issues[i]), "ERROR")
    }
  }
  
  # Save validation results
  saveRDS(validation_result, file.path(test_results_dir, "validation_results.rds"))
  
  # If auto-fix is enabled, save the fixed data
  if (validation_result$fixed) {
    log_test(paste("Fixed", validation_result$fixes_applied, "issues in the data"), "SUCCESS")
    saveRDS(validation_result$fixed_data, file.path(test_cache_dir, "traffic_data_fixed.rds"))
  }
  
  return(list(
    status = validation_result$valid,
    message = if(validation_result$valid) 
      "Data validation passed successfully" 
    else 
      paste("Data validation failed with", length(validation_result$issues), "issues"),
    details = validation_result
  ))
}

# Run data validation test
all_results$data_validation <- run_timed_test("Data Validation", test_data_validation)

# Test 4: Database Integration ----
test_database_integration <- function() {
  log_test("Testing traffic safety database integration")
  
  # Load test data (fixed if available, otherwise original)
  fixed_data_path <- file.path(test_cache_dir, "traffic_data_fixed.rds")
  original_data_path <- file.path(test_cache_dir, "traffic_data.rds")
  
  data_path <- if (file.exists(fixed_data_path)) fixed_data_path else original_data_path
  
  if (!file.exists(data_path)) {
    return(list(
      status = FALSE,
      message = "Traffic safety data not found. Run data fetching test first.",
      details = NULL
    ))
  }
  
  traffic_data <- readRDS(data_path)
  log_test(paste("Loaded", nrow(traffic_data), "traffic safety records for database test"), "INFO")
  
  # Ensure DuckDB is available
  if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("duckdb", quietly = TRUE)) {
    log_test("Required packages DBI and/or duckdb not found", "ERROR")
    return(list(
      status = FALSE,
      message = "Required packages for database integration not available",
      details = NULL
    ))
  }
  
  # Clean up existing test database if it exists
  if (file.exists(test_config$db_path)) {
    file.remove(test_config$db_path)
    log_test(paste("Removed existing test database:", test_config$db_path), "INFO")
  }
  
  # Create a new test database
  con <- tryCatch({
    DBI::dbConnect(duckdb::duckdb(), dbdir = test_config$db_path)
  }, error = function(e) {
    log_test(paste("Error connecting to database:", conditionMessage(e)), "ERROR")
    NULL
  })
  
  if (is.null(con)) {
    return(list(
      status = FALSE,
      message = "Failed to create test database",
      details = NULL
    ))
  }
  
  # Ensure we disconnect on exit
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  # Create the database schema
  log_test("Creating database schema for traffic safety data")
  
  schema_created <- tryCatch({
    # Create counties table
    DBI::dbExecute(con, "
      CREATE TABLE counties (
        geoid VARCHAR PRIMARY KEY,
        name VARCHAR,
        state_fips VARCHAR,
        state_name VARCHAR
      )
    ")
    
    # Create variables table
    DBI::dbExecute(con, "
      CREATE TABLE variables (
        variable_name VARCHAR PRIMARY KEY,
        description VARCHAR,
        category VARCHAR,
        units VARCHAR,
        source VARCHAR
      )
    ")
    
    # Create sdoh_data table with traffic safety data
    DBI::dbExecute(con, "
      CREATE TABLE sdoh_data (
        geoid VARCHAR,
        year INTEGER,
        variable_name VARCHAR,
        value DOUBLE,
        data_quality VARCHAR,
        PRIMARY KEY (geoid, year, variable_name)
      )
    ")
    
    TRUE
  }, error = function(e) {
    log_test(paste("Error creating database schema:", conditionMessage(e)), "ERROR")
    FALSE
  })
  
  if (!schema_created) {
    return(list(
      status = FALSE,
      message = "Failed to create database schema",
      details = NULL
    ))
  }
  
  log_test("Database schema created successfully", "SUCCESS")
  
  # Create list of unique counties from the data
  log_test("Adding county metadata to database")
  
  counties_added <- tryCatch({
    # Extract unique counties
    counties <- unique(traffic_data$geoid)
    
    # Create a simplified county data frame
    county_data <- data.frame(
      geoid = counties,
      name = paste("County", counties),  # Placeholder names
      state_fips = substr(counties, 1, 2),
      state_name = paste("State", substr(counties, 1, 2))
    )
    
    # Add to database
    DBI::dbWriteTable(con, "counties", county_data, append = TRUE)
    
    nrow(county_data)
  }, error = function(e) {
    log_test(paste("Error adding counties to database:", conditionMessage(e)), "ERROR")
    0
  })
  
  if (counties_added == 0) {
    log_test("Failed to add counties to database", "ERROR")
  } else {
    log_test(paste("Added", counties_added, "counties to database"), "SUCCESS")
  }
  
  # Add traffic safety variables
  log_test("Adding traffic safety variables metadata to database")
  
  vars_added <- tryCatch({
    # Get variable names
    traffic_vars <- get_traffic_safety_variable_names()
    
    # Create variables metadata
    var_data <- data.frame(
      variable_name = traffic_vars,
      description = sapply(traffic_vars, function(v) gsub("_", " ", tools::toTitleCase(v))),
      category = "Traffic Safety",
      units = ifelse(grepl("rate", traffic_vars), "per 100,000 population", "count"),
      source = "FARS/NHTSA"
    )
    
    # Add to database
    DBI::dbWriteTable(con, "variables", var_data, append = TRUE)
    
    nrow(var_data)
  }, error = function(e) {
    log_test(paste("Error adding variables to database:", conditionMessage(e)), "ERROR")
    0
  })
  
  if (vars_added == 0) {
    log_test("Failed to add variables to database", "ERROR")
  } else {
    log_test(paste("Added", vars_added, "traffic safety variables to database"), "SUCCESS")
  }
  
  # Transform and add traffic safety data
  log_test("Adding traffic safety data to sdoh_data table")
  
  data_rows_added <- tryCatch({
    rows_added <- 0
    
    # Process each variable
    traffic_vars <- get_traffic_safety_variable_names()
    traffic_vars <- intersect(traffic_vars, names(traffic_data))
    
    for (var in traffic_vars) {
      # Skip if variable doesn't exist
      if (!var %in% names(traffic_data)) next
      
      # Create long-format data for this variable
      var_data <- traffic_data[, c("geoid", "year", var)]
      
      # Skip completely missing variables
      if (all(is.na(var_data[[var]]))) {
        log_test(paste("Skipping completely missing variable:", var), "WARN")
        next
      }
      
      # Check for data quality column
      quality_col <- paste0("data_quality_", var)
      has_quality <- quality_col %in% names(traffic_data)
      
      if (has_quality) {
        quality_data <- traffic_data[[quality_col]]
      } else {
        quality_data <- rep("direct", nrow(var_data))
      }
      
      # Create data frame for database
      db_data <- data.frame(
        geoid = var_data$geoid,
        year = var_data$year,
        variable_name = rep(var, nrow(var_data)),
        value = var_data[[var]],
        data_quality = quality_data
      )
      
      # Remove NA values
      db_data <- db_data[!is.na(db_data$value), ]
      
      # Add to database
      if (nrow(db_data) > 0) {
        DBI::dbWriteTable(con, "sdoh_data", db_data, append = TRUE)
        rows_added <- rows_added + nrow(db_data)
        log_test(paste("Added", nrow(db_data), "data points for variable:", var), "INFO")
      }
    }
    
    rows_added
  }, error = function(e) {
    log_test(paste("Error adding data to database:", conditionMessage(e)), "ERROR")
    0
  })
  
  if (data_rows_added == 0) {
    log_test("Failed to add traffic safety data to database", "ERROR")
    return(list(
      status = FALSE,
      message = "Failed to add traffic safety data to database",
      details = NULL
    ))
  } else {
    log_test(paste("Added", data_rows_added, "traffic safety data rows to database"), "SUCCESS")
  }
  
  # Verify data with a query
  log_test("Verifying database with test queries")
  
  verification_success <- tryCatch({
    # Check counties
    county_count <- DBI::dbGetQuery(con, "SELECT COUNT(*) FROM counties")[1, 1]
    log_test(paste("Database contains", county_count, "counties"), "INFO")
    
    # Check variables
    var_count <- DBI::dbGetQuery(con, "SELECT COUNT(*) FROM variables")[1, 1]
    log_test(paste("Database contains", var_count, "variables"), "INFO")
    
    # Check data
    data_count <- DBI::dbGetQuery(con, "SELECT COUNT(*) FROM sdoh_data")[1, 1]
    log_test(paste("Database contains", data_count, "data points"), "INFO")
    
    # Test a summary query
    summary_query <- "
      SELECT 
        variable_name, 
        year, 
        COUNT(*) as county_count,
        AVG(value) as avg_value,
        MIN(value) as min_value,
        MAX(value) as max_value
      FROM sdoh_data
      GROUP BY variable_name, year
      ORDER BY variable_name, year
    "
    
    summary <- DBI::dbGetQuery(con, summary_query)
    log_test(paste("Successfully executed summary query with", nrow(summary), "rows"), "SUCCESS")
    
    # Save summary to results
    write.csv(summary, file.path(test_results_dir, "database_summary.csv"), row.names = FALSE)
    
    # Check for specific test county
    test_county_query <- sprintf("
      SELECT * FROM sdoh_data 
      WHERE geoid = '%s'
      LIMIT 10
    ", test_config$test_county)
    
    test_county_data <- DBI::dbGetQuery(con, test_county_query)
    
    if (nrow(test_county_data) > 0) {
      log_test(paste("Successfully retrieved test county data from database"), "SUCCESS")
    } else {
      log_test(paste("Test county data not found in database"), "WARN")
    }
    
    # Return verification status
    county_count > 0 && var_count > 0 && data_count > 0
  }, error = function(e) {
    log_test(paste("Error verifying database:", conditionMessage(e)), "ERROR")
    FALSE
  })
  
  return(list(
    status = verification_success,
    message = if (verification_success) 
      paste("Successfully integrated", data_rows_added, "traffic safety data points into database") 
    else
      "Database verification failed",
    details = list(
      db_path = test_config$db_path,
      counties_added = counties_added,
      variables_added = vars_added,
      data_rows_added = data_rows_added
    )
  ))
}

# Run database integration test
all_results$database_integration <- run_timed_test("Database Integration", test_database_integration)

# Test 5: Geospatial Analysis ----
test_geospatial <- function() {
  log_test("Testing traffic safety geospatial analysis")
  
  # Load test data
  data_path <- file.path(test_cache_dir, "traffic_data.rds")
  
  if (!file.exists(data_path)) {
    return(list(
      status = FALSE,
      message = "Traffic safety data not found. Run data fetching test first.",
      details = NULL
    ))
  }
  
  traffic_data <- readRDS(data_path)
  
  # Check if geospatial functions are available
  if (!exists("traffic_safety_geospatial.r") && !file.exists("traffic_safety_geospatial.r")) {
    log_test("traffic_safety_geospatial.r module not found", "WARN")
    
    # Check if sf package is available
    if (!requireNamespace("sf", quietly = TRUE)) {
      log_test("sf package required for geospatial analysis not found", "ERROR")
      return(list(
        status = FALSE,
        message = "Required geospatial packages not found",
        details = NULL
      ))
    }
    
    # Simple geospatial function to generate a map
    log_test("Creating basic map visualization")
    
    map_created <- tryCatch({
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        log_test("ggplot2 package required for mapping not found", "ERROR")
        return(FALSE)
      }
      
      # Get latest year data
      latest_year <- max(traffic_data$year)
      year_data <- traffic_data[traffic_data$year == latest_year, ]
      
      # Try to get county shapefile
      if (requireNamespace("tigris", quietly = TRUE)) {
        log_test("Downloading county shapefile using tigris package", "INFO")
        
        counties_sf <- tigris::counties(cb = TRUE, year = 2020)
        
        # Join with traffic data
        counties_sf$GEOID <- as.character(counties_sf$GEOID)
        map_data <- merge(counties_sf, 
                         year_data, 
                         by.x = "GEOID", 
                         by.y = "geoid", 
                         all.x = TRUE)
        
        # Create a map
        map_file <- file.path(test_results_dir, "traffic_fatalities_map.png")
        
        # Make the plot
        p <- ggplot2::ggplot(map_data) +
          ggplot2::geom_sf(ggplot2::aes(fill = traffic_fatality_rate)) +
          ggplot2::scale_fill_viridis_c(name = "Fatality Rate\nper 100,000", na.value = "grey90") +
          ggplot2::labs(
            title = paste("Traffic Fatality Rate by County,", latest_year),
            caption = "Data source: FARS/NHTSA"
          ) +
          ggplot2::theme_minimal()
        
        # Save map
        ggplot2::ggsave(map_file, p, width = 10, height = 7, dpi = 150)
        
        log_test(paste("Created basic traffic safety map at:", map_file), "SUCCESS")
        TRUE
      } else {
        log_test("tigris package required for mapping not found", "ERROR")
        FALSE
      }
    }, error = function(e) {
      log_test(paste("Error creating map:", conditionMessage(e)), "ERROR")
      FALSE
    })
    
    return(list(
      status = map_created,
      message = if (map_created) 
        "Created basic traffic safety map" 
      else 
        "Failed to create traffic safety map",
      details = NULL
    ))
  }
  
  # If geospatial module exists, load it
  geospatial_loaded <- tryCatch({
    source("traffic_safety_geospatial.r")
    log_test("Successfully loaded traffic_safety_geospatial.r module", "SUCCESS")
    TRUE
  }, error = function(e) {
    log_test(paste("Error loading geospatial module:", conditionMessage(e)), "ERROR")
    FALSE
  })
  
  if (!geospatial_loaded) {
    return(list(
      status = FALSE,
      message = "Failed to load geospatial module",
      details = NULL
    ))
  }
  
  # Check for the core geospatial analysis function
  if (!exists("analyze_traffic_safety_spatial")) {
    log_test("analyze_traffic_safety_spatial function not found", "ERROR")
    return(list(
      status = FALSE,
      message = "Required geospatial function not found",
      details = NULL
    ))
  }
  
  # Run spatial analysis
  log_test("Running traffic safety spatial analysis")
  
  spatial_result <- tryCatch({
    # Get latest year
    latest_year <- max(traffic_data$year)
    
    # Run analysis
    analyze_traffic_safety_spatial(
      data = traffic_data,
      variable = "traffic_fatality_rate",
      year = latest_year,
      output_dir = test_results_dir
    )
  }, error = function(e) {
    log_test(paste("Error in spatial analysis:", conditionMessage(e)), "ERROR")
    NULL
  })
  
  if (is.null(spatial_result)) {
    return(list(
      status = FALSE,
      message = "Spatial analysis failed",
      details = NULL
    ))
  }
  
  log_test("Spatial analysis completed successfully", "SUCCESS")
  
  # Check for hotspot identification
  hotspot_result <- NULL
  
  if (exists("identify_traffic_safety_hotspots")) {
    log_test("Running hotspot identification")
    
    hotspot_result <- tryCatch({
      # Get latest year
      latest_year <- max(traffic_data$year)
      
      # Run hotspot analysis
      identify_traffic_safety_hotspots(
        data = traffic_data,
        variable = "traffic_fatality_rate",
        year = latest_year,
        output_dir = test_results_dir
      )
    }, error = function(e) {
      log_test(paste("Error in hotspot identification:", conditionMessage(e)), "ERROR")
      NULL
    })
    
    if (!is.null(hotspot_result)) {
      log_test("Hotspot identification completed successfully", "SUCCESS")
    }
  }
  
  # Check if map was created
  map_files <- list.files(test_results_dir, pattern = "traffic.*map\\.png$", full.names = TRUE)
  
  if (length(map_files) > 0) {
    log_test(paste("Found", length(map_files), "map files in results directory"), "SUCCESS")
  } else {
    log_test("No map files found in results directory", "WARN")
  }
  
  return(list(
    status = TRUE,
    message = "Geospatial analysis completed successfully",
    details = list(
      spatial_analysis = !is.null(spatial_result),
      hotspot_analysis = !is.null(hotspot_result),
      maps_created = length(map_files)
    )
  ))
}

# Run geospatial analysis test if enabled
if (test_config$generate_maps) {
  all_results$geospatial <- run_timed_test("Geospatial Analysis", test_geospatial)
}

# Test 6: Forecasting ----
test_forecasting <- function() {
  log_test("Testing traffic safety forecasting")
  
  # Load test data
  data_path <- file.path(test_cache_dir, "traffic_data.rds")
  
  if (!file.exists(data_path)) {
    return(list(
      status = FALSE,
      message = "Traffic safety data not found. Run data fetching test first.",
      details = NULL
    ))
  }
  
  traffic_data <- readRDS(data_path)
  
  # Check if forecasting module exists
  if (!exists("traffic_safety_forecasting.r") && !file.exists("traffic_safety_forecasting.r")) {
    log_test("traffic_safety_forecasting.r module not found", "WARN")
    
    # Try basic forecasting using stats package
    log_test("Attempting simple time series forecasting")
    
    forecast_result <- tryCatch({
      # Get national level data
      national_data <- traffic_data %>%
        dplyr::group_by(year) %>%
        dplyr::summarize(
          traffic_fatalities = sum(traffic_fatalities, na.rm = TRUE),
          population = sum(population, na.rm = TRUE),
          traffic_fatality_rate = (traffic_fatalities / population) * 100000
        )
      
      # Create time series
      ts_data <- stats::ts(national_data$traffic_fatality_rate, 
                          start = min(national_data$year),
                          frequency = 1)
      
      # Simple exponential smoothing
      forecast_model <- stats::HoltWinters(ts_data)
      
      # Forecast 3 years ahead
      forecast_years <- 3
      forecast_values <- stats::predict(forecast_model, n.ahead = forecast_years)
      
      # Create forecast data frame
      forecast_df <- data.frame(
        year = seq(max(national_data$year) + 1, length.out = forecast_years),
        traffic_fatality_rate = as.numeric(forecast_values),
        forecast_type = "Simple exponential smoothing"
      )
      
      # Save forecast
      write.csv(forecast_df, file.path(test_results_dir, "simple_forecast.csv"), row.names = FALSE)
      
      log_test("Created simple national forecast for 3 years", "SUCCESS")
      TRUE
    }, error = function(e) {
      log_test(paste("Error in simple forecasting:", conditionMessage(e)), "ERROR")
      FALSE
    })
    
    return(list(
      status = forecast_result,
      message = if (forecast_result) 
        "Created simple national forecast" 
      else 
        "Failed to create forecast",
      details = NULL
    ))
  }
  
  # If forecasting module exists, load it
  forecasting_loaded <- tryCatch({
    source("traffic_safety_forecasting.r")
    log_test("Successfully loaded traffic_safety_forecasting.r module", "SUCCESS")
    TRUE
  }, error = function(e) {
    log_test(paste("Error loading forecasting module:", conditionMessage(e)), "ERROR")
    FALSE
  })
  
  if (!forecasting_loaded) {
    return(list(
      status = FALSE,
      message = "Failed to load forecasting module",
      details = NULL
    ))
  }
  
  # Check for the core forecasting function
  if (!exists("generate_traffic_forecast")) {
    log_test("generate_traffic_forecast function not found", "ERROR")
    return(list(
      status = FALSE,
      message = "Required forecasting function not found",
      details = NULL
    ))
  }
  
  # Run forecasting
  log_test("Running traffic safety forecasting")
  
  forecast_result <- tryCatch({
    # Generate forecast
    generate_traffic_forecast(
      data = traffic_data,
      forecast_years = 3,  # 3 years ahead
      variable = "traffic_fatality_rate",
      output_dir = test_results_dir
    )
  }, error = function(e) {
    log_test(paste("Error in forecasting:", conditionMessage(e)), "ERROR")
    NULL
  })
  
  if (is.null(forecast_result)) {
    return(list(
      status = FALSE,
      message = "Forecasting failed",
      details = NULL
    ))
  }
  
  log_test("Forecasting completed successfully", "SUCCESS")
  log_test(paste("Generated forecasts for", forecast_result$years_ahead, "years ahead"))
  
  # Check if visualization was created
  viz_files <- list.files(test_results_dir, pattern = "forecast.*\\.(png|pdf)$", full.names = TRUE)
  
  if (length(viz_files) > 0) {
    log_test(paste("Found", length(viz_files), "forecast visualization files"), "SUCCESS")
  } else {
    log_test("No forecast visualization files found", "WARN")
  }
  
  return(list(
    status = TRUE,
    message = "Forecasting completed successfully",
    details = list(
      forecast_years = forecast_result$years_ahead,
      visualization_files = length(viz_files)
    )
  ))
}

# Run forecasting test
all_results$forecasting <- run_timed_test("Forecasting", test_forecasting)

# Test 7: Pipeline Integration ----
test_pipeline_integration <- function() {
  log_test("Testing traffic safety integration with unified pipeline")
  
  # Check if unified pipeline exists
  if (!file.exists("unified_sdoh_pipeline.r")) {
    log_test("unified_sdoh_pipeline.r not found", "WARN")
    return(list(
      status = FALSE,
      message = "Unified pipeline file not found",
      details = NULL
    ))
  }
  
  # Check if traffic safety is referenced in the pipeline
  log_test("Checking unified pipeline for traffic safety references")
  
  pipeline_content <- readLines("unified_sdoh_pipeline.r")
  traffic_safety_lines <- grep("traffic_safety", pipeline_content)
  
  if (length(traffic_safety_lines) == 0) {
    log_test("No traffic safety references found in unified pipeline", "ERROR")
    return(list(
      status = FALSE,
      message = "Traffic safety not found in unified pipeline",
      details = NULL
    ))
  }
  
  log_test(paste("Found", length(traffic_safety_lines), "references to traffic safety in pipeline"), "SUCCESS")
  
  # Find important integration lines
  integration_points <- list(
    module_load = grep("source.*traffic_safety.*\\.r", pipeline_content),
    function_call = grep("get_traffic_safety_data", pipeline_content),
    database_integration = grep("traffic_safety.*database|database.*traffic_safety", pipeline_content)
  )
  
  # Report integration points
  for (point_name in names(integration_points)) {
    point_lines <- integration_points[[point_name]]
    if (length(point_lines) > 0) {
      log_test(paste("Found", length(point_lines), "references to", point_name), "SUCCESS")
      
      # Show a sample
      sample_line <- pipeline_content[point_lines[1]]
      log_test(paste("Sample line:", sample_line), "INFO")
    } else {
      log_test(paste("No references found for", point_name), "WARN")
    }
  }
  
  # Check for key integration steps
  all_required_found <- length(integration_points$module_load) > 0 && 
                        length(integration_points$function_call) > 0
  
  # Check if traffic safety data is included in the database
  if (file.exists(test_config$db_path)) {
    log_test("Checking if traffic safety data is in the test database")
    
    database_has_ts <- tryCatch({
      con <- DBI::dbConnect(duckdb::duckdb(), dbdir = test_config$db_path)
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
      
      # Check if sdoh_data table exists
      if (!DBI::dbExistsTable(con, "sdoh_data")) {
        return(FALSE)
      }
      
      # Check for traffic safety variables
      ts_vars <- DBI::dbGetQuery(con, "
        SELECT variable_name FROM variables 
        WHERE category = 'Traffic Safety' 
        OR variable_name LIKE '%traffic%' 
        OR variable_name LIKE '%fatality%'
      ")
      
      if (nrow(ts_vars) == 0) {
        return(FALSE)
      }
      
      # Check if there's actual data
      var_list <- paste("'", ts_vars$variable_name, "'", collapse = ",", sep = "")
      
      ts_data_count <- DBI::dbGetQuery(con, paste0("
        SELECT COUNT(*) FROM sdoh_data 
        WHERE variable_name IN (", var_list, ")"
      ))[1, 1]
      
      ts_data_count > 0
    }, error = function(e) {
      log_test(paste("Error checking database:", conditionMessage(e)), "ERROR")
      FALSE
    })
    
    if (database_has_ts) {
      log_test("Traffic safety data found in test database", "SUCCESS")
    } else {
      log_test("Traffic safety data not found in test database", "WARN")
    }
  }
  
  return(list(
    status = all_required_found,
    message = if (all_required_found) 
      "Traffic safety is properly integrated with unified pipeline" 
    else 
      "Traffic safety integration with pipeline is incomplete",
    details = list(
      integration_points = sapply(integration_points, length),
      database_has_ts = if (exists("database_has_ts")) database_has_ts else NA
    )
  ))
}

# Run pipeline integration test
all_results$pipeline_integration <- run_timed_test("Pipeline Integration", test_pipeline_integration)

# Test 8: Dashboard ----
test_dashboard <- function() {
  log_test("Testing traffic safety dashboard functionality")
  
  # Check if dashboard file exists
  if (!exists("traffic_safety_dashboard.r") && !file.exists("traffic_safety_dashboard.r")) {
    log_test("traffic_safety_dashboard.r not found", "WARN")
    return(list(
      status = FALSE,
      message = "Dashboard file not found",
      details = NULL
    ))
  }
  
  # Check for required packages
  dashboard_packages <- c("shiny", "shinydashboard", "plotly", "DT")
  missing_packages <- dashboard_packages[!sapply(dashboard_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    log_test(paste("Required dashboard packages not found:", paste(missing_packages, collapse = ", ")), "WARN")
    return(list(
      status = FALSE,
      message = paste("Required dashboard packages not available:", paste(missing_packages, collapse = ", ")),
      details = list(missing_packages = missing_packages)
    ))
  }
  
  # Try to source the dashboard file without running it
  dashboard_loaded <- tryCatch({
    env <- new.env()
    sys.source("traffic_safety_dashboard.r", envir = env)
    log_test("Successfully loaded dashboard code", "SUCCESS")
    TRUE
  }, error = function(e) {
    log_test(paste("Error loading dashboard:", conditionMessage(e)), "ERROR")
    FALSE
  })
  
  if (!dashboard_loaded) {
    return(list(
      status = FALSE,
      message = "Failed to load dashboard code",
      details = NULL
    ))
  }
  
  # Check for launch function
  if (!exists("launch_traffic_safety_dashboard")) {
    log_test("launch_traffic_safety_dashboard function not found", "ERROR")
    return(list(
      status = FALSE,
      message = "Dashboard launch function not found",
      details = NULL
    ))
  }
  
  # Check function arguments
  func_args <- names(formals(launch_traffic_safety_dashboard))
  log_test(paste("Dashboard function takes arguments:", paste(func_args, collapse = ", ")), "INFO")
  
  # If configured, try to launch the dashboard
  if (test_config$run_dashboard) {
    log_test("Attempting to launch dashboard")
    
    # Load test data
    data_path <- file.path(test_cache_dir, "traffic_data.rds")
    
    if (!file.exists(data_path)) {
      return(list(
        status = FALSE,
        message = "Test data not found for dashboard launch",
        details = NULL
      ))
    }
    
    traffic_data <- readRDS(data_path)
    
    # Launch dashboard
    dashboard_launched <- tryCatch({
      launch_traffic_safety_dashboard(
        traffic_data = traffic_data,
        port = 4321,  # Use non-standard port
        launch_browser = TRUE
      )
      
      # We won't reach here unless dashboard is manually closed
      TRUE
    }, error = function(e) {
      log_test(paste("Error launching dashboard:", conditionMessage(e)), "ERROR")
      FALSE
    })
    
    return(list(
      status = dashboard_launched,
      message = if (dashboard_launched) 
        "Dashboard launched successfully" 
      else 
        "Failed to launch dashboard",
      details = NULL
    ))
  } else {
    log_test("Dashboard launch test skipped (not enabled in config)", "INFO")
    return(list(
      status = TRUE,
      message = "Dashboard code loaded successfully (launch skipped)",
      details = list(
        function_exists = TRUE,
        function_args = func_args
      )
    ))
  }
}

# Run dashboard test
all_results$dashboard <- run_timed_test("Dashboard", test_dashboard)

# Test 9: Documentation ----
test_documentation <- function() {
  log_test("Testing traffic safety documentation")
  
  # Check for documentation files
  doc_files <- c(
    "docs/TRAFFIC_SAFETY_DATA.md",
    "docs/data_sources/TRAFFIC_SAFETY_DATA.md",
    "traffic_safety_README.md",
    "docs/traffic_safety_README.md"
  )
  
  found_docs <- sapply(doc_files, file.exists)
  
  if (!any(found_docs)) {
    log_test("No traffic safety documentation files found", "WARN")
    return(list(
      status = FALSE,
      message = "No traffic safety documentation files found",
      details = NULL
    ))
  }
  
  # Check the first found documentation file
  doc_file <- doc_files[which(found_docs)[1]]
  log_test(paste("Found documentation file:", doc_file), "SUCCESS")
  
  # Read documentation content
  doc_content <- readLines(doc_file)
  
  # Check for important content
  doc_checks <- list(
    variables = grep("variable|metric", doc_content, ignore.case = TRUE),
    data_source = grep("source|fars|nhtsa|FARS|NHTSA", doc_content, ignore.case = TRUE),
    methodology = grep("method|approach|calculate|analysis", doc_content, ignore.case = TRUE)
  )
  
  # Report on content
  for (check_name in names(doc_checks)) {
    check_lines <- doc_checks[[check_name]]
    if (length(check_lines) > 0) {
      log_test(paste("Found", length(check_lines), "references to", check_name, "in documentation"), "SUCCESS")
    } else {
      log_test(paste("No references to", check_name, "found in documentation"), "WARN")
    }
  }
  
  # Generate simple documentation if appropriate
  if (!any(sapply(doc_checks, length) > 0)) {
    log_test("Documentation appears incomplete. Generating basic documentation.", "WARN")
    
    # Create a basic documentation file
    basic_doc_file <- file.path(test_results_dir, "TRAFFIC_SAFETY_DATA.md")
    
    basic_doc_content <- c(
      "# Traffic Safety Data",
      "",
      "## Overview",
      "",
      "This document describes the traffic safety data integrated into the Social Determinants of Health database.",
      "",
      "## Data Sources",
      "",
      "Traffic safety data is primarily sourced from:",
      "",
      "- **FARS (Fatality Analysis Reporting System)**: Provides detailed data on fatal traffic crashes in the United States",
      "- **NHTSA (National Highway Traffic Safety Administration)**: Additional traffic safety statistics",
      "- **CDC WONDER**: Mortality data related to traffic incidents",
      "",
      "## Variables",
      "",
      "The following traffic safety variables are available in the database:",
      "",
      "| Variable Name | Description | Units | Source |",
      "|--------------|-------------|-------|--------|"
    )
    
    # Add variables if we have them
    if (exists("get_traffic_safety_variable_names")) {
      traffic_vars <- get_traffic_safety_variable_names()
      
      for (var in traffic_vars) {
        var_desc <- gsub("_", " ", tools::toTitleCase(var))
        units <- if (grepl("rate", var)) "per 100,000 population" else "count"
        basic_doc_content <- c(basic_doc_content,
                              paste("|", var, "|", var_desc, "|", units, "| FARS/NHTSA |"))
      }
    }
    
    # Add methodology section
    basic_doc_content <- c(basic_doc_content,
                          "",
                          "## Methodology",
                          "",
                          "Traffic safety data is processed as follows:",
                          "",
                          "1. Raw data is obtained from FARS and other sources",
                          "2. County-level aggregation is performed for each variable",
                          "3. Rates are calculated using population denominators",
                          "4. Data quality indicators track the provenance of each data point",
                          "5. Temporal interpolation fills gaps in time series data where appropriate",
                          "6. Database integration ensures all traffic safety variables are accessible with other SDOH metrics")
    
    # Write the documentation
    writeLines(basic_doc_content, basic_doc_file)
    log_test(paste("Generated basic documentation at:", basic_doc_file), "SUCCESS")
  }
  
  return(list(
    status = any(found_docs),
    message = if (any(found_docs)) 
      paste("Found documentation file:", doc_file) 
    else 
      "No traffic safety documentation found",
    details = list(
      found_files = doc_files[found_docs],
      content_checks = sapply(doc_checks, length)
    )
  ))
}

# Run documentation test
all_results$documentation <- run_timed_test("Documentation", test_documentation)

# Generate final report ----
cat("\n========================================================================\n")
cat("TRAFFIC SAFETY TESTING SUITE - SUMMARY REPORT")
cat("\n========================================================================\n\n")

# Calculate success rates
test_statuses <- sapply(all_results, function(x) x$status)
test_times <- sapply(all_results, function(x) x$time)
total_time <- sum(test_times, na.rm = TRUE)

passed <- sum(test_statuses)
total <- length(test_statuses)
success_rate <- passed / total * 100

# Display summary table
cat(sprintf("Success Rate: %.1f%% (%d/%d tests passed)\n", success_rate, passed, total))
cat(sprintf("Total Run Time: %.1f seconds\n\n", total_time))

cat("Test Results:\n")
cat("-------------\n")
for (test_name in names(all_results)) {
  result <- all_results[[test_name]]
  status_text <- if (result$status) "PASSED" else "FAILED"
  status_color <- if (result$status) "\033[32m" else "\033[31m"  # Green for pass, red for fail
  
  cat(sprintf("%s%s\033[0m: %s (%.1f seconds)\n", 
             status_color, status_text, test_name, result$time))
}

# Save test results
saveRDS(all_results, file.path(test_results_dir, "test_results.rds"))

# Generate a report markdown file
report_file <- file.path(test_dir, "test_report.md")

report_content <- c(
  "# Traffic Safety Implementation Test Report",
  "",
  paste("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste("Success Rate:", sprintf("%.1f%%", success_rate), sprintf("(%d/%d tests passed)", passed, total)),
  paste("Total Run Time:", sprintf("%.1f seconds", total_time)),
  "",
  "## Test Results",
  "",
  "| Test | Status | Time (sec) | Message |",
  "|------|--------|------------|---------|"
)

for (test_name in names(all_results)) {
  result <- all_results[[test_name]]
  status_text <- if (result$status) "PASSED" else "FAILED"
  message <- result$message
  
  report_content <- c(report_content,
                     paste("|", test_name, "|", status_text, "|", 
                          sprintf("%.1f", result$time), "|", message, "|"))
}

# Add conclusions
report_content <- c(report_content,
                   "",
                   "## Conclusions",
                   "")

if (success_rate == 100) {
  report_content <- c(report_content,
                     "All tests passed! The traffic safety implementation is fully functional and integrated with the unified pipeline.",
                     "",
                     "Key components validated:",
                     "",
                     "- ✅ Core traffic safety module loaded successfully",
                     "- ✅ Data fetching and processing works properly",
                     "- ✅ Database integration stores all traffic safety variables correctly",
                     "- ✅ Integration with the unified pipeline is complete",
                     "- ✅ Analysis and visualization capabilities work as expected")
} else if (success_rate >= 75) {
  report_content <- c(report_content,
                     "Most tests passed. The traffic safety implementation is mostly functional but has some issues that should be addressed.",
                     "",
                     "### Issues to address:",
                     "")
  
  for (test_name in names(all_results)) {
    result <- all_results[[test_name]]
    if (!result$status) {
      report_content <- c(report_content,
                         paste("- ❌", test_name, ":", result$message))
    }
  }
} else {
  report_content <- c(report_content,
                     "Several tests failed. The traffic safety implementation requires significant work before integration.",
                     "",
                     "### Critical issues to address:",
                     "")
  
  for (test_name in names(all_results)) {
    result <- all_results[[test_name]]
    if (!result$status) {
      report_content <- c(report_content,
                         paste("- ❌", test_name, ":", result$message))
    }
  }
}

# Write the report
writeLines(report_content, report_file)
cat(sprintf("\nTest report saved to: %s\n", report_file))

# Display any failed tests in detail
if (success_rate < 100) {
  cat("\nFailed Tests:\n")
  cat("--------------\n")
  
  for (test_name in names(all_results)) {
    result <- all_results[[test_name]]
    if (!result$status) {
      cat(sprintf("\033[31m%s:\033[0m %s\n", test_name, result$message))
      
      # Add details if available
      if (!is.null(result$results) && !is.null(result$results$details)) {
        details <- result$results$details
        if (length(details) > 0 && !is.null(details)) {
          cat("  Details:\n")
          for (detail_name in names(details)) {
            detail_value <- details[[detail_name]]
            if (length(detail_value) == 1) {
              cat(sprintf("  - %s: %s\n", detail_name, detail_value))
            } else if (length(detail_value) > 0) {
              cat(sprintf("  - %s: %s\n", detail_name, 
                        if (is.list(detail_value)) "[complex value]" else paste(head(detail_value, 3), collapse = ", ")))
            }
          }
        }
      }
      cat("\n")
    }
  }
}

cat("\nTraffic safety testing complete!\n")

# Return appropriate exit code
if (success_rate == 100) {
  quit(status = 0)
} else {
  quit(status = 1)
}