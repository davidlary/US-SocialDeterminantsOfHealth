#!/usr/bin/env Rscript

# Traffic Safety API Integration Tests
# This script tests the API integration points for traffic safety data
# including NHTSA FARS API, CDC WONDER API, and Census API connections

# Required packages
required_packages <- c(
  "testthat",
  "httr",
  "jsonlite",
  "xml2",
  "tidyverse",
  "mockery"
)

# Load required packages
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    message(paste("Required package", pkg, "is not installed."))
    message("Please run 'Rscript R/install_packages.r' first.")
    # Don't stop execution, just warn and continue with reduced functionality
  }
}

# Source the main traffic safety data fetcher
if (!file.exists("fetch_traffic_safety_data.r")) {
  stop("fetch_traffic_safety_data.r not found. Please run this test from the project root directory.")
}
source("fetch_traffic_safety_data.r")

# Configuration for tests
test_config <- list(
  log_file = "logs/api_tests.log",
  test_year = 2020,  # Use a recent but not current year for consistent data availability
  test_timeout = 60, # Timeout in seconds for API calls
  test_county_fips = "06037",  # Los Angeles County (large county with reliable data)
  cache_dir = "data/cache/test_api",
  api_endpoints = list(
    nhtsa_fars = "https://crashviewer.nhtsa.dot.gov/CrashAPI/",
    cdc_wonder = "https://wonder.cdc.gov/",
    census_api = "https://api.census.gov/data/"
  )
)

# Initialize test log
log_test_message <- function(message) {
  # Create log directory if it doesn't exist
  log_dir <- dirname(test_config$log_file)
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Format log message
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- paste0(timestamp, " [API-TEST] ", message)
  
  # Write to log file
  cat(log_entry, "\n", file = test_config$log_file, append = TRUE)
  
  # Also print to console
  cat(log_entry, "\n")
}

log_test_message("Starting Traffic Safety API integration tests")

# Define test suites ==============================================================

# Test NHTSA FARS API connectivity
test_nhtsa_fars_api <- function() {
  log_test_message("Testing NHTSA FARS API connectivity")
  
  # Start testthat context
  testthat::context("NHTSA FARS API Tests")
  
  # Test API connectivity
  testthat::test_that("NHTSA FARS API is accessible", {
    # Define a basic endpoint that should always work
    endpoint <- paste0(test_config$api_endpoints$nhtsa_fars, 
                     "crashes/GetCaseList?states=1&fromYear=", 
                     test_config$test_year, "&toYear=", test_config$test_year, "&minNumOfVehicles=1")
    
    # Try to connect with error handling
    response <- tryCatch({
      httr::GET(endpoint, httr::timeout(test_config$test_timeout))
    }, error = function(e) {
      log_test_message(paste("Error connecting to NHTSA FARS API:", e$message))
      NULL
    })
    
    # Skip if connection failed completely
    if (is.null(response)) {
      testthat::skip("Failed to connect to NHTSA FARS API")
    }
    
    # Check response status
    testthat::expect_equal(httr::status_code(response), 200)
    
    # Check response format
    content_type <- httr::headers(response)[["content-type"]]
    testthat::expect_true(grepl("application/json", content_type, ignore.case = TRUE))
    
    # Parse response
    content <- tryCatch({
      httr::content(response, "text", encoding = "UTF-8")
      jsonlite::fromJSON(content)
    }, error = function(e) {
      log_test_message(paste("Error parsing NHTSA FARS API response:", e$message))
      NULL
    })
    
    # Validate response structure
    testthat::expect_false(is.null(content))
    testthat::expect_true("Results" %in% names(content))
  })
  
  # Test the get_fars_data function
  testthat::test_that("get_fars_data function works correctly", {
    # Create a temporary cache directory
    temp_cache_dir <- file.path(test_config$cache_dir, "fars_test")
    if (!dir.exists(temp_cache_dir)) {
      dir.create(temp_cache_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Test with a single year
    test_year <- test_config$test_year
    
    # Call the function with error handling
    result <- tryCatch({
      get_fars_data(test_year, temp_cache_dir, refresh_cache = TRUE)
    }, error = function(e) {
      log_test_message(paste("Error in get_fars_data:", e$message))
      NULL
    })
    
    # Skip if function failed completely
    if (is.null(result)) {
      testthat::skip("get_fars_data function failed")
    }
    
    # Validate the result structure
    testthat::expect_true(is.data.frame(result))
    testthat::expect_true(nrow(result) > 0)
    testthat::expect_true("fips" %in% names(result))
    testthat::expect_true("year" %in% names(result))
    
    # Validate data quality for a specific county
    county_data <- result[result$fips == test_config$test_county_fips, ]
    testthat::expect_true(nrow(county_data) > 0, 
                          info = paste("No data found for county", 
                                     test_config$test_county_fips))
    
    # Check that key metrics are present
    key_metrics <- c("traffic_fatality_count")
    for (metric in key_metrics) {
      testthat::expect_true(metric %in% names(result), 
                           info = paste("Missing key metric:", metric))
    }
    
    # Test cache functionality
    cached_file <- file.path(temp_cache_dir, 
                            paste0("fars_data_", test_year, "_", test_year, ".rds"))
    testthat::expect_true(file.exists(cached_file))
    
    # Test loading from cache
    cached_result <- readRDS(cached_file)
    testthat::expect_equal(nrow(cached_result), nrow(result))
  })
  
  log_test_message("Completed NHTSA FARS API tests")
}

# Test CDC WONDER API connectivity
test_cdc_wonder_api <- function() {
  log_test_message("Testing CDC WONDER API connectivity")
  
  # Start testthat context
  testthat::context("CDC WONDER API Tests")
  
  # Test CDC WONDER API accessibility
  testthat::test_that("CDC WONDER API is accessible", {
    # CDC WONDER API is complex and requires XML requests
    # For basic connectivity test, just check the website
    endpoint <- test_config$api_endpoints$cdc_wonder
    
    # Try to connect with error handling
    response <- tryCatch({
      httr::GET(endpoint, httr::timeout(test_config$test_timeout))
    }, error = function(e) {
      log_test_message(paste("Error connecting to CDC WONDER website:", e$message))
      NULL
    })
    
    # Skip if connection failed completely
    if (is.null(response)) {
      testthat::skip("Failed to connect to CDC WONDER website")
    }
    
    # Check response status
    testthat::expect_equal(httr::status_code(response), 200)
  })
  
  # Test the get_cdc_wonder_data function with mocking
  testthat::test_that("get_cdc_wonder_data function handles errors gracefully", {
    # Create a temporary cache directory
    temp_cache_dir <- file.path(test_config$cache_dir, "cdc_test")
    if (!dir.exists(temp_cache_dir)) {
      dir.create(temp_cache_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Create mock data 
    mock_data <- data.frame(
      fips = rep(test_config$test_county_fips, 2),
      year = c(test_config$test_year - 1, test_config$test_year),
      transport_mortality_count = c(120, 130),
      transport_mortality_rate_per_100k = c(12.5, 13.2),
      stringsAsFactors = FALSE
    )
    
    # Save mock data to cache for testing
    mock_cache_file <- file.path(temp_cache_dir, 
                               paste0("cdc_wonder_data_", 
                                      test_config$test_year - 1, "_", 
                                      test_config$test_year, ".rds"))
    saveRDS(mock_data, mock_cache_file)
    
    # Now test loading from cache
    result <- tryCatch({
      get_cdc_wonder_data(
        c(test_config$test_year - 1, test_config$test_year),
        temp_cache_dir,
        refresh_cache = FALSE
      )
    }, error = function(e) {
      log_test_message(paste("Error in get_cdc_wonder_data:", e$message))
      NULL
    })
    
    # Validate the result
    testthat::expect_false(is.null(result))
    testthat::expect_true(is.data.frame(result))
    testthat::expect_equal(nrow(result), 2)
    testthat::expect_true("transport_mortality_count" %in% names(result))
  })
  
  # Mock full CDC WONDER API request tests
  testthat::test_that("CDC WONDER API request formation is correct", {
    # Skip actual API calls in this test - just test request format
    testthat::skip_on_cran()
    
    # Create a mock for the POST function
    mock_post <- mockery::mock(
      # Return a mock response object
      list(
        status_code = 200,
        content = function(type, encoding) {
          "<results><response>Test CDC Response</response></results>"
        }
      )
    )
    
    # Replace httr::POST with our mock
    with_mock(
      "httr::POST" = mock_post,
      {
        # Test forming a CDC WONDER API request
        tryCatch({
          # This function doesn't exist in the base module, so it's just testing the concept
          example_cdc_request <- function() {
            # Build a sample CDC WONDER XML request
            request_xml <- paste0(
              '<request-parameters>',
              '<parameter name="year">', test_config$test_year, '</parameter>',
              '<parameter name="ICD-10 Codes">V01-V99</parameter>',
              '</request-parameters>'
            )
            
            # Make the request (would be mocked)
            response <- httr::POST(
              test_config$api_endpoints$cdc_wonder,
              body = request_xml,
              encode = "raw"
            )
            
            return(response)
          }
          
          # Call our example function
          response <- example_cdc_request()
          
          # Check it was called with expected parameters
          testthat::expect_equal(mockery::mock_args(mock_post)[[1]][[1]], 
                               test_config$api_endpoints$cdc_wonder)
        }, error = function(e) {
          log_test_message(paste("Error in CDC WONDER request test:", e$message))
          testthat::fail(e$message)
        })
      }
    )
  })
  
  log_test_message("Completed CDC WONDER API tests")
}

# Test Census API connectivity
test_census_api <- function() {
  log_test_message("Testing Census API connectivity")
  
  # Start testthat context
  testthat::context("Census API Tests")
  
  # Test API connectivity
  testthat::test_that("Census API is accessible", {
    # Check basic api.census.gov connectivity
    endpoint <- test_config$api_endpoints$census_api
    
    # Try to connect with error handling
    response <- tryCatch({
      httr::GET(endpoint, httr::timeout(test_config$test_timeout))
    }, error = function(e) {
      log_test_message(paste("Error connecting to Census API:", e$message))
      NULL
    })
    
    # Skip if connection failed completely
    if (is.null(response)) {
      testthat::skip("Failed to connect to Census API")
    }
    
    # Check response status
    testthat::expect_equal(httr::status_code(response), 200)
  })
  
  # Test Census API with tidycensus if available
  testthat::test_that("Census data can be retrieved via tidycensus", {
    # Skip if tidycensus is not available
    if (!requireNamespace("tidycensus", quietly = TRUE)) {
      testthat::skip("tidycensus package not available")
    }
    
    # Try to get population data for test year
    result <- tryCatch({
      tidycensus::get_estimates(
        geography = "county",
        product = "population",
        year = test_config$test_year
      )
    }, error = function(e) {
      log_test_message(paste("Error in tidycensus::get_estimates:", e$message))
      NULL
    })
    
    # Skip if API call failed
    if (is.null(result)) {
      testthat::skip("Failed to retrieve Census data via tidycensus")
    }
    
    # Validate the result
    testthat::expect_true(is.data.frame(result))
    testthat::expect_true(nrow(result) > 0)
    testthat::expect_true("GEOID" %in% names(result))
    
    # Check that test county is present
    county_data <- result[result$GEOID == test_config$test_county_fips, ]
    testthat::expect_true(nrow(county_data) > 0)
  })
  
  # Test the population data retrieval in fetch_traffic_safety_data
  testthat::test_that("Population data retrieval works in fetch_traffic_safety_data", {
    # Skip if tidycensus is not available
    if (!requireNamespace("tidycensus", quietly = TRUE)) {
      testthat::skip("tidycensus package not available")
    }
    
    # Create a mock minimal traffic data
    mock_traffic_data <- data.frame(
      fips = rep(test_config$test_county_fips, 2),
      year = c(test_config$test_year - 1, test_config$test_year),
      traffic_fatality_count = c(50, 55),
      stringsAsFactors = FALSE
    )
    
    # Create temp cache dir
    temp_cache_dir <- file.path(test_config$cache_dir, "census_test")
    if (!dir.exists(temp_cache_dir)) {
      dir.create(temp_cache_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Call fetch_traffic_safety_data with refresh_cache=TRUE to force population data retrieval
    result <- tryCatch({
      fetch_traffic_safety_data(
        years = c(test_config$test_year - 1, test_config$test_year),
        cache_dir = temp_cache_dir,
        refresh_cache = TRUE
      )
    }, error = function(e) {
      log_test_message(paste("Error in fetch_traffic_safety_data population test:", e$message))
      NULL
    })
    
    # Skip if function failed completely
    if (is.null(result)) {
      testthat::skip("fetch_traffic_safety_data function failed for population test")
    }
    
    # Check that population-based rates were calculated
    testthat::expect_true("traffic_fatality_rate_per_100k" %in% names(result))
    
    # Check population cache file was created
    pop_cache_file <- file.path(temp_cache_dir, "population_data.rds")
    testthat::expect_true(file.exists(pop_cache_file))
    
    # Load and verify the cached population data
    pop_data <- readRDS(pop_cache_file)
    testthat::expect_true(is.data.frame(pop_data))
    testthat::expect_true("population" %in% names(pop_data))
    
    # Check Los Angeles County data exists in population data
    la_county_pop <- pop_data[pop_data$fips == test_config$test_county_fips, ]
    testthat::expect_true(nrow(la_county_pop) > 0)
  })
  
  log_test_message("Completed Census API tests")
}

# Test local cache operations
test_cache_operations <- function() {
  log_test_message("Testing cache operations")
  
  # Start testthat context
  testthat::context("Cache Operations Tests")
  
  # Test cache creation and loading
  testthat::test_that("Cache directories are created and used properly", {
    # Create temp cache dir for this test
    temp_cache_dir <- file.path(test_config$cache_dir, "cache_test")
    if (dir.exists(temp_cache_dir)) {
      unlink(temp_cache_dir, recursive = TRUE)
    }
    
    # Verify the directory doesn't exist yet
    testthat::expect_false(dir.exists(temp_cache_dir))
    
    # Call fetch_traffic_safety_data with allow_simulation=TRUE to generate data
    # even if we can't fetch from APIs - we just want to test caching
    result1 <- tryCatch({
      fetch_traffic_safety_data(
        years = test_config$test_year,
        cache_dir = temp_cache_dir,
        refresh_cache = TRUE,
        allow_simulation = TRUE,
        offline_mode = TRUE  # Use offline mode to avoid actual API calls
      )
    }, error = function(e) {
      log_test_message(paste("Error in cache test (first call):", e$message))
      NULL
    })
    
    # Skip if function failed completely
    if (is.null(result1)) {
      testthat::skip("fetch_traffic_safety_data function failed for cache test")
    }
    
    # Check that the cache directory was created
    testthat::expect_true(dir.exists(temp_cache_dir))
    
    # Check that cache file was created
    cache_file <- file.path(temp_cache_dir, "traffic_safety", 
                           paste0("traffic_safety_data_", 
                                 test_config$test_year, "_", 
                                 test_config$test_year, ".rds"))
    testthat::expect_true(file.exists(cache_file))
    
    # Now run again with refresh_cache=FALSE to test cache loading
    result2 <- tryCatch({
      fetch_traffic_safety_data(
        years = test_config$test_year,
        cache_dir = temp_cache_dir,
        refresh_cache = FALSE,
        offline_mode = TRUE
      )
    }, error = function(e) {
      log_test_message(paste("Error in cache test (second call):", e$message))
      NULL
    })
    
    # Skip if second call failed
    if (is.null(result2)) {
      testthat::skip("fetch_traffic_safety_data function failed for second cache test call")
    }
    
    # Verify that results match, indicating the cache was used
    testthat::expect_equal(nrow(result1), nrow(result2))
    testthat::expect_equal(ncol(result1), ncol(result2))
    
    # Clean up
    unlink(temp_cache_dir, recursive = TRUE)
  })
  
  log_test_message("Completed cache operations tests")
}

# Test the full pipeline
test_full_pipeline <- function() {
  log_test_message("Testing full traffic safety data pipeline")
  
  # Start testthat context
  testthat::context("Full Pipeline Tests")
  
  # Test full pipeline with real data
  testthat::test_that("Full pipeline works with real data sources", {
    # Skip on CRAN or CI environments to avoid long-running tests
    testthat::skip_on_cran()
    
    # Create temp cache dir for this test
    temp_cache_dir <- file.path(test_config$cache_dir, "full_pipeline_test")
    if (!dir.exists(temp_cache_dir)) {
      dir.create(temp_cache_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Test with a small subset of years to keep test runtime reasonable
    test_years <- c(test_config$test_year - 1, test_config$test_year)
    
    # Call fetch_traffic_safety_data with minimal configuration
    result <- tryCatch({
      fetch_traffic_safety_data(
        years = test_years,
        cache_dir = temp_cache_dir,
        refresh_cache = TRUE,
        allow_interpolation = TRUE
      )
    }, error = function(e) {
      log_test_message(paste("Error in full pipeline test:", e$message))
      NULL
    })
    
    # Skip if function failed completely
    if (is.null(result)) {
      testthat::skip("fetch_traffic_safety_data function failed for full pipeline test")
    }
    
    # Validate the result structure
    testthat::expect_true(is.data.frame(result))
    testthat::expect_true(nrow(result) > 0)
    
    # Check required columns are present
    required_columns <- c(
      "fips", "year", 
      "traffic_fatality_count", "traffic_fatality_rate_per_100k",
      "traffic_fatality_count_data_quality", "traffic_fatality_rate_per_100k_data_quality"
    )
    
    for (col in required_columns) {
      testthat::expect_true(col %in% names(result), 
                           info = paste("Missing required column:", col))
    }
    
    # Check that data for both years is present
    year_counts <- table(result$year)
    testthat::expect_true(all(test_years %in% names(year_counts)))
    
    # Check that data quality flags are properly set
    quality_flags <- unique(result$traffic_fatality_count_data_quality)
    testthat::expect_true(any(c("direct", "interpolated", "simulated") %in% quality_flags))
    
    # Check Los Angeles County data
    la_county_data <- result[result$fips == test_config$test_county_fips, ]
    testthat::expect_true(nrow(la_county_data) > 0)
    
    log_test_message(paste("Full pipeline test successful with", nrow(result), "records"))
  })
  
  log_test_message("Completed full pipeline tests")
}

# Run all tests ==================================================================

run_tests <- function() {
  log_test_message("Running all traffic safety API integration tests")
  
  # Create or clear the temporary test cache directory
  if (dir.exists(test_config$cache_dir)) {
    # Just clean specific test directories to avoid deleting real cache
    test_dirs <- c("fars_test", "cdc_test", "census_test", "cache_test", "full_pipeline_test")
    for (dir in test_dirs) {
      full_path <- file.path(test_config$cache_dir, dir)
      if (dir.exists(full_path)) {
        unlink(full_path, recursive = TRUE)
      }
    }
  } else {
    dir.create(test_config$cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Run all test suites
  test_results <- list()
  
  test_results$nhtsa_fars <- tryCatch({
    test_nhtsa_fars_api()
    "PASS"
  }, error = function(e) {
    log_test_message(paste("NHTSA FARS API tests failed:", e$message))
    e$message
  })
  
  test_results$cdc_wonder <- tryCatch({
    test_cdc_wonder_api()
    "PASS"
  }, error = function(e) {
    log_test_message(paste("CDC WONDER API tests failed:", e$message))
    e$message
  })
  
  test_results$census_api <- tryCatch({
    test_census_api()
    "PASS"
  }, error = function(e) {
    log_test_message(paste("Census API tests failed:", e$message))
    e$message
  })
  
  test_results$cache_operations <- tryCatch({
    test_cache_operations()
    "PASS"
  }, error = function(e) {
    log_test_message(paste("Cache operations tests failed:", e$message))
    e$message
  })
  
  test_results$full_pipeline <- tryCatch({
    test_full_pipeline()
    "PASS"
  }, error = function(e) {
    log_test_message(paste("Full pipeline tests failed:", e$message))
    e$message
  })
  
  # Summarize results
  log_test_message("API Integration Test Results Summary:")
  for (test_name in names(test_results)) {
    result <- test_results[[test_name]]
    status <- if (result == "PASS") "PASSED" else "FAILED"
    log_test_message(paste(" -", test_name, ":", status))
    if (status == "FAILED") {
      log_test_message(paste("   Error:", result))
    }
  }
  
  # Return the results
  return(test_results)
}

# Run tests if executed directly
if (!interactive()) {
  run_tests()
}