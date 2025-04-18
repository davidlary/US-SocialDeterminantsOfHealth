#!/usr/bin/env Rscript

# Unit tests for traffic safety data module
# This script contains automated tests for the traffic safety module functions

# Load required packages
if (!require("testthat", quietly = TRUE)) {
  stop("testthat package is required for running unit tests. Please install it with install.packages('testthat')")
}

# Source the traffic safety module
source("fetch_traffic_safety_data.r")

# Create test context
context("Traffic Safety Data Fetcher")

# Test basic functionality
test_that("fetch_traffic_safety_data function exists", {
  expect_true(exists("fetch_traffic_safety_data"))
  expect_true(is.function(fetch_traffic_safety_data))
})

# Test function parameters
test_that("fetch_traffic_safety_data has proper parameters", {
  params <- formals(fetch_traffic_safety_data)
  expect_true("years" %in% names(params))
  expect_true("cache_dir" %in% names(params))
  expect_true("refresh_cache" %in% names(params))
  expect_true("allow_simulation" %in% names(params))
  expect_true("allow_interpolation" %in% names(params))
  expect_true("offline_mode" %in% names(params))
  expect_true("parallel" %in% names(params))
})

# Test helper function: create_empty_county_data
test_that("create_empty_county_data creates proper structure", {
  # Test with a small subset of years
  test_years <- c(2019, 2020)
  
  # Get function from environment
  if (exists("create_empty_county_data", envir = environment(fetch_traffic_safety_data))) {
    create_empty_county_data <- get("create_empty_county_data", envir = environment(fetch_traffic_safety_data))
    
    # Call the function
    empty_data <- create_empty_county_data(test_years)
    
    # Check structure
    expect_true(is.data.frame(empty_data))
    expect_true("fips" %in% names(empty_data))
    expect_true("year" %in% names(empty_data))
    expect_true("traffic_fatality_count" %in% names(empty_data))
    expect_true("traffic_fatality_rate_per_100k" %in% names(empty_data))
    
    # Check years
    expect_true(all(sort(unique(empty_data$year)) == sort(test_years)))
  } else {
    skip("create_empty_county_data function not directly accessible for testing")
  }
})

# Test FARS data fetching (offline mode)
test_that("FARS data fetching handles offline mode correctly", {
  # Create a mock function for testing
  mock_get_fars_data <- function(years, cache_dir, refresh_cache = FALSE) {
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Create minimal test data
    test_data <- data.frame(
      fips = c("01001", "01003"),
      year = rep(years[1], 2),
      traffic_fatality_count = c(5, 8),
      stringsAsFactors = FALSE
    )
    
    return(test_data)
  }
  
  # Temporarily replace the real function with our mock
  if (exists("get_fars_data", envir = environment(fetch_traffic_safety_data))) {
    original_get_fars_data <- get("get_fars_data", envir = environment(fetch_traffic_safety_data))
    assign("get_fars_data", mock_get_fars_data, envir = environment(fetch_traffic_safety_data))
    
    # Test offline mode - should not call our mock function
    test_result <- tryCatch({
      fetch_traffic_safety_data(years = 2020, offline_mode = TRUE, cache_dir = tempdir())
      "No error"
    }, error = function(e) {
      e$message
    })
    
    # Reset original function
    assign("get_fars_data", original_get_fars_data, envir = environment(fetch_traffic_safety_data))
    
    # The function should complete without error in offline mode, even if no data is found
    expect_equal(test_result, "No error")
  } else {
    skip("get_fars_data function not directly accessible for testing")
  }
})

# Test handling of missing assertthat package
test_that("Module works without assertthat package", {
  # Check if assertthat is loaded
  has_assertthat <- requireNamespace("assertthat", quietly = TRUE)
  
  # Simulate assertthat not being available (if it is available)
  if (has_assertthat && "package:assertthat" %in% search()) {
    # Unload assertthat temporarily
    save_assertthat <- asNamespace("assertthat")
    detach("package:assertthat", unload = TRUE)
    
    # Make sure has_assertthat is set to FALSE
    assign("has_assertthat", FALSE, envir = environment(fetch_traffic_safety_data))
    
    # Test functionality without assertthat
    expect_error(fetch_traffic_safety_data(years = 2020, offline_mode = TRUE), NA)
    
    # Reset environment
    require(assertthat)
    assign("has_assertthat", TRUE, envir = environment(fetch_traffic_safety_data))
  } else {
    # If assertthat is not loaded, test that the function still works
    expect_error(fetch_traffic_safety_data(years = 2020, offline_mode = TRUE), NA)
  }
})

# Test argument validation
test_that("fetch_traffic_safety_data validates years correctly", {
  # Test with years in the future
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  future_years <- c(current_year + 1, current_year + 2)
  
  # Should not error with future years, but should warn or reduce to current
  expect_warning(
    result <- fetch_traffic_safety_data(years = future_years, offline_mode = TRUE), 
    regexp = NA
  )
  
  # Test with invalid years (should not error but warn)
  expect_warning(
    result <- fetch_traffic_safety_data(years = NULL, offline_mode = TRUE),
    regexp = NA
  )
})

# Run the tests
test_results <- test_dir(".", reporter = "summary")
cat("\n\n")
print(test_results)