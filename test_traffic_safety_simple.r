#!/usr/bin/env Rscript

# Simple test script for traffic safety integration
# Tests the basic functionality without requiring all dependencies

# Load minimal required packages
for (pkg in c("tidyverse", "magrittr")) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(paste("Required package", pkg, "is not installed.\n"))
    cat("Please run 'Rscript R/install_packages.r' first.\n")
    quit(status = 1)
  }
}

# Display message
cat("\n=== Simple Traffic Safety Integration Test ===\n\n")

# Test 1: Verify files exist
cat("Test 1: Verifying module files\n")
module_files <- c(
  "traffic_safety_integration.r",
  "traffic_safety_dashboard.r",
  "traffic_safety_validation.r",
  "traffic_safety_forecasting.r",
  "traffic_safety_geospatial.r",
  "traffic_safety_cache.r",
  "traffic_safety_api_tests.r"
)

for (file in module_files) {
  exists <- file.exists(file)
  status <- if (exists) "\033[32mFOUND\033[0m" else "\033[33mMISSING\033[0m"
  cat(paste(" -", file, ":", status, "\n"))
}

# Test 2: Source main integration file
cat("\nTest 2: Sourcing main integration file\n")
integration_result <- tryCatch({
  source("traffic_safety_integration.r")
  TRUE
}, error = function(e) {
  cat(paste(" - ERROR:", e$message, "\n"))
  FALSE
})

if (integration_result) {
  cat(" - \033[32mSUCCESS\033[0m: Integration file sourced successfully\n")
} else {
  cat(" - \033[31mFAIL\033[0m: Could not source integration file\n")
}

# Test 3: Check if main functions exist
cat("\nTest 3: Checking main function existence\n")
functions_to_check <- c(
  "fetch_enhanced_traffic_safety_data",
  "create_traffic_safety_visualizations",
  "add_traffic_safety_to_database",
  "load_traffic_safety_modules"
)

for (func in functions_to_check) {
  exists <- exists(func, mode = "function")
  status <- if (exists) "\033[32mFOUND\033[0m" else "\033[33mMISSING\033[0m"
  cat(paste(" -", func, ":", status, "\n"))
}

# Test 4: Verify dashboard function
cat("\nTest 4: Checking dashboard function\n")
dashboard_result <- tryCatch({
  source("traffic_safety_dashboard.r")
  TRUE
}, error = function(e) {
  cat(paste(" - ERROR:", e$message, "\n"))
  FALSE
})

if (dashboard_result) {
  dashboard_func_exists <- exists("launch_traffic_safety_dashboard", mode = "function")
  status <- if (dashboard_func_exists) "\033[32mSUCCESS\033[0m" else "\033[33mMISSING\033[0m"
  cat(paste(" - Dashboard function:", status, "\n"))
} else {
  cat(" - \033[31mFAIL\033[0m: Could not source dashboard file\n")
}

# Summary
cat("\n=== Test Summary ===\n")
all_files_exist <- all(sapply(module_files, file.exists))
all_funcs_exist <- all(sapply(functions_to_check, function(f) exists(f, mode = "function")))
dashboard_working <- dashboard_result && exists("launch_traffic_safety_dashboard", mode = "function")

overall_score <- sum(c(all_files_exist, integration_result, all_funcs_exist, dashboard_working))
overall_status <- if (overall_score >= 3) "\033[32mPASS\033[0m" else "\033[31mFAIL\033[0m"

cat(paste("Overall status:", overall_status, "(", overall_score, "/ 4 )\n"))
cat("Simple test complete at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")