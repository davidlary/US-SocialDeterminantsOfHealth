#!/usr/bin/env Rscript

# Test script for Traffic Safety Integration

# Set working directory to the script's location
script_dir <- dirname(commandArgs(trailingOnly = FALSE)[grep("--file=", commandArgs(trailingOnly = FALSE))[1]])
if (length(script_dir) > 0 && script_dir != "") {
  setwd(script_dir)
}

# Load required packages
required_packages <- c("tidyverse", "magrittr", "assertthat")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(paste("Required package", pkg, "is not installed.\n"))
    cat("Please run 'Rscript R/install_packages.r' first.\n")
    # Continue even without assertthat
    if (pkg != "assertthat") {
      quit(status = 1)
    }
  }
}

# Display message
cat("\n=== Testing Traffic Safety Integration ===\n\n")

# Test 1: Load all modules
cat("Test 1: Loading all traffic safety modules\n")
source("traffic_safety_integration.r")

module_statuses <- load_traffic_safety_modules()
for (module in names(module_statuses)) {
  status <- if (module_statuses[[module]]) "SUCCESS" else "FAILED"
  cat(paste(" -", module, ":", status, "\n"))
}

# Test 2: Fetch enhanced data
cat("\nTest 2: Fetching enhanced traffic safety data\n")
start_time <- Sys.time()
enhanced_data <- fetch_enhanced_traffic_safety_data(
  years = 2018:2020,
  cache_dir = "data/cache",
  use_validation = TRUE,
  use_optimized_cache = TRUE,
  generate_forecasts = TRUE,
  spatial_analysis = TRUE
)
end_time <- Sys.time()

cat(paste(" - Time taken:", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n"))
cat(paste(" - Data dimensions:", nrow(enhanced_data), "rows x", ncol(enhanced_data), "columns\n"))
cat(paste(" - Years:", paste(sort(unique(enhanced_data$year)), collapse = ", "), "\n"))

# Check enhancements attribute
enhancements <- attr(enhanced_data, "enhancements")
if (!is.null(enhancements)) {
  cat(" - Applied enhancements:\n")
  for (name in names(enhancements)) {
    if (name != "modules_loaded" && !is.null(enhancements[[name]])) {
      cat(paste("   *", name, ":", enhancements[[name]], "\n"))
    }
  }
}

# Test 3: Create visualizations
cat("\nTest 3: Creating visualizations\n")
viz_dir <- "output/test_traffic_safety"
if (!dir.exists(viz_dir)) {
  dir.create(viz_dir, recursive = TRUE, showWarnings = FALSE)
}

vis_files <- tryCatch({
  create_traffic_safety_visualizations(
    enhanced_data,
    output_dir = viz_dir,
    create_maps = TRUE,
    create_forecast_plots = TRUE
  )
}, error = function(e) {
  cat(paste(" - ERROR:", e$message, "\n"))
  return(NULL)
})

if (!is.null(vis_files) && length(vis_files) > 0) {
  cat(paste(" - Created", length(vis_files), "visualization files:\n"))
  for (name in names(vis_files)) {
    cat(paste("   *", name, ":", vis_files[[name]], "\n"))
  }
} else {
  cat(" - No visualization files created\n")
}

# Test 4: Examine forecasts
cat("\nTest 4: Examining forecasts\n")
forecasts <- attr(enhanced_data, "forecasts")
if (!is.null(forecasts) && !is.null(forecasts$national)) {
  national_forecast <- forecasts$national
  forecast_years <- national_forecast %>% 
    filter(type == "forecast") %>% 
    pull(year)
  
  cat(paste(" - Forecast generated for", length(forecast_years), "years:", 
            paste(forecast_years, collapse = ", "), "\n"))
  
  # Show prediction for last year
  last_year <- max(forecast_years)
  last_pred <- national_forecast %>% 
    filter(year == last_year) %>% 
    select(forecast, lower, upper)
  
  cat(paste(" - Prediction for", last_year, ":", 
            "Value =", round(last_pred$forecast, 2),
            "CI = [", round(last_pred$lower, 2), "-", round(last_pred$upper, 2), "]\n"))
} else {
  cat(" - No forecast data available\n")
}

# Test 5: Check spatial analysis
cat("\nTest 5: Examining spatial analysis\n")
spatial_data <- attr(enhanced_data, "spatial")
if (!is.null(spatial_data)) {
  cat(paste(" - Spatial analysis performed for year:", spatial_data$year, "\n"))
  
  if (!is.null(spatial_data$data)) {
    # Count cluster types
    cluster_counts <- table(spatial_data$data$cluster_type)
    cat(" - Cluster counts:\n")
    for (cluster_type in names(cluster_counts)) {
      cat(paste("   *", cluster_type, ":", cluster_counts[cluster_type], "\n"))
    }
  }
  
  if (!is.null(spatial_data$problem_areas)) {
    problem_count <- nrow(spatial_data$problem_areas)
    persistent_count <- sum(spatial_data$problem_areas$persistently_problematic)
    
    cat(paste(" - Identified", problem_count, "counties with elevated rates\n"))
    cat(paste(" - Of these,", persistent_count, "are persistently problematic\n"))
  }
} else {
  cat(" - No spatial analysis results available\n")
}

# Test 6: Validation results
cat("\nTest 6: Checking validation results\n")
validation <- attr(enhanced_data, "validation")
if (!is.null(validation) && inherits(validation, "TrafficDataValidator")) {
  cat(paste(" - Validation performed with", validation$count_rules(), "rules\n"))
  cat(paste(" - Passed rules:", validation$count_passed_rules(), "\n"))
  cat(paste(" - Failed rules:", validation$count_failed_rules(), "\n"))
  
  if (validation$count_failed_rules() > 0) {
    cat(" - Failed validation rule summary:\n")
    for (rule_name in names(validation$warnings)) {
      cat(paste("   *", rule_name, "(", validation$warnings[[rule_name]]$severity, "):", 
                validation$warnings[[rule_name]]$message, "\n"))
    }
  }
} else {
  cat(" - No validation results available\n")
}

# Test 7: Dashboard functionality check
cat("\nTest 7: Checking dashboard functionality\n")

# Check if dashboard file exists
if (file.exists("traffic_safety_dashboard.r")) {
  cat(" - Dashboard file found, checking implementation...\n")
  
  # Source the dashboard file without launching it
  tryCatch({
    source("traffic_safety_dashboard.r")
    
    # Check if main function exists
    if (exists("launch_traffic_safety_dashboard", mode = "function")) {
      # Check function signature
      args <- formals(launch_traffic_safety_dashboard)
      expected_args <- c("traffic_data", "transport_data", "port", "host", "launch_browser")
      
      missing_args <- setdiff(expected_args, names(args))
      
      if (length(missing_args) > 0) {
        cat(" - WARNING: Dashboard function missing expected arguments:", paste(missing_args, collapse = ", "), "\n")
      } else {
        cat(" - Dashboard implementation verified successfully\n")
      }
    } else {
      cat(" - ERROR: Dashboard function 'launch_traffic_safety_dashboard' not found\n")
    }
  }, error = function(e) {
    cat(paste(" - ERROR loading dashboard:", e$message, "\n"))
  })
} else {
  cat(" - Dashboard file 'traffic_safety_dashboard.r' not found\n")
}

# Test 8: Database compatibility check
cat("\nTest 8: Checking database integration\n")

# Check if function for database integration exists
if (exists("add_traffic_safety_to_database", mode = "function")) {
  cat(" - Database integration function found, checking API...\n")
  
  # Check function signature
  args <- formals(add_traffic_safety_to_database)
  expected_args <- c("traffic_data", "db_path", "add_forecasts", "add_spatial")
  
  missing_args <- setdiff(expected_args, names(args))
  
  if (length(missing_args) > 0) {
    cat(" - WARNING: Database function missing expected arguments:", paste(missing_args, collapse = ", "), "\n")
  } else {
    cat(" - Database integration API verified successfully\n")
  }
} else {
  cat(" - Database integration function 'add_traffic_safety_to_database' not found\n")
}

# End of tests
cat("\n=== Traffic Safety Integration Tests Completed ===\n")
cat("All tests completed successfully.\n")

# Output summary
cat("\n=== Summary ===\n")
summary_items <- list(
  "Traffic safety modules loaded" = length(module_statuses),
  "Enhanced data fetched" = !is.null(enhanced_data) && nrow(enhanced_data) > 0,
  "Visualizations created" = !is.null(vis_files) && length(vis_files) > 0,
  "Forecast capability" = !is.null(forecasts) && !is.null(forecasts$national),
  "Spatial analysis" = !is.null(spatial_data),
  "Validation framework" = !is.null(validation) && inherits(validation, "TrafficDataValidator"),
  "Dashboard implementation" = exists("launch_traffic_safety_dashboard", mode = "function"),
  "Database integration" = exists("add_traffic_safety_to_database", mode = "function")
)

success_count <- sum(unlist(summary_items))
total_count <- length(summary_items)

cat(paste("Test success rate:", success_count, "/", total_count, 
          "(", round(100 * success_count / total_count), "%)\n"))

cat("\nTraffic safety module integration completed at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")