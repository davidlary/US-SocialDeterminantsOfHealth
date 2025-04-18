#!/usr/bin/env Rscript

# Test harness for the traffic safety data module
# This script tests the traffic safety data fetcher with various configurations

# Source the traffic safety module
source("fetch_traffic_safety_data.r")

# Configure test parameters
test_years <- c(2015:2020)
cache_dir <- "data/cache"
test_modes <- c(
  "basic" = "Basic functionality with default settings",
  "offline" = "Offline mode using cached data only",
  "simulation" = "With simulation for missing data",
  "parallel" = "With parallel processing",
  "visualization" = "With data visualization"
)

# Function to run a test
run_test <- function(mode, years = test_years) {
  cat("\n==================================================\n")
  cat(paste("RUNNING TEST:", mode, "-", test_modes[mode], "\n"))
  cat("==================================================\n\n")
  
  # Configure parameters based on test mode
  params <- list(
    years = years,
    cache_dir = cache_dir,
    refresh_cache = FALSE
  )
  
  # Add mode-specific parameters
  if (mode == "offline") {
    params$offline_mode <- TRUE
  }
  
  if (mode == "simulation") {
    params$allow_simulation <- TRUE
  }
  
  if (mode == "parallel") {
    # Set up parallel processing if available
    if (require("parallel") && require("future") && require("future.apply")) {
      # Source parallel processor
      if (file.exists("parallel_processor.r")) {
        source("parallel_processor.r")
        params$parallel <- TRUE
        params$parallel_config <- setup_parallel_environment(workers = 2, progress = TRUE)
      } else {
        params$parallel <- FALSE
        cat("Warning: parallel_processor.r not found. Falling back to sequential processing.\n")
      }
    } else {
      params$parallel <- FALSE
      cat("Warning: Parallel processing packages not available.\n")
    }
  }
  
  # Record start time
  start_time <- Sys.time()
  
  # Run the fetch function with parameters
  result <- tryCatch({
    do.call(fetch_traffic_safety_data, params)
  }, error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
    return(NULL)
  })
  
  # Calculate elapsed time
  elapsed <- difftime(Sys.time(), start_time, units = "secs")
  
  # Print summary
  if (!is.null(result)) {
    cat("\nTEST RESULTS SUMMARY:\n")
    cat("- Rows retrieved:", nrow(result), "\n")
    cat("- Counties:", length(unique(result$fips)), "\n")
    cat("- Years:", length(unique(result$year)), "\n")
    cat("- Time taken:", round(elapsed, 2), "seconds\n")
    
    # Check for quality flags
    for (var in c("traffic_fatality_count", "dui_fatality_count")) {
      quality_var <- paste0(var, "_data_quality")
      if (quality_var %in% names(result)) {
        quality_counts <- table(result[[quality_var]], useNA = "ifany")
        cat(paste0("- ", var, " quality distribution:\n"))
        for (q in names(quality_counts)) {
          cat("  * ", q, ": ", quality_counts[q], "\n", sep = "")
        }
      }
    }
    
    # Generate visualization if in visualization mode
    if (mode == "visualization") {
      if (require("ggplot2")) {
        cat("\nGenerating visualizations...\n")
        
        # Create visualization directory if it doesn't exist
        viz_dir <- "output/visualizations/traffic_safety"
        if (!dir.exists(viz_dir)) {
          dir.create(viz_dir, recursive = TRUE, showWarnings = FALSE)
        }
        
        # Create time series plot of traffic fatalities
        p1 <- ggplot(result[!is.na(result$traffic_fatality_rate_per_100k),], 
               aes(x = year, y = traffic_fatality_rate_per_100k, group = fips, color = traffic_fatality_count_data_quality)) +
          geom_line(alpha = 0.3) +
          labs(title = "Traffic Fatality Rates by County",
               x = "Year", 
               y = "Fatalities per 100k population",
               color = "Data Quality") +
          theme_minimal()
        
        # Save plot
        ggsave(file.path(viz_dir, "traffic_fatality_rates.png"), p1, width = 10, height = 6)
        cat("- Saved visualization to", file.path(viz_dir, "traffic_fatality_rates.png"), "\n")
        
        # Create heatmap of fatalities by year
        if (length(unique(result$year)) > 1) {
          yearly_data <- aggregate(
            traffic_fatality_count ~ year, 
            data = result[!is.na(result$traffic_fatality_count),], 
            FUN = sum
          )
          
          p2 <- ggplot(yearly_data, aes(x = factor(year), y = 1, fill = traffic_fatality_count)) +
            geom_tile() +
            scale_fill_viridis_c() +
            labs(title = "Total Traffic Fatalities by Year",
                 x = "Year", 
                 y = "",
                 fill = "Fatality Count") +
            theme_minimal() +
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank())
          
          # Save plot
          ggsave(file.path(viz_dir, "yearly_fatalities_heatmap.png"), p2, width = 10, height = 3)
          cat("- Saved visualization to", file.path(viz_dir, "yearly_fatalities_heatmap.png"), "\n")
        }
      } else {
        cat("Warning: ggplot2 not available. Skipping visualizations.\n")
      }
    }
    
    # Return success
    return(TRUE)
  } else {
    cat("\nTEST FAILED\n")
    return(FALSE)
  }
}

# Run all tests
cat("TRAFFIC SAFETY MODULE TEST HARNESS\n")
cat("=================================\n\n")

# Check if a specific test was requested via command line
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0 && args[1] %in% names(test_modes)) {
  # Run only the requested test
  run_test(args[1])
} else {
  # Run all tests
  for (mode in names(test_modes)) {
    run_test(mode)
  }
}

cat("\nTest harness completed.\n")