#!/usr/bin/env Rscript

# Test Script for Traffic Safety Implementation
# This script tests all components of the traffic safety implementation:
# 1. Data fetching from FARS and CDC WONDER
# 2. Data validation and processing
# 3. Forecasting
# 4. Geospatial analysis
# 5. Integration with the unified pipeline

# Set working directory to project root
script_path <- commandArgs(trailingOnly = FALSE)
script_path <- sub("--file=", "", script_path[grep("--file=", script_path)])
if (length(script_path) > 0) {
  script_dir <- dirname(script_path)
  root_dir <- dirname(script_dir)
  setwd(root_dir)
}

# Source the required modules
required_scripts <- c(
  "R/fetch_traffic_safety_data.r",
  "R/traffic_safety_integration.r",
  "R/traffic_safety_validation.r",
  "R/traffic_safety_cache.r",
  "R/traffic_safety_forecasting.r",
  "R/traffic_safety_geospatial.r"
)

for (script in required_scripts) {
  if (file.exists(script)) {
    cat(paste("Loading", script, "...\n"))
    source(script)
  } else {
    cat(paste("Warning: Script", script, "not found\n"))
  }
}

# Check if enhanced fetch function exists
if (!exists("fetch_enhanced_traffic_safety_data")) {
  cat("Enhanced traffic safety data function not found. Using basic function.\n")
}

# Test data fetching for a limited year range to be quick
test_years <- 2018:2019
cat(paste("Testing traffic safety data fetching for years:", paste(test_years, collapse=", "), "\n"))

# Test basic data fetching
basic_data <- tryCatch({
  cat("Testing basic fetch_traffic_safety_data function...\n")
  fetch_traffic_safety_data(
    years = test_years,
    refresh_cache = FALSE,
    allow_interpolation = TRUE
  )
}, error = function(e) {
  cat(paste("Error in basic data fetching:", e$message, "\n"))
  NULL
})

if (!is.null(basic_data)) {
  cat(paste("Basic data fetching successful -", nrow(basic_data), "records fetched\n"))
  cat(paste("Counties:", length(unique(basic_data$fips)), "\n"))
  cat(paste("Years:", paste(sort(unique(basic_data$year)), collapse=", "), "\n"))
  cat(paste("Variables:", paste(names(basic_data), collapse=", "), "\n"))
} else {
  cat("Basic data fetching failed\n")
}

# Test enhanced data fetching if available
if (exists("fetch_enhanced_traffic_safety_data")) {
  enhanced_data <- tryCatch({
    cat("Testing enhanced fetch_enhanced_traffic_safety_data function...\n")
    fetch_enhanced_traffic_safety_data(
      years = test_years,
      use_validation = TRUE,
      use_optimized_cache = TRUE,
      generate_forecasts = TRUE,
      spatial_analysis = TRUE
    )
  }, error = function(e) {
    cat(paste("Error in enhanced data fetching:", e$message, "\n"))
    NULL
  })
  
  if (!is.null(enhanced_data)) {
    cat(paste("Enhanced data fetching successful -", nrow(enhanced_data), "records fetched\n"))
    cat(paste("Counties:", length(unique(enhanced_data$fips)), "\n"))
    cat(paste("Years:", paste(sort(unique(enhanced_data$year)), collapse=", "), "\n"))
    cat(paste("Variables:", paste(names(enhanced_data), collapse=", "), "\n"))
    
    # Check if enhancements were applied
    enhancements <- attr(enhanced_data, "enhancements")
    if (!is.null(enhancements)) {
      cat("Enhancement status:\n")
      for (name in names(enhancements)) {
        cat(paste(" -", name, ":", enhancements[[name]], "\n"))
      }
    }
    
    # Check if forecasts were generated
    forecasts <- attr(enhanced_data, "forecasts")
    if (!is.null(forecasts)) {
      cat("Forecasts generated successfully\n")
    }
    
    # Check if spatial analysis was done
    spatial <- attr(enhanced_data, "spatial")
    if (!is.null(spatial)) {
      cat("Spatial analysis performed successfully\n")
    }
  } else {
    cat("Enhanced data fetching failed\n")
  }
}

# Test validation function
if (exists("validate_traffic_safety_data") && !is.null(basic_data)) {
  cat("Testing data validation...\n")
  validation_result <- tryCatch({
    validate_traffic_safety_data(
      basic_data,
      auto_fix = TRUE,
      report_format = "markdown",
      report_file = "output/traffic_safety_validation_report.md"
    )
  }, error = function(e) {
    cat(paste("Error in data validation:", e$message, "\n"))
    NULL
  })
  
  if (!is.null(validation_result)) {
    cat(paste("Data validation successful -", 
             if(validation_result$valid) "All tests passed or fixed" 
             else "Some tests failed", "\n"))
  } else {
    cat("Data validation failed\n")
  }
}

# Test forecasting function
if (exists("generate_traffic_forecast") && !is.null(basic_data)) {
  cat("Testing forecasting...\n")
  forecast_result <- tryCatch({
    generate_traffic_forecast(
      basic_data,
      forecast_years = 3,
      method = "ensemble",
      variable_name = "traffic_fatality_rate_per_100k"
    )
  }, error = function(e) {
    cat(paste("Error in forecasting:", e$message, "\n"))
    NULL
  })
  
  if (!is.null(forecast_result)) {
    cat(paste("Forecasting successful -", nrow(forecast_result), "forecast records generated\n"))
    cat(paste("Forecast years:", paste(sort(unique(forecast_result$year)), collapse=", "), "\n"))
  } else {
    cat("Forecasting failed\n")
  }
  
  # Test combining historical and forecast data
  if (!is.null(forecast_result) && exists("combine_historical_and_forecast")) {
    cat("Testing combining historical and forecast data...\n")
    combined_result <- tryCatch({
      combine_historical_and_forecast(
        basic_data,
        forecast_result,
        variable_name = "traffic_fatality_rate_per_100k"
      )
    }, error = function(e) {
      cat(paste("Error in combining data:", e$message, "\n"))
      NULL
    })
    
    if (!is.null(combined_result)) {
      cat(paste("Data combining successful -", nrow(combined_result), "total records\n"))
      cat(paste("Combined years:", paste(sort(unique(combined_result$year)), collapse=", "), "\n"))
    } else {
      cat("Data combining failed\n")
    }
  }
}

# Test geospatial analysis
if (exists("analyze_traffic_safety_spatial") && !is.null(basic_data)) {
  cat("Testing geospatial analysis...\n")
  spatial_result <- tryCatch({
    analyze_traffic_safety_spatial(
      basic_data,
      variable_name = "traffic_fatality_rate_per_100k",
      year = max(basic_data$year),
      method = "moran"
    )
  }, error = function(e) {
    cat(paste("Error in geospatial analysis:", e$message, "\n"))
    NULL
  })
  
  if (!is.null(spatial_result)) {
    cat("Geospatial analysis successful\n")
    
    # Check global spatial autocorrelation results
    if (!is.null(spatial_result$global$moran)) {
      moran_i <- spatial_result$global$moran$estimate[1]
      p_value <- spatial_result$global$moran$p.value
      cat(paste("Moran's I:", round(moran_i, 4), "p-value:", round(p_value, 4), "\n"))
    }
  } else {
    cat("Geospatial analysis failed\n")
  }
  
  # Test hotspot identification
  if (exists("identify_traffic_safety_hotspots")) {
    cat("Testing hotspot identification...\n")
    hotspot_result <- tryCatch({
      identify_traffic_safety_hotspots(
        basic_data,
        variable_name = "traffic_fatality_rate_per_100k",
        year = max(basic_data$year),
        method = "lisa"
      )
    }, error = function(e) {
      cat(paste("Error in hotspot identification:", e$message, "\n"))
      NULL
    })
    
    if (!is.null(hotspot_result)) {
      cat(paste("Hotspot identification successful -", nrow(hotspot_result), "counties analyzed\n"))
      
      # Count of each cluster type
      cluster_counts <- table(hotspot_result$cluster_type)
      cat("Cluster types:\n")
      for (cluster in names(cluster_counts)) {
        cat(paste(" -", cluster, ":", cluster_counts[cluster], "\n"))
      }
    } else {
      cat("Hotspot identification failed\n")
    }
  }
  
  # Test map creation if tmap is available
  if (exists("create_choropleth_map") && requireNamespace("tmap", quietly = TRUE)) {
    cat("Testing choropleth map creation...\n")
    map_result <- tryCatch({
      create_choropleth_map(
        basic_data,
        variable_name = "traffic_fatality_rate_per_100k",
        year = max(basic_data$year),
        interactive = FALSE,
        output_file = "output/traffic_fatality_map.png"
      )
    }, error = function(e) {
      cat(paste("Error in map creation:", e$message, "\n"))
      NULL
    })
    
    if (!is.null(map_result) && file.exists("output/traffic_fatality_map.png")) {
      cat("Choropleth map creation successful\n")
    } else {
      cat("Choropleth map creation failed\n")
    }
  }
}

cat("\nTraffic safety module testing complete.\n")