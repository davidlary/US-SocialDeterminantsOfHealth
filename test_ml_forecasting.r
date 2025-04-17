#!/usr/bin/env Rscript

# Test ML forecasting script that works with the test database

cat("Testing ML forecasting with minimal test database...\n")

# Source the ml_forecasting script
source("ml_forecasting.r")

# Configure for test database
config <- ml_forecast_config(
  db_path = "output/test_sdoh.duckdb",
  output_path = "output/forecasts",
  workers = 1  # Use only 1 worker for testing
)

# Use basic models that are likely to be available
config$models <- c("arima", "ets")  # Simple time series models

# Run a basic forecast
cat("Running minimal forecast for median_household_income...\n")
tryCatch({
  result <- forecast_variable(
    variable_name = "median_household_income",
    config = config,
    counties = c("06001"),  # Just test with one county
    models = c("arima", "ets")
  )
  
  if (!is.null(result) && !is.null(result$forecasts) && nrow(result$forecasts) > 0) {
    cat("Forecast successful!\n")
    cat("Generated forecasts for years:", paste(unique(result$forecasts$year), collapse = ", "), "\n")
    cat("Number of forecast rows:", nrow(result$forecasts), "\n")
    cat("Models used:", paste(unique(result$forecasts$model), collapse = ", "), "\n")
  } else {
    cat("Forecast returned empty results.\n")
  }
}, error = function(e) {
  cat("Error during forecasting:", e$message, "\n")
})

cat("ML forecasting test complete.\n")