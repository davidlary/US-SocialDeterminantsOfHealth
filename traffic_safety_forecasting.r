#!/usr/bin/env Rscript

# Traffic Safety Forecasting Module
# This module provides forecasting capabilities for traffic safety data

# Required packages
required_packages <- c(
  "tidyverse",
  "forecast",
  "zoo",
  "tseries"
)

# Load required packages
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    message(paste("Required package", pkg, "is not installed."))
    message("Please run 'Rscript R/install_packages.r' first.")
    # Don't stop execution, just warn and continue with reduced functionality
  }
}

# Try to load additional forecasting packages if available
has_prophet <- require("prophet", quietly = TRUE)
has_h2o <- require("h2o", quietly = TRUE)

#' Generate traffic safety forecasts
#'
#' @param traffic_data Traffic safety data frame
#' @param forecast_years Number of years to forecast
#' @param method Forecasting method to use
#' @param variable_name Variable to forecast
#' @param min_years Minimum years of historical data required
#' @param coverage Forecast coverage (e.g., 0.95 for 95% CI)
#' @param ensemble_weights Named vector of weights for ensemble methods
#'
#' @return A data frame with forecasts
#' @export
generate_traffic_forecast <- function(traffic_data, 
                                     forecast_years = 5, 
                                     method = c("auto", "arima", "ets", "prophet", "ensemble"),
                                     variable_name = "traffic_fatality_rate_per_100k",
                                     min_years = 3,
                                     coverage = 0.95,
                                     ensemble_weights = NULL) {
  
  # Check inputs
  method <- match.arg(method)
  
  # Make sure we have the forecast variable
  if (!variable_name %in% names(traffic_data)) {
    stop("Forecast variable '", variable_name, "' not found in data")
  }
  
  # Make sure we have the needed columns
  if (!all(c("fips", "year") %in% names(traffic_data))) {
    stop("Required columns 'fips' and 'year' not found in data")
  }
  
  # Get all unique counties
  counties <- unique(traffic_data$fips)
  
  # Check if we have enough data
  years_per_county <- traffic_data %>%
    group_by(fips) %>%
    summarize(
      years_count = n_distinct(year),
      earliest_year = min(year, na.rm = TRUE),
      latest_year = max(year, na.rm = TRUE)
    )
  
  # Filter to counties with enough historical data
  counties_with_data <- years_per_county %>%
    filter(years_count >= min_years) %>%
    pull(fips)
  
  if (length(counties_with_data) == 0) {
    warning("No counties have enough historical data for forecasting (min_years=", min_years, ")")
    return(NULL)
  }
  
  # Determine forecasting method
  if (method == "prophet" && !has_prophet) {
    warning("Prophet package not available. Falling back to ARIMA.")
    method <- "arima"
  }
  
  if (method == "auto") {
    # Auto selection based on available packages and data characteristics
    if (has_prophet && min_years >= 4) {
      method <- "prophet"
    } else if (has_h2o && min_years >= 5) {
      method <- "h2o"
    } else {
      method <- "ensemble"  # Default to ensemble of ARIMA and ETS
    }
  }
  
  # Set ensemble weights if not provided
  if (method == "ensemble" && is.null(ensemble_weights)) {
    if (min_years >= 10) {
      # With lots of data, give more weight to ARIMA
      ensemble_weights <- c(arima = 0.6, ets = 0.3, naive = 0.1)
    } else if (min_years >= 5) {
      # With moderate data, balance ARIMA and ETS
      ensemble_weights <- c(arima = 0.4, ets = 0.4, naive = 0.2)
    } else {
      # With little data, give more weight to ETS and naive
      ensemble_weights <- c(arima = 0.2, ets = 0.5, naive = 0.3)
    }
  }
  
  # Get the maximum year in the data for each county (starting point for forecasts)
  max_years <- traffic_data %>%
    group_by(fips) %>%
    summarize(max_year = max(year, na.rm = TRUE))
  
  # Initialize a list to store forecasts
  county_forecasts <- list()
  
  # Process each county
  for (county_fips in counties_with_data) {
    # Get data for this county
    county_data <- traffic_data %>%
      filter(fips == county_fips) %>%
      arrange(year)
    
    # Get the forecast variable data
    county_ts_data <- county_data[[variable_name]]
    
    # Check for missing values
    if (sum(is.na(county_ts_data)) > length(county_ts_data) / 3) {
      # Skip counties with too many missing values
      next
    }
    
    # Try to fill in missing values using interpolation if needed
    if (any(is.na(county_ts_data))) {
      county_ts_data <- na.approx(county_ts_data, na.rm = FALSE)
      # Check if there are still NAs at the start or end
      if (any(is.na(county_ts_data))) {
        # Skip counties with missing values that couldn't be interpolated
        next
      }
    }
    
    # Create a time series object
    county_ts <- ts(county_ts_data, 
                   start = min(county_data$year), 
                   frequency = 1)  # Annual data
    
    # Get the maximum year for this county
    max_year <- max_years$max_year[max_years$fips == county_fips]
    
    # Generate forecast
    forecast_result <- tryCatch({
      if (method == "arima") {
        # ARIMA forecast
        arima_model <- auto.arima(county_ts)
        forecast(arima_model, h = forecast_years, level = coverage * 100)
      } else if (method == "ets") {
        # ETS forecast
        ets_model <- ets(county_ts)
        forecast(ets_model, h = forecast_years, level = coverage * 100)
      } else if (method == "prophet") {
        # Prophet forecast
        # Create prophet data frame
        prophet_df <- data.frame(
          ds = as.Date(paste0(county_data$year, "-01-01")),
          y = county_ts_data
        )
        
        # Fit prophet model
        model <- prophet::prophet(prophet_df)
        
        # Create future data frame
        future <- prophet::make_future_dataframe(model, periods = forecast_years, 
                                              freq = "year")
        
        # Generate forecast
        prophet_forecast <- prophet::predict(model, future)
        
        # Convert back to forecast object format
        forecast_years_seq <- seq(max_year + 1, max_year + forecast_years)
        fc <- list(
          mean = prophet_forecast$yhat[(length(county_ts) + 1):(length(county_ts) + forecast_years)],
          lower = prophet_forecast$yhat_lower[(length(county_ts) + 1):(length(county_ts) + forecast_years)],
          upper = prophet_forecast$yhat_upper[(length(county_ts) + 1):(length(county_ts) + forecast_years)]
        )
        class(fc) <- "forecast"
        fc
      } else if (method == "ensemble") {
        # Ensemble forecast (weighted average)
        
        # ARIMA
        arima_model <- tryCatch({
          auto.arima(county_ts)
        }, error = function(e) {
          # Fallback to simple ARIMA(1,0,0) if auto.arima fails
          arima(county_ts, order = c(1,0,0))
        })
        arima_fc <- forecast(arima_model, h = forecast_years, level = coverage * 100)
        
        # ETS
        ets_model <- tryCatch({
          ets(county_ts)
        }, error = function(e) {
          # Fallback to simple exponential smoothing if ets fails
          ses(county_ts)
        })
        ets_fc <- forecast(ets_model, h = forecast_years, level = coverage * 100)
        
        # Naive (last value)
        naive_fc <- naive(county_ts, h = forecast_years, level = coverage * 100)
        
        # Combine the forecasts using weighted average
        ensemble_mean <- ensemble_weights["arima"] * arima_fc$mean +
                       ensemble_weights["ets"] * ets_fc$mean +
                       ensemble_weights["naive"] * naive_fc$mean
        
        # For prediction intervals, use the widest of the models
        ensemble_lower <- pmin(
          arima_fc$lower[,1],
          ets_fc$lower[,1],
          naive_fc$lower[,1]
        )
        
        ensemble_upper <- pmax(
          arima_fc$upper[,1],
          ets_fc$upper[,1],
          naive_fc$upper[,1]
        )
        
        # Create a forecast object
        fc <- list(
          mean = ensemble_mean,
          lower = ensemble_lower,
          upper = ensemble_upper,
          method = "Ensemble"
        )
        class(fc) <- "forecast"
        fc
      }
    }, error = function(e) {
      message("Error forecasting county ", county_fips, ": ", e$message)
      return(NULL)
    })
    
    # Skip if forecast failed
    if (is.null(forecast_result)) {
      next
    }
    
    # Convert forecast to data frame
    forecast_df <- data.frame(
      fips = county_fips,
      year = seq(max_year + 1, max_year + forecast_years),
      forecast = as.numeric(forecast_result$mean),
      lower = as.numeric(forecast_result$lower[,1]),
      upper = as.numeric(forecast_result$upper[,1]),
      method = method,
      variable = variable_name
    )
    
    # Add county name if available
    if ("county_name" %in% names(county_data)) {
      forecast_df$county_name <- county_data$county_name[1]
    }
    
    # Store the forecast
    county_forecasts[[county_fips]] <- forecast_df
  }
  
  # Combine all forecasts
  if (length(county_forecasts) == 0) {
    warning("No valid forecasts could be generated")
    return(NULL)
  }
  
  all_forecasts <- bind_rows(county_forecasts)
  
  # Add data quality flag
  all_forecasts[[paste0(variable_name, "_data_quality")]] <- "forecast"
  
  # Rename forecast column to match the variable name
  all_forecasts <- all_forecasts %>%
    rename(!!sym(variable_name) := forecast) %>%
    rename(!!sym(paste0(variable_name, "_lower")) := lower) %>%
    rename(!!sym(paste0(variable_name, "_upper")) := upper)
  
  return(all_forecasts)
}

#' Generate a combined dataset with historical and forecast data
#'
#' @param historical_data Historical traffic safety data
#' @param forecast_data Forecast data generated by generate_traffic_forecast
#' @param variable_name Variable that was forecasted
#'
#' @return Combined dataset
#' @export
combine_historical_and_forecast <- function(historical_data, 
                                          forecast_data,
                                          variable_name = "traffic_fatality_rate_per_100k") {
  
  # Check that the required columns exist
  if (!all(c("fips", "year", variable_name) %in% names(historical_data))) {
    stop("Historical data missing required columns")
  }
  
  if (!all(c("fips", "year", variable_name) %in% names(forecast_data))) {
    stop("Forecast data missing required columns")
  }
  
  # Create a flag column for historical data if it doesn't exist
  quality_flag_col <- paste0(variable_name, "_data_quality")
  if (!quality_flag_col %in% names(historical_data)) {
    historical_data[[quality_flag_col]] <- "historical"
  }
  
  # Make sure forecast data has the quality flag
  if (!quality_flag_col %in% names(forecast_data)) {
    forecast_data[[quality_flag_col]] <- "forecast"
  }
  
  # Identify columns to keep from historical data
  historical_cols <- c("fips", "year", "county_name")
  historical_cols <- c(historical_cols, 
                      grep(variable_name, names(historical_data), value = TRUE))
  historical_cols <- intersect(historical_cols, names(historical_data))
  
  # Identify columns to keep from forecast data
  forecast_cols <- c("fips", "year", "county_name")
  forecast_cols <- c(forecast_cols, 
                    grep(variable_name, names(forecast_data), value = TRUE),
                    "method")
  forecast_cols <- intersect(forecast_cols, names(forecast_data))
  
  # Combine the datasets
  combined_data <- bind_rows(
    historical_data %>% select(all_of(historical_cols)),
    forecast_data %>% select(all_of(forecast_cols))
  ) %>%
    arrange(fips, year)
  
  return(combined_data)
}

#' Evaluate forecast accuracy using historical data
#'
#' @param traffic_data Historical traffic safety data
#' @param variable_name Variable to evaluate
#' @param test_years Number of years to hold out for testing
#' @param method Forecasting method to use
#'
#' @return Forecast accuracy metrics
#' @export
evaluate_forecast_accuracy <- function(traffic_data,
                                      variable_name = "traffic_fatality_rate_per_100k",
                                      test_years = 3,
                                      method = "ensemble") {
  
  # Check inputs
  if (!all(c("fips", "year", variable_name) %in% names(traffic_data))) {
    stop("Required columns not found in data")
  }
  
  # Get all unique counties
  counties <- unique(traffic_data$fips)
  
  # Check if we have enough data
  years_per_county <- traffic_data %>%
    group_by(fips) %>%
    summarize(
      years_count = n_distinct(year),
      min_year = min(year, na.rm = TRUE),
      max_year = max(year, na.rm = TRUE)
    )
  
  # Filter to counties with enough historical data for training and testing
  counties_with_data <- years_per_county %>%
    filter(years_count > test_years + 3) %>%  # Need at least a few years for training
    pull(fips)
  
  if (length(counties_with_data) == 0) {
    warning("No counties have enough historical data for evaluation")
    return(NULL)
  }
  
  # Initialize results
  accuracy_results <- data.frame()
  
  # Process each county
  for (county_fips in counties_with_data) {
    # Get data for this county
    county_data <- traffic_data %>%
      filter(fips == county_fips) %>%
      arrange(year)
    
    # Get the variable data
    county_var_data <- county_data[[variable_name]]
    
    # Skip if too many missing values
    if (sum(is.na(county_var_data)) > length(county_var_data) / 3) {
      next
    }
    
    # Try to interpolate missing values
    if (any(is.na(county_var_data))) {
      county_var_data <- na.approx(county_var_data, na.rm = FALSE)
      if (any(is.na(county_var_data))) {
        next
      }
    }
    
    # Split into training and test sets
    train_years <- sort(unique(county_data$year))[1:(length(unique(county_data$year)) - test_years)]
    test_years <- sort(unique(county_data$year))[(length(unique(county_data$year)) - test_years + 1):length(unique(county_data$year))]
    
    train_data <- county_data %>% filter(year %in% train_years)
    test_data <- county_data %>% filter(year %in% test_years)
    
    # Generate forecast using training data
    forecast_data <- tryCatch({
      generate_traffic_forecast(
        train_data,
        forecast_years = test_years, 
        method = method,
        variable_name = variable_name
      )
    }, error = function(e) {
      message("Error evaluating forecast for county ", county_fips, ": ", e$message)
      return(NULL)
    })
    
    if (is.null(forecast_data) || nrow(forecast_data) == 0) {
      next
    }
    
    # Compute accuracy metrics
    actual <- test_data[[variable_name]]
    predicted <- forecast_data[[variable_name]]
    
    # Make sure lengths match
    min_length <- min(length(actual), length(predicted))
    if (min_length == 0) next
    
    actual <- actual[1:min_length]
    predicted <- predicted[1:min_length]
    
    # Calculate metrics
    mae <- mean(abs(actual - predicted), na.rm = TRUE)
    rmse <- sqrt(mean((actual - predicted)^2, na.rm = TRUE))
    mape <- mean(abs((actual - predicted) / actual) * 100, na.rm = TRUE)
    
    # Calculate coverage of prediction intervals
    lower <- forecast_data[[paste0(variable_name, "_lower")]][1:min_length]
    upper <- forecast_data[[paste0(variable_name, "_upper")]][1:min_length]
    
    # Count how many actuals are within the prediction intervals
    in_interval <- sum(actual >= lower & actual <= upper, na.rm = TRUE)
    coverage <- in_interval / min_length
    
    # Store the results
    county_results <- data.frame(
      fips = county_fips,
      county_name = if ("county_name" %in% names(county_data)) county_data$county_name[1] else NA,
      years_data = nrow(train_data),
      years_test = min_length,
      method = method,
      mae = mae,
      rmse = rmse,
      mape = mape,
      coverage = coverage
    )
    
    accuracy_results <- bind_rows(accuracy_results, county_results)
  }
  
  # Return all accuracy results
  if (nrow(accuracy_results) == 0) {
    warning("No valid accuracy results could be calculated")
    return(NULL)
  }
  
  # Add summary row
  summary_row <- accuracy_results %>%
    summarize(
      fips = "ALL",
      county_name = "Summary",
      years_data = mean(years_data, na.rm = TRUE),
      years_test = mean(years_test, na.rm = TRUE),
      method = method,
      mae = mean(mae, na.rm = TRUE),
      rmse = mean(rmse, na.rm = TRUE),
      mape = mean(mape, na.rm = TRUE),
      coverage = mean(coverage, na.rm = TRUE)
    )
  
  accuracy_results <- bind_rows(accuracy_results, summary_row)
  
  return(accuracy_results)
}

# Let the pipeline know the module is loaded
cat("[INFO] Traffic safety forecasting module loaded successfully\n")