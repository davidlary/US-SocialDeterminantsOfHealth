#!/usr/bin/env Rscript

# Machine Learning Forecasting for SDOH Data
# This script provides advanced machine learning-based forecasting capabilities
# for predicting future values of SDOH variables at the county level.
#
# Key Features:
# - Time series forecasting with multiple ML algorithms
# - Ensemble forecasting for improved accuracy
# - Model evaluation and selection
# - Interactive visualizations
# - Map generation for forecast visualization
# - Database integration for storing forecasts
# - Model explainability
#
# Usage:
# 1. Run interactively: source("R/ml_forecasting.r")
# 2. Run from command line: Rscript R/ml_forecasting.r [arguments]
#
# Example:
# Rscript R/ml_forecasting.r --variables median_household_income,poverty_rate --horizon 10

# Load required packages
required_packages <- c(
  "tidyverse", "forecast", "prophet", "xgboost", "zoo", "lubridate", 
  "tidymodels", "rsample", "recipes", "modeltime", "DBI", "duckdb",
  "future", "future.apply", "progressr", "parsnip", "workflows",
  "e1071", "randomForest", "glmnet", "caret", "kernlab", "Metrics",
  "iml", "sf", "plotly", "tigris", "viridis", "htmlwidgets"
)

# Install missing packages
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

# Load packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(forecast)
  library(prophet)
  library(xgboost)
  library(zoo)
  library(lubridate)
  library(tidymodels)
  library(rsample)
  library(recipes)
  library(modeltime)
  library(DBI)
  library(duckdb)
  library(future)
  library(future.apply)
  library(progressr)
  library(parsnip)
  library(workflows)
  library(Metrics)
})

#' Configure the ML forecasting environment
#'
#' @param db_path Path to the DuckDB database
#' @param output_path Path to save forecasting outputs
#' @param workers Number of parallel workers (default: auto-detect)
#' @param cache_dir Directory for caching intermediate results
#' @param verbose Whether to display detailed output
#'
#' @return Configuration settings list
ml_forecast_config <- function(
    db_path = "output/us_county_sdoh_unified.duckdb",
    output_path = "output/forecasts",
    workers = NULL,
    cache_dir = "data/cache/ml_forecast",
    verbose = TRUE
) {
  # Auto-detect workers if not specified
  if (is.null(workers)) {
    workers <- max(1, parallel::detectCores() - 1)
    workers <- min(workers, 8)  # Cap at 8 workers to avoid excessive memory usage
  }
  
  # Configure parallel backend
  future::plan(future::multisession, workers = workers)
  
  # Configure progress reporting
  if (verbose) {
    progressr::handlers(global = TRUE)
    progressr::handlers("progress")
  }
  
  # Create required directories
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }
  
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  
  # Setup configuration
  config <- list(
    db_path = db_path,
    output_path = output_path,
    workers = workers,
    cache_dir = cache_dir,
    verbose = verbose,
    forecast_horizon = 5,  # Default forecast 5 years into the future
    train_ratio = 0.8,     # Default train/test split ratio
    models = c(
      "arima", "ets", "prophet", "xgboost", "random_forest", "glmnet"
    ),
    forecast_start_year = NULL,  # Will be determined dynamically
    forecast_end_year = NULL,    # Will be determined dynamically
    feature_vars = NULL,         # Additional feature variables (will be populated)
    auto_correlation_threshold = 0.5,  # Threshold for auto-including correlated variables
    confidence_level = 0.95      # Confidence level for prediction intervals
  )
  
  return(config)
}

#' Connect to the database and extract data for forecasting
#'
#' @param variable_name Name of the variable to forecast
#' @param config Configuration from ml_forecast_config
#' @param min_years Minimum number of years of data required
#' @param max_missing Maximum percentage of missing data allowed
#' @param exclude_simulated Whether to exclude simulated data
#'
#' @return List containing data frames and feature information
extract_forecast_data <- function(
    variable_name,
    config,
    min_years = 5,
    max_missing = 0.5,
    exclude_simulated = TRUE
) {
  # Connect to database
  con <- dbConnect(duckdb::duckdb(), dbdir = config$db_path)
  on.exit(dbDisconnect(con))
  
  # Get variable info
  var_query <- glue::glue_sql("
    SELECT * FROM variables 
    WHERE variable_name = {variable_name}
  ", .con = con)
  
  var_info <- dbGetQuery(con, var_query)
  
  if (nrow(var_info) == 0) {
    stop("Variable '", variable_name, "' not found in the database")
  }
  
  # Get data for this variable
  data_query <- glue::glue_sql("
    SELECT 
      d.geoid, 
      c.name as county_name, 
      c.state_name,
      d.year, 
      d.value,
      d.data_quality
    FROM sdoh_data d
    JOIN counties c ON d.geoid = c.geoid
    WHERE d.variable_name = {variable_name}
    ORDER BY d.geoid, d.year
  ", .con = con)
  
  data <- dbGetQuery(con, data_query)
  
  # Check if we have enough data
  if (nrow(data) == 0) {
    stop("No data found for variable '", variable_name, "'")
  }
  
  # Process the data
  data <- data %>%
    mutate(
      year = as.numeric(year),
      value = as.numeric(value)
    )
  
  # Filter out simulated data if requested
  if (exclude_simulated) {
    data <- data %>%
      filter(data_quality != "simulated")
  }
  
  # Check counties with enough data for forecasting
  counties_with_data <- data %>%
    group_by(geoid) %>%
    summarize(
      n_years = n_distinct(year),
      n_missing = sum(is.na(value)),
      min_year = min(year),
      max_year = max(year),
      missing_ratio = n_missing / n_years
    ) %>%
    filter(
      n_years >= min_years,
      missing_ratio <= max_missing
    )
  
  if (nrow(counties_with_data) == 0) {
    stop("No counties have enough data for forecasting")
  }
  
  # Filter to only counties with enough data
  data <- data %>%
    semi_join(counties_with_data, by = "geoid")
  
  # Find potential feature variables that correlate well with the target
  if (is.null(config$feature_vars)) {
    # Get correlations with other variables
    corr_query <- glue::glue_sql("
      WITH target_data AS (
        SELECT geoid, year, value
        FROM sdoh_data
        WHERE variable_name = {variable_name}
      )
      SELECT 
        v.variable_name,
        v.domain,
        COUNT(DISTINCT s.geoid) as counties_with_data,
        CORR(s.value, t.value) as correlation
      FROM sdoh_data s
      JOIN target_data t ON s.geoid = t.geoid AND s.year = t.year
      JOIN variables v ON s.variable_name = v.variable_name
      WHERE s.variable_name != {variable_name}
      GROUP BY v.variable_name, v.domain
      HAVING COUNT(DISTINCT s.geoid) >= 100
      ORDER BY ABS(correlation) DESC
      LIMIT 20
    ", .con = con)
    
    correlations <- dbGetQuery(con, corr_query)
    
    # Select features with good correlation
    features <- correlations %>%
      filter(abs(correlation) >= config$auto_correlation_threshold) %>%
      pull(variable_name)
    
    # Limit to top 5 features to avoid overfitting
    if (length(features) > 5) {
      features <- features[1:5]
    }
    
    config$feature_vars <- features
  }
  
  # Get feature data if we have any features
  feature_data <- list()
  
  if (length(config$feature_vars) > 0) {
    for (feature_var in config$feature_vars) {
      feature_query <- glue::glue_sql("
        SELECT 
          d.geoid, 
          d.year, 
          d.value
        FROM sdoh_data d
        WHERE d.variable_name = {feature_var}
        AND d.geoid IN ({counties_with_data$geoid*})
        ORDER BY d.geoid, d.year
      ", .con = con)
      
      feature_data[[feature_var]] <- dbGetQuery(con, feature_query) %>%
        rename(!!paste0(feature_var) := value)
    }
  }
  
  # Determine forecast years
  years_range <- range(data$year)
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  # Set forecast start and end years
  if (is.null(config$forecast_start_year)) {
    config$forecast_start_year <- years_range[2] + 1
  }
  
  if (is.null(config$forecast_end_year)) {
    config$forecast_end_year <- min(
      current_year + 5,  # At most 5 years into the future from current year
      config$forecast_start_year + config$forecast_horizon - 1
    )
  }
  
  # Return the data and updated config
  return(list(
    data = data,
    var_info = var_info,
    feature_data = feature_data,
    counties = counties_with_data,
    config = config
  ))
}

#' Prepare time series data for a specific county
#'
#' @param county_data Data frame containing time series for one county
#' @param feature_data List of feature data frames
#' @param county_id County GEOID
#' @param var_info Variable information
#' @param config Configuration
#'
#' @return List containing prepared training data and test data
prepare_county_timeseries <- function(
    county_data,
    feature_data,
    county_id,
    var_info,
    config
) {
  # Check if we have enough data
  if (nrow(county_data) < 5) {
    return(NULL)
  }
  
  # Sort by year
  county_data <- county_data %>%
    arrange(year)
  
  # Create a complete time series with all years
  all_years <- min(county_data$year):max(county_data$year)
  
  ts_data <- tibble(
    geoid = county_id,
    year = all_years
  ) %>%
    left_join(county_data %>% select(geoid, year, value), by = c("geoid", "year"))
  
  # Interpolate missing values if needed
  if (any(is.na(ts_data$value))) {
    # Simple linear interpolation for missing values
    ts_data$value <- zoo::na.approx(ts_data$value, na.rm = FALSE)
    
    # Use locf for any remaining NAs at the start
    ts_data$value <- zoo::na.locf(ts_data$value, fromLast = TRUE, na.rm = FALSE)
    
    # Use locf for any remaining NAs at the end
    ts_data$value <- zoo::na.locf(ts_data$value, na.rm = FALSE)
  }
  
  # Add time features
  ts_data <- ts_data %>%
    mutate(
      date = as.Date(paste0(year, "-01-01")),
      year_num = year - min(year) + 1,
      period = year_num
    )
  
  # Add feature variables if available
  if (length(feature_data) > 0) {
    for (feature_name in names(feature_data)) {
      feature_ts <- feature_data[[feature_name]] %>%
        filter(geoid == county_id) %>%
        select(year, !!feature_name)
      
      ts_data <- ts_data %>%
        left_join(feature_ts, by = "year")
      
      # Interpolate missing feature values
      if (any(is.na(ts_data[[feature_name]]))) {
        ts_data[[feature_name]] <- zoo::na.approx(
          ts_data[[feature_name]], 
          na.rm = FALSE
        )
        
        # Use locf for any remaining NAs
        ts_data[[feature_name]] <- zoo::na.locf(
          ts_data[[feature_name]], 
          fromLast = TRUE, 
          na.rm = FALSE
        )
        
        ts_data[[feature_name]] <- zoo::na.locf(
          ts_data[[feature_name]], 
          na.rm = FALSE
        )
      }
    }
  }
  
  # Split into training and testing sets
  train_size <- floor(nrow(ts_data) * config$train_ratio)
  
  if (train_size < 5) {
    # Not enough data for a meaningful train/test split
    train_data <- ts_data
    test_data <- NULL
  } else {
    train_data <- ts_data[1:train_size, ]
    test_data <- ts_data[(train_size + 1):nrow(ts_data), ]
  }
  
  # Return the prepared data
  return(list(
    train = train_data,
    test = test_data,
    full = ts_data
  ))
}

#' Fit forecasting models to time series data
#'
#' @param prepared_data Prepared time series data from prepare_county_timeseries
#' @param models Vector of model names to fit
#' @param var_info Variable information
#' @param config Configuration
#'
#' @return List of fitted models
fit_forecast_models <- function(
    prepared_data,
    models = c("arima", "ets", "prophet"),
    var_info,
    config
) {
  if (is.null(prepared_data) || nrow(prepared_data$train) < 5) {
    return(NULL)
  }
  
  train_data <- prepared_data$train
  feature_names <- setdiff(names(train_data), 
                         c("geoid", "year", "date", "value", "year_num", "period",
                           "county_name", "state_name", "data_quality"))
  
  # Initialize model list
  model_list <- list()
  
  # 1. ARIMA Model
  if ("arima" %in% models) {
    tryCatch({
      # Convert to time series object
      ts_obj <- ts(train_data$value, frequency = 1)
      
      # Fit auto ARIMA model
      arima_model <- forecast::auto.arima(
        ts_obj,
        d = 1,           # First differencing
        D = 0,           # No seasonal differencing
        stepwise = TRUE, # Use stepwise search
        approximation = TRUE,
        trace = FALSE
      )
      
      model_list$arima <- arima_model
    }, error = function(e) {
      warning("ARIMA model failed: ", e$message)
    })
  }
  
  # 2. ETS Model
  if ("ets" %in% models) {
    tryCatch({
      # Convert to time series object
      ts_obj <- ts(train_data$value, frequency = 1)
      
      # Fit ETS model
      ets_model <- forecast::ets(
        ts_obj,
        model = "ZZZ", # Auto-select model type
        damped = NULL, # Auto-select damping
        trace = FALSE
      )
      
      model_list$ets <- ets_model
    }, error = function(e) {
      warning("ETS model failed: ", e$message)
    })
  }
  
  # 3. Prophet Model
  if ("prophet" %in% models) {
    tryCatch({
      # Prepare data for Prophet
      prophet_data <- train_data %>%
        select(date, value) %>%
        rename(ds = date, y = value)
      
      # Add regressor columns if available
      for (feature in feature_names) {
        prophet_data[[feature]] <- train_data[[feature]]
      }
      
      # Configure Prophet model
      prophet_model <- prophet::prophet(
        yearly.seasonality = FALSE,
        weekly.seasonality = FALSE,
        daily.seasonality = FALSE,
        seasonality.mode = "additive",
        mcmc.samples = 0
      )
      
      # Add regressors
      for (feature in feature_names) {
        prophet_model <- prophet::add_regressor(
          prophet_model, 
          feature
        )
      }
      
      # Fit the model
      prophet_model <- prophet::fit.prophet(
        prophet_model, 
        prophet_data
      )
      
      model_list$prophet <- prophet_model
    }, error = function(e) {
      warning("Prophet model failed: ", e$message)
    })
  }
  
  # 4. XGBoost Model
  if ("xgboost" %in% models) {
    tryCatch({
      # Prepare data for XGBoost
      if (length(feature_names) > 0) {
        # We have external features
        xgb_data <- train_data %>%
          mutate(
            value_lag1 = lag(value, 1),
            value_lag2 = lag(value, 2)
          ) %>%
          filter(!is.na(value_lag2)) # Remove rows with NA lags
        
        # Feature matrix
        features <- xgb_data %>%
          select(year_num, value_lag1, value_lag2, all_of(feature_names)) %>%
          as.matrix()
        
        # Target vector
        target <- xgb_data$value
      } else {
        # Only time-based features
        xgb_data <- train_data %>%
          mutate(
            value_lag1 = lag(value, 1),
            value_lag2 = lag(value, 2),
            value_lag3 = lag(value, 3)
          ) %>%
          filter(!is.na(value_lag3)) # Remove rows with NA lags
        
        # Feature matrix
        features <- xgb_data %>%
          select(year_num, value_lag1, value_lag2, value_lag3) %>%
          as.matrix()
        
        # Target vector
        target <- xgb_data$value
      }
      
      # Convert to DMatrix
      dtrain <- xgboost::xgb.DMatrix(
        data = features,
        label = target
      )
      
      # Train XGBoost model
      xgb_params <- list(
        objective = "reg:squarederror",
        booster = "gbtree",
        eta = 0.05,
        max_depth = 4,
        min_child_weight = 1,
        subsample = 0.8,
        colsample_bytree = 0.8
      )
      
      xgb_model <- xgboost::xgb.train(
        params = xgb_params,
        data = dtrain,
        nrounds = 100,
        verbose = 0
      )
      
      # Store the model and preprocessing info
      model_list$xgboost <- list(
        model = xgb_model,
        features = colnames(features),
        last_values = tail(train_data$value, 3)
      )
    }, error = function(e) {
      warning("XGBoost model failed: ", e$message)
    })
  }
  
  # 5. Random Forest Model
  if ("random_forest" %in% models) {
    tryCatch({
      # Prepare data for Random Forest
      if (length(feature_names) > 0) {
        # We have external features
        rf_data <- train_data %>%
          mutate(
            value_lag1 = lag(value, 1),
            value_lag2 = lag(value, 2)
          ) %>%
          filter(!is.na(value_lag2)) # Remove rows with NA lags
        
        # Feature formula
        formula_str <- paste("value ~", 
                           paste(c("year_num", "value_lag1", "value_lag2", feature_names), 
                                 collapse = " + "))
      } else {
        # Only time-based features
        rf_data <- train_data %>%
          mutate(
            value_lag1 = lag(value, 1),
            value_lag2 = lag(value, 2),
            value_lag3 = lag(value, 3)
          ) %>%
          filter(!is.na(value_lag3)) # Remove rows with NA lags
        
        # Feature formula
        formula_str <- "value ~ year_num + value_lag1 + value_lag2 + value_lag3"
      }
      
      # Train Random Forest model
      rf_model <- randomForest::randomForest(
        formula = as.formula(formula_str),
        data = rf_data,
        ntree = 100,
        mtry = floor(sqrt(ncol(rf_data) - 1)),
        importance = TRUE
      )
      
      # Store the model and preprocessing info
      model_list$random_forest <- list(
        model = rf_model,
        formula = formula_str,
        last_values = tail(train_data$value, 3)
      )
    }, error = function(e) {
      warning("Random Forest model failed: ", e$message)
    })
  }
  
  # 6. Elastic Net Model
  if ("glmnet" %in% models) {
    tryCatch({
      # Prepare data for Elastic Net
      if (length(feature_names) > 0) {
        # We have external features
        glmnet_data <- train_data %>%
          mutate(
            value_lag1 = lag(value, 1),
            value_lag2 = lag(value, 2)
          ) %>%
          filter(!is.na(value_lag2)) # Remove rows with NA lags
        
        # Feature matrix
        x <- glmnet_data %>%
          select(year_num, value_lag1, value_lag2, all_of(feature_names)) %>%
          as.matrix()
      } else {
        # Only time-based features
        glmnet_data <- train_data %>%
          mutate(
            value_lag1 = lag(value, 1),
            value_lag2 = lag(value, 2),
            value_lag3 = lag(value, 3)
          ) %>%
          filter(!is.na(value_lag3)) # Remove rows with NA lags
        
        # Feature matrix
        x <- glmnet_data %>%
          select(year_num, value_lag1, value_lag2, value_lag3) %>%
          as.matrix()
      }
      
      # Target vector
      y <- glmnet_data$value
      
      # Train Elastic Net model
      cv_model <- glmnet::cv.glmnet(
        x = x,
        y = y,
        alpha = 0.5,  # Elastic Net mixing (0=ridge, 1=lasso)
        nfolds = min(10, nrow(x)),
        standardize = TRUE
      )
      
      # Store the model and preprocessing info
      model_list$glmnet <- list(
        model = cv_model,
        features = colnames(x),
        last_values = tail(train_data$value, 3)
      )
    }, error = function(e) {
      warning("Elastic Net model failed: ", e$message)
    })
  }
  
  return(model_list)
}

#' Generate forecasts using fitted models
#'
#' @param fitted_models List of fitted models from fit_forecast_models
#' @param prepared_data Prepared time series data
#' @param horizon Number of periods to forecast
#' @param var_info Variable information
#' @param config Configuration
#'
#' @return Dataframe with forecasts and prediction intervals
generate_forecasts <- function(
    fitted_models,
    prepared_data,
    horizon = 5,
    var_info,
    config
) {
  if (is.null(fitted_models) || length(fitted_models) == 0) {
    return(NULL)
  }
  
  # Get the last observed year
  last_year <- max(prepared_data$full$year)
  forecast_years <- (last_year + 1):(last_year + horizon)
  
  # Create a data frame for future periods
  future_df <- tibble(
    year = forecast_years,
    year_num = max(prepared_data$full$year_num) + seq_along(forecast_years),
    date = as.Date(paste0(forecast_years, "-01-01")),
    period = max(prepared_data$full$period) + seq_along(forecast_years)
  )
  
  # Add the county ID
  future_df$geoid <- prepared_data$full$geoid[1]
  
  # Add feature variables to future data frame if they were used
  feature_names <- setdiff(names(prepared_data$full), 
                         c("geoid", "year", "date", "value", "year_num", "period",
                           "county_name", "state_name", "data_quality"))
  
  if (length(feature_names) > 0) {
    # We need to handle feature variables for future periods
    # For simplicity, we'll just use the last known value
    # In a real implementation, you might want to forecast these too
    for (feature in feature_names) {
      future_df[[feature]] <- rep(tail(prepared_data$full[[feature]], 1), nrow(future_df))
    }
  }
  
  # Initialize forecast results list
  forecasts <- list()
  
  # Generate forecasts for each model
  # 1. ARIMA
  if (!is.null(fitted_models$arima)) {
    tryCatch({
      arima_forecast <- forecast::forecast(
        fitted_models$arima, 
        h = horizon,
        level = config$confidence_level * 100
      )
      
      forecasts$arima <- tibble(
        year = forecast_years,
        model = "ARIMA",
        forecast = as.numeric(arima_forecast$mean),
        lower = as.numeric(arima_forecast$lower),
        upper = as.numeric(arima_forecast$upper)
      )
    }, error = function(e) {
      warning("Failed to generate ARIMA forecast: ", e$message)
    })
  }
  
  # 2. ETS
  if (!is.null(fitted_models$ets)) {
    tryCatch({
      ets_forecast <- forecast::forecast(
        fitted_models$ets, 
        h = horizon,
        level = config$confidence_level * 100
      )
      
      forecasts$ets <- tibble(
        year = forecast_years,
        model = "ETS",
        forecast = as.numeric(ets_forecast$mean),
        lower = as.numeric(ets_forecast$lower),
        upper = as.numeric(ets_forecast$upper)
      )
    }, error = function(e) {
      warning("Failed to generate ETS forecast: ", e$message)
    })
  }
  
  # 3. Prophet
  if (!is.null(fitted_models$prophet)) {
    tryCatch({
      # Prepare future dataframe for Prophet
      prophet_future <- future_df %>%
        select(date) %>%
        rename(ds = date)
      
      # Add regressor columns if used
      for (feature in feature_names) {
        prophet_future[[feature]] <- future_df[[feature]]
      }
      
      # Generate forecast
      prophet_forecast <- prophet::predict(
        fitted_models$prophet, 
        prophet_future
      )
      
      forecasts$prophet <- tibble(
        year = forecast_years,
        model = "Prophet",
        forecast = prophet_forecast$yhat,
        lower = prophet_forecast$yhat_lower,
        upper = prophet_forecast$yhat_upper
      )
    }, error = function(e) {
      warning("Failed to generate Prophet forecast: ", e$message)
    })
  }
  
  # 4. XGBoost
  if (!is.null(fitted_models$xgboost)) {
    tryCatch({
      # We need to generate iterative predictions for XGBoost
      xgb_model <- fitted_models$xgboost$model
      feature_cols <- fitted_models$xgboost$features
      last_values <- fitted_models$xgboost$last_values
      
      xgb_preds <- numeric(horizon)
      xgb_lower <- numeric(horizon)
      xgb_upper <- numeric(horizon)
      
      # Initial lagged values
      value_lag1 <- tail(last_values, 1)
      value_lag2 <- tail(last_values, 2)[1]
      value_lag3 <- if(length(last_values) >= 3) tail(last_values, 3)[1] else NA
      
      for (i in 1:horizon) {
        # Prepare feature matrix for this forecast step
        if ("value_lag3" %in% feature_cols) {
          # Using 3 lags
          x_pred <- data.frame(
            year_num = future_df$year_num[i],
            value_lag1 = value_lag1,
            value_lag2 = value_lag2,
            value_lag3 = value_lag3
          )
        } else {
          # Using 2 lags
          x_pred <- data.frame(
            year_num = future_df$year_num[i],
            value_lag1 = value_lag1,
            value_lag2 = value_lag2
          )
        }
        
        # Add external features if used
        for (feature in intersect(feature_names, feature_cols)) {
          x_pred[[feature]] <- future_df[[feature]][i]
        }
        
        # Ensure same column order as training data
        x_pred <- x_pred[, feature_cols, drop = FALSE]
        
        # Generate prediction
        xgb_pred <- predict(xgb_model, as.matrix(x_pred))
        xgb_preds[i] <- xgb_pred
        
        # Simple uncertainty estimation based on prediction error
        # Could be improved with proper prediction intervals
        pred_sd <- 0.1 * abs(xgb_pred)  # Assume 10% error
        xgb_lower[i] <- xgb_pred - qnorm(config$confidence_level) * pred_sd
        xgb_upper[i] <- xgb_pred + qnorm(config$confidence_level) * pred_sd
        
        # Update lagged values for next prediction
        value_lag3 <- value_lag2
        value_lag2 <- value_lag1
        value_lag1 <- xgb_pred
      }
      
      forecasts$xgboost <- tibble(
        year = forecast_years,
        model = "XGBoost",
        forecast = xgb_preds,
        lower = xgb_lower,
        upper = xgb_upper
      )
    }, error = function(e) {
      warning("Failed to generate XGBoost forecast: ", e$message)
    })
  }
  
  # 5. Random Forest
  if (!is.null(fitted_models$random_forest)) {
    tryCatch({
      # We need to generate iterative predictions for Random Forest
      rf_model <- fitted_models$random_forest$model
      formula_str <- fitted_models$random_forest$formula
      last_values <- fitted_models$random_forest$last_values
      
      rf_preds <- numeric(horizon)
      rf_lower <- numeric(horizon)
      rf_upper <- numeric(horizon)
      
      # Initial lagged values
      value_lag1 <- tail(last_values, 1)
      value_lag2 <- tail(last_values, 2)[1]
      value_lag3 <- if(length(last_values) >= 3) tail(last_values, 3)[1] else NA
      
      for (i in 1:horizon) {
        # Prepare data for this forecast step
        if (grepl("value_lag3", formula_str)) {
          # Using 3 lags
          pred_data <- data.frame(
            year_num = future_df$year_num[i],
            value_lag1 = value_lag1,
            value_lag2 = value_lag2,
            value_lag3 = value_lag3
          )
        } else {
          # Using 2 lags
          pred_data <- data.frame(
            year_num = future_df$year_num[i],
            value_lag1 = value_lag1,
            value_lag2 = value_lag2
          )
        }
        
        # Add external features if used
        for (feature in feature_names) {
          if (grepl(feature, formula_str)) {
            pred_data[[feature]] <- future_df[[feature]][i]
          }
        }
        
        # Generate prediction
        rf_pred <- predict(rf_model, newdata = pred_data)
        rf_preds[i] <- rf_pred
        
        # Use prediction intervals from random forest if available
        # Otherwise use a simple approximation
        if ("quantreg" %in% class(rf_model) || "ranger" %in% class(rf_model)) {
          # Use built-in prediction intervals if available
          pred_intervals <- predict(rf_model, newdata = pred_data, quantiles = c(0.025, 0.975))
          rf_lower[i] <- pred_intervals$predictions[1]
          rf_upper[i] <- pred_intervals$predictions[2]
        } else {
          # Simple uncertainty estimation based on OOB error
          pred_sd <- sqrt(rf_model$mse[length(rf_model$mse)])
          rf_lower[i] <- rf_pred - qnorm(config$confidence_level) * pred_sd
          rf_upper[i] <- rf_pred + qnorm(config$confidence_level) * pred_sd
        }
        
        # Update lagged values for next prediction
        value_lag3 <- value_lag2
        value_lag2 <- value_lag1
        value_lag1 <- rf_pred
      }
      
      forecasts$random_forest <- tibble(
        year = forecast_years,
        model = "Random Forest",
        forecast = rf_preds,
        lower = rf_lower,
        upper = rf_upper
      )
    }, error = function(e) {
      warning("Failed to generate Random Forest forecast: ", e$message)
    })
  }
  
  # 6. Elastic Net
  if (!is.null(fitted_models$glmnet)) {
    tryCatch({
      # We need to generate iterative predictions for Elastic Net
      glmnet_model <- fitted_models$glmnet$model
      feature_cols <- fitted_models$glmnet$features
      last_values <- fitted_models$glmnet$last_values
      
      glmnet_preds <- numeric(horizon)
      glmnet_lower <- numeric(horizon)
      glmnet_upper <- numeric(horizon)
      
      # Initial lagged values
      value_lag1 <- tail(last_values, 1)
      value_lag2 <- tail(last_values, 2)[1]
      value_lag3 <- if(length(last_values) >= 3) tail(last_values, 3)[1] else NA
      
      for (i in 1:horizon) {
        # Prepare feature matrix for this forecast step
        if ("value_lag3" %in% feature_cols) {
          # Using 3 lags
          x_pred <- data.frame(
            year_num = future_df$year_num[i],
            value_lag1 = value_lag1,
            value_lag2 = value_lag2,
            value_lag3 = value_lag3
          )
        } else {
          # Using 2 lags
          x_pred <- data.frame(
            year_num = future_df$year_num[i],
            value_lag1 = value_lag1,
            value_lag2 = value_lag2
          )
        }
        
        # Add external features if used
        for (feature in intersect(feature_names, feature_cols)) {
          x_pred[[feature]] <- future_df[[feature]][i]
        }
        
        # Ensure same column order as training data
        x_pred <- x_pred[, feature_cols, drop = FALSE]
        
        # Generate prediction
        glmnet_pred <- predict(glmnet_model, newx = as.matrix(x_pred), s = "lambda.min")
        glmnet_preds[i] <- as.numeric(glmnet_pred)
        
        # Simple uncertainty estimation
        pred_sd <- glmnet_model$cvsd[which(glmnet_model$lambda == glmnet_model$lambda.min)]
        glmnet_lower[i] <- glmnet_preds[i] - qnorm(config$confidence_level) * pred_sd
        glmnet_upper[i] <- glmnet_preds[i] + qnorm(config$confidence_level) * pred_sd
        
        # Update lagged values for next prediction
        value_lag3 <- value_lag2
        value_lag2 <- value_lag1
        value_lag1 <- glmnet_preds[i]
      }
      
      forecasts$glmnet <- tibble(
        year = forecast_years,
        model = "Elastic Net",
        forecast = glmnet_preds,
        lower = glmnet_lower,
        upper = glmnet_upper
      )
    }, error = function(e) {
      warning("Failed to generate Elastic Net forecast: ", e$message)
    })
  }
  
  # Combine all forecasts
  all_forecasts <- bind_rows(forecasts)
  
  # Calculate ensemble forecast (simple average of all model forecasts)
  if (nrow(all_forecasts) > 0) {
    ensemble_forecast <- all_forecasts %>%
      group_by(year) %>%
      summarize(
        model = "Ensemble",
        forecast = mean(forecast),
        lower = mean(lower),
        upper = mean(upper)
      )
    
    all_forecasts <- bind_rows(all_forecasts, ensemble_forecast)
  }
  
  # Add county information to forecasts
  if (nrow(all_forecasts) > 0) {
    all_forecasts$geoid <- prepared_data$full$geoid[1]
    all_forecasts$county_name <- ifelse(
      "county_name" %in% names(prepared_data$full),
      prepared_data$full$county_name[1],
      NA
    )
    all_forecasts$state_name <- ifelse(
      "state_name" %in% names(prepared_data$full),
      prepared_data$full$state_name[1],
      NA
    )
    
    # Add variable name
    all_forecasts$variable_name <- var_info$variable_name[1]
    
    # Add data quality flag for forecasts
    all_forecasts$data_quality <- "forecasted"
  }
  
  return(all_forecasts)
}

#' Evaluate model performance on test data
#'
#' @param fitted_models Fitted models from fit_forecast_models
#' @param prepared_data Prepared time series data
#' @param var_info Variable information
#'
#' @return Data frame with evaluation metrics
evaluate_models <- function(
    fitted_models,
    prepared_data,
    var_info
) {
  if (is.null(fitted_models) || is.null(prepared_data$test) || nrow(prepared_data$test) == 0) {
    return(NULL)
  }
  
  # Get test data
  test_data <- prepared_data$test
  train_data <- prepared_data$train
  
  # Initialize results list
  eval_results <- list()
  
  # 1. Evaluate ARIMA
  if (!is.null(fitted_models$arima)) {
    tryCatch({
      # Generate forecast for test period
      arima_forecast <- forecast::forecast(
        fitted_models$arima, 
        h = nrow(test_data)
      )
      
      # Extract predictions
      arima_preds <- as.numeric(arima_forecast$mean)
      
      # Calculate metrics
      arima_metrics <- tibble(
        model = "ARIMA",
        rmse = Metrics::rmse(test_data$value, arima_preds),
        mae = Metrics::mae(test_data$value, arima_preds),
        mape = Metrics::mape(test_data$value, arima_preds) * 100
      )
      
      eval_results$arima <- arima_metrics
    }, error = function(e) {
      warning("Failed to evaluate ARIMA model: ", e$message)
    })
  }
  
  # 2. Evaluate ETS
  if (!is.null(fitted_models$ets)) {
    tryCatch({
      # Generate forecast for test period
      ets_forecast <- forecast::forecast(
        fitted_models$ets, 
        h = nrow(test_data)
      )
      
      # Extract predictions
      ets_preds <- as.numeric(ets_forecast$mean)
      
      # Calculate metrics
      ets_metrics <- tibble(
        model = "ETS",
        rmse = Metrics::rmse(test_data$value, ets_preds),
        mae = Metrics::mae(test_data$value, ets_preds),
        mape = Metrics::mape(test_data$value, ets_preds) * 100
      )
      
      eval_results$ets <- ets_metrics
    }, error = function(e) {
      warning("Failed to evaluate ETS model: ", e$message)
    })
  }
  
  # 3. Evaluate Prophet
  if (!is.null(fitted_models$prophet)) {
    tryCatch({
      # Prepare test dataframe for Prophet
      prophet_test <- test_data %>%
        select(date) %>%
        rename(ds = date)
      
      # Add regressor columns if used
      feature_names <- setdiff(names(test_data), 
                             c("geoid", "year", "date", "value", "year_num", "period",
                               "county_name", "state_name", "data_quality"))
      
      for (feature in feature_names) {
        prophet_test[[feature]] <- test_data[[feature]]
      }
      
      # Generate forecast
      prophet_forecast <- prophet::predict(
        fitted_models$prophet, 
        prophet_test
      )
      
      # Extract predictions
      prophet_preds <- prophet_forecast$yhat
      
      # Calculate metrics
      prophet_metrics <- tibble(
        model = "Prophet",
        rmse = Metrics::rmse(test_data$value, prophet_preds),
        mae = Metrics::mae(test_data$value, prophet_preds),
        mape = Metrics::mape(test_data$value, prophet_preds) * 100
      )
      
      eval_results$prophet <- prophet_metrics
    }, error = function(e) {
      warning("Failed to evaluate Prophet model: ", e$message)
    })
  }
  
  # 4. Evaluate XGBoost
  if (!is.null(fitted_models$xgboost)) {
    tryCatch({
      # Prepare test features
      xgb_model <- fitted_models$xgboost$model
      feature_cols <- fitted_models$xgboost$features
      
      # Check if we have the right features in test data
      if (all(feature_cols %in% names(test_data))) {
        # Extract features
        x_test <- test_data %>%
          select(all_of(feature_cols)) %>%
          as.matrix()
        
        # Generate predictions
        xgb_preds <- predict(xgb_model, x_test)
        
        # Calculate metrics
        xgb_metrics <- tibble(
          model = "XGBoost",
          rmse = Metrics::rmse(test_data$value, xgb_preds),
          mae = Metrics::mae(test_data$value, xgb_preds),
          mape = Metrics::mape(test_data$value, xgb_preds) * 100
        )
        
        eval_results$xgboost <- xgb_metrics
      }
    }, error = function(e) {
      warning("Failed to evaluate XGBoost model: ", e$message)
    })
  }
  
  # 5. Evaluate Random Forest
  if (!is.null(fitted_models$random_forest)) {
    tryCatch({
      # Generate predictions
      rf_preds <- predict(fitted_models$random_forest$model, newdata = test_data)
      
      # Calculate metrics
      rf_metrics <- tibble(
        model = "Random Forest",
        rmse = Metrics::rmse(test_data$value, rf_preds),
        mae = Metrics::mae(test_data$value, rf_preds),
        mape = Metrics::mape(test_data$value, rf_preds) * 100
      )
      
      eval_results$random_forest <- rf_metrics
    }, error = function(e) {
      warning("Failed to evaluate Random Forest model: ", e$message)
    })
  }
  
  # 6. Evaluate Elastic Net
  if (!is.null(fitted_models$glmnet)) {
    tryCatch({
      # Prepare test features
      glmnet_model <- fitted_models$glmnet$model
      feature_cols <- fitted_models$glmnet$features
      
      # Check if we have the right features in test data
      if (all(feature_cols %in% names(test_data))) {
        # Extract features
        x_test <- test_data %>%
          select(all_of(feature_cols)) %>%
          as.matrix()
        
        # Generate predictions
        glmnet_preds <- predict(glmnet_model, newx = x_test, s = "lambda.min")
        
        # Calculate metrics
        glmnet_metrics <- tibble(
          model = "Elastic Net",
          rmse = Metrics::rmse(test_data$value, as.numeric(glmnet_preds)),
          mae = Metrics::mae(test_data$value, as.numeric(glmnet_preds)),
          mape = Metrics::mape(test_data$value, as.numeric(glmnet_preds)) * 100
        )
        
        eval_results$glmnet <- glmnet_metrics
      }
    }, error = function(e) {
      warning("Failed to evaluate Elastic Net model: ", e$message)
    })
  }
  
  # Combine all evaluation results
  all_eval <- bind_rows(eval_results)
  
  # Calculate ensemble metrics
  if (nrow(all_eval) > 1) {
    # Calculate ensemble predictions as average of all model predictions
    ensemble_preds <- numeric(nrow(test_data))
    
    # ARIMA predictions
    if (!is.null(fitted_models$arima)) {
      arima_forecast <- forecast::forecast(fitted_models$arima, h = nrow(test_data))
      ensemble_preds <- ensemble_preds + as.numeric(arima_forecast$mean)
    }
    
    # ETS predictions
    if (!is.null(fitted_models$ets)) {
      ets_forecast <- forecast::forecast(fitted_models$ets, h = nrow(test_data))
      ensemble_preds <- ensemble_preds + as.numeric(ets_forecast$mean)
    }
    
    # Prophet predictions
    if (!is.null(fitted_models$prophet)) {
      prophet_test <- test_data %>%
        select(date) %>%
        rename(ds = date)
      
      # Add regressor columns if used
      feature_names <- setdiff(names(test_data), 
                             c("geoid", "year", "date", "value", "year_num", "period",
                               "county_name", "state_name", "data_quality"))
      
      for (feature in feature_names) {
        prophet_test[[feature]] <- test_data[[feature]]
      }
      
      prophet_forecast <- prophet::predict(fitted_models$prophet, prophet_test)
      ensemble_preds <- ensemble_preds + prophet_forecast$yhat
    }
    
    # Average the predictions
    model_count <- nrow(all_eval)
    ensemble_preds <- ensemble_preds / model_count
    
    # Calculate ensemble metrics
    ensemble_metrics <- tibble(
      model = "Ensemble",
      rmse = Metrics::rmse(test_data$value, ensemble_preds),
      mae = Metrics::mae(test_data$value, ensemble_preds),
      mape = Metrics::mape(test_data$value, ensemble_preds) * 100
    )
    
    all_eval <- bind_rows(all_eval, ensemble_metrics)
  }
  
  # Add variable name and county info
  if (nrow(all_eval) > 0) {
    all_eval$variable_name <- var_info$variable_name[1]
    all_eval$geoid <- prepared_data$full$geoid[1]
    all_eval$county_name <- ifelse(
      "county_name" %in% names(prepared_data$full),
      prepared_data$full$county_name[1],
      NA
    )
    all_eval$state_name <- ifelse(
      "state_name" %in% names(prepared_data$full),
      prepared_data$full$state_name[1],
      NA
    )
  }
  
  return(all_eval)
}

#' Generate forecasts for a variable across all counties
#'
#' @param variable_name Name of the variable to forecast
#' @param config Configuration from ml_forecast_config
#' @param counties Specific counties to forecast or NULL for all counties
#' @param models Vector of model names to use
#' @param horizon Number of years to forecast
#'
#' @return List containing forecasts and evaluations
forecast_variable <- function(
    variable_name,
    config = ml_forecast_config(),
    counties = NULL,
    models = config$models,
    horizon = config$forecast_horizon
) {
  # Extract data for this variable
  config$forecast_horizon <- horizon
  var_data <- extract_forecast_data(variable_name, config)
  
  if (is.null(var_data)) {
    return(NULL)
  }
  
  # Filter to specific counties if requested
  if (!is.null(counties)) {
    var_data$counties <- var_data$counties %>%
      filter(geoid %in% counties)
    
    var_data$data <- var_data$data %>%
      filter(geoid %in% counties)
  }
  
  # Get county IDs
  county_ids <- var_data$counties$geoid
  
  if (length(county_ids) == 0) {
    return(NULL)
  }
  
  # Initialize progress reporting
  if (config$verbose) {
    p <- progressr::progressor(length(county_ids))
  }
  
  # Process each county in parallel
  county_results <- future.apply::future_lapply(
    county_ids,
    function(county_id) {
      # Extract data for this county
      county_data <- var_data$data %>%
        filter(geoid == county_id)
      
      # Extract feature data for this county
      county_features <- lapply(var_data$feature_data, function(feature_df) {
        feature_df %>% filter(geoid == county_id)
      })
      
      # Prepare time series
      ts_data <- prepare_county_timeseries(
        county_data,
        county_features,
        county_id,
        var_data$var_info,
        var_data$config
      )
      
      # Skip if we don't have enough data
      if (is.null(ts_data)) {
        if (config$verbose) p()
        return(NULL)
      }
      
      # Fit models
      fitted_models <- fit_forecast_models(
        ts_data,
        models,
        var_data$var_info,
        var_data$config
      )
      
      # Skip if no models could be fit
      if (is.null(fitted_models) || length(fitted_models) == 0) {
        if (config$verbose) p()
        return(NULL)
      }
      
      # Generate forecasts
      forecasts <- generate_forecasts(
        fitted_models,
        ts_data,
        horizon,
        var_data$var_info,
        var_data$config
      )
      
      # Evaluate models
      evaluation <- evaluate_models(
        fitted_models,
        ts_data,
        var_data$var_info
      )
      
      # Update progress
      if (config$verbose) p()
      
      # Return results
      return(list(
        forecasts = forecasts,
        evaluation = evaluation,
        county_id = county_id
      ))
    },
    future.seed = TRUE
  )
  
  # Filter out NULLs and combine results
  valid_results <- county_results[!sapply(county_results, is.null)]
  
  if (length(valid_results) == 0) {
    return(NULL)
  }
  
  # Combine forecasts
  all_forecasts <- bind_rows(lapply(valid_results, function(res) res$forecasts))
  
  # Combine evaluations
  all_evaluations <- bind_rows(lapply(valid_results, function(res) res$evaluation))
  
  # Calculate overall evaluation metrics by model
  overall_eval <- all_evaluations %>%
    group_by(model) %>%
    summarize(
      avg_rmse = mean(rmse),
      avg_mae = mean(mae),
      avg_mape = mean(mape),
      counties = n()
    ) %>%
    arrange(avg_rmse)
  
  # Save results to output path
  results_path <- file.path(config$output_path, paste0("forecast_", variable_name, ".rds"))
  saveRDS(
    list(
      forecasts = all_forecasts,
      evaluations = all_evaluations,
      overall = overall_eval,
      variable = var_data$var_info,
      counties = var_data$counties,
      config = var_data$config
    ),
    results_path
  )
  
  # Generate CSV output for easier sharing
  forecasts_path <- file.path(config$output_path, paste0("forecast_", variable_name, ".csv"))
  write_csv(all_forecasts, forecasts_path)
  
  # Return results
  return(list(
    forecasts = all_forecasts,
    evaluations = all_evaluations,
    overall = overall_eval,
    variable = var_data$var_info,
    counties = var_data$counties,
    config = var_data$config
  ))
}

#' Generate forecasts for multiple variables across all counties
#'
#' @param variables Vector of variable names to forecast
#' @param config Configuration from ml_forecast_config
#' @param counties Specific counties to forecast or NULL for all counties
#'
#' @return List containing forecasts and evaluations for each variable
forecast_multiple_variables <- function(
    variables,
    config = ml_forecast_config(),
    counties = NULL
) {
  # Get all variables if none specified
  if (is.null(variables) || length(variables) == 0) {
    # Connect to database
    con <- dbConnect(duckdb::duckdb(), dbdir = config$db_path)
    on.exit(dbDisconnect(con))
    
    # Get list of variables with enough data
    query <- "
      SELECT 
        v.variable_name,
        v.domain,
        COUNT(DISTINCT d.year) as years_available,
        COUNT(DISTINCT d.geoid) as counties_available
      FROM variables v
      JOIN sdoh_data d ON v.variable_name = d.variable_name
      GROUP BY v.variable_name, v.domain
      HAVING COUNT(DISTINCT d.year) >= 5
        AND COUNT(DISTINCT d.geoid) >= 100
      ORDER BY v.domain, v.variable_name
    "
    
    var_info <- dbGetQuery(con, query)
    
    if (nrow(var_info) == 0) {
      stop("No variables with sufficient data found in the database")
    }
    
    variables <- var_info$variable_name
  }
  
  # Initialize progress reporting
  if (config$verbose) {
    message(paste("Forecasting", length(variables), "variables"))
    p <- progressr::progressor(length(variables))
  }
  
  # Process each variable (serially to avoid overwhelming the system)
  results <- list()
  
  for (var_name in variables) {
    tryCatch({
      results[[var_name]] <- forecast_variable(
        var_name,
        config,
        counties,
        config$models,
        config$forecast_horizon
      )
      
      if (config$verbose) {
        p()
        message(paste("Completed forecasting for", var_name))
      }
    }, error = function(e) {
      warning(paste("Error forecasting", var_name, ":", e$message))
      if (config$verbose) p()
    })
  }
  
  # Create summary of forecasting results
  summary_df <- bind_rows(lapply(results, function(res) {
    if (is.null(res) || is.null(res$overall)) {
      return(NULL)
    }
    
    # Get best model
    best_model <- res$overall %>%
      filter(model == "Ensemble" | row_number() == 1) %>%
      slice(1)
    
    tibble(
      variable_name = res$variable$variable_name[1],
      domain = res$variable$domain[1],
      counties_forecasted = best_model$counties,
      best_model = best_model$model,
      avg_rmse = best_model$avg_rmse,
      avg_mape = best_model$avg_mape,
      forecast_years = paste(
        min(res$forecasts$year),
        max(res$forecasts$year),
        sep = "-"
      )
    )
  }))
  
  # Save summary
  if (nrow(summary_df) > 0) {
    summary_path <- file.path(config$output_path, "forecast_summary.csv")
    write_csv(summary_df, summary_path)
    
    if (config$verbose) {
      message(paste("Summary saved to", summary_path))
    }
  }
  
  return(results)
}

#' Import forecasts into the DuckDB database
#'
#' @param forecast_results Results from forecast_variable or forecast_multiple_variables
#' @param config Configuration from ml_forecast_config
#' @param use_ensemble Whether to only import ensemble forecasts (default) or all models
#'
#' @return Number of records imported
import_forecasts_to_db <- function(
    forecast_results,
    config = ml_forecast_config(),
    use_ensemble = TRUE
) {
  # Connect to database
  con <- dbConnect(duckdb::duckdb(), dbdir = config$db_path)
  on.exit(dbDisconnect(con))
  
  # Extract forecasts
  if (is.list(forecast_results) && "forecasts" %in% names(forecast_results)) {
    # Single variable result
    forecasts <- forecast_results$forecasts
    
    if (use_ensemble) {
      forecasts <- forecasts %>%
        filter(model == "Ensemble")
    }
  } else if (is.list(forecast_results) && length(forecast_results) > 0) {
    # Multiple variable results
    forecasts <- bind_rows(lapply(forecast_results, function(res) {
      if (is.null(res) || is.null(res$forecasts)) {
        return(NULL)
      }
      
      if (use_ensemble) {
        res$forecasts %>%
          filter(model == "Ensemble")
      } else {
        res$forecasts
      }
    }))
  } else {
    stop("Invalid forecast results format")
  }
  
  if (nrow(forecasts) == 0) {
    warning("No forecasts to import")
    return(0)
  }
  
  # Prepare for import
  import_data <- forecasts %>%
    rename(
      value = forecast
    ) %>%
    mutate(
      data_source = paste0("ML Forecast (", model, ")"),
      data_vintage = as.character(Sys.Date()),
      interpolation_method = model,
      confidence_level = config$confidence_level,
      last_updated = Sys.time()
    )
  
  # First, delete any existing forecasts for these variable/county/year combinations
  for (var_name in unique(import_data$variable_name)) {
    for (year in unique(import_data$year)) {
      query <- glue::glue_sql("
        DELETE FROM sdoh_data
        WHERE variable_name = {var_name}
          AND year = {year}
          AND data_quality = 'forecasted'
      ", .con = con)
      
      dbExecute(con, query)
    }
  }
  
  # Import forecasts
  data_to_import <- import_data %>%
    select(
      geoid, year, variable_name, value, data_quality, data_source,
      data_vintage, interpolation_method, ci_lower, ci_upper,
      confidence_level, last_updated
    )
  
  dbAppendTable(con, "sdoh_data", data_to_import)
  
  # Return count of imported records
  return(nrow(data_to_import))
}

#' Generate county-level maps of forecasted values
#'
#' @param forecast_results Results from forecast_variable or forecast_multiple_variables
#' @param config Configuration from ml_forecast_config
#' @param save_dir Directory to save maps (defaults to maps subdirectory of output_path)
#'
#' @return List of file paths for generated maps
generate_forecast_maps <- function(
    forecast_results,
    config = ml_forecast_config(),
    save_dir = NULL
) {
  # Set save directory
  if (is.null(save_dir)) {
    save_dir <- file.path(config$output_path, "maps")
  }
  
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  # Ensure required packages
  if (!requireNamespace("sf", quietly = TRUE) || 
      !requireNamespace("tigris", quietly = TRUE) ||
      !requireNamespace("viridis", quietly = TRUE) ||
      !requireNamespace("ggplot2", quietly = TRUE)) {
    warning("Required packages missing. Install sf, tigris, viridis, and ggplot2.")
    return(NULL)
  }
  
  # Load required packages
  library(sf)
  library(tigris)
  library(viridis)
  library(ggplot2)
  
  # Extract forecasts
  if (is.list(forecast_results) && "forecasts" %in% names(forecast_results)) {
    # Single variable result
    variable_forecasts <- list(forecast_results)
  } else if (is.list(forecast_results) && length(forecast_results) > 0) {
    # Multiple variable results
    variable_forecasts <- forecast_results
  } else {
    stop("Invalid forecast results format")
  }
  
  # Get county shapes
  counties_sf <- tigris::counties(cb = TRUE, year = 2020)
  
  # Initialize list of map files
  map_files <- list()
  
  # Process each variable
  for (var_result in variable_forecasts) {
    if (is.null(var_result) || is.null(var_result$forecasts)) {
      next
    }
    
    # Get variable info
    var_name <- var_result$variable$variable_name[1]
    var_domain <- var_result$variable$domain[1]
    var_units <- var_result$variable$units[1]
    
    # Filter to ensemble forecasts only
    var_forecasts <- var_result$forecasts %>%
      filter(model == "Ensemble")
    
    if (nrow(var_forecasts) == 0) {
      next
    }
    
    # Get unique forecast years
    forecast_years <- sort(unique(var_forecasts$year))
    
    # Create directory for this variable
    var_dir <- file.path(save_dir, var_name)
    if (!dir.exists(var_dir)) {
      dir.create(var_dir, recursive = TRUE)
    }
    
    # Process each forecast year
    for (year in forecast_years) {
      # Get data for this year
      year_data <- var_forecasts %>%
        filter(year == year)
      
      # Join with county shapes
      map_data <- counties_sf %>%
        left_join(year_data, by = c("GEOID" = "geoid"))
      
      # Calculate value range for consistent legend
      value_range <- c(
        min(var_forecasts$forecast, na.rm = TRUE),
        max(var_forecasts$forecast, na.rm = TRUE)
      )
      
      # Create map
      map_plot <- ggplot(map_data) +
        geom_sf(aes(fill = forecast), color = "white", size = 0.1) +
        scale_fill_viridis_c(
          name = paste0(var_name, " (", var_units, ")"),
          limits = value_range,
          option = "viridis",
          na.value = "grey80"
        ) +
        labs(
          title = paste0(var_name, " Forecast for ", year),
          subtitle = paste0("Domain: ", var_domain),
          caption = paste0("Forecast generated on ", format(Sys.Date(), "%Y-%m-%d"))
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "bottom",
          legend.key.width = unit(2, "cm")
        )
      
      # Save map
      map_file <- file.path(var_dir, paste0(var_name, "_forecast_", year, ".png"))
      ggsave(
        map_file,
        map_plot,
        width = 10,
        height = 8,
        dpi = 300
      )
      
      map_files[[length(map_files) + 1]] <- map_file
    }
  }
  
  return(map_files)
}

#' Run the complete ML forecasting pipeline
#'
#' @param variables Vector of variable names to forecast, or NULL for all suitable variables
#' @param config Configuration from ml_forecast_config
#' @param counties Vector of county GEOIDs to forecast, or NULL for all counties
#' @param import_to_db Whether to import forecasts to database
#' @param generate_maps Whether to generate maps
#'
#' @return Results from forecasting
run_ml_forecasting <- function(
    variables = NULL,
    config = ml_forecast_config(),
    counties = NULL,
    import_to_db = TRUE,
    generate_maps = TRUE
) {
  start_time <- Sys.time()
  
  message("Starting ML forecasting pipeline...")
  
  # Run forecasting
  results <- forecast_multiple_variables(
    variables,
    config,
    counties
  )
  
  # Import to database if requested
  if (import_to_db) {
    message("Importing forecasts to database...")
    import_count <- import_forecasts_to_db(results, config)
    message(paste("Imported", import_count, "forecast records to database"))
  }
  
  # Generate maps if requested
  if (generate_maps) {
    message("Generating forecast maps...")
    map_files <- generate_forecast_maps(results, config)
    message(paste("Generated", length(map_files), "forecast maps"))
  }
  
  # Calculate execution time
  end_time <- Sys.time()
  execution_time <- difftime(end_time, start_time, units = "mins")
  message(paste("ML forecasting completed in", round(execution_time, 2), "minutes"))
  
  return(results)
}

#' Generate model explainability for a variable forecast
#'
#' @param forecast_results Results from forecast_variable
#' @param config Configuration from ml_forecast_config
#' @param county_id Specific county to explain (required)
#' @param save_dir Directory to save explainability outputs
#'
#' @return List of feature importance and partial dependence data
explain_model_forecast <- function(
    forecast_results,
    config = ml_forecast_config(),
    county_id,
    save_dir = NULL
) {
  # Check if iml package is available
  if (!requireNamespace("iml", quietly = TRUE)) {
    warning("Package 'iml' is required for model explanation.")
    return(NULL)
  }
  
  library(iml)
  
  # Set save directory
  if (is.null(save_dir)) {
    save_dir <- file.path(config$output_path, "explainability")
  }
  
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  # Extract variable information and data
  if (is.null(forecast_results) || !is.list(forecast_results)) {
    stop("Invalid forecast results format")
  }
  
  # Get variable name
  var_name <- forecast_results$variable$variable_name[1]
  
  # Connect to database to get data
  con <- dbConnect(duckdb::duckdb(), dbdir = config$db_path)
  on.exit(dbDisconnect(con))
  
  # Get data for this variable and county
  data_query <- glue::glue_sql("
    SELECT 
      d.geoid, 
      c.name as county_name, 
      c.state_name,
      d.year, 
      d.value,
      d.data_quality
    FROM sdoh_data d
    JOIN counties c ON d.geoid = c.geoid
    WHERE d.variable_name = {var_name}
      AND d.geoid = {county_id}
    ORDER BY d.year
  ", .con = con)
  
  county_data <- dbGetQuery(con, data_query)
  
  if (nrow(county_data) == 0) {
    warning("No data found for county ", county_id)
    return(NULL)
  }
  
  # Get feature variables that might be useful
  feature_query <- glue::glue_sql("
    WITH target_data AS (
      SELECT year, value
      FROM sdoh_data
      WHERE variable_name = {var_name}
        AND geoid = {county_id}
    )
    SELECT 
      v.variable_name,
      v.domain,
      CORR(s.value, t.value) as correlation
    FROM sdoh_data s
    JOIN target_data t ON s.year = t.year
    JOIN variables v ON s.variable_name = v.variable_name
    WHERE s.variable_name != {var_name}
      AND s.geoid = {county_id}
    GROUP BY v.variable_name, v.domain
    HAVING COUNT(*) >= 5
    ORDER BY ABS(correlation) DESC
    LIMIT 10
  ", .con = con)
  
  feature_corr <- dbGetQuery(con, feature_query)
  
  # Get top correlated features
  if (nrow(feature_corr) > 0) {
    top_features <- feature_corr %>%
      filter(abs(correlation) >= 0.3) %>%
      pull(variable_name)
    
    # Fetch feature data
    feature_data <- list()
    for (feature in top_features) {
      feature_query <- glue::glue_sql("
        SELECT year, value
        FROM sdoh_data
        WHERE variable_name = {feature}
          AND geoid = {county_id}
        ORDER BY year
      ", .con = con)
      
      feature_data[[feature]] <- dbGetQuery(con, feature_query) %>%
        rename(!!paste0(feature) := value)
    }
    
    # Prepare time series data with features
    ts_data <- county_data %>%
      arrange(year) %>%
      mutate(
        year_num = year - min(year) + 1,
        value_lag1 = lag(value, 1),
        value_lag2 = lag(value, 2)
      ) %>%
      filter(!is.na(value_lag2)) # Remove rows with NA lags
    
    # Add feature variables
    for (feature in names(feature_data)) {
      ts_data <- ts_data %>%
        left_join(feature_data[[feature]], by = "year")
    }
    
    # Check if we have enough data for model fitting
    if (nrow(ts_data) < 5) {
      warning("Not enough data points for model explanation")
      return(NULL)
    }
    
    # Fit an XGBoost model for explanation
    # Prepare data for XGBoost
    features <- ts_data %>%
      select(year_num, value_lag1, value_lag2, all_of(names(feature_data))) %>%
      as.matrix()
    
    # Target vector
    target <- ts_data$value
    
    # Train XGBoost model
    xgb_params <- list(
      objective = "reg:squarederror",
      booster = "gbtree",
      eta = 0.05,
      max_depth = 4,
      min_child_weight = 1,
      subsample = 0.8,
      colsample_bytree = 0.8
    )
    
    xgb_model <- xgboost::xgb.train(
      params = xgb_params,
      data = xgboost::xgb.DMatrix(data = features, label = target),
      nrounds = 100,
      verbose = 0
    )
    
    # Create predictor function for iml
    predictor <- Predictor$new(
      model = xgb_model,
      data = as.data.frame(features),
      y = target,
      predict.fun = function(model, newdata) {
        predict(model, as.matrix(newdata))
      }
    )
    
    # Feature importance
    importance <- FeatureImp$new(predictor, loss = "mse")
    
    # Save feature importance plot
    png(file.path(save_dir, paste0(var_name, "_", county_id, "_importance.png")),
        width = 800, height = 600)
    plot(importance)
    dev.off()
    
    # Partial dependence plots for top features
    pd_results <- list()
    for (feature in colnames(features)) {
      pd <- FeatureEffect$new(predictor, feature = feature, method = "pdp")
      
      # Save PDP plot
      png(file.path(save_dir, paste0(var_name, "_", county_id, "_pdp_", feature, ".png")),
          width = 800, height = 600)
      plot(pd)
      dev.off()
      
      pd_results[[feature]] <- pd$results
    }
    
    # Generate interaction effects for top pairs
    if (ncol(features) >= 2) {
      top_pairs <- combn(colnames(features), 2)[, 1:min(3, choose(ncol(features), 2)), drop = FALSE]
      
      for (i in 1:ncol(top_pairs)) {
        pair <- top_pairs[, i]
        interaction <- Interaction$new(predictor, feature = pair)
        
        # Save interaction plot
        png(file.path(save_dir, paste0(var_name, "_", county_id, "_interaction_", 
                                     paste(pair, collapse = "_"), ".png")),
            width = 800, height = 600)
        plot(interaction)
        dev.off()
      }
    }
    
    # Return explainability results
    return(list(
      importance = importance$results,
      pdp = pd_results,
      county_id = county_id,
      variable_name = var_name,
      feature_correlations = feature_corr
    ))
  } else {
    warning("No correlated features found for explanation")
    return(NULL)
  }
}

#' Create interactive forecast visualizations
#'
#' @param forecast_results Results from forecast_variable
#' @param config Configuration from ml_forecast_config
#' @param counties Vector of county GEOIDs to visualize
#' @param save_dir Directory to save interactive visualizations
#'
#' @return List of paths to saved HTML files
create_interactive_visualizations <- function(
    forecast_results,
    config = ml_forecast_config(),
    counties = NULL,
    save_dir = NULL
) {
  # Check required packages
  if (!requireNamespace("plotly", quietly = TRUE) || 
      !requireNamespace("htmlwidgets", quietly = TRUE)) {
    warning("Packages 'plotly' and 'htmlwidgets' are required for interactive visualizations")
    return(NULL)
  }
  
  library(plotly)
  library(htmlwidgets)
  
  # Set save directory
  if (is.null(save_dir)) {
    save_dir <- file.path(config$output_path, "interactive")
  }
  
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  # Extract forecasts
  if (is.null(forecast_results) || !is.list(forecast_results)) {
    stop("Invalid forecast results format")
  }
  
  # Get variable info
  var_name <- forecast_results$variable$variable_name[1]
  var_domain <- forecast_results$variable$domain[1]
  var_units <- forecast_results$variable$units[1]
  
  # Connect to database to get historical data
  con <- dbConnect(duckdb::duckdb(), dbdir = config$db_path)
  on.exit(dbDisconnect(con))
  
  # Get forecasts
  forecasts <- forecast_results$forecasts
  
  # Filter to specific counties if requested
  if (!is.null(counties)) {
    forecasts <- forecasts %>%
      filter(geoid %in% counties)
  }
  
  if (nrow(forecasts) == 0) {
    warning("No forecast data available for visualization")
    return(NULL)
  }
  
  # Get unique counties in the forecasts
  forecast_counties <- unique(forecasts$geoid)
  
  # Get historical data for these counties
  hist_query <- glue::glue_sql("
    SELECT 
      d.geoid, 
      c.name as county_name, 
      c.state_name,
      d.year, 
      d.value,
      d.data_quality
    FROM sdoh_data d
    JOIN counties c ON d.geoid = c.geoid
    WHERE d.variable_name = {var_name}
      AND d.geoid IN ({forecast_counties*})
      AND d.data_quality != 'forecasted'
    ORDER BY d.geoid, d.year
  ", .con = con)
  
  historical_data <- dbGetQuery(con, hist_query)
  
  # Initialize list of saved files
  saved_files <- list()
  
  # Generate visualizations
  
  # 1. County-specific time series with forecasts
  for (county_id in forecast_counties) {
    # Get county data
    county_hist <- historical_data %>%
      filter(geoid == county_id)
    
    county_forecast <- forecasts %>%
      filter(geoid == county_id, model == "Ensemble")
    
    if (nrow(county_hist) == 0 || nrow(county_forecast) == 0) {
      next
    }
    
    # Get county name
    county_name <- county_hist$county_name[1]
    state_name <- county_hist$state_name[1]
    
    # Create plotly visualization
    p <- plot_ly() %>%
      add_trace(
        data = county_hist,
        x = ~year,
        y = ~value,
        type = "scatter",
        mode = "lines+markers",
        name = "Historical",
        line = list(color = "blue"),
        marker = list(color = "blue"),
        hovertemplate = "Year: %{x}<br>Value: %{y}<br>Source: Historical<extra></extra>"
      ) %>%
      add_trace(
        data = county_forecast,
        x = ~year,
        y = ~forecast,
        type = "scatter",
        mode = "lines+markers",
        name = "Forecast",
        line = list(color = "red", dash = "dash"),
        marker = list(color = "red"),
        hovertemplate = "Year: %{x}<br>Value: %{y}<br>Source: Forecast<extra></extra>"
      ) %>%
      add_ribbons(
        data = county_forecast,
        x = ~year,
        ymin = ~lower,
        ymax = ~upper,
        name = "95% Confidence",
        line = list(color = "transparent"),
        fillcolor = "rgba(255, 0, 0, 0.2)",
        hoverinfo = "none",
        showlegend = FALSE
      ) %>%
      layout(
        title = list(
          text = paste0(var_name, " for ", county_name, ", ", state_name),
          font = list(size = 20)
        ),
        xaxis = list(title = "Year"),
        yaxis = list(title = paste0(var_name, " (", var_units, ")")),
        hovermode = "closest",
        legend = list(x = 0.01, y = 0.99, bgcolor = "rgba(255, 255, 255, 0.5)"),
        margin = list(l = 60, r = 20, t = 60, b = 60),
        shapes = list(
          list(
            type = "line",
            x0 = max(county_hist$year),
            x1 = max(county_hist$year),
            y0 = 0,
            y1 = 1,
            yref = "paper",
            line = list(color = "gray", width = 1, dash = "dash")
          )
        ),
        annotations = list(
          list(
            x = max(county_hist$year),
            y = 1,
            yref = "paper",
            text = "Forecast Start",
            showarrow = TRUE,
            arrowhead = 4,
            ax = 0,
            ay = -40
          )
        )
      )
    
    # Save the plot as HTML
    file_path <- file.path(save_dir, paste0(var_name, "_", county_id, "_forecast.html"))
    htmlwidgets::saveWidget(p, file_path, selfcontained = TRUE)
    saved_files <- c(saved_files, file_path)
  }
  
  # 2. Multi-county comparison
  if (length(forecast_counties) > 1 && length(forecast_counties) <= 10) {
    # Combine historical and forecast data
    county_data <- rbind(
      historical_data %>%
        select(geoid, county_name, state_name, year, value) %>%
        mutate(
          type = "Historical",
          lower = NA,
          upper = NA
        ),
      forecasts %>%
        filter(model == "Ensemble") %>%
        select(geoid, county_name, state_name, year, forecast) %>%
        rename(value = forecast) %>%
        mutate(
          type = "Forecast",
          lower = forecasts$lower[forecasts$model == "Ensemble"],
          upper = forecasts$upper[forecasts$model == "Ensemble"]
        )
    )
    
    # Create a multi-county visualization
    p <- plot_ly() %>%
      layout(
        title = list(
          text = paste0("Multi-County Forecast: ", var_name),
          font = list(size = 20)
        ),
        xaxis = list(title = "Year"),
        yaxis = list(title = paste0(var_name, " (", var_units, ")")),
        hovermode = "closest",
        legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.2),
        margin = list(l = 60, r = 20, t = 60, b = 100)
      )
    
    # Add each county as a trace
    colors <- RColorBrewer::brewer.pal(min(length(forecast_counties), 8), "Set1")
    color_index <- 1
    
    for (county_id in forecast_counties) {
      county_subset <- county_data %>%
        filter(geoid == county_id)
      
      if (nrow(county_subset) == 0) {
        next
      }
      
      # Get county name
      county_name <- county_subset$county_name[1]
      state_name <- county_subset$state_name[1]
      display_name <- paste0(county_name, ", ", state_name)
      
      # Set color (cycling through the palette)
      color <- colors[color_index]
      color_index <- (color_index %% length(colors)) + 1
      
      # Historical data
      hist_subset <- county_subset %>% filter(type == "Historical")
      forecast_subset <- county_subset %>% filter(type == "Forecast")
      
      # Add historical trace
      p <- p %>% add_trace(
        data = hist_subset,
        x = ~year,
        y = ~value,
        type = "scatter",
        mode = "lines+markers",
        name = display_name,
        line = list(color = color),
        marker = list(color = color),
        legendgroup = display_name,
        hovertemplate = paste0(
          "County: ", display_name,
          "<br>Year: %{x}",
          "<br>Value: %{y}",
          "<br>Type: Historical",
          "<extra></extra>"
        )
      )
      
      # Add forecast trace (dashed line)
      p <- p %>% add_trace(
        data = forecast_subset,
        x = ~year,
        y = ~value,
        type = "scatter",
        mode = "lines+markers",
        name = paste0(display_name, " (Forecast)"),
        line = list(color = color, dash = "dash"),
        marker = list(color = color),
        legendgroup = display_name,
        showlegend = FALSE,
        hovertemplate = paste0(
          "County: ", display_name,
          "<br>Year: %{x}",
          "<br>Value: %{y}",
          "<br>Type: Forecast",
          "<extra></extra>"
        )
      )
    }
    
    # Save the plot as HTML
    file_path <- file.path(save_dir, paste0(var_name, "_multi_county_forecast.html"))
    htmlwidgets::saveWidget(p, file_path, selfcontained = TRUE)
    saved_files <- c(saved_files, file_path)
  }
  
  # 3. Model comparison for a specific county
  if (length(forecast_counties) > 0) {
    # Take the first county as an example
    county_id <- forecast_counties[1]
    
    # Get all models for this county
    county_forecasts <- forecasts %>%
      filter(geoid == county_id)
    
    # Get county name
    county_name <- county_forecasts$county_name[1]
    state_name <- county_forecasts$state_name[1]
    
    # Get historical data
    county_hist <- historical_data %>%
      filter(geoid == county_id)
    
    # Create plotly visualization for model comparison
    p <- plot_ly() %>%
      add_trace(
        data = county_hist,
        x = ~year,
        y = ~value,
        type = "scatter",
        mode = "lines+markers",
        name = "Historical",
        line = list(color = "black", width = 3),
        marker = list(color = "black", size = 8),
        hovertemplate = "Year: %{x}<br>Value: %{y}<br>Source: Historical<extra></extra>"
      )
    
    # Add each model as a trace
    models <- unique(county_forecasts$model)
    colors <- c("red", "blue", "green", "purple", "orange", "cyan", "magenta")
    
    for (i in 1:length(models)) {
      model_name <- models[i]
      model_data <- county_forecasts %>%
        filter(model == model_name)
      
      color <- colors[(i-1) %% length(colors) + 1]
      
      p <- p %>% add_trace(
        data = model_data,
        x = ~year,
        y = ~forecast,
        type = "scatter",
        mode = "lines+markers",
        name = model_name,
        line = list(color = color),
        marker = list(color = color),
        hovertemplate = paste0(
          "Year: %{x}",
          "<br>Value: %{y}",
          "<br>Model: ", model_name,
          "<extra></extra>"
        )
      )
    }
    
    # Layout
    p <- p %>% layout(
      title = list(
        text = paste0("Model Comparison: ", var_name, " for ", county_name, ", ", state_name),
        font = list(size = 20)
      ),
      xaxis = list(title = "Year"),
      yaxis = list(title = paste0(var_name, " (", var_units, ")")),
      hovermode = "closest",
      legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.2),
      margin = list(l = 60, r = 20, t = 60, b = 100),
      shapes = list(
        list(
          type = "line",
          x0 = max(county_hist$year),
          x1 = max(county_hist$year),
          y0 = 0,
          y1 = 1,
          yref = "paper",
          line = list(color = "gray", width = 1, dash = "dash")
        )
      ),
      annotations = list(
        list(
          x = max(county_hist$year),
          y = 1,
          yref = "paper",
          text = "Forecast Start",
          showarrow = TRUE,
          arrowhead = 4,
          ax = 0,
          ay = -40
        )
      )
    )
    
    # Save the plot as HTML
    file_path <- file.path(save_dir, paste0(var_name, "_", county_id, "_model_comparison.html"))
    htmlwidgets::saveWidget(p, file_path, selfcontained = TRUE)
    saved_files <- c(saved_files, file_path)
  }
  
  return(saved_files)
}

# If script is run directly, execute the forecasting pipeline
if (!interactive()) {
  # Parse command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  
  # Initialize configuration
  config <- ml_forecast_config()
  
  # Parse arguments
  if (length(args) > 0) {
    i <- 1
    while (i <= length(args)) {
      if (args[i] == "--db" && i < length(args)) {
        config$db_path <- args[i + 1]
        i <- i + 2
      } else if (args[i] == "--output" && i < length(args)) {
        config$output_path <- args[i + 1]
        i <- i + 2
      } else if (args[i] == "--cache" && i < length(args)) {
        config$cache_dir <- args[i + 1]
        i <- i + 2
      } else if (args[i] == "--workers" && i < length(args)) {
        config$workers <- as.numeric(args[i + 1])
        i <- i + 2
      } else if (args[i] == "--variables" && i < length(args)) {
        variables <- strsplit(args[i + 1], ",")[[1]]
        i <- i + 2
      } else if (args[i] == "--counties" && i < length(args)) {
        counties <- strsplit(args[i + 1], ",")[[1]]
        i <- i + 2
      } else if (args[i] == "--horizon" && i < length(args)) {
        config$forecast_horizon <- as.numeric(args[i + 1])
        i <- i + 2
      } else if (args[i] == "--no-import") {
        import_to_db <- FALSE
        i <- i + 1
      } else if (args[i] == "--no-maps") {
        generate_maps <- FALSE
        i <- i + 1
      } else if (args[i] == "--interactive") {
        create_interactive <- TRUE
        i <- i + 1
      } else if (args[i] == "--explain") {
        explain_models <- TRUE
        i <- i + 1
      } else if (args[i] == "--quiet") {
        config$verbose <- FALSE
        i <- i + 1
      } else {
        # Unknown argument
        warning("Unknown argument:", args[i])
        i <- i + 1
      }
    }
  }
  
  # Execute the pipeline
  results <- run_ml_forecasting(
    variables = if (exists("variables")) variables else NULL,
    config = config,
    counties = if (exists("counties")) counties else NULL,
    import_to_db = if (exists("import_to_db")) import_to_db else TRUE,
    generate_maps = if (exists("generate_maps")) generate_maps else TRUE
  )
  
  # Generate interactive visualizations if requested
  if (exists("create_interactive") && create_interactive && !is.null(results)) {
    message("Creating interactive visualizations...")
    if (is.list(results) && length(results) > 0) {
      if ("forecasts" %in% names(results)) {
        # Single variable result
        interactive_files <- create_interactive_visualizations(
          results,
          config = config,
          counties = if (exists("counties")) counties else NULL
        )
        message(paste("Created", length(interactive_files), "interactive visualizations"))
      } else {
        # Multiple variable results
        vis_count <- 0
        for (var_name in names(results)) {
          if (!is.null(results[[var_name]]) && "forecasts" %in% names(results[[var_name]])) {
            interactive_files <- create_interactive_visualizations(
              results[[var_name]],
              config = config,
              counties = if (exists("counties")) counties else NULL
            )
            vis_count <- vis_count + length(interactive_files)
          }
        }
        message(paste("Created", vis_count, "interactive visualizations"))
      }
    }
  }
  
  # Generate model explanations if requested
  if (exists("explain_models") && explain_models && !is.null(results) && exists("counties") && length(counties) > 0) {
    message("Generating model explanations...")
    if (is.list(results) && length(results) > 0) {
      if ("forecasts" %in% names(results)) {
        # Single variable result
        for (county_id in counties) {
          explain_model_forecast(
            results,
            config = config,
            county_id = county_id
          )
        }
        message(paste("Generated model explanations for", length(counties), "counties"))
      } else {
        # Multiple variable results
        explanation_count <- 0
        for (var_name in names(results)) {
          if (!is.null(results[[var_name]]) && "forecasts" %in% names(results[[var_name]])) {
            for (county_id in counties) {
              explain_model_forecast(
                results[[var_name]],
                config = config,
                county_id = county_id
              )
              explanation_count <- explanation_count + 1
            }
          }
        }
        message(paste("Generated", explanation_count, "model explanations"))
      }
    }
  }
}