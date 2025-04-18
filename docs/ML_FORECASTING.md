# Machine Learning Forecasting for SDOH Data

This document provides detailed examples of how to use the ML forecasting functionality in the unified SDOH county-level pipeline.

## Overview

The `ml_forecasting.r` script provides advanced machine learning capabilities for predicting future values of SDOH variables at the county level. It supports multiple forecasting models, ensemble predictions, interactive visualizations, and model explainability.

## Key Features

- **Multiple Forecasting Models**: ARIMA, ETS, Prophet, XGBoost, Random Forest, Elastic Net
- **Ensemble Forecasting**: Combines predictions from multiple models for improved accuracy
- **Interactive Visualizations**: Create interactive HTML visualizations to explore forecasts
- **Model Explainability**: Feature importance and partial dependence plots
- **County-Level Forecasts**: Generate forecasts for specific counties or all counties
- **Database Integration**: Import forecasts back into the unified database
- **Map Generation**: Create county-level choropleth maps of forecasted values

## Basic Usage

### 1. Load the Script

```r
source("R/ml_forecasting.r")
```

### 2. Configure the ML Environment

```r
# Default configuration
config <- ml_forecast_config()

# Custom configuration
config <- ml_forecast_config(
  db_path = "output/my_database.duckdb",
  output_path = "output/my_forecasts",
  workers = 4,                 # Number of parallel workers
  forecast_horizon = 5,        # Number of years to forecast into the future
  models = c("prophet", "xgboost", "random_forest")  # Models to use
)
```

### 3. Generate Forecasts for a Single Variable

```r
# Forecast median household income for all counties
income_forecast <- forecast_variable(
  variable_name = "median_household_income",
  config = config
)

# Forecast for specific counties
poverty_forecast <- forecast_variable(
  variable_name = "poverty_rate",
  config = config,
  counties = c("06001", "06075", "36061")  # Alameda CA, San Francisco CA, New York NY
)
```

### 4. Generate Forecasts for Multiple Variables

```r
# Forecast multiple specific variables
variables <- c("median_household_income", "poverty_rate", "life_expectancy")
forecasts <- forecast_multiple_variables(
  variables = variables,
  config = config
)

# Forecast all suitable variables (automatically selects variables with enough data)
all_forecasts <- forecast_multiple_variables(
  variables = NULL,  # Auto-select variables
  config = config
)
```

### 5. Run the Complete ML Pipeline

```r
# Run the complete ML forecasting pipeline
results <- run_ml_forecasting(
  variables = c("median_household_income", "poverty_rate"),
  config = config,
  counties = NULL,  # All counties
  import_to_db = TRUE,  # Import results to the database
  generate_maps = TRUE  # Generate choropleth maps
)
```

### 6. Generate Interactive Visualizations

```r
# Create interactive visualizations for a forecast result
vis_files <- create_interactive_visualizations(
  forecast_results = income_forecast,
  config = config,
  counties = c("06001", "06075", "36061")  # Specific counties
)

# Open the first visualization in a browser
browseURL(vis_files[1])
```

### 7. Explain Model Forecasts

```r
# Generate model explainability for a specific county
explanation <- explain_model_forecast(
  forecast_results = income_forecast,
  config = config,
  county_id = "06001"  # Alameda County, CA
)
```

## Command-Line Usage

The ML forecasting script can also be run directly from the command line:

```bash
# Forecast median household income and poverty rate
Rscript R/ml_forecasting.r --variables median_household_income,poverty_rate

# Forecast with a 10-year horizon
Rscript R/ml_forecasting.r --variables life_expectancy --horizon 10

# Forecast for specific counties with interactive visualizations
Rscript R/ml_forecasting.r --variables median_household_income --counties 06001,06075,36061 --interactive

# Run with model explainability
Rscript R/ml_forecasting.r --variables median_household_income --counties 06001 --explain
```

## Command-Line Arguments

The following command-line arguments are supported:

| Argument | Description |
|----------|-------------|
| `--db` | Path to the DuckDB database |
| `--output` | Path to save forecasting outputs |
| `--cache` | Path to the cache directory |
| `--workers` | Number of parallel workers |
| `--variables` | Comma-separated list of variables to forecast |
| `--counties` | Comma-separated list of county GEOIDs |
| `--horizon` | Number of years to forecast into the future |
| `--no-import` | Don't import forecasts to the database |
| `--no-maps` | Don't generate forecast maps |
| `--interactive` | Generate interactive visualizations |
| `--explain` | Generate model explanations |
| `--quiet` | Suppress verbose output |

## Output Files

The ML forecasting process generates several types of output files:

1. **Forecast data files** (.rds and .csv) with full forecast results
2. **Static maps** (.png) showing forecasted values by county
3. **Interactive visualizations** (.html) for exploring forecasts
4. **Model explainability plots** (.png) showing feature importance and dependencies

All outputs are saved to the configured output directory.

## Database Integration

Forecasts can be integrated back into the unified database. When imported into the database:

- Each forecast gets the data quality flag `'forecasted'`
- The interpolation_method field contains the model name
- The data_source field identifies it as ML forecast data
- Confidence intervals are stored as ci_lower and ci_upper

You can query forecasts using:

```sql
SELECT * FROM sdoh_data 
WHERE data_quality = 'forecasted' 
  AND variable_name = 'median_household_income'
ORDER BY geoid, year;
```

## Advanced Use Cases

### Custom Forecasting Models

You can customize which models are used by setting the `models` parameter:

```r
config <- ml_forecast_config()
config$models <- c("prophet", "xgboost")  # Only use Prophet and XGBoost

forecast <- forecast_variable(
  variable_name = "median_household_income",
  config = config,
  models = c("prophet", "xgboost", "arima")  # Override config models
)
```

### Variable-Specific Configuration

For different variables, you might want different configuration settings:

```r
# Economic variables might benefit from longer horizons
economic_config <- ml_forecast_config()
economic_config$forecast_horizon <- 10

# Health variables might need more conservative forecasts
health_config <- ml_forecast_config()
health_config$forecast_horizon <- 3
health_config$confidence_level <- 0.99  # Higher confidence level
```

### Forecast Evaluation

You can evaluate the quality of forecasts using the evaluation results:

```r
# Get the forecast results
forecast <- forecast_variable("median_household_income")

# Look at the overall model performance
View(forecast$overall)

# Look at detailed county-level evaluations
View(forecast$evaluations)
```

### Using Feature Variables

The forecasting system automatically identifies and uses correlated variables as features:

```r
# Override the feature variables to use
config <- ml_forecast_config()
config$feature_vars <- c("poverty_rate", "unemployment_rate")

forecast <- forecast_variable("median_household_income", config)
```

## Performance Tips

1. **Adjust workers**: Set the `workers` parameter based on your system's CPU cores
2. **Limit counties**: For testing, use a small set of counties to speed up execution
3. **Cache results**: Results are cached in the output directory for future reference
4. **Reduce models**: Using fewer models speeds up execution
5. **Memory management**: For large forecasts, run individual variables instead of all at once

## Troubleshooting

Common issues and solutions:

1. **Missing packages**: If you get errors about missing packages, run `install_packages.r` or install them manually
2. **Memory issues**: Reduce the number of workers or counties, or run variables individually
3. **Forecast failures**: Some variables might not have enough historical data; try variables with longer time series
4. **Database connectivity**: Ensure the DuckDB database path is correct
5. **Parallel processing issues**: If you encounter parallel processing problems, reduce the number of workers or set `config$workers = 1`

## Examples

### Example 1: Basic Forecast and Visualization

```r
# Load the script
source("R/ml_forecasting.r")

# Configure the environment
config <- ml_forecast_config()

# Generate forecasts for median household income
income_forecast <- forecast_variable("median_household_income", config)

# Create interactive visualizations
vis_files <- create_interactive_visualizations(income_forecast, config)

# Open the visualization in a browser
browseURL(vis_files[1])
```

### Example 2: Multi-County Comparison

```r
# Select specific counties of interest
counties <- c(
  "06001",  # Alameda County, CA
  "06075",  # San Francisco County, CA
  "36061",  # New York County, NY
  "17031",  # Cook County, IL
  "13121"   # Fulton County, GA
)

# Generate forecasts for these counties
income_forecast <- forecast_variable(
  "median_household_income",
  config = ml_forecast_config(),
  counties = counties
)

# Create a multi-county visualization
vis_files <- create_interactive_visualizations(income_forecast)

# Open the multi-county comparison
browseURL(vis_files[2])  # Second file is the multi-county comparison
```

### Example 3: Model Comparison and Explainability

```r
# Generate forecasts using all available models
config <- ml_forecast_config()
config$models <- c("arima", "ets", "prophet", "xgboost", "random_forest", "glmnet")

forecast <- forecast_variable(
  "median_household_income",
  config = config,
  counties = c("06001")  # Alameda County, CA
)

# Create visualizations including model comparison
vis_files <- create_interactive_visualizations(forecast)

# Generate model explainability
explanation <- explain_model_forecast(
  forecast,
  county_id = "06001"
)

# List the generated explainability files
list.files(file.path(config$output_path, "explainability"))
```

### Example 4: Scheduling Regular Forecast Updates

This example shows how to set up a script to regularly update forecasts:

```r
# forecast_updater.r
source("R/ml_forecasting.r")

# Configure the environment
config <- ml_forecast_config(
  db_path = "output/us_county_sdoh_unified.duckdb",
  output_path = paste0("output/forecasts/", format(Sys.Date(), "%Y%m%d"))
)

# Key economic and health variables
key_variables <- c(
  "median_household_income", 
  "poverty_rate", 
  "unemployment_rate",
  "life_expectancy", 
  "food_insecurity_rate",
  "broadband_access_pct"
)

# Run forecasts
results <- forecast_multiple_variables(key_variables, config)

# Import to database
import_forecasts_to_db(results, config)

# Generate maps
generate_forecast_maps(results, config)

# Create interactive visualizations
for (var_name in names(results)) {
  create_interactive_visualizations(results[[var_name]], config)
}

# Log completion
cat("Forecast update completed at", format(Sys.time()), "\n",
    "Variables updated:", paste(key_variables, collapse = ", "), "\n",
    file = "logs/forecast_updates.log", append = TRUE)
```

You can schedule this script to run regularly using a system scheduler (cron, Task Scheduler, etc.).