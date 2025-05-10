# Traffic Safety Module Guide

This guide provides examples and best practices for using the enhanced traffic safety module in the Social Determinants of Health pipeline.

## Table of Contents
- [Overview](#overview)
- [Basic Usage](#basic-usage)
- [Enhanced Features](#enhanced-features)
- [Data Integration Options](#data-integration-options)
- [Visualization Examples](#visualization-examples)
- [Interactive Dashboard](#interactive-dashboard)
- [Advanced Configuration](#advanced-configuration)
- [Troubleshooting](#troubleshooting)
- [FAQ](#faq)

## Overview

The enhanced traffic safety module provides comprehensive capabilities for analyzing traffic-related fatalities and injuries at the county level. Key enhancements include:

- **Geospatial Analysis**: Spatial clustering, hotspot detection, and corridor analysis
- **Data Validation**: Comprehensive data quality checks and flagging
- **Time Series Forecasting**: Trend analysis and multi-method forecasting
- **Interactive Dashboard**: Web-based exploration and visualization interface
- **Optimized Caching**: Intelligent data storage and retrieval strategies
- **Seamless Pipeline Integration**: Works with the unified SDOH pipeline

## Basic Usage

### Simple Data Retrieval

```r
# Basic usage with original module
source("fetch_traffic_safety_data.r")

# Fetch data for recent years
traffic_data <- fetch_traffic_safety_data(
  years = 2015:2020,
  cache_dir = "data/cache",
  refresh_cache = FALSE  # Set to TRUE to refresh cached data
)

# View the data structure
str(traffic_data)

# Basic summary
summary(traffic_data[, c(
  "traffic_fatality_count", 
  "traffic_fatality_rate_per_100k", 
  "dui_fatality_count"
)])
```

### Using Enhanced Features

```r
# Load the enhanced integration module
source("traffic_safety_integration.r")

# Fetch enhanced data with all features
enhanced_data <- fetch_enhanced_traffic_safety_data(
  years = 2015:2020,
  use_validation = TRUE,       # Apply data validation
  use_optimized_cache = TRUE,  # Use enhanced caching
  generate_forecasts = TRUE,   # Include forecasts
  spatial_analysis = TRUE      # Perform spatial analysis
)

# Create visualizations from enhanced data
vis_files <- create_traffic_safety_visualizations(
  enhanced_data,
  output_dir = "output/visualizations/traffic_safety",
  create_maps = TRUE,
  create_forecast_plots = TRUE
)
```

### Accessing Data Quality Information

Each data point has an associated quality flag:

```r
# Check quality distribution
table(traffic_data$traffic_fatality_count_data_quality, useNA = "ifany")

# Filter to only direct (non-interpolated) data
direct_data <- traffic_data %>%
  filter(traffic_fatality_count_data_quality == "direct")

# View quality for a specific county over time
la_county_data <- traffic_data %>%
  filter(fips == "06037") %>%  # Los Angeles County
  select(year, traffic_fatality_count, traffic_fatality_count_data_quality)
```

## Enhanced Features

### Geospatial Analysis

The geospatial module provides tools for spatial pattern analysis:

```r
# Load the geospatial module directly
source("traffic_safety_geospatial.r")

# Prepare spatial data for analysis
spatial_data <- prepare_spatial_data(
  traffic_data = traffic_data,
  year = 2020,
  variable = "traffic_fatality_rate_per_100k"
)

# Calculate spatial autocorrelation
moran_result <- calculate_morans_i(
  spatial_data = spatial_data,
  variable = "traffic_fatality_rate_per_100k"
)
print(moran_result)

# Identify spatial clusters (hot spots and cold spots)
clusters <- identify_spatial_clusters(
  spatial_data = spatial_data,
  method = "lisa"  # Local Indicators of Spatial Association
)

# Create hotspot map
hotspot_map <- create_hotspot_map(
  clusters,
  title = "Traffic Fatality Rate Clusters (2020)"
)
```

### Data Validation

The validation module ensures data quality and consistency:

```r
# Load the validation module directly
source("traffic_safety_validation.r")

# Create validator for traffic data
validator <- TrafficDataValidator$new(traffic_data)

# Add validation rules
validator$add_rule("no_negative_counts", function(data) {
  all(data$traffic_fatality_count >= 0, na.rm = TRUE)
})

validator$add_rule("rate_consistency", function(data) {
  data %>%
    filter(!is.na(traffic_fatality_count), !is.na(traffic_fatality_rate_per_100k)) %>%
    mutate(calc_rate = traffic_fatality_count / population * 100000) %>%
    filter(abs(calc_rate - traffic_fatality_rate_per_100k) > 0.1) %>%
    nrow() == 0
})

# Run validation
validation_result <- validator$validate()
print(validation_result)

# Generate validation report
validation_report <- validator$generate_report(format = "markdown")
```

### Time Series Forecasting

The forecasting module provides tools for trend analysis and prediction:

```r
# Load the forecasting module directly
source("traffic_safety_forecasting.r")

# Prepare time series data
ts_data <- prepare_timeseries_data(
  traffic_data = traffic_data,
  variable = "traffic_fatality_rate_per_100k",
  region_type = "national"  # Options: national, state, county
)

# Generate national forecast
national_forecast <- generate_forecast(
  ts_data = ts_data,
  forecast_years = 5,
  method = "auto.arima"  # Options: auto.arima, ets, prophet, ensemble
)

# Plot the forecast
forecast_plot <- plot_forecast(
  national_forecast,
  title = "U.S. Traffic Fatality Rate Forecast",
  y_label = "Fatalities per 100,000 Population"
)

# Identify counties with concerning trends
problem_counties <- identify_concerning_trends(
  traffic_data = traffic_data,
  variable = "traffic_fatality_rate_per_100k",
  threshold_z = 1.96,  # Z-score threshold (default: 95% confidence)
  min_years = 3        # Minimum years of data required
)
```

## Data Integration Options

### Using with Census Population Data

```r
# Option 1: Automatically fetch population data
traffic_data <- fetch_traffic_safety_data(
  years = 2015:2020,
  refresh_cache = FALSE
)  # Will attempt to fetch Census data automatically

# Option 2: Provide population data from another source
# For example, if you already have census data from the pipeline:
source("fetch_county_data_final.r")  # Or whichever census data source you use
census_data <- fetch_county_population_data(years = 2015:2020)

traffic_data <- fetch_traffic_safety_data(
  years = 2015:2020,
  census_data = census_data  # Pass your population data
)
```

### Combining with Other SDOH Data

```r
# Get enhanced traffic safety data
source("traffic_safety_integration.r")
traffic_data <- fetch_enhanced_traffic_safety_data(
  years = 2015:2020,
  generate_forecasts = TRUE
)

# Get healthcare access data
source("fetch_healthcare_data.r")
healthcare_data <- fetch_healthcare_data(years = 2015:2020)

# Merge the datasets
combined_data <- traffic_data %>%
  select(fips, year, traffic_fatality_rate_per_100k, dui_fatality_rate_per_100k) %>%
  inner_join(
    healthcare_data %>%
      select(fips, year, primary_care_physicians_per_100k, preventable_hospital_stays),
    by = c("fips", "year")
  )

# Analyze relationships
cor_result <- cor(
  combined_data[, c(
    "traffic_fatality_rate_per_100k", 
    "primary_care_physicians_per_100k",
    "preventable_hospital_stays"
  )],
  use = "pairwise.complete.obs"
)

# Access forecasts from enhanced data
forecasts <- attr(traffic_data, "forecasts")
national_forecast <- forecasts$national
```

### Database Integration

```r
# Add traffic safety data to the database
source("traffic_safety_integration.r")

# Fetch enhanced data
enhanced_data <- fetch_enhanced_traffic_safety_data(
  years = 2015:2020,
  generate_forecasts = TRUE,
  spatial_analysis = TRUE
)

# Add to database
add_traffic_safety_to_database(
  traffic_data = enhanced_data,
  db_path = "us_county_sdoh_data.duckdb",
  add_forecasts = TRUE,
  add_spatial = TRUE
)

# To later query from database:
library(DBI)
library(duckdb)

con <- dbConnect(duckdb(), dbdir = "us_county_sdoh_data.duckdb")
traffic_data <- dbGetQuery(con, "SELECT * FROM traffic_safety_variables")
problem_areas <- dbGetQuery(con, "SELECT * FROM problem_areas WHERE variable_name = 'traffic_fatality_rate_per_100k'")
dbDisconnect(con)
```

## Visualization Examples

### Enhanced Visualizations

```r
# Use the integration module for enhanced visualizations
source("traffic_safety_integration.r")

# Fetch enhanced data
enhanced_data <- fetch_enhanced_traffic_safety_data(
  years = 2015:2020,
  generate_forecasts = TRUE,
  spatial_analysis = TRUE
)

# Create visualizations
vis_files <- create_traffic_safety_visualizations(
  enhanced_data,
  output_dir = "output/visualizations/traffic_safety",
  create_maps = TRUE,
  create_forecast_plots = TRUE,
  create_animation = TRUE
)

# List generated files
print(vis_files)
```

### Creating Choropleth Maps

```r
library(sf)
library(tigris)
library(ggplot2)

# Get county boundaries
counties_sf <- counties(cb = TRUE, year = 2020)

# Join with traffic data for most recent year
map_data <- traffic_data %>%
  filter(year == max(year)) %>%
  mutate(GEOID = fips) %>%
  select(GEOID, traffic_fatality_rate_per_100k)

counties_map <- counties_sf %>%
  left_join(map_data, by = "GEOID")

# Create map
ggplot(counties_map) +
  geom_sf(aes(fill = traffic_fatality_rate_per_100k), color = NA) +
  scale_fill_viridis_c(option = "plasma", name = "Fatalities\nper 100k") +
  labs(title = "Traffic Fatality Rates by County") +
  theme_minimal()
```

## Interactive Dashboard

The module includes a comprehensive Shiny dashboard for interactive exploration:

```r
# Load the dashboard module
source("traffic_safety_dashboard.r")

# Launch with default settings
launch_traffic_safety_dashboard()

# Or provide data directly
traffic_data <- fetch_traffic_safety_data(years = 2015:2020)
transport_data <- load_transportation_infrastructure_data(years = 2015:2020)

launch_traffic_safety_dashboard(
  traffic_data = traffic_data,
  transport_data = transport_data,
  port = 3838,
  host = "0.0.0.0",
  launch_browser = TRUE
)
```

The dashboard offers the following features:

1. **Dashboard Overview**
   - Summary metrics and key indicators
   - National trend visualization
   - Geographic distribution map
   - Year-over-year change indicators

2. **Safety Metrics**
   - Detailed analysis of traffic safety indicators
   - County-level rankings and comparisons
   - Fatality type breakdowns
   - Risk factor analysis

3. **Transportation Infrastructure**
   - Infrastructure metrics and their relationship to safety
   - Transit usage vs. fatality rates
   - Infrastructure quality indicators
   - Multi-variable correlation analysis

4. **County Explorer**
   - County-level deep dives
   - Neighboring county comparisons
   - Trend analysis for individual counties
   - Metric cards with year-over-year changes

5. **Time Series Analysis**
   - Trend analysis with smoothing options
   - Seasonal pattern detection
   - Multi-variable trend comparisons
   - Forecast visualization

6. **Data Quality**
   - Data coverage visualizations
   - Quality metrics by variable
   - Interpolation analysis
   - Source documentation

## Advanced Configuration

### Using Parallel Processing

```r
# Enable parallel processing to speed up data retrieval and processing
traffic_data <- fetch_traffic_safety_data(
  years = 2010:2020,
  parallel = TRUE,
  # Optional: configure parallel environment
  parallel_config = list(
    workers = 4,  # Number of cores to use
    strategy = "multisession"  # Or "multicore" on Linux
  )
)
```

### Customizing Interpolation and Simulation

```r
# Control how missing data is handled
traffic_data <- fetch_traffic_safety_data(
  years = 2000:2020,
  allow_interpolation = TRUE,  # Fill gaps using time series interpolation
  allow_simulation = TRUE,     # Generate simulated data where appropriate
  
  # Customize data quality flags
  data_quality_flags = list(
    direct = "observed",       # Rename quality flag for direct observations
    interpolated = "filled",   # Custom name for interpolated values
    extrapolated = "projected", # Custom name for extrapolated values
    simulated = "estimated",   # Custom name for simulated values
    missing = NA,              # How to mark missing values
    imputed = "derived"        # Custom name for imputed values
  )
)
```

### Testing the Module

```r
# Run the comprehensive test suite
Rscript test_traffic_safety_integration.r

# Run a simpler test with fewer dependencies
Rscript test_traffic_safety_simple.r

# Test specific API functionality
Rscript test_traffic_safety_api.r
```

## Troubleshooting

### Common Issues

1. **Missing data for recent years**
   - NHTSA FARS data typically has a 1-2 year lag
   - CDC WONDER data may have even longer lags
   - Solution: Use `generate_forecasts = TRUE` for provisional estimates

2. **Error: Required package X is not installed**
   - The module has dependencies for enhanced functionality
   - Solution: Run the comprehensive installer:
   ```r
   source("install_packages.r")
   ```

3. **Error in dashboard launch**
   - Shiny and related packages might be missing
   - Solution: Install dashboard dependencies:
   ```r
   install.packages(c("shiny", "shinydashboard", "plotly", "leaflet", "DT"))
   ```

4. **Type conversion warnings**
   - May occur when joining data from different sources
   - Usually harmless, but check data with `str()` if concerned

## FAQ

### What's the difference between fetch_traffic_safety_data and fetch_enhanced_traffic_safety_data?

`fetch_traffic_safety_data` is the original function that provides basic traffic safety metrics. `fetch_enhanced_traffic_safety_data` wraps this function and adds geospatial analysis, validation, forecasting, and optimized caching. The enhanced version returns the same data structure but with additional attributes containing the enhancements.

### How are rates calculated?

Rates are calculated per 100,000 population:
```
rate = (count / population) * 100,000
```

Population data is obtained from Census sources or can be provided directly.

### How fresh is the data?

- NHTSA FARS data is typically released with a 1-2 year lag
- CDC WONDER data usually has a 1-3 year lag
- Census population estimates are available with a 1-year lag
- The forecasting module can generate projections for more recent years

### What forecasting methods are available?

The module supports multiple forecasting methods:
- **ARIMA**: Auto-regressive Integrated Moving Average
- **ETS**: Exponential Smoothing State Space models
- **Prophet**: Facebook's Prophet algorithm for time series
- **Ensemble**: Combined forecasts from multiple methods

### What do the data quality flags mean?

- **direct**: Data obtained directly from the source
- **interpolated**: Values interpolated from surrounding years
- **extrapolated**: Values projected beyond available time series
- **simulated**: Values generated based on patterns/averages
- **imputed**: Values statistically derived using covariates

### How can I extend the module?

The module is designed with extensibility in mind:
- Add new validation rules to `traffic_safety_validation.r`
- Implement additional forecasting methods in `traffic_safety_forecasting.r`
- Create new geospatial analyses in `traffic_safety_geospatial.r`
- Add caching strategies in `traffic_safety_cache.r`
- Extend the dashboard in `traffic_safety_dashboard.r`