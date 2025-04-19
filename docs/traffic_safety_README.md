# Traffic Safety Module

This enhanced module provides comprehensive traffic safety data analysis capabilities for the Social Determinants of Health (SDOH) pipeline. It includes geospatial analysis, data validation, time series forecasting, interactive dashboard, and optimized caching.

## Key Features

### 1. Geospatial Analysis
- Spatial cluster identification (LISA, Moran's I, Getis-Ord G*)
- Traffic safety hotspot mapping
- Comparison of neighboring counties
- Problem corridor identification
- Spatial-temporal animations

### 2. Data Validation
- Comprehensive validation rules for traffic safety data
- Outlier detection and handling
- Temporal consistency checks
- Spatial consistency validation
- Automated quality reports in multiple formats

### 3. Time Series Forecasting
- Traffic fatality trend analysis
- Seasonal decomposition
- Multiple forecasting methods (ARIMA, ETS, Prophet, ensemble)
- County-level forecast generation
- Identification of counties with concerning trends

### 4. Interactive Dashboard
- Multi-tab Shiny web application
- Interactive maps and visualizations
- County comparison tools
- Forecast visualization
- Data quality transparency
- Export capabilities

### 5. Optimized Caching
- Smart expiry based on data type
- Incremental updates for certain data sources
- Cache compression and size management
- Cache diagnostics and reporting

### 6. Integration with Main Pipeline
- Seamless integration with the unified SDOH pipeline
- Automatic generation of visualizations
- Custom database tables and views for enhanced analyses
- Graceful degradation if components are missing

## Components

- `traffic_safety_integration.r` - Main integration module
- `traffic_safety_geospatial.r` - Geospatial analysis functions
- `traffic_safety_validation.r` - Data validation framework
- `traffic_safety_forecasting.r` - Time series forecasting capabilities
- `traffic_safety_cache.r` - Enhanced caching system
- `traffic_safety_dashboard.r` - Interactive Shiny dashboard
- `traffic_safety_api_tests.r` - API integration tests

## Usage

### Module Integration

The module is automatically detected and used by the unified SDOH pipeline when present. No additional configuration is needed.

For standalone usage:

```r
# Load the integration module
source("traffic_safety_integration.r")

# Fetch enhanced traffic safety data
data <- fetch_enhanced_traffic_safety_data(
  years = 2010:2022,
  use_validation = TRUE,
  use_optimized_cache = TRUE,
  generate_forecasts = TRUE,
  spatial_analysis = TRUE
)

# Create visualizations
vis_files <- create_traffic_safety_visualizations(
  data,
  output_dir = "output/visualizations/traffic_safety",
  create_maps = TRUE,
  create_forecast_plots = TRUE,
  create_animation = TRUE
)
```

### Interactive Dashboard

```r
# Launch the dashboard application
source("traffic_safety_dashboard.r")
launch_traffic_safety_dashboard(
  traffic_data = data,  # Optional - will load data if not provided
  port = 3838,
  host = "0.0.0.0",
  launch_browser = TRUE
)
```

### Database Integration

```r
# Add enhanced traffic safety data to the SDOH database
add_traffic_safety_to_database(
  traffic_data = data,
  db_path = "us_county_sdoh_data.duckdb",
  add_forecasts = TRUE,
  add_spatial = TRUE
)
```

## Data Sources

This module analyzes traffic safety data from multiple authoritative sources:
- NHTSA Fatality Analysis Reporting System (FARS)
- CDC WONDER mortality data
- Census population estimates
- State transportation department data (where available)
- Federal Highway Administration (FHWA) data

## Metrics

The module provides the following key metrics:

| Metric | Description | Unit |
|--------|-------------|------|
| `traffic_fatality_count` | Total traffic fatalities | Count |
| `traffic_fatality_rate_per_100k` | Traffic fatality rate per 100k population | Rate |
| `dui_fatality_count` | Alcohol-involved fatalities | Count |
| `dui_fatality_rate_per_100k` | Alcohol-involved fatality rate | Rate |
| `ped_bike_fatality_count` | Pedestrian/cyclist fatalities | Count |
| `ped_bike_fatality_rate_per_100k` | Pedestrian/cyclist fatality rate | Rate |
| `speeding_fatality_count` | Speed-related fatalities | Count |
| `speeding_fatality_rate_per_100k` | Speed-related fatality rate | Rate |

## Dashboard Features

The Traffic Safety Dashboard provides an interactive web interface structured in tabs:

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

## Testing

To run the comprehensive test suite:

```r
# Run all tests
Rscript test_traffic_safety_integration.r

# Run specific API tests
Rscript test_traffic_safety_api.r
```

## Output

The module generates:
- Enhanced traffic safety dataset with quality flags
- Hotspot maps and spatial cluster visualizations
- Trend analysis and forecasts
- Data quality reports
- Custom database tables and views
- Interactive dashboard application

## Requirements

Required R packages:
- Core: tidyverse, R6, lubridate, jsonlite
- Dashboard: shiny, shinydashboard, plotly, leaflet, DT
- Geospatial: sf, spdep, tmap, tigris
- Forecasting: forecast, tseries, zoo
- Caching: digest, R.utils
- Validation: assertthat

## Acknowledgements

This module uses data from multiple federal agencies including NHTSA, CDC, Census Bureau, and FHWA.