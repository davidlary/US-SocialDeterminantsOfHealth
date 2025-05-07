# Traffic Safety Module

This enhanced module provides comprehensive traffic safety data analysis capabilities for the Social Determinants of Health (SDOH) pipeline. It includes geospatial analysis, data validation, time series forecasting, interactive dashboard, and optimized caching.

## Quick Start

Run the complete implementation with a single command:

```bash
./run_full_pipeline.sh
```

This script will:
1. Remove existing database file to start fresh
2. Fix traffic safety integration
3. Populate the database with traffic safety data
4. Run the unified pipeline with a force update
5. Verify the implementation

## Individual Scripts

If you prefer to run the steps individually:

1. Fix traffic safety integration:
   ```bash
   Rscript fix_traffic_safety_integration.r
   ```

2. Populate the database with traffic safety data:
   ```bash
   Rscript populate_traffic_safety_data.r
   ```

3. Run the unified pipeline with force update:
   ```bash
   Rscript unified_sdoh_pipeline.r --force-update
   ```

4. Verify the implementation:
   ```bash
   Rscript check_traffic_safety_variables.r
   ```

## Alternative: Direct Database Creation

If you're experiencing issues with the full pipeline, you can use the direct database creation script to create a simplified database with traffic safety data:

```bash
Rscript create_unified_database_with_traffic_safety.r
```

This script:
1. Creates a new database with the proper schema
2. Adds county information from Census data
3. Creates traffic safety variables
4. Populates the database with sample traffic safety data
5. Verifies the data has been properly added

After running this script, you can verify the traffic safety data:

```bash
Rscript verify_traffic_safety_data.r
```

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
- `fix_traffic_safety_integration.r` - Script to fix integration issues
- `populate_traffic_safety_data.r` - Script to populate database with traffic safety data
- `check_traffic_safety_variables.r` - Script to verify traffic safety data in database
- `traffic_safety_geospatial.r` - Geospatial analysis functions
- `traffic_safety_validation.r` - Data validation framework
- `traffic_safety_forecasting.r` - Time series forecasting capabilities
- `traffic_safety_cache.r` - Enhanced caching system
- `traffic_safety_dashboard.r` - Interactive Shiny dashboard
- `traffic_safety_api_tests.r` - API integration tests

## Traffic Safety Variables

The following 12 traffic safety variables are included:

- `traffic_fatalities` - Number of motor vehicle crash fatalities
- `traffic_fatality_rate` - Motor vehicle crash fatalities per 100,000 population
- `pedestrian_fatalities` - Number of pedestrian fatalities
- `pedestrian_fatality_rate` - Pedestrian fatalities per 100,000 population  
- `bicycle_fatalities` - Number of bicyclist fatalities
- `bicycle_fatality_rate` - Bicyclist fatalities per 100,000 population
- `motorcycle_fatalities` - Number of motorcycle fatalities
- `motorcycle_fatality_rate` - Motorcycle fatalities per 100,000 population
- `alcohol_impaired_fatalities` - Number of alcohol-impaired driving fatalities
- `alcohol_impaired_fatality_rate` - Alcohol-impaired driving fatalities per 100,000 population
- `speeding_related_fatalities` - Number of speeding-related fatalities
- `speeding_related_fatality_rate` - Speeding-related fatalities per 100,000 population

## Usage

### Module Integration

The module is automatically detected and used by the unified SDOH pipeline when present. No additional configuration is needed.

For standalone usage:

```r
# Load the integration module
source("traffic_safety_integration.r")

# Fetch enhanced traffic safety data
data <- get_traffic_safety_data(
  years = 2010:2022,
  refresh = TRUE,
  parallel = TRUE
)
```

### Database Integration

The traffic safety data is automatically integrated into the SDOH database when running the unified pipeline:

```r
# Run the unified pipeline with force update
Rscript unified_sdoh_pipeline.r --force-update
```

If you need to manually populate the database with traffic safety data:

```r
# Populate the database with traffic safety data
Rscript populate_traffic_safety_data.r
```

## Data Sources

This module analyzes traffic safety data from multiple authoritative sources:
- NHTSA Fatality Analysis Reporting System (FARS)
- CDC WONDER mortality data
- Census population estimates
- State transportation department data (where available)
- Federal Highway Administration (FHWA) data

## Troubleshooting

If you encounter any issues:

1. Check the database for traffic safety data:
   ```bash
   Rscript check_traffic_safety_variables.r
   ```

2. Verify the cache file exists:
   ```bash
   ls -l data/cache/traffic_safety_data.rds
   ```

3. Check for traffic safety maps:
   ```bash
   ls -l output/maps/by_variable/traffic_fatality_rate_*.png
   ```

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