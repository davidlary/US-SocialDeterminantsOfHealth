# Traffic Safety Data Module

## Overview

This directory contains traffic safety data from authoritative sources, focusing on traffic fatalities, injuries, and related risk factors at the county level across the United States. The data comes primarily from the National Highway Traffic Safety Administration's Fatality Analysis Reporting System (NHTSA FARS) and the CDC WONDER mortality database.

## Directory Structure

- `/traffic_safety/fars/` - NHTSA FARS data at county level (1975-present)
- `/traffic_safety/cdc/` - CDC WONDER transportation mortality data (1970-present)

## Data Sources

### NHTSA Fatality Analysis Reporting System (FARS)

The FARS database is a nationwide census of fatal injuries in motor vehicle crashes. It contains detailed data on all vehicle crashes in the United States that occur on a public roadway and involve a fatality.

- **Official Website**: https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars
- **Data Format**: Annual county-level summaries with fatality counts
- **Years Available**: 1975 to present
- **Update Frequency**: Annual (with approximately 1-year lag)

### CDC WONDER Multiple Cause of Death

CDC WONDER's Multiple Cause of Death data provides access to mortality information, including transportation-related deaths.

- **Official Website**: https://wonder.cdc.gov/
- **Data Format**: Annual county-level mortality data for transportation-related causes
- **Years Available**: 1970 to present
- **Update Frequency**: Annual (with approximately 1-2 year lag)

## Variables Available

| Variable | Description | Unit | Source |
|----------|-------------|------|--------|
| traffic_fatality_count | Total traffic fatalities | Count | NHTSA FARS |
| traffic_fatality_rate_per_100k | Traffic fatality rate per 100,000 population | Rate | NHTSA FARS + Census |
| traffic_injury_count | Traffic injuries | Count | NHTSA FARS |
| traffic_injury_rate_per_100k | Traffic injury rate per 100,000 population | Rate | NHTSA FARS + Census |
| ped_bike_fatality_count | Pedestrian/cyclist fatalities | Count | NHTSA FARS |
| ped_bike_fatality_rate_per_100k | Pedestrian/cyclist fatality rate per 100,000 population | Rate | NHTSA FARS + Census |
| dui_fatality_count | DUI-related fatalities | Count | NHTSA FARS |
| dui_fatality_rate_per_100k | DUI-related fatality rate per 100,000 population | Rate | NHTSA FARS + Census |
| speeding_fatality_count | Speeding-related fatalities | Count | NHTSA FARS |
| speeding_fatality_rate_per_100k | Speeding-related fatality rate per 100,000 population | Rate | NHTSA FARS + Census |
| transport_mortality_count | Total transport-related mortality | Count | CDC WONDER |

## Fallback Mechanism

The traffic safety module includes robust fallback mechanisms to ensure data availability even when external APIs are unavailable:

1. **Primary API Access**: First attempts to fetch data from official APIs
2. **Alternative APIs**: If primary API fails, tries alternative endpoints
3. **Direct File Download**: If APIs are unavailable, attempts direct file downloads
4. **Pre-downloaded Data**: Uses locally stored data files when all online sources fail
5. **Sample Data**: As a last resort, uses realistic sample data based on real county-level statistics

## Usage in R

The traffic safety data can be accessed through the main SDOH pipeline or directly using the traffic safety module:

```r
# Load the module
source("R/fetch_traffic_safety_data.r")

# Fetch traffic safety data for specific years
traffic_data <- fetch_traffic_safety_data(
  years = 2010:2020,
  cache_dir = "data/cache",
  refresh_cache = FALSE,
  allow_interpolation = TRUE
)

# Access enhanced features through the integration module
source("R/traffic_safety_integration.r")
enhanced_data <- fetch_enhanced_traffic_safety_data(
  years = 2010:2020,
  cache_dir = "data/cache",
  refresh_cache = FALSE,
  generate_forecasts = TRUE,
  spatial_analysis = TRUE
)
```

## Module Components

The traffic safety module consists of several R scripts:

- **fetch_traffic_safety_data.r** - Main data fetcher for traffic safety data
- **traffic_safety_integration.r** - Integration with the unified SDOH pipeline
- **traffic_safety_cache.r** - Optimized caching system
- **traffic_safety_validation.r** - Data quality validation
- **traffic_safety_forecasting.r** - Time series forecasting with multiple models
- **traffic_safety_geospatial.r** - Spatial analysis and mapping of traffic safety data

## Contact

For questions or issues related to the traffic safety module, please contact David Lary (davidlary@me.com).

## Last Updated

April 19, 2025