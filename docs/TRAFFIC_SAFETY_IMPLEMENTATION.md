# Traffic Safety Data Implementation

This document describes the implementation of the traffic safety data module in the SDOH pipeline.

## Overview

The traffic safety module integrates data from the National Highway Traffic Safety Administration's Fatality Analysis Reporting System (FARS) into the SDOH database. This data provides county-level information on traffic fatalities, including breakdowns by type (pedestrian, bicycle, motorcycle) and contributing factors (alcohol impairment, speeding).

## Data Sources

The primary data source is FARS, which provides yearly data on all fatal motor vehicle crashes in the United States. FARS data files are stored in:

```
data/traffic_safety/fars/FARS_[YEAR]_county.csv
```

For example: `data/traffic_safety/fars/FARS_2020_county.csv`

## Implementation

The traffic safety integration is implemented in `traffic_safety_integration.r`, which provides the following functions:

### Key Functions

1. `get_traffic_safety_variable_names()` - Returns a list of all traffic safety variables that should be included in the database
2. `load_traffic_safety_data(file_path, cache_dir, refresh)` - Loads data from a specific FARS file
3. `process_traffic_safety_data(data)` - Processes raw traffic safety data to ensure it has all required variables
4. `create_dummy_traffic_safety_data(n_counties, years)` - Creates placeholder data for demonstration/testing
5. `get_traffic_safety_data(years, refresh, parallel, parallel_config)` - Main function for retrieving traffic safety data for multiple years

### Variables

The module currently processes the following 12 key traffic fatality variables:

| Variable Name | Description | Type |
|---------------|-------------|------|
| `traffic_fatalities` | Total traffic fatalities | numeric_count |
| `traffic_fatality_rate` | Traffic fatalities per 100,000 population | numeric_rate |
| `pedestrian_fatalities` | Pedestrian traffic fatalities | numeric_count |
| `pedestrian_fatality_rate` | Pedestrian fatalities per 100,000 population | numeric_rate |
| `bicycle_fatalities` | Bicycle traffic fatalities | numeric_count |
| `bicycle_fatality_rate` | Bicycle fatalities per 100,000 population | numeric_rate |
| `motorcycle_fatalities` | Motorcycle traffic fatalities | numeric_count |
| `motorcycle_fatality_rate` | Motorcycle fatalities per 100,000 population | numeric_rate |
| `alcohol_impaired_fatalities` | Alcohol-impaired driving fatalities | numeric_count |
| `alcohol_impaired_fatality_rate` | Alcohol-impaired fatalities per 100,000 population | numeric_rate |
| `speeding_related_fatalities` | Speeding-related traffic fatalities | numeric_count |
| `speeding_related_fatality_rate` | Speeding-related fatalities per 100,000 population | numeric_rate |

## Integration with Pipeline

The traffic safety module is integrated into the SDOH pipeline in `unified_sdoh_pipeline.r`. The pipeline loads the module and calls `get_traffic_safety_data()` to retrieve the processed data. The module supports parallel processing when the appropriate configuration is provided.

## Data Quality

Data quality is tracked for each variable using columns with the pattern `data_quality_[variable_name]`, which can have the following values:

- `direct` - Data directly from source file
- `derived` - Data calculated from other variables (e.g., rates calculated from counts and population)
- `missing` - No data available for this variable
- `interpolated` - Data has been interpolated (not currently implemented in this module)

## Handling Missing Data

The module handles missing data in the following ways:

1. For years with no available FARS data file, it creates placeholder entries with NULL values
2. Missing variables are included with NULL values rather than synthetic data
3. The module tries to find alternative data files if the primary file doesn't exist

## Caching

The module supports caching of processed data to improve performance. Cached data is stored in:

```
data/cache/traffic_safety_data.rds
```

The cache can be refreshed by setting the `refresh` parameter to `TRUE` when calling `get_traffic_safety_data()`.

## Future Enhancements

Potential future enhancements to the traffic safety module include:

1. Adding more variables from the full FARS dataset
2. Implementing temporal interpolation for missing years
3. Adding additional data sources for traffic safety beyond FARS
4. Integrating with county-level population data for better rate calculations
5. Adding confidence intervals and uncertainty estimates