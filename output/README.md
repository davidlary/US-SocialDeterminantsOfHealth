# Unified Social Determinants of Health County-Level Dataset

Generated on: 2025-04-20 00:23:17

## Overview

This dataset combines county-level data on social determinants of health from multiple authoritative sources:

- **U.S. Census Bureau** (Decennial Census, American Community Survey, Population Estimates Program)
- **CDC PLACES** (county-level health indicators)
- **IPUMS NHGIS** (harmonized time series data)
- **USDA Food Environment Atlas** (food access measures)
- **EPA** (environmental quality measures)
- **HUD** (housing statistics)
- **HRSA** (healthcare access measures)
- Additional specialized data sources for various SDOH domains

The data has been processed to provide consistent variable names across sources and years,
with interpolation for missing years where appropriate and comprehensive data quality tracking.

## Data Domains

This unified dataset includes variables across the following domains:

1. **Demographics**: Population, age, sex, race/ethnicity distributions
2. **Socioeconomic Status**: Income, poverty, education, employment
3. **Health Status**: Health outcomes, health behaviors, healthcare access
4. **Housing**: Home values, housing burden, overcrowding, homelessness
5. **Food Environment & Access**: Food insecurity, grocery store access, SNAP
6. **Built Environment**: Walkability, park access, recreation resources
7. **Environmental Health**: Air/water quality, toxic sites, climate indicators
8. **Transportation**: Transit access, commuting patterns, vehicle access
9. **Social Cohesion**: Civic participation, social capital
10. **Crime and Safety**: Crime rates, incarceration, safety measures

## Data Sources and URLs

| Source | Description | URL |
| ------ | ----------- | --- |
| US Census Bureau | Demographics, socioeconomic data | https://www.census.gov/data.html |
| IPUMS NHGIS | Harmonized historical Census data | https://www.nhgis.org/ |
| CDC PLACES | Local health outcome data | https://www.cdc.gov/places/ |
| IHME | Life expectancy data | https://www.healthdata.org/ |
| USDA Food Environment Atlas | Food access metrics | https://www.ers.usda.gov/data-products/food-environment-atlas/ |
| EPA Environmental Justice Screening | Environmental metrics | https://www.epa.gov/ejscreen |
| HUD Comprehensive Housing Affordability | Housing metrics | https://www.huduser.gov/portal/datasets/cp.html |
| HRSA Area Health Resources Files | Healthcare workforce and facilities | https://data.hrsa.gov/topics/health-workforce/ahrf |
| Bureau of Transportation Statistics | Transportation metrics | https://www.bts.gov/ |
| Eviction Lab | Housing stability and evictions | https://evictionlab.org/ |
| Opportunity Insights | Economic mobility metrics | https://opportunityinsights.org/ |
| National Center for Education Statistics | Education metrics | https://nces.ed.gov/ |
| FBI Uniform Crime Reports | Crime and safety metrics | https://www.fbi.gov/services/cjis/ucr |

## Data Structure

The database contains the following main tables:

- `sdoh_data` - Main data table with all variables by county and year
- `counties` - Information about each county
- `variables` - Descriptions and metadata for each variable

And the following views:

- `latest_county_data` - The most recent data available for each county and variable
- `county_time_series` - All years of data for all counties
- Domain-specific views for each major data domain
- Summary views for data quality assessment

## Data Quality Flags

Each record includes data quality indicators:

- `data_quality` - One of: 'direct' (from source), 'interpolated' (gap-filled), 'extrapolated' (extended), 'simulated' (for estimation), or 'imputed' (statistically derived)
- `data_source` - Original source of the data
- `data_vintage` - Year and specific collection the data came from

## Usage Examples

```r
# Connect to the database
library(DBI)
library(duckdb)
con <- dbConnect(duckdb::duckdb(), 'output/us_county_sdoh_unified.duckdb')

# Get the latest data for all counties
latest_data <- dbGetQuery(con, "SELECT * FROM latest_county_data")

# Get time series data for a specific county
la_county <- dbGetQuery(con, "
  SELECT * FROM county_time_series 
  WHERE geoid = '06037' -- Los Angeles County
  ORDER BY variable_name, year
")

# Get variables for a specific domain
food_env_data <- dbGetQuery(con, "SELECT * FROM food_environment_variables")

# Close the connection
dbDisconnect(con)
```

## Running the Pipeline

```bash
# Install required packages
Rscript R/install_packages.r

# Run the unified pipeline with default settings
Rscript R/unified_sdoh_pipeline.r

# Run with specific options
Rscript R/unified_sdoh_pipeline.r --force-update --verbose
```

## Command Line Options

- `--force-update` or `-f`: Force refresh of all cached data
- `--verbose` or `-v`: Show detailed processing information
- `--skip-interpolation`: Disable interpolation for missing data points
- `--allow-simulation`: Allow simulated data where real data is unavailable
- `--offline-mode` or `--offline`: Run in offline mode using only cached data

## Citation

If you use this dataset in your research or applications, please cite it as:

```
Unified Social Determinants of Health County-Level Dataset (2025). Generated using data from U.S. Census Bureau, CDC PLACES, IPUMS NHGIS, and other authoritative sources.
```
