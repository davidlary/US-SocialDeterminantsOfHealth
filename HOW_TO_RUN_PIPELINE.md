# How to Run the SDOH Pipeline with All 255+ Variables

This guide provides step-by-step instructions for running the Social Determinants of Health (SDOH) pipeline with all 255+ variables, including traffic safety data.

## Prerequisites

1. R version 4.0 or higher
2. Required R packages installed (see below)
3. Access to data or use cached data (will be automatically handled)

### Required Packages

Run the following to install all required packages:

```r
Rscript install_packages.r
```

Key packages needed include:
- `tidyverse`, `dplyr`, `DBI`, `duckdb` (core pipeline)
- `sf`, `tigris` (for spatial data)
- `httr`, `jsonlite` (for API access)
- `future`, `future.apply` (for parallel processing)

## Quick Start: Complete Database with All Variables

The recommended approach to create a complete database with all 255+ variables and generate maps:

```bash
# Make the script executable first
chmod +x run_full_database_rebuild_and_verification.sh

# Run the complete database rebuild with map generation
./run_full_database_rebuild_and_verification.sh
```

This master script:
1. Creates the database with proper schema
2. Defines all 255+ variables including traffic safety
3. Populates the database with real county-level data
4. Verifies all variables are properly loaded
5. Generates maps for visualization
6. Reports on data quality and coverage

## Alternative: Running the Original Pipeline

The original unified pipeline can also be used, but requires additional steps to ensure traffic safety data integration:

```bash
# Run the complete pipeline with all default settings
Rscript unified_sdoh_pipeline.r

# Run with a custom configuration file
Rscript unified_sdoh_pipeline.r path/to/custom_config.yaml
```

### Command Line Options

The pipeline supports several command-line options:

```bash
# Force a complete refresh of cached data
Rscript unified_sdoh_pipeline.r --force-update

# Overwrite the existing database
Rscript unified_sdoh_pipeline.r --overwrite-db

# Force a full rebuild of all data
Rscript unified_sdoh_pipeline.r --force-full-rebuild

# Process all 255 variables in the crosswalk
Rscript unified_sdoh_pipeline.r --process-all-variables

# Restart from a specific step
Rscript unified_sdoh_pipeline.r --restart-from=fetch
```

Valid restart points include:
- `crosswalk`: Restart from the variable crosswalk building step
- `fetch`: Restart from the data fetching step
- `process`: Restart from the data processing step
- `database`: Restart from the database creation step
- `maps`: Restart from the map generation step
- `documentation`: Restart from the documentation generation step

## Step-by-Step Approach

If you prefer to run each step individually:

### 1. Create the Database with All Variables

```r
# Create a comprehensive database with all 255+ variables
Rscript create_unified_database_with_all_variables.r
```

This script:
- Creates the database schema with tables for counties, variables, and data
- Defines all 255+ variables with proper metadata
- Populates the database with real data for all counties
- Uses data quality indicators for each data point

### 2. Verify All Variables

```r
# Verify that all variables exist and have data
Rscript verify_all_variables.r
```

This script:
- Checks that all 255+ variables are defined in the database
- Verifies data quality and coverage
- Reports on any missing or incomplete data

### 3. Verify Traffic Safety Data Specifically

```r
# Specifically verify traffic safety variables
Rscript verify_traffic_safety_data.r
```

This script:
- Focuses on the 12 traffic safety variables
- Verifies that they have real data (not "pending")
- Reports on their coverage and quality

### 4. Generate Maps for Visualization

```r
# Generate maps for all variables
Rscript generate_county_maps.r
```

This creates county-level choropleth maps for all variables across available years.

## Working with Traffic Safety Data

The traffic safety component includes 12 variables from NHTSA FARS and CDC WONDER:

1. `traffic_fatalities` - Count of motor vehicle crash fatalities
2. `traffic_fatality_rate` - Traffic fatalities per 100,000 population
3. `pedestrian_fatalities` - Count of pedestrian fatalities
4. `pedestrian_fatality_rate` - Pedestrian fatalities per 100,000 population
5. `bicycle_fatalities` - Count of bicyclist fatalities 
6. `bicycle_fatality_rate` - Bicyclist fatalities per 100,000 population
7. `motorcycle_fatalities` - Count of motorcycle fatalities
8. `motorcycle_fatality_rate` - Motorcycle fatalities per 100,000 population
9. `alcohol_impaired_fatalities` - Count of alcohol-impaired driving fatalities
10. `alcohol_impaired_fatality_rate` - Alcohol-impaired driving fatalities per 100,000 population
11. `speeding_related_fatalities` - Count of speeding-related fatalities
12. `speeding_related_fatality_rate` - Speeding-related fatalities per 100,000 population

To specifically analyze traffic safety data:

```r
# Connect to the database
library(DBI)
library(duckdb)
library(dplyr)

# Connect to database
con <- dbConnect(duckdb(), dbdir = "output/us_county_sdoh_unified.duckdb")

# Get traffic fatality rates by state (aggregated by county)
state_fatality_rates <- dbGetQuery(con, "
  SELECT 
    c.state_name, 
    d.year,
    AVG(d.value) as avg_fatality_rate
  FROM 
    counties c
    JOIN sdoh_data d ON c.geoid = d.geoid
  WHERE 
    d.variable_name = 'traffic_fatality_rate'
    AND d.year BETWEEN 2018 AND 2022
  GROUP BY 
    c.state_name, d.year
  ORDER BY 
    avg_fatality_rate DESC, c.state_name, d.year
")

# Close connection
dbDisconnect(con, shutdown = TRUE)
```

## Testing Individual Components

### Testing Geospatial Analysis

```r
# Load the geospatial module
source("traffic_safety_geospatial.r")

# Run spatial analysis on traffic fatality rates
spatial_result <- analyze_traffic_safety_spatial(
  traffic_data,
  variable_name = "traffic_fatality_rate_per_100k",
  year = 2021
)

# Identify hotspots
hotspots <- identify_traffic_safety_hotspots(
  traffic_data,
  variable_name = "traffic_fatality_rate_per_100k",
  year = 2021
)
```

### Testing Forecasting

```r
# Load the forecasting module
source("traffic_safety_forecasting.r")

# Generate forecasts for future years
forecast_result <- generate_traffic_forecast(
  traffic_data,
  forecast_years = 3,
  method = "ensemble",
  variable_name = "traffic_fatality_rate_per_100k"
)
```

### Running the Dashboard

```r
# Launch the interactive dashboard
source("traffic_safety_dashboard.r")
launch_traffic_safety_dashboard(traffic_data = traffic_data)
```

## Configuration Options

The pipeline's behavior can be customized through the `config.yaml` file. Key settings related to traffic safety include:

```yaml
traffic_safety:
  data_years: [2015, 2016, 2017, 2018, 2019, 2020, 2021]
  use_fallback: false
  fetch_cdc_data: true
  allow_interpolation: true
  validate_data: true
```

## Troubleshooting

### Common Issues

1. **Memory Errors**: 
   - If you encounter out-of-memory errors, use the direct database creation approach
   - Reduce the parallel workers or simplify the analysis

2. **Missing Data Files**:
   - Ensure all required data files are in their expected locations in the `data/` directory
   - Traffic safety data should be in `data/traffic_safety/fars/` and `data/traffic_safety/cdc/`

3. **Failed Verification**:
   - If verification fails, check the specific variables that are problematic
   - Try rebuilding the database using `create_unified_database_with_all_variables.r`

4. **Database Access Issues**:
   - Ensure the database is not locked by another process
   - Check file permissions for the database file
   - Use the `read_only = TRUE` parameter for read-only access

### Getting Help

For more detailed information, consult these resources:

- `UNIFIED_PIPELINE_GUIDE.md`: Comprehensive pipeline guide
- `TRAFFIC_SAFETY_IMPLEMENTATION_SUMMARY.md`: Detailed documentation of traffic safety implementation
- `docs/data_sources/TRAFFIC_SAFETY_DATA.md`: Documentation of traffic safety variables
- `COMPLETE_IMPLEMENTATION_SUMMARY.md`: Complete overview of the implementation

## Database Structure

The unified database has a clear, simple structure:

- `counties`: Contains county geographic information (GEOID, name, state)
- `variables`: Defines all 255+ variables with metadata (name, description, units, category, source)
- `sdoh_data`: The main data table containing all values with quality indicators

### Example Queries

Here are some example queries you can run on the database:

#### 1. Basic Variable Query

```r
# Connect to the database
library(DBI)
library(duckdb)
con <- dbConnect(duckdb(), dbdir = "output/us_county_sdoh_unified.duckdb")

# Query a specific variable for a specific year
traffic_data <- dbGetQuery(con, "
  SELECT c.geoid, c.name AS county_name, c.state_name, 
         d.value, d.data_quality
  FROM counties c
  JOIN sdoh_data d ON c.geoid = d.geoid
  WHERE d.variable_name = 'traffic_fatality_rate' 
  AND d.year = 2021
  ORDER BY d.value DESC
  LIMIT 20
")

# Disconnect when done
dbDisconnect(con, shutdown = TRUE)
```

#### 2. Cross-Domain Analysis

```r
# Query relationships between multiple domains
cross_domain <- dbGetQuery(con, "
  WITH income_data AS (
    SELECT geoid, value as median_income
    FROM sdoh_data
    WHERE variable_name = 'median_household_income' AND year = 2020
  ),
  traffic_data AS (
    SELECT geoid, value as fatality_rate
    FROM sdoh_data
    WHERE variable_name = 'traffic_fatality_rate' AND year = 2020
  ),
  education_data AS (
    SELECT geoid, value as college_pct
    FROM sdoh_data
    WHERE variable_name = 'bachelors_or_higher_pct' AND year = 2020
  )
  SELECT c.state_name, 
         AVG(i.median_income) as avg_income,
         AVG(t.fatality_rate) as avg_fatality_rate,
         AVG(e.college_pct) as avg_college_pct
  FROM counties c
  JOIN income_data i ON c.geoid = i.geoid
  JOIN traffic_data t ON c.geoid = t.geoid
  JOIN education_data e ON c.geoid = e.geoid
  GROUP BY c.state_name
  ORDER BY avg_fatality_rate DESC
")
```

#### 3. Trend Analysis

```r
# Query trends over time
trend_data <- dbGetQuery(con, "
  SELECT c.state_name, d.year, 
         AVG(d.value) as avg_value
  FROM counties c
  JOIN sdoh_data d ON c.geoid = d.geoid
  WHERE d.variable_name = 'traffic_fatality_rate'
  AND d.year BETWEEN 2018 AND 2022
  GROUP BY c.state_name, d.year
  ORDER BY c.state_name, d.year
")
```