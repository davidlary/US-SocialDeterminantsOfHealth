# Unified Pipeline Guide: Reading All 255+ Variables with Real Data

This guide explains how to use the comprehensive pipeline to load real data for all 255+ variables across all U.S. counties, including traffic safety data.

## Quick Start

To rebuild the database with all 255+ variables including traffic safety data, use the `run_full_database_rebuild_and_verification.sh` script:

```bash
# Make the script executable
chmod +x run_full_database_rebuild_and_verification.sh

# Run the script
./run_full_database_rebuild_and_verification.sh
```

This script:
1. Creates a unified database with proper schema
2. Defines all 255+ variables including traffic safety variables
3. Populates the database with real county-level data
4. Verifies all variables are properly loaded
5. Performs specific verification of traffic safety data
6. Generates county-level maps for all variables

For the original optimized pipeline (without guaranteed traffic safety data), use:

```bash
Rscript run_full_data_load.r
```

## Verifying the Data

The pipeline creates a DuckDB database at `output/us_county_sdoh_unified.duckdb` containing:
- All 255+ variables for all counties (including traffic safety variables)
- Data quality indicators for each datapoint ('direct', 'interpolated', 'estimated')
- Source and vintage information for each record

To verify the database contents:

```r
library(DBI)
library(duckdb)

# Connect to database
con <- dbConnect(duckdb(), dbdir = "output/us_county_sdoh_unified.duckdb")

# Check variable count
dbGetQuery(con, "SELECT COUNT(DISTINCT variable_name) FROM sdoh_data")

# Check data quality
dbGetQuery(con, "SELECT data_quality, COUNT(*) AS count FROM sdoh_data GROUP BY data_quality")

# Check county coverage
dbGetQuery(con, "SELECT COUNT(DISTINCT geoid) FROM sdoh_data")

# Close connection
dbDisconnect(con, shutdown = TRUE)
```

## Pipeline Components

The unified pipeline consists of these main components:

1. **Variable Crosswalk**: Defines all 255+ variables, their metadata, and domains
2. **Data Fetchers**: Modules for each data category (Census, NHGIS, EPA, NHTSA, etc.)
3. **Data Processors**: Handle cleaning, standardization, and quality indicators
4. **Database Builder**: Creates and populates the unified database
5. **Map Generator**: Creates visualizations for all variables

## Available Variables by Domain

The database includes variables from these domains:

- **Demographics and Race/Ethnicity** (30+ variables)
- **Socioeconomic Status** (25+ variables)
- **Education** (20+ variables)
- **Housing** (25+ variables)
- **Transportation** (15+ variables)
- **Health Behaviors and Outcomes** (40+ variables)
- **Healthcare Access and Insurance** (15+ variables)
- **Environmental Factors** (25+ variables)
- **Traffic Safety** (12 variables)
- **Food Environment and Access** (20+ variables)
- **Social Cohesion and Capital** (10+ variables)
- **Built Environment** (20+ variables)

## Data Sources

The pipeline reads data from multiple sources:

- **U.S. Census Bureau**: Demographics, economic, housing data (ACS, Decennial Census, PEP)
- **CDC**: Health outcomes, behaviors (PLACES, WONDER)
- **EPA**: Environmental quality (Air Quality System, EJSCREEN)
- **NHTSA**: Traffic safety (FARS)
- **USDA**: Food environment (Food Environment Atlas)
- **IHME**: Health metrics (life expectancy)
- **Multiple others**: Education, healthcare, social factors

## Data Quality Management

When real data isn't available for specific years or counties:
- The system uses data interpolation between available years
- Statistical estimation for sparse data
- All data points are marked with quality indicators:
  - **direct**: Raw data from authoritative sources
  - **interpolated**: Calculated from surrounding years
  - **estimated**: Derived from models/methods
  - **pending**: Not available (minimized in current implementation)

## Performance Optimization

For large datasets, the pipeline uses:
- Parallel processing across multiple cores
- Efficient data caching
- Memory-optimized operations
- Incremental database updates

## Traffic Safety Variables

The database includes these traffic safety variables from NHTSA FARS and CDC WONDER:

| Variable Name | Description | Units |
|---------------|-------------|-------|
| traffic_fatalities | Number of motor vehicle crash fatalities | count |
| traffic_fatality_rate | Motor vehicle crash fatalities per 100,000 population | rate |
| pedestrian_fatalities | Number of pedestrian fatalities | count |
| pedestrian_fatality_rate | Pedestrian fatalities per 100,000 population | rate |
| bicycle_fatalities | Number of bicyclist fatalities | count |
| bicycle_fatality_rate | Bicyclist fatalities per 100,000 population | rate |
| motorcycle_fatalities | Number of motorcycle fatalities | count |
| motorcycle_fatality_rate | Motorcycle fatalities per 100,000 population | rate |
| alcohol_impaired_fatalities | Number of alcohol-impaired driving fatalities | count |
| alcohol_impaired_fatality_rate | Alcohol-impaired driving fatalities per 100,000 population | rate |
| speeding_related_fatalities | Number of speeding-related fatalities | count |
| speeding_related_fatality_rate | Speeding-related fatalities per 100,000 population | rate |

## Troubleshooting

If you encounter issues:

1. **Memory Errors**: Reduce parallel workers in configuration or use direct approach
2. **Missing Data**: Check the data source directories in `data/`
3. **Slow Performance**: Use the `refresh_cache = FALSE` option or rebuild with static data
4. **Database Errors**: Try the direct database creation with `create_unified_database_with_all_variables.r`

## Next Steps

After loading all data:
- Analyze the data using SQL through DuckDB
- View generated maps in `output/maps/`
- Create custom visualizations using the database
- Run forecasting models with `ml_forecasting.r`
- Set up the API server with `api_server.r` to provide data access
- Generate county-level maps with `generate_county_maps.r`