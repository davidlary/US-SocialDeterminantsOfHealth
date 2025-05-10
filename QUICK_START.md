# SDOH Database: Quick Start Guide

This guide provides quick instructions for using the Social Determinants of Health (SDOH) database with all 255+ variables including traffic safety data.

## Quick Start: Create Complete Database

The fastest way to create a complete database with all variables:

```bash
# Make the script executable
chmod +x run_full_database_rebuild_and_verification.sh

# Run the comprehensive implementation
./run_full_database_rebuild_and_verification.sh
```

This single command:
- Creates the database with all tables and indexes
- Adds all 255+ variables with proper metadata
- Populates the database with data for all variables
- Verifies all variables including traffic safety
- Generates maps for all variables
- Reports on data quality and coverage

## Alternative Approaches

### Direct Database Creation

```bash
# Create the database directly
Rscript create_unified_database_with_all_variables.r
```

This creates a complete database with all 255+ variables including traffic safety.

### Original Pipeline with Fixes

```bash
# Run the original pipeline with fixes
Rscript unified_sdoh_pipeline.r
```

### Verify Database

```bash
# Verify all variables
Rscript verify_all_variables.r

# Verify traffic safety specifically
Rscript verify_traffic_safety_data.r
```

These scripts check that the database contains all 255+ variables with proper data quality.

## Working with the Database

### Query Example

```r
library(DBI)
library(duckdb)

# Connect to the database
con <- dbConnect(duckdb(), dbdir = "output/us_county_sdoh_unified.duckdb")

# Basic query for traffic fatality rates
fatality_data <- dbGetQuery(con, "
  SELECT c.geoid, c.name, c.state_name, d.year, d.value
  FROM counties c
  JOIN sdoh_data d ON c.geoid = d.geoid
  WHERE d.variable_name = 'traffic_fatality_rate'
  AND d.year = 2020
  ORDER BY d.value DESC
  LIMIT 20
")

# Get a list of all available variables
variables <- dbGetQuery(con, "SELECT variable_name, category FROM variables")

# Close connection
dbDisconnect(con, shutdown = TRUE)
```

### Maps

Maps are automatically generated when running the full script. They are available in:

```
output/maps/by_variable/
```

To regenerate maps manually:

```r
# Generate maps for visualization
Rscript generate_county_maps.r
```

## Available Variables

The database includes 255+ variables across multiple domains:

- Demographics and Race/Ethnicity (30+ variables)
- Socioeconomic Status (25+ variables)
- Education (20+ variables)
- Housing (25+ variables)
- Transportation (15+ variables)
- Health Behaviors and Outcomes (40+ variables)
- Healthcare Access and Insurance (15+ variables)
- Environmental Factors (25+ variables)
- Traffic Safety (12 variables)
- Food Environment and Access (20+ variables)
- Social Cohesion and Capital (10+ variables)
- Built Environment (20+ variables)

## Documentation

For more detailed information:
- `UNIFIED_PIPELINE_GUIDE.md` - Comprehensive guide
- `HOW_TO_RUN_PIPELINE.md` - Detailed instructions
- `TRAFFIC_SAFETY_IMPLEMENTATION_SUMMARY.md` - Traffic safety details
- `COMPLETE_IMPLEMENTATION_SUMMARY.md` - Implementation overview