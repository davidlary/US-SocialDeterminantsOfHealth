# Social Determinants of Health Dataset

A comprehensive county-level dataset for analyzing social determinants of health across the United States.

## Overview

This dataset combines county-level data on social determinants of health from multiple authoritative sources:

- **U.S. Census Bureau** (Decennial Census, American Community Survey, Population Estimates Program)
- **CDC PLACES** (county-level health indicators)
- **IPUMS NHGIS** (harmonized time series data)
- **FBI Uniform Crime Reports** (crime and safety metrics)
- **USDA Food Environment Atlas** (food access measures)
- **EPA** (environmental quality measures)
- **HUD** (housing statistics)
- **HRSA** (healthcare access measures)
- **And many other specialized data sources**

The data has been processed to provide consistent variable names across sources and years, with interpolation for missing years where appropriate and comprehensive data quality tracking.

## Documentation

All documentation is consolidated in the `docs` directory:

- [Data Dictionary](docs/DATA_DICTIONARY.md) - Comprehensive documentation of all variables by domain
- [Technical Documentation](docs/TECHNICAL_DOCUMENTATION.md) - Detailed information about the data pipeline and scripts
- [Data Sources](docs/data_sources/) - Specific documentation for each data domain

## Data Structure

The database contains organized tables with standardized variables across multiple domains:

- Demographics and Population
- Economic Factors
- Education
- Health Status
- Healthcare Access 
- Housing
- Environmental Factors
- Food Environment
- Transportation
- Social Cohesion
- Crime and Safety
- Built Environment

## Getting Started

### Installation

1. Clone this repository:
```
git clone https://github.com/davidlary/US-SocialDeterminantsOfHealth.git
cd US-SocialDeterminantsOfHealth
```

2. Install required R packages:
```
cd US-SocialDeterminantsOfHealth
Rscript R/install_packages.r
```

3. Run the data pipeline:
```
cd US-SocialDeterminantsOfHealth/R
Rscript unified_sdoh_pipeline.r
```

### Command Line Options

- `--force-update` or `-f`: Force refresh of all cached data
- `--verbose` or `-v`: Show detailed processing information
- `--skip-interpolation`: Disable interpolation for missing data points
- `--allow-simulation`: Allow simulated data where real data is unavailable
- `--offline-mode` or `--offline`: Run in offline mode using only cached data

## Using the Dataset

The pipeline creates a DuckDB database in `output/us_county_sdoh_unified.duckdb`. You can connect to it using:

```r
library(DBI)
library(duckdb)

# Connect to the database
con <- dbConnect(duckdb::duckdb(), 'US-SocialDeterminantsOfHealth/R/output/us_county_sdoh_unified.duckdb')

# Get the latest data for all counties
latest_data <- dbGetQuery(con, "SELECT * FROM latest_county_data")

# Get time series data for a specific county 
la_county <- dbGetQuery(con, "
  SELECT * FROM county_time_series 
  WHERE geoid = '06037' -- Los Angeles County
  ORDER BY variable_name, year
")

# Close the connection
dbDisconnect(con)
```

## Data Quality Indicators

Every record in the dataset includes comprehensive data quality indicators:

- **Data Source**: Original source of the data (Census, CDC, etc.)
- **Data Vintage**: Year and specific collection the data came from
- **Data Quality**: One of: 'direct', 'interpolated', 'extrapolated', 'simulated', or 'imputed'

This allows for full transparency and filtering based on your quality requirements.

## Citation

If you use this dataset in your research or applications, please cite it as:

```
Unified Social Determinants of Health County-Level Dataset (2025). 
Generated using data from U.S. Census Bureau, CDC PLACES, IPUMS NHGIS, 
and other authoritative sources for comprehensive social determinants of health analyses.
```

## Contact

For questions or issues related to this dataset, please contact David Lary (davidlary@me.com).