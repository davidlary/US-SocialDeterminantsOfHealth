# US Social Determinants of Health Dataset

A comprehensive county-level dataset for analyzing social determinants of health across the United States, spanning from 1970 to present.

## Overview

This project provides a unified pipeline to collect, process, and analyze social determinants of health data at the county level for the entire United States. The dataset combines data from multiple authoritative sources, standardizes variable names, handles missing values through interpolation where appropriate, and provides a consistent interface for data access and visualization.

### Key Features

- **Comprehensive Data**: 255 variables across 15 domains covering all aspects of social determinants of health
- **Consistent Interface**: Standardized variable names across sources and years
- **Temporal Coverage**: Data from 1970 to present with interpolation for missing years
- **Modular Architecture**: Extensible pipeline design for easy maintenance and updates
- **Data Quality Tracking**: Comprehensive metadata on data sources and quality
- **Interactive Visualization**: Built-in map generation and data exploration tools
- **Performance Optimized**: Parallel processing and efficient data handling
- **Thorough Documentation**: Detailed variable descriptions and usage guides

## Data Sources

The pipeline aggregates data from 25+ authoritative sources:

- **U.S. Census Bureau**: Decennial Census, American Community Survey (ACS), Population Estimates Program (PEP)
- **CDC**: PLACES, WONDER databases (mortality data)
- **IPUMS NHGIS**: Harmonized historical census data
- **IHME**: County-level life expectancy estimates by race/ethnicity and gender
- **NHTSA**: Fatality Analysis Reporting System (FARS) for traffic safety
- **EPA**: Environmental quality measures (air quality, toxic releases)
- **USDA**: Food Environment Atlas, economic typology
- **FBI**: Uniform Crime Reports
- **HUD**: Housing statistics (CHAS, Fair Market Rent)
- **HRSA**: Area Health Resources Files for healthcare access
- **BLS**: Unemployment and labor data
- **Trust for Public Land**: ParkScore data
- **Many other specialized sources**

## Quick Start with Sample Data

For new users who want to quickly test the pipeline without setting up API credentials or downloading large datasets:

1. **Generate Sample Data**:
   ```bash
   Rscript R/utilities/create_sample_data.r
   ```

2. **Install Essential Packages** (automatically handles missing packages):
   ```bash
   Rscript R/install_missing_packages.r
   ```

3. **Run the Pipeline with Sample Data**:
   ```bash
   Rscript R/unified_sdoh_pipeline.r --use-sample-data
   ```

This will create a minimal working dataset that demonstrates the pipeline's functionality without requiring external data sources or API keys.

## System Requirements

### Hardware
- **CPU**: 4+ cores recommended for parallel processing
- **RAM**: Minimum 8GB, 16GB+ recommended for full dataset processing
- **Storage**: 10GB+ free space (additional space for caching large datasets)

### Software
- **R**: Version 4.0.0 or newer
- **Operating Systems**:
  - Linux (Ubuntu 18.04+, CentOS 7+)
  - macOS (10.15 Catalina or newer)
  - Windows 10/11 (WSL2 recommended for best performance)

### System Dependencies
For spatial features:
- **Linux**: `sudo apt-get install libudunits2-dev libgdal-dev libgeos-dev libproj-dev`
- **macOS**: `brew install udunits gdal geos proj`
- **Windows**: Install Rtools and ensure PATH is set correctly

## Complete Installation

1. **Clone the Repository**:
   ```bash
   git clone https://github.com/davidlary/US-SocialDeterminantsOfHealth.git
   cd US-SocialDeterminantsOfHealth
   ```

2. **Install R Packages**:
   
   **Option 1**: Install all packages (recommended for full functionality):
   ```bash
   Rscript R/install_packages.r
   ```
   
   **Option 2**: Install only essential packages:
   ```bash
   Rscript R/install_missing_packages.r
   ```

   > **Note for Apple Silicon**: Some packages may require special installation. If you encounter errors:
   > ```R
   > options(repos = c(CRAN = "https://cloud.r-project.org"))
   > install.packages("package_name", type = "binary")
   > ```

3. **Set Up API Credentials** (required for full access):
   ```bash
   # Census Bureau API key
   Rscript R/utilities/set_api_key.r YOUR_CENSUS_API_KEY

   # IPUMS/NHGIS credentials
   Rscript R/utilities/set_ipums_credentials.r YOUR_USERNAME YOUR_PASSWORD
   ```

   - Get Census API key: [api.census.gov/data/key_signup.html](https://api.census.gov/data/key_signup.html)
   - Get IPUMS account: [usa.ipums.org/usa/](https://usa.ipums.org/usa/)

4. **Configure Settings** (optional):
   ```bash
   # Edit configuration file
   vi R/config.yaml
   ```

5. **Run the Full Pipeline**:
   ```bash
   # Default configuration
   Rscript R/unified_sdoh_pipeline.r

   # Custom configuration file
   Rscript R/unified_sdoh_pipeline.r /path/to/custom_config.yaml
   ```

## Command Line Options

- `--years=1970:2023`: Specify year range (default: most recent 10 years)
- `--force-update` or `-f`: Force refresh of all cached data
- `--verbose` or `-v`: Show detailed processing information
- `--skip-interpolation`: Disable interpolation for missing data points
- `--force-real-data=TRUE`: Ensure only real data is used (no simulations)
- `--offline-mode=TRUE`: Run in offline mode using only cached data
- `--output-format=csv,duckdb,sqlite`: Specify output format(s)
- `--modules=traffic_safety,climate,housing`: Run only specific modules
- `--use-sample-data`: Use generated sample data for testing
- `--incremental=TRUE|FALSE`: Enable/disable incremental processing (only update new/changed data)
- `--force-full-rebuild=TRUE`: Force a full database rebuild even when in incremental mode

## YAML Configuration

The pipeline uses YAML configuration to separate code from data storage locations, particularly useful when using external drives or network storage for large datasets.

### Basic Example
```yaml
# SDOH Pipeline Configuration
directories:
  data_dir: "data"
  output_dir: "output"

database:
  db_path: "output/us_county_sdoh_unified.duckdb"
  
years:
  min_year: 1990
  max_year: 2025
```

### Network Drive Example
```yaml
# Using a network drive for data storage
directories:
  # Code files location (must point to where the R scripts are located)
  root_dir: "/Users/username/Projects/SDOH/R"

# Data storage on network drive
network_paths:
  data_dir: "/Volumes/NetworkDrive/SDOH/data"
  output_dir: "/Volumes/NetworkDrive/SDOH/output"
  logs_dir: "/Volumes/NetworkDrive/SDOH/logs"
  
database:
  db_path: "/Volumes/NetworkDrive/SDOH/output/sdoh_database.duckdb"
```

For complete configuration options, see the [Configuration Guide](docs/CONFIG_GUIDE.md).

## Modular Pipeline Architecture

The pipeline uses a modular architecture to improve maintainability and extensibility:

### Core Modules

1. **Core Module** (`module_core.r`):
   - Handles initialization, logging, and utilities
   - Manages YAML configuration and parallel processing setup
   - Provides core functionality used by all other modules

2. **Crosswalk Module** (`module_crosswalk.r`):
   - Builds and validates the unified variable crosswalk
   - Ensures all variables are properly defined and categorized
   - Updates documentation with accurate variable counts

3. **Data Fetching Module** (`module_data_fetching.r`):
   - Retrieves data from multiple sources
   - Implements caching and fallback mechanisms
   - Handles data quality tracking and source attribution

4. **Database Module** (`module_database.r`):
   - Creates and manages the DuckDB database
   - Implements the normalized schema design
   - Creates views for easy data access

5. **Maps Module** (`module_maps.r`):
   - Generates county-level choropleth maps
   - Creates visualizations by variable, year, and domain
   - Supports both CONUS and state-level maps

6. **Documentation Module** (`module_documentation.r`):
   - Generates comprehensive documentation
   - Maintains data dictionaries and README files
   - Ensures consistency across all documentation

For details on the modular architecture, see the [Modular Pipeline Guide](docs/MODULAR_PIPELINE.md).

## Performance Optimizations

The pipeline includes several key performance optimizations:

1. **Parallel Processing**:
   - Automatic processor core detection and allocation
   - Adaptive strategy selection (multicore/multisession) based on OS
   - Chunked processing for large datasets
   - Progress tracking with the progressr package

2. **Memory Management**:
   - Dataset size estimation to choose optimal processing approach
   - Conservative memory limits to prevent out-of-memory errors
   - Intelligent garbage collection
   - Efficient data structures

3. **Database Optimizations**:
   - Sophisticated indexing strategies based on common query patterns
   - Materialized views for frequently accessed data
   - Memory-mapped I/O for large datasets
   - Resource-aware configuration that adapts to available hardware
   - Advanced caching strategies for query acceleration

4. **IHME Data Processing**:
   - Optimized for 46+ million rows of life expectancy data
   - Memory-efficient key processing with context-aware batch sizing
   - Parallel batch processing with fallback mechanisms
   - Chunked processing for race/ethnicity data

5. **Incremental Processing**:
   - Smart detection of previously processed data
   - Efficient updates that only process new or changed data
   - Metadata tracking to avoid redundant processing
   - Transaction-based updates for data consistency

6. **Caching**:
   - Comprehensive caching of all data sources
   - Multiple fallback mechanisms for offline operation
   - Selective refresh of outdated data

For IHME-specific optimizations, see the [IHME Data Processing Guide](docs/IHME_DATA_PROCESSING.md).
For database optimization details, see the [Database Optimizations Guide](docs/DATABASE_OPTIMIZATIONS.md).

## Offline Mode and Data Caching

The pipeline supports comprehensive offline operation:

### Pre-download All Data
```bash
# Cache all sources
Rscript R/cache_sdoh_data.r

# Run in offline mode
Rscript R/unified_sdoh_pipeline.r --offline-mode
```

### Selective Caching
```bash
# Cache specific data sources
Rscript R/cache_federal_data.r --sources=traffic_safety,census
```

### Caching Features
- Multiple fallback mechanisms for each data source
- Pre-downloaded data as fallback when APIs fail
- Sample data generation as last resort (when enabled)
- Comprehensive coverage across all data domains

## Data Dictionary

The dataset includes 255 variables across 15 domains. Here's a summary of the main variable categories:

### Demographics and Population
- Total population, age distribution, gender breakdown
- Race/ethnicity percentages
- Population density, urban/rural status
- Migration and natural change rates

### Economic Factors
- Income measures (median household income, earnings)
- Poverty rates and income inequality
- Employment statistics
- Economic mobility and opportunity measures

### Education
- Educational attainment levels
- School quality metrics
- Educational opportunity and achievement gaps
- School funding and resources

### Health Status
- Life expectancy by race/ethnicity and gender
- Disease prevalence (diabetes, heart disease, etc.)
- Mental health indicators
- Mortality rates and causes

### Healthcare Access
- Insurance coverage
- Provider availability
- Healthcare infrastructure
- Preventative care metrics

### Housing
- Housing costs and affordability
- Homeownership rates
- Housing quality
- Homelessness and housing instability

### Environmental Factors
- Air and water quality
- Toxic exposures
- Climate indicators
- Built environment measures

### Food Environment
- Food access measures
- Food insecurity rates
- Grocery store and food retailer availability
- Nutrition assistance program participation

### Transportation
- Commuting patterns
- Vehicle access
- Public transit availability
- Transportation costs

### Traffic Safety
- Traffic fatalities and injuries
- DUI-related incidents
- Pedestrian and cyclist safety
- Speeding and risky driving measures

### Social Cohesion
- Civic participation
- Social capital measures
- Family structure
- Community organizations

### Crime and Safety
- Violent and property crime rates
- Incarceration statistics
- Community violence exposure
- Juvenile justice measures

### Built Environment
- Land use diversity
- Walkability measures
- Recreation access
- Housing density

### Digital Access
- Internet and computer access
- Broadband availability
- Digital literacy measures
- Technology equity indicators

### Climate and Weather
- Temperature patterns
- Precipitation
- Extreme weather events
- Climate vulnerability indices

For the complete data dictionary with all 255 variables, see the [Data Dictionary](docs/DATA_DICTIONARY.md).

## Data Quality and Provenance

Each data point in the dataset includes comprehensive quality metadata:

- **Source**: Original data source (Census, CDC, NHTSA, etc.)
- **Vintage**: Year and specific collection the data came from
- **Quality Flag**: 
  - 'direct' - Directly from authoritative source
  - 'interpolated' - Estimated between known data points
  - 'extrapolated' - Projected beyond available data
  - 'calculated' - Derived from other values
  - 'imputed' - Statistically estimated for missing data
  - 'forecast' - Predicted using time series models

The pipeline strictly prioritizes real data over simulated data, with clear documentation when any form of estimation is used.

## Using the Database

The pipeline creates a DuckDB database containing all processed data:

```r
library(DBI)
library(duckdb)

# Connect to the database
con <- dbConnect(duckdb::duckdb(), 'output/us_county_sdoh_unified.duckdb')

# Get the latest data for all counties
latest_data <- dbGetQuery(con, "
  SELECT GEOID, county_name, 
         median_household_income, life_expectancy,
         traffic_fatality_rate_per_100k, air_pollution_pm25
  FROM latest_county_data
")

# Get time series data for a specific county
la_data <- dbGetQuery(con, "
  SELECT year, traffic_fatality_count, traffic_fatality_rate_per_100k,
         life_expectancy, poverty_rate
  FROM county_time_series 
  WHERE GEOID = '06037' -- Los Angeles County
  ORDER BY year
")

# Close the connection
dbDisconnect(con)
```

## Troubleshooting

### Package Installation Issues
- For sf/rgdal/rgeos: Ensure system dependencies are installed
- For rJava-based packages: Verify Java is installed and R can find it
- For prophet: May require Rtools on Windows or compiler tools on Linux/Mac

### Memory Issues
- Adjust R memory limits: `options(future.globals.maxSize = 4 * 1024^3)`
- On Windows: Use `memory.limit(size = 16000)` to increase R's memory allocation
- Use the `--low-memory` flag to enable more conservative memory usage

### Network/API Issues
- Check internet connection and firewall settings
- Verify API credentials are correctly configured
- Use offline mode: `--offline-mode=TRUE`

### File Permission Errors
- Ensure write permissions for the project directory
- For system-wide installation: Use sudo (Linux/Mac) or run as administrator (Windows)

## Citation and License

If you use this dataset in your research or applications, please cite it as:

```
US Social Determinants of Health Dataset (2025). 
Comprehensive county-level data from U.S. Census Bureau, CDC PLACES, 
NHTSA FARS, and other authoritative sources for social determinants 
of health analyses from 1970 to present.
```

The code in this repository is licensed under the MIT License, while the aggregated data is provided under CC BY 4.0. Individual data sources maintain their original licensing terms.

## Contact

For questions or issues related to this dataset, please contact David Lary (davidlary@me.com).

## Future Improvement Suggestions

Based on our analysis of the pipeline, we recommend the following improvements:

### Performance Optimizations
1. ✅ **Database Indexing**: Implemented sophisticated indexing in DuckDB for faster query performance
2. ✅ **Materialized Views**: Added materialized views for common query patterns
3. ✅ **Memory-Mapped Files**: Implemented memory-mapped files for very large datasets
4. ✅ **Data Compression**: Added adaptive compression based on available resources
5. ✅ **Incremental Updates**: Added support for incremental updates rather than full refreshes
6. **Compute Kernel Optimization**: Add GPU acceleration for specific computational tasks
7. **Distributed Computation**: Add support for distributed processing across multiple nodes

### Robustness Improvements
1. **More Comprehensive Unit Testing**: Expand test coverage for all modules
2. **Validation Framework**: Add formal data validation framework with schema checks
3. **API Rate Limiting**: Implement more sophisticated API rate limiting and retry logic
4. **Dependency Injection**: Refactor to use formal dependency injection for easier testing
5. **Circuit Breakers**: Add circuit breakers for external dependencies to prevent cascading failures

### Feature Enhancements
1. **API Layer**: Add a REST API to expose the data
2. **Interactive Dashboard**: Develop a Shiny dashboard for interactive exploration
3. **Machine Learning Integration**: Add direct integration with popular ML frameworks
4. **Geospatial Analysis**: Expand geospatial analysis capabilities
5. **Time Series Forecasting**: Enhance forecasting abilities with more models and validation

### Documentation and Usability
1. **User Guides**: Create role-specific user guides (researcher, data scientist, etc.)
2. **Video Tutorials**: Add video tutorials for common workflows
3. **Example Notebooks**: Provide more example notebooks for different use cases
4. **CLI Improvements**: Enhance the command-line interface with more options
5. **Docker Container**: Provide a ready-to-use Docker container with all dependencies