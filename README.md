# US Social Determinants of Health Dataset

A comprehensive county-level dataset for analyzing social determinants of health across the United States, spanning from 1970 to present.

## Overview

This dataset combines county-level data on social determinants of health from multiple authoritative sources, processed to provide consistent variable names across sources and years, with interpolation for missing years where appropriate and comprehensive data quality tracking.

### Data Sources

- **U.S. Census Bureau** (Decennial Census, American Community Survey, Population Estimates Program)
- **CDC PLACES** (county-level health indicators)
- **CDC WONDER** (mortality data including transportation-related deaths)
- **IPUMS NHGIS** (harmonized time series data)
- **FBI Uniform Crime Reports** (crime and safety metrics)
- **NHTSA FARS** (Fatality Analysis Reporting System for traffic safety)
- **USDA Food Environment Atlas** (food access measures)
- **EPA** (environmental quality measures)
- **HUD** (housing statistics)
- **HRSA** (healthcare access measures)
- **And many other specialized data sources**

## Data Structure

The database contains organized tables with standardized variables across multiple domains. For a complete listing of all variables and their metadata, see the [Data Dictionary](docs/DATA_DICTIONARY.md).

### Summary of Variables by Domain

| Domain | Number of Variables | Primary Data Sources |
|--------|---------------------|----------------------|
| Demographics & Population | 6 | Census Bureau, IPUMS NHGIS, SEER |
| Economic Factors | 34 | Census ACS, BLS, Opportunity Insights |
| Education | 20 | Census ACS, NCES, Stanford Education Data Archive |
| Health Status | 46 | CDC PLACES, CDC WONDER, IHME |
| Healthcare Access | 16 | HRSA Area Health Resources Files, CMS |
| Housing | 24 | Census ACS, HUD CHAS, Eviction Lab |
| Environmental Health | 18 | EPA Air Quality System, EPA TRI, CDC Environmental Public Health Tracking |
| Food Environment | 15 | USDA Food Environment Atlas, Feeding America |
| Transportation | 17 | Census ACS, National Transit Database |
| Traffic Safety | 12 | NHTSA FARS, CDC WONDER |
| Social Cohesion | 11 | Census ACS, County Health Rankings, MIT Election Data |
| Crime & Safety | 5 | FBI Uniform Crime Reports, Bureau of Justice Statistics |
| Built Environment | 10 | EPA Smart Location Database, Trust for Public Land |
| Digital Access | 6 | FCC, Census ACS |
| Climate & Weather | 7 | NOAA, EPA |
| **Total** | **255** |

The domains include:

- **Demographics and Population**: Population counts, age distribution, race/ethnicity
- **Economic Factors**: Income, poverty, employment, economic mobility
- **Education**: Educational attainment, quality of schools, dropout rates
- **Health Status**: Disease prevalence, mortality, disability status
- **Healthcare Access**: Insurance coverage, provider availability, preventative care
- **Housing**: Housing affordability, homeownership, housing quality
- **Environmental Factors**: Air and water quality, toxic exposure, climate indicators
- **Food Environment**: Food access, food insecurity, nutrition assistance
- **Transportation**: Commuting patterns, vehicle access, public transit
- **Traffic Safety**: Fatalities, injuries, risk factors like DUI and speeding
- **Social Cohesion**: Social capital, civic participation, family structure
- **Crime and Safety**: Crime rates, community violence, safety perceptions
- **Built Environment**: Land use, walkability, recreation access
- **Digital Access**: Internet and computer access, broadband availability
- **Climate & Weather**: Temperature, precipitation, extreme weather events

### Required Data Sources

For real-world analysis, ensure these data files exist:
- NHGIS data files (CSV format) in `/data/nhgis/`
- SEER population data files in `/data/seer/`
- Census Bureau data (via API with proper credentials)
- NHGIS/IPUMS data (via API with proper credentials)

The pipeline will use actual data from these sources when available, falling back to cached data when needed.

## Getting Started

### Installation

#### System Requirements

1. **Operating System**:
   - Linux (Ubuntu 18.04+, CentOS 7+, etc.)
   - macOS (10.15 Catalina or newer)
   - Windows 10/11 with WSL2 recommended for best performance

2. **Hardware Requirements**:
   - CPU: 4+ cores recommended for parallel processing
   - RAM: Minimum 8GB, 16GB+ recommended for full dataset processing
   - Storage: 10GB+ free space (additional space required for caching large datasets)

3. **Software Dependencies**:
   - R version 4.0.0 or newer
   - RStudio (optional but recommended)
   - Git
   - For spatial features:
     - Linux: `sudo apt-get install libudunits2-dev libgdal-dev libgeos-dev libproj-dev`
     - macOS: `brew install udunits gdal geos proj`
     - Windows: Install Rtools and ensure PATH is set correctly

#### Installation Steps

1. **Clone the Repository**:
   ```bash
   git clone https://github.com/davidlary/US-SocialDeterminantsOfHealth.git
   cd US-SocialDeterminantsOfHealth
   ```

2. **Install Required R Packages**:

   The project provides two installation options:
   
   **Option 1**: Install all packages (recommended for full functionality):
   ```bash
   Rscript R/install_packages.r
   ```
   
   This will install all necessary packages including:
   - Core packages: tidyverse, DBI, duckdb, data.table, zoo, sf, tigris, etc.
   - Visualization: ggplot2, viridis, leaflet, plotly, etc.
   - Data processing: imputeTS, forecast, furrr, future, etc.
   - Machine learning: prophet, xgboost, tidymodels, etc.
   - API: plumber, swagger

   **Option 2**: Install only essential packages:
   ```bash
   Rscript R/install_missing_packages.r
   ```
   
   This installs only the minimum required packages for basic functionality.

   > **Note for Apple Silicon (M1/M2) Users**: Some packages may require special installation. If you encounter errors, try using:
   > ```R
   > options(repos = c(CRAN = "https://cloud.r-project.org"))
   > install.packages("package_name", type = "binary")
   > ```

3. **Set Up API Credentials** (required for full access to data sources):
   ```bash
   # For Census Bureau data
   Rscript R/utilities/set_api_key.r YOUR_CENSUS_API_KEY

   # For IPUMS/NHGIS access
   Rscript R/utilities/set_ipums_credentials.r YOUR_USERNAME YOUR_PASSWORD
   ```

   To obtain these credentials:
   - Census API key: Register at https://api.census.gov/data/key_signup.html
   - IPUMS/NHGIS: Create an account at https://usa.ipums.org/usa/

4. **Clear Existing Database** (if updating from previous installation):
   ```bash
   rm -f data/sdoh_county.duckdb*
   ```

5. **Configure Settings** (optional):
   ```bash
   # Edit the configuration file to customize paths and settings
   vi R/config.yaml
   ```

6. **Run the Pipeline**:
   ```bash
   # Run with default configuration
   Rscript R/unified_sdoh_pipeline.r

   # OR specify a custom configuration file
   Rscript R/unified_sdoh_pipeline.r /path/to/custom_config.yaml
   ```

#### Troubleshooting Common Installation Issues

1. **Package Installation Failures**:
   - For sf/rgdal/rgeos: Ensure system dependencies are installed (see above)
   - For rJava-based packages: Verify Java is installed and R can find it
   - For prophet: May require Rtools on Windows or compiler tools on Linux/Mac

2. **Memory Issues During Processing**:
   - Adjust R memory limits: Add `options(future.globals.maxSize = 4 * 1024^3)` to the start of scripts
   - On Windows: Use `memory.limit(size = 16000)` to increase R's memory allocation

3. **File Permission Errors**:
   - Ensure write permissions for the project directory
   - For system-wide installation: Use sudo (Linux/Mac) or run as administrator (Windows)

4. **Network/API Issues**:
   - Check internet connection and firewall settings
   - Verify API credentials are correctly configured
   - Use the offline mode if APIs are unavailable: `--offline-mode=TRUE`

### Command Line Options

- `--years=1970:2023`: Specify year range (default: most recent 10 years)
- `--force-update` or `-f`: Force refresh of all cached data
- `--verbose` or `-v`: Show detailed processing information
- `--skip-interpolation`: Disable interpolation for missing data points
- `--force-real-data=TRUE`: Ensure only real data is used (no simulations)
- `--offline-mode=TRUE`: Run in offline mode using only cached data
- `--output-format=csv,duckdb,sqlite`: Specify output format(s)
- `--modules=traffic_safety,climate,housing`: Run only specific modules

### Configuration with YAML

The pipeline now supports YAML configuration to separate code from data storage locations. This is especially useful when using external drives or network storage for large datasets.

#### Basic Configuration Example

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

#### Network Drive Configuration Example

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

For complete details on all configuration options, see the [Configuration Guide](docs/CONFIG_GUIDE.md).

## Offline Mode and Data Caching

The pipeline supports comprehensive offline operation using the data caching system:

### Caching All Data (Recommended)

To predownload all necessary data and create fallbacks:

```bash
# Cache all sources
Rscript R/cache_sdoh_data.r

# Run the pipeline in offline mode
Rscript R/unified_sdoh_pipeline.r --offline-mode
```

### Selective Caching

To cache only specific data sources:

```bash
# Cache just traffic safety and census data
Rscript R/cache_federal_data.r --sources=traffic_safety,census
```

### Caching Features

- **Multiple Fallbacks**: Each data source has multiple fallback methods
- **Pre-downloaded Data**: Uses locally stored files when APIs fail
- **Sample Data Generation**: Creates realistic sample data as a last resort
- **Comprehensive Coverage**: Covers all data domains in the pipeline

## Data Dictionary

### Demographics
| Variable | Description | Unit | Source | Years |
|----------|-------------|------|--------|-------|
| total_population | Total population | Count | Census | 1970-present |
| median_age | Median age | Years | Census | 1970-present |
| male_population | Male population | Count | Census | 1970-present |
| female_population | Female population | Count | Census | 1970-present |
| population_under_18 | Population under 18 | Count | Census | 1970-present |
| population_65_over | Population 65 and over | Count | Census | 1970-present |
| white_nonhispanic_pct | White, not Hispanic | Percentage | Census | 1970-present |
| black_pct | Black/African American | Percentage | Census | 1970-present |
| hispanic_latino_pct | Hispanic/Latino | Percentage | Census | 1970-present |
| asian_pct | Asian | Percentage | Census | 1970-present |
| native_american_pct | American Indian/Alaska Native | Percentage | Census | 1970-present |
| population_density | Population per square mile | Density | Census | 1970-present |

### Economic Factors
| Variable | Description | Unit | Source | Years |
|----------|-------------|------|--------|-------|
| median_household_income | Median household income | Dollars | Census ACS | 1970-present |
| poverty_rate | Population below poverty line | Percentage | Census ACS | 1970-present |
| gini_index | Income inequality (Gini Index) | Index (0-1) | Census ACS | 1990-present |
| snap_benefits_pct | Households receiving SNAP | Percentage | Census ACS | 1990-present |
| unemployment_rate | Unemployment rate | Percentage | BLS | 1990-present |
| labor_force_participation | Labor force participation rate | Percentage | Census ACS | 1990-present |
| median_earnings | Median earnings for workers | Dollars | Census ACS | 1990-present |

### Education
| Variable | Description | Unit | Source | Years |
|----------|-------------|------|--------|-------|
| less_than_highschool_pct | Less than high school education | Percentage | Census ACS | 1970-present |
| highschool_only_pct | High school degree only | Percentage | Census ACS | 1970-present |
| some_college_pct | Some college or associate's | Percentage | Census ACS | 1970-present |
| bachelors_or_higher_pct | Bachelor's degree or higher | Percentage | Census ACS | 1970-present |

### Housing
| Variable | Description | Unit | Source | Years |
|----------|-------------|------|--------|-------|
| median_home_value | Median home value | Dollars | Census ACS | 1970-present |
| median_gross_rent | Median gross rent | Dollars | Census ACS | 1970-present |
| homeownership_rate | Homeownership rate | Percentage | Census ACS | 1970-present |
| vacant_housing_rate | Vacant housing rate | Percentage | Census ACS | 1970-present |
| severe_housing_cost_burden | Severe housing cost burden | Percentage | HUD | 1990-present |
| overcrowded_housing_pct | >1 person per room | Percentage | Census ACS | 1990-present |
| housing_no_kitchen_pct | Lacking kitchen facilities | Percentage | Census ACS | 1990-present |
| housing_no_plumbing_pct | Lacking plumbing facilities | Percentage | Census ACS | 1990-present |

### Transportation
| Variable | Description | Unit | Source | Years |
|----------|-------------|------|--------|-------|
| mean_commute_time | Mean travel time to work | Minutes | Census ACS | 1990-present |
| commute_public_transit_pct | Public transit commuters | Percentage | Census ACS | 1990-present |
| no_vehicle_households_pct | Households with no vehicle | Percentage | Census ACS | 1990-present |
| commute_carpool_pct | Carpool commuters | Percentage | Census ACS | 1990-present |
| commute_walking_pct | Walking commuters | Percentage | Census ACS | 1990-present |
| commute_long_pct | Commute ≥60 minutes | Percentage | Census ACS | 1990-present |

### Traffic Safety
| Variable | Description | Unit | Source | Years |
|----------|-------------|------|--------|-------|
| traffic_fatalities | Traffic fatalities | Count | NHTSA FARS | 1975-present |
| traffic_fatality_rate | Traffic fatality rate | Rate per 100k | NHTSA FARS | 1975-present |
| pedestrian_fatalities | Pedestrian fatalities | Count | NHTSA FARS | 1975-present |
| pedestrian_fatality_rate | Pedestrian fatality rate | Rate per 100k | NHTSA FARS | 1975-present |
| bicycle_fatalities | Bicycle fatalities | Count | NHTSA FARS | 1975-present |
| bicycle_fatality_rate | Bicycle fatality rate | Rate per 100k | NHTSA FARS | 1975-present |
| motorcycle_fatalities | Motorcycle fatalities | Count | NHTSA FARS | 1975-present |
| motorcycle_fatality_rate | Motorcycle fatality rate | Rate per 100k | NHTSA FARS | 1975-present |
| alcohol_impaired_fatalities | Alcohol-impaired fatalities | Count | NHTSA FARS | 1975-present |
| alcohol_impaired_fatality_rate | Alcohol-impaired fatality rate | Rate per 100k | NHTSA FARS | 1975-present |
| speeding_related_fatalities | Speeding-related fatalities | Count | NHTSA FARS | 1975-present |
| speeding_related_fatality_rate | Speeding-related fatality rate | Rate per 100k | NHTSA FARS | 1975-present |

> Note: All traffic safety data comes from the actual NHTSA Fatality Analysis Reporting System (FARS) dataset. The pipeline processes real data files and does not use simulated data.

### Health Insurance
| Variable | Description | Unit | Source | Years |
|----------|-------------|------|--------|-------|
| uninsured_pct | Without health insurance | Percentage | Census ACS | 1990-present |
| private_health_insurance_pct | Private health insurance | Percentage | Census ACS | 1990-present |
| public_health_insurance_pct | Public health insurance | Percentage | Census ACS | 1990-present |
| medicaid_pct | Medicaid coverage | Percentage | Census ACS | 1990-present |
| medicare_pct | Medicare coverage | Percentage | Census ACS | 1990-present |

### Health Status
| Variable | Description | Unit | Source | Years |
|----------|-------------|------|--------|-------|
| life_expectancy | Life expectancy at birth | Years | IHME | 2000-2019 |
| life_expectancy_male | Male life expectancy at birth | Years | IHME | 2000-2019 |
| life_expectancy_female | Female life expectancy at birth | Years | IHME | 2000-2019 |
| life_expectancy_hispanic | Hispanic life expectancy at birth | Years | IHME | 2000-2019 |
| life_expectancy_nhw | Non-Hispanic White life expectancy | Years | IHME | 2000-2019 |
| life_expectancy_nhb | Non-Hispanic Black life expectancy | Years | IHME | 2000-2019 |
| life_expectancy_nhaian | Non-Hispanic AIAN life expectancy | Years | IHME | 2000-2019 |
| life_expectancy_nhasian | Non-Hispanic Asian life expectancy | Years | IHME | 2000-2019 |
| poor_physical_health_pct | Poor physical health | Percentage | CDC PLACES | 2016-present |
| poor_mental_health_pct | Poor mental health | Percentage | CDC PLACES | 2016-present |
| depression_pct | Depression | Percentage | CDC PLACES | 2016-present |
| obesity_pct | Obesity | Percentage | CDC PLACES | 2016-present |
| diabetes_pct | Diabetes | Percentage | CDC PLACES | 2016-present |
| high_blood_pressure_pct | High blood pressure | Percentage | CDC PLACES | 2016-present |
| high_cholesterol_pct | High cholesterol | Percentage | CDC PLACES | 2016-present |
| asthma_pct | Asthma | Percentage | CDC PLACES | 2016-present |
| arthritis_pct | Arthritis | Percentage | CDC PLACES | 2016-present |
| cancer_pct | Cancer history | Percentage | CDC PLACES | 2016-present |
| copd_pct | COPD | Percentage | CDC PLACES | 2016-present |
| kidney_disease_pct | Kidney disease | Percentage | CDC PLACES | 2016-present |
| coronary_heart_disease_pct | Coronary heart disease | Percentage | CDC PLACES | 2016-present |
| stroke_pct | Stroke history | Percentage | CDC PLACES | 2016-present |

> Note: The IHME life expectancy variables use real data from the Institute for Health Metrics and Evaluation's county-level life expectancy datasets. The full dataset includes 29 life expectancy variables with breakdowns by gender, race/ethnicity, and confidence intervals.

### Health Behaviors
| Variable | Description | Unit | Source | Years |
|----------|-------------|------|--------|-------|
| smoking_pct | Current smokers | Percentage | CDC PLACES | 2016-present |
| binge_drinking_pct | Binge drinking | Percentage | CDC PLACES | 2016-present |
| physical_inactivity_pct | Physical inactivity | Percentage | CDC PLACES | 2016-present |
| insufficient_sleep_pct | Insufficient sleep | Percentage | CDC PLACES | 2016-present |

### Environmental Factors
| Variable | Description | Unit | Source | Years |
|----------|-------------|------|--------|-------|
| air_pollution_pm25 | PM2.5 concentration | µg/m³ | EPA | 1990-present |
| severe_housing_problems | Severe housing problems | Percentage | HUD | 1990-present |

### Social Factors
| Variable | Description | Unit | Source | Years |
|----------|-------------|------|--------|-------|
| single_parent_households_pct | Single-parent households | Percentage | Census ACS | 1990-present |
| limited_english_pct | Limited English proficiency | Percentage | Census ACS | 1990-present |
| broadband_access_pct | Broadband internet access | Percentage | Census ACS | 2013-present |
| grandparents_caregivers_pct | Grandparents as caregivers | Percentage | Census ACS | 1990-present |
| internet_access_pct | Internet access | Percentage | Census ACS | 2013-present |
| computer_access_pct | Computer access | Percentage | Census ACS | 2013-present |
| non_english_home_pct | Non-English at home | Percentage | Census ACS | 1990-present |

### Food Environment
| Variable | Description | Unit | Source | Years |
|----------|-------------|------|--------|-------|
| food_insecurity_pct | Food insecurity | Percentage | USDA | 2000-present |

### Disability
| Variable | Description | Unit | Source | Years |
|----------|-------------|------|--------|-------|
| disability_pct | Any disability | Percentage | Census ACS | 1990-present |
| disability_under_18_pct | Disability under 18 | Percentage | Census ACS | 1990-present |
| disability_18_64_pct | Disability 18-64 | Percentage | Census ACS | 1990-present |
| disability_65_over_pct | Disability 65+ | Percentage | Census ACS | 1990-present |
| cognitive_disability_pct | Cognitive disability | Percentage | Census ACS | 1990-present |
| ambulatory_disability_pct | Ambulatory disability | Percentage | Census ACS | 1990-present |
| independent_living_disability_pct | Independent living disability | Percentage | Census ACS | 1990-present |

## Recent Updates

### YAML Configuration System (April 2025)

A new YAML-based configuration system has been added to the SDOH pipeline, allowing for flexible separation of code from data storage. This is particularly useful for working with large datasets on network drives or external storage.

#### Key Features

1. **Flexible Path Configuration**:
   - Specify custom paths for data directory, output directory, and database
   - Support for absolute and relative paths
   - Special handling for network paths and external drives

2. **Complete Configuration**:
   - Database settings (path, overwrite options)
   - Data refresh options (cache usage, maximum data age)
   - Processing options (parallel execution, core count)
   - Map generation settings
   - Year range for data processing
   - Documentation generation options
   - API credentials configuration
   - Traffic safety data options

3. **Multiple Configuration Methods**:
   - Default YAML file (`config.yaml` in project root)
   - Custom configuration file via command line
   - Environment variables override YAML settings
   - Programmatic access via the `load_config()` function

#### Benefits of YAML Configuration

- **Separation of Concerns**: Code and data storage can be managed independently
- **Enhanced Portability**: Easy to move between different environments
- **Improved Collaboration**: Different users can use their own configuration
- **Network Storage Support**: Use large network drives without modifying code
- **Configuration Versioning**: Track configuration changes in version control

For detailed instructions on using the YAML configuration system, see the [Configuration Guide](docs/CONFIG_GUIDE.md).

### Modular Pipeline Architecture (April 2025)

The SDOH pipeline has been refactored into a modular architecture to improve maintainability, readability, and extensibility. This new architecture breaks down the monolithic pipeline into focused, independent components:

#### Key Components

1. **Core Module** (`module_core.r`):
   - Handles initialization, logging, and utilities
   - Manages YAML configuration and parallel processing setup
   - Provides core functionality used by all other modules

2. **Crosswalk Module** (`module_crosswalk.r`):
   - Builds and validates the unified variable crosswalk
   - Ensures all 255 variables are properly defined and categorized
   - Updates documentation with accurate variable counts

3. **Data Fetching Module** (`module_data_fetching.r`):
   - Retrieves data from Census, NHGIS, CDC, and other sources
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

#### Benefits of the Modular Architecture

- **Improved Maintainability**: Each module can be updated independently
- **Easier Debugging**: Issues are isolated to specific modules
- **Better Organization**: Clear separation of concerns
- **Enhanced Extensibility**: New features can be added as new modules
- **Simplified Testing**: Modules can be tested in isolation

For detailed instructions on using and extending the modular pipeline, see the [Modular Pipeline Guide](docs/MODULAR_PIPELINE.md).

### Temporal Interpolation for Missing Data (April 2025)

The pipeline now features a sophisticated temporal interpolation system to handle missing county-year combinations in the data. This system:

1. **Identifies Missing Data**: Detects gaps in the time series for each county and variable
2. **Finds Bracketing Years**: Identifies available data points before and after each gap
3. **Interpolates Values**: Uses statistical methods to estimate values for missing years
4. **Tracks Data Quality**: Flags interpolated values with appropriate quality indicators

#### Key Features

1. **Multiple Interpolation Methods**:
   - Linear interpolation (default) - Straight-line estimation between known points
   - Spline interpolation - Smooth curves using cubic splines (requires 4+ data points)
   - Stineman interpolation - Preserves monotonicity and local extrema

2. **Configurable Parameters**:
   - Minimum gap size - Only interpolate gaps of a certain size
   - Maximum gap size - Avoid interpolating across very large gaps
   - Method selection - Choose the appropriate interpolation algorithm

3. **Comprehensive Quality Tracking**:
   - All interpolated values are flagged with "interpolated" quality indicator
   - Original values maintain their "direct" quality indicator
   - Visualizations distinguish between direct and interpolated data points

4. **Validation Tools**:
   - Dedicated testing script to validate interpolation accuracy
   - Visual comparison of original vs. interpolated values
   - Error metrics (MAE, RMSE) to assess interpolation quality

#### Usage Examples

```r
# Run the pipeline with default interpolation
Rscript R/unified_sdoh_pipeline.r

# Run with specific interpolation settings
Rscript R/unified_sdoh_pipeline.r --interpolation-method=spline --max-gap-size=3

# Disable interpolation completely
Rscript R/unified_sdoh_pipeline.r --skip-interpolation
```

For detailed information about the temporal interpolation system, see the [Temporal Interpolation Guide](docs/TEMPORAL_INTERPOLATION.md).

### Traffic Safety Module (April 2025)

The traffic safety module is a comprehensive component that fetches and analyzes traffic safety data at the county level across the United States. It provides detailed information about traffic fatalities, injuries, and related risk factors from 1970 to the present.

#### Key Features

1. **Data Sources Integration**:
   - NHTSA Fatality Analysis Reporting System (FARS) - county-level traffic fatality data
   - CDC WONDER - transportation mortality data
   - Census population data - for calculating rates per population

2. **Comprehensive Metrics**:
   - Traffic fatality counts and rates
   - Traffic injury counts and rates
   - Pedestrian and cyclist fatality counts and rates
   - DUI-related fatality counts and rates
   - Speeding-related fatality counts and rates

3. **Advanced Analytics**:
   - Time series forecasting with multiple models (ARIMA, ETS, ensemble)
   - Spatial analysis including hotspot detection
   - Local and global spatial autocorrelation (Moran's I, Getis-Ord G*)
   - Persistent problem area identification across years

4. **Data Quality Tracking**:
   - Source attribution for every data point
   - Quality flags: direct, interpolated, extrapolated, calculated
   - Gap filling with appropriate statistical methods
   - Comprehensive validation checks

5. **Visualization Capabilities**:
   - County-level choropleth maps
   - Hotspot maps showing spatial clusters
   - Time series visualizations
   - Interactive and static outputs

#### Integration with Pipeline

The traffic safety module is fully integrated with the unified SDOH pipeline, with these components:

1. **Main Data Fetcher** (`fetch_traffic_safety_data.r`):
   - Primary function to retrieve traffic safety data from multiple sources
   - Handles caching, quality control, and data fusion

2. **Enhancement Modules**:
   - `traffic_safety_cache.r` - Optimized caching with compression
   - `traffic_safety_validation.r` - Data quality validation
   - `traffic_safety_forecasting.r` - Time series forecasting
   - `traffic_safety_geospatial.r` - Spatial analysis and mapping

3. **Integration Module** (`traffic_safety_integration.r`):
   - Safely loads and coordinates all traffic safety components
   - Prevents pipeline hanging with timeout management
   - Provides fallback mechanisms if components fail

### Comprehensive Data Caching System (April 2025)

A new comprehensive caching system ensures the pipeline can run reliably even when external APIs are unavailable:

#### Key Features

1. **Multiple Fallback Mechanisms**:
   - Primary API access with error handling
   - Alternative API endpoints if primary fails
   - Direct file download if APIs are unavailable
   - Pre-downloaded sample data as final fallback

2. **Coverage for All Data Sources**:
   - Traffic safety data (NHTSA FARS, CDC WONDER)
   - County shapefiles from Census Bureau
   - CDC PLACES health indicators
   - USDA Food Environment Atlas
   - EPA environmental data (TRI, Air Quality)
   - Census Bureau data (ACS, Decennial, PEP)
   - FBI Crime data (UCR)
   - Healthcare data (HRSA AHRF)
   - Housing data (HUD CHAS, FMR)
   - Transportation data (NHTS)
   - IPUMS NHGIS time series data
   - IHME life expectancy data

3. **Robust Implementation**:
   - Three dedicated caching scripts:
     - `cache_sdoh_data.r` - Comprehensive caching for all sources
     - `cache_federal_data.r` - Focused on federal data sources
     - `traffic_safety_cache.r` - Special handling for traffic safety data
   - Safe download functions with timeouts and retries
   - Consistent directory structure for all cached data
   - Detailed logging and reporting of cache status

## Using the Dataset

The pipeline creates a DuckDB database in `output/us_county_sdoh_unified.duckdb`. You can connect to it using:

```r
library(DBI)
library(duckdb)

# Connect to the database
con <- dbConnect(duckdb::duckdb(), 'output/us_county_sdoh_unified.duckdb')

# Get the latest traffic safety data for all counties
latest_data <- dbGetQuery(con, "
  SELECT GEOID, county_name, 
         traffic_fatality_rate_per_100k, dui_fatality_rate_per_100k, 
         traffic_fatality_count_data_quality
  FROM latest_county_data
")

# Get time series traffic safety data for Los Angeles County
la_traffic_data <- dbGetQuery(con, "
  SELECT year, traffic_fatality_count, traffic_fatality_rate_per_100k,
         dui_fatality_count, ped_bike_fatality_count
  FROM county_time_series 
  WHERE GEOID = '06037' -- Los Angeles County
  ORDER BY year
")

# View hotspot analysis results
hotspots <- dbGetQuery(con, "
  SELECT * FROM traffic_safety_hotspots
  WHERE year = 2020
")

# Close the connection
dbDisconnect(con)
```

## Data Quality Indicators

Every record in the dataset includes comprehensive data quality indicators:

- **Data Source**: Original source of the data (Census, NHTSA FARS, CDC WONDER, etc.)
- **Data Vintage**: Year and specific collection the data came from
- **Data Quality**: One of:
  - 'direct' - Data directly from authoritative source
  - 'interpolated' - Values estimated between known data points
  - 'extrapolated' - Values projected beyond the available data
  - 'calculated' - Derived values (e.g., rates from counts and population)
  - 'imputed' - Values estimated using statistical methods
  - 'forecast' - Values predicted by time series models

## Citation

If you use this dataset in your research or applications, please cite it as:

```
US Social Determinants of Health Dataset (2025). 
Comprehensive county-level data from U.S. Census Bureau, CDC PLACES, 
NHTSA FARS, and other authoritative sources for social determinants 
of health analyses from 1970 to present.
```

## Contact

For questions or issues related to this dataset, please contact David Lary (davidlary@me.com).

## License

This dataset is provided for research and public health purposes. The code in this repository is licensed under the MIT License, while the aggregated data is provided under CC BY 4.0. Individual data sources maintain their original licensing terms.

