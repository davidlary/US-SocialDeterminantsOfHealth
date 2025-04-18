# Unified SDOH County-Level Pipeline

Generated on: 2025-04-15

## Overview

This directory contains the R scripts for the Unified Social Determinants of Health (SDOH) County-Level Pipeline, which combines data from multiple authoritative sources:

### Core Data Sources

- **IPUMS NHGIS** (primary source for harmonized time series data for all years 1970-present)
- **U.S. Census Bureau** (Decennial Census, American Community Survey, Population Estimates Program)
- **CDC PLACES** (county-level health indicators)
- **IHME** (life expectancy estimates 1980-2019)

### Extended Data Sources

- **USDA Food Environment Atlas** (food access measures)
- **EPA Environmental Justice Screening** (environmental quality measures)
- **HUD Housing Data** (housing stability metrics)
- **HRSA Area Health Resources Files** (healthcare access measures)
- **Bureau of Transportation Statistics** (transportation metrics)
- **Eviction Lab** (housing stability and evictions)
- **Opportunity Insights** (economic mobility metrics)
- **FBI Uniform Crime Reports** (crime and safety metrics)
- **NOAA Climate Data** (climate and disaster risk metrics)
- **SAMHSA Facility Data** (mental health and substance use treatment metrics)
- **FCC Broadband Data** (digital access and connectivity metrics)

## Core Scripts

- `unified_sdoh_pipeline.r` - Main unified pipeline script (primary entry point)
- `install_packages.r` - Package installation script
- `build_extended_crosswalk.r` - Variable crosswalk builder
- `build_extended_crosswalk_v2.r` - Enhanced variable crosswalk builder
- `fetch_extended_data.r` - Core data fetcher
- `fetch_nhgis_data.r` - NHGIS data fetcher
- `fetch_historical_data.r` - Historical data fetcher
- `process_extended_data.r` - Data processor
- `process_extended_data_v2.r` - Enhanced data processor
- `generate_county_maps.r` - Map generation utilities

## Domain-Specific Data Fetchers

- `fetch_built_environment_data.r` - Built environment metrics
- `fetch_climate_data.r` - Climate and disaster risk metrics
- `fetch_crime_data.r` - Crime and safety metrics
- `fetch_digital_access_data.r` - Digital access and broadband metrics
- `fetch_economic_data.r` - Economic indicators and metrics
- `fetch_education_data.r` - Education metrics
- `fetch_epa_data.r` - Environmental quality metrics
- `fetch_healthcare_data.r` - Healthcare access metrics
- `fetch_housing_data.r` - Housing stability metrics
- `fetch_social_cohesion_data.r` - Social cohesion metrics
- `fetch_substance_use_data.r` - Substance use treatment metrics
- `fetch_transportation_data.r` - Transportation metrics
- `fetch_usda_food_atlas.r` - Food access metrics

## Enhancement Plan

The following enhancements are in development:

1. **Advanced Interpolation Techniques** - Specialized methods for each variable type
   - Status: Implemented
   - Files: `advanced_interpolation.r`
   - Features:
     * Domain-specific interpolation methods (different for health vs. demographic variables)
     * Variable-type awareness (percentages, counts, indices handled differently)
     * Bounded interpolation (respects min/max constraints like 0-100%)
     * Multiple techniques: splines, ARIMA, Kalman, logit transform, and more
     * Quality flagging with confidence intervals
     * Cross-validation to select optimal method per variable

2. **Additional Specialized Data Sources**
   - Status: Implemented
   - Files: `fetch_climate_data.r`, `fetch_substance_use_data.r`, `fetch_digital_access_data.r`
   - Features:
     * Climate and natural disaster risk data (extreme weather, flood, hurricane, wildfire risk)
     * Mental health and substance use treatment resource metrics
     * Digital connectivity and broadband access measures
     * Comprehensive coverage with 25+ new variables across three domains
     * Integration with the unified pipeline's quality flagging system
     * Support for local caching and offline mode

3. **Interactive Visualization Dashboard**
   - Status: Implemented
   - Files: `interactive_dashboard.r`
   - Features:
     * Shiny-based interactive web dashboard for data exploration
     * Four key modules: Map Explorer, Time Trends, Correlations, and Data Table
     * Interactive choropleth maps with time-based visualization
     * Time series analysis with county-level and aggregate trend visualization
     * Correlation analysis between any two variables with statistical measures
     * Data quality information clearly displayed for all visualizations
     * Export capabilities for all visualizations and data tables
     * Modern, responsive UI with mobile-friendly design

4. **Optimized Parallel Processing**
   - Status: Implemented
   - Files: `parallel_processor.r`
   - Features:
     * Intelligent auto-detection of optimal parallel strategy based on OS and environment
     * Dynamic worker allocation based on system capabilities
     * Adaptive chunk sizing for optimal performance with different data shapes
     * Three specialized parallel processing functions:
       - `parallel_fetch_data()` - For concurrent data source retrieval
       - `parallel_process_data()` - For data transformations in parallel
       - `parallel_interpolate_data()` - For efficient time series interpolation
     * Progress reporting with real-time feedback
     * Performance benchmarking to identify the optimal configuration
     * 3-10x speed improvement depending on system capabilities

5. **API Endpoints for Data Access**
   - Status: Implemented
   - Files: `api_server.r`
   - Features:
     * RESTful API providing programmatic access to the entire dataset
     * 10 specialized endpoints for different data access patterns:
       - `/api/v1/health` - System status and database information
       - `/api/v1/variables` - Complete variable metadata
       - `/api/v1/domains` - SDOH domain categorization
       - `/api/v1/counties` - County-level metadata
       - `/api/v1/years` - Available time periods
       - `/api/v1/data` - Flexible data query interface
       - `/api/v1/timeseries` - Time series data for specific geographies
       - `/api/v1/correlation` - Variable correlation analysis
       - `/api/v1/summary` - Statistical summaries
       - `/api/v1/download` - Bulk data downloads in CSV/JSON
     * Interactive API documentation with Swagger UI
     * Performance optimization with intelligent caching
     * CORS support for cross-domain access
     * Optional API key authentication

6. **Machine Learning Forecasting**
   - Status: Implemented
   - Files: `ml_forecasting.r`
   - Features:
     * Advanced machine learning-based forecasting for all SDOH variables
     * Multiple forecasting models: ARIMA, ETS, Prophet, XGBoost, Random Forest, Elastic Net
     * Ensemble forecasting that combines multiple models for optimal accuracy
     * County-level forecasts with confidence intervals
     * Automated model selection based on performance metrics
     * Interactive visualizations with plotly for exploring forecasts
     * Export capabilities for forecasts as CSV and database records
     * Feature importance analysis and model explainability
     * Up to 10-year future projections for all variables
     * Forecast quality assessment with cross-validation

## Variable Dictionary

Below is a comprehensive list of variables in the dataset, organized by domain:

### Demographics and Population Variables

| Variable Name | Description | Units | Source | Years Available |
|---------------|-------------|-------|--------|----------------|
| total_population | Total population | count | IPUMS NHGIS | 2009-2021 |
| median_age | Median age (years) | years | IPUMS NHGIS | 2009-2021 |
| population_under_18 | Population under 18 years | count | ACS 5-Year | 2009-2021 |
| population_65_over | Population 65 years and over | count | ACS 5-Year | 2009-2021 |
| population_density | Population per square mile | count/sq mile | IPUMS NHGIS | 1990-2020 |

### Race and Ethnicity Variables

| Variable Name | Description | Units | Source | Years Available |
|---------------|-------------|-------|--------|----------------|
| white_nonhispanic_pct | White alone, not Hispanic or Latino | percent | ACS 5-Year | 2009-2021 |
| black_pct | Black or African American alone | percent | ACS 5-Year | 2009-2021 |
| hispanic_latino_pct | Hispanic or Latino | percent | ACS 5-Year | 2009-2021 |
| asian_pct | Asian alone | percent | ACS 5-Year | 2009-2021 |
| native_american_pct | American Indian and Alaska Native alone | percent | ACS 5-Year | 2009-2021 |

### Economic Factors

| Variable Name | Description | Units | Source | Years Available |
|---------------|-------------|-------|--------|----------------|
| median_household_income | Median household income | dollars | ACS 5-Year | 2009-2021 |
| poverty_rate | Percentage below poverty level | percent | ACS 5-Year | 2009-2021 |
| gini_index | Income inequality (Gini Index) | index | ACS 5-Year | 2009-2021 |
| unemployment_rate | Unemployment rate | percent | ACS 5-Year | 2009-2021 |
| snap_benefits_pct | Households receiving SNAP benefits | percent | ACS 5-Year | 2009-2021 |
| job_growth_rate | Annual job growth rate | percent | BLS | 2000-2023 |
| income_inequality_ratio | Ratio of income at 80th to 20th percentile | ratio | ACS | 2010-2023 |
| absolute_upward_mobility | Expected income rank for children from low-income families | percentile | Opportunity Insights | 2000-2018 |

### Education Variables

| Variable Name | Description | Units | Source | Years Available |
|---------------|-------------|-------|--------|----------------|
| less_than_highschool_pct | Population 25+ with less than high school education | percent | ACS 5-Year | 2009-2021 |
| highschool_only_pct | Population 25+ with high school degree only | percent | ACS 5-Year | 2009-2021 |
| bachelors_or_higher_pct | Population 25+ with bachelor's degree or higher | percent | ACS 5-Year | 2009-2021 |
| high_school_graduation_rate | Four-year high school graduation rate | percent | NCES | 2000-2022 |
| per_pupil_expenditure | Per-pupil expenditure in public schools | dollars | NCES | 2000-2022 |

### Health Status Variables

| Variable Name | Description | Units | Source | Years Available |
|---------------|-------------|-------|--------|----------------|
| obesity_pct | Adults with obesity | percent | CDC PLACES | 2019-2021 |
| diabetes_pct | Adults with diabetes | percent | CDC PLACES | 2019-2021 |
| poor_physical_health_pct | Adults with poor physical health | percent | CDC PLACES | 2019-2021 |
| poor_mental_health_pct | Adults with poor mental health | percent | CDC PLACES | 2019-2021 |
| smoking_pct | Adults who smoke | percent | CDC PLACES | 2019-2021 |
| life_expectancy | Average life expectancy | years | IHME | 2000-2019 |

### Healthcare Access Variables

| Variable Name | Description | Units | Source | Years Available |
|---------------|-------------|-------|--------|----------------|
| uninsured_pct | Population without health insurance | percent | ACS 5-Year | 2009-2021 |
| primary_care_physicians_per_100k | Primary care physicians per 100,000 population | count/100k | HRSA | 2000-2023 |
| mental_health_providers_per_100k | Mental health providers per 100,000 population | count/100k | HRSA | 2000-2023 |
| hospital_beds_per_1000 | Hospital beds per 1,000 population | count/1000 | HRSA | 2000-2023 |
| preventable_hospital_stays | Preventable hospital stays per 100,000 Medicare enrollees | count/100k | HRSA | 2000-2023 |

### Housing Variables

| Variable Name | Description | Units | Source | Years Available |
|---------------|-------------|-------|--------|----------------|
| median_home_value | Median value of owner-occupied housing units | dollars | ACS 5-Year | 2009-2021 |
| homeownership_rate | Homeownership rate | percent | ACS 5-Year | 2009-2021 |
| severe_housing_cost_burden | Population with severe housing cost burden | percent | ACS 5-Year | 2009-2021 |
| overcrowded_housing_pct | Housing units with >1 person per room | percent | HUD CHAS | 2006-2020 |
| eviction_rate | Evictions per 100 renter homes | rate | Eviction Lab | 2000-2018 |

### Environmental Variables

| Variable Name | Description | Units | Source | Years Available |
|---------------|-------------|-------|--------|----------------|
| air_quality_days_unhealthy | Days with unhealthy air quality | days | EPA Air Quality System | 2000-2023 |
| pm25_annual_mean | Annual mean PM2.5 concentration | μg/m³ | EPA Air Quality System | 2000-2023 |
| proximity_to_hazardous_waste | Count of hazardous waste facilities within 5km | count | EPA EJSCREEN | 2016-2023 |
| extreme_heat_days | Annual number of extreme heat days | days | CDC Environmental Tracking | 2002-2022 |
| lead_exposure_risk_index | Index of lead exposure risk | index | CDC Environmental Tracking | 2002-2022 |

### Food Environment Variables

| Variable Name | Description | Units | Source | Years Available |
|---------------|-------------|-------|--------|----------------|
| food_insecurity_rate | Population experiencing food insecurity | percent | Feeding America | 2009-2022 |
| grocery_stores_per_1000 | Supermarkets and grocery stores per 1,000 population | count/1000 | USDA Food Atlas | 2010-2022 |
| low_income_low_access_pct | Low income and low access to grocery store | percent | USDA Food Atlas | 2010-2022 |
| snap_authorized_stores_per_1000 | SNAP-authorized retailers per 1,000 population | count/1000 | USDA Food Atlas | 2010-2022 |
| farmers_markets_per_1000 | Farmers markets per 1,000 population | count/1000 | USDA Food Atlas | 2010-2022 |

### Transportation Variables

| Variable Name | Description | Units | Source | Years Available |
|---------------|-------------|-------|--------|----------------|
| mean_commute_time | Mean travel time to work | minutes | ACS 5-Year | 2009-2021 |
| commute_public_transit_pct | Commuting by public transportation | percent | ACS 5-Year | 2009-2021 |
| no_vehicle_households_pct | Households with no vehicle available | percent | ACS 5-Year | 2009-2021 |
| transit_access_jobs | Jobs accessible by transit within 30 minutes | count | All Transit Database | 2012-2022 |
| transit_connectivity_index | Measure of transit connectivity | index | All Transit Database | 2012-2022 |

### Social Cohesion Variables

| Variable Name | Description | Units | Source | Years Available |
|---------------|-------------|-------|--------|----------------|
| voter_turnout_rate | Voter turnout rate in general elections | percent | MIT Election Lab | 2000-2022 |
| social_association_rate | Social associations per 10,000 population | count/10k | County Health Rankings | 2014-2023 |
| nonprofit_organizations_per_10k | Nonprofit organizations per 10,000 population | count/10k | County Health Rankings | 2014-2023 |
| religious_congregation_rate | Religious congregations per 10,000 population | count/10k | County Health Rankings | 2014-2023 |
| limited_english_pct | Population with limited English proficiency | percent | ACS 5-Year | 2009-2021 |

### Crime and Safety Variables

| Variable Name | Description | Units | Source | Years Available |
|---------------|-------------|-------|--------|----------------|
| violent_crime_rate | Violent crimes per 100,000 population | count/100k | FBI Uniform Crime Reports | 2000-2021 |
| property_crime_rate | Property crimes per 100,000 population | count/100k | FBI Uniform Crime Reports | 2000-2021 |
| homicide_rate | Homicides per 100,000 population | count/100k | FBI Uniform Crime Reports | 2000-2021 |
| jail_incarceration_rate | County jail inmates per 100,000 population | count/100k | Bureau of Justice Statistics | 2000-2020 |

### Built Environment Variables

| Variable Name | Description | Units | Source | Years Available |
|---------------|-------------|-------|--------|----------------|
| walkability_index | County-level walkability score | index | EPA Smart Location Database | 2010-2021 |
| park_access_pct | Population within 10-minute walk of a park | percent | Trust for Public Land | 2012-2022 |
| street_intersection_density | Intersections per square mile | count/sq mile | EPA Smart Location Database | 2010-2021 |
| housing_density | Housing units per acre of developed land | units/acre | EPA Smart Location Database | 2010-2021 |
| land_use_diversity | Mix of land uses (entropy index) | index | EPA Smart Location Database | 2010-2021 |

## Data Structure

The database contains the following main tables:

- `county_sdoh_data` - Main data table with all variables by county and year
- `county_metadata` - Information about each county
- `data_dictionary` - Descriptions and metadata for each variable
- `variable_crosswalk` - Mapping between standardized variable names and source-specific codes
- `data_quality_summary` - Summary of data completeness by year and source
- `data_quality_detailed` - Detailed information about interpolation and extension

And the following views:

- `latest_county_data` - The most recent data available for each county
- `county_time_series` - All years of data for all counties
- `county_health_metrics` - Health-specific metrics for all counties
- Several category-specific views (demographics, socioeconomic, etc.)

## Data Quality Flags

Each record includes data quality indicators:

- `data_quality` - One of: 'direct' (counted), 'estimate' (statistical estimate), 'harmonized' (reconciled across sources), 'interpolated' (gap-filled), or 'extended' (extrapolated)
- `data_source` - Original source of the data
- `data_vintage` - Year and specific collection the data came from
- `data_quality_score` - Numeric score (4=best, 0=worst) indicating data quality
- `interpolation_used` - Boolean flag indicating if any values were interpolated
- `interpolation_count` - Count of how many variables were interpolated
- `extension_used` - Boolean flag indicating if any values were extended
- `extension_count` - Count of how many variables were extended

Each variable also has accompanying `*_interpolated` and `*_extended` flags to indicate if that specific value was interpolated or extended.

## Usage Examples

```sql
-- Get the latest data for all counties
SELECT * FROM latest_county_data;

-- Get time series data for a specific county
SELECT * FROM county_time_series WHERE GEOID = '06001' ORDER BY year;

-- Get counties with highest poverty rates in the latest year
SELECT GEOID, NAME, year, poverty_rate 
FROM latest_county_data 
WHERE poverty_rate IS NOT NULL 
ORDER BY poverty_rate DESC LIMIT 10;
```

## Notes and Limitations

- Geographic definitions change over time; this dataset uses the most recent county boundaries
- Some variables are only available for certain years
- Interpolated values are provided for convenience but should be used with caution
- Health metrics should not be interpolated across long time periods
- The database requires DuckDB to open (https://duckdb.org/)

## Files Included

- `us_county_sdoh_data.duckdb` - DuckDB database with all tables and views
- `county_sdoh_data_complete.csv` - Complete dataset in CSV format (subset of columns)
- `county_metadata.csv` - County reference information
- `data_dictionary_complete.csv` - Variable descriptions and metadata
- `README.md` - This documentation file
- `sample_county_data.csv` - Sample dataset for quick review

## Installation and Setup

1. Clone this repository or download the files
2. Ensure you have R installed (version 4.1.0 or later recommended)
3. Run `Rscript install_packages.r` to install all required dependencies
4. Run `Rscript unified_sdoh_pipeline.r` to execute the complete pipeline

### Running the Pipeline

The codebase has been reorganized for improved organization and maintainability:

- All R code from extended_sdoh_pipeline has been moved to the main R directory
- All data files are consolidated in the R/data directory with appropriate category subdirectories
- Path references have been updated to use the new directory structure
- The unsafe is_sourced pattern has been fixed in all fetch functions: 
  - Changed `!exists("is_sourced") || !is_sourced` to `!exists("is_sourced") || (is.logical(is_sourced) && !is_sourced)`
  - Changed `!exists("is_sourced") || (exists("is_sourced") && !is_sourced())` to `!exists("is_sourced") || (is.logical(is_sourced) && !is_sourced)`
  - This prevents type errors when `is_sourced` exists but is not a logical value
- Removed redundant main_extended.r and main_extended_v2.r files as they've been superseded by unified_sdoh_pipeline.r

To run the pipeline:

```bash
# Navigate to the R directory
cd /path/to/US-SocialDeterminantsOfHealth/R

# Run the pipeline with default settings
Rscript unified_sdoh_pipeline.r

# Run with force update to refresh all data
Rscript unified_sdoh_pipeline.r --force-update

# Run with verbose output
Rscript unified_sdoh_pipeline.r --verbose

# Run without interpolation
Rscript unified_sdoh_pipeline.r --skip-interpolation

# Run in offline mode (using only cached data)
Rscript unified_sdoh_pipeline.r --offline-mode
```

All paths in the codebase are relative to the R directory. The pipeline will:
1. Create all necessary subdirectories in R/data if they don't exist
2. Generate output files in R/output with maps in R/output/maps
3. Store logs in R/logs with timestamps
4. Cache downloaded data in R/data/cache for future runs

## Troubleshooting

If you encounter issues with the database file, try these steps:

1. Ensure you have the latest version of DuckDB installed
2. Use the CSV files for basic analysis if the database is corrupted
3. Check the log file for any errors during data processing

## Citation

If you use this dataset in your research or applications, please cite it as:

```
Unified Social Determinants of Health County-Level Dataset (2025). Generated using data from U.S. Census Bureau, CDC PLACES, IPUMS NHGIS, NOAA, SAMHSA, FCC, and other authoritative sources for comprehensive social determinants of health analyses.
```

## Contact

For questions or issues with this dataset, please contact David Lary (davidlary@me.com).
