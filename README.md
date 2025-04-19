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

The database contains organized tables with standardized variables across multiple domains.

### Summary of Variables by Domain

| Domain | Number of Variables | Primary Data Sources |
|--------|---------------------|----------------------|
| Demographics & Population | 12 | Census Bureau, IPUMS NHGIS |
| Economic Factors | 11 | Census ACS, BLS, Opportunity Insights |
| Education | 8 | Census ACS, NCES, Stanford Education Data Archive |
| Health Status | 14 | CDC PLACES, CDC WONDER |
| Healthcare Access | 10 | HRSA Area Health Resources Files, CMS |
| Housing | 8 | Census ACS, HUD CHAS, Eviction Lab |
| Environmental Health | 15 | EPA Air Quality System, CDC Environmental Public Health Tracking |
| Food Environment | 7 | USDA Food Environment Atlas, Feeding America |
| Transportation | 7 | Census ACS, National Transit Database |
| Traffic Safety | 11 | NHTSA FARS, CDC WONDER |
| Social Cohesion | 7 | Census ACS, County Health Rankings, MIT Election Data |
| Crime & Safety | 5 | FBI Uniform Crime Reports, Bureau of Justice Statistics |
| Built Environment | 10 | EPA Smart Location Database, Trust for Public Land |
| Disability | 7 | Census ACS |
| Health Behaviors | 4 | CDC PLACES |
| **Total** | **136** | |

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

## Getting Started

### Installation

1. Clone this repository:
```
git clone https://github.com/davidlary/US-SocialDeterminantsOfHealth.git
cd US-SocialDeterminantsOfHealth
```

2. Install required R packages:
```
Rscript R/install_packages.r
```

3. Run the data pipeline:
```
Rscript R/unified_sdoh_pipeline.r
```

### Command Line Options

- `--years=1970:2023`: Specify year range (default: most recent 10 years)
- `--force-update` or `-f`: Force refresh of all cached data
- `--verbose` or `-v`: Show detailed processing information
- `--skip-interpolation`: Disable interpolation for missing data points
- `--offline-mode` or `--offline`: Run in offline mode using only cached data
- `--output-format=csv,duckdb,sqlite`: Specify output format(s)

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
| traffic_fatality_count | Traffic fatalities | Count | NHTSA FARS | 1975-present |
| traffic_fatality_rate_per_100k | Traffic fatality rate | Rate per 100k | NHTSA FARS | 1975-present |
| traffic_injury_count | Traffic injuries | Count | NHTSA FARS | 1975-present |
| traffic_injury_rate_per_100k | Traffic injury rate | Rate per 100k | NHTSA FARS | 1975-present |
| ped_bike_fatality_count | Pedestrian/cyclist fatalities | Count | NHTSA FARS | 1975-present |
| ped_bike_fatality_rate_per_100k | Pedestrian/cyclist fatality rate | Rate per 100k | NHTSA FARS | 1975-present |
| dui_fatality_count | DUI-related fatalities | Count | NHTSA FARS | 1975-present |
| dui_fatality_rate_per_100k | DUI-related fatality rate | Rate per 100k | NHTSA FARS | 1975-present |
| speeding_fatality_count | Speeding-related fatalities | Count | NHTSA FARS | 1975-present |
| speeding_fatality_rate_per_100k | Speeding-related fatality rate | Rate per 100k | NHTSA FARS | 1975-present |
| transport_mortality_count | Transport-related deaths | Count | CDC WONDER | 1970-present |

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

## Traffic Safety Module

The traffic safety module is a comprehensive component that fetches and analyzes traffic safety data at the county level across the United States. It provides detailed information about traffic fatalities, injuries, and related risk factors from 1970 to the present.

### Key Features

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

### Integration with Pipeline

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

## Using the Dataset

The pipeline creates a DuckDB database in `output/us_county_sdoh_unified.duckdb`. You can connect to it using:

```r
library(DBI)
library(duckdb)

# Connect to the database
con <- dbConnect(duckdb::duckdb(), 'output/us_county_sdoh_unified.duckdb')

# Get the latest traffic safety data for all counties
latest_data <- dbGetQuery(con, "
  SELECT fips, county_name, 
         traffic_fatality_rate_per_100k, dui_fatality_rate_per_100k, 
         traffic_fatality_count_data_quality
  FROM latest_county_data
")

# Get time series traffic safety data for Los Angeles County
la_traffic_data <- dbGetQuery(con, "
  SELECT year, traffic_fatality_count, traffic_fatality_rate_per_100k,
         dui_fatality_count, ped_bike_fatality_count
  FROM county_time_series 
  WHERE geoid = '06037' -- Los Angeles County
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