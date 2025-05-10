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

The database contains a total of 178 variables organized across multiple domains:

| Domain | Number of Variables | Primary Data Sources |
|--------|---------------------|----------------------|
| Demographics & Population | 24 | Census Bureau, IPUMS NHGIS, SEER |
| Economic Factors | 17 | Census ACS, BLS, Opportunity Insights |
| Education | 15 | Census ACS, NCES, Stanford Education Data Archive |
| Health Status | 29 | CDC PLACES, CDC WONDER, IHME |
| Healthcare Access | 11 | HRSA Area Health Resources Files, CMS |
| Housing | 18 | Census ACS, HUD CHAS, Eviction Lab |
| Environmental Health | 14 | EPA Air Quality System, EPA TRI, CDC Environmental Public Health Tracking |
| Food Environment | 12 | USDA Food Environment Atlas, Feeding America |
| Transportation | 13 | Census ACS, National Transit Database |
| Traffic Safety | 7 | NHTSA FARS, CDC WONDER |
| Social Cohesion | 12 | Census ACS, County Health Rankings, MIT Election Data |
| Crime & Safety | 8 | FBI Uniform Crime Reports, Bureau of Justice Statistics |
| Built Environment | 5 | EPA Smart Location Database, Trust for Public Land |
| Digital Access | 6 | FCC, Census ACS |
| Climate & Weather | 7 | NOAA, EPA |
| **Total** | **178** | |

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

3. Set up credentials (required for full access to data sources):
   - For Census data: `Rscript R/utilities/set_api_key.r YOUR_CENSUS_API_KEY`
   - For IPUMS/NHGIS: `Rscript R/utilities/set_ipums_credentials.r YOUR_USERNAME YOUR_PASSWORD`

4. Run the data pipeline:
```
cd US-SocialDeterminantsOfHealth/R
Rscript unified_sdoh_pipeline.r
```

### Command Line Options

- `--years=1970:2023`: Specify year range (default: most recent 10 years)
- `--force-update` or `-f`: Force refresh of all cached data
- `--verbose` or `-v`: Show detailed processing information
- `--skip-interpolation`: Disable interpolation for missing data points
- `--force-real-data=TRUE`: Ensure only real data is used (no simulations)
- `--offline-mode=TRUE`: Run in offline mode using only cached data
- `--output-format=csv,duckdb,sqlite`: Specify output format(s)

## Data Dictionary Summary

### Demographics & Population Data (24 variables)
Population counts, age distribution, race/ethnicity metrics including: total population, median age, population by gender, age groups, racial/ethnic groups, urban/rural breakdown, dependency ratio, and migration rates.

### Economic Factors (17 variables)
Income, poverty, employment, economic mobility metrics including: median household income, poverty rate, income inequality measures, unemployment, labor force participation, economic opportunity indices, and persistent poverty indicators.

### Education (15 variables)
Educational attainment, quality of schools, educational outcomes including: educational attainment levels, educational opportunity indices, achievement gaps, graduation rates, school funding, student-teacher ratios.

### Health Status (29 variables)
Disease prevalence, mortality, health behaviors including: prevalence of various chronic conditions, mental health indicators, life expectancy, mortality rates, health behaviors like smoking and physical activity.

### Healthcare Access (11 variables)
Insurance coverage, provider availability, healthcare utilization including: insurance status, healthcare provider density, hospital availability, preventive services utilization.

### Housing (18 variables)
Housing affordability, homeownership, housing quality including: home values, rent levels, homeownership rates, housing cost burden, eviction rates, housing quality indicators.

### Environmental Health (14 variables)
Air and water quality, toxic exposure, climate indicators including: air pollution measures, water quality violations, lead exposure, extreme weather metrics, proximity to environmental hazards.

### Food Environment (12 variables)
Food access, food insecurity, nutrition assistance including: food insecurity rates, grocery store access, food retail environment, SNAP participation.

### Transportation (13 variables)
Commuting patterns, vehicle access, public transit including: commute times, commute modes, vehicle access, public transit availability and usage, transportation costs.

### Traffic Safety (7 variables)
Fatalities, injuries, risk factors like DUI and speeding including: traffic fatality and injury counts and rates, pedestrian/cyclist safety metrics, transport-related mortality.

### Social Cohesion (12 variables)
Social capital, civic participation, family structure including: family structures, language proficiency, digital connectivity, organizational density, civic participation, social association rates.

### Crime & Safety (8 variables)
Crime rates, community violence, incarceration including: violent and property crime rates, homicide rates, incarceration metrics, juvenile justice indicators.

### Built Environment (5 variables)
Land use, walkability, recreation access including: employment accessibility, housing density, land use diversity, park access and availability.

### Digital Access (6 variables)
Internet and computer access, broadband availability including: broadband access, internet connectivity, computer ownership, cellular coverage.

### Climate & Weather (7 variables)
Temperature, precipitation, extreme weather events including: drought severity, extreme heat and precipitation events, flood risk, temperature and precipitation patterns, natural disaster frequency.

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
- **Data Quality**: One of: 'direct', 'interpolated', 'extrapolated', 'calculated', 'imputed', or 'forecast'

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