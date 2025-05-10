# Data Quality Policy

This document outlines the data quality requirements and handling procedures for the Social Determinants of Health dataset project.

## No Simulated Data Policy

As of April 2025, the project has a strict **No Simulated Data** policy. This means:

1. All data in the final dataset must be based on real, authoritative data sources
2. Variables with no available data will be marked as missing rather than populated with simulated values
3. Error messages will clearly indicate when data is missing and what files are needed

## Data Quality Flags

To maintain transparency about data origins, each variable includes a data quality flag:

- `direct`: Data obtained directly from an authoritative source
- `extrapolated`: Data extended beyond the available time series
- `interpolated`: Data estimated between existing data points
- `missing`: No data available (replaces any previously simulated data)

## File Format Support

Each data source must properly handle all known file formats:

### IHME Life Expectancy Data
- **Standard Format**: Files with `RACE_ETHN` in the name, containing `race_name`, `sex_name`, and `val` columns
- **Legacy Format**: Files without `RACE_ETHN` in the name, containing `LE_both`, `LE_male`, `LE_female` columns

### Census Bureau Data
- **ACS**: American Community Survey files
- **Decennial**: Decennial Census files
- **PEP**: Population Estimates Program files

### Traffic Safety Data
- **FARS**: Fatality Analysis Reporting System data
- **CDC WONDER**: Centers for Disease Control and Prevention mortality data

## Error Handling

When data is missing, the system follows this approach:

1. Check multiple locations for data files
2. Check multiple file formats
3. Check cache for previously processed data
4. If all checks fail, return an empty dataframe with the proper structure
5. Log a clear error message indicating:
   - What data is missing
   - Where to find or download the required data
   - How to place data in the expected location

## Testing and Validation

A comprehensive test suite (see `test_data_formats.r`) verifies:

1. All data sources can handle multiple file formats
2. No simulated data is used anywhere in the codebase
3. Error messages are clear and helpful when data is missing
4. Data quality flags accurately represent the data's origin

## Race/Ethnicity Standardization

For demographic data, the following standard race/ethnicity categories are used:

- `white`: White, non-Hispanic
- `black`: Black or African American, non-Hispanic
- `aian`: American Indian and Alaska Native, non-Hispanic
- `asian`: Asian, non-Hispanic
- `nhpi`: Native Hawaiian and Pacific Islander, non-Hispanic
- `latino`: Hispanic or Latino, any race
- `multi`: Two or more races, non-Hispanic
- `nhasian`: Asian, Native Hawaiian, and Pacific Islander (combined category)
- `total`: All races and ethnicities combined

## Fallback Order

When primary data sources are not available, the system will use this fallback order:

1. Primary data source (e.g., newest IHME dataset, newest ACS 5-year)
2. Alternative vintage/year of the same data source (e.g., previous year ACS)
3. Alternative related data source (e.g., ACS 1-year instead of 5-year)
4. Cache of previously processed data (if available)
5. Return empty dataset with clear error message

Importantly, fallback will NEVER include generating simulated data.