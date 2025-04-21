# Data Quality Policy

This document outlines the data quality requirements and handling procedures for the Social Determinants of Health dataset project.

## No Simulated Data Policy

As of April 2025, the project has a strict **No Simulated Data** policy. This means:

1. All data in the final dataset must be based on real, authoritative data sources
2. Variables with no available data will be marked as missing rather than populated with simulated values
3. Error messages will clearly indicate when data is missing and what files are needed

## Data Quality Flags

The following data quality flags are used to track the origin of each data point:

- `direct` - Data comes directly from an authoritative source
- `interpolated` - Data is interpolated from real values (filling gaps in time series)
- `extrapolated` - Data is extrapolated from real values (extending beyond available years)
- `imputed` - Data is imputed from other real values using statistical methods
- `missing` - Data is not available and marked as missing (NA values)

## File Format Support

The pipeline now supports multiple file formats for each data source:

### IHME Life Expectancy Data
- Standard format: `IHME_USA_LE_COUNTY_RACE_ETHN_*.CSV` files with columns:
  - `location_id`, `location_name`, `race_name`, `sex_name`, `val`, `lower`, `upper`
- Legacy format: `IHME_USA_LE_COUNTY_*.CSV` files with columns:
  - `Location`, `FIPS`, `LE_both`, `LE_male`, `LE_female`, `LE_race_*`

### Traffic Safety Data
- FARS CSV files:
  - Direct FARS files with columns: `state`, `county`, etc.
  - Pre-processed county-level files with `fips`, `year`, `traffic_fatality_count`, etc.
- CDC WONDER data:
  - CSV files with columns: `year`, `fips`, `deaths`, `population`, `crude_rate`

### Census Bureau Data
- ACS data: `acs*_county_*.csv` files with proper column headers
- Decennial Census: `dec_county_*.csv` files
- Population Estimates: `pep_county_*.csv` files

## Error Messages

When required data is missing, the pipeline now provides clear error messages that:

1. Identify which data is missing
2. Explain where to get the missing data
3. Describe the required file format
4. Provide file placement instructions

Example:
```
ERROR: No Census data files found. Please download Census data.
Required files should be in one of the following directories: data/census_acs, data/census_decennial, data/census_pep, data/cache/census
File names should include 'acs', 'dec', or 'pep' with a CSV extension.
```

## Handling Missing Data

Instead of generating simulated data when real data is unavailable:

1. Variables will be marked as `NA` with a `_data_quality` flag set to "missing"
2. Error messages will indicate what data is missing and how to obtain it
3. The pipeline will continue processing other available data sources
4. The final database will include quality metadata to filter out missing values

## Fallback Order

When multiple sources may provide the same data:

1. Primary data files in standard locations
2. Cached data files (if not forced to refresh)
3. Alternative data formats (if available)
4. Mark as missing (never generate simulated data)

## Data Validation

All data goes through validation checks:

1. Confirmation of proper file format
2. Verification of required columns
3. Standardization of column names and types
4. Range checks for known values
5. Consistency checks across sources

## Quality Report

After processing, the pipeline generates a data quality report showing:
- Number of variables with direct data
- Number of variables with interpolated data
- Number of variables with extrapolated data 
- Number of variables with missing data