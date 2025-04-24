# IHME Life Expectancy Data Processing

This document explains how the IHME (Institute for Health Metrics and Evaluation) life expectancy data is processed in our pipeline.

## Data Source

IHME provides county-level life expectancy estimates by race/ethnicity and gender. The data is available from 2000-2019 and is located in the `data/ihme/CSV` directory.

## File Formats

The IHME data is provided in two different formats:

### 1. Standard Format

Files with names like `IHME_USA_LE_COUNTY_RACE_ETHN_2000_2019_LT_2019_BOTH_Y2022M06D16.CSV` use the standard format with columns:
- `location_id` - County FIPS code
- `location_name` - County name
- `race_name` - Race/ethnicity name (Total, Latino, White, Black, AIAN, API, etc.)
- `sex_name` - Sex (Both, Male, Female)
- `val` - Life expectancy value
- `lower` - Lower confidence interval
- `upper` - Upper confidence interval

### 2. Legacy Format

Files with names like `IHME_USA_LE_COUNTY_BOTH_2019.CSV` use the legacy format with columns:
- `location_id` - County FIPS code
- `location_name` - County name
- `LE_both` - Life expectancy for all races combined
- `LE_race_aian` - Life expectancy for American Indian/Alaska Native
- `LE_race_api` - Life expectancy for Asian/Pacific Islander
- `LE_race_black` - Life expectancy for Black
- `LE_race_latino` - Life expectancy for Hispanic/Latino
- `LE_race_white` - Life expectancy for White

## Race/Ethnicity Mapping

Our pipeline standardizes race/ethnicity categories across all data sources. For IHME data, we use the following mapping:

| IHME Category | Standardized Category |
|--------------|------------------------|
| Total | total |
| White | white |
| Black | black |
| AIAN | aian (American Indian/Alaska Native) |
| API | nhasian (Asian, Native Hawaiian, Pacific Islander combined) |
| Latino | latino |

## Variable Structure

The IHME data is processed into the following standardized variables:

1. `life_expectancy_total` - Life expectancy for all races/ethnicities combined
2. `life_expectancy_white` - Life expectancy for White, non-Hispanic
3. `life_expectancy_black` - Life expectancy for Black, non-Hispanic
4. `life_expectancy_aian` - Life expectancy for American Indian/Alaska Native, non-Hispanic
5. `life_expectancy_nhasian` - Life expectancy for Asian/Pacific Islander, non-Hispanic
6. `life_expectancy_latino` - Life expectancy for Hispanic/Latino, any race

For each race/ethnicity category, we also create gender-specific variables:
- `life_expectancy_[race]_male`
- `life_expectancy_[race]_female`

## Processing Steps

1. **Format Detection**: The code automatically detects the format of each file by:
   - Checking for "RACE_ETHN" in the filename (standard format)
   - Examining column names for "val" vs "LE_*" patterns

2. **Standard Format Processing**:
   - Files are filtered by year
   - Data is grouped by county FIPS, race/ethnicity, and sex
   - Values are pivoted to create variables in the standardized format

3. **Legacy Format Processing**:
   - Race-specific columns are renamed according to our standardization rules
   - Data is restructured to match the format of processed standard-format data

4. **Race/Ethnicity Standardization**:
   - All race/ethnicity categories are mapped to our standardized categories
   - Special handling for "API" mapping to "nhasian"

5. **Missing Data Handling**:
   - Missing values are preserved as NA
   - No simulated data is generated to fill gaps
   - Data quality flags track the source of each value (direct vs interpolated)

6. **Performance Optimizations**:
   - Adaptive parallel processing with automatic strategy selection (multicore/multisession) and fallback to sequential
   - Automatic dataset size estimation to determine optimal processing approach
   - Memory-efficient key processing with context-aware batch sizing (larger batches for parallel mode)
   - Chunked processing for race/ethnicity data with robust error handling
   - Optimized county-level data merging with parallel batch processing and conservative memory limits
   - Vectorization of key creation for single-column cases
   - Progress tracking with the progressr package for better visibility
   - Intelligent garbage collection to free memory between processing phases

## Data Usage

The processed IHME data provides 29 distinct variables:
- 1 overall life expectancy variable
- 4 race/ethnicity-specific life expectancy variables
- 4 race/ethnicity and gender-specific (male) life expectancy variables
- 4 race/ethnicity and gender-specific (female) life expectancy variables
- 16 year-specific versions of these variables for all available years

## Error Handling

If IHME data files are missing, the system:
1. Searches multiple directories for IHME data files
2. Checks both standard and legacy formats
3. Returns a clear error message if no files are found
4. Provides an empty dataframe with the proper structure
5. Logs the error with guidance on where to obtain the data

## Testing

To test proper IHME data processing, run:
```r
Rscript test_ihme_processing.r
```

This script tests:
1. Standard format detection and processing
2. Legacy format detection and processing
3. Race/ethnicity mapping
4. Data quality flagging