# IHME Life Expectancy Data Processing

This document explains how the IHME (Institute for Health Metrics and Evaluation) life expectancy data is processed in our pipeline.

## Data Source

IHME provides county-level life expectancy estimates by race/ethnicity and gender. The data is available from 2000-2019 and is located in the `data/ihme/CSV` directory.

The data contains life expectancy estimates for:
- Overall population
- By gender (male/female)
- By race/ethnicity (White, Black, Hispanic, Asian, American Indian/Alaska Native, etc.)
- By combinations of gender and race/ethnicity

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
- `Location` - County name
- `FIPS` - County FIPS code
- `State` - State abbreviation
- `LE_both` - Life expectancy for both genders
- `LE_male` - Life expectancy for males
- `LE_female` - Life expectancy for females
- `SD_both` - Standard deviation for both genders
- `SD_male` - Standard deviation for males
- `SD_female` - Standard deviation for females
- `LE_race_white` - Life expectancy for White population
- `LE_race_black` - Life expectancy for Black population
- `LE_race_hispanic` - Life expectancy for Hispanic population
- `LE_race_asian` - Life expectancy for Asian population

## Race/Ethnicity Mapping

To standardize the race/ethnicity values across our dataset, we map the IHME race names to our standard codes:

| IHME Race Name | Our Code | Description |
|----------------|----------|-------------|
| Total | all | All races/ethnicities |
| Latino | hispanic | Hispanic/Latino |
| White | nhw | Non-Hispanic White |
| Black | nhb | Non-Hispanic Black |
| Asian | nhasian | Non-Hispanic Asian |
| AIAN | nhaian | Non-Hispanic American Indian/Alaska Native |
| NHPI | nhpi | Non-Hispanic Pacific Islander |
| API | nhasian | Asian/Pacific Islander (older files) |
| Multiple races | multirace | Multiple races |
| Other | multirace | Other races |

## Variable Naming Convention

The life expectancy variables follow this naming pattern:
- `life_expectancy` - Overall life expectancy
- `life_expectancy_[gender]` - Life expectancy by gender (male/female)
- `life_expectancy_[race]` - Life expectancy by race/ethnicity
- `life_expectancy_[gender]_[race]` - Life expectancy by gender and race/ethnicity
- `le_[race]_lower_ci` - Lower confidence interval
- `le_[race]_upper_ci` - Upper confidence interval

## Processing Steps

1. Detect file format based on column names
2. Extract year and gender from filename
3. For standard format:
   - Map race_name to standardized race codes
   - Rename columns to match our schema
   - Add year and gender columns
4. For legacy format:
   - Extract year from filename
   - Create separate entries for overall, male, and female life expectancy
   - Process race-specific columns
   - Create confidence intervals using standard deviations where available
5. Format FIPS codes consistently
6. Combine all data

## Total IHME Variables

Our dataset includes 29 IHME life expectancy variables with different combinations of:
- Gender (overall, male, female)
- Race/ethnicity (overall, Hispanic, Black, White, Asian, American Indian, etc.)
- Confidence intervals (lower and upper bounds)

These variables provide a comprehensive view of life expectancy differences across demographic groups.