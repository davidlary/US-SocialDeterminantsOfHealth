# Traffic Safety Data

This directory contains county-level traffic safety data for the Social Determinants of Health (SDOH) pipeline.

## Data Sources

The traffic safety data is collected from multiple authoritative sources:

1. **NHTSA's Fatality Analysis Reporting System (FARS)**
   - Source: National Highway Traffic Safety Administration
   - URL: https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars
   - Coverage: 1975-present
   - Description: Nationwide census providing data on all vehicle crashes in the United States that result in a fatality

2. **CDC WONDER - Multiple Cause of Death Database**
   - Source: Centers for Disease Control and Prevention
   - URL: https://wonder.cdc.gov/mcd.html
   - Coverage: 1999-present
   - Description: County-level mortality data including transportation-related deaths (ICD-10 codes V01-V99)

## Data Processing

The script `fetch_traffic_safety_data.r` retrieves data from these sources and processes it for integration into the SDOH pipeline:

1. Data is retrieved from APIs where available
2. Local data files are used as backups
3. Geographic data is standardized to county FIPS codes
4. Interpolation is applied for missing years (when enabled)
5. Data quality flags track the origin of each value
6. Placeholder simulation can generate representative data when real data is unavailable

## Variables

The dataset includes the following key variables:

| Variable | Description | Source |
|----------|-------------|--------|
| traffic_fatality_count | Total number of traffic-related deaths | FARS/CDC |
| traffic_fatality_rate_per_100k | Traffic fatality rate per 100,000 population | Calculated |
| traffic_injury_count | Total number of traffic-related injuries | FARS |
| traffic_injury_rate_per_100k | Traffic injury rate per 100,000 population | Calculated |
| ped_bike_fatality_count | Pedestrian and cyclist fatalities | FARS |
| ped_bike_fatality_rate_per_100k | Pedestrian and cyclist fatality rate per 100,000 | Calculated |
| dui_fatality_count | Alcohol-related traffic fatalities | FARS |
| dui_fatality_rate_per_100k | Alcohol-related fatality rate per 100,000 | Calculated |
| speeding_fatality_count | Speeding-related traffic fatalities | FARS |
| speeding_fatality_rate_per_100k | Speeding-related fatality rate per 100,000 | Calculated |

## Data Quality

Each value includes a corresponding `_data_quality` field with one of the following values:

- `direct`: Data obtained directly from the source
- `interpolated`: Data interpolated from surrounding years
- `extrapolated`: Data extrapolated beyond available years
- `simulated`: Synthetic data generated when real data unavailable
- `imputed`: Values estimated using statistical methods
- `NA`: Missing data

## Usage

To access this data via the SDOH pipeline:

1. Set `allow_interpolation = TRUE` to fill gaps in time series
2. Set `allow_simulation = TRUE` to generate placeholder data when necessary
3. Set `offline_mode = TRUE` to use only locally cached data

Example:
```r
traffic_data <- fetch_traffic_safety_data(
  years = 2000:2020,
  cache_dir = "data/cache",
  refresh_cache = FALSE,
  allow_interpolation = TRUE
)
```

## References

1. National Highway Traffic Safety Administration. (2021). Fatality Analysis Reporting System (FARS). https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars

2. Centers for Disease Control and Prevention. (2022). CDC WONDER: Multiple Cause of Death, 1999-2020. https://wonder.cdc.gov/mcd.html

3. Kochanek, K. D., Murphy, S. L., Xu, J., & Arias, E. (2019). Deaths: Final data for 2017. National Vital Statistics Reports, 68(9), 1-77.