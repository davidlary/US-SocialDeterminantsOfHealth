# Extended SDOH County-Level Pipeline

This pipeline extends the existing Social Determinants of Health (SDOH) county-level dataset with additional variables from multiple authoritative sources. It operates parallel to the original pipeline, preserving all existing functionality while adding comprehensive new data domains.

## New Data Domains

This extended pipeline adds the following new SDOH domains:

1. **Food Environment & Access**: Food deserts, grocery store access, SNAP retailers
2. **Built Environment**: Walkability, parks, recreation access
3. **Environmental Health**: Air/water quality, toxic sites, climate indicators
4. **Economic Factors**: Income inequality, employment stability, job quality
5. **Housing**: Homelessness, housing stability, eviction rates
6. **Healthcare Access**: Provider ratios, facility proximity
7. **Transportation**: Transit access, connectivity
8. **Social Cohesion**: Voter participation, civic organizations
9. **Crime and Safety**: Violence rates, incarceration
10. **Educational Resources**: School quality, educational outcomes

## Pipeline Structure

The pipeline follows the same architecture as the original:

1. **Crosswalk Building**: `build_extended_crosswalk_v2.r`
   - Creates unified variable mapping across all data sources
   - Incorporates new variables with consistent naming

2. **Data Fetching**: Multiple fetchers for different data sources
   - `fetch_usda_food_atlas.r`: Food environment variables
   - `fetch_epa_data.r`: Environmental health variables
   - `fetch_housing_data.r`: Housing stability variables
   - And more specialized fetchers for each domain

3. **Data Processing**: `process_extended_data_v2.r`
   - Standardizes, cleans, and validates data
   - Performs intelligent interpolation with quality flags
   - Integrates with existing data

4. **Visualization**: `generate_extended_maps.r`
   - Creates choropleth maps for all new variables
   - Maintains organization by variable subdirectories

## Running the Extended Pipeline

```bash
# Run the complete extended pipeline
Rscript extended_sdoh_pipeline/main_extended_v2.r

# Run with specific options
Rscript extended_sdoh_pipeline/main_extended_v2.r --verbose --skip-interpolation --offline-mode

# Run specific components
Rscript extended_sdoh_pipeline/fetch_usda_food_atlas.r
Rscript extended_sdoh_pipeline/fetch_epa_data.r
```

### Command Line Options

- `--verbose` or `-v`: Show detailed processing information including data quality statistics
- `--force-update` or `-f`: Force refresh of cached data
- `--skip-interpolation`: Disable all interpolation for missing data points
- `--allow-interpolation`: Explicitly enable interpolation (default)
- `--allow-simulation`: Allow simulated data when real data is unavailable
- `--offline-mode` or `--offline`: Run in offline mode using only cached data

## Data Quality Tracking

The extended pipeline implements a comprehensive data quality tracking system:

- Every variable gets a corresponding `_data_quality`, `_data_source`, and `_data_vintage` column
- Data quality flags indicate how each data point was obtained:
  - `direct`: Data directly from source without modification
  - `interpolated`: Data interpolated from existing points (within time range)
  - `extrapolated`: Data extrapolated beyond available time range
  - `simulated`: Fully simulated data (not based on real values)
  - `missing`: Data that couldn't be obtained (stored as `NA`)
  - `imputed`: Values filled by statistical methods

When data is missing, the system prioritizes transparency:
- Missing values are always represented as `NA` rather than using simulated values by default
- The quality flag for missing values is set to `NA` (not a string) to ensure consistent identification
- All data fetchers use the same quality flag format for consistency across domains
- Missing value statistics are logged clearly when running with `--verbose`
- Original data source and data vintage are preserved for direct values; derived sources include detail for interpolated/extrapolated values

The system offers full control over how missing data is handled:
- `--allow-simulation`: Enable creation of artificial data when real data is unavailable
- `--allow-interpolation`: Enable filling gaps between known data points (on by default)
- `--skip-interpolation`: Prevent any interpolation, keeping only direct data
- `--offline-mode`: Use only cached data without download attempts

## Implementation Strategy

This pipeline is designed to run alongside the existing pipeline without interference:

- Uses separate directory structure
- Maintains compatibility with existing data
- Can eventually replace the original pipeline
- Preserves all data quality flags and processing standards
- Uses relative paths throughout for portability across systems

## Output Data

The extended pipeline produces:

- Enhanced DuckDB database with all original and new variables
- CSV exports with comprehensive variable coverage
- Variable-specific maps for visualization
- Detailed documentation of sources and methodologies

## Extended Variable Dictionary

See `extended_variable_dictionary.md` for details on all new variables including units, sources, and time ranges.

## Progress Tracking

Current implementation status is tracked in `implementation_status.md`.

## Dependencies

Additional R packages required:
- `httr` and `jsonlite` for API access
- `sf` for spatial operations
- `tidygeocoder` for geocoding operations
- `readxl` for Excel file processing