# Temporal Interpolation in the SDOH Pipeline

This document describes the temporal interpolation functionality implemented in the SDOH pipeline to handle missing county-year combinations in the data.

## Overview

The temporal interpolation feature addresses gaps in time series data for counties by:

1. Identifying missing year-county combinations
2. Checking if values exist for years before and after the gap
3. Performing linear (or other) interpolation between the bracketing years
4. Flagging the interpolated values with a data quality indicator

## Implementation Details

The interpolation functionality is implemented in the `interpolate_temporal_gaps` function in the `handle_year_county_variation.r` file. This function:

- Works variable by variable across all counties
- Only interpolates gaps with valid data points on both sides
- Maintains data quality flags to track which values are interpolated
- Provides multiple interpolation methods (linear, spline, and Stineman)
- Respects configurable gap size limits to prevent excessive extrapolation

## Interpolation Methods

The function supports three interpolation methods:

1. **Linear Interpolation** (default): Simple straight-line interpolation between two points
2. **Spline Interpolation**: Uses cubic splines for smoother curves (requires at least 4 data points)
3. **Stineman Interpolation**: A specialized interpolation method that preserves monotonicity

## Data Quality Flags

All interpolated values are flagged with a data quality indicator to maintain transparency about data sources:

- `direct`: Original data from the source
- `interpolated`: Data created through temporal interpolation
- `missing`: Data that could not be interpolated

## Integration with the Pipeline

The temporal interpolation is integrated into the SDOH pipeline in `module_data_fetching.r`. It runs after all domains are loaded and merged, allowing it to interpolate across all variables in the dataset.

## Configuration Options

The interpolation function can be configured with several parameters:

- `method`: Interpolation method ("linear", "spline", or "stine")
- `min_gap_size`: Minimum gap size to interpolate (in years)
- `max_gap_size`: Maximum gap size to interpolate (in years)

## Testing and Validation

A dedicated test script (`test_temporal_interpolation.r`) is provided to validate the accuracy of the interpolation function. This script:

- Creates synthetic test data with known values
- Introduces missing values following realistic patterns
- Applies the interpolation function
- Measures the accuracy of interpolated values
- Generates visualizations comparing original and interpolated data

## Usage

To use the temporal interpolation functionality:

1. Make sure `handle_year_county_variation.r` is in the project directory
2. The pipeline will automatically use the interpolation function when processing data
3. You can run `test_temporal_interpolation.r` to validate the interpolation accuracy

## Example

Here's a simple example of how the interpolation works:

For a county that has data for years 2000 and 2005, but missing data for years 2001-2004:

| Year | Value    | Data Quality |
|------|----------|--------------|
| 2000 | 100.0    | direct       |
| 2001 | [missing] | missing      |
| 2002 | [missing] | missing      |
| 2003 | [missing] | missing      |
| 2004 | [missing] | missing      |
| 2005 | 150.0    | direct       |

After interpolation:

| Year | Value    | Data Quality |
|------|----------|--------------|
| 2000 | 100.0    | direct       |
| 2001 | 110.0    | interpolated |
| 2002 | 120.0    | interpolated |
| 2003 | 130.0    | interpolated |
| 2004 | 140.0    | interpolated |
| 2005 | 150.0    | direct       |