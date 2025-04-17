# Data Directory Structure

This directory contains the data files for the US Social Determinants of Health Dataset.

## Directory Organization

```
data/
├── cache/                    # Cached data files to improve performance
├── cdc_places/              # CDC PLACES county-level health data
├── census_historical/       # Historical census data files
├── epa/                     # Environmental Protection Agency data
│   ├── aqs/                 # Air Quality System data
│   ├── echo/                # Enforcement and Compliance History data
│   ├── ejscreen/           # Environmental Justice Screening data
│   └── tri/                # Toxic Release Inventory data
├── ihme/                    # Institute for Health Metrics and Evaluation data
│   ├── CSV/                # CSV files with life expectancy estimates
│   ├── Docs/               # Documentation for IHME data
│   └── Zip/                # Original zip archives (if needed)
├── nhgis/                   # IPUMS NHGIS (National Historical Geographic Information System) data
│   └── [symbolic links to ihme/CSV/*.zip files]
├── seer/                    # NCI SEER (Surveillance, Epidemiology, and End Results) data
├── shapefiles/             # County boundary shapefiles for different years
│   ├── counties_1990/      # 1990 Census county boundaries
│   ├── counties_2000/      # 2000 Census county boundaries
│   ├── counties_2010/      # 2010 Census county boundaries
│   └── counties_2020/      # 2020 Census county boundaries
├── usda_food_atlas/        # USDA Food Environment Atlas data
└── last_update.txt         # Timestamp of last data update
```

## Symbolic Links

This project uses symbolic links between directories to save space and maintain compatibility with the original pipeline:

- Files in the `nhgis/` directory are symbolic links to files in the `ihme/CSV/` directory
- This preserves the original directory structure expected by the pipeline code
- Windows users may need special permissions to create symbolic links (run as Administrator or enable Developer Mode)

## Data Sources and Update Frequency

| Directory | Source | Update Frequency | Last Updated | Documentation |
|-----------|--------|------------------|--------------|---------------|
| cdc_places | CDC PLACES | Annual | April 2025 | [CDC PLACES Documentation](https://www.cdc.gov/places) |
| census_historical | US Census Bureau | Static (Historical) | N/A | Census Bureau Technical Documentation |
| epa | US EPA | Quarterly | January 2025 | [EPA Data Catalog](https://www.epa.gov/data) |
| ihme | IHME | Annual | February 2025 | [IHME Data Catalog](https://www.healthdata.org/data-tools-practices/data-catalog) |
| nhgis | IPUMS NHGIS | Quarterly | March 2025 | [NHGIS Documentation](https://www.nhgis.org/) |
| seer | NCI SEER | Annual | December 2024 | [SEER Documentation](https://seer.cancer.gov/) |
| shapefiles | US Census Bureau | Decennial + Annual Updates | 2020 (Decennial) | [Census TIGER Documentation](https://www.census.gov/programs-surveys/geography/technical-documentation/complete-technical-documentation/tiger-geo-line.html) |
| usda_food_atlas | USDA ERS | Annual | March 2025 | [Food Environment Atlas](https://www.ers.usda.gov/data-products/food-environment-atlas/) |

## File Formats

- CSV: Comma-separated values text files
- ZIP: Compressed archives containing data files
- SHP/DBF/SHX/PRJ: ESRI Shapefile components for geographic data
- XLS/XLSX: Microsoft Excel spreadsheets
- TXT: Plain text files (typically with documentation)

## Working with Large Files

Some data files in this directory are quite large. To improve performance:

1. The pipeline uses a caching system that stores processed data in the `cache/` directory
2. For the largest datasets (e.g., NHGIS, EPA AQS), files are processed in chunks
3. Parallel processing is implemented to speed up data loading and transformation
4. Some very large original files are excluded from the Git repository (see .gitignore)

## Adding New Data Sources

When adding new data sources:

1. Create a new subdirectory with an appropriate name
2. Include a README.md file in the subdirectory explaining the data source
3. Add documentation in the appropriate format
4. Update the `last_update.txt` file with the current timestamp
5. Run the tests to ensure the pipeline can access the new data

## Data Security and Privacy

This dataset does not contain personally identifiable information (PII). All data is aggregated at the county level, with appropriate statistical disclosure controls applied by the original data sources.

## Error Reporting

If you encounter issues with any data files, please report them by creating an issue in the GitHub repository.
EOL < /dev/null