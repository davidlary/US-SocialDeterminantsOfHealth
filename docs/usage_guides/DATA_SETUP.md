# Data Setup for US Social Determinants of Health Dataset

## Data Directory Structure

The following data directories have been set up:

```
R/data/
│
├── nhgis/                    # NHGIS data (symbolic links to IHME)
├── ihme/                     # IHME life expectancy data
│   ├── CSV/                  # CSV data files
│   └── Docs/                 # Documentation
├── shapefiles/               # County boundary files for mapping
├── usda_food_atlas/          # USDA Food Environment Atlas data
├── census_historical/        # Historical Census data
├── epa/                      # EPA environmental data
│   └── tri/                  # Toxic Release Inventory data
├── cdc_places/               # CDC PLACES health metrics
├── seer/                     # SEER cancer registry data
├── cache/                    # Cached processed data
└── README.md                 # Documentation about the data structure
```

## Data Files Copied

1. **County Shapefiles**: All county boundary files for 1990, 2000, 2010, and 2020
2. **USDA Food Atlas**: Food environment data files
3. **Census Historical**: Historical population data
4. **IHME Data**: Life expectancy data for years 2015-2019 (sample of the full dataset)
5. **NHGIS Symbolic Links**: Links to the IHME data files
6. **EPA Data**: Sample environmental data files (AQS, EJSCREEN, TRI)
7. **CDC PLACES**: Health metrics data at the county level
8. **SEER Data**: Cancer registry population data
9. **Cache Files**: Sample cached data for faster loading

## Symbolic Links

Symbolic links have been created in the `nhgis/` directory to point to the actual data files in the `ihme/CSV/` directory. This maintains the same structure as the original project.

## Notes for Users

- This is a sample of the complete dataset for demonstration purposes
- For full functionality, users may need to download additional data files from the original sources
- Symbolic links structure must be maintained for the code to work correctly
- The included sample data should be sufficient for testing the pipeline

## Next Steps

1. Run the `test_pipeline.r` script to verify the data setup
2. Install required R packages using `install_packages.r`
3. Run the main pipeline using `main_extended.r`
4. Test specialized features like ML forecasting
5. If needed, download additional data from the original sources to expand the dataset