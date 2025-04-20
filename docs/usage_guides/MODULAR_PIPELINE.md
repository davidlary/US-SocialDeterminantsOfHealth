# Modular Pipeline Usage Guide

This guide provides detailed instructions for using the modular SDOH pipeline. The modular architecture offers greater flexibility, maintainability, and extensibility compared to the traditional monolithic pipeline.

## Overview

The modular pipeline breaks down the SDOH data processing into six core modules:

1. **Core Module** - Basic utilities and initialization
2. **Crosswalk Module** - Variable definitions and metadata
3. **Data Fetching Module** - Retrieving data from sources
4. **Database Module** - Creating and managing the database
5. **Maps Module** - Generating visualizations
6. **Documentation Module** - Creating and updating documentation

## Getting Started

### Running the Full Pipeline

To run the complete pipeline with default settings:

```bash
cd /path/to/US-SocialDeterminantsOfHealth/R
Rscript unified_sdoh_pipeline_modular.r
```

### Configuration Options

You can modify the pipeline behavior by editing the `options` list in the `unified_sdoh_pipeline_modular.r` file:

```r
options <- list(
  # Output and data directories
  root_dir = getwd(),
  data_dir = "data",
  output_dir = "output",
  logs_dir = "logs",
  
  # Database configuration
  db_path = "output/us_county_sdoh_unified.duckdb",
  overwrite_db = FALSE,
  
  # Data refresh options
  refresh_cache = FALSE,
  max_data_age_days = 30,
  
  # Processing options
  parallel = TRUE,
  cores = parallel::detectCores() - 1,
  min_cores = 2,
  
  # Map generation options
  generate_maps = TRUE,
  conus_only = TRUE,
  
  # Year range
  min_year = 1970,
  max_year = 2025,
  
  # Documentation options
  update_documentation = TRUE
)
```

### Running Specific Modules

You can also run individual modules directly for development or debugging:

```bash
# Run just the crosswalk builder
Rscript pipeline_modules/module_crosswalk.r

# Run just the map generation
Rscript pipeline_modules/module_maps.r
```

Note that some modules require input from other modules to function properly.

## Extending the Pipeline

### Adding New Variables

To add new variables to the pipeline:

1. Edit the `build_unified_crosswalk` function in `consolidate_crosswalks.r`
2. Add new variables to the appropriate domain section
3. Run the crosswalk module to validate your changes

Example of adding a new variable:

```r
# Add to the appropriate domain section in consolidate_crosswalks.r
new_variable <- tibble::tribble(
  ~variable_name, ~domain, ~description, ~type, ~units,
  "my_new_variable", "My Domain", "Description of the variable", "numeric_percent", "percent"
)

# Add metadata
new_variable <- new_variable %>%
  mutate(
    source = "Data Source Name",
    min_year = 2000,
    max_year = 2023,
    extended_only = TRUE,
    data_quality_flag_required = TRUE
  )

# Add to all_variables
all_variables <- bind_rows(all_variables, new_variable)
```

### Creating New Modules

To create a new module:

1. Create a new R script in the `pipeline_modules/` directory
2. Follow the module template pattern (include core functions, error handling, etc.)
3. Add the module to the main pipeline script

## Troubleshooting

### Common Issues

1. **Database Connection Errors**
   - Ensure the database path is correct
   - Check that DuckDB is installed
   - Verify that no other process has locked the database file

2. **Missing Data**
   - Check that API credentials are properly set
   - Verify that cache directories exist
   - Ensure the necessary data files are available

3. **Map Generation Failures**
   - Verify that shapefile data is available
   - Check for column name mismatches between database and shapefile
   - Ensure required R packages (sf, ggplot2) are installed

### Diagnosing Problems

For detailed diagnostics, check the log files in the `logs/` directory. Each pipeline run creates a timestamped log file with detailed information about every step.

## Performance Optimization

### Parallel Processing

The pipeline supports parallel processing to speed up data fetching and processing. Adjust the `cores` parameter in the options list to control the number of cores used.

### Caching Strategies

For better performance and offline usage:

1. Run `Rscript cache_sdoh_data.r` to pre-cache all data sources
2. Set `refresh_cache = FALSE` in the options to use cached data
3. Set `max_data_age_days` to control when cached data is considered stale

## Advanced Usage

### Custom Data Sources

To add a custom data source:

1. Create a new data fetcher function in `module_data_fetching.r`
2. Add the necessary variables to the crosswalk
3. Modify the database schema if needed to accommodate the new data

### Database Querying

The modular pipeline creates a normalized database structure that can be queried directly:

```r
library(DBI)
library(duckdb)

# Connect to the database
con <- dbConnect(duckdb::duckdb(), "output/us_county_sdoh_unified.duckdb")

# Get all data for a specific variable across all counties and years
result <- dbGetQuery(con, "
  SELECT 
    c.geoid, 
    c.name, 
    d.year, 
    d.value
  FROM counties c
  JOIN sdoh_data d ON c.geoid = d.geoid
  WHERE d.variable_name = 'median_household_income'
  ORDER BY c.name, d.year
")

# Close the connection
dbDisconnect(con)
```

## Contributing

When contributing to the modular pipeline:

1. Follow the established module pattern
2. Maintain backward compatibility where possible
3. Document all functions and parameters
4. Add appropriate error handling
5. Update the relevant documentation

## References

- [Main README](../../README.md)
- [Data Dictionary](../DATA_DICTIONARY.md)
- [Traffic Safety Guide](../TRAFFIC_SAFETY_GUIDE.md)