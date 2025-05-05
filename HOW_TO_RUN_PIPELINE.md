# How to Run the SDOH Pipeline

This guide explains how to run the Social Determinants of Health (SDOH) pipeline to ensure all 255 variables are loaded and maps are generated for each variable. The pipeline now includes enhanced traffic safety data integration.

## Running the Complete Pipeline

The pipeline has been fully fixed and enhanced. You can now run it directly without needing to apply any fixes first:

```bash
Rscript unified_sdoh_pipeline.r
```

This command will run the complete pipeline, which:
1. Builds the variable crosswalk with all 255 variables
2. Fetches data from multiple sources, including traffic safety data
3. Processes and combines the data
4. Creates a unified database with all 255 variables
5. Generates maps for all variables
6. Updates documentation

## Verifying the Results

After the pipeline completes, verify that all variables are properly included:

```bash
# Check that all 255 variables are in the database
Rscript check_database.r

# Verify traffic safety variables specifically
Rscript check_traffic_safety_variables.r
```

You should see that the database contains all 255 variables, including the traffic safety variables.

To verify that maps were generated for all variables, check the output directory:

```bash
ls -la output/maps/by_variable/ | wc -l
```

This should show approximately 255 files (one for each variable).

## Running Specific Steps

If you need to restart from a specific step, use the `--restart-from` flag:

```bash
# Restart from the database creation step
Rscript unified_sdoh_pipeline.r --restart-from=database

# Restart from the map generation step
Rscript unified_sdoh_pipeline.r --restart-from=maps
```

## Additional Flags

The pipeline supports several additional flags for specialized operations:

```bash
# Force complete rebuild of the database
Rscript unified_sdoh_pipeline.r --force-full-rebuild

# Force data refresh (download new data)
Rscript unified_sdoh_pipeline.r --force-update

# Ensure all 255 variables are processed
Rscript unified_sdoh_pipeline.r --process-all-variables
```

## Enhanced Traffic Safety Module

The traffic safety module has been enhanced to ensure proper integration of all traffic safety variables. To verify just the traffic safety implementation:

```bash
Rscript check_traffic_safety_variables.r
```

This will show detailed information about all traffic safety variables in the database.

## Database Optimization Features

The pipeline now includes several database optimization features:

1. **Batch Processing**: Large datasets are processed in batches to prevent memory issues
2. **Parallel Processing**: The traffic safety module supports parallel processing for faster execution
3. **Incremental Updates**: The database can be updated incrementally when new data becomes available
4. **Data Quality Tracking**: All data points include detailed quality information

## Troubleshooting

If you encounter issues:

1. **Database Issues**: Check the logs directory for detailed error messages
2. **Map Generation Issues**: Use `--restart-from=maps` to regenerate only the maps
3. **Missing Variables**: Use `--process-all-variables` to ensure all variables are included
4. **Traffic Safety Issues**: Check the traffic safety implementation with `check_traffic_safety_variables.r`

For persistent issues, you can force a full rebuild with:

```bash
Rscript unified_sdoh_pipeline.r --force-full-rebuild --process-all-variables
```

## Documentation

For more details, refer to:

- `FINAL_FIX_SUMMARY.md`: Complete summary of all fixes implemented
- `QUICK_START.md`: Essential commands for running the pipeline
- `docs/TRAFFIC_SAFETY_IMPLEMENTATION.md`: Detailed documentation of the traffic safety module
- `docs/DATABASE_OPTIMIZATIONS.md`: Information about database performance optimizations
EOF < /dev/null