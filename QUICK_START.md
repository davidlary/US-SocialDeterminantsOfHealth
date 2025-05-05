# Quick Start Guide: Social Determinants of Health Pipeline

This quick start guide provides the most essential commands to run the pipeline correctly with all 255 variables.

## Running the Complete Pipeline

The pipeline has been fully fixed and enhanced to include all 255 variables, including traffic safety variables. To run the complete pipeline with all enhancements:

```bash
Rscript unified_sdoh_pipeline.r
```

This will:
- Build the variable crosswalk
- Fetch data from all sources (including traffic safety data)
- Process all 255 variables 
- Create a fully populated database
- Generate maps for all variables

## Verify Results

Check that all variables are in the database:
```bash
# Verify all 255 variables are in the database
Rscript check_database.r

# Verify traffic safety variables specifically
Rscript check_traffic_safety_variables.r

# Check map generation
ls -l output/maps/by_variable/ | wc -l
```

## Specific Commands

If you need to restart from a specific step:

```bash
# Restart from the database creation
Rscript unified_sdoh_pipeline.r --restart-from=database

# Restart from map generation
Rscript unified_sdoh_pipeline.r --restart-from=maps
```

If you encounter any issues, use the force-full-rebuild flag:

```bash
Rscript unified_sdoh_pipeline.r --force-full-rebuild
```

## Need More Info?

See the detailed instructions in the following files:
- `HOW_TO_RUN_PIPELINE.md`: Step-by-step instructions for running the pipeline
- `FINAL_FIX_SUMMARY.md`: Summary of all fixes implemented
- `docs/TRAFFIC_SAFETY_IMPLEMENTATION.md`: Details on the traffic safety module
EOF < /dev/null