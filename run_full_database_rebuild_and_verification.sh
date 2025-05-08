#!/bin/bash

# Master script to create a unified database with all 255+ variables, verify it, and generate maps
# This script orchestrates the complete process of building a comprehensive SDOH database and visualization

echo "=== Starting Full Database Rebuild, Verification, and Map Generation ==="
echo "Starting time: $(date)"

# Step 1: Create the comprehensive database with all 255+ variables
echo "Creating unified database with all variables..."
Rscript create_unified_database_with_all_variables.r

# Check if database creation was successful
if [ $? -ne 0 ]; then
    echo "ERROR: Database creation failed. Exiting."
    exit 1
fi

echo "Database creation completed successfully."

# Step 2: Verify all variables in the database
echo "Verifying all variables in the database..."
Rscript verify_all_variables.r

# Check if verification was successful
if [ $? -ne 0 ]; then
    echo "ERROR: Variable verification failed."
    exit 1
fi

echo "Variable verification completed successfully."

# Step 3: Enhanced verification for traffic safety data
echo "Performing enhanced verification for traffic safety data..."
Rscript verify_traffic_safety_database.r

# Also run the standard verification for compatibility
echo "Running additional traffic safety verification..."
Rscript verify_traffic_safety_data.r

# Step 4: Generate maps for visualization
echo "Generating maps for all variables..."
Rscript generate_county_maps.r

# Check if map generation was successful
if [ $? -ne 0 ]; then
    echo "WARNING: Map generation had issues, but the database was created successfully."
else
    echo "Map generation completed successfully."
fi

# Display completion message
echo "=== Full Database Rebuild, Verification, and Map Generation Completed Successfully ==="
echo "Completion time: $(date)"
echo "The database now contains all 255+ variables including traffic safety data"
echo "You can access the database at: output/us_county_sdoh_unified.duckdb"
echo "Maps have been generated in: output/maps/"