# FARS Data Directory

This directory contains pre-downloaded data from the NHTSA Fatality Analysis Reporting System (FARS).

## Data File Instructions

1. Download files from one of these sources:
   - NHTSA FARS downloads: https://www.nhtsa.gov/crash-data-systems/fatality-analysis-reporting-system
   - NHTSA FARS FTP: https://www.nhtsa.gov/file-downloads?p=nhtsa/downloads/FARS/
   - NHTSA Crash Data Resource: https://crashstats.nhtsa.dot.gov/

2. Save files in this directory with names following these patterns:
   - FARS_YYYY.csv
   - YYYY_FARS_data.csv
   - FARS_YYYY_county.csv

3. Data files can be either .csv or .xlsx format

## Note
Having pre-downloaded data here ensures the pipeline can run even when the FARS APIs are unavailable.
EOF < /dev/null