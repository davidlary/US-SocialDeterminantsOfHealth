# Social Determinants of Health Dataset - R Code Directory

This directory contains the R scripts for the Social Determinants of Health Dataset project.

Please see the main [README.md](../README.md) in the parent directory for complete documentation.

## Quick Start

```bash
# Install required packages
Rscript install_packages.r

# Run the unified pipeline
Rscript unified_sdoh_pipeline.r

# Run with verbose output
Rscript unified_sdoh_pipeline.r --verbose
```

## Script Overview

- `unified_sdoh_pipeline.r` - Main pipeline script that orchestrates the data collection and processing
- `fetch_*.r` files - Data fetchers for different sources (e.g., Census, NHGIS, FBI crime data)
- `build_extended_crosswalk.r` - Creates variable mapping across different data sources
- `process_extended_data.r` - Processes raw data into standardized format

## Documentation

For comprehensive documentation, including:
- Complete data dictionary
- Detailed source information
- Usage examples

Please see the documentation in the [/docs](../docs/) directory.