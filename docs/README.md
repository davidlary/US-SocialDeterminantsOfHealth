# Social Determinants of Health Documentation

This directory contains all documentation for the Social Determinants of Health dataset.

## Core Documentation

- [Data Dictionary](./DATA_DICTIONARY.md) - Comprehensive list of all 98 variables across 10 domains, their sources, and available years
- [ML Forecasting Guide](./ML_FORECASTING.md) - Documentation for machine learning forecasting features
- [Database Optimizations](./DATABASE_OPTIMIZATIONS.md) - Guide to database performance optimizations

## Data Source Documentation

- [Crime & Safety Data](./data_sources/CRIME_DATA.md) - Information about FBI Uniform Crime Reports and Bureau of Justice Statistics data

## Setup and Usage Guides

- [Data Setup Guide](./usage_guides/DATA_SETUP.md) - Guide for setting up the data pipeline
- [Setup Summary](./usage_guides/SETUP_SUMMARY.md) - Quick reference for setup requirements

## Main Repository

The main README file in the repository root contains:
- Complete overview of the dataset
- Installation instructions
- Database usage examples
- Citation information

See [Main README](./MAIN_README.md) for a copy of the root README.

## Script Overview

The R directory contains scripts organized by function:

- **Core Pipeline**:
  - `unified_sdoh_pipeline.r` - Main unified pipeline script (primary entry point)
  - `build_extended_crosswalk.r` - Variable standardization crosswalk
  - `process_extended_data.r` - Data processing utilities
  - `install_packages.r` - Package installation

- **Data Fetchers**:
  - `fetch_nhgis_data.r` - NHGIS historical census data
  - `fetch_extended_data.r` - Multiple core data sources
  - `fetch_crime_data.r` - FBI crime statistics