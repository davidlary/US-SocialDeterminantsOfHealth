# Social Determinants of Health Documentation

This directory contains all documentation for the Social Determinants of Health dataset.

## Core Documentation

- [Data Dictionary](DATA_DICTIONARY.md) - Comprehensive list of all 98 variables across 10 domains, their sources, and available years
- [Technical Documentation](TECHNICAL_DOCUMENTATION.md) - Details on the data pipeline, scripts, and implementation
- [ML Forecasting Guide](ML_FORECASTING.md) - Documentation for machine learning forecasting features

## Data Source Documentation

- [Crime & Safety Data](data_sources/CRIME_DATA.md) - Information about FBI Uniform Crime Reports and Bureau of Justice Statistics data

## Setup and Usage Guides

- [Data Setup Guide](usage_guides/DATA_SETUP.md) - Guide for setting up the data pipeline
- [Setup Summary](usage_guides/SETUP_SUMMARY.md) - Quick reference for setup requirements

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
  - And other domain-specific fetchers

## Data Structure

The pipeline produces a DuckDB database with the following structure:

- **counties** table: County metadata and geographic information
- **variables** table: Variable metadata and descriptions
- **sdoh_data** table: Main data table with all time series data

Plus a set of views for easy analysis:
- **latest_county_data**: Most recent data by county
- **county_time_series**: Complete time series with county information
- Domain-specific views for focused analysis

## Getting Started

The main README in the project root has complete instructions for running the pipeline and using the dataset.