# SDOH Pipeline Configuration Guide

This guide explains how to configure the SDOH pipeline using the YAML configuration system. This allows you to separate code from data storage, making it easy to use external drives or network storage for large datasets.

## Basic Usage

The pipeline uses a `config.yaml` file in the project root directory by default. You can specify a different configuration file by passing its path as a command line argument:

```bash
Rscript unified_sdoh_pipeline_modular.r /path/to/my_config.yaml
```

## Configuration File Structure

The configuration file is structured in sections, each controlling different aspects of the pipeline:

### Directories

The `directories` section defines the code location and where data files are stored:

```yaml
directories:
  # Code root directory - where the R scripts are located
  root_dir: "/path/to/project/root"
  
  # Data storage directories - can be relative or absolute paths
  data_dir: "/path/to/data"
  output_dir: "/path/to/output"
  logs_dir: "/path/to/logs"
  cache_dir: "/path/to/data/cache"
  maps_dir: "/path/to/output/maps"
  visualizations_dir: "/path/to/output/visualizations"
```

- `root_dir`: **IMPORTANT** - This must point to where the R code files are located
- Other directories: These specify where data is stored and can be on different drives
- Paths can be absolute (recommended) or relative to the root_dir

### Network Paths

To store data on a separate drive or network storage, use the `network_paths` section:

```yaml
network_paths:
  data_dir: "/Volumes/ExternalDrive/SDOH/data"
  output_dir: "/Volumes/ExternalDrive/SDOH/output"
```

This will override the corresponding paths in the `directories` section.

### Database Configuration

Control database settings:

```yaml
database:
  db_name: "us_county_sdoh_unified.duckdb"
  db_path: "output/us_county_sdoh_unified.duckdb"
  overwrite_db: false
  optimizations:
    auto_detect_resources: true   # Automatically detect system resources
    memory_mapped_io: true        # Use memory-mapped I/O for large datasets
    indices:                      # Indexing strategy
      strategy: "auto"            # Options: "minimal", "standard", "comprehensive", "advanced", "auto"
      analyze_tables: true        # Run ANALYZE on tables for query optimization
    materialized_views:
      enabled: true               # Use materialized views when sufficient memory is available
      refresh_on_update: true     # Refresh materialized views when data is updated
      memory_threshold_gb: 4      # Minimum memory required for materialized views (GB)
    performance:
      compression: "auto"         # Options: "none", "light", "medium", "high", "auto"
      threads: "auto"             # Number of threads or "auto" to detect
      cache_size_percent: 20      # Percentage of available memory to use for cache
```

See [Database Optimizations](./DATABASE_OPTIMIZATIONS.md) for detailed documentation on these settings.

### Data Refresh Options

Control when and how data is refreshed:

```yaml
data_refresh:
  refresh_cache: false
  max_data_age_days: 30
```

### Processing Options

Configure processing behavior including parallelism and incremental mode:

```yaml
processing:
  parallel: true
  cores: 4  # Set to null to use automatic detection
  min_cores: 2
  incremental: true  # Enable incremental processing (only update new/changed data)
  force_full_rebuild: false  # Force full reprocessing of all data
```

The incremental processing option is particularly important for efficiency:
- When `incremental: true`, the pipeline will only process new or changed data, making subsequent runs much faster
- When `force_full_rebuild: true`, the pipeline will perform a full rebuild regardless of incremental setting
- You can override both settings via command line with `--incremental=TRUE|FALSE` and `--force-full-rebuild=TRUE`

### Maps and Visualization

Control map generation:

```yaml
maps:
  generate_maps: true
  conus_only: true
```

### Year Range

Define the years of data to process:

```yaml
years:
  min_year: 1970
  max_year: 2025
```

### Documentation Options

Control documentation generation:

```yaml
documentation:
  update_documentation: true
```

### API Credentials

Store API keys (these will be overridden by environment variables if set):

```yaml
api_keys:
  census_api_key: "your-census-api-key"
```

### IPUMS Credentials

Store IPUMS credentials (these will be overridden by environment variables if set):

```yaml
ipums:
  username: "your-ipums-username"
  password: "your-ipums-password"
```

### Traffic Safety Options

Configure traffic safety data:

```yaml
traffic_safety:
  use_fallback: false
  data_years: [2020, 2021, 2022]
```

## Environment Variables

The following environment variables will override settings in the configuration file:

- `CENSUS_API_KEY` - Census API key
- `IPUMS_USERNAME` - IPUMS username
- `IPUMS_PASSWORD` - IPUMS password

## Examples

### Using a Network Drive for Data Storage

```yaml
directories:
  root_dir: "/Users/username/Projects/SDOH"

network_paths:
  data_dir: "/Volumes/NetworkDrive/SDOH/data"
  output_dir: "/Volumes/NetworkDrive/SDOH/output"
```

### Minimal Configuration

```yaml
database:
  db_path: "output/sdoh_database.duckdb"

years:
  min_year: 2010
  max_year: 2022
```

### Full Refresh Configuration

```yaml
data_refresh:
  refresh_cache: true
  max_data_age_days: 0

database:
  overwrite_db: true
```

## Technical Details

The configuration system follows these principles:

1. Default values are provided for all settings
2. YAML configuration overrides defaults
3. Environment variables override YAML configuration
4. Command-line parameters override environment variables

The system resolves relative paths to absolute paths based on the `root_dir` setting.

## Troubleshooting

If you encounter issues with the configuration:

1. Check that the YAML syntax is valid
2. Ensure paths exist and are accessible
3. For network paths, verify network connectivity
4. Check file permissions for database and output directories