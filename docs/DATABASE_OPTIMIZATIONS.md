# Database Optimizations Guide

This document explains the database optimizations implemented in the Social Determinants of Health (SDOH) pipeline to improve query performance, reduce memory usage, and enable efficient incremental processing.

## Overview

The SDOH pipeline uses DuckDB for storing and querying county-level health and social data. Several optimizations have been implemented to ensure efficient operation across various system configurations:

> **Note:** For information on the robust upsert pattern used for handling primary key constraints and data updates, see the [Database Upsert Implementation Guide](DATABASE_UPSERT_IMPLEMENTATION.md).

1. **Resource-Aware Configuration** - Automatically detects available system resources (memory, CPU, disk) and configures database settings accordingly
2. **Sophisticated Indexing** - Creates optimized indices based on common query patterns
3. **Materialized Views** - Pre-computes common query results for faster access
4. **Memory-Mapped I/O** - Uses memory mapping for efficient data access with large datasets
5. **Adaptive Strategy Selection** - Chooses appropriate optimization strategies based on available resources

## Configuration Options

Database optimization settings can be configured in `config.yaml` under the `database.optimizations` section:

```yaml
database:
  # ... other database settings ...
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

## Resource Detection

The system automatically detects:

- **Memory**: Total and available system memory
- **CPU Cores**: Physical and logical cores, hyperthreading status
- **Disk Space**: Available space on the system
- **I/O Performance**: Estimated I/O speed through a quick benchmark

Based on these detected resources, the database adjusts its configuration for optimal performance.

## Indexing Strategies

The following indexing strategies are available:

1. **Minimal**: Basic indices only on primary key columns
2. **Standard**: Primary key indices plus common composite indices
3. **Comprehensive**: Standard indices plus covering indices for common queries
4. **Advanced**: Comprehensive indices plus specialized indices for complex queries

When set to `auto`, the system selects the appropriate strategy based on available resources.

## Materialized Views

Materialized views pre-compute and store the results of common queries. The following materialized views are implemented based on available memory:

1. **Latest Data** (≥4GB RAM): Stores the most recent data for each county and variable
2. **Data Coverage** (≥4GB RAM): Tracks county coverage percentages for each variable and year
3. **State Statistics** (≥8GB RAM): Pre-computes state-level aggregations
4. **County Pivoted Data** (≥16GB RAM): Creates a wide-format view with variables as columns

Materialized views are refreshed automatically when data is updated in incremental processing mode.

## Memory-Mapped I/O

Memory-mapped I/O improves performance for large datasets by allowing the database to access files directly through memory mapping rather than through file I/O calls. The system configures:

- **Memory Map Size**: Allocated based on available system memory (up to 50% of RAM)
- **Direct I/O**: Enabled for HDDs, disabled for SSDs
- **Checkpoint Threshold**: Configures when data is persisted to disk (based on disk space)

## Compression

Data compression is configured based on available disk space and CPU resources:

- **High Compression**: Used when disk space is limited (<200GB)
- **Medium Compression**: Used with plenty of CPU cores (≥8)
- **Light Compression**: Used with limited CPU resources

## Performance Tuning

Additional performance settings include:

- **Thread Configuration**: Set based on detected cores and hyperthreading status
- **Cache Size**: Configured based on available memory and I/O speed
- **Temporary Directory**: Custom location for temporary files
- **Query Optimization**: Tables are analyzed to generate statistics for query planning

## Using the Database Efficiently

### Recommended Query Patterns

For best performance:

1. Use the provided views and materialized views when possible
2. Filter by indexed columns (geoid, year, variable_name) in your WHERE clauses
3. For time-series analysis, use the `county_time_series` view
4. For latest data, use the `latest_data` view (or `latest_data_materialized` if available)

### Example Queries

```sql
-- Get latest value for a specific variable across all counties
SELECT geoid, name, state_name, value
FROM latest_data
WHERE variable_name = 'traffic_fatality_rate'
ORDER BY value DESC
LIMIT 10;

-- Time series for a specific county and variable
SELECT year, value
FROM county_time_series
WHERE geoid = '01001' AND variable_name = 'life_expectancy'
ORDER BY year;

-- State-level averages for a variable (using materialized view)
SELECT state_name, avg_value
FROM state_stats_materialized
WHERE variable_name = 'median_household_income' AND year = 2020
ORDER BY avg_value DESC;
```

## Troubleshooting

If you encounter performance issues:

1. Check the logs for warnings about memory constraints or view creation failures
2. Consider reducing the complexity of your queries
3. If using a resource-constrained system, set `materialized_views.enabled: false` in config
4. For large datasets on limited memory, reduce `cache_size_percent` to 10%
5. Set `indices.strategy: "standard"` on systems with very limited resources