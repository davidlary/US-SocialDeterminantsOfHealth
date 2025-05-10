# Incremental Processing Guide

This document explains the incremental processing feature in the Social Determinants of Health pipeline, which significantly improves performance for repeated runs.

## Overview

Incremental processing allows the pipeline to only process new or changed data, rather than reprocessing all data every time. This provides several key benefits:

1. **Faster Execution**: Subsequent pipeline runs are much faster
2. **Reduced Resource Usage**: Less CPU, memory, and disk I/O required
3. **Better for Automation**: Practical for daily/weekly scheduled runs
4. **Preserves Existing Data**: Previously processed data remains unchanged

## How Incremental Processing Works

The incremental processing system works through the following mechanisms:

1. **Metadata Tracking**: 
   - The pipeline maintains a metadata table in the database with information about previously processed data
   - This includes which variables and data sources have been processed, along with timestamps

2. **Change Detection**:
   - When incremental mode is enabled, the pipeline checks what data is already processed
   - Only new or changed data is processed and added to the database
   - Existing data for unchanged variables is preserved

3. **Database Operations**:
   - For new data, standard insert operations are used
   - For potentially changed data, "upsert" operations are used to update existing records
   - Data versioning tracks when records were last updated

## Enabling Incremental Processing

Incremental processing can be enabled in two ways:

### 1. Via Configuration File (config.yaml)

```yaml
processing:
  incremental: true  # Enable incremental processing
  force_full_rebuild: false  # Set to true to force a full rebuild
```

### 2. Via Command Line Parameters

```bash
# Enable incremental processing
Rscript R/unified_sdoh_pipeline.r --incremental=TRUE

# Force a full rebuild even in incremental mode
Rscript R/unified_sdoh_pipeline.r --incremental=TRUE --force-full-rebuild=TRUE
```

## When to Use Each Mode

### Incremental Mode (Default)

Use incremental mode (`incremental: true`) for:
- Regular pipeline runs where most data doesn't change
- Daily or weekly automated updates
- Adding new years of data to an existing database
- Normal production operation

### Full Rebuild Mode

Use full rebuild mode (`force_full_rebuild: true` or `incremental: false`) when:
- Making significant changes to processing logic
- After updating the variable crosswalk
- When data quality issues need to be addressed across all data
- When a fresh start is needed

## Monitoring and Verification

The pipeline provides detailed logging about incremental processing:

```
[2025-04-24 14:13:47] [ INFO ] Using INCREMENTAL processing mode - only updating new or changed data
[2025-04-24 14:13:48] [ INFO ] Found metadata for 255 previously processed variables
[2025-04-24 14:13:49] [ INFO ] Skipping 347891 already processed records for batch 3
[2025-04-24 14:14:02] [ INFO ] Processing mode: INCREMENTAL (only new/changed data processed)
```

You can verify which records were processed by examining:
1. The pipeline log files in the logs directory
2. The processing_metadata table in the database

## Performance Comparison

Typical performance improvements with incremental processing:

| Scenario | Full Processing | Incremental Processing | Improvement |
|----------|----------------|-----------------------|-------------|
| First run | 30 minutes | 30 minutes | - |
| Subsequent run (no changes) | 30 minutes | 2 minutes | 15x faster |
| Adding new year of data | 30 minutes | 5 minutes | 6x faster |
| Small data update | 30 minutes | 3 minutes | 10x faster |

## Implementation Details

The incremental processing system is implemented primarily in the database module:

1. **Metadata Table**: 
   ```sql
   CREATE TABLE IF NOT EXISTS processing_metadata (
     data_source VARCHAR,
     variable_name VARCHAR,
     min_year INTEGER,
     max_year INTEGER,
     record_count INTEGER,
     last_processed TIMESTAMP,
     data_version VARCHAR,
     PRIMARY KEY (data_source, variable_name)
   )
   ```

2. **Upsert Operations**:
   ```sql
   INSERT OR REPLACE INTO sdoh_data 
   SELECT * FROM temp_batch_data
   ```
   
   For more details on the upsert implementation, see the [Database Upsert Implementation Guide](DATABASE_UPSERT_IMPLEMENTATION.md).

3. **Metadata Updates**:
   - After processing, the metadata table is updated with information about the processed data
   - This includes timestamp and record counts
   - Variables already in the database but not in the current processing batch retain their metadata

## Limitations

Current limitations of the incremental processing system:

1. **Data Dependencies**: Changes in one variable don't automatically trigger reprocessing of dependent variables
2. **No Schema Evolution**: Changes to the database schema require a full rebuild
3. **Manual Override Needed**: Major code changes may require manually forcing a full rebuild
4. **Metadata Size**: The metadata table grows with the number of variables

## Future Enhancements

Planned improvements to the incremental processing system:

1. **Data Dependency Tracking**: Automatically reprocess dependent variables when source variables change
2. **Schema Migration**: Support changes to the database schema without full rebuilds
3. **Partial Rebuilds**: Allow rebuilding specific subsets of data
4. **Change Tracking**: More detailed tracking of what changed between runs
5. **Parallel Incremental Processing**: Better parallelism for incremental mode