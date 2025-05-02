# Database Upsert Implementation

This document explains the robust upsert approach implemented in the Social Determinants of Health (SDOH) pipeline database module to handle primary key constraints more effectively.

## Overview

The database module now uses an atomic "upsert" pattern with `INSERT OR REPLACE` operations instead of the previous approach of separate `DELETE` followed by `INSERT` operations. This change brings several important benefits:

1. **Atomic Operations**: Ensures data consistency by making updates in a single operation
2. **Improved Performance**: More efficient than separate delete-then-insert operations
3. **Better Handling of Primary Key Constraints**: Eliminates primary key violation errors
4. **Reduced Locking**: Minimizes database locking during updates
5. **Simpler Code**: More straightforward implementation with fewer potential failure points

## Implementation Details

The upsert pattern is implemented through the following approach:

### 1. Using Temporary Tables and INSERT OR REPLACE

For each batch of data to be inserted or updated:

```r
# Create temp table with new data
temp_counties <- paste0("temp_counties_", format(Sys.time(), "%H%M%S"))
dbWriteTable(con, temp_counties, unique_counties, temporary = TRUE)

# Use INSERT OR REPLACE for atomic upsert
dbExecute(con, paste0("INSERT OR REPLACE INTO counties SELECT * FROM ", temp_counties))

# Clean up temp table
dbExecute(con, paste0("DROP TABLE IF EXISTS ", temp_counties))
```

This pattern is used throughout the database module for all table updates.

### 2. Comparison with Previous Approach

#### Previous Approach (DELETE then INSERT):
```r
# Clear existing data first
dbExecute(con, "DELETE FROM counties WHERE geoid IN (...)")

# Insert new data
dbWriteTable(con, "counties", unique_counties, append = TRUE)
```

#### Issues with Previous Approach:
- Not atomic: If process interrupts between DELETE and INSERT, data is lost
- Less efficient: Two separate operations instead of one
- Primary key errors: Could still occur if primary keys weren't properly handled
- More locks: Holds locks longer across separate operations
- Transaction complexity: Required transaction wrapping to be safe

### 3. Benefits of the New Approach

- **Atomicity**: Either the entire upsert succeeds or fails, preventing partial updates
- **Performance**: Single operation is more efficient than two separate ones
- **Robustness**: Automatically handles primary key constraints correctly
- **Consistency**: Data is always in a consistent state even during updates
- **Simplicity**: Simpler pattern that's easier to maintain

## When Upsert Operations Occur

The upsert pattern is used in several key places in the database module:

1. **County Data**: When adding or updating county information
2. **Variable Definitions**: When adding or updating variable metadata
3. **SDOH Data**: When adding or updating the main data values
4. **Processing Metadata**: When updating processing status information

## Effect on Incremental Processing

The upsert approach particularly benefits incremental processing:

- Allows efficient updates of only changed data
- Preserves existing data that hasn't changed
- Makes database operations more reliable during partial updates
- Improves performance for large datasets with small changes

## DuckDB-Specific Implementation

This implementation takes advantage of DuckDB's support for the SQL standard `INSERT OR REPLACE` syntax, which automatically:

1. Checks if a record with the same primary key exists
2. If it exists, replaces it with the new values
3. If it doesn't exist, inserts a new record

## Best Practices

When working with the database module, follow these best practices:

1. Always use the upsert pattern (temp table + INSERT OR REPLACE) for updates
2. Ensure primary keys are properly defined on all tables
3. Use unique identifiers for temporary tables to avoid collisions
4. Always clean up temporary tables after use
5. For very large datasets, consider batch processing with multiple upserts

## Future Enhancements

Potential future improvements to the upsert implementation:

1. **Diff-Based Updates**: Only update changed columns rather than entire rows
2. **Advanced Conflict Resolution**: More sophisticated handling of conflicts
3. **Audit Trail**: Track history of changes through upserts
4. **Parallelized Upserts**: Perform multiple upserts in parallel for better performance