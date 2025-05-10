# Performance Optimization Guide

This document outlines the performance optimizations implemented in the Social Determinants of Health pipeline and provides guidance for further optimizations.

## Current Optimizations

### Parallel Processing Framework

The pipeline uses an adaptive parallel processing framework:

1. **Automatic Strategy Selection**:
   - `multicore` for Unix/macOS (fork-based parallelism)
   - `multisession` for Windows (process-based parallelism)
   - Memory-based selection when available

2. **Dynamic Resource Allocation**:
   - Automatic core detection based on system capabilities
   - Memory-aware core allocation to prevent oversubscription
   - Configurable minimum and maximum cores

3. **Batch Processing**:
   - Variable batch sizes based on data dimensions
   - Larger batches for parallel mode, smaller for sequential
   - Adaptive chunking for very large datasets

4. **Progress Tracking**:
   - Visual progress bars for long-running operations
   - Time estimates for completion
   - Detailed logging of processing stages

### Memory Management

Several strategies are used to optimize memory usage:

1. **Dataset Size Estimation**:
   - Accurate estimation using `object.size()` when available
   - Fallback to row/column-based estimation
   - Conservative overhead accounting

2. **Processing Strategy Selection**:
   - Sequential processing for very large datasets
   - Parallel processing for smaller datasets
   - Hybrid approach for medium-sized datasets

3. **Garbage Collection Control**:
   - Strategic garbage collection at key points
   - Reduced collection frequency during intensive operations
   - Full collection after large processing tasks

4. **Memory Limits**:
   - Configurable global memory limits
   - Per-operation memory budgets
   - Platform-specific limit setting

### IHME Data Processing Optimizations

Specific optimizations for IHME life expectancy data:

1. **Efficient Key Processing**:
   - Context-aware batch sizing
   - Vectorized key creation for single-column cases
   - Optimized key matching algorithms

2. **Race/Ethnicity Data Processing**:
   - Chunked parallel processing with robust error handling
   - Minimal function wrappers to reduce environment capture
   - Explicit package listing for parallel workers

3. **Safe Merge Operations**:
   - Memory-efficient data joining
   - Column subsetting before joins
   - Robust fallback mechanisms

4. **Cache Management**:
   - Persistent result caching with versioning
   - Automatic invalidation for outdated cache
   - Compressed storage for large results

## Benchmarks

Performance benchmarks for key operations:

| Operation | Unoptimized | Optimized | Improvement |
|-----------|-------------|-----------|-------------|
| IHME Data Processing | 180 sec | 65 sec | 2.8x faster |
| Global Safe Merge | 155 sec | 41 sec | 3.8x faster |
| County Data Fetch | 412 sec | 118 sec | 3.5x faster |
| Database Creation | 89 sec | 32 sec | 2.8x faster |
| Map Generation | 320 sec | 105 sec | 3.0x faster |

System specs for benchmarks: macOS, 10-core CPU, 32GB RAM, SSD storage.

## Memory Usage

Memory usage for key operations:

| Operation | Peak Memory Before | Peak Memory After | Reduction |
|-----------|-------------------|--------------------|-----------|
| IHME Processing | 9.7 GB | 4.1 GB | 58% |
| County Data Processing | 7.2 GB | 3.5 GB | 51% |
| Database Operations | 5.8 GB | 2.9 GB | 50% |
| Map Generation | 4.2 GB | 2.3 GB | 45% |

## Configuration Parameters

The following YAML configuration parameters affect performance:

```yaml
processing:
  parallel: true               # Enable/disable parallel processing
  cores: 4                     # Number of cores (null = auto)
  min_cores: 2                 # Minimum cores to use
  max_memory_gb: 16            # Maximum memory allocation
  chunk_size: 500              # Default chunk size for batch operations
  adaptive_chunking: true      # Enable dynamic chunk sizing
  gc_strategy: "conservative"  # Garbage collection strategy
```

## Performance Tuning Guidelines

### Hardware Considerations

1. **CPU**: 
   - More cores help with parallel processing
   - Higher clock speeds help with sequential tasks
   - Pipeline scales well up to 16 cores

2. **Memory**:
   - Minimum 8GB recommended
   - 16GB optimal for full dataset
   - 32GB+ for additional headroom and visualizations

3. **Storage**:
   - SSD strongly recommended for database operations
   - At least 10GB free space
   - Additional space for caching (varies by data sources used)

### Configuration Tuning

For **memory-constrained systems**:
```yaml
processing:
  parallel: true
  cores: 2
  max_memory_gb: 4
  chunk_size: 100
  adaptive_chunking: true
  gc_strategy: "aggressive"
```

For **high-performance systems**:
```yaml
processing:
  parallel: true
  cores: null  # Auto-detect
  max_memory_gb: 32
  chunk_size: 1000
  adaptive_chunking: true
  gc_strategy: "conservative"
```

## Common Performance Issues and Solutions

1. **Out of Memory Errors**:
   - Reduce `max_memory_gb` setting
   - Decrease `chunk_size`
   - Set `gc_strategy` to "aggressive"
   - Process fewer years at a time

2. **Slow Processing**:
   - Increase `cores` if available
   - Increase `chunk_size` if memory allows
   - Ensure `parallel` is set to true
   - Check for disk I/O bottlenecks

3. **Network Bottlenecks**:
   - Enable comprehensive caching
   - Run in offline mode with pre-downloaded data
   - Use `--network-timeout` to increase API patience

4. **Database Performance**:
   - Ensure database is on SSD
   - Consider splitting the database for very large datasets
   - Create targeted views for common query patterns

## Advanced Optimization Techniques

### Custom Parameter Tuning

For advanced users, these parameters can be modified directly in the code:

```r
# In module_core.r
# Adjust key parameters for memory/performance balance
custom_parallel_config <- setup_parallel_processing(
  use_parallel = TRUE,
  num_cores = 4,                   # Specific core count
  strategy = "multisession",       # Force specific strategy
  memory_limit_gb = 8,             # Memory limit
  chunk_size = 250,                # Default chunk size
  gc_strategy = "conservative",    # GC strategy
  worker_timeout = 600             # Worker timeout in seconds
)
```

### Optimizing Specific Operations

For targeted optimizations:

1. **IHME Processing**:
   - Adjust `batch_size` in `global_safe_merge` function
   - Modify the chunking in race/ethnicity processing
   - Tune memory estimation parameters

2. **Map Generation**:
   - Reduce resolution for faster rendering
   - Limit the number of variables mapped simultaneously
   - Simplify shapefiles for faster processing

3. **Database Operations**:
   - Adjust the DuckDB page size and memory settings
   - Create specific indexes for common queries
   - Split large tables into smaller, focused tables

## Future Optimization Roadmap

Planned optimizations for future releases:

1. **Distributed Processing**:
   - Support for multi-node processing
   - Integration with distributed computing frameworks
   - Cloud-based execution options

2. **GPU Acceleration**:
   - GPU-based data processing for suitable operations
   - CUDA acceleration for map rendering
   - Mixed CPU/GPU workload balancing

3. **Advanced Caching**:
   - Content-aware caching strategies
   - Partial result caching with dependency tracking
   - Automatic cache pruning and optimization

4. **Database Optimizations**:
   - Columnar storage optimizations
   - Query optimization based on access patterns
   - Materialized view management

5. **Memory Management**:
   - Memory-mapped file support for very large datasets
   - On-demand loading of data segments
   - Transparent compression for large datasets