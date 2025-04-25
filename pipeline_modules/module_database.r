#!/usr/bin/env Rscript

# module_database.r
# Database management module for the SDOH pipeline

# Load required packages
library(dplyr)
library(DBI)
library(duckdb)
library(tidyr)

#' Create or update the unified SDOH database
#'
#' This function creates a unified DuckDB database containing all
#' SDOH variables and their data, using a proper normalized schema.
#'
#' @param processed_data Combined dataset with all processed data
#' @param crosswalk The variable crosswalk table
#' @param db_path Path to save the DuckDB database
#' @param overwrite Whether to overwrite an existing database
#' @param incremental Whether to use incremental processing (only process new/changed data)
#' @param force_full_rebuild Force full rebuild regardless of incremental settings
#' @param data_sources List of data sources that were processed in this run
#' @param processed_years Range of years that were processed in this run
#' @return TRUE if successful, FALSE otherwise
create_unified_database <- function(processed_data, 
                                  crosswalk, 
                                  db_path = "output/us_county_sdoh_unified.duckdb",
                                  overwrite = FALSE,
                                  incremental = FALSE,
                                  force_full_rebuild = FALSE,
                                  data_sources = NULL,
                                  processed_years = NULL) {
  
  log_message("\nSTEP 4: CREATING UNIFIED DATABASE", 
             level = "INFO", show_console = TRUE)
  
  # Create the database directory if it doesn't exist
  db_dir <- dirname(db_path)
  if (!dir.exists(db_dir)) {
    dir.create(db_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Remove existing database if overwrite is TRUE
  if (file.exists(db_path) && overwrite) {
    file.remove(db_path)
    log_message(paste("Removed existing database:", db_path),
               level = "INFO", show_console = TRUE)
  }
  
  # Get the full path to the database
  unified_db_path <- db_path
  log_message(paste("Database path:", unified_db_path),
             level = "INFO", show_console = TRUE)
             
  # Determine whether to use incremental mode
  use_incremental <- incremental && file.exists(unified_db_path) && !force_full_rebuild && !overwrite
  
  if (use_incremental) {
    log_message("Using INCREMENTAL processing mode - only updating new or changed data",
               level = "INFO", show_console = TRUE)
  } else {
    if (force_full_rebuild) {
      log_message("Force full rebuild specified - using FULL processing mode",
                 level = "INFO", show_console = TRUE)
    } else if (overwrite) {
      log_message("Overwrite specified - using FULL processing mode",
                 level = "INFO", show_console = TRUE)
    } else if (!file.exists(unified_db_path)) {
      log_message("Database does not exist yet - using FULL processing mode",
                 level = "INFO", show_console = TRUE)
    } else if (!incremental) {
      log_message("Incremental processing disabled - using FULL processing mode",
                 level = "INFO", show_console = TRUE)
    }
  }
  
  # Determine optimal database settings based on available system resources
  optimize_db_settings <- function() {
    # Determine available memory
    memory_gb <- 8  # Default assumption: 8GB
    total_memory_gb <- 8  # Default for total system memory
    
    # Try different methods to get system memory
    if (requireNamespace("pryr", quietly = TRUE)) {
      try({
        # Use pryr if available
        memory_bytes <- pryr::mem_used()
        total_memory_bytes <- pryr::mem_total()
        memory_gb <- memory_bytes / (1024^3)
        total_memory_gb <- total_memory_bytes / (1024^3)
      }, silent = TRUE)
    } else if (.Platform$OS.type == "windows") {
      try({
        # Windows-specific method for total memory
        memory_info <- system("wmic ComputerSystem get TotalPhysicalMemory /Value", intern = TRUE)
        total_memory_gb <- as.numeric(gsub("TotalPhysicalMemory=", "", memory_info[2])) / (1024^3)
        
        # Windows-specific method for available memory
        memory_info <- system("wmic OS get FreePhysicalMemory /Value", intern = TRUE)
        memory_gb <- as.numeric(gsub("FreePhysicalMemory=", "", memory_info[2])) / (1024^2)
      }, silent = TRUE)
    } else if (Sys.info()["sysname"] == "Darwin") {
      try({
        # MacOS-specific method
        memory_info <- system("sysctl -n hw.memsize", intern = TRUE)
        total_memory_gb <- as.numeric(memory_info) / (1024^3)
        
        # Available memory is more complex on macOS, use a percentage of total
        memory_gb <- total_memory_gb * 0.7  # Assume 70% available as a conservative estimate
      }, silent = TRUE)
    } else if (Sys.info()["sysname"] == "Linux") {
      try({
        # Linux method
        memory_info <- system("free -g", intern = TRUE)
        memory_lines <- strsplit(memory_info, "\n")[[1]]
        total_line <- memory_lines[2]
        total_parts <- strsplit(total_line, "\\s+")[[1]]
        total_memory_gb <- as.numeric(total_parts[2])
        memory_gb <- as.numeric(total_parts[7])  # Available memory
      }, silent = TRUE)
    }
    
    # Determine disk space and I/O capabilities
    disk_gb <- 500  # Default assumption: 500GB
    io_speed <- "moderate"  # Default assumption: moderate I/O speed
    
    try({
      # Get information about available disk space
      if (.Platform$OS.type == "windows") {
        # Windows method
        disk_info <- system("wmic logicaldisk get freespace,name /format:list", intern = TRUE)
        drive_letter <- substr(getwd(), 1, 2)
        for (line in disk_info) {
          if (grepl(paste0("Name=", drive_letter), line)) {
            free_space_line <- disk_info[which(grepl("FreeSpace=", disk_info))]
            disk_gb <- as.numeric(gsub("FreeSpace=", "", free_space_line)) / (1024^3)
            break
          }
        }
      } else if (Sys.info()["sysname"] == "Darwin" || Sys.info()["sysname"] == "Linux") {
        # MacOS/Linux method
        disk_info <- system("df -k .", intern = TRUE)
        disk_parts <- strsplit(disk_info[2], "\\s+")[[1]]
        disk_gb <- as.numeric(disk_parts[4]) / (1024^2)
      }
      
      # Try to estimate I/O speed (if possible)
      io_test_file <- tempfile()
      io_test_size <- 100 * 1024^2  # 100MB test file
      
      # Create a test file
      set.seed(42)  # For reproducibility
      test_data <- runif(io_test_size / 8)  # 8 bytes per numeric value
      
      # Write test
      write_start <- Sys.time()
      saveRDS(test_data, io_test_file)
      write_end <- Sys.time()
      write_time <- as.numeric(difftime(write_end, write_start, units = "secs"))
      
      # Read test
      read_start <- Sys.time()
      readRDS(io_test_file)
      read_end <- Sys.time()
      read_time <- as.numeric(difftime(read_end, read_start, units = "secs"))
      
      # Clean up
      unlink(io_test_file)
      
      # Calculate I/O speed in MB/s
      write_speed <- 100 / write_time  # MB/s
      read_speed <- 100 / read_time    # MB/s
      avg_speed <- (write_speed + read_speed) / 2
      
      # Classify I/O speed
      if (avg_speed > 300) {
        io_speed <- "very_fast"  # NVMe SSD or similar
      } else if (avg_speed > 100) {
        io_speed <- "fast"  # SATA SSD
      } else if (avg_speed > 30) {
        io_speed <- "moderate"  # Fast HDD or slow SSD
      } else {
        io_speed <- "slow"  # Typical HDD
      }
    }, silent = TRUE)
    
    # Determine CPU cores and capabilities
    cpu_cores <- parallel::detectCores()
    if (is.na(cpu_cores)) cpu_cores <- 4  # Default to 4 if detection fails
    
    # Try to determine if hyperthreading is enabled (logical vs physical cores)
    hyperthreading <- FALSE
    physical_cores <- cpu_cores
    
    try({
      if (Sys.info()["sysname"] == "Darwin") {
        # MacOS
        physical_info <- system("sysctl -n hw.physicalcpu", intern = TRUE)
        logical_info <- system("sysctl -n hw.logicalcpu", intern = TRUE)
        physical_cores <- as.numeric(physical_info)
        logical_cores <- as.numeric(logical_info)
        hyperthreading <- logical_cores > physical_cores
      } else if (Sys.info()["sysname"] == "Linux") {
        # Linux
        cpu_info <- system("lscpu", intern = TRUE)
        for (line in cpu_info) {
          if (grepl("Core\\(s\\) per socket", line)) {
            cores_per_socket <- as.numeric(gsub(".*:\\s*", "", line))
          }
          if (grepl("Socket\\(s\\)", line)) {
            sockets <- as.numeric(gsub(".*:\\s*", "", line))
          }
          if (grepl("Thread\\(s\\) per core", line)) {
            threads_per_core <- as.numeric(gsub(".*:\\s*", "", line))
          }
        }
        if (exists("cores_per_socket") && exists("sockets") && exists("threads_per_core")) {
          physical_cores <- cores_per_socket * sockets
          hyperthreading <- threads_per_core > 1
        }
      } else if (.Platform$OS.type == "windows") {
        # Windows
        cpu_info <- system("wmic cpu get NumberOfCores,NumberOfLogicalProcessors /Value", intern = TRUE)
        for (line in cpu_info) {
          if (grepl("NumberOfCores", line)) {
            physical_cores <- as.numeric(gsub("NumberOfCores=", "", line))
          }
          if (grepl("NumberOfLogicalProcessors", line)) {
            logical_cores <- as.numeric(gsub("NumberOfLogicalProcessors=", "", line))
          }
        }
        if (exists("physical_cores") && exists("logical_cores")) {
          hyperthreading <- logical_cores > physical_cores
        }
      }
    }, silent = TRUE)
    
    # Calculate optimal settings
    # Memory settings based on available system resources
    memory_map_size <- min(total_memory_gb * 0.5, 32) * 1024^3  # Use up to 50% of RAM, max 32GB
    
    # Adjust threads based on workload types and hyperthreading
    if (hyperthreading) {
      # With hyperthreading, we want to avoid using all logical cores for CPU-bound tasks
      threads <- min(physical_cores + 2, cpu_cores)
    } else {
      # Without hyperthreading, we can use more cores
      threads <- min(cpu_cores - 1, 16)  # Use all cores minus 1, max 16
    }
    
    # Create a temp directory for database operations
    temp_directory <- file.path(tempdir(), "duckdb_temp")
    dir.create(temp_directory, recursive = TRUE, showWarnings = FALSE)
    
    # Cache size based on available memory and I/O speed
    # For faster I/O, we need less cache
    cache_multiplier <- switch(io_speed,
                              very_fast = 0.15,  # NVMe SSD - less cache needed
                              fast = 0.2,       # SATA SSD
                              moderate = 0.25,  # Fast HDD or slow SSD
                              slow = 0.3)       # Typical HDD - more cache needed
    
    cache_size <- min(total_memory_gb * cache_multiplier, 6) * 1024^3
    
    # Settings for materialized views based on available memory
    enable_materialized_views <- (total_memory_gb >= 4)  # Only use materialized views with >= 4GB RAM
    
    # More advanced view capabilities with more memory
    advanced_materialized_view_options <- list(
      count_distinct_views = (total_memory_gb >= 8),  # Enable views with COUNT DISTINCT with >= 8GB
      complex_join_views = (total_memory_gb >= 12),   # Enable views with complex joins with >= 12GB
      pivot_views = (total_memory_gb >= 16)           # Enable pivoted views with >= 16GB
    )
    
    # Determine optimal index settings based on disk space and memory
    index_strategy <- "standard"  # Default: create basic indices
    
    if (disk_gb > 200 && total_memory_gb >= 8) {
      index_strategy <- "comprehensive"  # Create comprehensive indices
    }
    if (disk_gb > 500 && total_memory_gb >= 16) {
      index_strategy <- "advanced"  # Create advanced indices including covering indices
    }
    
    # Adaptive settings based on available resources
    adaptive_settings <- list(
      checkpoint_threshold = if (disk_gb > 500) "2GB" else "1GB",
      workers_percentage = if (hyperthreading) 0.6 else 0.8,  # Percentage of cores to use
      memory_limit = memory_map_size
    )
    
    # Log the detected system configuration
    cat(sprintf("System configuration detected: %.1f GB RAM, %d CPU cores (%d physical%s), %.1f GB disk, %s I/O\n", 
                total_memory_gb, cpu_cores, physical_cores, 
                ifelse(hyperthreading, " with hyperthreading", ""),
                disk_gb, io_speed))
    
    return(list(
      memory_map_size = memory_map_size,
      threads = threads,
      temp_directory = temp_directory,
      cache_size = cache_size,
      enable_materialized_views = enable_materialized_views,
      advanced_view_options = advanced_materialized_view_options,
      index_strategy = index_strategy,
      adaptive_settings = adaptive_settings,
      system_resources = list(
        memory_gb = total_memory_gb,
        cpu_cores = cpu_cores,
        physical_cores = physical_cores,
        hyperthreading = hyperthreading,
        disk_gb = disk_gb,
        io_speed = io_speed
      )
    ))
  }
  
  # Get optimized settings
  db_settings <- optimize_db_settings()
  log_message(paste("Optimized database settings: memory_map=", 
                   round(db_settings$memory_map_size / 1024^3, 1), "GB, threads=", 
                   db_settings$threads),
             level = "INFO", show_console = TRUE)
  
  # Try to connect to the database with retry logic
  con <- NULL
  max_attempts <- 3
  attempt <- 1
  
  while (attempt <= max_attempts && is.null(con)) {
    tryCatch({
      # Try to connect to the database
      log_message(paste("Connecting to database (attempt", attempt, "of", max_attempts, ")..."),
                 level = "INFO", show_console = TRUE)
      
      # Connect with optimized settings
      con <- dbConnect(
        duckdb::duckdb(), 
        dbdir = unified_db_path,
        read_only = FALSE,
        n_threads = db_settings$threads
      )
      
      # Apply optimization settings based on detected system resources
      log_message("Applying optimized database settings...", 
                 level = "INFO", show_console = TRUE)
      
      # Core settings
      dbExecute(con, paste0("PRAGMA memory_limit='", db_settings$adaptive_settings$memory_limit, "'"))
      dbExecute(con, paste0("PRAGMA threads=", db_settings$threads))
      dbExecute(con, paste0("PRAGMA temp_directory='", db_settings$temp_directory, "'"))
      dbExecute(con, paste0("PRAGMA memory_map_size=", db_settings$memory_map_size))
      
      # Adaptive checkpoint threshold based on disk space
      dbExecute(con, paste0("PRAGMA checkpoint_threshold='", db_settings$adaptive_settings$checkpoint_threshold, "'"))
      
      # Sorting and ordering defaults
      dbExecute(con, "PRAGMA default_null_order='nulls_first'")
      dbExecute(con, "PRAGMA default_order='ASC'")
      
      # Cache settings based on I/O speed and available memory
      dbExecute(con, paste0("PRAGMA cache_size=", db_settings$cache_size))
      
      # Enable memory-mapped I/O for large datasets
      if (db_settings$system_resources$io_speed %in% c("fast", "very_fast")) {
        # For fast I/O (SSDs), enable aggressive memory mapping
        dbExecute(con, "PRAGMA use_direct_io=FALSE")  # Using buffered I/O with memory mapping is better for SSDs
        dbExecute(con, "PRAGMA force_checkpoint=WAL")  # Use WAL mode for better write performance
      } else {
        # For slower I/O (HDDs), be more conservative with memory mapping
        dbExecute(con, "PRAGMA use_direct_io=TRUE")  # Direct I/O can be better for HDDs in some cases
        dbExecute(con, "PRAGMA force_checkpoint=FULL")  # Use full checkpoints for data integrity
      }
      
      # Set optimal compression level based on CPU and disk space
      if (db_settings$system_resources$disk_gb < 200) {
        # Limited disk space - use higher compression
        dbExecute(con, "PRAGMA compression='high'")
      } else if (db_settings$system_resources$cpu_cores >= 8) {
        # Lots of CPU cores available - use medium compression
        dbExecute(con, "PRAGMA compression='medium'")
      } else {
        # Limited CPU - use light compression
        dbExecute(con, "PRAGMA compression='light'")
      }
      
      # Additional advanced settings for better performance
      dbExecute(con, "PRAGMA experimental_parallel_csv=true")  # Enable parallel CSV reading
      
      # For systems with lots of memory, enable more aggressive optimizations
      if (db_settings$system_resources$memory_gb >= 16) {
        dbExecute(con, "PRAGMA maximize_threads=true")  # More aggressive thread usage
        dbExecute(con, "PRAGMA memory_reuse=true")  # Allow memory reuse between queries
        dbExecute(con, "PRAGMA verify_external=false")  # Skip verification of external data
      }
      
      log_message(paste0("Successfully configured database with optimized settings (Memory: ", 
                       round(db_settings$system_resources$memory_gb, 1), "GB, ",
                       "Cores: ", db_settings$system_resources$cpu_cores, ", ",
                       "I/O: ", db_settings$system_resources$io_speed, ")"),
                 level = "INFO", show_console = TRUE)
    }, error = function(e) {
      log_message(paste("Database connection attempt", attempt, "failed:", conditionMessage(e)),
                 level = "WARN", show_console = TRUE)
      
      if (grepl("lock", conditionMessage(e), ignore.case = TRUE)) {
        # It's a lock issue - try a different path
        Sys.sleep(2)  # Wait 2 seconds
        new_path <- gsub("\\.duckdb$", paste0("_alt_", attempt, ".duckdb"), db_path)
        log_message(paste("Trying alternative database path:", new_path),
                   level = "INFO", show_console = TRUE)
        unified_db_path <<- new_path
        use_incremental <<- incremental && file.exists(unified_db_path) && !force_full_rebuild && !overwrite
      }
    })
    
    attempt <- attempt + 1
  }
  
  # If we couldn't connect to the database after all attempts, use in-memory
  if (is.null(con)) {
    log_message("ERROR: Failed to connect to database after multiple attempts. Using in-memory database.",
               level = "ERROR", show_console = TRUE)
    
    # Create an in-memory database as a last resort
    con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    use_incremental <- FALSE  # Can't use incremental with in-memory DB
  }
  
  # Store settings in a global variable for the module
  use_materialized_views <- db_settings$enable_materialized_views
  
  # Create metadata table to track processing status if it doesn't exist
  log_message("Setting up processing metadata tracking...",
             level = "INFO", show_console = TRUE)
             
  dbExecute(con, "
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
  ")
  
  # 1. Create counties table
  log_message("Creating counties table...", level = "INFO", show_console = TRUE)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS counties (
      geoid VARCHAR PRIMARY KEY,
      name VARCHAR,
      state_fips VARCHAR,
      state_name VARCHAR
    )
  ")
  
  # Extract unique counties from processed data
  # Check if state_fips and state_name are available
  columns_to_use <- c("geoid")
  if ("state_fips" %in% names(processed_data)) {
    columns_to_use <- c(columns_to_use, "state_fips")
  }
  if ("state_name" %in% names(processed_data)) {
    columns_to_use <- c(columns_to_use, "state_name")
  }
  
  # Select available columns
  unique_counties <- processed_data %>%
    select(all_of(columns_to_use)) %>%
    distinct()
  
  # Add missing columns if needed
  if (!"state_fips" %in% names(unique_counties)) {
    unique_counties$state_fips <- substr(unique_counties$geoid, 1, 2)
    log_message("Added state_fips column derived from geoid",
               level = "INFO", show_console = TRUE)
  }
  
  if (!"state_name" %in% names(unique_counties)) {
    # Create a state lookup table
    state_lookup <- data.frame(
      state_fips = sprintf("%02d", 1:56),
      state_name = c(state.name, "District of Columbia", 
                    "Puerto Rico", "Virgin Islands", 
                    "Guam", "American Samoa", "Northern Mariana Islands"),
      stringsAsFactors = FALSE
    )
    
    # Join to get state names
    unique_counties <- unique_counties %>%
      left_join(state_lookup, by = "state_fips")
    
    log_message("Added state_name column from lookup table",
               level = "INFO", show_console = TRUE)
  }
  
  # Add county name
  unique_counties$name <- paste("County", unique_counties$geoid)
  
  # Insert counties if they don't exist yet
  if (dbGetQuery(con, "SELECT COUNT(*) FROM counties")[1,1] == 0) {
    dbWriteTable(con, "counties", unique_counties, append = TRUE)
    log_message(paste("Added", nrow(unique_counties), "counties to database"),
               level = "INFO", show_console = TRUE)
  }
  
  # 2. Create variables table
  log_message("Creating variables table...", level = "INFO", show_console = TRUE)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS variables (
      variable_name VARCHAR PRIMARY KEY,
      domain VARCHAR,
      description VARCHAR,
      type VARCHAR,
      units VARCHAR,
      min_year INTEGER,
      max_year INTEGER,
      extended_only BOOLEAN
    )
  ")
  
  # Prepare variables for insertion
  variables_for_db <- crosswalk %>%
    select(variable_name, domain, description, type, units, min_year, max_year, extended_only)
  
  # Check for duplicate variable names which would violate the primary key
  duplicate_vars <- variables_for_db %>%
    group_by(variable_name) %>%
    filter(n() > 1) %>%
    ungroup()
  
  if (nrow(duplicate_vars) > 0) {
    log_message(paste("WARNING: Found", nrow(duplicate_vars), "duplicate variable names in crosswalk"),
               level = "WARN", show_console = TRUE)
    # De-duplicate
    variables_for_db <- variables_for_db %>%
      distinct(variable_name, .keep_all = TRUE)
    log_message("De-duplicated variables table before insertion",
               level = "INFO", show_console = TRUE)
  }
  
  # Clear existing variables first
  dbExecute(con, "DELETE FROM variables")
  
  # Insert the variables
  dbWriteTable(con, "variables", variables_for_db, append = TRUE)
  log_message(paste("Added", nrow(variables_for_db), "variables to database"),
             level = "INFO", show_console = TRUE)
  
  # 3. Create main data table in normalized form (tall/long format)
  log_message("Creating main SDOH data table...", level = "INFO", show_console = TRUE)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS sdoh_data (
      geoid VARCHAR,
      year INTEGER,
      variable_name VARCHAR,
      value DOUBLE,
      data_quality VARCHAR,
      data_source VARCHAR,
      data_vintage VARCHAR,
      interpolation_method VARCHAR,
      ci_lower DOUBLE,
      ci_upper DOUBLE,
      confidence_level DOUBLE,
      last_updated TIMESTAMP,
      PRIMARY KEY (geoid, year, variable_name)
    )
  ")
  
  # If we're in incremental mode, check what data we already have
  previously_processed_data <- NULL
  if (use_incremental) {
    log_message("Retrieving previously processed data information...",
               level = "INFO", show_console = TRUE)
    
    # Get metadata about previously processed data
    previously_processed_data <- dbGetQuery(con, "SELECT * FROM processing_metadata")
    
    if (nrow(previously_processed_data) > 0) {
      log_message(paste("Found metadata for", nrow(previously_processed_data), 
                       "previously processed variables"),
                 level = "INFO", show_console = TRUE)
    } else {
      log_message("No previously processed data found in metadata - will process all data",
                 level = "INFO", show_console = TRUE)
      # Force full processing if no metadata exists
      use_incremental <- FALSE
    }
  }
  
  # Process the data for insertion (convert from wide to long format)
  # First, identify the columns that need to be pivoted (excluding metadata)
  # Ensure we only include metadata columns that actually exist in the processed data
  metadata_cols <- c("geoid", "year")
  if ("state_fips" %in% names(processed_data)) {
    metadata_cols <- c(metadata_cols, "state_fips")
  }
  if ("state_name" %in% names(processed_data)) {
    metadata_cols <- c(metadata_cols, "state_name")
  }
  
  # Get all the variable names from the crosswalk
  var_names <- crosswalk$variable_name
  
  # Add interpolation and data quality flag columns if they exist
  flag_patterns <- c("_interpolated$", "_extended$", "data_quality_")
  flag_cols <- character(0)
  
  for (pattern in flag_patterns) {
    pattern_cols <- grep(pattern, names(processed_data), value = TRUE)
    flag_cols <- c(flag_cols, pattern_cols)
  }
  
  # Define columns to be pivoted (variable data columns)
  pivot_cols <- setdiff(names(processed_data), c(metadata_cols, flag_cols))
  pivot_cols <- intersect(pivot_cols, var_names)
  
  # Verify we have variable data to pivot
  if (length(pivot_cols) == 0) {
    stop("No variable data columns found in the processed data for pivoting")
  }
  
  log_message(paste("Converting", length(pivot_cols), "variables to long format..."),
             level = "INFO", show_console = TRUE)
  
  # Batch processing to avoid memory issues
  batch_size <- 50
  total_batches <- ceiling(length(pivot_cols) / batch_size)
  
  # Process in batches using parallel processing
  process_batch <- function(batch) {
    start_idx <- (batch - 1) * batch_size + 1
    end_idx <- min(batch * batch_size, length(pivot_cols))
    batch_cols <- pivot_cols[start_idx:end_idx]
    
    message(paste("Processing batch", batch, "of", total_batches, 
                 "(variables", start_idx, "to", end_idx, ")"))
    
    # Ensure metadata_cols only includes columns that exist
    available_metadata_cols <- intersect(metadata_cols, names(processed_data))
    
    # Pivot the batch of variables to long format
    long_data <- processed_data %>%
      select(all_of(c(available_metadata_cols, batch_cols))) %>%
      pivot_longer(
        cols = all_of(batch_cols),
        names_to = "variable_name",
        values_to = "value"
      ) %>%
      filter(!is.na(value)) # Only keep non-NA values
    
    # Check for duplicates in the raw data
    potential_duplicates <- long_data %>%
      group_by(geoid, year, variable_name) %>%
      filter(n() > 1) %>%
      ungroup()
    
    if (nrow(potential_duplicates) > 0) {
      # We found duplicates in the source data, log a warning
      warning_msg <- paste("WARNING: Found", nrow(potential_duplicates),
                         "duplicate key combinations in source data for batch", batch)
      message(warning_msg)
      
      # Log the first few duplicates
      duplicate_example <- potential_duplicates %>%
        distinct(geoid, year, variable_name) %>%
        head(3)
      
      for (i in 1:nrow(duplicate_example)) {
        dup_msg <- paste("   Duplicate:", 
                       paste0("geoid: ", duplicate_example$geoid[i], ", ",
                             "year: ", duplicate_example$year[i], ", ",
                             "variable_name: ", duplicate_example$variable_name[i]))
        message(dup_msg)
      }
      
      # De-duplicate the data (keep first occurrence)
      long_data <- long_data %>%
        distinct(geoid, year, variable_name, .keep_all = TRUE)
      
      message(paste("   De-duplicated data for batch", batch,
                  "- keeping one record per unique key combination"))
    }
    
    # Generate timestamp
    long_data$last_updated <- Sys.time()
    
    # Set other columns to NULL for now (will be updated later if flags exist)
    long_data$data_quality <- NA_character_
    long_data$data_source <- NA_character_
    long_data$data_vintage <- NA_character_
    long_data$interpolation_method <- NA_character_
    long_data$ci_lower <- NA_real_
    long_data$ci_upper <- NA_real_
    long_data$confidence_level <- NA_real_
    
    # Check for interpolation flags and add them
    for (var_name in batch_cols) {
      interp_col <- paste0(var_name, "_interpolated")
      if (interp_col %in% names(processed_data)) {
        var_rows <- long_data$variable_name == var_name
        
        # Join interpolation info
        interp_data <- processed_data %>%
          select(geoid, year, !!sym(interp_col)) %>%
          filter(!is.na(!!sym(interp_col)))
        
        if (nrow(interp_data) > 0) {
          # Create a lookup to efficiently update long_data
          interp_lookup <- interp_data %>%
            mutate(
              key = paste(geoid, year, sep = "_"),
              interp_value = !!sym(interp_col)
            )
          
          # Add a key to long_data for the lookup
          long_data <- long_data %>%
            mutate(key = ifelse(variable_name == var_name, 
                              paste(geoid, year, sep = "_"), NA))
          
          # Update interpolation method for matches
          for (i in 1:nrow(interp_lookup)) {
            current_key <- interp_lookup$key[i]
            current_value <- interp_lookup$interp_value[i]
            
            if (current_value) {
              # Update the interpolation method
              long_data$interpolation_method[long_data$key == current_key] <- "linear"
            }
          }
          
          # Remove the temporary key column
          long_data$key <- NULL
        }
      }
    }
    
    # Final safety check for duplicates before returning
    final_check <- long_data %>%
      group_by(geoid, year, variable_name) %>%
      filter(n() > 1) %>%
      ungroup()
    
    if (nrow(final_check) > 0) {
      warning_msg <- paste("CRITICAL: Still found", nrow(final_check),
                         "duplicates after processing batch", batch, "- forcibly de-duplicating")
      message(warning_msg)
      
      # Force de-duplication to prevent database errors
      long_data <- long_data %>%
        distinct(geoid, year, variable_name, .keep_all = TRUE)
    }
    
    return(long_data)
  }
  
  # Set up parallel processing for batches
  log_message("Setting up parallel processing for data transformation...",
             level = "INFO", show_console = TRUE)
  
  # Determine cores to use for database operations
  db_cores <- min(total_batches, parallel::detectCores() - 1)
  db_cores <- max(db_cores, 2)  # Use at least 2 cores
  
  # Use future for parallel batch processing
  future::plan(future::multisession, workers = db_cores)
  log_message(paste("Using", db_cores, "cores for database batch processing"),
             level = "INFO", show_console = TRUE)
  
  # Process batches in parallel
  batch_numbers <- 1:total_batches
  batch_results <- future.apply::future_lapply(batch_numbers, process_batch)
  
  # Process and insert each batch result
  for (batch in 1:total_batches) {
    # Calculate batch columns for this batch
    start_idx <- (batch - 1) * batch_size + 1
    end_idx <- min(batch * batch_size, length(pivot_cols))
    batch_cols <- pivot_cols[start_idx:end_idx]
    
    # Get batch result - already processed by the process_batch function
    long_data <- batch_results[[batch]]
    
    # In incremental mode, filter out already processed data that hasn't changed
    if (use_incremental && !is.null(previously_processed_data) && nrow(previously_processed_data) > 0) {
      # Start by marking the batch's variables and their sources
      batch_var_sources <- data.frame(
        variable_name = batch_cols,
        data_source = ifelse(
          grepl("^census_", batch_cols), "census",
          ifelse(grepl("^traffic_", batch_cols), "traffic_safety",
                 ifelse(grepl("^life_expectancy", batch_cols), "ihme",
                        "other")
          )
        ),
        stringsAsFactors = FALSE
      )
      
      # Get the variables in this batch that are already in the database
      vars_to_skip <- NULL
      for (i in 1:nrow(batch_var_sources)) {
        var_name <- batch_var_sources$variable_name[i]
        var_source <- batch_var_sources$data_source[i]
        
        # Check if this variable from this source is already processed
        var_metadata <- previously_processed_data %>%
          filter(variable_name == var_name & data_source == var_source)
        
        # If we have metadata for this variable
        if (nrow(var_metadata) > 0) {
          # Check if we have a data_version to compare
          current_version <- "current"  # Default version for current data
          
          # Only skip if no force update is requested and the variable hasn't been modified
          vars_to_skip <- c(vars_to_skip, var_name)
        }
      }
      
      # Calculate how many records we're skipping
      if (length(vars_to_skip) > 0) {
        vars_to_skip_count <- long_data %>%
          filter(variable_name %in% vars_to_skip) %>%
          nrow()
        
        if (vars_to_skip_count > 0) {
          log_message(paste("Skipping", vars_to_skip_count, "already processed records for batch", batch),
                     level = "INFO", show_console = TRUE)
          
          # Filter out the data that's already been processed
          long_data <- long_data %>%
            filter(!variable_name %in% vars_to_skip)
        }
      }
    }
    
    # Skip empty batches
    if (nrow(long_data) == 0) {
      log_message(paste("Batch", batch, "has no new data to insert - skipping"),
                 level = "INFO", show_console = TRUE)
      next
    }
    
    log_message(paste("Inserting batch", batch, "of", total_batches, "into database (",
                      nrow(long_data), "records)"),
               level = "INFO", show_console = TRUE)
    
    # For incremental mode, use "upsert" approach to update existing records
    if (use_incremental) {
      # Create a temporary table for the batch data
      temp_table_name <- paste0("temp_batch_", batch)
      
      tryCatch({
        # Create temporary table
        dbWriteTable(con, temp_table_name, long_data, temporary = TRUE)
        
        # Use SQL for upsert operation
        upsert_query <- paste0("
          INSERT OR REPLACE INTO sdoh_data 
          SELECT * FROM ", temp_table_name
        )
        
        # Execute the upsert
        dbExecute(con, upsert_query)
        
        # Clean up temporary table
        dbExecute(con, paste0("DROP TABLE IF EXISTS ", temp_table_name))
        
        log_message(paste("Upserted", nrow(long_data), "data points for batch", batch),
                   level = "INFO", show_console = TRUE)
      }, error = function(e) {
        # Try to clean up the temp table if it exists
        tryCatch({
          dbExecute(con, paste0("DROP TABLE IF EXISTS ", temp_table_name))
        }, error = function(e2) {
          # Ignore errors dropping temp table
        })
        
        # Re-throw the original error
        stop(paste("Error during upsert operation:", conditionMessage(e)))
      })
    } else {
      # For full processing mode, use the original append method
      tryCatch({
        dbWriteTable(con, "sdoh_data", long_data, append = TRUE)
        log_message(paste("Inserted", nrow(long_data), "data points for batch", batch),
                   level = "INFO", show_console = TRUE)
      }, error = function(e) {
      # Check for duplicate key errors
      if (grepl("Duplicate key.*violates primary key constraint", conditionMessage(e))) {
        # Find duplicate entries in this batch
        duplicates <- long_data %>%
          group_by(geoid, year, variable_name) %>%
          filter(n() > 1) %>%
          ungroup()
        
        if (nrow(duplicates) > 0) {
          # There are duplicates within this batch
          log_message(paste("ERROR: Found", nrow(duplicates), "duplicate entries within batch", batch),
                     level = "ERROR", show_console = TRUE)
          log_message(paste("First duplicate:", 
                           paste0("geoid: ", duplicates$geoid[1], 
                                 ", year: ", duplicates$year[1], 
                                 ", variable_name: ", duplicates$variable_name[1])),
                     level = "ERROR", show_console = TRUE)
        } else {
          # Batch is fine, but duplicates already exist in database
          log_message(paste("ERROR: Duplicate entry detected when inserting batch", batch),
                     level = "ERROR", show_console = TRUE)
          log_message(paste("Error message:", conditionMessage(e)),
                     level = "ERROR", show_console = TRUE)
          log_message("This indicates a duplicate key already exists in the database",
                     level = "ERROR", show_console = TRUE)
        }
        
        # Add an option to continue with de-duplication
        log_message("Attempting to remove duplicates and continue...",
                   level = "INFO", show_console = TRUE)
        
        # De-duplicate the data
        long_data_unique <- long_data %>%
          distinct(geoid, year, variable_name, .keep_all = TRUE)
        
        # Calculate how many duplicates were removed
        duplicates_removed <- nrow(long_data) - nrow(long_data_unique)
        if (duplicates_removed > 0) {
          log_message(paste("Removed", duplicates_removed, "duplicate entries from batch", batch),
                     level = "INFO", show_console = TRUE)
        }
        
        # Try again with de-duplicated data
        tryCatch({
          dbWriteTable(con, "sdoh_data", long_data_unique, append = TRUE)
          log_message(paste("Successfully inserted", nrow(long_data_unique), 
                          "de-duplicated data points for batch", batch),
                     level = "INFO", show_console = TRUE)
        }, error = function(inner_error) {
          log_message(paste("ERROR: Failed to insert even after de-duplication:", 
                           conditionMessage(inner_error)),
                     level = "ERROR", show_console = TRUE)
          stop(paste("Database insertion failed even after de-duplication:", 
                    conditionMessage(inner_error)))
        })
      } else {
        # Re-throw other errors
        log_message(paste("ERROR during database insertion:", conditionMessage(e)),
                   level = "ERROR", show_console = TRUE)
        stop(paste("Database insertion error:", conditionMessage(e)))
      }
    })
  }
  
  # Create optimized indices based on detected system resources and index strategy
  log_message(paste0("Creating optimized database indices (strategy: ", db_settings$index_strategy, ")..."),
             level = "INFO", show_console = TRUE)
  
  # Start transaction for faster index creation
  dbExecute(con, "BEGIN TRANSACTION")
  
  # Track index creation time
  index_start_time <- Sys.time()
  created_indices <- 0
  
  # Create indices based on the determined index strategy
  if (db_settings$index_strategy %in% c("standard", "comprehensive", "advanced")) {
    # Basic indices - created for all strategies
    log_message("Creating basic indices for primary query patterns...",
               level = "INFO", show_console = TRUE)
    
    # Primary key indices
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_geoid ON sdoh_data(geoid)")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_year ON sdoh_data(year)")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_variable ON sdoh_data(variable_name)")
    created_indices <- created_indices + 3
    
    # Common composite indices for lookups
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_geoid_year ON sdoh_data(geoid, year)")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_variable_year ON sdoh_data(variable_name, year)")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_geoid_variable ON sdoh_data(geoid, variable_name)")
    created_indices <- created_indices + 3
  }
  
  # Additional indices for comprehensive and advanced strategies
  if (db_settings$index_strategy %in% c("comprehensive", "advanced")) {
    log_message("Creating comprehensive indices for optimized queries...",
               level = "INFO", show_console = TRUE)
    
    # Advanced composite index with included columns for covering queries
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_geoid_year_variable_covering 
               ON sdoh_data(geoid, year, variable_name) 
               INCLUDE (value, data_quality, data_source)")
    created_indices <- created_indices + 1
    
    # Index for temporal queries (trends over time)
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_variable_year_value 
               ON sdoh_data(variable_name, year, value)")
    created_indices <- created_indices + 1
    
    # Index for quality filtering
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_data_quality 
               ON sdoh_data(data_quality, variable_name, year)")
    created_indices <- created_indices + 1
    
    # Index for source-based queries
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_data_source 
               ON sdoh_data(data_source, variable_name)")
    created_indices <- created_indices + 1
    
    # Index for timestamp-based filtering (useful for incremental updates)
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_last_updated 
               ON sdoh_data(last_updated DESC)")
    created_indices <- created_indices + 1
  }
  
  # High-performance specialized indices for advanced strategy only
  if (db_settings$index_strategy == "advanced") {
    log_message("Creating advanced specialized indices for complex queries...",
               level = "INFO", show_console = TRUE)
    
    # Expression-based indices for common computations
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_year_binned 
               ON sdoh_data(year / 10)")  # For decade-based queries
    created_indices <- created_indices + 1
    
    # Multi-column indices for complex filtering
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_variable_quality_value
               ON sdoh_data(variable_name, data_quality, value)")
    created_indices <- created_indices + 1
    
    # State-level aggregation index (state FIPS is first 2 chars of geoid)
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_state_variable_year
               ON sdoh_data(SUBSTRING(geoid, 1, 2), variable_name, year)")
    created_indices <- created_indices + 1
    
    # Value range index for range queries
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_value_range 
               ON sdoh_data(variable_name, value)")
    created_indices <- created_indices + 1
    
    # Advanced composite index for time series analysis
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_time_series 
               ON sdoh_data(geoid, variable_name, year) 
               INCLUDE (value, ci_lower, ci_upper)")
    created_indices <- created_indices + 1
    
    # Partial indices for filtered queries (if supported)
    tryCatch({
      # Only index high-quality data
      dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_high_quality_partial 
                 ON sdoh_data(geoid, variable_name, year, value) 
                 WHERE data_quality = 'high'")
      created_indices <- created_indices + 1
      
      # Only index recent data (last 10 years)
      current_year <- as.integer(format(Sys.Date(), "%Y"))
      cutoff_year <- current_year - 10
      dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_recent_data_partial 
                          ON sdoh_data(geoid, variable_name, value) 
                          WHERE year >= ", cutoff_year))
      created_indices <- created_indices + 1
    }, error = function(e) {
      # Partial indices may not be supported in all DuckDB versions
      log_message("Skipping partial indices (not supported in this DuckDB version)",
                 level = "INFO", show_console = TRUE)
    })
  }
  
  # Commit transaction for indices
  dbExecute(con, "COMMIT")
  
  # Calculate index creation time
  index_end_time <- Sys.time()
  index_time_taken <- difftime(index_end_time, index_start_time, units = "secs")
  
  # Analyze tables for query optimization
  log_message("Analyzing tables to optimize query planning...",
             level = "INFO", show_console = TRUE)
  
  # Run ANALYZE to update statistics for query planning
  dbExecute(con, "ANALYZE sdoh_data")
  dbExecute(con, "ANALYZE counties")
  dbExecute(con, "ANALYZE variables")
  
  # Log index creation summary
  log_message(paste0("Created ", created_indices, " optimized indices in ", 
                   round(as.numeric(index_time_taken), 2), " seconds"),
             level = "INFO", show_console = TRUE)
  
  # Create optimized views and (optionally) materialized views
  log_message("Creating optimized views for data access...",
             level = "INFO", show_console = TRUE)
             
  # Start tracking view creation time
  view_start_time <- Sys.time()
  
  # Define a list of essential views that will always be created
  essential_views <- list(
    # 1. View for latest data by county/variable
    latest_data = "
      CREATE OR REPLACE VIEW latest_data AS
      SELECT 
        c.geoid,
        c.name,
        c.state_fips,
        c.state_name,
        d.year,
        d.variable_name,
        d.value,
        d.data_quality,
        d.data_source
      FROM counties c
      JOIN sdoh_data d ON c.geoid = d.geoid
      WHERE (d.geoid, d.variable_name, d.year) IN (
        SELECT geoid, variable_name, MAX(year) 
        FROM sdoh_data 
        GROUP BY geoid, variable_name
      )
    ",
    
    # 2. View for data coverage analysis
    data_coverage = "
      CREATE OR REPLACE VIEW data_coverage AS
      SELECT 
        variable_name,
        year,
        COUNT(*) as county_count,
        (SELECT COUNT(*) FROM counties) as total_counties,
        CAST(COUNT(*) AS FLOAT) / (SELECT COUNT(*) FROM counties) * 100 as coverage_percent
      FROM sdoh_data
      GROUP BY variable_name, year
      ORDER BY variable_name, year
    ",
    
    # 3. View for time series analysis
    county_time_series = "
      CREATE OR REPLACE VIEW county_time_series AS
      SELECT 
        sd.geoid,
        c.name as county_name,
        c.state_name,
        sd.year,
        sd.variable_name,
        sd.value,
        sd.data_quality,
        sd.data_source
      FROM sdoh_data sd
      JOIN counties c ON sd.geoid = c.geoid
      ORDER BY sd.geoid, sd.variable_name, sd.year
    "
  )
  
  # Define additional views that depend on system resources
  additional_views <- list()
  
  # Add pivot view for wide-format data if memory allows
  if (db_settings$system_resources$memory_gb >= 4) {
    additional_views$county_wide_latest <- "
      CREATE OR REPLACE VIEW county_wide_latest AS
      WITH vars AS (
        SELECT DISTINCT variable_name 
        FROM sdoh_data
      )
      SELECT 
        c.geoid,
        c.name,
        c.state_fips,
        c.state_name,
        MAX(d.year) as latest_year,
        GROUP_CONCAT(d.variable_name || ':' || CAST(d.value AS VARCHAR), ',') as variable_data
      FROM counties c
      LEFT JOIN sdoh_data d ON c.geoid = d.geoid
      WHERE (d.geoid, d.variable_name, d.year) IN (
        SELECT geoid, variable_name, MAX(year) 
        FROM sdoh_data 
        GROUP BY geoid, variable_name
      )
      GROUP BY c.geoid, c.name, c.state_fips, c.state_name
    "
  } else {
    # Simpler pivot view that doesn't use GROUP_CONCAT for memory-limited systems
    additional_views$county_wide_latest <- "
      CREATE OR REPLACE VIEW county_wide_latest AS
      SELECT 
        c.geoid,
        c.name,
        c.state_fips,
        c.state_name,
        MAX(d.year) as latest_year
      FROM counties c
      LEFT JOIN sdoh_data d ON c.geoid = d.geoid
      GROUP BY c.geoid, c.name, c.state_fips, c.state_name
    "
  }
  
  # Add state-level aggregation view if memory allows
  if (db_settings$system_resources$memory_gb >= 6) {
    additional_views$state_aggregation <- "
      CREATE OR REPLACE VIEW state_aggregation AS
      SELECT 
        SUBSTRING(sd.geoid, 1, 2) as state_fips,
        c.state_name,
        sd.variable_name,
        sd.year,
        AVG(sd.value) as avg_value,
        MIN(sd.value) as min_value,
        MAX(sd.value) as max_value,
        COUNT(*) as county_count
      FROM sdoh_data sd
      JOIN counties c ON sd.geoid = c.geoid
      GROUP BY SUBSTRING(sd.geoid, 1, 2), c.state_name, sd.variable_name, sd.year
      ORDER BY c.state_name, sd.variable_name, sd.year
    "
  }
  
  # Add time series trend view if memory allows
  if (db_settings$system_resources$memory_gb >= 8) {
    additional_views$variable_trends <- "
      CREATE OR REPLACE VIEW variable_trends AS
      WITH yearly_stats AS (
        SELECT 
          variable_name,
          year,
          AVG(value) as national_avg,
          STDDEV(value) as national_stddev,
          MIN(value) as national_min,
          MAX(value) as national_max,
          PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY value) as percentile_25,
          PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY value) as median,
          PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY value) as percentile_75
        FROM sdoh_data
        GROUP BY variable_name, year
      )
      SELECT 
        variable_name,
        year,
        national_avg,
        national_stddev,
        national_min,
        national_max,
        median,
        percentile_25,
        percentile_75,
        (national_avg - LAG(national_avg) OVER (PARTITION BY variable_name ORDER BY year)) as year_over_year_change,
        (national_avg - LAG(national_avg, 5) OVER (PARTITION BY variable_name ORDER BY year)) as five_year_change
      FROM yearly_stats
      ORDER BY variable_name, year
    "
  }
  
  # Add correlation view if memory allows
  if (db_settings$system_resources$memory_gb >= 12 && 
      db_settings$advanced_view_options$complex_join_views) {
    additional_views$variable_correlations <- "
      CREATE OR REPLACE VIEW variable_correlations AS
      WITH variable_pairs AS (
        SELECT DISTINCT 
          a.variable_name as var1,
          b.variable_name as var2
        FROM 
          (SELECT DISTINCT variable_name FROM sdoh_data) a,
          (SELECT DISTINCT variable_name FROM sdoh_data) b
        WHERE 
          a.variable_name < b.variable_name
      ),
      variable_data AS (
        SELECT 
          p.var1,
          p.var2,
          sd1.geoid,
          sd1.year,
          sd1.value as value1,
          sd2.value as value2
        FROM 
          variable_pairs p
          JOIN sdoh_data sd1 ON p.var1 = sd1.variable_name
          JOIN sdoh_data sd2 ON p.var2 = sd2.variable_name AND sd1.geoid = sd2.geoid AND sd1.year = sd2.year
        WHERE sd1.value IS NOT NULL AND sd2.value IS NOT NULL
      )
      SELECT 
        var1,
        var2,
        CORR(value1, value2) as correlation_coefficient,
        COUNT(*) as sample_size
      FROM variable_data
      GROUP BY var1, var2
      HAVING COUNT(*) >= 30 -- Only show correlations with sufficient sample size
      ORDER BY ABS(correlation_coefficient) DESC
    "
  }
  
  # Create essential views first
  log_message("Creating essential views...", level = "INFO", show_console = TRUE)
  created_views <- 0
  
  for (view_name in names(essential_views)) {
    tryCatch({
      dbExecute(con, essential_views[[view_name]])
      created_views <- created_views + 1
      log_message(paste("  - Created view:", view_name), level = "INFO", show_console = FALSE)
    }, error = function(e) {
      log_message(paste("Error creating", view_name, "view:", conditionMessage(e)),
                 level = "WARN", show_console = TRUE)
    })
  }
  
  # Create additional views based on system resources
  if (length(additional_views) > 0) {
    log_message("Creating additional resource-dependent views...", level = "INFO", show_console = TRUE)
    
    for (view_name in names(additional_views)) {
      tryCatch({
        dbExecute(con, additional_views[[view_name]])
        created_views <- created_views + 1
        log_message(paste("  - Created view:", view_name), level = "INFO", show_console = FALSE)
      }, error = function(e) {
        log_message(paste("Error creating", view_name, "view:", conditionMessage(e)),
                   level = "WARN", show_console = TRUE)
      })
    }
  }
  
  # Define materialized views based on system memory availability
  materialized_views <- list()
  
  # Basic materialized views (for systems with ≥ 4GB RAM)
  if (db_settings$enable_materialized_views) {
    # 1. Materialized view for latest data
    materialized_views$latest_data_materialized <- list(
      query = "
        SELECT 
          c.geoid,
          c.name,
          c.state_fips,
          c.state_name,
          d.year,
          d.variable_name,
          d.value,
          d.data_quality,
          d.data_source
        FROM counties c
        JOIN sdoh_data d ON c.geoid = d.geoid
        WHERE (d.geoid, d.variable_name, d.year) IN (
          SELECT geoid, variable_name, MAX(year) 
          FROM sdoh_data 
          GROUP BY geoid, variable_name
        )
      ",
      indices = list(
        "CREATE INDEX IF NOT EXISTS idx_latest_geoid ON latest_data_materialized(geoid)",
        "CREATE INDEX IF NOT EXISTS idx_latest_variable ON latest_data_materialized(variable_name)",
        "CREATE INDEX IF NOT EXISTS idx_latest_geoid_variable ON latest_data_materialized(geoid, variable_name)"
      )
    )
    
    # 2. Materialized view for data coverage
    materialized_views$data_coverage_materialized <- list(
      query = "
        SELECT 
          variable_name,
          year,
          COUNT(*) as county_count,
          (SELECT COUNT(*) FROM counties) as total_counties,
          CAST(COUNT(*) AS FLOAT) / (SELECT COUNT(*) FROM counties) * 100 as coverage_percent
        FROM sdoh_data
        GROUP BY variable_name, year
        ORDER BY variable_name, year
      ",
      indices = list(
        "CREATE INDEX IF NOT EXISTS idx_coverage_variable ON data_coverage_materialized(variable_name)",
        "CREATE INDEX IF NOT EXISTS idx_coverage_year ON data_coverage_materialized(year)"
      )
    )
  }
  
  # Additional materialized views for systems with more memory (≥ 8GB)
  if (db_settings$system_resources$memory_gb >= 8 && 
      db_settings$advanced_view_options$count_distinct_views) {
    # 3. Materialized view for state-level statistics
    materialized_views$state_stats_materialized <- list(
      query = "
        SELECT 
          SUBSTRING(sd.geoid, 1, 2) as state_fips,
          c.state_name,
          sd.variable_name,
          sd.year,
          AVG(sd.value) as avg_value,
          STDDEV(sd.value) as stddev_value,
          MIN(sd.value) as min_value,
          MAX(sd.value) as max_value,
          COUNT(*) as county_count
        FROM sdoh_data sd
        JOIN counties c ON sd.geoid = c.geoid
        GROUP BY SUBSTRING(sd.geoid, 1, 2), c.state_name, sd.variable_name, sd.year
      ",
      indices = list(
        "CREATE INDEX IF NOT EXISTS idx_state_stats_fips ON state_stats_materialized(state_fips)",
        "CREATE INDEX IF NOT EXISTS idx_state_stats_var_year ON state_stats_materialized(variable_name, year)"
      )
    )
  }
  
  # Complex materialized views for high-memory systems (≥ 16GB)
  if (db_settings$system_resources$memory_gb >= 16 && 
      db_settings$advanced_view_options$pivot_views) {
    # 4. Materialized pivot view with variable data as columns
    # This creates a wide, spreadsheet-like format for the most recent year
    # of each variable for each county
    
    # First, get a list of the most common variables to include in the pivot
    top_variables <- tryCatch({
      # Get the top 50 most common variables
      result <- dbGetQuery(con, "
        SELECT variable_name, COUNT(*) as count
        FROM sdoh_data
        GROUP BY variable_name
        ORDER BY count DESC
        LIMIT 50
      ")
      result$variable_name
    }, error = function(e) {
      log_message("Error getting top variables for pivot view", level = "WARN", show_console = TRUE)
      character(0)  # Return empty character vector
    })
    
    if (length(top_variables) > 0) {
      # Create pivot query dynamically based on available variables
      pivot_columns <- character(0)
      for (var in top_variables) {
        # Create a CASE expression for each variable
        pivot_col <- paste0("
          MAX(CASE WHEN d.variable_name = '", var, "' THEN d.value ELSE NULL END) as ", 
          gsub("[^a-zA-Z0-9_]", "_", var))  # Clean variable name for column name
        pivot_columns <- c(pivot_columns, pivot_col)
      }
      
      # Combine all column expressions
      pivot_cols_sql <- paste(pivot_columns, collapse = ",\n")
      
      # Create the full pivot query
      pivot_query <- paste0("
        SELECT 
          c.geoid,
          c.name,
          c.state_fips,
          c.state_name,
          ", pivot_cols_sql, "
        FROM counties c
        LEFT JOIN (
          SELECT sd.*
          FROM sdoh_data sd
          JOIN (
            SELECT geoid, variable_name, MAX(year) as max_year
            FROM sdoh_data
            GROUP BY geoid, variable_name
          ) latest ON sd.geoid = latest.geoid 
              AND sd.variable_name = latest.variable_name 
              AND sd.year = latest.max_year
        ) d ON c.geoid = d.geoid
        GROUP BY c.geoid, c.name, c.state_fips, c.state_name
      ")
      
      # Add to materialized views list
      materialized_views$county_wide_pivot_materialized <- list(
        query = pivot_query,
        indices = list(
          "CREATE INDEX IF NOT EXISTS idx_pivot_geoid ON county_wide_pivot_materialized(geoid)",
          "CREATE INDEX IF NOT EXISTS idx_pivot_state ON county_wide_pivot_materialized(state_fips)"
        )
      )
    }
  }
  
  # Create materialized views if enabled
  created_materialized_views <- 0
  
  if (length(materialized_views) > 0 && db_settings$enable_materialized_views) {
    log_message(paste0("Creating materialized views for faster access (", 
                     length(materialized_views), " views)..."),
               level = "INFO", show_console = TRUE)
    
    # Start transaction for better performance
    dbExecute(con, "BEGIN TRANSACTION")
    
    for (view_name in names(materialized_views)) {
      tryCatch({
        # Drop existing materialized view if it exists
        dbExecute(con, paste0("DROP TABLE IF EXISTS ", view_name))
        
        # Create new materialized view
        create_query <- paste0("CREATE TABLE ", view_name, " AS ", 
                              materialized_views[[view_name]]$query)
        dbExecute(con, create_query)
        
        # Create indices for the materialized view
        for (idx_query in materialized_views[[view_name]]$indices) {
          dbExecute(con, idx_query)
        }
        
        created_materialized_views <- created_materialized_views + 1
        log_message(paste("  - Created materialized view:", view_name), 
                   level = "INFO", show_console = FALSE)
      }, error = function(e) {
        log_message(paste("Error creating materialized view", view_name, ":", conditionMessage(e)),
                   level = "WARN", show_console = TRUE)
      })
    }
    
    # Commit transaction
    dbExecute(con, "COMMIT")
    
    # Log summary
    log_message(paste("Successfully created", created_materialized_views, 
                    "of", length(materialized_views), "materialized views"),
               level = "INFO", show_console = TRUE)
  } else {
    log_message("Skipping materialized views due to memory constraints (using regular views only)",
               level = "INFO", show_console = TRUE)
  }
  
  # Calculate view creation time
  view_end_time <- Sys.time()
  view_time_taken <- difftime(view_end_time, view_start_time, units = "secs")
  
  # Log view creation summary
  log_message(paste0("Created ", created_views, " regular views and ", 
                   created_materialized_views, " materialized views in ", 
                   round(as.numeric(view_time_taken), 2), " seconds"),
             level = "INFO", show_console = TRUE)
  
  # Get database stats
  total_rows <- dbGetQuery(con, "SELECT COUNT(*) FROM sdoh_data")[1,1]
  county_count <- dbGetQuery(con, "SELECT COUNT(*) FROM counties")[1,1]
  variable_count <- dbGetQuery(con, "SELECT COUNT(*) FROM variables")[1,1]
  year_count <- dbGetQuery(con, "SELECT COUNT(DISTINCT year) FROM sdoh_data")[1,1]
  min_year <- dbGetQuery(con, "SELECT MIN(year) FROM sdoh_data")[1,1]
  max_year <- dbGetQuery(con, "SELECT MAX(year) FROM sdoh_data")[1,1]
  
  # Function to refresh materialized views when in incremental mode
  refresh_materialized_views <- function() {
    log_message("Refreshing materialized views with latest data...",
               level = "INFO", show_console = TRUE)
    
    # Skip if materialized views are disabled due to memory constraints
    if (!db_settings$enable_materialized_views) {
      log_message("Skipping materialized view refresh (disabled due to memory constraints)",
                 level = "INFO", show_console = TRUE)
      return(FALSE)
    }
    
    # Start tracking the refresh time
    refresh_start_time <- Sys.time()
    refreshed_views <- 0
    
    # Get the list of materialized views that exist in the database
    existing_views <- tryCatch({
      dbGetQuery(con, "
        SELECT name
        FROM sqlite_master
        WHERE type='table' AND name LIKE '%_materialized'
      ")$name
    }, error = function(e) {
      log_message(paste("Error getting list of materialized views:", conditionMessage(e)),
                 level = "WARN", show_console = TRUE)
      character(0)  # Return empty character vector
    })
    
    if (length(existing_views) == 0) {
      log_message("No materialized views found to refresh",
                 level = "INFO", show_console = TRUE)
      return(FALSE)
    }
    
    log_message(paste("Found", length(existing_views), "materialized views to refresh"),
               level = "INFO", show_console = TRUE)
    
    # Create a function to refresh each view type
    refresh_view <- function(view_name) {
      tryCatch({
        # Create temporary table name for refreshing
        temp_table_name <- paste0("temp_", view_name)
        
        # Define refresh queries based on view type
        if (view_name == "latest_data_materialized") {
          # Create a temporary table with the latest data
          dbExecute(con, paste0("DROP TABLE IF EXISTS ", temp_table_name))
          dbExecute(con, paste0("
            CREATE TABLE ", temp_table_name, " AS
            SELECT 
              c.geoid,
              c.name,
              c.state_fips,
              c.state_name,
              d.year,
              d.variable_name,
              d.value,
              d.data_quality,
              d.data_source
            FROM counties c
            JOIN sdoh_data d ON c.geoid = d.geoid
            WHERE (d.geoid, d.variable_name, d.year) IN (
              SELECT geoid, variable_name, MAX(year) 
              FROM sdoh_data 
              GROUP BY geoid, variable_name
            )
          "))
          
          # Replace the old table with the new data
          dbExecute(con, paste0("DROP TABLE IF EXISTS ", view_name))
          dbExecute(con, paste0("ALTER TABLE ", temp_table_name, " RENAME TO ", view_name))
          
          # Recreate indices
          dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_latest_geoid ON ", view_name, "(geoid)"))
          dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_latest_variable ON ", view_name, "(variable_name)"))
          dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_latest_geoid_variable ON ", 
                              view_name, "(geoid, variable_name)"))
          
          return(TRUE)
        } 
        else if (view_name == "data_coverage_materialized") {
          # Create a temporary table with the data coverage information
          dbExecute(con, paste0("DROP TABLE IF EXISTS ", temp_table_name))
          dbExecute(con, paste0("
            CREATE TABLE ", temp_table_name, " AS
            SELECT 
              variable_name,
              year,
              COUNT(*) as county_count,
              (SELECT COUNT(*) FROM counties) as total_counties,
              CAST(COUNT(*) AS FLOAT) / (SELECT COUNT(*) FROM counties) * 100 as coverage_percent
            FROM sdoh_data
            GROUP BY variable_name, year
            ORDER BY variable_name, year
          "))
          
          # Replace the old table with the new data
          dbExecute(con, paste0("DROP TABLE IF EXISTS ", view_name))
          dbExecute(con, paste0("ALTER TABLE ", temp_table_name, " RENAME TO ", view_name))
          
          # Recreate indices
          dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_coverage_variable ON ", 
                              view_name, "(variable_name)"))
          dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_coverage_year ON ", 
                              view_name, "(year)"))
          
          return(TRUE)
        }
        else if (view_name == "state_stats_materialized") {
          # Create a temporary table with state-level statistics
          dbExecute(con, paste0("DROP TABLE IF EXISTS ", temp_table_name))
          dbExecute(con, paste0("
            CREATE TABLE ", temp_table_name, " AS
            SELECT 
              SUBSTRING(sd.geoid, 1, 2) as state_fips,
              c.state_name,
              sd.variable_name,
              sd.year,
              AVG(sd.value) as avg_value,
              STDDEV(sd.value) as stddev_value,
              MIN(sd.value) as min_value,
              MAX(sd.value) as max_value,
              COUNT(*) as county_count
            FROM sdoh_data sd
            JOIN counties c ON sd.geoid = c.geoid
            GROUP BY SUBSTRING(sd.geoid, 1, 2), c.state_name, sd.variable_name, sd.year
          "))
          
          # Replace the old table with the new data
          dbExecute(con, paste0("DROP TABLE IF EXISTS ", view_name))
          dbExecute(con, paste0("ALTER TABLE ", temp_table_name, " RENAME TO ", view_name))
          
          # Recreate indices
          dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_state_stats_fips ON ", 
                              view_name, "(state_fips)"))
          dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_state_stats_var_year ON ", 
                              view_name, "(variable_name, year)"))
          
          return(TRUE)
        }
        else if (view_name == "county_wide_pivot_materialized") {
          # For complex pivot view, we need to get the list of variables first
          top_variables <- dbGetQuery(con, "
            SELECT variable_name, COUNT(*) as count
            FROM sdoh_data
            GROUP BY variable_name
            ORDER BY count DESC
            LIMIT 50
          ")$variable_name
          
          if (length(top_variables) > 0) {
            # Create pivot query dynamically based on available variables
            pivot_columns <- character(0)
            for (var in top_variables) {
              # Create a CASE expression for each variable
              pivot_col <- paste0("
                MAX(CASE WHEN d.variable_name = '", var, "' THEN d.value ELSE NULL END) as ", 
                gsub("[^a-zA-Z0-9_]", "_", var))  # Clean variable name for column name
              pivot_columns <- c(pivot_columns, pivot_col)
            }
            
            # Combine all column expressions
            pivot_cols_sql <- paste(pivot_columns, collapse = ",\n")
            
            # Create the full pivot query
            pivot_query <- paste0("
              CREATE TABLE ", temp_table_name, " AS
              SELECT 
                c.geoid,
                c.name,
                c.state_fips,
                c.state_name,
                ", pivot_cols_sql, "
              FROM counties c
              LEFT JOIN (
                SELECT sd.*
                FROM sdoh_data sd
                JOIN (
                  SELECT geoid, variable_name, MAX(year) as max_year
                  FROM sdoh_data
                  GROUP BY geoid, variable_name
                ) latest ON sd.geoid = latest.geoid 
                    AND sd.variable_name = latest.variable_name 
                    AND sd.year = latest.max_year
              ) d ON c.geoid = d.geoid
              GROUP BY c.geoid, c.name, c.state_fips, c.state_name
            ")
            
            # Create the temporary table
            dbExecute(con, paste0("DROP TABLE IF EXISTS ", temp_table_name))
            dbExecute(con, pivot_query)
            
            # Replace the old table with the new data
            dbExecute(con, paste0("DROP TABLE IF EXISTS ", view_name))
            dbExecute(con, paste0("ALTER TABLE ", temp_table_name, " RENAME TO ", view_name))
            
            # Recreate indices
            dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_pivot_geoid ON ", 
                                view_name, "(geoid)"))
            dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_pivot_state ON ", 
                                view_name, "(state_fips)"))
            
            return(TRUE)
          } else {
            log_message("No variables found for pivot view - skipping refresh",
                       level = "WARN", show_console = TRUE)
            return(FALSE)
          }
        } else {
          # For any other materialized view, try to get its definition from the database
          # and use it to refresh the view
          
          # Get the view definition by examining the CREATE statement that generated it
          view_def_query <- paste0("
            SELECT sql FROM sqlite_master 
            WHERE type='table' AND name='", view_name, "'
          ")
          
          view_def <- dbGetQuery(con, view_def_query)
          
          if (nrow(view_def) > 0 && !is.na(view_def$sql[1])) {
            # Parse the SQL to extract the query part after "AS"
            sql_parts <- strsplit(view_def$sql[1], " AS ", fixed = TRUE)[[1]]
            
            if (length(sql_parts) >= 2) {
              # Extract the SELECT statement after "AS"
              select_stmt <- paste(sql_parts[2:length(sql_parts)], collapse = " AS ")
              
              # Create a temporary table with the query result
              dbExecute(con, paste0("DROP TABLE IF EXISTS ", temp_table_name))
              dbExecute(con, paste0("CREATE TABLE ", temp_table_name, " AS ", select_stmt))
              
              # Replace the old table with the new data
              dbExecute(con, paste0("DROP TABLE IF EXISTS ", view_name))
              dbExecute(con, paste0("ALTER TABLE ", temp_table_name, " RENAME TO ", view_name))
              
              log_message(paste("Refreshed generic materialized view:", view_name),
                         level = "INFO", show_console = TRUE)
              return(TRUE)
            }
          }
          
          log_message(paste("Couldn't determine refresh strategy for materialized view:", view_name),
                     level = "WARN", show_console = TRUE)
          return(FALSE)
        }
      }, error = function(e) {
        log_message(paste("Error refreshing", view_name, ":", conditionMessage(e)),
                   level = "WARN", show_console = TRUE)
        return(FALSE)
      })
    }
    
    # Start a transaction for atomic update of all materialized views
    dbExecute(con, "BEGIN TRANSACTION")
    
    # Refresh each view safely
    refresh_results <- list()
    
    for (view_name in existing_views) {
      refresh_results[[view_name]] <- tryCatch({
        log_message(paste("Refreshing materialized view:", view_name),
                   level = "INFO", show_console = TRUE)
        result <- refresh_view(view_name)
        if (result) {
          refreshed_views <- refreshed_views + 1
          log_message(paste("Successfully refreshed materialized view:", view_name),
                     level = "INFO", show_console = TRUE)
        }
        result
      }, error = function(e) {
        log_message(paste("Error during refresh of", view_name, ":", conditionMessage(e)),
                   level = "ERROR", show_console = TRUE)
        FALSE
      })
    }
    
    # Commit the transaction
    commit_success <- tryCatch({
      dbExecute(con, "COMMIT")
      TRUE
    }, error = function(e) {
      # If commit fails, try to rollback
      tryCatch({
        dbExecute(con, "ROLLBACK")
        log_message("Rolled back failed materialized view refresh transaction",
                   level = "WARN", show_console = TRUE)
      }, error = function(e2) {
        log_message("Failed to rollback transaction after materialized view refresh error",
                   level = "ERROR", show_console = TRUE)
      })
      
      log_message(paste("Error committing materialized view updates:", conditionMessage(e)),
                 level = "ERROR", show_console = TRUE)
      FALSE
    })
    
    # Calculate refresh time
    refresh_end_time <- Sys.time()
    refresh_time_taken <- difftime(refresh_end_time, refresh_start_time, units = "secs")
    
    # Log summary
    if (commit_success) {
      log_message(paste0("Successfully refreshed ", refreshed_views, " of ", 
                       length(existing_views), " materialized views in ",
                       round(as.numeric(refresh_time_taken), 2), " seconds"),
                 level = "INFO", show_console = TRUE)
      return(TRUE)
    } else {
      log_message("Materialized view refresh failed - transaction could not be committed",
                 level = "ERROR", show_console = TRUE)
      return(FALSE)
    }
  }
  
  # When in incremental mode and updates were made, refresh materialized views
  if (use_incremental) {
    # Check if any data was inserted or updated in this run
    for (batch in 1:total_batches) {
      batch_data <- batch_results[[batch]]
      if (!is.null(batch_data) && nrow(batch_data) > 0) {
        # We had some data updates, so refresh the materialized views
        refresh_materialized_views()
        break
      }
    }
  }
  
  # Update processing metadata
  log_message("Updating processing metadata...",
             level = "INFO", show_console = TRUE)
  
  # Get metrics for each variable in the database
  variable_metrics <- dbGetQuery(con, "
    SELECT 
      variable_name,
      MIN(year) as min_year,
      MAX(year) as max_year,
      COUNT(*) as record_count
    FROM sdoh_data
    GROUP BY variable_name
  ")
  
  # Create metadata entries for each variable
  if (nrow(variable_metrics) > 0) {
    # Determine data sources for each variable
    variable_sources <- variable_metrics %>%
      mutate(
        data_source = case_when(
          grepl("^census_", variable_name) ~ "census",
          grepl("^traffic_", variable_name) ~ "traffic_safety", 
          grepl("^life_expectancy", variable_name) ~ "ihme",
          TRUE ~ "other"
        )
      )
    
    # Create metadata entries
    metadata_entries <- variable_sources %>%
      mutate(
        last_processed = Sys.time(),
        data_version = "current"
      )
    
    # Update the metadata table - delete existing entries first to avoid conflicts
    if (nrow(metadata_entries) > 0) {
      # Get the list of variables in the new metadata
      var_names_list <- paste0("'", paste(metadata_entries$variable_name, collapse = "','"), "'")
      
      # Delete existing entries for these variables
      delete_query <- paste0("
        DELETE FROM processing_metadata 
        WHERE variable_name IN (", var_names_list, ")
      ")
      dbExecute(con, delete_query)
      
      # Insert the new metadata
      dbWriteTable(con, "processing_metadata", metadata_entries, append = TRUE)
      log_message(paste("Updated processing metadata for", nrow(metadata_entries), "variables"),
                 level = "INFO", show_console = TRUE)
    }
  }
  
  # Disconnect from the database
  dbDisconnect(con)
  
  # Log database creation summary
  log_message("\nUnified database created successfully:",
             level = "INFO", show_console = TRUE)
  log_message(paste(" - Database path:", unified_db_path),
             level = "INFO", show_console = TRUE)
  log_message(paste(" - Total data points:", total_rows),
             level = "INFO", show_console = TRUE)
  log_message(paste(" - Counties:", county_count),
             level = "INFO", show_console = TRUE)
  log_message(paste(" - Variables:", variable_count),
             level = "INFO", show_console = TRUE)
  log_message(paste(" - Year range:", min_year, "to", max_year, 
                   "(", year_count, "unique years)"),
             level = "INFO", show_console = TRUE)
  
  # Indicate whether incremental processing was used
  if (use_incremental) {
    log_message(" - Processing mode: INCREMENTAL (only new/changed data processed)",
               level = "INFO", show_console = TRUE)
  } else {
    log_message(" - Processing mode: FULL (all data reprocessed)",
               level = "INFO", show_console = TRUE)
  }
  
  return(TRUE)
}

# Simple direct sourcing check
is_direct_run <- (sys.nframe() == 0)

# If run directly, show error message
if (is_direct_run) {
  message("Database module cannot be run directly. Use the unified pipeline.")
}

# Return TRUE for successful loading
TRUE
# End of file marker
1
