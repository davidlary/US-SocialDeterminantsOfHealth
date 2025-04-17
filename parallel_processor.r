#!/usr/bin/env Rscript

# Optimized Parallel Processing for SDOH Data Pipeline
# This script provides advanced parallel processing capabilities for the
# unified SDOH pipeline, significantly improving performance for data
# processing, interpolation, and visualization tasks.

# Required packages
if (!require("future")) install.packages("future", repos = "https://cloud.r-project.org")
if (!require("future.apply")) install.packages("future.apply", repos = "https://cloud.r-project.org")
if (!require("progressr")) install.packages("progressr", repos = "https://cloud.r-project.org")
if (!require("furrr")) install.packages("furrr", repos = "https://cloud.r-project.org")
if (!require("tidyverse")) install.packages("tidyverse", repos = "https://cloud.r-project.org")
if (!require("foreach")) install.packages("foreach", repos = "https://cloud.r-project.org")
if (!require("doParallel")) install.packages("doParallel", repos = "https://cloud.r-project.org")
if (!require("doFuture")) install.packages("doFuture", repos = "https://cloud.r-project.org")
if (!require("bench")) install.packages("bench", repos = "https://cloud.r-project.org")

# Load required packages
library(future)
library(future.apply)
library(progressr)
library(furrr)
library(tidyverse)
library(foreach)
library(doParallel)
library(doFuture)
library(bench)

#' Set up the parallel processing environment with optimal strategy detection
#'
#' @param workers Number of workers to use (default: auto-detect)
#' @param strategy Parallel strategy to use (default: auto-detect best strategy)
#' @param chunk_size Size of chunks for parallel operations (default: auto-detect)
#' @param memory_limit Memory limit in GB per worker (default: auto-detect)
#' @param progress Whether to show progress bars (default: TRUE)
#'
#' @return List of parallel configuration settings
setup_parallel_environment <- function(
    workers = NULL,
    strategy = NULL,
    chunk_size = NULL,
    memory_limit = NULL,
    progress = TRUE
) {
  # Auto-detect optimal number of workers
  if (is.null(workers)) {
    # Try to detect available cores
    total_cores <- parallel::detectCores(logical = TRUE)
    
    if (is.na(total_cores) || total_cores < 1) {
      # Fallback if detection fails
      workers <- 2
      warning("Could not detect CPU cores. Defaulting to 2 workers.")
    } else {
      # Use N-1 cores to leave one for system processes
      workers <- max(1, total_cores - 1)
      
      # Cap at 16 workers by default to avoid excessive memory usage
      # User can still manually specify higher values if needed
      workers <- min(workers, 16)
    }
  }
  
  # Auto-detect best strategy based on OS and environment
  if (is.null(strategy)) {
    os_type <- Sys.info()["sysname"]
    
    if (os_type == "Windows") {
      # Windows: use multisession for better stability
      strategy <- "multisession"
    } else if (os_type == "Darwin") {
      # macOS: use multisession as fork can be problematic on Mac
      strategy <- "multisession"
    } else {
      # Linux/Unix: prefer fork for better performance
      if (identical(getOption("future.fork.enable"), TRUE)) {
        strategy <- "multicore"
      } else {
        strategy <- "multisession"
      }
    }
    
    # Check if running in RStudio, which can have issues with fork
    if (exists(".rs.api.versionInfo", mode = "function") && strategy == "multicore") {
      strategy <- "multisession"
    }
  }
  
  # Auto-detect optimal chunk size based on workers and data characteristics
  if (is.null(chunk_size)) {
    # Default: use sqrt of workers as multiplier for small to medium datasets
    chunk_size <- max(1, ceiling(sqrt(workers)) * 10)
  }
  
  # Auto-detect memory limit based on system memory
  if (is.null(memory_limit)) {
    # Try to detect system memory
    try_memory <- tryCatch({
      system_memory <- as.numeric(system("awk '/MemTotal/ {print $2}' /proc/meminfo", intern = TRUE)) / 1024^2
      if (is.na(system_memory) || system_memory <= 0) stop("Invalid memory detection")
      system_memory
    }, error = function(e) {
      # Fallback if detection fails
      8
    })
    
    # If memory detection failed, use conservative default
    if (is.numeric(try_memory)) {
      total_memory_gb <- try_memory
    } else {
      total_memory_gb <- 8
      warning("Could not detect system memory. Defaulting to 8GB assumption.")
    }
    
    # Allocate memory per worker based on total memory and worker count
    # with a safety margin to avoid out of memory errors
    memory_limit <- max(1, floor((total_memory_gb * 0.8) / workers))
  }
  
  # Set progress reporting
  if (progress) {
    progressr::handlers(global = TRUE)
    progressr::handlers("progress")
  }
  
  # Configure the parallel backend based on strategy
  if (strategy == "multicore" || strategy == "multisession") {
    # Configure future to use the selected strategy
    future::plan(strategy, workers = workers)
    
    # Set multicore and multisession options
    if (strategy == "multicore") {
      options(
        future.fork.enable = TRUE,
        future.globals.maxSize = memory_limit * 1024^3
      )
    } else {
      options(
        future.globals.maxSize = memory_limit * 1024^3
      )
    }
    
    # Set up doFuture for foreach
    doFuture::registerDoFuture()
  } else if (strategy == "cluster") {
    # Set up cluster backend
    cl <- parallel::makeCluster(workers)
    doParallel::registerDoParallel(cl)
    future::plan(cluster, workers = cl)
    
    # Configure cluster options
    options(future.globals.maxSize = memory_limit * 1024^3)
  }
  
  # Return configuration info
  parallel_config <- list(
    workers = workers,
    strategy = strategy,
    chunk_size = chunk_size,
    memory_limit = memory_limit,
    progress = progress
  )
  
  # Print configuration summary
  message(sprintf("Parallel environment configured with %d workers using '%s' strategy",
                 workers, strategy))
  message(sprintf("Memory limit: %d GB per worker, Chunk size: %d", 
                 memory_limit, chunk_size))
  
  return(parallel_config)
}

#' Clean up parallel environment when done
#'
#' @return NULL
cleanup_parallel_environment <- function() {
  # Reset future plan
  future::plan(sequential)
  
  # Clean up any registered parallel backends
  if (foreach::getDoParRegistered()) {
    foreach::registerDoSEQ()
  }
  
  # Force garbage collection
  gc(verbose = FALSE)
  
  message("Parallel environment cleaned up")
  return(invisible(NULL))
}

#' Parallelize data fetching operations
#'
#' @param sources List of data sources to fetch
#' @param fetch_func Function to call for fetching (must accept source as first argument)
#' @param ... Additional arguments to pass to fetch_func
#' @param parallel_config Parallel configuration from setup_parallel_environment
#'
#' @return List of fetched data by source
parallel_fetch_data <- function(
    sources,
    fetch_func,
    ...,
    parallel_config = setup_parallel_environment()
) {
  # Define the worker function for each source
  fetch_worker <- function(source, fetch_func, ...) {
    args <- list(...)
    args$source <- source
    
    # Call the fetch function with the source and additional arguments
    tryCatch({
      data <- do.call(fetch_func, args)
      return(list(source = source, data = data, error = NULL))
    }, error = function(e) {
      return(list(source = source, data = NULL, error = e$message))
    })
  }
  
  # Set up progress reporting
  if (parallel_config$progress) {
    p <- progressr::progressor(along = sources)
  }
  
  # Run fetches in parallel
  fetch_results <- future.apply::future_lapply(
    sources,
    function(source) {
      result <- fetch_worker(source, fetch_func, ...)
      if (parallel_config$progress) p()
      return(result)
    },
    future.packages = c("tidyverse"),
    future.seed = TRUE
  )
  
  # Organize results
  names(fetch_results) <- sapply(fetch_results, function(x) x$source)
  
  # Check for errors
  errors <- sapply(fetch_results, function(x) !is.null(x$error))
  if (any(errors)) {
    warning(sprintf("Errors occurred in %d of %d fetch operations", sum(errors), length(sources)))
    for (i in which(errors)) {
      warning(sprintf("Error fetching %s: %s", fetch_results[[i]]$source, fetch_results[[i]]$error))
    }
  }
  
  # Extract data
  data_list <- lapply(fetch_results, function(x) x$data)
  
  return(data_list)
}

#' Parallelize data processing operations
#'
#' @param data List or dataframe to process
#' @param process_func Function to apply to each chunk of data
#' @param by Variable to split data by (e.g., "year", "geoid"), or NULL for chunking
#' @param ... Additional arguments to pass to process_func
#' @param parallel_config Parallel configuration from setup_parallel_environment
#'
#' @return Processed data, combined from all parallel operations
parallel_process_data <- function(
    data,
    process_func,
    by = NULL,
    ...,
    parallel_config = setup_parallel_environment()
) {
  # If data is a list, process each element separately
  if (is.list(data) && !is.data.frame(data)) {
    # Set up progress reporting
    if (parallel_config$progress) {
      p <- progressr::progressor(along = names(data))
    }
    
    # Process each list element in parallel
    results <- future.apply::future_lapply(
      names(data),
      function(name) {
        result <- process_func(data[[name]], ...)
        if (parallel_config$progress) p()
        return(result)
      },
      future.packages = c("tidyverse"),
      future.seed = TRUE
    )
    
    # Name results
    names(results) <- names(data)
    return(results)
  }
  
  # If data is a dataframe, process in parallel chunks
  if (is.data.frame(data)) {
    if (!is.null(by)) {
      # Split data by the specified variable(s)
      if (length(by) == 1) {
        split_data <- split(data, data[[by]])
      } else {
        split_data <- split(data, interaction(data[by], drop = TRUE))
      }
      
      # Set up progress reporting
      if (parallel_config$progress) {
        p <- progressr::progressor(along = split_data)
      }
      
      # Process each group in parallel
      results <- future.apply::future_lapply(
        split_data,
        function(chunk) {
          result <- process_func(chunk, ...)
          if (parallel_config$progress) p()
          return(result)
        },
        future.packages = c("tidyverse"),
        future.seed = TRUE
      )
      
      # Combine results
      if (is.data.frame(results[[1]])) {
        combined_results <- dplyr::bind_rows(results)
      } else {
        combined_results <- results
      }
      
      return(combined_results)
    } else {
      # Split into chunks by rows
      n_rows <- nrow(data)
      chunk_size <- min(parallel_config$chunk_size, ceiling(n_rows / parallel_config$workers))
      chunks <- split(data, ceiling(seq_len(n_rows) / chunk_size))
      
      # Set up progress reporting
      if (parallel_config$progress) {
        p <- progressr::progressor(along = chunks)
      }
      
      # Process each chunk in parallel
      results <- future.apply::future_lapply(
        chunks,
        function(chunk) {
          result <- process_func(chunk, ...)
          if (parallel_config$progress) p()
          return(result)
        },
        future.packages = c("tidyverse"),
        future.seed = TRUE
      )
      
      # Combine results
      if (is.data.frame(results[[1]])) {
        combined_results <- dplyr::bind_rows(results)
      } else {
        combined_results <- results
      }
      
      return(combined_results)
    }
  }
  
  # If not a list or dataframe, process as is
  return(process_func(data, ...))
}

#' Parallelize data interpolation operations
#'
#' @param data Dataframe containing time series data to interpolate
#' @param interpolate_func Function to apply for interpolation
#' @param id_vars Vector of ID variables that uniquely identify time series
#' @param time_var Name of the time/date variable
#' @param value_vars Vector of value variables to interpolate, or NULL for all non-id/time vars
#' @param ... Additional arguments to pass to interpolate_func
#' @param parallel_config Parallel configuration from setup_parallel_environment
#'
#' @return Dataframe with interpolated values
parallel_interpolate_data <- function(
    data,
    interpolate_func,
    id_vars,
    time_var,
    value_vars = NULL,
    ...,
    parallel_config = setup_parallel_environment()
) {
  # Validate inputs
  if (!is.data.frame(data)) {
    stop("Input must be a dataframe")
  }
  
  if (!all(c(id_vars, time_var) %in% names(data))) {
    stop("All ID variables and time variable must exist in the dataframe")
  }
  
  # Determine value variables if not specified
  if (is.null(value_vars)) {
    value_vars <- setdiff(names(data), c(id_vars, time_var))
  } else if (!all(value_vars %in% names(data))) {
    stop("All value variables must exist in the dataframe")
  }
  
  # Create a unique series ID for each time series
  data$.__series_id <- interaction(data[id_vars], drop = TRUE)
  
  # Split data by series ID
  series_list <- split(data, data$.__series_id)
  
  # Set up progress reporting
  if (parallel_config$progress) {
    p <- progressr::progressor(along = series_list)
  }
  
  # Interpolate each time series in parallel
  results <- future.apply::future_lapply(
    series_list,
    function(series_data) {
      # Sort by time variable
      series_data <- series_data[order(series_data[[time_var]]), ]
      
      # Apply interpolation function
      result <- interpolate_func(
        series_data,
        time_var = time_var,
        value_vars = value_vars,
        ...
      )
      
      if (parallel_config$progress) p()
      return(result)
    },
    future.packages = c("tidyverse"),
    future.seed = TRUE
  )
  
  # Combine results
  combined_results <- dplyr::bind_rows(results)
  
  # Remove temporary series ID
  combined_results$.__series_id <- NULL
  
  return(combined_results)
}

#' Parallelize map generation operations
#'
#' @param variables Vector of variable names to map
#' @param years Vector of years to map
#' @param map_func Function to apply for map generation
#' @param ... Additional arguments to pass to map_func
#' @param parallel_config Parallel configuration from setup_parallel_environment
#'
#' @return List of map generation results
parallel_generate_maps <- function(
    variables,
    years,
    map_func,
    ...,
    parallel_config = setup_parallel_environment()
) {
  # Create combinations of variables and years
  map_tasks <- expand.grid(
    variable = variables,
    year = years,
    stringsAsFactors = FALSE
  )
  
  # Set up progress reporting
  if (parallel_config$progress) {
    p <- progressr::progressor(along = seq_len(nrow(map_tasks)))
  }
  
  # Generate maps in parallel
  map_results <- future.apply::future_lapply(
    seq_len(nrow(map_tasks)),
    function(i) {
      task <- map_tasks[i, ]
      
      # Create a unique task name
      task_name <- paste0(task$variable, "_", task$year)
      
      # Apply map function
      result <- tryCatch({
        map_result <- map_func(
          variable = task$variable,
          year = task$year,
          ...
        )
        list(task_name = task_name, result = map_result, error = NULL)
      }, error = function(e) {
        list(task_name = task_name, result = NULL, error = e$message)
      })
      
      if (parallel_config$progress) p()
      return(result)
    },
    future.packages = c("tidyverse", "sf"),
    future.seed = TRUE
  )
  
  # Check for errors
  errors <- sapply(map_results, function(x) !is.null(x$error))
  if (any(errors)) {
    warning(sprintf("Errors occurred in %d of %d map generation tasks", sum(errors), length(map_results)))
    for (i in which(errors)) {
      warning(sprintf("Error generating map %s: %s", map_results[[i]]$task_name, map_results[[i]]$error))
    }
  }
  
  # Organize results
  results_list <- lapply(map_results, function(x) x$result)
  names(results_list) <- sapply(map_results, function(x) x$task_name)
  
  return(results_list)
}

#' Benchmark parallel processing performance
#'
#' @param data Dataframe to use for benchmarking
#' @param func Function to benchmark
#' @param ... Additional arguments to pass to func
#' @param strategies Vector of parallelization strategies to test
#' @param workers_options Vector of worker counts to test
#'
#' @return Benchmark results
benchmark_parallel_performance <- function(
    data,
    func,
    ...,
    strategies = c("sequential", "multisession", "multicore", "cluster"),
    workers_options = c(1, 2, 4, 8)
) {
  # Filter out multicore on Windows (not supported)
  if (Sys.info()["sysname"] == "Windows") {
    strategies <- setdiff(strategies, "multicore")
  }
  
  # Create benchmark scenarios
  scenarios <- expand.grid(
    strategy = strategies,
    workers = workers_options,
    stringsAsFactors = FALSE
  )
  
  # Remove invalid combinations
  scenarios <- scenarios[!(scenarios$strategy == "sequential" & scenarios$workers > 1), ]
  
  # Prepare benchmark expressions
  bench_exprs <- list()
  
  # Add sequential version for baseline
  bench_exprs[["sequential"]] <- bquote(
    func(data, ..., parallel_config = setup_parallel_environment(workers = 1, strategy = "sequential", progress = FALSE))
  )
  
  # Add parallel versions
  for (i in seq_len(nrow(scenarios))) {
    if (scenarios$strategy[i] == "sequential") next
    
    # Create a unique name for this scenario
    scenario_name <- sprintf("%s_%d", scenarios$strategy[i], scenarios$workers[i])
    
    # Create the benchmark expression
    bench_exprs[[scenario_name]] <- bquote(
      func(data, ..., 
           parallel_config = setup_parallel_environment(
             workers = .(scenarios$workers[i]), 
             strategy = .(scenarios$strategy[i]),
             progress = FALSE
           )
      )
    )
  }
  
  # Run benchmarks
  results <- bench::mark(
    exprs = bench_exprs,
    iterations = 3,
    check = FALSE
  )
  
  # Clean up parallel environment after benchmarking
  cleanup_parallel_environment()
  
  # Return results
  return(results)
}

#' Utility function for determining optimal parallel configuration
#'
#' @param data Sample data to use for testing
#' @param test_func Function to test with
#' @param ... Additional arguments to pass to test_func
#'
#' @return List with optimal configuration settings
optimize_parallel_settings <- function(
    data,
    test_func,
    ...
) {
  message("Testing parallel configurations to determine optimal settings...")
  
  # Get system info
  sys_info <- list(
    os = Sys.info()["sysname"],
    cores = parallel::detectCores(logical = TRUE),
    memory = NA
  )
  
  # Try to detect memory
  try({
    if (sys_info$os == "Linux") {
      sys_info$memory <- as.numeric(system("awk '/MemTotal/ {print $2}' /proc/meminfo", intern = TRUE)) / 1024^2
    } else if (sys_info$os == "Darwin") {
      mem_str <- system("sysctl hw.memsize", intern = TRUE)
      mem_bytes <- as.numeric(sub("hw.memsize: ", "", mem_str))
      sys_info$memory <- mem_bytes / 1024^3
    } else if (sys_info$os == "Windows") {
      mem_str <- system("wmic OS get TotalVisibleMemorySize /Value", intern = TRUE)
      mem_kb <- as.numeric(sub("TotalVisibleMemorySize=", "", mem_str[grep("TotalVisibleMemorySize", mem_str)]))
      sys_info$memory <- mem_kb / 1024^2
    }
  }, silent = TRUE)
  
  # Determine strategies to test based on OS
  strategies <- "multisession"
  if (sys_info$os == "Linux" || sys_info$os == "Darwin") {
    strategies <- c("multisession", "multicore")
  }
  
  # Determine worker counts to test
  max_cores <- min(sys_info$cores, 16)
  worker_options <- unique(c(1, 2, min(4, max_cores), min(8, max_cores), max_cores))
  
  # Run benchmarks
  bench_results <- benchmark_parallel_performance(
    data = data,
    func = test_func,
    ...,
    strategies = strategies,
    workers_options = worker_options
  )
  
  # Find the fastest configuration
  fastest_idx <- which.min(bench_results$median)
  fastest_config <- bench_results$expression[[fastest_idx]]
  
  # Extract parameters from the fastest config
  config_str <- deparse(fastest_config)
  
  # Extract strategy and workers using regex
  strategy_match <- regexpr("strategy\\s*=\\s*['\"]([^'\"]+)['\"]", config_str)
  workers_match <- regexpr("workers\\s*=\\s*([0-9]+)", config_str)
  
  if (strategy_match > 0) {
    strategy_str <- regmatches(config_str, strategy_match)
    strategy <- sub(".*['\"]([^'\"]+)['\"].*", "\\1", strategy_str)
  } else {
    strategy <- "multisession"
  }
  
  if (workers_match > 0) {
    workers_str <- regmatches(config_str, workers_match)
    workers <- as.numeric(sub(".*=\\s*([0-9]+).*", "\\1", workers_str))
  } else {
    workers <- max(1, sys_info$cores - 1)
  }
  
  # Determine chunk size
  if (workers == 1) {
    chunk_size <- 100
  } else {
    chunk_size <- max(1, ceiling(sqrt(workers)) * 10)
  }
  
  # Determine memory limit
  if (is.na(sys_info$memory)) {
    memory_per_worker <- 2
  } else {
    memory_per_worker <- max(1, floor((sys_info$memory * 0.8) / workers))
  }
  
  # Create optimal config
  optimal_config <- list(
    workers = workers,
    strategy = strategy,
    chunk_size = chunk_size,
    memory_limit = memory_per_worker,
    progress = TRUE
  )
  
  message("Optimal parallel configuration determined:")
  message(sprintf("- Strategy: %s", optimal_config$strategy))
  message(sprintf("- Workers: %d", optimal_config$workers))
  message(sprintf("- Chunk size: %d", optimal_config$chunk_size))
  message(sprintf("- Memory per worker: %d GB", optimal_config$memory_limit))
  
  # Benchmark comparison
  message("\nPerformance comparison:")
  sequential_time <- bench_results$median[1]
  optimal_time <- bench_results$median[fastest_idx]
  speedup <- sequential_time / optimal_time
  
  message(sprintf("- Sequential: %.2f seconds", sequential_time))
  message(sprintf("- Optimal parallel: %.2f seconds", optimal_time))
  message(sprintf("- Speedup: %.1fx", speedup))
  
  return(optimal_config)
}

# If script is run directly, demonstrate functionality
if (!interactive()) {
  # Create sample data frame
  set.seed(123)
  n_counties <- 3000
  n_years <- 20
  
  sample_data <- expand.grid(
    county_id = sprintf("C%05d", 1:n_counties),
    year = 2000:(2000 + n_years - 1),
    stringsAsFactors = FALSE
  )
  
  sample_data$value1 <- runif(nrow(sample_data)) * 100
  sample_data$value2 <- runif(nrow(sample_data)) * 50
  
  # Introduce NAs for testing interpolation
  na_indices <- sample(1:nrow(sample_data), size = nrow(sample_data) * 0.2)
  sample_data$value1[na_indices] <- NA
  
  # Sample processing function
  process_func <- function(data, multiplier = 1) {
    Sys.sleep(0.01)  # Simulate processing time
    data$processed_value <- data$value1 * multiplier
    return(data)
  }
  
  # Sample interpolation function
  interpolate_func <- function(data, time_var, value_vars, method = "linear") {
    # Sort by time
    data <- data[order(data[[time_var]]), ]
    
    # Perform interpolation for each value variable
    for (var in value_vars) {
      if (any(is.na(data[[var]]))) {
        x <- data[[time_var]]
        y <- data[[var]]
        
        if (method == "linear") {
          # Linear interpolation
          data[[var]] <- approx(x, y, xout = x, rule = 2)$y
        } else if (method == "spline") {
          # Spline interpolation
          data[[var]] <- spline(x, y, xout = x, method = "natural")$y
        }
      }
    }
    
    return(data)
  }
  
  # Test parallel processing
  cat("Testing parallel processing...\n")
  parallel_config <- setup_parallel_environment(workers = 2)
  
  results <- parallel_process_data(
    sample_data,
    process_func,
    by = "year",
    multiplier = 2,
    parallel_config = parallel_config
  )
  
  cat("Processed", nrow(results), "rows\n")
  
  # Test parallel interpolation
  cat("\nTesting parallel interpolation...\n")
  
  interp_results <- parallel_interpolate_data(
    sample_data,
    interpolate_func,
    id_vars = "county_id",
    time_var = "year",
    value_vars = c("value1", "value2"),
    method = "linear",
    parallel_config = parallel_config
  )
  
  cat("Interpolated", sum(is.na(sample_data$value1)), "missing values\n")
  
  # Clean up
  cleanup_parallel_environment()
  
  cat("\nParallel processing test completed successfully\n")
}