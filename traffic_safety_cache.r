#!/usr/bin/env Rscript

# Traffic Safety Cache Module
# This module provides advanced caching capabilities for traffic safety data
# including intelligent cache invalidation, compression, and performance optimization

# Required packages
required_packages <- c(
  "digest",
  "R6",
  "fs"
)

# Load required packages
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    message(paste("Required package", pkg, "is not installed."))
    message("Please run 'Rscript R/install_packages.r' first.")
    # Don't stop execution, just warn and continue with reduced functionality
  }
}

#' Safe implementation of gunzip that handles errors gracefully
#'
#' @param src Source file path
#' @param destname Destination file path
#' @param remove Whether to remove the source file after extraction
#' @return Path to the destination file if successful, NULL otherwise
safe_gunzip <- function(src, destname, remove = FALSE) {
  result <- tryCatch({
    R.utils::gunzip(src, destname = destname, remove = remove, overwrite = TRUE)
    return(destname)
  }, error = function(e) {
    message(paste("Error unzipping file:", e$message))
    return(NULL)
  })
  
  return(result)
}

#' Safe implementation of gzip that handles errors gracefully
#'
#' @param src Source file path
#' @param destname Destination file path 
#' @param remove Whether to remove the source file after compression
#' @return Path to the destination file if successful, NULL otherwise
safe_gzip <- function(src, destname, remove = FALSE) {
  result <- tryCatch({
    R.utils::gzip(src, destname = destname, remove = remove, overwrite = TRUE)
    return(destname)
  }, error = function(e) {
    message(paste("Error zipping file:", e$message))
    return(NULL)
  })
  
  return(result)
}

#' Execute a function with optimized caching
#'
#' @param func Function to execute
#' @param cache_dir Directory for caching
#' @param cache_ttl Time-to-live for cache in seconds (default 7 days)
#' @param cache_key Optional custom cache key
#' @param refresh Force refresh ignoring cache
#' @return Result of the function execution
with_optimized_cache <- function(func, 
                                cache_dir = "data/cache", 
                                cache_ttl = 7*24*60*60,
                                cache_key = NULL,
                                refresh = FALSE) {
  # Create the cache directory if it doesn't exist
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Create a cache key based on the function body if not provided
  if (is.null(cache_key)) {
    func_str <- deparse(func)
    cache_key <- digest::digest(func_str, algo = "md5")
  }
  
  # Define the cache file path
  cache_file <- file.path(cache_dir, paste0("func_cache_", cache_key, ".rds"))
  cache_meta_file <- file.path(cache_dir, paste0("func_cache_meta_", cache_key, ".rds"))
  
  # Check if we should use cache
  use_cache <- FALSE
  
  if (!refresh && file.exists(cache_file) && file.exists(cache_meta_file)) {
    # Read the metadata
    meta <- tryCatch({
      readRDS(cache_meta_file)
    }, error = function(e) {
      message("Error reading cache metadata. Ignoring cache.")
      return(NULL)
    })
    
    if (!is.null(meta) && is.list(meta) && "timestamp" %in% names(meta)) {
      # Check if cache is still valid
      cache_age <- as.numeric(Sys.time()) - as.numeric(meta$timestamp)
      if (cache_age < cache_ttl) {
        use_cache <- TRUE
      }
    }
  }
  
  # Return cached result if valid
  if (use_cache) {
    message("Using cached result")
    return(tryCatch({
      readRDS(cache_file)
    }, error = function(e) {
      message("Error reading cache. Executing function instead.")
      result <- func()
      
      # Update cache after execution
      saveRDS(result, cache_file)
      saveRDS(list(timestamp = Sys.time()), cache_meta_file)
      
      return(result)
    }))
  }
  
  # Execute the function if not using cache
  message("Executing function (not using cache)")
  result <- func()
  
  # Cache the result
  tryCatch({
    saveRDS(result, cache_file)
    saveRDS(list(timestamp = Sys.time()), cache_meta_file)
  }, error = function(e) {
    message(paste("Error caching result:", e$message))
  })
  
  return(result)
}

#' Cache traffic safety data
#'
#' @param data Traffic safety data to cache
#' @param cache_dir Directory for caching
#' @param years Years included in the data
#' @return List with cache operation status
cache_traffic_safety_data <- function(data, cache_dir = "data/cache", years = NULL) {
  # Create cache directory if it doesn't exist
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Create traffic safety subdirectory
  ts_cache_dir <- file.path(cache_dir, "traffic_safety")
  if (!dir.exists(ts_cache_dir)) {
    dir.create(ts_cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Generate appropriate cache file name
  if (is.null(years)) {
    # Extract years from data if not provided
    if ("year" %in% names(data)) {
      years <- sort(unique(data$year))
    } else {
      # Default to current year if no years found
      years <- as.numeric(format(Sys.Date(), "%Y"))
    }
  }
  
  # Create cache file path
  cache_file_base <- file.path(ts_cache_dir, 
                              paste0("traffic_safety_", 
                                     min(years), "_", max(years)))
  cache_file_rds <- paste0(cache_file_base, ".rds")
  cache_file_gz <- paste0(cache_file_base, ".rds.gz")
  
  # Create metadata
  metadata <- list(
    timestamp = Sys.time(),
    years = years,
    rows = nrow(data),
    columns = ncol(data),
    column_names = names(data)
  )
  
  # Save metadata
  meta_file <- file.path(ts_cache_dir, 
                        paste0("traffic_safety_", 
                               min(years), "_", max(years), "_meta.rds"))
  
  tryCatch({
    # Save the data
    saveRDS(data, cache_file_rds)
    
    # Compress the data
    safe_gzip(cache_file_rds, cache_file_gz, remove = TRUE)
    
    # Save metadata
    saveRDS(metadata, meta_file)
    
    return(list(
      success = TRUE,
      message = paste("Data cached successfully for years", min(years), "to", max(years)),
      file = cache_file_gz,
      metadata = metadata
    ))
  }, error = function(e) {
    return(list(
      success = FALSE,
      message = paste("Error caching data:", e$message),
      file = NULL,
      metadata = NULL
    ))
  })
}

#' Retrieve traffic safety data from cache
#'
#' @param cache_dir Directory for caching
#' @param years Years to retrieve
#' @return List with retrieval status and data
retrieve_traffic_safety_data <- function(cache_dir = "data/cache", years = NULL) {
  # Path to traffic safety cache
  ts_cache_dir <- file.path(cache_dir, "traffic_safety")
  
  if (!dir.exists(ts_cache_dir)) {
    return(list(
      success = FALSE,
      data = NULL,
      message = "Cache directory does not exist"
    ))
  }
  
  # If no years specified, get the most recent cache file
  if (is.null(years)) {
    # Look for all metadata files
    meta_files <- list.files(ts_cache_dir, pattern = "_meta.rds$", full.names = TRUE)
    
    if (length(meta_files) == 0) {
      return(list(
        success = FALSE,
        data = NULL,
        message = "No cached data found"
      ))
    }
    
    # Read metadata to find the most recent
    metadata_list <- lapply(meta_files, function(f) {
      tryCatch({
        meta <- readRDS(f)
        meta$file <- sub("_meta.rds$", ".rds.gz", f)
        return(meta)
      }, error = function(e) NULL)
    })
    
    # Remove NULL entries
    metadata_list <- metadata_list[!sapply(metadata_list, is.null)]
    
    if (length(metadata_list) == 0) {
      return(list(
        success = FALSE,
        data = NULL,
        message = "No valid metadata found in cache"
      ))
    }
    
    # Sort by timestamp (newest first)
    timestamps <- sapply(metadata_list, function(m) as.numeric(m$timestamp))
    metadata <- metadata_list[[which.max(timestamps)]]
    
    # Get the cache file path
    cache_file <- metadata$file
    years <- metadata$years
  } else {
    # Look for cache file matching the specified years
    cache_file_base <- file.path(ts_cache_dir, 
                                paste0("traffic_safety_", 
                                       min(years), "_", max(years)))
    cache_file <- paste0(cache_file_base, ".rds.gz")
    
    # Check if uncompressed version exists
    uncompressed_file <- paste0(cache_file_base, ".rds")
    if (!file.exists(cache_file) && file.exists(uncompressed_file)) {
      cache_file <- uncompressed_file
    }
    
    # Check if exact match exists
    if (!file.exists(cache_file)) {
      # Look for any cache file that might include the years we want
      all_cache_files <- list.files(ts_cache_dir, pattern = "\\.rds(\\.gz)?$", full.names = TRUE)
      all_cache_files <- all_cache_files[!grepl("_meta\\.rds$", all_cache_files)]
      
      if (length(all_cache_files) == 0) {
        return(list(
          success = FALSE,
          data = NULL,
          message = "No cached data found"
        ))
      }
      
      # Parse the year ranges from filenames
      year_ranges <- lapply(all_cache_files, function(f) {
        # Extract year range from filename
        basename_f <- basename(f)
        year_match <- regexpr("traffic_safety_([0-9]+)_([0-9]+)", basename_f)
        if (year_match > 0) {
          start_year <- as.numeric(sub(".*traffic_safety_([0-9]+)_([0-9]+).*", "\\1", basename_f))
          end_year <- as.numeric(sub(".*traffic_safety_([0-9]+)_([0-9]+).*", "\\2", basename_f))
          return(list(file = f, start = start_year, end = end_year))
        }
        return(NULL)
      })
      
      # Remove NULL entries
      year_ranges <- year_ranges[!sapply(year_ranges, is.null)]
      
      if (length(year_ranges) == 0) {
        return(list(
          success = FALSE,
          data = NULL,
          message = "No valid cache files found"
        ))
      }
      
      # Find files that cover all requested years
      covering_files <- sapply(year_ranges, function(yr) {
        all(years >= yr$start & years <= yr$end)
      })
      
      if (any(covering_files)) {
        # Use the first file that covers all years
        cache_file <- year_ranges[[which(covering_files)[1]]]$file
      } else {
        return(list(
          success = FALSE,
          data = NULL,
          message = paste("No cache file covers all requested years:", paste(years, collapse = ", "))
        ))
      }
    }
  }
  
  # We have a cache file to use, try to read it
  temp_file <- NULL
  data <- tryCatch({
    if (grepl("\\.gz$", cache_file)) {
      # Decompress to a temporary file
      temp_file <- tempfile(fileext = ".rds")
      safe_gunzip(cache_file, temp_file)
      data <- readRDS(temp_file)
      file.remove(temp_file)
    } else {
      # Read directly
      data <- readRDS(cache_file)
    }
    
    # If years were specified, filter to just those years
    if (!is.null(years) && "year" %in% names(data)) {
      data <- data[data$year %in% years, ]
    }
    
    return(data)
  }, error = function(e) {
    # Clean up temp file if it exists
    if (!is.null(temp_file) && file.exists(temp_file)) {
      file.remove(temp_file)
    }
    
    message(paste("Error reading cache file:", e$message))
    return(NULL)
  })
  
  if (is.null(data)) {
    return(list(
      success = FALSE,
      data = NULL,
      message = paste("Failed to read cache file:", cache_file)
    ))
  }
  
  # Add cache metadata attributes
  attr(data, "cache_info") <- list(
    source = cache_file,
    cache_date = file.info(cache_file)$mtime,
    years = years
  )
  
  return(list(
    success = TRUE,
    data = data,
    message = paste("Successfully retrieved data from cache for years", 
                   paste(sort(unique(data$year)), collapse = ", "))
  ))
}

#' Validate cache
#'
#' @param cache_dir Directory for caching
#' @param years Years to validate
#' @return List with validation status
validate_cache <- function(cache_dir = "data/cache", years = NULL) {
  # Path to traffic safety cache
  ts_cache_dir <- file.path(cache_dir, "traffic_safety")
  
  if (!dir.exists(ts_cache_dir)) {
    return(list(
      valid = FALSE,
      message = "Cache directory does not exist"
    ))
  }
  
  # If years are provided, look for a specific cache file
  if (!is.null(years)) {
    cache_file_base <- file.path(ts_cache_dir, 
                                paste0("traffic_safety_", 
                                       min(years), "_", max(years)))
    cache_file <- paste0(cache_file_base, ".rds.gz")
    
    # Check if uncompressed version exists
    uncompressed_file <- paste0(cache_file_base, ".rds")
    if (!file.exists(cache_file) && file.exists(uncompressed_file)) {
      cache_file <- uncompressed_file
    }
    
    # Check if metadata exists
    meta_file <- paste0(cache_file_base, "_meta.rds")
    
    if (!file.exists(cache_file)) {
      return(list(
        valid = FALSE,
        message = paste("Cache file for years", min(years), "to", max(years), "not found")
      ))
    }
    
    if (!file.exists(meta_file)) {
      return(list(
        valid = TRUE,
        message = paste("Cache file exists but missing metadata. Treat as valid.")
      ))
    }
    
    # Read metadata to verify
    metadata <- tryCatch({
      readRDS(meta_file)
    }, error = function(e) NULL)
    
    if (is.null(metadata)) {
      return(list(
        valid = TRUE,
        message = paste("Cache file exists but metadata could not be read. Treat as valid.")
      ))
    }
    
    # Check if cache is current
    cache_age <- difftime(Sys.time(), metadata$timestamp, units = "days")
    
    # Determine if cache should be considered valid
    # For this example, we'll consider anything older than 30 days invalid
    if (cache_age > 30) {
      return(list(
        valid = FALSE,
        message = paste("Cache is too old:", round(cache_age, 1), "days")
      ))
    }
    
    return(list(
      valid = TRUE,
      message = paste("Cache is valid, age:", round(cache_age, 1), "days")
    ))
  }
  
  # If no years specified, check if any cache exists
  cache_files <- list.files(ts_cache_dir, pattern = "\\.rds(\\.gz)?$", full.names = TRUE)
  cache_files <- cache_files[!grepl("_meta\\.rds$", cache_files)]
  
  if (length(cache_files) == 0) {
    return(list(
      valid = FALSE,
      message = "No cache files found"
    ))
  }
  
  # Check the most recent cache file
  file_info <- file.info(cache_files)
  most_recent <- rownames(file_info)[which.max(file_info$mtime)]
  
  # Check age of most recent cache
  cache_age <- difftime(Sys.time(), file_info[most_recent, "mtime"], units = "days")
  
  # Determine if cache should be considered valid
  if (cache_age > 30) {
    return(list(
      valid = FALSE,
      message = paste("Most recent cache is too old:", round(cache_age, 1), "days")
    ))
  }
  
  return(list(
    valid = TRUE,
    message = paste("Cache is valid, most recent age:", round(cache_age, 1), "days"),
    file = most_recent
  ))
}

#' Clean traffic safety cache
#'
#' @param cache_dir Directory for caching
#' @param max_age_days Maximum age in days to keep files
#' @return List with cleanup status
clean_traffic_safety_cache <- function(cache_dir = "data/cache", max_age_days = 30) {
  # Path to traffic safety cache
  ts_cache_dir <- file.path(cache_dir, "traffic_safety")
  
  if (!dir.exists(ts_cache_dir)) {
    return(list(
      success = FALSE,
      message = "Cache directory does not exist"
    ))
  }
  
  # Get all cache files
  all_files <- list.files(ts_cache_dir, full.names = TRUE)
  
  if (length(all_files) == 0) {
    return(list(
      success = TRUE,
      message = "No files to clean up",
      removed = 0
    ))
  }
  
  # Get file info
  file_info <- file.info(all_files)
  
  # Determine which files are too old
  too_old <- difftime(Sys.time(), file_info$mtime, units = "days") > max_age_days
  files_to_remove <- all_files[too_old]
  
  # Remove old files
  if (length(files_to_remove) > 0) {
    removed <- sapply(files_to_remove, function(f) {
      tryCatch({
        file.remove(f)
        return(TRUE)
      }, error = function(e) {
        message(paste("Error removing", f, ":", e$message))
        return(FALSE)
      })
    })
    
    return(list(
      success = TRUE,
      message = paste("Removed", sum(removed), "old cache files out of", length(files_to_remove), "identified"),
      removed = sum(removed),
      attempted = length(files_to_remove)
    ))
  }
  
  return(list(
    success = TRUE,
    message = "No files needed cleaning up",
    removed = 0
  ))
}

# Let the pipeline know the module is loaded
cat("[INFO] Traffic safety cache module loaded successfully\n")