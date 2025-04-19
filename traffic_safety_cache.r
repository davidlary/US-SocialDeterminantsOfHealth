#!/usr/bin/env Rscript

# Traffic Safety Cache Management
# This script provides enhanced caching strategies for traffic safety data
# Including smart expiry, incremental updates, and cache diagnostics

# Required packages
required_packages <- c(
  "tidyverse",
  "digest",
  "jsonlite",
  "lubridate",
  "R.utils"
)

# Load required packages
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    message(paste("Required package", pkg, "is not installed."))
    message("Please run 'Rscript install_packages.r' first.")
    # Don't stop execution, just warn and continue with reduced functionality
  }
}

#' Class for managing traffic safety data cache
#' @export
TrafficSafetyCache <- R6::R6Class(
  "TrafficSafetyCache",
  
  public = list(
    #' @field base_dir Base directory for cache storage
    base_dir = NULL,
    
    #' @field config Caching configuration settings
    config = list(
      max_age = list(
        fars = 30,         # NHTSA FARS data refreshed monthly (days)
        cdc = 90,          # CDC WONDER updated quarterly (days)
        census = 365,      # Census population once per year (days)
        shapefile = 365,   # County boundaries change rarely (days)
        processed = 7      # Final processed data (days)
      ),
      compression = TRUE,  # Whether to compress cache files
      log_file = "logs/cache.log",
      max_cache_size_mb = 1000,  # Maximum cache size (MB)
      priority = c(        # Files to keep when pruning, in order of importance
        "processed",
        "fars",
        "cdc",
        "census", 
        "shapefile"
      )
    ),
    
    #' @field metadata Cache metadata tracking
    metadata = list(
      files = data.frame(),
      last_pruned = NULL
    ),
    
    #' @description Create a new cache manager
    #' @param base_dir Base directory for cache storage
    #' @param config Optional custom configuration
    initialize = function(base_dir = "data/cache", config = NULL) {
      self$base_dir <- base_dir
      
      # Apply custom config if provided
      if (!is.null(config)) {
        self$config <- modifyList(self$config, config)
      }
      
      # Create cache directories
      self$ensure_cache_dirs()
      
      # Load metadata if it exists
      self$load_metadata()
      
      # Log initialization
      self$log_message("Initialized cache manager")
    },
    
    #' @description Ensure all necessary cache directories exist
    ensure_cache_dirs = function() {
      # Create main cache directory if it doesn't exist
      if (!dir.exists(self$base_dir)) {
        dir.create(self$base_dir, recursive = TRUE, showWarnings = FALSE)
      }
      
      # Create subdirectories for different data types
      data_types <- c("traffic_safety", "fars", "cdc", "census", "shapefile")
      for (type in data_types) {
        dir_path <- file.path(self$base_dir, type)
        if (!dir.exists(dir_path)) {
          dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
        }
      }
      
      # Create metadata directory
      metadata_dir <- file.path(self$base_dir, "metadata")
      if (!dir.exists(metadata_dir)) {
        dir.create(metadata_dir, recursive = TRUE, showWarnings = FALSE)
      }
      
      return(invisible(self))
    },
    
    #' @description Load cache metadata from disk
    load_metadata = function() {
      metadata_file <- file.path(self$base_dir, "metadata", "cache_metadata.rds")
      
      if (file.exists(metadata_file)) {
        tryCatch({
          self$metadata <- readRDS(metadata_file)
          self$log_message(paste("Loaded cache metadata with", 
                                nrow(self$metadata$files), "files"))
        }, error = function(e) {
          self$log_message(paste("Error loading cache metadata:", e$message))
          # Initialize empty metadata
          self$metadata <- list(
            files = data.frame(
              path = character(),
              type = character(),
              size_bytes = numeric(),
              created = character(),
              last_accessed = character(),
              hash = character(),
              stringsAsFactors = FALSE
            ),
            last_pruned = NULL
          )
        })
      } else {
        # Initialize empty metadata
        self$metadata <- list(
          files = data.frame(
            path = character(),
            type = character(),
            size_bytes = numeric(),
            created = character(),
            last_accessed = character(),
            hash = character(),
            stringsAsFactors = FALSE
          ),
          last_pruned = NULL
        )
        self$log_message("Initialized new cache metadata")
      }
      
      # Run a scan to make sure metadata is up to date
      self$scan_cache_files(update_existing = FALSE)
      
      return(invisible(self))
    },
    
    #' @description Save cache metadata to disk
    save_metadata = function() {
      metadata_file <- file.path(self$base_dir, "metadata", "cache_metadata.rds")
      
      tryCatch({
        saveRDS(self$metadata, metadata_file)
        self$log_message(paste("Saved cache metadata with", 
                              nrow(self$metadata$files), "files"))
      }, error = function(e) {
        self$log_message(paste("Error saving cache metadata:", e$message))
      })
      
      return(invisible(self))
    },
    
    #' @description Scan cache directory for files and update metadata
    #' @param update_existing Whether to update metadata for existing files
    scan_cache_files = function(update_existing = TRUE) {
      # Get all RDS files in the cache directory
      cache_files <- list.files(
        self$base_dir, 
        pattern = "\\.rds|\\.rds\\.gz$", 
        recursive = TRUE,
        full.names = TRUE
      )
      
      # Filter out the metadata file itself
      cache_files <- cache_files[!grepl("metadata/cache_metadata.rds", cache_files)]
      
      # Process each file
      for (file_path in cache_files) {
        # Check if file already in metadata
        existing_idx <- which(self$metadata$files$path == file_path)
        
        if (length(existing_idx) == 0 || update_existing) {
          # Get file information
          file_info <- file.info(file_path)
          
          # Determine file type
          if (grepl("/fars/", file_path)) {
            file_type <- "fars"
          } else if (grepl("/cdc/", file_path)) {
            file_type <- "cdc"
          } else if (grepl("/census/", file_path)) {
            file_type <- "census"
          } else if (grepl("/shapefile/", file_path)) {
            file_type <- "shapefile"
          } else if (grepl("/traffic_safety/", file_path)) {
            file_type <- "processed"
          } else {
            file_type <- "other"
          }
          
          # Calculate file hash (for change detection) - skip large files
          file_hash <- NA_character_
          if (file_info$size < 10 * 1024 * 1024) {  # Skip files > 10MB
            tryCatch({
              if (grepl("\\.gz$", file_path)) {
                # For compressed files, first decompress if R.utils is available
                if (requireNamespace("R.utils", quietly = TRUE)) {
                  temp_file <- tempfile()
                  R.utils::gunzip(file_path, destname = temp_file, remove = FALSE)
                  file_hash <- digest::digest(temp_file, algo = "md5")
                  unlink(temp_file)
                } else {
                  # R.utils not available, use file path directly
                  file_hash <- digest::digest(file_path, algo = "md5")
                }
              } else {
                file_hash <- digest::digest(file_path, algo = "md5")
              }
            }, error = function(e) {
              file_hash <- NA_character_
            })
          }
          
          # Create or update metadata entry
          file_metadata <- data.frame(
            path = file_path,
            type = file_type,
            size_bytes = file_info$size,
            created = as.character(file_info$ctime),
            last_accessed = as.character(file_info$atime),
            hash = file_hash,
            stringsAsFactors = FALSE
          )
          
          if (length(existing_idx) == 0) {
            # Add new file
            self$metadata$files <- rbind(self$metadata$files, file_metadata)
          } else {
            # Update existing file
            self$metadata$files[existing_idx, ] <- file_metadata
          }
        }
      }
      
      # Remove entries for files that no longer exist
      missing_files <- self$metadata$files$path[!self$metadata$files$path %in% cache_files]
      if (length(missing_files) > 0) {
        self$metadata$files <- self$metadata$files[!self$metadata$files$path %in% missing_files, ]
        self$log_message(paste("Removed", length(missing_files), 
                              "missing files from cache metadata"))
      }
      
      # Save the updated metadata
      self$save_metadata()
      
      return(invisible(self))
    },
    
    #' @description Check if a cached file has expired based on max age
    #' @param file_path Path to the cached file
    #' @param max_age_days Maximum age in days before expiration
    #' @return Logical indicating if file has expired
    is_expired = function(file_path, max_age_days = NULL) {
      # Find file in metadata
      idx <- which(self$metadata$files$path == file_path)
      
      if (length(idx) == 0) {
        # File not in metadata, consider it expired
        return(TRUE)
      }
      
      # Get file type and created date
      file_type <- self$metadata$files$type[idx]
      created_date <- as.POSIXct(self$metadata$files$created[idx])
      
      # Determine max age based on file type if not provided
      if (is.null(max_age_days)) {
        if (file_type %in% names(self$config$max_age)) {
          max_age_days <- self$config$max_age[[file_type]]
        } else {
          max_age_days <- 30  # Default to 30 days
        }
      }
      
      # Check if file age exceeds max age
      file_age_days <- as.numeric(difftime(Sys.time(), created_date, units = "days"))
      return(file_age_days > max_age_days)
    },
    
    #' @description Get cached data file, or return NULL if expired
    #' @param file_path Path to the cached file
    #' @param max_age_days Optional override for max age
    #' @return Cached data if valid, NULL if expired or not found
    get_cached_data = function(file_path, max_age_days = NULL) {
      # Check if file exists and hasn't expired
      if (!file.exists(file_path) || self$is_expired(file_path, max_age_days)) {
        return(NULL)
      }
      
      # Try to load the data
      result <- tryCatch({
        # Check if file is compressed
        if (grepl("\\.gz$", file_path)) {
          # Decompress and read
          readRDS(gzfile(file_path))
        } else {
          # Regular RDS file
          readRDS(file_path)
        }
      }, error = function(e) {
        self$log_message(paste("Error reading cached file:", e$message))
        NULL
      })
      
      # Update last accessed time in metadata
      if (!is.null(result)) {
        idx <- which(self$metadata$files$path == file_path)
        if (length(idx) > 0) {
          self$metadata$files$last_accessed[idx] <- as.character(Sys.time())
          self$save_metadata()
        }
      }
      
      return(result)
    },
    
    #' @description Save data to cache with optional compression
    #' @param data Data to cache
    #' @param file_path Cache file path
    #' @param compress Whether to compress the data
    #' @param type Cache data type for metadata
    #' @return Logical indicating success
    save_to_cache = function(data, file_path, compress = NULL, type = NULL) {
      # Determine if compression should be used
      if (is.null(compress)) {
        compress <- self$config$compression
      }
      
      # Ensure directory exists
      dir_path <- dirname(file_path)
      if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
      }
      
      # Append .gz extension if compressing
      if (compress && !grepl("\\.gz$", file_path)) {
        file_path <- paste0(file_path, ".gz")
      }
      
      # Try to save the data
      success <- tryCatch({
        if (compress) {
          # Save compressed with gzfile connection
          saveRDS(data, gzfile(file_path))
        } else {
          # Regular RDS save
          saveRDS(data, file_path)
        }
        TRUE
      }, error = function(e) {
        self$log_message(paste("Error saving to cache:", e$message))
        FALSE
      })
      
      # Update metadata if save was successful
      if (success) {
        # Check total cache size and prune if needed
        self$check_and_prune_cache()
        
        # Get file information
        file_info <- file.info(file_path)
        
        # Calculate file hash
        file_hash <- NA_character_
        if (file_info$size < 10 * 1024 * 1024) {  # Skip hash for large files
          tryCatch({
            if (compress) {
              # For compressed files, can't directly hash, use file info
              file_hash <- digest::digest(list(file_info$size, file_info$mtime), algo = "md5")
            } else {
              file_hash <- digest::digest(file_path, algo = "md5")
            }
          }, error = function(e) {
            file_hash <- NA_character_
          })
        }
        
        # Determine file type if not provided
        if (is.null(type)) {
          if (grepl("/fars/", file_path)) {
            type <- "fars"
          } else if (grepl("/cdc/", file_path)) {
            type <- "cdc"
          } else if (grepl("/census/", file_path)) {
            type <- "census"
          } else if (grepl("/shapefile/", file_path)) {
            type <- "shapefile"
          } else if (grepl("/traffic_safety/", file_path)) {
            type <- "processed"
          } else {
            type <- "other"
          }
        }
        
        # Update or add metadata entry
        idx <- which(self$metadata$files$path == file_path)
        file_metadata <- data.frame(
          path = file_path,
          type = type,
          size_bytes = file_info$size,
          created = as.character(file_info$ctime),
          last_accessed = as.character(file_info$atime),
          hash = file_hash,
          stringsAsFactors = FALSE
        )
        
        if (length(idx) == 0) {
          # Add new entry
          self$metadata$files <- rbind(self$metadata$files, file_metadata)
        } else {
          # Update existing entry
          self$metadata$files[idx, ] <- file_metadata
        }
        
        # Save metadata
        self$save_metadata()
        
        self$log_message(paste("Saved", type, "data to cache:", basename(file_path), 
                              "(", round(file_info$size / 1024 / 1024, 2), "MB )"))
      }
      
      return(invisible(success))
    },
    
    #' @description Check total cache size and prune if exceeds limit
    check_and_prune_cache = function() {
      # Calculate current total cache size
      total_size_bytes <- sum(self$metadata$files$size_bytes, na.rm = TRUE)
      total_size_mb <- total_size_bytes / (1024 * 1024)
      
      # Check if cache size exceeds limit
      if (total_size_mb > self$config$max_cache_size_mb) {
        self$log_message(paste("Cache size (", round(total_size_mb, 2), 
                              "MB ) exceeds limit (", self$config$max_cache_size_mb, 
                              "MB ). Pruning..."))
        
        # Calculate how much to prune
        excess_mb <- total_size_mb - (self$config$max_cache_size_mb * 0.8)  # Prune to 80% of limit
        excess_bytes <- excess_mb * 1024 * 1024
        
        # Prune cache
        self$prune_cache(bytes_to_remove = excess_bytes)
      }
      
      return(invisible(self))
    },
    
    #' @description Prune cache files to free up space
    #' @param bytes_to_remove Target number of bytes to remove
    prune_cache = function(bytes_to_remove = NULL) {
      # If no specific amount to remove, default to 20% of current total
      if (is.null(bytes_to_remove)) {
        total_size <- sum(self$metadata$files$size_bytes, na.rm = TRUE)
        bytes_to_remove <- total_size * 0.2  # Remove 20% of current cache size
      }
      
      # Start with files not in the priority list
      all_types <- unique(self$metadata$files$type)
      non_priority_types <- all_types[!all_types %in% self$config$priority]
      
      # Order files for pruning
      files_to_check <- rbind(
        # First, files not in priority list, ordered by oldest access time
        self$metadata$files %>%
          filter(type %in% non_priority_types) %>%
          arrange(last_accessed),
        
        # Then files in priority list, in reverse priority order
        lapply(rev(self$config$priority), function(p_type) {
          self$metadata$files %>%
            filter(type == p_type) %>%
            arrange(last_accessed)
        }) %>% bind_rows()
      )
      
      # Prune files until we've removed enough bytes
      bytes_removed <- 0
      pruned_files <- list()
      
      for (i in 1:nrow(files_to_check)) {
        if (bytes_removed >= bytes_to_remove) break
        
        file_path <- files_to_check$path[i]
        file_size <- files_to_check$size_bytes[i]
        
        # Skip recently accessed processed files
        if (files_to_check$type[i] == "processed") {
          last_access <- as.POSIXct(files_to_check$last_accessed[i])
          days_since_access <- as.numeric(difftime(Sys.time(), last_access, units = "days"))
          
          # Skip if accessed in the last day
          if (days_since_access < 1) next
        }
        
        # Try to remove the file
        if (file.exists(file_path)) {
          if (unlink(file_path) == 0) {
            bytes_removed <- bytes_removed + file_size
            pruned_files <- c(pruned_files, file_path)
            self$log_message(paste("Pruned cache file:", basename(file_path), 
                                  "(", round(file_size / 1024 / 1024, 2), "MB )"))
          }
        }
      }
      
      # Update metadata - remove pruned files
      if (length(pruned_files) > 0) {
        self$metadata$files <- self$metadata$files[!self$metadata$files$path %in% pruned_files, ]
        self$metadata$last_pruned <- Sys.time()
        self$save_metadata()
      }
      
      # Log pruning summary
      self$log_message(paste("Pruned", length(pruned_files), "files, freed up", 
                            round(bytes_removed / 1024 / 1024, 2), "MB of cache space"))
      
      return(invisible(self))
    },
    
    #' @description Save incremental update to a cache file
    #' @param new_data New data to add or update
    #' @param file_path Cache file path
    #' @param key_cols Columns that uniquely identify records for merging
    #' @param update_existing Whether to update existing records
    #' @param compress Whether to compress the data
    #' @param type Cache data type for metadata
    #' @return Logical indicating success
    save_incremental_update = function(new_data, file_path, key_cols, 
                                    update_existing = TRUE, compress = NULL, type = NULL) {
      # Check if new_data is a data frame
      if (!is.data.frame(new_data)) {
        self$log_message("Error: new_data must be a data frame for incremental updates")
        return(FALSE)
      }
      
      # Check if key_cols are present in new_data
      if (!all(key_cols %in% names(new_data))) {
        self$log_message(paste("Error: key columns", paste(key_cols, collapse=", "), 
                              "must exist in new_data"))
        return(FALSE)
      }
      
      # Try to load existing data
      existing_data <- self$get_cached_data(file_path)
      
      if (is.null(existing_data)) {
        # No existing data, just save new data directly
        return(self$save_to_cache(new_data, file_path, compress, type))
      } else {
        # Check if existing data is a data frame
        if (!is.data.frame(existing_data)) {
          self$log_message("Error: existing data is not a data frame, cannot update incrementally")
          return(FALSE)
        }
        
        # Check if key_cols are present in existing_data
        if (!all(key_cols %in% names(existing_data))) {
          self$log_message(paste("Error: key columns", paste(key_cols, collapse=", "), 
                                "must exist in existing data"))
          return(FALSE)
        }
        
        # Identify new records
        existing_keys <- existing_data %>%
          select(all_of(key_cols)) %>%
          mutate(key = do.call(paste, c(select(., all_of(key_cols)), sep = "___")))
        
        new_keys <- new_data %>%
          select(all_of(key_cols)) %>%
          mutate(key = do.call(paste, c(select(., all_of(key_cols)), sep = "___")))
        
        # Find records that are new vs. existing
        truly_new_records <- new_data[!new_keys$key %in% existing_keys$key, ]
        updating_records <- new_data[new_keys$key %in% existing_keys$key, ]
        
        # Create updated dataset
        if (nrow(truly_new_records) > 0) {
          # Add new records
          combined_data <- rbind(existing_data, truly_new_records)
        } else {
          combined_data <- existing_data
        }
        
        # Update existing records if requested
        if (update_existing && nrow(updating_records) > 0) {
          # Remove existing records that will be updated
          updating_keys <- updating_records %>%
            select(all_of(key_cols)) %>%
            mutate(key = do.call(paste, c(select(., all_of(key_cols)), sep = "___")))
          
          combined_data <- combined_data %>%
            mutate(temp_key = do.call(paste, c(select(., all_of(key_cols)), sep = "___"))) %>%
            filter(!temp_key %in% updating_keys$key) %>%
            select(-temp_key)
          
          # Add updated records
          combined_data <- rbind(combined_data, updating_records)
        }
        
        # Log update stats
        self$log_message(paste("Incremental update:", nrow(truly_new_records), "new records,", 
                              nrow(updating_records), "updated records"))
        
        # Save updated data
        return(self$save_to_cache(combined_data, file_path, compress, type))
      }
    },
    
    #' @description Generate cache metrics and diagnostics
    #' @return A list containing cache usage metrics
    get_cache_metrics = function() {
      # Refresh cache scan first
      self$scan_cache_files()
      
      # Calculate basic metrics
      metrics <- list(
        total_files = nrow(self$metadata$files),
        total_size_mb = sum(self$metadata$files$size_bytes, na.rm = TRUE) / (1024 * 1024),
        files_by_type = as.list(table(self$metadata$files$type)),
        size_by_type = tapply(self$metadata$files$size_bytes, 
                             self$metadata$files$type, 
                             sum, na.rm = TRUE) / (1024 * 1024),
        oldest_file = if(nrow(self$metadata$files) > 0) {
          min(as.POSIXct(self$metadata$files$created), na.rm = TRUE)
        } else {
          NA
        },
        newest_file = if(nrow(self$metadata$files) > 0) {
          max(as.POSIXct(self$metadata$files$created), na.rm = TRUE)
        } else {
          NA
        },
        last_pruned = self$metadata$last_pruned,
        cache_limit_mb = self$config$max_cache_size_mb,
        usage_percent = sum(self$metadata$files$size_bytes, na.rm = TRUE) / 
          (self$config$max_cache_size_mb * 1024 * 1024) * 100
      )
      
      # Add access patterns - days since last access
      if (nrow(self$metadata$files) > 0) {
        last_access_dates <- as.POSIXct(self$metadata$files$last_accessed)
        days_since_access <- as.numeric(difftime(Sys.time(), last_access_dates, units = "days"))
        
        metrics$access_patterns <- list(
          not_accessed_30days = sum(days_since_access > 30, na.rm = TRUE),
          not_accessed_90days = sum(days_since_access > 90, na.rm = TRUE),
          median_days_since_access = median(days_since_access, na.rm = TRUE),
          least_recently_accessed = self$metadata$files$path[which.max(days_since_access)]
        )
      }
      
      # Identify expired files
      expired_files <- lapply(split(self$metadata$files, self$metadata$files$type), function(type_files) {
        # Get max age for this type
        file_type <- type_files$type[1]
        if (file_type %in% names(self$config$max_age)) {
          max_age_days <- self$config$max_age[[file_type]]
        } else {
          max_age_days <- 30  # Default
        }
        
        # Check which files are expired
        created_dates <- as.POSIXct(type_files$created)
        days_since_created <- as.numeric(difftime(Sys.time(), created_dates, units = "days"))
        return(sum(days_since_created > max_age_days, na.rm = TRUE))
      })
      
      metrics$expired_files_by_type <- expired_files
      metrics$total_expired_files <- sum(unlist(expired_files))
      
      return(metrics)
    },
    
    #' @description Generate a formatted cache diagnostic report
    #' @param format Output format ("text", "html", "json")
    #' @param file Output file path (optional)
    #' @return Formatted report
    generate_cache_report = function(format = "text", file = NULL) {
      # Get cache metrics
      metrics <- self$get_cache_metrics()
      
      # Generate report in requested format
      if (format == "text") {
        report <- self$generate_text_report(metrics)
      } else if (format == "html") {
        report <- self$generate_html_report(metrics)
      } else if (format == "json") {
        report <- jsonlite::toJSON(metrics, auto_unbox = TRUE, pretty = TRUE)
      } else {
        stop("Unsupported format. Choose from: text, html, json")
      }
      
      # Write to file if requested
      if (!is.null(file)) {
        writeLines(report, file)
      }
      
      return(report)
    },
    
    #' @description Generate a text format cache report
    #' @param metrics Cache metrics from get_cache_metrics()
    #' @return Text report
    generate_text_report = function(metrics) {
      lines <- c(
        "TRAFFIC SAFETY CACHE DIAGNOSTIC REPORT",
        paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        paste0("Cache directory: ", self$base_dir),
        "",
        "SUMMARY:",
        paste0("  Total files: ", metrics$total_files),
        paste0("  Total size: ", round(metrics$total_size_mb, 2), " MB"),
        paste0("  Cache usage: ", round(metrics$usage_percent, 1), "% of ", 
               metrics$cache_limit_mb, " MB limit"),
        paste0("  Total expired files: ", metrics$total_expired_files),
        ""
      )
      
      # Add breakdown by file type
      lines <- c(lines, "FILES BY TYPE:")
      for (type in names(metrics$files_by_type)) {
        count <- metrics$files_by_type[[type]]
        size <- metrics$size_by_type[[type]]
        expired <- metrics$expired_files_by_type[[type]]
        
        if (!is.null(count) && !is.null(size)) {
          lines <- c(lines, paste0("  ", type, ": ", count, " files, ", 
                                  round(size, 2), " MB, ", 
                                  expired, " expired"))
        }
      }
      
      # Add time information
      lines <- c(lines, "",
                "TIME INFORMATION:")
      
      if (!is.null(metrics$oldest_file) && !is.na(metrics$oldest_file)) {
        lines <- c(lines, paste0("  Oldest file: ", format(metrics$oldest_file, "%Y-%m-%d %H:%M:%S")))
      }
      
      if (!is.null(metrics$newest_file) && !is.na(metrics$newest_file)) {
        lines <- c(lines, paste0("  Newest file: ", format(metrics$newest_file, "%Y-%m-%d %H:%M:%S")))
      }
      
      if (!is.null(metrics$last_pruned)) {
        lines <- c(lines, paste0("  Last pruned: ", format(metrics$last_pruned, "%Y-%m-%d %H:%M:%S")))
      } else {
        lines <- c(lines, "  Last pruned: Never")
      }
      
      # Add access patterns
      if (!is.null(metrics$access_patterns)) {
        lines <- c(lines, "",
                  "ACCESS PATTERNS:")
        lines <- c(lines, paste0("  Files not accessed in 30 days: ", 
                                metrics$access_patterns$not_accessed_30days))
        lines <- c(lines, paste0("  Files not accessed in 90 days: ", 
                                metrics$access_patterns$not_accessed_90days))
        lines <- c(lines, paste0("  Median days since last access: ", 
                                round(metrics$access_patterns$median_days_since_access, 1)))
      }
      
      # Combine and return
      return(paste(lines, collapse = "\n"))
    },
    
    #' @description Generate an HTML format cache report
    #' @param metrics Cache metrics from get_cache_metrics()
    #' @return HTML report
    generate_html_report = function(metrics) {
      html_lines <- c(
        "<!DOCTYPE html>",
        "<html>",
        "<head>",
        "  <title>Traffic Safety Cache Diagnostic Report</title>",
        "  <style>",
        "    body { font-family: Arial, sans-serif; margin: 40px; }",
        "    h1 { color: #336699; }",
        "    h2 { color: #336699; margin-top: 30px; }",
        "    table { border-collapse: collapse; width: 100%; margin-top: 20px; }",
        "    th, td { padding: 8px; text-align: left; border-bottom: 1px solid #ddd; }",
        "    tr:hover { background-color: #f5f5f5; }",
        "    th { background-color: #336699; color: white; }",
        "    .warning { color: orange; }",
        "    .danger { color: red; }",
        "    .good { color: green; }",
        "    .usage-bar { width: 100%; background-color: #f0f0f0; border-radius: 4px; height: 20px; }",
        "    .usage-fill { height: 100%; background-color: #336699; border-radius: 4px; }",
        "    .danger-fill { background-color: #d9534f; }",
        "    .warning-fill { background-color: #f0ad4e; }",
        "  </style>",
        "</head>",
        "<body>",
        "  <h1>Traffic Safety Cache Diagnostic Report</h1>",
        paste0("  <p>Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "</p>"),
        paste0("  <p>Cache directory: ", self$base_dir, "</p>"),
        "",
        "  <h2>Summary</h2>",
        "  <table>",
        "    <tr><th>Metric</th><th>Value</th></tr>",
        paste0("    <tr><td>Total files</td><td>", metrics$total_files, "</td></tr>"),
        paste0("    <tr><td>Total size</td><td>", round(metrics$total_size_mb, 2), " MB</td></tr>"),
        paste0("    <tr><td>Cache limit</td><td>", metrics$cache_limit_mb, " MB</td></tr>"),
        "    <tr><td>Cache usage</td><td>",
        paste0("<div class='usage-bar'><div class='usage-fill", 
              ifelse(metrics$usage_percent > 90, " danger-fill", 
                    ifelse(metrics$usage_percent > 70, " warning-fill", "")), 
              "' style='width:", min(100, metrics$usage_percent), "%;'></div></div>",
              round(metrics$usage_percent, 1), "% of limit"),
        "</td></tr>",
        paste0("    <tr><td>Total expired files</td><td>", metrics$total_expired_files, "</td></tr>"),
        "  </table>",
        "",
        "  <h2>Files by Type</h2>",
        "  <table>",
        "    <tr><th>Type</th><th>Count</th><th>Size (MB)</th><th>Expired</th></tr>"
      )
      
      # Add rows for each file type
      for (type in names(metrics$files_by_type)) {
        count <- metrics$files_by_type[[type]]
        size <- metrics$size_by_type[[type]]
        expired <- metrics$expired_files_by_type[[type]]
        
        if (!is.null(count) && !is.null(size)) {
          html_lines <- c(html_lines, paste0(
            "    <tr><td>", type, "</td><td>", count, "</td><td>", 
            round(size, 2), "</td><td>", expired, "</td></tr>"
          ))
        }
      }
      
      html_lines <- c(html_lines, 
                     "  </table>",
                     "",
                     "  <h2>Time Information</h2>",
                     "  <table>",
                     "    <tr><th>Metric</th><th>Value</th></tr>")
      
      # Add time information
      if (!is.null(metrics$oldest_file) && !is.na(metrics$oldest_file)) {
        html_lines <- c(html_lines, paste0(
          "    <tr><td>Oldest file</td><td>", 
          format(metrics$oldest_file, "%Y-%m-%d %H:%M:%S"), "</td></tr>"
        ))
      }
      
      if (!is.null(metrics$newest_file) && !is.na(metrics$newest_file)) {
        html_lines <- c(html_lines, paste0(
          "    <tr><td>Newest file</td><td>", 
          format(metrics$newest_file, "%Y-%m-%d %H:%M:%S"), "</td></tr>"
        ))
      }
      
      if (!is.null(metrics$last_pruned)) {
        html_lines <- c(html_lines, paste0(
          "    <tr><td>Last pruned</td><td>", 
          format(metrics$last_pruned, "%Y-%m-%d %H:%M:%S"), "</td></tr>"
        ))
      } else {
        html_lines <- c(html_lines, 
                       "    <tr><td>Last pruned</td><td>Never</td></tr>")
      }
      
      html_lines <- c(html_lines, 
                     "  </table>")
      
      # Add access patterns
      if (!is.null(metrics$access_patterns)) {
        html_lines <- c(html_lines,
                       "",
                       "  <h2>Access Patterns</h2>",
                       "  <table>",
                       "    <tr><th>Metric</th><th>Value</th></tr>",
                       paste0("    <tr><td>Files not accessed in 30 days</td><td>", 
                             metrics$access_patterns$not_accessed_30days, "</td></tr>"),
                       paste0("    <tr><td>Files not accessed in 90 days</td><td>", 
                             metrics$access_patterns$not_accessed_90days, "</td></tr>"),
                       paste0("    <tr><td>Median days since last access</td><td>", 
                             round(metrics$access_patterns$median_days_since_access, 1), "</td></tr>"),
                       "  </table>")
      }
      
      # Close HTML
      html_lines <- c(html_lines,
                     "</body>",
                     "</html>")
      
      return(paste(html_lines, collapse = "\n"))
    },
    
    #' @description Log a message to the cache log file
    #' @param message Message to log
    log_message = function(message) {
      # Create log directory if it doesn't exist
      log_dir <- dirname(self$config$log_file)
      if (!dir.exists(log_dir)) {
        dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
      }
      
      # Format log entry
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      log_entry <- paste0(timestamp, " [TrafficSafetyCache] ", message)
      
      # Append to log file
      cat(log_entry, "\n", file = self$config$log_file, append = TRUE)
      
      return(invisible(self))
    }
  )
)

#' Get a smart cached file handling automatic expiry and incremental updates
#'
#' @param file_path Full path to the cache file
#' @param generator Function to generate data if cache is missing or expired
#' @param key_cols Key columns for incremental updates (if applicable)
#' @param incremental Whether to support incremental updates
#' @param type Cache data type for expiry calculation
#' @param max_age_days Maximum age in days before expiration
#' @param compress Whether to compress the cached data
#' @param cache_manager Optional custom cache manager instance
#'
#' @return The cached or freshly generated data
#' @export
get_smart_cached_data <- function(file_path, 
                               generator, 
                               key_cols = NULL,
                               incremental = FALSE,
                               type = NULL, 
                               max_age_days = NULL,
                               compress = NULL,
                               cache_manager = NULL) {
  
  # Create or get cache manager
  if (is.null(cache_manager)) {
    cache_dir <- dirname(dirname(file_path))  # Go up one level from the file path
    cache_manager <- TrafficSafetyCache$new(cache_dir)
  }
  
  # Try to get cached data
  cached_data <- cache_manager$get_cached_data(file_path, max_age_days)
  
  # If cache is valid, return it
  if (!is.null(cached_data)) {
    return(cached_data)
  }
  
  # Cache is missing or expired, generate new data
  cache_manager$log_message(paste("Generating fresh data for", basename(file_path)))
  new_data <- generator()
  
  # If incremental update is supported and we had cached data
  if (incremental && !is.null(cached_data) && !is.null(key_cols)) {
    cache_manager$save_incremental_update(
      new_data = new_data,
      file_path = file_path,
      key_cols = key_cols,
      compress = compress,
      type = type
    )
  } else {
    # Regular save
    cache_manager$save_to_cache(
      data = new_data,
      file_path = file_path,
      compress = compress,
      type = type
    )
  }
  
  return(new_data)
}

#' Apply the optimized cache strategy to traffic safety data
#'
#' @param func Function to wrap with optimized caching
#' @param cache_dir Directory to store cached data
#' @param ... Additional arguments to pass to func
#'
#' @return The cached or freshly generated data
#' @export
with_optimized_cache <- function(func, cache_dir = "data/cache", ...) {
  # Create a cache manager
  cache_manager <- TrafficSafetyCache$new(cache_dir)
  
  # Define a function to process arguments and determine cache path
  generate_cache_path <- function(args) {
    # Extract years from arguments
    years <- args$years
    if (is.null(years)) {
      # Default to current year if no years specified
      years <- as.numeric(format(Sys.Date(), "%Y"))
    }
    
    # Sort years for consistent cache path
    years <- sort(years)
    
    # Create cache key based on key parameters
    refresh_cache <- args$refresh_cache %||% FALSE
    allow_interpolation <- args$allow_interpolation %||% TRUE
    allow_simulation <- args$allow_simulation %||% FALSE
    
    # Create cache path based on function name and parameters
    func_name <- deparse(substitute(func))
    if (grepl("fetch_traffic_safety_data", func_name)) {
      cache_file <- file.path(
        cache_dir, 
        "traffic_safety",
        paste0("traffic_safety_data_", min(years), "_", max(years), ".rds")
      )
    } else if (grepl("get_fars_data", func_name)) {
      cache_file <- file.path(
        cache_dir, 
        "fars",
        paste0("fars_data_", min(years), "_", max(years), ".rds")
      )
    } else if (grepl("get_cdc_wonder_data", func_name)) {
      cache_file <- file.path(
        cache_dir, 
        "cdc",
        paste0("cdc_wonder_data_", min(years), "_", max(years), ".rds")
      )
    } else {
      # Generic cache file for other functions
      param_hash <- digest::digest(list(years, refresh_cache, allow_interpolation, allow_simulation))
      cache_file <- file.path(
        cache_dir, 
        "processed",
        paste0(func_name, "_", param_hash, ".rds")
      )
    }
    
    return(cache_file)
  }
  
  # Process the supplied arguments
  args <- list(...)
  cache_file <- generate_cache_path(args)
  
  # Determine if we should do incremental updates based on the function
  func_name <- deparse(substitute(func))
  incremental <- FALSE
  key_cols <- NULL
  data_type <- NULL
  
  if (grepl("fetch_traffic_safety_data", func_name)) {
    incremental <- FALSE  # Final data is not incremental
    data_type <- "processed"
  } else if (grepl("get_fars_data|get_cdc_wonder_data", func_name)) {
    incremental <- TRUE  # Source data can be updated incrementally
    key_cols <- c("fips", "year")
    data_type <- ifelse(grepl("get_fars_data", func_name), "fars", "cdc")
  }
  
  # Allow bypassing cache if refresh_cache is TRUE
  if (!is.null(args$refresh_cache) && args$refresh_cache) {
    # Force regenerate data
    cache_manager$log_message(paste("Cache refresh requested for", basename(cache_file)))
    
    # Generate new data
    new_data <- do.call(func, args)
    
    # Save to cache
    if (incremental) {
      cache_manager$save_incremental_update(
        new_data = new_data,
        file_path = cache_file,
        key_cols = key_cols,
        type = data_type
      )
    } else {
      cache_manager$save_to_cache(
        data = new_data,
        file_path = cache_file,
        type = data_type
      )
    }
    
    return(new_data)
  } else {
    # Use smart caching
    return(get_smart_cached_data(
      file_path = cache_file,
      generator = function() do.call(func, args),
      key_cols = key_cols,
      incremental = incremental,
      type = data_type
    ))
  }
}

# Example usage when run directly
if (!interactive()) {
  # Parse command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) > 0 && args[1] == "--clean") {
    # Clean cache based on expiry rules
    cache <- TrafficSafetyCache$new("data/cache")
    cache$scan_cache_files()
    cache$prune_cache()
    
    # Generate and print report
    report <- cache$generate_cache_report("text")
    cat(report, "\n")
    
  } else if (length(args) > 0 && args[1] == "--test") {
    # Create a test function
    test_function <- function(years = 2020:2021, test_param = "default") {
      # Generate some test data
      result <- data.frame(
        fips = rep(c("01001", "01003", "01005"), each = length(years)),
        year = rep(years, 3),
        test_value = runif(3 * length(years)),
        param = test_param,
        stringsAsFactors = FALSE
      )
      
      # Simulate work by sleeping
      Sys.sleep(1)
      
      return(result)
    }
    
    # Create temporary cache directory for testing
    test_cache_dir <- "data/cache/test"
    
    # Test the cache optimization
    start_time <- Sys.time()
    result1 <- with_optimized_cache(test_function, cache_dir = test_cache_dir)
    time1 <- difftime(Sys.time(), start_time, units = "secs")
    
    # Test accessing cached version
    start_time <- Sys.time()
    result2 <- with_optimized_cache(test_function, cache_dir = test_cache_dir)
    time2 <- difftime(Sys.time(), start_time, units = "secs")
    
    # Print results
    cat("First call (generate): ", time1, " seconds\n")
    cat("Second call (cached): ", time2, " seconds\n")
    cat("Cache speedup factor: ", time1 / time2, "\n")
    
    # Print cache report
    cache <- TrafficSafetyCache$new(test_cache_dir)
    report <- cache$generate_cache_report("text")
    cat("\nCache Report:\n")
    cat(report, "\n")
  }
}