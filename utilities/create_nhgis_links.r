#\!/usr/bin/env Rscript

# NHGIS Symbolic Links Creation Utility
# This script automatically creates symbolic links between IHME and NHGIS directories
# to ensure the pipeline can find NHGIS data files even when stored in the IHME directory

# Load required packages
suppressPackageStartupMessages({
  library(tidyverse)
})

#' Create symbolic links for NHGIS data files
#'
#' This function creates symbolic links from the IHME/CSV directory to the NHGIS directory
#' to ensure NHGIS data files are available to the pipeline without duplication of storage.
#'
#' @param ihme_dir Path to the IHME/CSV directory containing the data files
#' @param nhgis_dir Path to the NHGIS directory where the symbolic links should be created
#' @param force Whether to force recreation of symbolic links even if they already exist
#' @param verbose Whether to print detailed information during execution
#'
#' @return A data frame with information about the created symbolic links
#'
create_nhgis_links <- function(ihme_dir = "data/ihme/CSV", 
                              nhgis_dir = "data/nhgis",
                              force = FALSE,
                              verbose = TRUE) {
  # Check if directories exist
  if (\!dir.exists(ihme_dir)) {
    message("IHME directory does not exist: ", ihme_dir)
    # Create the directory if it doesn't exist
    dir.create(ihme_dir, recursive = TRUE, showWarnings = FALSE)
    message("Created IHME directory: ", ihme_dir)
  }
  
  if (\!dir.exists(nhgis_dir)) {
    message("NHGIS directory does not exist: ", nhgis_dir)
    # Create the directory if it doesn't exist
    dir.create(nhgis_dir, recursive = TRUE, showWarnings = FALSE)
    message("Created NHGIS directory: ", nhgis_dir)
  }
  
  # Get absolute paths
  ihme_dir_abs <- normalizePath(ihme_dir, mustWork = TRUE)
  nhgis_dir_abs <- normalizePath(nhgis_dir, mustWork = TRUE)
  
  if (verbose) {
    message("IHME directory: ", ihme_dir_abs)
    message("NHGIS directory: ", nhgis_dir_abs)
  }
  
  # Find all CSV files in the IHME directory
  ihme_files <- list.files(ihme_dir_abs, pattern = "\\.csv$|\\.CSV$|\\.zip$|\\.ZIP$", 
                           full.names = TRUE, recursive = FALSE)
  
  if (length(ihme_files) == 0) {
    message("No CSV or ZIP files found in IHME directory: ", ihme_dir_abs)
    return(tibble(source_file = character(), link_file = character(), status = character()))
  }
  
  if (verbose) {
    message("Found ", length(ihme_files), " files in IHME directory")
  }
  
  # Create a data frame to track links
  link_info <- tibble(
    source_file = ihme_files,
    link_file = file.path(nhgis_dir_abs, basename(ihme_files)),
    status = NA_character_
  )
  
  # Create symbolic links based on the operating system
  if (.Platform$OS.type == "unix") {
    # Unix/Linux/MacOS approach using system commands
    for (i in 1:nrow(link_info)) {
      source_file <- link_info$source_file[i]
      link_file <- link_info$link_file[i]
      
      # Check if link already exists
      if (file.exists(link_file) && \!force) {
        if (verbose) {
          message("Link already exists: ", link_file)
        }
        link_info$status[i] <- "already exists"
        next
      }
      
      # Remove existing link if force=TRUE
      if (file.exists(link_file) && force) {
        unlink(link_file)
        if (verbose) {
          message("Removed existing link: ", link_file)
        }
      }
      
      # Create symbolic link
      result <- system2("ln", args = c("-sf", source_file, link_file))
      
      if (result == 0) {
        link_info$status[i] <- "created"
        if (verbose) {
          message("Created symbolic link: ", link_file)
        }
      } else {
        link_info$status[i] <- "failed"
        warning("Failed to create symbolic link: ", link_file)
      }
    }
  } else {
    # Windows approach using file.copy
    warning("Windows does not easily support symbolic links. Copying files instead.")
    
    for (i in 1:nrow(link_info)) {
      source_file <- link_info$source_file[i]
      link_file <- link_info$link_file[i]
      
      # Check if file already exists
      if (file.exists(link_file) && \!force) {
        if (verbose) {
          message("File already exists: ", link_file)
        }
        link_info$status[i] <- "already exists"
        next
      }
      
      # Remove existing file if force=TRUE
      if (file.exists(link_file) && force) {
        unlink(link_file)
        if (verbose) {
          message("Removed existing file: ", link_file)
        }
      }
      
      # Copy file
      result <- file.copy(source_file, link_file)
      
      if (result) {
        link_info$status[i] <- "copied"
        if (verbose) {
          message("Copied file: ", link_file)
        }
      } else {
        link_info$status[i] <- "failed"
        warning("Failed to copy file: ", link_file)
      }
    }
  }
  
  # Create README file in NHGIS directory explaining symbolic links
  readme_path <- file.path(nhgis_dir_abs, "README_SYMBOLIC_LINKS.md")
  
  readme_content <- c(
    "# NHGIS Directory - Symbolic Links",
    "",
    "This directory contains symbolic links to NHGIS data files stored in the IHME/CSV directory.",
    "The links are created to ensure the pipeline can find NHGIS data files without duplicating storage.",
    "",
    "## Link Information",
    "",
    paste("- Total Links:", nrow(link_info)),
    paste("- Created:", sum(link_info$status == "created")),
    paste("- Copied:", sum(link_info$status == "copied")),
    paste("- Already Existing:", sum(link_info$status == "already exists")),
    paste("- Failed:", sum(link_info$status == "failed")),
    "",
    "## Source Location",
    "",
    paste("Files are linked from:", ihme_dir_abs),
    "",
    "## Windows Users",
    "",
    "Windows does not easily support symbolic links by default. If you are using Windows:",
    "",
    "1. You may see copies of files instead of symbolic links",
    "2. To enable symbolic links on Windows, you need to:",
    "   - Run as Administrator, or",
    "   - Enable Developer Mode (Windows 10+), or",
    "   - Use the `mklink` command in a Command Prompt run as Administrator",
    "",
    "## Link List",
    "",
    "The following links were created:"
  )
  
  # Add link details to README
  for (i in 1:nrow(link_info)) {
    readme_content <- c(
      readme_content,
      paste0("- ", basename(link_info$source_file[i]), " → ", link_info$status[i])
    )
  }
  
  # Write README file
  writeLines(readme_content, readme_path)
  
  if (verbose) {
    message("Created README file: ", readme_path)
    message("Summary: ")
    message("  Total Links: ", nrow(link_info))
    message("  Created: ", sum(link_info$status == "created"))
    message("  Copied: ", sum(link_info$status == "copied"))
    message("  Already Existing: ", sum(link_info$status == "already exists"))
    message("  Failed: ", sum(link_info$status == "failed"))
  }
  
  return(link_info)
}

# Function to find data files in the repository
find_data_files <- function(base_dir = ".", 
                           file_patterns = c("\\.csv$", "\\.CSV$", "\\.zip$", "\\.ZIP$"),
                           verbose = TRUE) {
  # Get a list of all subdirectories
  all_dirs <- list.dirs(base_dir, recursive = TRUE)
  
  # Initialize results list
  results <- list()
  
  # Create regex pattern for matching
  pattern <- paste(file_patterns, collapse = "|")
  
  # Search each directory for matching files
  for (dir in all_dirs) {
    files <- list.files(dir, pattern = pattern, full.names = TRUE)
    
    if (length(files) > 0) {
      dir_name <- basename(dir)
      parent_dir <- basename(dirname(dir))
      path_key <- paste0(parent_dir, "/", dir_name)
      
      results[[path_key]] <- files
      
      if (verbose) {
        message("Found ", length(files), " files in ", dir)
      }
    }
  }
  
  # Create a data frame with the results
  result_df <- tibble(
    directory = character(),
    file_path = character(),
    file_name = character(),
    file_ext = character(),
    file_size_mb = numeric()
  )
  
  for (dir_name in names(results)) {
    files <- results[[dir_name]]
    
    for (file in files) {
      file_info <- file.info(file)
      
      result_df <- result_df %>%
        add_row(
          directory = dir_name,
          file_path = file,
          file_name = basename(file),
          file_ext = tools::file_ext(file),
          file_size_mb = file_info$size / (1024 * 1024)
        )
    }
  }
  
  return(result_df)
}

# Execute as script if run directly
if (\!interactive()) {
  # Process command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  
  # Default values
  ihme_dir <- "data/ihme/CSV"
  nhgis_dir <- "data/nhgis"
  force <- FALSE
  verbose <- TRUE
  
  # Parse arguments
  i <- 1
  while (i <= length(args)) {
    if (args[i] == "--ihme-dir" && i < length(args)) {
      ihme_dir <- args[i + 1]
      i <- i + 2
    } else if (args[i] == "--nhgis-dir" && i < length(args)) {
      nhgis_dir <- args[i + 1]
      i <- i + 2
    } else if (args[i] == "--force") {
      force <- TRUE
      i <- i + 1
    } else if (args[i] == "--quiet") {
      verbose <- FALSE
      i <- i + 1
    } else if (args[i] == "--find-files") {
      # Find all data files in the repository
      message("Finding all data files in the repository...")
      data_files <- find_data_files(base_dir = ".", verbose = verbose)
      
      # Print summary of found files
      message("\nFound ", nrow(data_files), " data files in ", length(unique(data_files$directory)), " directories")
      
      # Group by directory and count
      dir_summary <- data_files %>%
        group_by(directory) %>%
        summarize(
          count = n(),
          total_size_mb = sum(file_size_mb),
          extensions = paste(unique(file_ext), collapse = ", ")
        ) %>%
        arrange(desc(count))
      
      # Print directory summary
      print(dir_summary)
      
      # Look for potential NHGIS files
      nhgis_candidates <- data_files %>%
        filter(grepl("nhgis|NHGIS|ipums|IPUMS|census|CENSUS", file_name))
      
      if (nrow(nhgis_candidates) > 0) {
        message("\nFound ", nrow(nhgis_candidates), " potential NHGIS files:")
        print(
          nhgis_candidates %>%
            select(directory, file_name, file_size_mb) %>%
            arrange(directory, file_name)
        )
      }
      
      # Exit after finding files
      quit(save = "no", status = 0)
    } else if (args[i] == "--help" || args[i] == "-h") {
      cat("Usage: Rscript create_nhgis_links.r [options]\n")
      cat("\nOptions:\n")
      cat("  --ihme-dir DIR      Set IHME directory (default: data/ihme/CSV)\n")
      cat("  --nhgis-dir DIR     Set NHGIS directory (default: data/nhgis)\n")
      cat("  --force             Force recreation of symbolic links\n")
      cat("  --quiet             Suppress verbose output\n")
      cat("  --find-files        Find all data files in the repository\n")
      cat("  --help, -h          Show this help message\n")
      quit(save = "no", status = 0)
    } else {
      i <- i + 1
    }
  }
  
  # Create the symbolic links
  create_nhgis_links(ihme_dir, nhgis_dir, force, verbose)
}
EOL < /dev/null