#\!/usr/bin/env Rscript

# Federal Data Caching Utility
# This script systematically downloads and archives federal agency data sources
# to protect against potential data unavailability in the future.

# Load required packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(httr)
  library(jsonlite)
  library(lubridate)
  library(digest)
  library(parallel)
  library(future)
  library(future.apply)
})

#' Cache Federal Agency Data Sources
#'
#' This function systematically downloads and caches data from federal agencies
#' to ensure availability even if the original sources become unavailable.
#'
#' @param agencies Vector of agency names to cache (NULL for all)
#' @param years Vector of years to download data for
#' @param cache_dir Base directory for cached data
#' @param max_threads Maximum number of parallel download threads
#' @param refresh Force refresh of existing cached data
#' @param verbose Print detailed progress information
#'
#' @return A data frame with information about cached data sources
#'
cache_federal_data <- function(agencies = NULL, 
                              years = 1970:format(Sys.Date(), "%Y"),
                              cache_dir = "data/cache/federal",
                              max_threads = 4,
                              refresh = FALSE,
                              verbose = TRUE) {
  # Ensure cache directory exists
  if (\!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    if (verbose) {
      message("Created cache directory: ", cache_dir)
    }
  }
  
  # Define agencies and their data sources
  all_agencies <- list(
    "CDC" = list(
      name = "Centers for Disease Control and Prevention",
      base_url = "https://data.cdc.gov/api",
      endpoints = list(
        list(
          name = "PLACES",
          description = "County-level health indicators",
          endpoint = "/views/cwsq-ngmh/rows.csv",
          params = list(accessType = "DOWNLOAD"),
          years = 2019:2022
        ),
        list(
          name = "BRFSS",
          description = "Behavioral Risk Factor Surveillance System",
          endpoint = "/views/dtd7-bn96/rows.csv",
          params = list(accessType = "DOWNLOAD"),
          years = 2011:2022
        ),
        list(
          name = "WONDER",
          description = "Mortality data",
          endpoint = "/views/muzy-jte6/rows.csv",
          params = list(accessType = "DOWNLOAD"),
          years = 1999:2022
        ),
        list(
          name = "NVSS",
          description = "National Vital Statistics System",
          endpoint = "/views/kn79-hsxy/rows.csv",
          params = list(accessType = "DOWNLOAD"),
          years = 1999:2022
        )
      ),
      cache_subdirectory = "cdc"
    ),
    
    "EPA" = list(
      name = "Environmental Protection Agency",
      base_url = "https://aqs.epa.gov/aqsweb/airdata",
      endpoints = list(
        list(
          name = "AQS",
          description = "Air Quality System annual summary data",
          endpoint = function(year) sprintf("/annual_conc_by_county_%d.zip", year),
          years = 1990:format(Sys.Date(), "%Y"),
          custom_filename = function(year) sprintf("annual_aqs_%d.zip", year)
        ),
        list(
          name = "EJSCREEN",
          description = "Environmental Justice Screening",
          endpoint = "/EJSCREEN_2023_StatePctile.csv",
          years = 2022:2023
        ),
        list(
          name = "TRI",
          description = "Toxic Release Inventory",
          endpoint = function(year) sprintf("/tri_facilities_%d.csv", year),
          years = 2010:2022
        )
      ),
      cache_subdirectory = "epa"
    ),
    
    "NOAA" = list(
      name = "National Oceanic and Atmospheric Administration",
      base_url = "https://www.ncei.noaa.gov/pub/data",
      endpoints = list(
        list(
          name = "Climate Normals",
          description = "Climate normal data by county",
          endpoint = "/cag/county/county-climate-normals.csv",
          years = c(NULL)  # Not year-specific
        ),
        list(
          name = "Storm Events",
          description = "Storm events database",
          endpoint = function(year) sprintf("/storm-events/csvfiles/StormEvents_details-%d.csv.gz", year),
          years = 1950:format(Sys.Date(), "%Y")
        ),
        list(
          name = "Drought Index",
          description = "Palmer Drought Severity Index",
          endpoint = "/drought/county-pdsi-values.csv",
          years = c(NULL)  # Not year-specific
        )
      ),
      cache_subdirectory = "noaa"
    ),
    
    "NIH" = list(
      name = "National Institutes of Health",
      base_url = "https://opendata.cancer.gov/api/views",
      endpoints = list(
        list(
          name = "SEER",
          description = "Cancer statistics by county",
          endpoint = "/ej94-qhaw/rows.csv",
          params = list(accessType = "DOWNLOAD"),
          years = 2000:2020
        ),
        list(
          name = "Cancer Atlas",
          description = "State Cancer Profiles",
          endpoint = "/p5vy-54qm/rows.csv",
          params = list(accessType = "DOWNLOAD"),
          years = 2015:2021
        )
      ),
      cache_subdirectory = "nih"
    ),
    
    "HUD" = list(
      name = "Housing and Urban Development",
      base_url = "https://www.huduser.gov/portal/datasets",
      endpoints = list(
        list(
          name = "CHAS",
          description = "Comprehensive Housing Affordability Strategy",
          endpoint = function(year) sprintf("/CHAS/data/CHAS_%d-2020_county.csv", year - 4),
          years = 2013:2021
        ),
        list(
          name = "FMR",
          description = "Fair Market Rents",
          endpoint = function(year) sprintf("/fmr/County_FMR_%d.csv", year),
          years = 2006:format(Sys.Date(), "%Y")
        )
      ),
      cache_subdirectory = "hud"
    ),
    
    "USDA" = list(
      name = "US Department of Agriculture",
      base_url = "https://www.ers.usda.gov/webdocs/DataFiles",
      endpoints = list(
        list(
          name = "Food Environment Atlas",
          description = "Food access and food security indicators",
          endpoint = "/134373/FoodEnvironmentAtlas.xls",
          years = c(NULL)  # Not year-specific
        ),
        list(
          name = "Food Access Research Atlas",
          description = "Food access measures",
          endpoint = "/134467/FoodAccessResearchAtlasData.xlsx",
          years = c(NULL)  # Not year-specific
        ),
        list(
          name = "Rural Atlas",
          description = "County-level measures of rural prosperity",
          endpoint = "/134423/RuralAtlasData.xlsx",
          years = c(NULL)  # Not year-specific
        )
      ),
      cache_subdirectory = "usda"
    ),
    
    "HRSA" = list(
      name = "Health Resources and Services Administration",
      base_url = "https://data.hrsa.gov/data/download",
      endpoints = list(
        list(
          name = "AHRF",
          description = "Area Health Resources Files",
          endpoint = "/AHRF/AHRF_SAS.zip",
          years = c(NULL)  # Updated annually but covers multiple years
        ),
        list(
          name = "Health Centers",
          description = "Health center service locations",
          endpoint = "/BPHCDA/BCD_HCSCMS_CLINICAL_QUALITY.csv",
          years = c(NULL)  # Not year-specific
        )
      ),
      cache_subdirectory = "hrsa"
    ),
    
    "Census" = list(
      name = "US Census Bureau",
      base_url = "https://www2.census.gov/programs-surveys",
      endpoints = list(
        list(
          name = "PEP",
          description = "Population Estimates Program",
          endpoint = function(year) sprintf("/popest/datasets/%d/counties/totals/co-est%d-alldata.csv", year, year),
          years = 2000:format(Sys.Date(), "%Y")
        ),
        list(
          name = "ACS",
          description = "American Community Survey 5-year estimates",
          endpoint = function(year) sprintf("/acs%d/data/5_year/profile/data/dp02-dp05/county.csv", year),
          years = 2009:2022
        ),
        list(
          name = "CBP",
          description = "County Business Patterns",
          endpoint = function(year) sprintf("/cbp/%d/cbp%d.txt", year, substr(as.character(year), 3, 4)),
          years = 1990:2022
        )
      ),
      cache_subdirectory = "census"
    ),
    
    "BEA" = list(
      name = "Bureau of Economic Analysis",
      base_url = "https://apps.bea.gov/regional/zip",
      endpoints = list(
        list(
          name = "CAINC1",
          description = "Personal Income by County",
          endpoint = "/CAINC1.zip",
          years = c(NULL)  # Comprehensive time series
        ),
        list(
          name = "CAINC4",
          description = "Personal Income and Employment by Major Component",
          endpoint = "/CAINC4.zip",
          years = c(NULL)  # Comprehensive time series
        )
      ),
      cache_subdirectory = "bea"
    ),
    
    "BLS" = list(
      name = "Bureau of Labor Statistics",
      base_url = "https://download.bls.gov/pub/time.series/la",
      endpoints = list(
        list(
          name = "LAUS",
          description = "Local Area Unemployment Statistics",
          endpoint = "/la.data.64.County",
          years = c(NULL)  # Comprehensive time series
        ),
        list(
          name = "QCEW",
          description = "Quarterly Census of Employment and Wages",
          endpoint = function(year) sprintf("/qcew/pub/data/county_high_level_%d.csv", year),
          base_url_override = "https://www.bls.gov/cew/data/files",
          years = 1990:2023
        )
      ),
      cache_subdirectory = "bls"
    ),
    
    "FCC" = list(
      name = "Federal Communications Commission",
      base_url = "https://opendata.fcc.gov/api/views",
      endpoints = list(
        list(
          name = "477",
          description = "Form 477 Broadband Deployment Data",
          endpoint = "/epm5-gmk8/rows.csv",
          params = list(accessType = "DOWNLOAD"),
          years = c(NULL)  # Comprehensive data
        ),
        list(
          name = "ACS Connectivity",
          description = "ACS County Connectivity Metrics",
          endpoint = "/ktfm-qs4f/rows.csv", 
          params = list(accessType = "DOWNLOAD"),
          years = 2015:2022
        )
      ),
      cache_subdirectory = "fcc"
    )
  )
  
  # Filter to requested agencies
  if (\!is.null(agencies)) {
    all_agencies <- all_agencies[names(all_agencies) %in% agencies]
    if (length(all_agencies) == 0) {
      stop("No matching agencies found. Available agencies: ", 
           paste(names(all_agencies), collapse = ", "))
    }
  }
  
  # Initialize results tracking
  results <- tibble(
    agency = character(),
    dataset = character(),
    year = numeric(),
    url = character(),
    local_path = character(),
    status = character(),
    timestamp = as.POSIXct(character()),
    file_size_mb = numeric(),
    error_message = character()
  )
  
  # Set up parallel processing
  cores_to_use <- min(max_threads, parallel::detectCores() - 1)
  if (cores_to_use < 1) cores_to_use <- 1
  
  if (verbose) {
    message("Using ", cores_to_use, " cores for parallel downloads")
  }
  
  # Use future for parallel processing
  future::plan(future::multiprocess, workers = cores_to_use)
  
  # Function to download a single dataset
  download_dataset <- function(agency_name, agency_info, endpoint_info, year = NULL) {
    tryCatch({
      # Determine the endpoint URL
      base_url <- if (\!is.null(endpoint_info$base_url_override)) {
        endpoint_info$base_url_override
      } else {
        agency_info$base_url
      }
      
      endpoint <- if (is.function(endpoint_info$endpoint)) {
        endpoint_info$endpoint(year)
      } else {
        endpoint_info$endpoint
      }
      
      # Build the full URL
      url <- paste0(base_url, endpoint)
      
      # Determine the local filename
      if (\!is.null(endpoint_info$custom_filename) && is.function(endpoint_info$custom_filename)) {
        filename <- endpoint_info$custom_filename(year)
      } else {
        # Extract filename from URL, fallback to endpoint name and year
        filename_from_url <- basename(endpoint)
        if (filename_from_url == "") {
          # Use endpoint name and year if no filename in URL
          year_suffix <- if (\!is.null(year)) paste0("_", year) else ""
          filename <- paste0(endpoint_info$name, year_suffix, ".csv")
        } else {
          filename <- filename_from_url
        }
      }
      
      # Create cache path
      cache_subdir <- file.path(cache_dir, agency_info$cache_subdirectory)
      if (\!dir.exists(cache_subdir)) {
        dir.create(cache_subdir, recursive = TRUE, showWarnings = FALSE)
      }
      
      # If year-specific path is needed
      if (\!is.null(year)) {
        year_subdir <- file.path(cache_subdir, paste0("year_", year))
        if (\!dir.exists(year_subdir) && \!grepl("\\d{4}", filename)) {
          dir.create(year_subdir, recursive = TRUE, showWarnings = FALSE)
          cache_path <- file.path(year_subdir, filename)
        } else {
          cache_path <- file.path(cache_subdir, filename)
        }
      } else {
        cache_path <- file.path(cache_subdir, filename)
      }
      
      # Check if file already exists and is not empty
      file_exists <- file.exists(cache_path) && file.info(cache_path)$size > 0
      
      if (file_exists && \!refresh) {
        if (verbose) {
          message("Already cached: ", cache_path)
        }
        
        timestamp <- file.info(cache_path)$mtime
        file_size <- file.info(cache_path)$size / (1024 * 1024) # Convert to MB
        
        return(tibble(
          agency = agency_name,
          dataset = endpoint_info$name,
          year = if (\!is.null(year)) year else NA_integer_,
          url = url,
          local_path = cache_path,
          status = "already_cached",
          timestamp = timestamp,
          file_size_mb = file_size,
          error_message = NA_character_
        ))
      }
      
      # Download the file
      if (verbose) {
        dataset_desc <- paste0(agency_name, "/", endpoint_info$name)
        if (\!is.null(year)) {
          dataset_desc <- paste0(dataset_desc, " (", year, ")")
        }
        message("Downloading ", dataset_desc, " to ", cache_path)
      }
      
      # Prepare query parameters if available
      query_params <- if (\!is.null(endpoint_info$params)) endpoint_info$params else list()
      
      # Make the request with a reasonable timeout
      response <- httr::GET(
        url,
        query = query_params,
        httr::timeout(300), # 5 minute timeout
        httr::write_disk(cache_path, overwrite = TRUE)
      )
      
      # Check the response
      if (httr::status_code(response) >= 400) {
        # HTTP error
        error_message <- paste("HTTP error:", httr::status_code(response), 
                              httr::http_status(response)$message)
        
        # Clean up the failed download
        if (file.exists(cache_path)) {
          unlink(cache_path)
        }
        
        return(tibble(
          agency = agency_name,
          dataset = endpoint_info$name,
          year = if (\!is.null(year)) year else NA_integer_,
          url = url,
          local_path = cache_path,
          status = "failed",
          timestamp = Sys.time(),
          file_size_mb = 0,
          error_message = error_message
        ))
      }
      
      # Get file info
      file_size <- file.info(cache_path)$size / (1024 * 1024) # Convert to MB
      
      # Check if file is empty or too small (likely an error)
      if (file.info(cache_path)$size < 100) { # Less than 100 bytes
        # Read the file to see if it contains an error message
        content <- readLines(cache_path, warn = FALSE)
        error_message <- if (length(content) > 0) paste(content, collapse = " ") else "Empty file"
        
        # Clean up the failed download
        unlink(cache_path)
        
        return(tibble(
          agency = agency_name,
          dataset = endpoint_info$name,
          year = if (\!is.null(year)) year else NA_integer_,
          url = url,
          local_path = cache_path,
          status = "failed",
          timestamp = Sys.time(),
          file_size_mb = 0,
          error_message = error_message
        ))
      }
      
      # Check for zip files that need to be archived as-is
      if (grepl("\\.zip$|\\.zip\\?.*$", tolower(url)) || 
          grepl("\\.zip$", tolower(cache_path))) {
        # For zip files, we're done - just return success
        return(tibble(
          agency = agency_name,
          dataset = endpoint_info$name,
          year = if (\!is.null(year)) year else NA_integer_,
          url = url,
          local_path = cache_path,
          status = "success",
          timestamp = Sys.time(),
          file_size_mb = file_size,
          error_message = NA_character_
        ))
      }
      
      # Success
      return(tibble(
        agency = agency_name,
        dataset = endpoint_info$name,
        year = if (\!is.null(year)) year else NA_integer_,
        url = url,
        local_path = cache_path,
        status = "success",
        timestamp = Sys.time(),
        file_size_mb = file_size,
        error_message = NA_character_
      ))
      
    }, error = function(e) {
      # Capture any other errors
      error_message <- conditionMessage(e)
      
      # Create a result row with the error
      tibble(
        agency = agency_name,
        dataset = endpoint_info$name,
        year = if (\!is.null(year)) year else NA_integer_,
        url = if (exists("url")) url else "unknown",
        local_path = if (exists("cache_path")) cache_path else "unknown",
        status = "error",
        timestamp = Sys.time(),
        file_size_mb = 0,
        error_message = error_message
      )
    })
  }
  
  # Create a list of all download tasks
  download_tasks <- list()
  
  for (agency_name in names(all_agencies)) {
    agency_info <- all_agencies[[agency_name]]
    
    for (endpoint_info in agency_info$endpoints) {
      # Check if this endpoint is year-specific
      endpoint_years <- endpoint_info$years
      
      if (is.null(endpoint_years) || length(endpoint_years) == 0) {
        # Not year-specific - add as a single task
        download_tasks[[length(download_tasks) + 1]] <- list(
          agency_name = agency_name,
          agency_info = agency_info,
          endpoint_info = endpoint_info,
          year = NULL
        )
      } else {
        # Year-specific - add a task for each year in the intersection
        # of endpoint_years and requested years
        years_to_download <- intersect(endpoint_years, years)
        
        if (length(years_to_download) > 0) {
          for (year in years_to_download) {
            download_tasks[[length(download_tasks) + 1]] <- list(
              agency_name = agency_name,
              agency_info = agency_info,
              endpoint_info = endpoint_info,
              year = year
            )
          }
        }
      }
    }
  }
  
  if (verbose) {
    message("Created ", length(download_tasks), " download tasks")
  }
  
  # Execute all download tasks in parallel
  if (length(download_tasks) > 0) {
    # Use future.apply for parallel processing
    download_results <- future.apply::future_lapply(
      download_tasks,
      function(task) {
        download_dataset(
          task$agency_name, 
          task$agency_info, 
          task$endpoint_info, 
          task$year
        )
      },
      future.seed = TRUE
    )
    
    # Combine results
    results <- bind_rows(download_results)
  }
  
  # Create summary report
  if (verbose) {
    message("\nDownload Summary:")
    message("----------------")
    message("Total downloads attempted: ", nrow(results))
    message("Successful: ", sum(results$status == "success"))
    message("Already cached: ", sum(results$status == "already_cached"))
    message("Failed: ", sum(results$status %in% c("failed", "error")))
    message("Total data size: ", round(sum(results$file_size_mb), 2), " MB")
    
    # Show failed downloads if any
    failed <- results %>% filter(status %in% c("failed", "error"))
    if (nrow(failed) > 0) {
      message("\nFailed downloads:")
      for (i in 1:nrow(failed)) {
        fail_info <- failed[i, ]
        year_info <- if (\!is.na(fail_info$year)) paste0(" (", fail_info$year, ")") else ""
        message("- ", fail_info$agency, "/", fail_info$dataset, year_info, ": ", 
                fail_info$error_message)
      }
    }
  }
  
  # Create a summary file in the cache directory
  summary_file <- file.path(cache_dir, "cache_summary.csv")
  write_csv(results, summary_file)
  
  # Also create a more readable markdown report
  report_file <- file.path(cache_dir, "cache_report.md")
  
  report_content <- c(
    "# Federal Data Cache Report",
    "",
    paste("Generated on:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "",
    "## Summary",
    "",
    paste("- Total downloads attempted:", nrow(results)),
    paste("- Successful:", sum(results$status == "success")),
    paste("- Already cached:", sum(results$status == "already_cached")),
    paste("- Failed:", sum(results$status %in% c("failed", "error"))),
    paste("- Total data size:", round(sum(results$file_size_mb), 2), "MB"),
    "",
    "## Cache Contents by Agency",
    ""
  )
  
  # Add agency-specific summaries
  for (agency_name in unique(results$agency)) {
    agency_results <- results %>% filter(agency == agency_name)
    
    report_content <- c(report_content,
      paste("### ", agency_name),
      "",
      paste("Total datasets:", length(unique(agency_results$dataset))),
      paste("Total files:", nrow(agency_results)),
      paste("Total size:", round(sum(agency_results$file_size_mb), 2), "MB"),
      "",
      "| Dataset | Year | Status | Size (MB) | Path |",
      "| ------- | ---- | ------ | --------: | ---- |"
    )
    
    # Group by dataset
    for (dataset_name in unique(agency_results$dataset)) {
      dataset_results <- agency_results %>% filter(dataset == dataset_name)
      
      for (i in 1:nrow(dataset_results)) {
        row <- dataset_results[i, ]
        year_value <- if (\!is.na(row$year)) as.character(row$year) else "N/A"
        
        # Format file size with a consistent 2 decimal places
        size_value <- if (row$file_size_mb > 0) {
          sprintf("%.2f", row$file_size_mb)
        } else {
          "0"
        }
        
        report_content <- c(report_content,
          paste("|", row$dataset, "|", year_value, "|", row$status, "|", 
                size_value, "|", row$local_path, "|")
        )
      }
    }
    
    report_content <- c(report_content, "")
  }
  
  # Add failed downloads section
  failed <- results %>% filter(status %in% c("failed", "error"))
  if (nrow(failed) > 0) {
    report_content <- c(report_content,
      "## Failed Downloads",
      "",
      "| Agency | Dataset | Year | Error |",
      "| ------ | ------- | ---- | ----- |"
    )
    
    for (i in 1:nrow(failed)) {
      row <- failed[i, ]
      year_value <- if (\!is.na(row$year)) as.character(row$year) else "N/A"
      
      report_content <- c(report_content,
        paste("|", row$agency, "|", row$dataset, "|", year_value, "|", 
              row$error_message, "|")
      )
    }
  }
  
  # Write the report
  writeLines(report_content, report_file)
  
  if (verbose) {
    message("\nCache summary written to: ", summary_file)
    message("Detailed report written to: ", report_file)
  }
  
  # Return the results
  return(results)
}

# Execute as script if run directly
if (\!interactive()) {
  # Process command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  
  # Default values
  agencies <- NULL
  years_from <- 1970
  years_to <- as.numeric(format(Sys.Date(), "%Y"))
  cache_dir <- "data/cache/federal"
  max_threads <- 4
  refresh <- FALSE
  verbose <- TRUE
  
  # Parse arguments
  i <- 1
  while (i <= length(args)) {
    if (args[i] == "--agencies" && i < length(args)) {
      agencies <- strsplit(args[i + 1], ",")[[1]]
      i <- i + 2
    } else if (args[i] == "--years-from" && i < length(args)) {
      years_from <- as.numeric(args[i + 1])
      i <- i + 2
    } else if (args[i] == "--years-to" && i < length(args)) {
      years_to <- as.numeric(args[i + 1])
      i <- i + 2
    } else if (args[i] == "--cache-dir" && i < length(args)) {
      cache_dir <- args[i + 1]
      i <- i + 2
    } else if (args[i] == "--threads" && i < length(args)) {
      max_threads <- as.numeric(args[i + 1])
      i <- i + 2
    } else if (args[i] == "--refresh") {
      refresh <- TRUE
      i <- i + 1
    } else if (args[i] == "--quiet") {
      verbose <- FALSE
      i <- i + 1
    } else if (args[i] == "--help" || args[i] == "-h") {
      cat("Usage: Rscript cache_federal_data.r [options]\n")
      cat("\nOptions:\n")
      cat("  --agencies LIST     Comma-separated list of agencies to cache (default: all)\n")
      cat("                      Available: CDC, EPA, NOAA, NIH, HUD, USDA, HRSA, Census, BEA, BLS, FCC\n")
      cat("  --years-from YEAR   Start year for data (default: 1970)\n")
      cat("  --years-to YEAR     End year for data (default: current year)\n")
      cat("  --cache-dir DIR     Base directory for cached data (default: data/cache/federal)\n")
      cat("  --threads N         Maximum number of parallel download threads (default: 4)\n")
      cat("  --refresh           Force refresh of existing cached data\n")
      cat("  --quiet             Suppress verbose output\n")
      cat("  --help, -h          Show this help message\n")
      quit(save = "no", status = 0)
    } else {
      message("Unknown option: ", args[i])
      i <- i + 1
    }
  }
  
  # Validate years
  if (is.na(years_from) || is.na(years_to) || years_from > years_to) {
    stop("Invalid year range. years-from must be less than or equal to years-to.")
  }
  
  # Create year sequence
  years <- years_from:years_to
  
  # Run the caching function
  cache_federal_data(
    agencies = agencies,
    years = years,
    cache_dir = cache_dir,
    max_threads = max_threads,
    refresh = refresh,
    verbose = verbose
  )
}
EOL < /dev/null