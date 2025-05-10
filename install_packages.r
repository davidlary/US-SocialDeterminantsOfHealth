#!/usr/bin/env Rscript

# Enhanced installer for required packages for the US-SocialDeterminantsOfHealth pipeline

# Helper function to print colored messages
print_message <- function(message, type = "INFO") {
  color_start <- switch(type,
                        "INFO" = "\033[0;36m",  # Cyan
                        "SUCCESS" = "\033[0;32m",  # Green
                        "WARNING" = "\033[0;33m",  # Yellow
                        "ERROR" = "\033[0;31m",  # Red
                        "\033[0m")  # Default/Reset
  color_end <- "\033[0m"
  
  # Check if terminal supports colors
  if (Sys.getenv("TERM") != "" && Sys.info()["sysname"] != "Windows") {
    cat(paste0(color_start, "[", type, "] ", message, color_end, "\n"))
  } else {
    cat(paste0("[", type, "] ", message, "\n"))
  }
}

print_message("Starting installation of required packages for the US-SocialDeterminantsOfHealth pipeline", "INFO")

# Check for system dependencies first
print_message("Checking R version and system information...", "INFO")
r_version <- getRversion()
if (r_version < "4.0.0") {
  print_message(paste("Your R version is", r_version, "which is older than the recommended minimum version 4.0.0"), "WARNING")
  print_message("Some packages may not install or function correctly. Consider updating R.", "WARNING")
}

# System info
sys_info <- Sys.info()
print_message(paste("System:", sys_info["sysname"], "- Version:", sys_info["release"]), "INFO")

# Check for system dependencies based on OS
check_system_dependencies <- function() {
  os <- Sys.info()["sysname"]
  
  dependency_message <- ""
  
  if (os == "Linux") {
    dependency_message <- paste(
      "You are running Linux. For spatial packages, you may need to install: ",
      "libudunits2-dev libgdal-dev libgeos-dev libproj-dev",
      "\n\nOn Ubuntu/Debian: sudo apt-get install libudunits2-dev libgdal-dev libgeos-dev libproj-dev",
      "\nOn CentOS/RHEL: sudo yum install udunits2-devel gdal-devel geos-devel proj-devel"
    )
  } else if (os == "Darwin") {  # macOS
    dependency_message <- paste(
      "You are running macOS. For spatial packages, you may need to install: ",
      "udunits gdal geos proj",
      "\n\nUsing Homebrew: brew install udunits gdal geos proj",
      "\nFor Apple Silicon (M1/M2) Macs: You may need to install packages with type='binary'"
    )
  } else if (os == "Windows") {
    dependency_message <- paste(
      "You are running Windows. For spatial packages: ",
      "\n- Ensure you have Rtools installed from https://cran.r-project.org/bin/windows/Rtools/",
      "\n- Make sure PATH is set correctly",
      "\n- For memory issues, consider running: memory.limit(size = 16000)"
    )
  }
  
  if (dependency_message != "") {
    print_message("System Dependencies Information:", "INFO")
    cat(dependency_message, "\n\n")
  }
}

check_system_dependencies()

# Define package categories
print_message("Defining required packages by category...", "INFO")

# Core packages
core_packages <- c(
  "tidyverse", "DBI", "duckdb", "data.table", "zoo", "yaml", 
  "jsonlite", "glue", "stringr", "lubridate", "httr", "readxl", "curl"
)

# Spatial packages (may require system dependencies)
spatial_packages <- c(
  "sf", "tigris", "leaflet", "mapview", "tmap", "rgdal", "rgeos", "raster", "stars"
)

# Visualization packages
viz_packages <- c(
  "ggplot2", "viridis", "RColorBrewer", "plotly", "shiny",
  "shinydashboard", "DT", "htmlwidgets", "gridExtra", "scales", "patchwork",
  "viridisLite"
)

# Data processing packages
data_packages <- c(
  "imputeTS", "forecast", "furrr", "future", "future.apply", "progressr",
  "purrr", "dplyr", "tidyr", "readr", "janitor", "xml2", "rvest", "openxlsx"
)

# Traffic safety module packages
traffic_packages <- c(
  "digest", "R6", "fs", "R.utils", "quantmod", "tseries", 
  "forecastHybrid", "rlang", "arrow"
)

# Machine learning packages
ml_packages <- c(
  "prophet", "xgboost", "randomForest", "glmnet", "e1071", "kernlab",
  "Metrics", "tidymodels", "rsample", "recipes", "parsnip", "workflows",
  "iml", "modeltime", "caret"
)

# API packages
api_packages <- c(
  "plumber", "swagger"
)

# Define package priorities (grouped by importance and dependency relationships)
priority_packages <- list(
  critical = c("yaml", "jsonlite", "httr", "curl", "stringr", "data.table", "dplyr"), 
  essential = c("DBI", "duckdb", "readr", "zoo", "lubridate", "rlang", "purrr", "R6"),
  standard = c("tidyverse", "ggplot2", "readxl", "openxlsx", "fs", "future", "future.apply"),
  spatial = spatial_packages,
  extended = c(viz_packages, data_packages, traffic_packages),
  optional = c(ml_packages, api_packages)
)

# Install packages in priority order
install_priority_packages <- function(packages, priority_name) {
  print_message(paste("Installing", priority_name, "packages..."), "INFO")
  missing_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    print_message(paste("Installing", length(missing_packages), priority_name, "packages:", 
                     paste(missing_packages, collapse = ", ")), "INFO")
    
    # Try to install each package individually to avoid halting on a single failure
    results <- sapply(missing_packages, function(pkg) {
      tryCatch({
        install.packages(pkg, repos = "https://cloud.r-project.org", dependencies = TRUE)
        return(TRUE)
      }, error = function(e) {
        print_message(paste("Failed to install", pkg, "-", conditionMessage(e)), "ERROR")
        return(FALSE)
      })
    })
    
    success_count <- sum(results)
    if (success_count > 0) {
      print_message(paste("Successfully installed", success_count, "of", length(missing_packages), 
                       priority_name, "packages"), "SUCCESS")
    }
    
    # Check for still missing packages
    still_missing <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
    if (length(still_missing) > 0) {
      print_message(paste("Some", priority_name, "packages could not be installed:", 
                       paste(still_missing, collapse = ", ")), "WARNING")
    }
  } else {
    print_message(paste("All", priority_name, "packages are already installed!"), "SUCCESS")
  }
}

# Try to create a temporary environment variable to increase timeout
old_timeout <- getOption("timeout")
options(timeout = max(300, old_timeout)) # 5 minutes or current value, whichever is higher

# Install packages in priority order
for (priority in names(priority_packages)) {
  install_priority_packages(priority_packages[[priority]], priority)
}

# Reset timeout
options(timeout = old_timeout)

# Final check of all packages
all_packages <- unique(unlist(priority_packages))
still_missing <- all_packages[!sapply(all_packages, requireNamespace, quietly = TRUE)]

if (length(still_missing) > 0) {
  print_message("Installation Summary:", "WARNING")
  print_message(paste("The following packages could not be installed automatically:", 
                 paste(still_missing, collapse = ", ")), "WARNING")
  
  # Provide targeted advice for spatial packages
  spatial_missing <- still_missing[still_missing %in% spatial_packages]
  if (length(spatial_missing) > 0) {
    print_message("Spatial packages often require system dependencies:", "INFO")
    
    os <- Sys.info()["sysname"]
    if (os == "Linux") {
      print_message("Try: sudo apt-get install libudunits2-dev libgdal-dev libgeos-dev libproj-dev", "INFO")
    } else if (os == "Darwin") {
      print_message("Try: brew install udunits gdal geos proj", "INFO")
    } else if (os == "Windows") {
      print_message("Ensure Rtools is installed and PATH is set correctly", "INFO")
    }
  }
  
  # Advice for manually installing packages
  print_message("\nTo manually install missing packages, try:", "INFO")
  print_message("install.packages(\"package_name\", repos = \"https://cloud.r-project.org\", dependencies = TRUE)", "INFO")
  
  # For Apple Silicon specific advice
  if (Sys.info()["sysname"] == "Darwin" && grepl("arm64", Sys.info()["machine"])) {
    print_message("\nFor Apple Silicon (M1/M2) Mac, try installing as binary:", "INFO")
    print_message("install.packages(\"package_name\", type = \"binary\", repos = \"https://cloud.r-project.org\")", "INFO")
  }
} else {
  print_message("All packages have been successfully installed!", "SUCCESS")
}

# Print a summary of installed packages by category
print_summary <- function() {
  print_message("\nInstallation Summary by Category:", "INFO")
  
  for (category in names(priority_packages)) {
    packages <- priority_packages[[category]]
    installed <- sum(sapply(packages, requireNamespace, quietly = TRUE))
    total <- length(packages)
    
    status <- if (installed == total) "SUCCESS" else "WARNING"
    print_message(paste0(category, ": ", installed, "/", total, " installed"), status)
  }
  
  # Print final success message
  total_installed <- sum(sapply(all_packages, requireNamespace, quietly = TRUE))
  print_message(paste0("Total: ", total_installed, "/", length(all_packages), " packages installed"), 
             if (total_installed == length(all_packages)) "SUCCESS" else "WARNING")
}

print_summary()
print_message("Package installation process complete!", "SUCCESS")