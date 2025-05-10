#!/usr/bin/env Rscript

# Essential packages installer for the US-SocialDeterminantsOfHealth pipeline
# This script installs only the minimum required packages to run the pipeline

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

print_message("Installing essential packages for the US-SocialDeterminantsOfHealth pipeline", "INFO")
print_message("This is the minimal installation option - for full functionality use install_packages.r", "INFO")

# Check for system dependencies first
print_message("Checking R version and system information...", "INFO")
r_version <- getRversion()
if (r_version < "4.0.0") {
  print_message(paste("Your R version is", r_version, "which is older than the recommended version 4.0.0"), "WARNING")
}

# System info
sys_info <- Sys.info()
print_message(paste("System:", sys_info["sysname"], "- Version:", sys_info["release"]), "INFO")

# Essential packages list - only what's absolutely needed for core functionality
essential_packages <- c(
  # Core data handling
  "DBI", "duckdb", "data.table", "dplyr", "readr", "yaml", "jsonlite", 
  
  # Utilities
  "R.utils", "digest", "R6", "fs", "rlang", "httr", "curl", "stringr",
  
  # File handling
  "openxlsx", "readxl", "lubridate", "zoo", 
  
  # Traffic safety module essentials
  "glue", "purrr", "tidyr", "future", "future.apply",
  
  # Visualization packages
  "ggplot2", "viridis", "viridisLite", "RColorBrewer", "gridExtra"
)

# For spatial module (only if needed)
spatial_essentials <- c("sf", "tigris")

# Prompt user about spatial dependencies
if (interactive()) {
  install_spatial <- readline(prompt = "Do you want to install spatial packages (requires system dependencies)? (y/n): ")
  if (tolower(install_spatial) == "y") {
    essential_packages <- c(essential_packages, spatial_essentials)
    
    # Show spatial system dependencies
    os <- Sys.info()["sysname"]
    if (os == "Linux") {
      print_message("For spatial packages on Linux, you may need:", "INFO")
      print_message("sudo apt-get install libudunits2-dev libgdal-dev libgeos-dev libproj-dev", "INFO")
    } else if (os == "Darwin") {
      print_message("For spatial packages on macOS, you may need:", "INFO")
      print_message("brew install udunits gdal geos proj", "INFO")
    } else if (os == "Windows") {
      print_message("For spatial packages on Windows:", "INFO")
      print_message("Ensure Rtools is installed from https://cran.r-project.org/bin/windows/Rtools/", "INFO")
    }
    
    print_message("Installing spatial packages may take longer due to system dependencies", "INFO")
  }
} else {
  # Default to not installing spatial packages in non-interactive mode
  print_message("Skipping spatial packages in non-interactive mode", "INFO")
}

# Try to increase timeout for downloads
old_timeout <- getOption("timeout")
options(timeout = max(300, old_timeout)) # 5 minutes or current value, whichever is higher

# Install missing packages
missing_packages <- essential_packages[!sapply(essential_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  print_message(paste("Installing", length(missing_packages), "missing packages:", 
                 paste(missing_packages, collapse = ", ")), "INFO")
  
  # Install packages individually to avoid failing on a single package
  results <- sapply(missing_packages, function(pkg) {
    tryCatch({
      print_message(paste("Installing", pkg, "..."), "INFO")
      install.packages(pkg, repos = "https://cloud.r-project.org", dependencies = TRUE)
      return(TRUE)
    }, error = function(e) {
      print_message(paste("Failed to install", pkg, "-", conditionMessage(e)), "ERROR")
      return(FALSE)
    })
  })
  
  # Report on success rate
  success_count <- sum(results)
  if (success_count > 0) {
    print_message(paste("Successfully installed", success_count, "of", length(missing_packages), "packages"), "SUCCESS")
  }
} else {
  print_message("All essential packages are already installed!", "SUCCESS")
}

# Reset timeout
options(timeout = old_timeout)

# Final check and report
still_missing <- essential_packages[!sapply(essential_packages, requireNamespace, quietly = TRUE)]

if (length(still_missing) > 0) {
  print_message(paste("Some packages could not be installed automatically:", 
                 paste(still_missing, collapse = ", ")), "WARNING")
  
  # Provide targeted advice for spatial packages
  spatial_missing <- still_missing[still_missing %in% spatial_essentials]
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
  
  print_message("\nFor manual installation, try:", "INFO")
  print_message("install.packages(\"package_name\", repos = \"https://cloud.r-project.org\")", "INFO")
  
  # Apple Silicon specific advice
  if (Sys.info()["sysname"] == "Darwin" && grepl("arm64", Sys.info()["machine"])) {
    print_message("\nFor Apple Silicon (M1/M2) Mac, try installing as binary:", "INFO")
    print_message("install.packages(\"package_name\", type = \"binary\")", "INFO")
  }
  
  print_message("\nYou can still run parts of the pipeline, but some functionality may be limited.", "INFO")
} else {
  print_message("All essential packages have been successfully installed!", "SUCCESS")
  print_message("The pipeline should now be able to run with core functionality.", "SUCCESS")
  print_message("For advanced features, consider running the full installer: Rscript install_packages.r", "INFO")
}

# Print summary
installed_count <- sum(sapply(essential_packages, requireNamespace, quietly = TRUE))
print_message(paste("Installation summary:", installed_count, "of", length(essential_packages), "essential packages installed"), 
           if (installed_count == length(essential_packages)) "SUCCESS" else "WARNING")