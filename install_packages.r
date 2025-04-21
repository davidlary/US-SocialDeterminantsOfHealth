#!/usr/bin/env Rscript

# Install required packages for the US-SocialDeterminantsOfHealth pipeline

cat("Installing required packages for the US-SocialDeterminantsOfHealth pipeline...\n")

# Core packages
core_packages <- c(
  "tidyverse", "DBI", "duckdb", "data.table", "zoo", "sf", "tigris",
  "jsonlite", "glue", "stringr", "lubridate", "httr", "readxl"
)

# Visualization packages
viz_packages <- c(
  "ggplot2", "viridis", "RColorBrewer", "plotly", "leaflet", "shiny",
  "shinydashboard", "DT", "htmlwidgets", "mapview", "gridExtra"
)

# Data processing packages
data_packages <- c(
  "imputeTS", "forecast", "furrr", "future", "future.apply", "progressr"
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

# Combine all packages
all_packages <- c(core_packages, viz_packages, data_packages, traffic_packages, ml_packages, api_packages)

# Install missing packages
missing_packages <- all_packages[!sapply(all_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  cat("Installing the following packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
} else {
  cat("All required packages are already installed.\n")
}

# Check if all packages are now installed
still_missing <- all_packages[!sapply(all_packages, requireNamespace, quietly = TRUE)]

if (length(still_missing) > 0) {
  cat("Warning: The following packages could not be installed:", paste(still_missing, collapse = ", "), "\n")
  cat("Please install them manually.\n")
} else {
  cat("All packages have been successfully installed!\n")
}

cat("Package installation complete!\n")