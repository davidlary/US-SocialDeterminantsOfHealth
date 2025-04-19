#!/usr/bin/env Rscript

# Simple script to install missing packages required for the traffic safety module

cat("Installing required packages for traffic safety module...\n")

# Traffic safety module packages
packages_to_install <- c(
  "R.utils",
  "digest",
  "openxlsx",
  "R6"
)

# Install missing packages
missing_packages <- packages_to_install[!sapply(packages_to_install, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  cat("Installing the following packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
} else {
  cat("All required packages are already installed.\n")
}

# Check if all packages are now installed
still_missing <- packages_to_install[!sapply(packages_to_install, requireNamespace, quietly = TRUE)]

if (length(still_missing) > 0) {
  cat("Warning: The following packages could not be installed:", paste(still_missing, collapse = ", "), "\n")
  cat("Please install them manually.\n")
} else {
  cat("All packages have been successfully installed!\n")
}

cat("Package installation complete!\n")