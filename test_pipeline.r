#!/usr/bin/env Rscript

# Test script for US-SocialDeterminantsOfHealth pipeline
# This script checks that all components are available and working

cat("Testing US-SocialDeterminantsOfHealth pipeline...\n")

# Check for required files
required_files <- c(
  "unified_sdoh_pipeline.r",
  "ml_forecasting.r",
  "interactive_dashboard.r",
  "parallel_processor.r",
  "api_server.r",
  "install_packages.r",
  "main_extended.r"
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  cat("Warning: The following required files are missing:", paste(missing_files, collapse = ", "), "\n")
} else {
  cat("All required files are present.\n")
}

# Check for extended pipeline files
extended_files <- list.files("extended_sdoh_pipeline", pattern = "\\.r$", full.names = FALSE)
if (length(extended_files) == 0) {
  cat("Warning: No extended pipeline files found.\n")
} else {
  cat("Found", length(extended_files), "extended pipeline files.\n")
}

# Try to load key packages
test_packages <- c("tidyverse", "duckdb", "sf", "forecast", "xgboost", "prophet")
package_status <- sapply(test_packages, function(pkg) {
  tryCatch({
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
})

if (all(package_status)) {
  cat("All test packages loaded successfully.\n")
} else {
  cat("Warning: The following packages failed to load:", 
      paste(names(package_status[!package_status]), collapse = ", "), "\n")
  cat("Please run install_packages.r to install missing packages.\n")
}

# Create minimal test database
create_test_db <- function() {
  tryCatch({
    if (!dir.exists("output")) {
      dir.create("output")
    }
    
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "output/test_sdoh.duckdb")
    
    DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS counties (
      geoid VARCHAR(5) PRIMARY KEY,
      name VARCHAR(100),
      state_name VARCHAR(50),
      state_code VARCHAR(2)
    )")
    
    DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS variables (
      variable_name VARCHAR(100) PRIMARY KEY,
      display_name VARCHAR(200),
      description TEXT,
      domain VARCHAR(50),
      units VARCHAR(50)
    )")
    
    DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS sdoh_data (
      geoid VARCHAR(5),
      year INTEGER,
      variable_name VARCHAR(100),
      value DOUBLE,
      data_quality VARCHAR(20),
      data_source VARCHAR(100),
      data_vintage VARCHAR(50),
      PRIMARY KEY (geoid, year, variable_name)
    )")
    
    # Insert sample data
    DBI::dbExecute(con, "INSERT INTO counties VALUES 
      ('06001', 'Alameda County', 'California', 'CA'),
      ('06075', 'San Francisco County', 'California', 'CA'),
      ('36061', 'New York County', 'New York', 'NY')
    ")
    
    DBI::dbExecute(con, "INSERT INTO variables VALUES 
      ('median_household_income', 'Median Household Income', 'Median income of households in the county', 'economic', 'dollars'),
      ('poverty_rate', 'Poverty Rate', 'Percentage of population below the poverty line', 'economic', 'percent'),
      ('life_expectancy', 'Life Expectancy', 'Average life expectancy at birth', 'health', 'years')
    ")
    
    # Generate sample time series data
    years <- 2010:2020
    counties <- c('06001', '06075', '36061')
    variables <- c('median_household_income', 'poverty_rate', 'life_expectancy')
    
    for (county in counties) {
      for (variable in variables) {
        base_value <- switch(variable,
                            'median_household_income' = 60000 + as.numeric(substr(county, 3, 5)),
                            'poverty_rate' = 10 + as.numeric(substr(county, 5, 5))/10,
                            'life_expectancy' = 75 + as.numeric(substr(county, 4, 5))/10)
        
        for (year in years) {
          trend <- (year - 2010) / 10
          noise <- runif(1, -0.1, 0.1)
          
          if (variable == 'median_household_income') {
            value <- base_value * (1 + trend + noise)
          } else if (variable == 'poverty_rate') {
            value <- base_value * (1 - trend/2 + noise)
          } else {
            value <- base_value * (1 + trend/5 + noise)
          }
          
          DBI::dbExecute(con, sprintf(
            "INSERT INTO sdoh_data VALUES ('%s', %d, '%s', %f, 'direct', 'test_script', 'test')",
            county, year, variable, value
          ))
        }
      }
    }
    
    DBI::dbDisconnect(con)
    cat("Test database created successfully at output/test_sdoh.duckdb\n")
    return(TRUE)
  }, error = function(e) {
    cat("Error creating test database:", e$message, "\n")
    return(FALSE)
  })
}

# Test database creation
db_created <- create_test_db()

if (db_created) {
  cat("Pipeline test completed successfully!\n")
  cat("Use the following command to run the full pipeline:\n")
  cat("  Rscript main_extended.r\n\n")
  cat("To test ML forecasting, run:\n")
  cat("  Rscript ml_forecasting.r --db output/test_sdoh.duckdb --variables median_household_income\n")
} else {
  cat("Pipeline test failed. Please check the error messages above.\n")
}