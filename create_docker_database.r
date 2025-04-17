#\!/usr/bin/env Rscript

# create_docker_database.r
# Creates an initial database structure for Docker deployment
# with sample data to allow the pipeline to function without full data files

library(duckdb)
library(dplyr)
library(tidyr)
library(stringr)

# Create database file path
db_path <- "./data/us_county_sdoh_data.duckdb"

# Ensure directory exists
dir.create("./data", showWarnings = FALSE, recursive = TRUE)

# Connect to database
cat("Creating database at", db_path, "\n")
con <- dbConnect(duckdb::duckdb(), db_path)

# Create counties table
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS counties (
    geoid VARCHAR(5) PRIMARY KEY,
    name VARCHAR(100),
    state_fips VARCHAR(2),
    county_fips VARCHAR(3),
    state_name VARCHAR(100),
    region VARCHAR(20),
    division VARCHAR(20),
    land_area_sqmi FLOAT,
    population_2020 INTEGER
  )
")

# Insert sample county data
sample_counties <- data.frame(
  geoid = c("06001", "06037", "06075", "36061", "17031"),
  name = c("Alameda County", "Los Angeles County", "San Francisco County", "New York County", "Cook County"),
  state_fips = c("06", "06", "06", "36", "17"),
  county_fips = c("001", "037", "075", "061", "031"),
  state_name = c("California", "California", "California", "New York", "Illinois"),
  region = c("West", "West", "West", "Northeast", "Midwest"),
  division = c("Pacific", "Pacific", "Pacific", "Middle Atlantic", "East North Central"),
  land_area_sqmi = c(739.02, 4057.88, 46.87, 22.83, 944.99),
  population_2020 = c(1682353, 10014009, 873965, 1694251, 5275541)
)

dbWriteTable(con, "counties", sample_counties, append = TRUE)

# Create variables table
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS variables (
    variable_id VARCHAR(100) PRIMARY KEY,
    variable_name VARCHAR(100),
    description TEXT,
    category VARCHAR(50),
    subcategory VARCHAR(50),
    units VARCHAR(50),
    source VARCHAR(100),
    temporal_coverage VARCHAR(100),
    interpolation_method VARCHAR(50),
    extension_method VARCHAR(50),
    min_value FLOAT,
    max_value FLOAT,
    typical_range VARCHAR(50),
    notes TEXT
  )
")

# Insert sample variable data
sample_variables <- data.frame(
  variable_id = c("median_household_income", "poverty_rate", "life_expectancy", "total_population", "uninsured_pct"),
  variable_name = c("Median Household Income", "Poverty Rate", "Life Expectancy", "Total Population", "Uninsured Percentage"),
  description = c("Median household income in inflation-adjusted dollars", "Percentage of population living below the poverty line", "Average life expectancy at birth in years", "Total resident population", "Percentage of population without health insurance"),
  category = c("Economic", "Economic", "Health", "Demographic", "Health"),
  subcategory = c("Income", "Poverty", "Outcomes", "Population", "Coverage"),
  units = c("USD", "%", "Years", "Count", "%"),
  source = c("Census ACS", "Census ACS", "IHME", "Census PEP", "Census ACS"),
  temporal_coverage = c("2005-2022", "2005-2022", "1980-2019", "1970-2022", "2008-2022"),
  interpolation_method = c("ARIMA", "Cubic Spline", "Linear", "ARIMA", "Cubic Spline"),
  extension_method = c("Prophet", "XGBoost", "ARIMA", "Linear", "XGBoost"),
  min_value = c(0, 0, 0, 0, 0),
  max_value = c(NULL, 100, 100, NULL, 100),
  typical_range = c("30000-150000", "5-40", "65-85", "1000-10000000", "0-30"),
  notes = c("Adjusted for inflation to most recent year", "Uses federal poverty threshold", "Combined estimate across race/ethnicity", "Census resident population", "Non-elderly population without health insurance")
)

dbWriteTable(con, "variables", sample_variables, append = TRUE)

# Create main data table
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS county_sdoh_data (
    geoid VARCHAR(5),
    year INTEGER,
    variable_id VARCHAR(100),
    value FLOAT,
    data_quality VARCHAR(20),
    data_source VARCHAR(100),
    data_vintage VARCHAR(50),
    data_quality_score INTEGER,
    interpolated BOOLEAN,
    extended BOOLEAN,
    PRIMARY KEY (geoid, year, variable_id),
    FOREIGN KEY (geoid) REFERENCES counties(geoid),
    FOREIGN KEY (variable_id) REFERENCES variables(variable_id)
  )
")

# Create sample data for each variable, county, and year from 2015-2022
generate_sample_data <- function() {
  # Years to generate
  years <- 2015:2022
  
  # Create all combinations of counties, years, and variables
  data <- expand.grid(
    geoid = sample_counties$geoid,
    year = years,
    variable_id = sample_variables$variable_id,
    stringsAsFactors = FALSE
  )
  
  # Add values based on the variable type
  data$value <- NA
  data$data_quality <- "direct"
  data$data_source <- "Simulated"
  data$data_vintage <- paste0("SDOH_DB_", format(Sys.Date(), "%Y%m%d"))
  data$data_quality_score <- 4
  data$interpolated <- FALSE
  data$extended <- FALSE
  
  # Generate realistic values for each variable
  for (var_id in unique(data$variable_id)) {
    var_data <- sample_variables[sample_variables$variable_id == var_id,]
    
    # Set base and trend parameters based on variable type
    if (var_id == "median_household_income") {
      # Income increases over time with county variations
      for (county in unique(data$geoid)) {
        county_idx <- which(data$geoid == county & data$variable_id == var_id)
        base <- runif(1, 40000, 100000) # Different base for each county
        growth <- runif(1, 0.01, 0.05) # Different growth rate
        
        for (i in seq_along(years)) {
          idx <- county_idx[data$year[county_idx] == years[i]]
          data$value[idx] <- base * (1 + growth)^(i-1) + runif(1, -2000, 2000)
        }
      }
    } else if (var_id == "poverty_rate") {
      # Poverty rate slowly declining with variations
      for (county in unique(data$geoid)) {
        county_idx <- which(data$geoid == county & data$variable_id == var_id)
        base <- runif(1, 5, 25) # Different base for each county
        trend <- runif(1, -0.5, 0.2) # Some improving, some worsening
        
        for (i in seq_along(years)) {
          idx <- county_idx[data$year[county_idx] == years[i]]
          data$value[idx] <- max(0, min(100, base + trend * (i-1) + runif(1, -1, 1)))
        }
      }
    } else if (var_id == "life_expectancy") {
      # Life expectancy slowly increasing with variations
      for (county in unique(data$geoid)) {
        county_idx <- which(data$geoid == county & data$variable_id == var_id)
        base <- runif(1, 70, 82) # Different base for each county
        trend <- runif(1, 0, 0.3) # Slowly improving
        
        for (i in seq_along(years)) {
          idx <- county_idx[data$year[county_idx] == years[i]]
          data$value[idx] <- base + trend * (i-1) + runif(1, -0.3, 0.3)
        }
      }
    } else if (var_id == "total_population") {
      # Population growing over time
      for (county in unique(data$geoid)) {
        county_idx <- which(data$geoid == county & data$variable_id == var_id)
        # Use the 2020 population as reference
        base <- sample_counties$population_2020[sample_counties$geoid == county]
        growth <- runif(1, -0.01, 0.02) # Some shrinking, some growing
        
        for (i in seq_along(years)) {
          year_diff <- years[i] - 2020
          idx <- county_idx[data$year[county_idx] == years[i]]
          data$value[idx] <- base * (1 + growth)^year_diff + runif(1, -1000, 1000)
        }
      }
    } else if (var_id == "uninsured_pct") {
      # Uninsured percentage declining
      for (county in unique(data$geoid)) {
        county_idx <- which(data$geoid == county & data$variable_id == var_id)
        base <- runif(1, 5, 25) # Different base for each county
        trend <- runif(1, -2, -0.2) # Generally improving
        
        for (i in seq_along(years)) {
          idx <- county_idx[data$year[county_idx] == years[i]]
          data$value[idx] <- max(0, min(100, base + trend * (i-1) + runif(1, -1, 1)))
        }
      }
    }
  }
  
  # Add some interpolated and extended values for realism
  interp_idx <- sample(1:nrow(data), nrow(data) * 0.1) # 10% interpolated
  data$interpolated[interp_idx] <- TRUE
  data$data_quality[interp_idx] <- "interpolated"
  data$data_quality_score[interp_idx] <- 2
  
  ext_idx <- sample(1:nrow(data), nrow(data) * 0.05) # 5% extended
  data$extended[ext_idx] <- TRUE
  data$data_quality[ext_idx] <- "extended"
  data$data_quality_score[ext_idx] <- 1
  
  return(data)
}

# Generate and insert sample data
sample_data <- generate_sample_data()
dbWriteTable(con, "county_sdoh_data", sample_data, append = TRUE)

# Create quality summary table
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS data_quality_summary (
    year INTEGER,
    variable_id VARCHAR(100),
    total_counties INTEGER,
    direct_count INTEGER,
    interpolated_count INTEGER,
    extended_count INTEGER,
    missing_count INTEGER,
    completeness_pct FLOAT,
    PRIMARY KEY (year, variable_id),
    FOREIGN KEY (variable_id) REFERENCES variables(variable_id)
  )
")

# Insert sample quality summary
sample_quality <- data.frame(
  year = rep(2015:2022, each = length(sample_variables$variable_id)),
  variable_id = rep(sample_variables$variable_id, times = 8),
  total_counties = rep(nrow(sample_counties), 40),
  direct_count = sample(3:5, 40, replace = TRUE),
  interpolated_count = sample(0:1, 40, replace = TRUE),
  extended_count = sample(0:1, 40, replace = TRUE),
  missing_count = 0,
  completeness_pct = 100
)

dbWriteTable(con, "data_quality_summary", sample_quality, append = TRUE)

# Create useful views

# Latest data view
dbExecute(con, "
  CREATE VIEW latest_county_data AS
  SELECT c.geoid, c.name, c.state_name, d.year, v.variable_id, v.variable_name, d.value, 
         d.data_quality, d.data_source, d.interpolated, d.extended
  FROM counties c
  JOIN (
    SELECT geoid, variable_id, MAX(year) as max_year
    FROM county_sdoh_data
    GROUP BY geoid, variable_id
  ) latest ON c.geoid = latest.geoid
  JOIN county_sdoh_data d ON d.geoid = latest.geoid AND d.variable_id = latest.variable_id AND d.year = latest.max_year
  JOIN variables v ON v.variable_id = d.variable_id
")

# Time series view
dbExecute(con, "
  CREATE VIEW county_time_series AS
  SELECT c.geoid, c.name, c.state_name, d.year, v.variable_id, v.variable_name, 
         v.category, v.subcategory, d.value, d.data_quality, 
         d.interpolated, d.extended
  FROM counties c
  JOIN county_sdoh_data d ON c.geoid = d.geoid
  JOIN variables v ON v.variable_id = d.variable_id
  ORDER BY c.geoid, v.variable_id, d.year
")

# Health metrics view
dbExecute(con, "
  CREATE VIEW county_health_metrics AS
  SELECT c.geoid, c.name, c.state_name, d.year, v.variable_id, v.variable_name, 
         d.value, d.data_quality, d.interpolated, d.extended
  FROM counties c
  JOIN county_sdoh_data d ON c.geoid = d.geoid
  JOIN variables v ON v.variable_id = d.variable_id
  WHERE v.category = 'Health'
  ORDER BY c.geoid, v.variable_id, d.year
")

# Economic metrics view
dbExecute(con, "
  CREATE VIEW county_economic_metrics AS
  SELECT c.geoid, c.name, c.state_name, d.year, v.variable_id, v.variable_name, 
         d.value, d.data_quality, d.interpolated, d.extended
  FROM counties c
  JOIN county_sdoh_data d ON c.geoid = d.geoid
  JOIN variables v ON v.variable_id = d.variable_id
  WHERE v.category = 'Economic'
  ORDER BY c.geoid, v.variable_id, d.year
")

# Demographic metrics view
dbExecute(con, "
  CREATE VIEW county_demographic_metrics AS
  SELECT c.geoid, c.name, c.state_name, d.year, v.variable_id, v.variable_name, 
         d.value, d.data_quality, d.interpolated, d.extended
  FROM counties c
  JOIN county_sdoh_data d ON c.geoid = d.geoid
  JOIN variables v ON v.variable_id = d.variable_id
  WHERE v.category = 'Demographic'
  ORDER BY c.geoid, v.variable_id, d.year
")

# Create directory structure for docker setup
dir.create("./data/cache", showWarnings = FALSE, recursive = TRUE)
dir.create("./output/maps", showWarnings = FALSE, recursive = TRUE)
dir.create("./logs", showWarnings = FALSE, recursive = TRUE)

# Create a last_update.txt file in the data directory
cat(as.character(Sys.time()), file = "./data/last_update.txt")

# Close database connection
dbDisconnect(con, shutdown = TRUE)

cat("Database initialization complete.\n")
cat("Sample data created for", nrow(sample_counties), "counties\n")
cat("Sample data created for", nrow(sample_variables), "variables\n")
cat("Sample data created for", length(unique(sample_data$year)), "years\n")
cat("Total sample records:", nrow(sample_data), "\n")
EOL < /dev/null