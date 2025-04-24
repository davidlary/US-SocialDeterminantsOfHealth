# Create Sample Data for Testing
#
# This script generates sample data files for testing the SDOH pipeline
# without needing to download large datasets or access restricted APIs.
#
# Usage: Rscript R/utilities/create_sample_data.r

# Check and install required packages
required_packages <- c("dplyr", "tidyr", "readr", "yaml", "stringr")
new_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(new_packages) > 0) {
  install.packages(new_packages)
}

# Load required libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(yaml)
  library(stringr)
})

# Create main directories if they don't exist
main_dirs <- c(
  "data/cache",
  "data/census_acs",
  "data/census_decennial",
  "data/census_pep",
  "data/cdc_places",
  "data/epa/air_quality",
  "data/epa/tri",
  "data/healthcare",
  "data/traffic_safety/fars",
  "output/maps",
  "logs"
)

for (dir in main_dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat("Created directory:", dir, "\n")
  }
}

# Set random seed for reproducibility
set.seed(42)

# ----- Create sample counties dataset -----
cat("Creating sample county base dataset...\n")

# Get list of US states and territories
states <- data.frame(
  STATE = c(1:56, 72),
  STATE_ABBR = c(
    "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
    "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
    "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
    "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
    "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI",
    "WY", "AS", "GU", "MP", "PR", "VI"
  ),
  stringsAsFactors = FALSE
)

# Create sample counties
# In real data, each state would have many counties, but for sample data
# we'll create 5 counties per state to keep the dataset manageable
counties <- data.frame()

for (i in 1:nrow(states)) {
  state_fips <- sprintf("%02d", states$STATE[i])
  state_abbr <- states$STATE_ABBR[i]
  
  # Generate 5 sample counties per state (just for demonstration)
  for (j in 1:5) {
    county_fips <- sprintf("%03d", j)
    geoid <- paste0(state_fips, county_fips)
    
    county_name <- paste0(state_abbr, " Sample County ", j)
    
    # For realistic population distribution, use exponentially decreasing sizes
    pop_base <- 500000 * exp(-0.5 * j) + runif(1, 5000, 20000)
    
    counties <- rbind(counties, data.frame(
      GEOID = geoid,
      STATE = state_fips,
      COUNTY = county_fips,
      NAME = county_name,
      STATE_NAME = state_abbr,
      POPULATION = round(pop_base),
      stringsAsFactors = FALSE
    ))
  }
}

# ----- Create sample Census ACS data -----
cat("Creating sample Census ACS data...\n")

# Generate realistic sample ACS data
acs_data <- counties %>%
  mutate(
    YEAR = 2021,
    
    # Demographic variables
    median_age = round(runif(n(), 25, 65), 1),
    pct_under_18 = round(runif(n(), 10, 35), 1),
    pct_over_65 = round(runif(n(), 5, 30), 1),
    
    # Economic variables
    median_household_income = round(runif(n(), 30000, 120000)),
    per_capita_income = round(runif(n(), 15000, 70000)),
    pct_poverty = round(runif(n(), 2, 30), 1),
    pct_unemployment = round(runif(n(), 2, 15), 1),
    
    # Housing variables
    median_home_value = round(runif(n(), 80000, 750000)),
    pct_homeownership = round(runif(n(), 40, 90), 1),
    pct_housing_cost_burden = round(runif(n(), 10, 50), 1),
    
    # Education variables
    pct_high_school_grad = round(runif(n(), 60, 98), 1),
    pct_bachelors_degree = round(runif(n(), 5, 70), 1),
    
    # Healthcare variables
    pct_uninsured = round(runif(n(), 1, 25), 1),
    
    # Transportation variables
    mean_travel_time = round(runif(n(), 10, 45), 1),
    pct_public_transit = round(runif(n(), 0, 30), 1)
  )

# Write sample ACS data
write_csv(acs_data, "data/census_acs/acs5_county_2021.csv")

# ----- Create sample Census Decennial data -----
cat("Creating sample Census Decennial data...\n")

# Generate realistic sample decennial census data
dec_data <- counties %>%
  mutate(
    YEAR = 2020,
    
    # Demographic variables
    total_population = POPULATION, # use the base population
    total_housing_units = round(total_population / runif(n(), 2.2, 3.1)),
    
    # Race/ethnicity variables
    pct_white = round(runif(n(), 30, 95), 1),
    pct_black = round(runif(n(), 0.5, 45), 1),
    pct_hispanic = round(runif(n(), 1, 60), 1),
    pct_asian = round(runif(n(), 0.5, 35), 1),
    pct_native = round(runif(n(), 0.1, 15), 1)
  ) %>%
  # Adjust percentages to sum to approximately 100 (with some overlaps for multi-racial)
  mutate(
    pct_white = pct_white * 100 / (pct_white + pct_black + pct_hispanic + pct_asian + pct_native),
    pct_black = pct_black * 100 / (pct_white + pct_black + pct_hispanic + pct_asian + pct_native),
    pct_hispanic = pct_hispanic * 100 / (pct_white + pct_black + pct_hispanic + pct_asian + pct_native),
    pct_asian = pct_asian * 100 / (pct_white + pct_black + pct_hispanic + pct_asian + pct_native),
    pct_native = pct_native * 100 / (pct_white + pct_black + pct_hispanic + pct_asian + pct_native)
  )

# Write sample decennial data
write_csv(dec_data, "data/census_decennial/dec_county_2020.csv")

# ----- Create sample Population Estimates data -----
cat("Creating sample Population Estimates data...\n")

# Generate realistic sample population estimates
pep_data <- counties %>%
  mutate(
    YEAR = 2022,
    
    # Population with a small change from the base
    population_estimate = round(POPULATION * (1 + runif(n(), -0.05, 0.15))),
    
    # Components of change
    births = round(population_estimate * runif(n(), 0.005, 0.015)),
    deaths = round(population_estimate * runif(n(), 0.005, 0.015)),
    international_migration = round(population_estimate * runif(n(), -0.005, 0.015)),
    domestic_migration = round(population_estimate * runif(n(), -0.03, 0.03))
  )

# Write sample PEP data
write_csv(pep_data, "data/census_pep/pep_county_2022.csv")

# ----- Create sample EPA Air Quality data -----
cat("Creating sample EPA Air Quality data...\n")

# Generate realistic sample air quality data
aqi_data <- counties %>%
  mutate(
    YEAR = 2021,
    
    # Air quality variables
    aqi_mean = round(runif(n(), 30, 150)),
    aqi_max = round(aqi_mean + runif(n(), 10, 100)),
    aqi_90th_percentile = round(aqi_mean + (aqi_max - aqi_mean) * 0.7),
    
    days_unhealthy = round(runif(n(), 0, 60)),
    days_very_unhealthy = round(runif(n(), 0, days_unhealthy * 0.3)),
    
    pm25_mean = round(runif(n(), 3, 20), 1),
    ozone_mean = round(runif(n(), 0.02, 0.07), 3)
  )

# Write sample AQI data
write_csv(aqi_data, "data/epa/air_quality/aqi_2021.csv")

# ----- Create sample FARS data -----
cat("Creating sample FARS traffic safety data...\n")

# Generate realistic sample FARS data
fars_data <- counties %>%
  mutate(
    YEAR = 2020,
    
    # Traffic safety variables
    total_fatalities = round(POPULATION * runif(n(), 0.00001, 0.0003)),
    
    # Ensure we have at least some fatalities in most counties
    total_fatalities = pmax(1, total_fatalities),
    
    alcohol_impaired = round(total_fatalities * runif(n(), 0.2, 0.5)),
    speeding_related = round(total_fatalities * runif(n(), 0.25, 0.45)),
    unrestrained = round(total_fatalities * runif(n(), 0.15, 0.55)),
    
    fatality_rate_per_100k = round(total_fatalities / POPULATION * 100000, 2)
  )

# Write sample FARS data
write_csv(fars_data, "data/traffic_safety/fars/FARS_2020_county.csv")

# ----- Create a sample CDC WONDER data -----
cat("Creating sample CDC WONDER data...\n")

# Generate realistic sample CDC WONDER data
cdc_data <- counties %>%
  slice(1:50) %>%  # Use only a subset for this sample
  mutate(
    Notes = "Sample data for testing purposes",
    
    # Crude rate per 100,000
    Crude_Rate = round(runif(n(), 5, 30), 1),
    
    # Deaths and Population
    Deaths = round(POPULATION * Crude_Rate / 100000),
    Population = POPULATION,
    
    # Add standard statistical measures
    Standard_Error = round(sqrt(Deaths) / (Population / 100000), 2),
    Rate_Lower_95CI = pmax(0, round(Crude_Rate - 1.96 * Standard_Error, 1)),
    Rate_Upper_95CI = round(Crude_Rate + 1.96 * Standard_Error, 1)
  ) %>%
  select(
    County = NAME, 
    State = STATE_NAME,
    FIPS = GEOID,
    Deaths,
    Population,
    Crude_Rate,
    Standard_Error,
    Rate_Lower_95CI,
    Rate_Upper_95CI,
    Notes
  )

# Write sample CDC WONDER data
write_csv(cdc_data, "data/traffic_safety/cdc/sample_cdc_wonder_data.csv")

# ----- Create a timestamp file to track when sample data was created -----
cat("Creating timestamp and README files...\n")

# Create a timestamp file
timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
writeLines(paste0("Sample data created: ", timestamp), "data/last_update.txt")

cat("\nSample data creation complete!\n")
cat("You can now run 'Rscript R/unified_sdoh_pipeline.r --use-sample-data' to test the pipeline.\n")