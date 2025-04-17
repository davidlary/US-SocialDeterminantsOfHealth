#!/usr/bin/env Rscript

# Extended Variable Crosswalk Builder
# This script creates a comprehensive crosswalk between standard and extended variables
# to ensure consistent naming and categorization across the entire pipeline

library(tidyverse)
library(readxl)
library(here)

#' Build extended variable crosswalk
#'
#' Creates a comprehensive crosswalk of all variables in the extended pipeline,
#' including metadata like source, availability, and relationships to standard variables.
#'
#' @param output_dir Directory to store output files
#' @param force_update Whether to rebuild the crosswalk even if it exists
#' @param verbose Whether to print verbose output
#' @return TRUE if successful, FALSE otherwise
build_extended_crosswalk_v2 <- function(output_dir = "output",
                                      force_update = FALSE,
                                      verbose = FALSE) {
  # Helper function for clean output
  print_msg <- function(msg, detail_level = 1) {
    # If verbose is FALSE, only print messages with detail_level = 1
    # If verbose is TRUE, print all messages
    if (verbose || detail_level == 1) {
      # Check if being run interactively
      is_interactive_run <- !exists("is_sourced") || (is.logical(is_sourced) && !is_sourced)
      if (is_interactive_run) {
        message(msg)
      } else {
        cat(msg, "\n")
      }
    }
  }
  
  # Check if output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    print_msg(paste("Created output directory at:", output_dir))
  }
  
  # Define output file
  crosswalk_file <- file.path(output_dir, "variable_crosswalk_extended.csv")
  
  # Check if file exists and force_update is FALSE
  if (!force_update && file.exists(crosswalk_file)) {
    print_msg("Using existing variable crosswalk file.")
    return(TRUE)
  }
  
  print_msg("Building extended variable crosswalk...")
  
  # Try to find the original variable_crosswalk file (if it exists)
  original_crosswalk_file <- NULL
  potential_paths <- c(
    here("variable_crosswalk.csv"),
    here("variable_crosswalk_expanded.csv"),
    file.path(dirname(output_dir), "variable_crosswalk.csv"),
    file.path(dirname(output_dir), "variable_crosswalk_expanded.csv"),
    file.path(dirname(dirname(output_dir)), "variable_crosswalk.csv"),
    file.path(dirname(dirname(output_dir)), "variable_crosswalk_expanded.csv")
  )
  
  for (path in potential_paths) {
    if (file.exists(path)) {
      original_crosswalk_file <- path
      print_msg(paste("Found original crosswalk at:", path), 2)
      break
    }
  }
  
  # Define standard variable domains
  standard_domains <- c(
    "Demographic", 
    "Socioeconomic", 
    "Education", 
    "Housing", 
    "Transportation",
    "Healthcare", 
    "Health Outcomes", 
    "Health Behaviors", 
    "Environmental"
  )
  
  # Define extended domains 
  extended_domains <- c(
    "Food Environment & Access",
    "Built Environment",
    "Environmental Health",
    "Economic Factors",
    "Housing",
    "Healthcare Access",
    "Transportation",
    "Social Cohesion & Capital",
    "Crime & Safety",
    "Educational Resources & Quality"
  )
  
  # Mapping between extended and standard domains
  domain_mapping <- list(
    "Food Environment & Access" = "Health Behaviors",
    "Built Environment" = "Environmental",
    "Environmental Health" = "Environmental",
    "Economic Factors" = "Socioeconomic",
    "Housing" = "Housing",
    "Healthcare Access" = "Healthcare",
    "Transportation" = "Transportation",
    "Social Cohesion & Capital" = "Socioeconomic",
    "Crime & Safety" = "Socioeconomic",
    "Educational Resources & Quality" = "Education"
  )
  
  # Define variable data types
  variable_types <- c(
    "numeric_percent", # Percentage values (0-100)
    "numeric_rate", # Rate values (per 1,000, 10,000, 100,000, etc.)
    "numeric_ratio", # Ratio values (often around 1)
    "numeric_count", # Count values (integers, often large)
    "numeric_index", # Index values (often normalized, e.g., 0-10)
    "numeric_money", # Monetary values (dollars)
    "numeric_distance", # Distance values (miles, km, etc.)
    "numeric_density", # Density values (per square mile/km)
    "categorical", # Categorical values (string)
    "binary" # Binary values (0/1, TRUE/FALSE)
  )
  
  # Read the original crosswalk if it exists
  original_vars <- NULL
  if (!is.null(original_crosswalk_file)) {
    tryCatch({
      original_vars <- read_csv(original_crosswalk_file, show_col_types = FALSE)
      print_msg(paste("Loaded", nrow(original_vars), "variables from original crosswalk"), 2)
    }, error = function(e) {
      print_msg(paste("Error reading original crosswalk:", conditionMessage(e)))
      original_vars <- NULL
    })
  }
  
  # Define all new extended variables by domain
  
  # 1. Food Environment & Access
  food_vars <- tibble(
    variable_name = c(
      "grocery_stores_per_1000",
      "supercenters_per_1000",
      "convenience_stores_per_1000",
      "snap_authorized_stores_per_1000",
      "wic_authorized_stores_per_1000",
      "farmers_markets_per_1000",
      "fast_food_restaurants_per_1000",
      "full_service_restaurants_per_1000",
      "low_income_low_access_pct",
      "children_low_access_pct",
      "seniors_low_access_pct",
      "snap_benefits_redemption_per_capita",
      "food_insecurity_rate",
      "child_food_insecurity_rate",
      "food_insecurity_cost_per_person"
    ),
    domain = "Food Environment & Access",
    description = c(
      "Number of supermarkets and grocery stores per 1,000 population",
      "Number of supercenter and club stores per 1,000 population",
      "Number of convenience stores per 1,000 population",
      "SNAP-authorized retailers per 1,000 population",
      "WIC-authorized stores per 1,000 population",
      "Farmers markets per 1,000 population",
      "Fast food restaurants per 1,000 population",
      "Full-service restaurants per 1,000 population",
      "Percentage of population that is low income and has low access to a grocery store",
      "Percentage of children with low access to a grocery store",
      "Percentage of seniors with low access to a grocery store",
      "SNAP benefits redemption per capita",
      "Percentage of overall population experiencing food insecurity",
      "Percentage of children experiencing food insecurity",
      "Average cost per person to meet food needs"
    ),
    type = c(
      rep("numeric_rate", 8),
      rep("numeric_percent", 3),
      "numeric_money",
      rep("numeric_percent", 2),
      "numeric_money"
    ),
    source = c(
      rep("USDA Food Environment Atlas", 12),
      rep("Feeding America Map the Meal Gap", 3)
    ),
    min_year = c(
      rep(2010, 12),
      rep(2009, 3)
    ),
    max_year = c(
      rep(2022, 12),
      rep(2022, 3)
    ),
    units = c(
      rep("count/1000", 8),
      rep("percent", 3),
      "dollars",
      rep("percent", 2),
      "dollars"
    ),
    related_to_standard = c(
      rep("food_insecurity_pct", 12),
      "food_insecurity_pct",
      "food_insecurity_pct",
      "food_insecurity_pct"
    )
  )
  
  # 2. Built Environment
  built_env_vars <- tibble(
    variable_name = c(
      "walkability_index",
      "land_use_diversity",
      "street_intersection_density",
      "employment_access_index",
      "transit_service_density",
      "housing_density",
      "park_access_pct",
      "park_acres_per_1000",
      "park_spending_per_capita",
      "playgrounds_per_10000"
    ),
    domain = "Built Environment",
    description = c(
      "County-level walkability score",
      "Mix of land uses (entropy index)",
      "Number of intersections per square mile",
      "Access to employment centers",
      "Transit routes and stops per square mile",
      "Housing units per acre of developed land",
      "Percentage of residents living within 10-minute walk of a park",
      "Park acres per 1,000 residents",
      "Park system spending per resident",
      "Playgrounds per 10,000 residents"
    ),
    type = c(
      "numeric_index",
      "numeric_index",
      "numeric_density",
      "numeric_index",
      "numeric_density",
      "numeric_density",
      "numeric_percent",
      "numeric_rate",
      "numeric_money",
      "numeric_rate"
    ),
    source = c(
      rep("EPA Smart Location Database", 6),
      rep("Trust for Public Land ParkScore", 4)
    ),
    min_year = c(
      rep(2010, 6),
      rep(2012, 4)
    ),
    max_year = c(
      rep(2021, 6),
      rep(2022, 4)
    ),
    units = c(
      "index",
      "index",
      "count/sq mile",
      "index",
      "count/sq mile",
      "units/acre",
      "percent",
      "acres/1000",
      "dollars",
      "count/10000"
    ),
    related_to_standard = c(
      rep(NA_character_, 6),
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_
    )
  )
  
  # 3. Environmental Health
  env_health_vars <- tibble(
    variable_name = c(
      "air_quality_days_unhealthy",
      "pm25_annual_mean",
      "ozone_days_exceeding",
      "air_toxics_cancer_risk",
      "diesel_pm_concentration",
      "respiratory_hazard_index",
      "extreme_heat_days",
      "extreme_precipitation_events",
      "drought_severity_index",
      "public_water_violations",
      "lead_exposure_risk_index",
      "proximity_to_hazardous_waste",
      "proximity_to_npl_sites",
      "wastewater_discharge",
      "traffic_proximity",
      "lead_paint_indicator"
    ),
    domain = "Environmental Health",
    description = c(
      "Number of days with unhealthy air quality",
      "Annual mean PM2.5 concentration",
      "Days exceeding ozone standards",
      "Air toxics cancer risk",
      "Diesel particulate matter concentration",
      "Respiratory hazard index from air pollutants",
      "Annual number of extreme heat days",
      "Annual number of extreme precipitation events",
      "Average drought severity index",
      "Number of public water system violations",
      "Index of lead exposure risk",
      "Count of hazardous waste facilities within 5km",
      "Proximity to National Priorities List (Superfund) sites",
      "Toxicity-weighted concentrations in stream reach",
      "Count of vehicles at major roads within 500m",
      "Percentage of housing units built pre-1960"
    ),
    type = c(
      "numeric_count",
      "numeric_index",
      "numeric_count",
      "numeric_rate",
      "numeric_index",
      "numeric_index",
      "numeric_count",
      "numeric_count",
      "numeric_index",
      "numeric_count",
      "numeric_index",
      "numeric_count",
      "numeric_index",
      "numeric_index",
      "numeric_count",
      "numeric_percent"
    ),
    source = c(
      rep("EPA Air Quality System", 6),
      rep("CDC Environmental Public Health Tracking", 5),
      rep("EPA EJSCREEN", 5)
    ),
    min_year = c(
      rep(2000, 6),
      rep(2002, 5),
      rep(2016, 5)
    ),
    max_year = c(
      rep(2023, 6),
      rep(2022, 5),
      rep(2023, 5)
    ),
    units = c(
      "days",
      "μg/m³",
      "days",
      "per million",
      "μg/m³",
      "index",
      "days",
      "count",
      "index",
      "count",
      "index",
      "count",
      "index",
      "concentration",
      "count",
      "percent"
    ),
    related_to_standard = c(
      "air_pollution_pm25",
      "air_pollution_pm25",
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_
    )
  )
  
  # 4. Economic Factors
  economic_vars <- tibble(
    variable_name = c(
      "employment_volatility_index",
      "job_growth_rate",
      "income_inequality_ratio",
      "economic_typology",
      "persistent_poverty_county",
      "persistent_child_poverty_county",
      "economic_distress_index",
      "income_mobility_index",
      "absolute_upward_mobility",
      "mean_commute_distance",
      "job_density_index"
    ),
    domain = "Economic Factors",
    description = c(
      "Index of employment stability/volatility",
      "Annual job growth rate",
      "Ratio of income at 80th percentile to income at 20th percentile",
      "County economic typology",
      "Flag for counties with persistent poverty",
      "Flag for counties with persistent child poverty",
      "Composite index of economic distress",
      "Measure of intergenerational economic mobility",
      "Expected income rank for children from low-income families",
      "Average commute distance",
      "Number of jobs within typical commute distance"
    ),
    type = c(
      "numeric_index",
      "numeric_percent",
      "numeric_ratio",
      "categorical",
      "binary",
      "binary",
      "numeric_index",
      "numeric_index",
      "numeric_index",
      "numeric_distance",
      "numeric_index"
    ),
    source = c(
      "USDA Economic Research Service",
      "Bureau of Labor Statistics",
      "American Community Survey",
      "USDA Economic Research Service",
      "USDA Economic Research Service",
      "USDA Economic Research Service",
      "Appalachian Regional Commission",
      "Opportunity Insights",
      "Opportunity Insights",
      "Opportunity Insights",
      "Opportunity Insights"
    ),
    min_year = c(
      2000,
      2000,
      2010,
      2000,
      2000,
      2000,
      2000,
      2000,
      2000,
      2000,
      2000
    ),
    max_year = c(
      2023,
      2023,
      2023,
      2023,
      2023,
      2023,
      2023,
      2018,
      2018,
      2018,
      2018
    ),
    units = c(
      "index",
      "percent",
      "ratio",
      "category",
      "binary",
      "binary",
      "index",
      "index",
      "percentile",
      "miles",
      "index"
    ),
    related_to_standard = c(
      "unemployment_rate",
      "unemployment_rate",
      "gini_index",
      NA_character_,
      "poverty_rate",
      "poverty_rate",
      NA_character_,
      NA_character_,
      NA_character_,
      "mean_commute_time",
      NA_character_
    )
  )
  
  # 5. Housing
  housing_vars <- tibble(
    variable_name = c(
      "severely_cost_burdened_owners_pct",
      "severely_cost_burdened_renters_pct",
      "low_income_renters_affordable_units_ratio",
      "housing_problems_pct",
      "overcrowded_housing_pct",
      "eviction_rate",
      "eviction_filing_rate",
      "rent_burden_pct",
      "mortgage_denial_rate",
      "high_cost_loans_pct",
      "foreclosure_rate"
    ),
    domain = "Housing",
    description = c(
      "Percentage of owner households spending >50% of income on housing",
      "Percentage of renter households spending >50% of income on housing",
      "Ratio of affordable units to low-income renters",
      "Percentage of households with at least one housing problem",
      "Percentage of housing units with >1 person per room",
      "Number of evictions per 100 renter homes",
      "Number of eviction filings per 100 renter homes",
      "Percentage of income spent on rent (median)",
      "Percentage of mortgage applications denied",
      "Percentage of loans that are high-cost",
      "Foreclosures per 1,000 housing units"
    ),
    type = c(
      "numeric_percent",
      "numeric_percent",
      "numeric_ratio",
      "numeric_percent",
      "numeric_percent",
      "numeric_rate",
      "numeric_rate",
      "numeric_percent",
      "numeric_percent",
      "numeric_percent",
      "numeric_rate"
    ),
    source = c(
      rep("HUD CHAS", 5),
      rep("Eviction Lab", 3),
      rep("Federal Reserve HMDA", 3)
    ),
    min_year = c(
      rep(2006, 5),
      rep(2000, 3),
      rep(2007, 3)
    ),
    max_year = c(
      rep(2020, 5),
      rep(2018, 3),
      rep(2023, 3)
    ),
    units = c(
      "percent",
      "percent",
      "ratio",
      "percent",
      "percent",
      "rate",
      "rate",
      "percent",
      "percent",
      "percent",
      "rate"
    ),
    related_to_standard = c(
      "severe_housing_cost_burden",
      "severe_housing_cost_burden",
      NA_character_,
      "severe_housing_problems",
      "overcrowded_housing_pct",
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_
    )
  )
  
  # 6. Healthcare Access
  healthcare_vars <- tibble(
    variable_name = c(
      "primary_care_physicians_per_100k",
      "mental_health_providers_per_100k",
      "dentists_per_100k",
      "hospital_beds_per_1000",
      "fqhc_access_pct",
      "pharmacies_per_100k",
      "preventable_hospital_stays",
      "medicare_spending_per_beneficiary",
      "preventive_services_pct",
      "ambulatory_care_sensitive_conditions"
    ),
    domain = "Healthcare Access",
    description = c(
      "Primary care physicians per 100,000 population",
      "Mental health providers per 100,000 population",
      "Dentists per 100,000 population",
      "Hospital beds per 1,000 population",
      "Percentage of population with access to Federally Qualified Health Centers",
      "Pharmacies per 100,000 population",
      "Preventable hospital stays per 100,000 Medicare enrollees",
      "Medicare spending per beneficiary",
      "Percentage of Medicare beneficiaries receiving preventive services",
      "Rate of hospitalization for ambulatory care sensitive conditions"
    ),
    type = c(
      "numeric_rate",
      "numeric_rate",
      "numeric_rate",
      "numeric_rate",
      "numeric_percent",
      "numeric_rate",
      "numeric_rate",
      "numeric_money",
      "numeric_percent",
      "numeric_rate"
    ),
    source = c(
      rep("HRSA Area Health Resources Files", 7),
      rep("CMS Geographic Variation Public Use File", 3)
    ),
    min_year = c(
      rep(2000, 7),
      rep(2007, 3)
    ),
    max_year = c(
      rep(2023, 7),
      rep(2021, 3)
    ),
    units = c(
      "count/100k",
      "count/100k",
      "count/100k",
      "count/1000",
      "percent",
      "count/100k",
      "count/100k",
      "dollars",
      "percent",
      "rate"
    ),
    related_to_standard = c(
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_
    )
  )
  
  # 7. Transportation
  transportation_vars <- tibble(
    variable_name = c(
      "vehicle_miles_traveled_per_capita",
      "transportation_cost_burden_pct",
      "zero_vehicle_households_pct",
      "public_transit_trips_per_capita",
      "transit_connectivity_index",
      "transit_access_jobs",
      "transit_performance_index"
    ),
    domain = "Transportation",
    description = c(
      "Annual vehicle miles traveled per capita",
      "Transportation costs as percentage of household income",
      "Percentage of households with no vehicles",
      "Public transit trips per capita",
      "Measure of transit connectivity",
      "Number of jobs accessible by transit within 30 minutes",
      "Composite measure of transit performance"
    ),
    type = c(
      "numeric_distance",
      "numeric_percent",
      "numeric_percent",
      "numeric_rate",
      "numeric_index",
      "numeric_count",
      "numeric_index"
    ),
    source = c(
      rep("National Household Travel Survey", 2),
      "American Community Survey",
      "National Transit Database",
      rep("All Transit Database", 3)
    ),
    min_year = c(
      2001,
      2001,
      2009,
      2000,
      2012,
      2012,
      2012
    ),
    max_year = c(
      2017,
      2017,
      2023,
      2022,
      2022,
      2022,
      2022
    ),
    units = c(
      "miles",
      "percent",
      "percent",
      "count",
      "index",
      "count",
      "index"
    ),
    related_to_standard = c(
      NA_character_,
      NA_character_,
      "no_vehicle_households_pct",
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_
    )
  )
  
  # 8. Social Cohesion & Capital
  social_vars <- tibble(
    variable_name = c(
      "voter_turnout_rate",
      "voter_registration_rate",
      "political_competition_index",
      "social_association_rate",
      "religious_congregation_rate",
      "nonprofit_organizations_per_10k"
    ),
    domain = "Social Cohesion & Capital",
    description = c(
      "Voter turnout rate in general elections",
      "Voter registration as percentage of eligible population",
      "Index measuring political competition",
      "Social associations per 10,000 population",
      "Religious congregations per 10,000 population",
      "Nonprofit organizations per 10,000 population"
    ),
    type = c(
      "numeric_percent",
      "numeric_percent",
      "numeric_index",
      "numeric_rate",
      "numeric_rate",
      "numeric_rate"
    ),
    source = c(
      rep("MIT Election Data and Science Lab", 3),
      rep("County Health Rankings", 3)
    ),
    min_year = c(
      rep(2000, 3),
      rep(2014, 3)
    ),
    max_year = c(
      rep(2022, 3),
      rep(2023, 3)
    ),
    units = c(
      "percent",
      "percent",
      "index",
      "count/10k",
      "count/10k",
      "count/10k"
    ),
    related_to_standard = c(
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_
    )
  )
  
  # 9. Crime & Safety
  crime_vars <- tibble(
    variable_name = c(
      "violent_crime_rate",
      "property_crime_rate",
      "homicide_rate",
      "jail_incarceration_rate",
      "pretrial_detention_rate"
    ),
    domain = "Crime & Safety",
    description = c(
      "Violent crimes per 100,000 population",
      "Property crimes per 100,000 population",
      "Homicides per 100,000 population",
      "County jail inmates per 100,000 population",
      "Pretrial detainees per 100,000 population"
    ),
    type = c(
      "numeric_rate",
      "numeric_rate",
      "numeric_rate",
      "numeric_rate",
      "numeric_rate"
    ),
    source = c(
      rep("FBI Uniform Crime Reports", 3),
      rep("Bureau of Justice Statistics", 2)
    ),
    min_year = c(
      rep(2000, 3),
      rep(2000, 2)
    ),
    max_year = c(
      rep(2021, 3),
      rep(2020, 2)
    ),
    units = c(
      "count/100k",
      "count/100k",
      "count/100k",
      "count/100k",
      "count/100k"
    ),
    related_to_standard = c(
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_
    )
  )
  
  # 10. Educational Resources & Quality
  education_vars <- tibble(
    variable_name = c(
      "student_teacher_ratio",
      "per_pupil_expenditure",
      "high_school_graduation_rate",
      "preschool_enrollment_rate",
      "school_funding_equity",
      "reading_achievement_gap",
      "math_achievement_gap",
      "educational_opportunity_index"
    ),
    domain = "Educational Resources & Quality",
    description = c(
      "Student-to-teacher ratio in public schools",
      "Per-pupil expenditure in public schools",
      "Four-year high school graduation rate",
      "Percentage of 3-4 year-olds enrolled in preschool",
      "Ratio of funding in high-poverty vs. low-poverty districts",
      "Achievement gap in reading scores by race/ethnicity",
      "Achievement gap in math scores by race/ethnicity",
      "Measure of educational opportunity"
    ),
    type = c(
      "numeric_ratio",
      "numeric_money",
      "numeric_percent",
      "numeric_percent",
      "numeric_ratio",
      "numeric_index",
      "numeric_index",
      "numeric_index"
    ),
    source = c(
      rep("National Center for Education Statistics", 5),
      rep("Stanford Education Data Archive", 3)
    ),
    min_year = c(
      rep(2000, 5),
      rep(2009, 3)
    ),
    max_year = c(
      rep(2022, 5),
      rep(2018, 3)
    ),
    units = c(
      "ratio",
      "dollars",
      "percent",
      "percent",
      "ratio",
      "z-score",
      "z-score",
      "index"
    ),
    related_to_standard = c(
      NA_character_,
      NA_character_,
      "high_school_graduation_rate",
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_
    )
  )
  
  # Combine all extended variables
  all_extended_vars <- bind_rows(
    food_vars,
    built_env_vars,
    env_health_vars,
    economic_vars,
    housing_vars,
    healthcare_vars,
    transportation_vars,
    social_vars,
    crime_vars,
    education_vars
  )
  
  # Map extended domains to standard domains
  all_extended_vars <- all_extended_vars %>%
    mutate(standard_domain = domain_mapping[domain])
  
  # Add additional metadata for crosswalk
  all_extended_vars <- all_extended_vars %>%
    mutate(
      extended_only = TRUE,
      notes = NA_character_,
      api_source = NA_character_,
      api_variable = NA_character_,
      data_quality_flag_required = TRUE
    )
  
  # Integrate with original crosswalk if it exists
  if (!is.null(original_vars)) {
    # First, make sure columns match between the two dataframes
    missing_cols <- setdiff(names(all_extended_vars), names(original_vars))
    for (col in missing_cols) {
      original_vars[[col]] <- NA
    }
    
    missing_cols <- setdiff(names(original_vars), names(all_extended_vars))
    for (col in missing_cols) {
      all_extended_vars[[col]] <- NA
    }
    
    # Mark original variables as not extended_only
    original_vars$extended_only <- FALSE
    
    # Combine
    combined_vars <- bind_rows(
      original_vars,
      all_extended_vars
    )
    
    # Order columns logically
    col_order <- c(
      "variable_name", 
      "domain", 
      "standard_domain",
      "description", 
      "type", 
      "units", 
      "source", 
      "min_year", 
      "max_year", 
      "extended_only",
      "related_to_standard",
      "api_source", 
      "api_variable", 
      "data_quality_flag_required",
      "notes"
    )
    
    # Keep only columns in col_order that exist in the dataframe
    col_order <- intersect(col_order, names(combined_vars))
    
    # Add any remaining columns
    col_order <- c(col_order, setdiff(names(combined_vars), col_order))
    
    # Reorder
    combined_vars <- combined_vars %>%
      select(all_of(col_order))
    
  } else {
    # If no original crosswalk, just use the extended variables
    combined_vars <- all_extended_vars
  }
  
  # Write to CSV
  write_csv(combined_vars, crosswalk_file)
  print_msg(paste("Wrote", nrow(combined_vars), "variables to crosswalk file:", crosswalk_file))
  
  # Create simpler view for data dictionary
  data_dict <- combined_vars %>%
    select(variable_name, description, domain, type, source, min_year, max_year, units) %>%
    arrange(domain, variable_name)
  
  # Write data dictionary
  dict_file <- file.path(output_dir, "extended_data_dictionary.csv")
  write_csv(data_dict, dict_file)
  print_msg(paste("Wrote data dictionary to:", dict_file))
  
  return(TRUE)
}

# This lets the function be used when the script is sourced
is_sourced <- function() {
  # Check if the calling environment is the global environment
  # If it's not, the function is being sourced
  parent_env <- parent.frame()
  return(!identical(parent_env, .GlobalEnv))
}

# If the script is run directly, test the function
if (!is_sourced()) {
  cat("Testing extended variable crosswalk builder...\n")
  
  # Test the function
  result <- build_extended_crosswalk_v2(
    output_dir = "output",
    force_update = TRUE,
    verbose = TRUE
  )
  
  cat("Test completed with result:", result, "\n")
}
