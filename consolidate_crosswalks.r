#!/usr/bin/env Rscript

# consolidate_crosswalks.r
# This script provides a unified crosswalk builder and consolidator for the SDOH project
# It merges all crosswalk-related functionality into a single, definitive implementation

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(here)

# Define is_sourced function if it doesn't exist
if (!exists("is_sourced")) {
  is_sourced <- function() {
    # Check if the calling environment is the global environment
    # If it's not, the function is being sourced
    parent_env <- parent.frame()
    return(!identical(parent_env, .GlobalEnv))
  }
}

#' Build a complete and consolidated SDOH variable crosswalk
#'
#' This function creates a comprehensive crosswalk of all variables including
#' standard demographics, extended variables and domain-specific variables.
#' It also ensures consistency across all outputs and updates documentation.
#'
#' @param output_dir Directory to store output files
#' @param force_update Whether to rebuild the crosswalk even if it exists
#' @param verbose Whether to print verbose output
#' @param update_documentation Whether to update README and other documentation
#' @return tibble containing the consolidated crosswalk
build_unified_crosswalk <- function(output_dir = "output",
                                   force_update = FALSE,
                                   verbose = TRUE,
                                   update_documentation = TRUE) {
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

  print_msg("Building unified SDOH variable crosswalk...")
  
  # Check if output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    print_msg(paste("Created output directory at:", output_dir))
  }
  
  # Define output files
  crosswalk_file <- file.path(output_dir, "variable_crosswalk_consolidated.csv")
  dict_file <- file.path(output_dir, "extended_data_dictionary.csv")
  
  # Check if file exists and force_update is FALSE
  if (!force_update && file.exists(crosswalk_file)) {
    print_msg("Using existing consolidated crosswalk file.")
    return(read_csv(crosswalk_file, show_col_types = FALSE))
  }
  
  # Initialize all variables tibble
  all_variables <- tibble()
  
  # PART 1: DEFINE BASIC DEMOGRAPHIC VARIABLES
  # This replaces functionality in build_crosswalk_final.r
  print_msg("Adding basic demographic variables...", 2)
  
  demographic_vars <- tibble::tribble(
    ~variable_name, ~domain, ~description, ~type, ~units,
    "total_population", "Demographic", "Total population", "numeric_count", "people",
    "median_age", "Demographic", "Median age of population", "numeric_years", "years",
    "male_population", "Demographic", "Male population", "numeric_count", "people",
    "female_population", "Demographic", "Female population", "numeric_count", "people",
    "population_under_18", "Demographic", "Population under 18 years old", "numeric_count", "people",
    "population_over_65", "Demographic", "Population 65 years and older", "numeric_count", "people",
    "white_population", "Race/Ethnicity", "White alone population", "numeric_count", "people",
    "black_population", "Race/Ethnicity", "Black or African American alone population", "numeric_count", "people",
    "hispanic_population", "Race/Ethnicity", "Hispanic or Latino population (any race)", "numeric_count", "people",
    "asian_population", "Race/Ethnicity", "Asian alone population", "numeric_count", "people",
    "aian_population", "Race/Ethnicity", "American Indian and Alaska Native alone population", "numeric_count", "people",
    "nhpi_population", "Race/Ethnicity", "Native Hawaiian and Other Pacific Islander alone population", "numeric_count", "people",
    "multiracial_population", "Race/Ethnicity", "Two or more races population", "numeric_count", "people",
    "other_race_population", "Race/Ethnicity", "Some other race alone population", "numeric_count", "people",
    "white_pct", "Race/Ethnicity", "White alone percentage", "numeric_percent", "percent",
    "black_pct", "Race/Ethnicity", "Black or African American alone percentage", "numeric_percent", "percent",
    "hispanic_pct", "Race/Ethnicity", "Hispanic or Latino percentage (any race)", "numeric_percent", "percent",
    "asian_pct", "Race/Ethnicity", "Asian alone percentage", "numeric_percent", "percent",
    "aian_pct", "Race/Ethnicity", "American Indian and Alaska Native alone percentage", "numeric_percent", "percent",
    "nhpi_pct", "Race/Ethnicity", "Native Hawaiian and Other Pacific Islander alone percentage", "numeric_percent", "percent",
    "multiracial_pct", "Race/Ethnicity", "Two or more races percentage", "numeric_percent", "percent",
    "other_race_pct", "Race/Ethnicity", "Some other race alone percentage", "numeric_percent", "percent"
  )
  
  # Add Census variables and available years
  demographic_vars <- demographic_vars %>%
    mutate(
      source = "US Census Bureau",
      min_year = 2000,
      max_year = 2023,
      extended_only = FALSE,
      data_quality_flag_required = FALSE
    )
  
  # Add to all variables
  all_variables <- bind_rows(all_variables, demographic_vars)
  
  # PART 2: DEFINE SOCIOECONOMIC VARIABLES
  print_msg("Adding socioeconomic variables...", 2)
  
  socioeconomic_vars <- tibble::tribble(
    ~variable_name, ~domain, ~description, ~type, ~units,
    "median_household_income", "Economic", "Median household income", "numeric_money", "dollars",
    "mean_household_income", "Economic", "Mean household income", "numeric_money", "dollars",
    "per_capita_income", "Economic", "Per capita income", "numeric_money", "dollars",
    "poverty_rate", "Economic", "Percentage of population below poverty level", "numeric_percent", "percent",
    "child_poverty_rate", "Economic", "Percentage of children below poverty level", "numeric_percent", "percent",
    "senior_poverty_rate", "Economic", "Percentage of seniors (65+) below poverty level", "numeric_percent", "percent",
    "unemployment_rate", "Economic", "Unemployment rate", "numeric_percent", "percent",
    "labor_force_participation", "Economic", "Labor force participation rate", "numeric_percent", "percent",
    "gini_index", "Economic", "Gini index of income inequality", "numeric_index", "index",
    "median_earnings", "Economic", "Median earnings for workers", "numeric_money", "dollars",
    "median_male_earnings", "Economic", "Median earnings for male workers", "numeric_money", "dollars",
    "median_female_earnings", "Economic", "Median earnings for female workers", "numeric_money", "dollars",
    "income_less_10k", "Economic", "Households with income less than $10,000", "numeric_percent", "percent",
    "income_10k_15k", "Economic", "Households with income $10,000 to $14,999", "numeric_percent", "percent",
    "income_15k_25k", "Economic", "Households with income $15,000 to $24,999", "numeric_percent", "percent",
    "income_25k_35k", "Economic", "Households with income $25,000 to $34,999", "numeric_percent", "percent",
    "income_35k_50k", "Economic", "Households with income $35,000 to $49,999", "numeric_percent", "percent",
    "income_50k_75k", "Economic", "Households with income $50,000 to $74,999", "numeric_percent", "percent",
    "income_75k_100k", "Economic", "Households with income $75,000 to $99,999", "numeric_percent", "percent",
    "income_100k_150k", "Economic", "Households with income $100,000 to $149,999", "numeric_percent", "percent",
    "income_150k_200k", "Economic", "Households with income $150,000 to $199,999", "numeric_percent", "percent",
    "income_200k_plus", "Economic", "Households with income $200,000 or more", "numeric_percent", "percent",
    "snap_benefits", "Economic", "Households receiving SNAP/Food Stamps", "numeric_percent", "percent"
  )
  
  # Add ACS sources and years
  socioeconomic_vars <- socioeconomic_vars %>%
    mutate(
      source = "American Community Survey",
      min_year = 2010,
      max_year = 2023,
      extended_only = FALSE,
      data_quality_flag_required = FALSE
    )
  
  # Add to all variables
  all_variables <- bind_rows(all_variables, socioeconomic_vars)
  
  # PART 3: DEFINE EDUCATION VARIABLES
  print_msg("Adding education variables...", 2)
  
  education_vars <- tibble::tribble(
    ~variable_name, ~domain, ~description, ~type, ~units,
    "less_than_high_school", "Education", "Population with less than high school education", "numeric_count", "people",
    "high_school_only", "Education", "Population with high school diploma only", "numeric_count", "people",
    "some_college", "Education", "Population with some college or associate's degree", "numeric_count", "people",
    "bachelors_or_higher", "Education", "Population with bachelor's degree or higher", "numeric_count", "people",
    "graduate_degree", "Education", "Population with graduate or professional degree", "numeric_count", "people",
    "less_than_high_school_pct", "Education", "Percentage with less than high school education", "numeric_percent", "percent",
    "high_school_only_pct", "Education", "Percentage with high school diploma only", "numeric_percent", "percent",
    "some_college_pct", "Education", "Percentage with some college or associate's degree", "numeric_percent", "percent",
    "bachelors_or_higher_pct", "Education", "Percentage with bachelor's degree or higher", "numeric_percent", "percent",
    "graduate_degree_pct", "Education", "Percentage with graduate or professional degree", "numeric_percent", "percent",
    "high_school_graduation_rate", "Education", "High school graduation rate", "numeric_percent", "percent",
    "enrolled_in_college", "Education", "Population enrolled in college or graduate school", "numeric_count", "people",
    "enrolled_in_college_pct", "Education", "Percentage enrolled in college or graduate school", "numeric_percent", "percent"
  )
  
  # Add ACS sources and years
  education_vars <- education_vars %>%
    mutate(
      source = "American Community Survey",
      min_year = 2010,
      max_year = 2023,
      extended_only = FALSE,
      data_quality_flag_required = FALSE
    )
  
  # Add to all variables
  all_variables <- bind_rows(all_variables, education_vars)
  
  # PART 4: DEFINE HOUSING VARIABLES
  print_msg("Adding housing variables...", 2)
  
  housing_vars <- tibble::tribble(
    ~variable_name, ~domain, ~description, ~type, ~units,
    "total_housing_units", "Housing", "Total housing units", "numeric_count", "units",
    "occupied_housing_units", "Housing", "Occupied housing units", "numeric_count", "units",
    "vacant_housing_units", "Housing", "Vacant housing units", "numeric_count", "units",
    "homeownership_rate", "Housing", "Homeownership rate", "numeric_percent", "percent",
    "rental_rate", "Housing", "Rental rate", "numeric_percent", "percent",
    "median_home_value", "Housing", "Median home value", "numeric_money", "dollars",
    "median_rent", "Housing", "Median gross rent", "numeric_money", "dollars",
    "rent_burden_pct", "Housing", "Percentage of household income spent on rent", "numeric_percent", "percent",
    "severe_housing_cost_burden", "Housing", "Households with severe housing cost burden (>50% of income)", "numeric_percent", "percent",
    "severe_housing_problems", "Housing", "Households with at least one severe housing problem", "numeric_percent", "percent",
    "overcrowded_housing_pct", "Housing", "Percentage of housing units with more than 1 person per room", "numeric_percent", "percent",
    "housing_without_plumbing", "Housing", "Housing units lacking complete plumbing facilities", "numeric_percent", "percent",
    "housing_without_kitchen", "Housing", "Housing units lacking complete kitchen facilities", "numeric_percent", "percent",
    "housing_built_before_1940", "Housing", "Housing units built before 1940", "numeric_percent", "percent",
    "housing_built_after_2010", "Housing", "Housing units built 2010 or later", "numeric_percent", "percent"
  )
  
  # Add ACS sources and years
  housing_vars <- housing_vars %>%
    mutate(
      source = "American Community Survey / HUD CHAS",
      min_year = 2010,
      max_year = 2023,
      extended_only = FALSE,
      data_quality_flag_required = FALSE
    )
  
  # Add to all variables
  all_variables <- bind_rows(all_variables, housing_vars)
  
  # PART 5: DEFINE HEALTHCARE VARIABLES
  print_msg("Adding healthcare variables...", 2)
  
  healthcare_vars <- tibble::tribble(
    ~variable_name, ~domain, ~description, ~type, ~units,
    "uninsured_pct", "Healthcare", "Percentage of population without health insurance", "numeric_percent", "percent",
    "medicare_pct", "Healthcare", "Percentage of population with Medicare coverage", "numeric_percent", "percent",
    "medicaid_pct", "Healthcare", "Percentage of population with Medicaid coverage", "numeric_percent", "percent",
    "primary_care_physicians_per_100k", "Healthcare", "Primary care physicians per 100,000 population", "numeric_rate", "count/100k",
    "dental_visit_pct", "Healthcare", "Percentage of adults who visited a dentist in the past year", "numeric_percent", "percent",
    "annual_checkup_pct", "Healthcare", "Percentage of adults who had an annual checkup", "numeric_percent", "percent",
    "no_health_insurance_pct", "Healthcare", "Percentage of adults without any health insurance", "numeric_percent", "percent"
  )
  
  # Add BRFSS/PLACES data sources and years
  healthcare_vars <- healthcare_vars %>%
    mutate(
      source = "CDC PLACES / SAHIE",
      min_year = 2010,
      max_year = 2022,
      extended_only = FALSE,
      data_quality_flag_required = FALSE
    )
  
  # Add to all variables
  all_variables <- bind_rows(all_variables, healthcare_vars)
  
  # PART 6: DEFINE HEALTH OUTCOMES VARIABLES
  print_msg("Adding health outcome variables...", 2)
  
  health_vars <- tibble::tribble(
    ~variable_name, ~domain, ~description, ~type, ~units,
    "life_expectancy", "Health Outcomes", "Life expectancy at birth", "numeric_years", "years",
    "infant_mortality_rate", "Health Outcomes", "Infant mortality rate per 1,000 live births", "numeric_rate", "count/1000",
    "poor_physical_health_pct", "Health Outcomes", "Percentage of adults reporting poor physical health", "numeric_percent", "percent",
    "poor_mental_health_pct", "Health Outcomes", "Percentage of adults reporting poor mental health", "numeric_percent", "percent",
    "obesity_pct", "Health Outcomes", "Percentage of adults with obesity (BMI ≥ 30)", "numeric_percent", "percent", 
    "diabetes_pct", "Health Outcomes", "Percentage of adults with diagnosed diabetes", "numeric_percent", "percent",
    "high_blood_pressure_pct", "Health Outcomes", "Percentage of adults with high blood pressure", "numeric_percent", "percent",
    "high_cholesterol_pct", "Health Outcomes", "Percentage of adults with high cholesterol", "numeric_percent", "percent",
    "heart_disease_pct", "Health Outcomes", "Percentage of adults with heart disease", "numeric_percent", "percent",
    "stroke_pct", "Health Outcomes", "Percentage of adults who have had a stroke", "numeric_percent", "percent",
    "asthma_pct", "Health Outcomes", "Percentage of adults with asthma", "numeric_percent", "percent",
    "arthritis_pct", "Health Outcomes", "Percentage of adults with arthritis", "numeric_percent", "percent",
    "cancer_pct", "Health Outcomes", "Percentage of adults with cancer (excluding skin cancer)", "numeric_percent", "percent",
    "copd_pct", "Health Outcomes", "Percentage of adults with COPD", "numeric_percent", "percent",
    "kidney_disease_pct", "Health Outcomes", "Percentage of adults with kidney disease", "numeric_percent", "percent",
    "depression_pct", "Health Outcomes", "Percentage of adults with diagnosed depression", "numeric_percent", "percent",
    "coronary_heart_disease_pct", "Health Outcomes", "Percentage of adults with coronary heart disease", "numeric_percent", "percent"
  )
  
  # Add CDC data sources and years
  health_vars <- health_vars %>%
    mutate(
      source = "CDC PLACES / CDC WONDER",
      min_year = 2010,
      max_year = 2022,
      extended_only = FALSE,
      data_quality_flag_required = FALSE
    )
  
  # Add to all variables
  all_variables <- bind_rows(all_variables, health_vars)
  
  # PART 7: DEFINE HEALTH BEHAVIOR VARIABLES
  print_msg("Adding health behavior variables...", 2)
  
  health_behavior_vars <- tibble::tribble(
    ~variable_name, ~domain, ~description, ~type, ~units,
    "smoking_pct", "Health Behaviors", "Percentage of adults who currently smoke", "numeric_percent", "percent",
    "binge_drinking_pct", "Health Behaviors", "Percentage of adults reporting binge drinking", "numeric_percent", "percent",
    "physical_inactivity_pct", "Health Behaviors", "Percentage of adults reporting no leisure-time physical activity", "numeric_percent", "percent",
    "insufficient_sleep_pct", "Health Behaviors", "Percentage of adults reporting insufficient sleep", "numeric_percent", "percent",
    "food_insecurity_pct", "Health Behaviors", "Percentage of population with food insecurity", "numeric_percent", "percent"
  )
  
  # Add CDC data sources and years
  health_behavior_vars <- health_behavior_vars %>%
    mutate(
      source = "CDC PLACES / Feeding America",
      min_year = 2010,
      max_year = 2022,
      extended_only = FALSE,
      data_quality_flag_required = FALSE
    )
  
  # Add to all variables
  all_variables <- bind_rows(all_variables, health_behavior_vars)
  
  # PART 8: DEFINE ENVIRONMENTAL VARIABLES
  print_msg("Adding environmental variables...", 2)
  
  environmental_vars <- tibble::tribble(
    ~variable_name, ~domain, ~description, ~type, ~units,
    "air_pollution_pm25", "Environmental", "Fine particulate matter (PM2.5) concentration", "numeric_index", "µg/m³",
    "population_density", "Environmental", "Population per square mile", "numeric_density", "people/sq mile"
  )
  
  # Add environmental data sources and years
  environmental_vars <- environmental_vars %>%
    mutate(
      source = "EPA / Census Bureau",
      min_year = 2000,
      max_year = 2023,
      extended_only = FALSE,
      data_quality_flag_required = FALSE
    )
  
  # Add to all variables
  all_variables <- bind_rows(all_variables, environmental_vars)
  
  # PART 9: DEFINE TRANSPORTATION VARIABLES
  print_msg("Adding transportation variables...", 2)
  
  transportation_vars <- tibble::tribble(
    ~variable_name, ~domain, ~description, ~type, ~units,
    "commute_car_alone", "Transportation", "Workers commuting by driving alone", "numeric_percent", "percent",
    "commute_carpool", "Transportation", "Workers commuting by carpooling", "numeric_percent", "percent",
    "commute_public_transit", "Transportation", "Workers commuting by public transportation", "numeric_percent", "percent",
    "commute_walk", "Transportation", "Workers commuting by walking", "numeric_percent", "percent",
    "commute_bicycle", "Transportation", "Workers commuting by bicycle", "numeric_percent", "percent",
    "commute_other", "Transportation", "Workers commuting by other means", "numeric_percent", "percent",
    "commute_work_at_home", "Transportation", "Workers working at home", "numeric_percent", "percent",
    "mean_commute_time", "Transportation", "Mean commute time (minutes)", "numeric_time", "minutes",
    "commute_long_pct", "Transportation", "Percentage of workers with commute >30 minutes", "numeric_percent", "percent",
    "no_vehicle_households_pct", "Transportation", "Percentage of households with no vehicle available", "numeric_percent", "percent"
  )
  
  # Add ACS source and years
  transportation_vars <- transportation_vars %>%
    mutate(
      source = "American Community Survey",
      min_year = 2010,
      max_year = 2023,
      extended_only = FALSE,
      data_quality_flag_required = FALSE
    )
  
  # Add to all variables
  all_variables <- bind_rows(all_variables, transportation_vars)
  
  # PART 10: DEFINE SOCIAL COHESION VARIABLES
  print_msg("Adding social cohesion variables...", 2)
  
  social_vars <- tibble::tribble(
    ~variable_name, ~domain, ~description, ~type, ~units,
    "single_parent_households_pct", "Social", "Percentage of single-parent households", "numeric_percent", "percent",
    "disconnected_youth", "Social", "Percentage of teens and young adults (16-24) neither working nor in school", "numeric_percent", "percent",
    "households_with_computer", "Social", "Percentage of households with a computer", "numeric_percent", "percent",
    "households_with_internet", "Social", "Percentage of households with broadband internet subscription", "numeric_percent", "percent",
    "civilian_veterans", "Social", "Percentage of civilian population who are veterans", "numeric_percent", "percent"
  )
  
  # Add ACS source and years
  social_vars <- social_vars %>%
    mutate(
      source = "American Community Survey",
      min_year = 2010,
      max_year = 2023,
      extended_only = FALSE,
      data_quality_flag_required = FALSE
    )
  
  # Add to all variables
  all_variables <- bind_rows(all_variables, social_vars)
  
  # PART 11: ADD IHME LIFE EXPECTANCY VARIABLES
  print_msg("Adding IHME life expectancy variables...", 2)
  
  ihme_vars <- tibble::tribble(
    ~variable_name, ~domain, ~description, ~type, ~units,
    "life_expectancy", "Health Outcomes", "Life expectancy at birth", "numeric_years", "years",
    "life_expectancy_male", "Health Outcomes", "Male life expectancy at birth", "numeric_years", "years",
    "life_expectancy_female", "Health Outcomes", "Female life expectancy at birth", "numeric_years", "years",
    "life_expectancy_hispanic", "Health Outcomes", "Hispanic life expectancy at birth", "numeric_years", "years",
    "life_expectancy_nhw", "Health Outcomes", "Non-Hispanic White life expectancy at birth", "numeric_years", "years",
    "life_expectancy_nhb", "Health Outcomes", "Non-Hispanic Black life expectancy at birth", "numeric_years", "years",
    "life_expectancy_nhaian", "Health Outcomes", "Non-Hispanic AIAN life expectancy at birth", "numeric_years", "years",
    "life_expectancy_nhasian", "Health Outcomes", "Non-Hispanic Asian life expectancy at birth", "numeric_years", "years",
    "life_expectancy_nhpi", "Health Outcomes", "Non-Hispanic Pacific Islander life expectancy at birth", "numeric_years", "years",
    "life_expectancy_multirace", "Health Outcomes", "Non-Hispanic multiracial life expectancy at birth", "numeric_years", "years",
    "life_expectancy_male_hispanic", "Health Outcomes", "Hispanic male life expectancy at birth", "numeric_years", "years",
    "life_expectancy_male_nhw", "Health Outcomes", "Non-Hispanic White male life expectancy at birth", "numeric_years", "years",
    "life_expectancy_male_nhb", "Health Outcomes", "Non-Hispanic Black male life expectancy at birth", "numeric_years", "years",
    "life_expectancy_male_nhaian", "Health Outcomes", "Non-Hispanic AIAN male life expectancy at birth", "numeric_years", "years",
    "life_expectancy_male_nhasian", "Health Outcomes", "Non-Hispanic Asian male life expectancy at birth", "numeric_years", "years",
    "life_expectancy_male_nhpi", "Health Outcomes", "Non-Hispanic Pacific Islander male life expectancy at birth", "numeric_years", "years",
    "life_expectancy_male_multirace", "Health Outcomes", "Non-Hispanic multiracial male life expectancy at birth", "numeric_years", "years",
    "life_expectancy_female_hispanic", "Health Outcomes", "Hispanic female life expectancy at birth", "numeric_years", "years",
    "life_expectancy_female_nhw", "Health Outcomes", "Non-Hispanic White female life expectancy at birth", "numeric_years", "years",
    "life_expectancy_female_nhb", "Health Outcomes", "Non-Hispanic Black female life expectancy at birth", "numeric_years", "years",
    "life_expectancy_female_nhaian", "Health Outcomes", "Non-Hispanic AIAN female life expectancy at birth", "numeric_years", "years",
    "life_expectancy_female_nhasian", "Health Outcomes", "Non-Hispanic Asian female life expectancy at birth", "numeric_years", "years",
    "life_expectancy_female_nhpi", "Health Outcomes", "Non-Hispanic Pacific Islander female life expectancy at birth", "numeric_years", "years",
    "life_expectancy_female_multirace", "Health Outcomes", "Non-Hispanic multiracial female life expectancy at birth", "numeric_years", "years",
    "le_lower_ci", "Health Outcomes", "Lower confidence interval for life expectancy", "numeric_years", "years",
    "le_upper_ci", "Health Outcomes", "Upper confidence interval for life expectancy", "numeric_years", "years",
    "le_male_lower_ci", "Health Outcomes", "Lower confidence interval for male life expectancy", "numeric_years", "years",
    "le_male_upper_ci", "Health Outcomes", "Upper confidence interval for male life expectancy", "numeric_years", "years",
    "le_female_lower_ci", "Health Outcomes", "Lower confidence interval for female life expectancy", "numeric_years", "years",
    "le_female_upper_ci", "Health Outcomes", "Upper confidence interval for female life expectancy", "numeric_years", "years"
  )
  
  # Add IHME source and years
  ihme_vars <- ihme_vars %>%
    mutate(
      source = "IHME (Institute for Health Metrics and Evaluation)",
      min_year = 2000,
      max_year = 2019,
      extended_only = TRUE,
      data_quality_flag_required = TRUE,
      notes = ifelse(variable_name == "life_expectancy", "Overall life expectancy for all races and genders combined", NA)
    )
  
  # Add to all variables
  all_variables <- bind_rows(all_variables, ihme_vars)
  
  # PART 12: ADD TRAFFIC SAFETY VARIABLES
  print_msg("Adding traffic safety variables...", 2)
  
  traffic_vars <- tibble::tribble(
    ~variable_name, ~domain, ~description, ~type, ~units,
    "traffic_fatalities", "Traffic Safety", "Total traffic fatalities", "numeric_count", "count",
    "traffic_fatality_rate", "Traffic Safety", "Traffic fatalities per 100,000 population", "numeric_rate", "count/100k",
    "pedestrian_fatalities", "Traffic Safety", "Pedestrian traffic fatalities", "numeric_count", "count",
    "pedestrian_fatality_rate", "Traffic Safety", "Pedestrian fatalities per 100,000 population", "numeric_rate", "count/100k",
    "bicycle_fatalities", "Traffic Safety", "Bicycle traffic fatalities", "numeric_count", "count", 
    "bicycle_fatality_rate", "Traffic Safety", "Bicycle fatalities per 100,000 population", "numeric_rate", "count/100k",
    "motorcycle_fatalities", "Traffic Safety", "Motorcycle traffic fatalities", "numeric_count", "count",
    "motorcycle_fatality_rate", "Traffic Safety", "Motorcycle fatalities per 100,000 population", "numeric_rate", "count/100k",
    "alcohol_impaired_fatalities", "Traffic Safety", "Alcohol-impaired driving fatalities", "numeric_count", "count",
    "alcohol_impaired_fatality_rate", "Traffic Safety", "Alcohol-impaired fatalities per 100,000 population", "numeric_rate", "count/100k",
    "speeding_related_fatalities", "Traffic Safety", "Speeding-related traffic fatalities", "numeric_count", "count",
    "speeding_related_fatality_rate", "Traffic Safety", "Speeding-related fatalities per 100,000 population", "numeric_rate", "count/100k"
  )
  
  # Add FARS source and years
  traffic_vars <- traffic_vars %>%
    mutate(
      source = "NHTSA FARS (Fatality Analysis Reporting System)",
      min_year = 1975,
      max_year = 2021,
      extended_only = TRUE,
      data_quality_flag_required = TRUE
    )
  
  # Add to all variables
  all_variables <- bind_rows(all_variables, traffic_vars)
  
  # PART 13: ADD EXTENDED VARIABLES 
  # This replaces functionality from build_extended_crosswalk_v2.r
  # Food Environment variables, Built Environment variables, etc.
  print_msg("Adding extended variables from each domain...", 2)
  
  # There are ~100 extended variables - to keep this file manageable,
  # we'll load from the existing extended crosswalk if available
  extended_file <- "output/variable_crosswalk_extended.csv"
  extended_vars <- NULL
  
  if (file.exists(extended_file)) {
    print_msg("Loading extended variables from existing crosswalk...", 2)
    extended_vars <- read_csv(extended_file, show_col_types = FALSE)
    
    # Filter extended vars to exclude any already in our all_variables
    extended_vars <- extended_vars %>%
      filter(!variable_name %in% all_variables$variable_name)
    
    # Add these to our all_variables
    if (nrow(extended_vars) > 0) {
      all_variables <- bind_rows(all_variables, extended_vars)
      print_msg(paste("Added", nrow(extended_vars), "extended variables from existing crosswalk"), 2)
    }
  }
  
  # PART 14: PROCESS AND FINALIZE THE CROSSWALK
  print_msg("Finalizing consolidated crosswalk...", 2)
  
  # Remove any duplicate variables by variable_name (keeping the first occurrence)
  all_variables <- all_variables %>%
    distinct(variable_name, .keep_all = TRUE)
  
  # Define standard and extended domains
  standard_domains <- c(
    "Demographic", 
    "Race/Ethnicity",
    "Economic", 
    "Education", 
    "Housing", 
    "Transportation",
    "Healthcare", 
    "Health Outcomes", 
    "Health Behaviors", 
    "Environmental",
    "Social"
  )
  
  extended_domains <- c(
    "Food Environment & Access",
    "Built Environment",
    "Environmental Health",
    "Economic Factors",
    "Healthcare Access",
    "Traffic Safety",
    "Social Cohesion & Capital",
    "Crime & Safety",
    "Educational Resources & Quality"
  )
  
  # Mapping between extended and standard domains
  domain_mapping <- list(
    "Food Environment & Access" = "Health Behaviors",
    "Built Environment" = "Environmental",
    "Environmental Health" = "Environmental",
    "Economic Factors" = "Economic",
    "Healthcare Access" = "Healthcare",
    "Traffic Safety" = "Transportation",
    "Social Cohesion & Capital" = "Social",
    "Crime & Safety" = "Social",
    "Educational Resources & Quality" = "Education"
  )
  
  # Add standard domain mappings one by one for extended domains
  all_variables$standard_domain <- NA_character_
  
  # First, map standard domains directly
  standard_idx <- all_variables$domain %in% standard_domains
  all_variables$standard_domain[standard_idx] <- all_variables$domain[standard_idx]
  
  # Then map extended domains using the mapping
  for (ext_domain in names(domain_mapping)) {
    std_domain <- domain_mapping[[ext_domain]]
    idx <- all_variables$domain == ext_domain
    all_variables$standard_domain[idx] <- std_domain
  }
  
  # Add any missing columns with default values
  if (!"extended_only" %in% names(all_variables)) {
    all_variables$extended_only <- FALSE
  }
  
  if (!"data_quality_flag_required" %in% names(all_variables)) {
    all_variables$data_quality_flag_required <- FALSE
  }
  
  # Ensure we have other common columns with NA values if they don't exist
  for (col in c("related_to_standard", "notes", "api_source", "api_variable")) {
    if (!col %in% names(all_variables)) {
      all_variables[[col]] <- NA_character_
    }
  }
  
  # Order the columns logically
  col_order <- c(
    "variable_name", 
    "domain", 
    "description", 
    "type", 
    "source", 
    "min_year", 
    "max_year", 
    "units", 
    "related_to_standard", 
    "standard_domain",
    "extended_only",
    "notes",
    "api_source", 
    "api_variable", 
    "data_quality_flag_required"
  )
  
  # Keep only columns in col_order that exist in the dataframe
  col_order <- intersect(col_order, names(all_variables))
  
  # Add any remaining columns
  col_order <- c(col_order, setdiff(names(all_variables), col_order))
  
  # Reorder and sort
  all_variables <- all_variables %>%
    select(all_of(col_order)) %>%
    arrange(domain, variable_name)
  
  # Write the consolidated crosswalk
  print_msg(paste("Writing", nrow(all_variables), "variables to consolidated crosswalk..."), 2)
  write_csv(all_variables, crosswalk_file)
  print_msg(paste("Wrote consolidated crosswalk with", nrow(all_variables), "variables to", crosswalk_file))
  
  # Create a simpler data dictionary view for documentation
  data_dict <- all_variables %>%
    select(variable_name, description, domain, type, source, min_year, max_year, units) %>%
    arrange(domain, variable_name)
  
  # Write data dictionary
  write_csv(data_dict, dict_file)
  print_msg(paste("Wrote data dictionary to:", dict_file))
  
  # Update README.md if it exists and update_documentation is TRUE
  if (update_documentation) {
    update_variable_documentation(all_variables)
  }
  
  # Return the consolidated crosswalk
  return(all_variables)
}

#' Update documentation files with correct variable counts
#'
#' This function updates the README.md file and other documentation
#' with the correct variable counts from the crosswalk
#'
#' @param crosswalk The variable crosswalk dataframe
#' @return TRUE if successful, FALSE otherwise
update_variable_documentation <- function(crosswalk) {
  # Find README.md
  readme_file <- "README.md"
  if (!file.exists(readme_file)) {
    readme_file <- "R/README.md"
  }
  
  if (!file.exists(readme_file)) {
    print(paste("README.md not found in current or R directory"))
    return(FALSE)
  }
  
  print(paste("Updating README.md with correct variable count..."))
  
  # Read the README
  readme_content <- readLines(readme_file)
  
  # Look for the summary table with variable counts
  summary_table_start <- grep("\\| Domain \\| Number of Variables \\|", readme_content)
  if (length(summary_table_start) > 0) {
    # Find the end of the summary table
    summary_table_end <- 0
    for (i in (summary_table_start + 1):length(readme_content)) {
      if (!grepl("^\\|", readme_content[i]) || grepl("^\\| \\*\\*Total\\*\\*", readme_content[i])) {
        summary_table_end <- i
        break
      }
    }
    
    if (summary_table_end > 0) {
      # Extract the table rows
      table_rows <- readme_content[(summary_table_start + 1):(summary_table_end - 1)]
      
      # Create a mapping between README domains and crosswalk categories
      domain_mapping <- list(
        "Demographics & Population" = c("Demographics", "Demographic"),
        "Economic Factors" = c("Economic Factors", "Socioeconomic", "Economic"),
        "Education" = c("Education", "Educational Resources & Quality"),
        "Health Status" = c("Health Status", "Health Outcomes"),
        "Healthcare Access" = c("Healthcare Access", "Healthcare", "Health Access"),
        "Housing" = c("Housing"),
        "Environmental Health" = c("Environmental Health", "Environmental"),
        "Food Environment" = c("Food Environment & Access", "Food Environment"),
        "Transportation" = c("Transportation"),
        "Traffic Safety" = c("Traffic Safety"),
        "Social Cohesion" = c("Social Cohesion & Capital", "Social Factors", "Social"),
        "Crime & Safety" = c("Crime & Safety"),
        "Built Environment" = c("Built Environment"),
        "Disability" = c("Disability"),
        "Health Behaviors" = c("Health Behaviors"),
        "Race/Ethnicity" = c("Race/Ethnicity")
      )
      
      # Count variables by domain
      domain_counts <- crosswalk %>%
        group_by(domain) %>%
        summarise(count = n()) %>%
        arrange(desc(count))
      
      # Calculate counts for each README domain
      readme_domain_counts <- list()
      for (readme_domain in names(domain_mapping)) {
        crosswalk_domains <- domain_mapping[[readme_domain]]
        count <- sum(domain_counts$count[domain_counts$domain %in% crosswalk_domains], na.rm = TRUE)
        readme_domain_counts[[readme_domain]] <- count
      }
      
      # Create updated table rows
      updated_rows <- c()
      for (row in table_rows) {
        # Check if this row contains a domain count
        updated <- FALSE
        for (domain in names(domain_mapping)) {
          if (grepl(paste0("\\| ", domain, " \\|"), row)) {
            # Extract the count for this domain
            count <- readme_domain_counts[[domain]]
            if (!is.null(count) && count > 0) {
              # Replace the count in the row
              updated_row <- gsub("\\| \\d+ \\|", paste0("| ", count, " |"), row)
              updated_rows <- c(updated_rows, updated_row)
              updated <- TRUE
              break
            }
          }
        }
        
        # If not updated, keep the original row
        if (!updated) {
          updated_rows <- c(updated_rows, row)
        }
      }
      
      # Create the total row
      total_count <- nrow(crosswalk)
      total_row <- paste0("| **Total** | **", total_count, "** |")
      
      # Build the updated README
      updated_readme <- c(
        readme_content[1:summary_table_start],
        updated_rows,
        total_row,
        readme_content[(summary_table_end+1):length(readme_content)]
      )
      
      # Create a backup of the README
      backup_readme <- paste0(readme_file, ".bak")
      file.copy(readme_file, backup_readme, overwrite = TRUE)
      print(paste("Created backup of README at", backup_readme))
      
      # Write the updated README
      writeLines(updated_readme, readme_file)
      print(paste("Updated README with corrected variable counts"))
      
      # Also update DATA_DICTIONARY.md if it exists
      data_dictionary_file <- "docs/DATA_DICTIONARY.md"
      if (file.exists(data_dictionary_file)) {
        print(paste("Updating DATA_DICTIONARY.md..."))
        
        # Create a backup
        data_dict_backup <- paste0(data_dictionary_file, ".bak")
        file.copy(data_dictionary_file, data_dict_backup, overwrite = TRUE)
        
        # Read the data dictionary
        dd_content <- readLines(data_dictionary_file)
        
        # Look for the line with the variable count
        var_count_line <- grep("This dataset contains [0-9]+ county-level variables", dd_content)
        if (length(var_count_line) > 0) {
          # Update the count
          dd_content[var_count_line] <- gsub(
            "This dataset contains [0-9]+ county-level variables",
            paste0("This dataset contains ", total_count, " county-level variables"),
            dd_content[var_count_line]
          )
          
          # Write the updated file
          writeLines(dd_content, data_dictionary_file)
          print(paste("Updated DATA_DICTIONARY.md with correct variable count"))
        }
      }
    }
  }
  
  return(TRUE)
}

#' Consolidate crosswalks (legacy function for backwards compatibility)
#'
#' This function simply calls build_unified_crosswalk for backward compatibility
#'
#' @return TRUE if successful, FALSE otherwise
consolidate_crosswalks <- function() {
  cat("Consolidating SDOH variable crosswalk files...\n")
  
  # Call the new unified builder
  result <- build_unified_crosswalk(
    output_dir = "output",
    force_update = TRUE,
    verbose = TRUE,
    update_documentation = TRUE
  )
  
  return(!is.null(result) && nrow(result) > 0)
}

# Execute the function if run directly
if (!is_sourced()) {
  build_unified_crosswalk(force_update = TRUE)
}