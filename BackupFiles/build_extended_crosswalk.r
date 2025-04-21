library(tidyverse)
library(jsonlite)
library(tidycensus)
library(ipumsr)  # For IPUMS NHGIS data
library(tigris)  # For geography

#' Build an extended crosswalk for social determinants of health
#' 
#' This function creates a comprehensive crosswalk that maps standardized variable
#' names to their corresponding variables in Census datasets (Decennial, ACS, PEP),
#' IPUMS NHGIS datasets, and CDC PLACES datasets.
#' 
#' @param include_nhgis Logical; whether to include NHGIS variables (default: TRUE)
#' @param include_places Logical; whether to include CDC PLACES variables (default: TRUE)
build_extended_crosswalk <- function(include_nhgis = TRUE, include_places = TRUE) {
  
  # Setup clean output function
  clean_output <- function(msg) {
    # Check if running interactively or being sourced
    is_interactive_run <- !exists("is_sourced") || (exists("is_sourced") && !is_sourced())
    
    # Use message for cleaner output when interactive
    if (is_interactive_run) {
      message(msg)
    } else {
      cat(msg, "\n")
    }
  }

  # Load the current crosswalk as a starting point
  clean_output("Building on existing crosswalk...")
  if (file.exists("variable_crosswalk_expanded.csv")) {
    base_crosswalk <- read_csv("variable_crosswalk_expanded.csv", show_col_types = FALSE)
  } else {
    # Call the original crosswalk builder if needed
    source("build_crosswalk_final.r")
    base_crosswalk <- build_crosswalk()
  }

  clean_output("Adding IPUMS NHGIS variables...")
  
  # Define IPUMS NHGIS datasets that contain harmonized time series data
  # Note: You'll need to download these datasets from NHGIS and place them in the data folder
  nhgis_datasets <- tribble(
    ~dataset_id, ~description, ~years_covered, ~geographic_level,
    "NHGIS_RACE", "Time Series Tables on Race and Hispanic Origin", "1990-2020", "county",
    "NHGIS_HOUSING", "Time Series Tables on Housing Characteristics", "1990-2020", "county",
    "NHGIS_EDUC", "Time Series Tables on Educational Attainment", "1990-2020", "county",
    "NHGIS_INCOME", "Time Series Tables on Income and Earnings", "1990-2020", "county",
    "NHGIS_EMPLOYMENT", "Time Series Tables on Employment Status", "1990-2020", "county"
  )
  
  # Define variables for CDC PLACES data
  clean_output("Adding CDC PLACES variables...")
  cdc_places_vars <- tribble(
    ~var_id, ~short_name, ~description, ~category,
    "CASTHMA", "asthma_pct", "Current asthma among adults aged ≥18 years", "Chronic Disease",
    "ARTHRITIS", "arthritis_pct", "Arthritis among adults aged ≥18 years", "Chronic Disease",
    "BPHIGH", "high_blood_pressure_pct", "High blood pressure among adults aged ≥18 years", "Cardiovascular",
    "CANCER", "cancer_pct", "Cancer (excluding skin cancer) among adults aged ≥18 years", "Chronic Disease",
    "CHD", "coronary_heart_disease_pct", "Coronary heart disease among adults aged ≥18 years", "Cardiovascular",
    "CHECKUP", "annual_checkup_pct", "Visits to doctor for routine checkup within the past year among adults aged ≥18 years", "Prevention",
    "DEPRESSION", "depression_pct", "Depression among adults aged ≥18 years", "Mental Health",
    "DIABETES", "diabetes_pct", "Diagnosed diabetes among adults aged ≥18 years", "Chronic Disease",
    "HIGHCHOL", "high_cholesterol_pct", "High cholesterol among adults aged ≥18 years who have been screened in the past 5 years", "Cardiovascular",
    "KIDNEY", "kidney_disease_pct", "Chronic kidney disease among adults aged ≥18 years", "Chronic Disease",
    "OBESITY", "obesity_pct", "Obesity among adults aged ≥18 years", "Health Risk Behavior",
    "STROKE", "stroke_pct", "Stroke among adults aged ≥18 years", "Cardiovascular",
    "PHLTH", "poor_physical_health_pct", "Physical health not good for ≥14 days among adults aged ≥18 years", "Health Status",
    "MHLTH", "poor_mental_health_pct", "Mental health not good for ≥14 days among adults aged ≥18 years", "Mental Health",
    "CSMOKING", "smoking_pct", "Current smoking among adults aged ≥18 years", "Health Risk Behavior",
    "DENTAL", "dental_visit_pct", "Dental visit in the past year among adults aged ≥18 years", "Prevention",
    "SLEEP", "insufficient_sleep_pct", "Sleeping less than 7 hours among adults aged ≥18 years", "Health Risk Behavior",
    "ACCESS2", "no_health_insurance_pct", "Current lack of health insurance among adults aged 18-64 years", "Access",
    "BINGE", "binge_drinking_pct", "Binge drinking among adults aged ≥18 years", "Health Risk Behavior",
    "COPD", "copd_pct", "Chronic obstructive pulmonary disease among adults aged ≥18 years", "Chronic Disease"
  )
  
  # Define base variables first (ACS only)
  base_social_determinants <- tribble(
    ~variable_name, ~category, ~description, ~acs_var, ~nhgis_var, ~cdc_places_var,
    
    # Demographics (expanding existing)
    "population_under_18", "Demographics", "Population under 18 years of age", "B01001_003E", NA_character_, NA_character_,
    "population_65_over", "Demographics", "Population 65 years and over", "B01001_020E", NA_character_, NA_character_,
    
    # Race & Ethnicity 
    "white_nonhispanic_pct", "Race/Ethnicity", "White alone, not Hispanic or Latino, percent", "DP05_0077PE", NA_character_, NA_character_,
    "black_pct", "Race/Ethnicity", "Black or African American alone, percent", "DP05_0065PE", NA_character_, NA_character_,
    "hispanic_latino_pct", "Race/Ethnicity", "Hispanic or Latino, percent", "DP05_0071PE", NA_character_, NA_character_,
    "asian_pct", "Race/Ethnicity", "Asian alone, percent", "DP05_0067PE", NA_character_, NA_character_,
    "native_american_pct", "Race/Ethnicity", "American Indian and Alaska Native alone, percent", "DP05_0066PE", NA_character_, NA_character_,
    
    # Socioeconomic Status
    "median_household_income", "Socioeconomic", "Median household income (dollars)", "B19013_001E", NA_character_, NA_character_,
    "poverty_rate", "Socioeconomic", "Percentage of population below poverty level", "S1701_C03_001E", NA_character_, NA_character_,
    "gini_index", "Socioeconomic", "Income inequality (Gini Index)", "B19083_001E", NA_character_, NA_character_,
    "snap_benefits_pct", "Socioeconomic", "Percentage of households receiving SNAP benefits", "S2201_C04_001E", NA_character_, NA_character_,
    
    # Education
    "less_than_highschool_pct", "Education", "Percentage of population 25+ with less than high school education", "B15003_002E", NA_character_, NA_character_,
    "highschool_only_pct", "Education", "Percentage of population 25+ with high school degree only", "B15003_017E", NA_character_, NA_character_,
    "some_college_pct", "Education", "Percentage of population 25+ with some college or associate's degree", "B15003_018E", NA_character_, NA_character_,
    "bachelors_or_higher_pct", "Education", "Percentage of population 25+ with bachelor's degree or higher", "B15003_022E", NA_character_, NA_character_,
    
    # Housing
    "median_home_value", "Housing", "Median value of owner-occupied housing units", "B25077_001E", NA_character_, NA_character_,
    "median_gross_rent", "Housing", "Median gross rent", "B25064_001E", NA_character_, NA_character_,
    "homeownership_rate", "Housing", "Homeownership rate", "DP04_0046PE", NA_character_, NA_character_,
    "vacant_housing_rate", "Housing", "Vacant housing rate", "DP04_0003PE", NA_character_, NA_character_,
    
    # Employment
    "unemployment_rate", "Employment", "Unemployment rate", "DP03_0009PE", NA_character_, NA_character_,
    "labor_force_participation", "Employment", "Labor force participation rate", "DP03_0002PE", NA_character_, NA_character_,
    "median_earnings", "Employment", "Median earnings for workers", "B20017_001E", NA_character_, NA_character_,
    
    # Transportation
    "mean_commute_time", "Transportation", "Mean travel time to work (minutes)", "DP03_0025E", NA_character_, NA_character_,
    "commute_public_transit_pct", "Transportation", "Percentage commuting by public transportation", "DP03_0021PE", NA_character_, NA_character_,
    "no_vehicle_households_pct", "Transportation", "Percentage of households with no vehicle available", "DP04_0058PE", NA_character_, NA_character_,
    "commute_carpool_pct", "Transportation", "Percentage commuting by carpool", "DP03_0019PE", NA_character_, NA_character_,
    "commute_walking_pct", "Transportation", "Percentage commuting by walking", "DP03_0023PE", NA_character_, NA_character_,
    "commute_long_pct", "Transportation", "Percentage with commute of 60 minutes or more", "DP03_0034PE", NA_character_, NA_character_,
    
    # Health Insurance & Access
    "uninsured_pct", "Health Insurance", "Percentage without health insurance", "S2701_C05_001E", NA_character_, NA_character_,
    "private_health_insurance_pct", "Health Insurance", "Percentage with private health insurance", "S2701_C03_001E", NA_character_, NA_character_,
    "public_health_insurance_pct", "Health Insurance", "Percentage with public health insurance", "S2701_C04_001E", NA_character_, NA_character_,
    "medicaid_pct", "Health Insurance", "Percentage with Medicaid/means-tested public coverage", "S2704_C05_001E", NA_character_, NA_character_,
    "medicare_pct", "Health Insurance", "Percentage with Medicare coverage", "S2704_C04_001E", NA_character_, NA_character_,
    
    # Social Factors
    "single_parent_households_pct", "Social Factors", "Percentage of households with single parent", "B09002_002E", NA_character_, NA_character_,
    "limited_english_pct", "Social Factors", "Percentage with limited English proficiency", "S1602_C03_001E", NA_character_, NA_character_,
    "broadband_access_pct", "Social Factors", "Percentage with broadband internet access", "S2801_C01_012E", NA_character_, NA_character_,
    "grandparents_caregivers_pct", "Social Factors", "Percentage of grandparents responsible for their grandchildren", "B10051_003E", NA_character_, NA_character_,
    "internet_access_pct", "Social Factors", "Percentage of households with internet access", "S2801_C01_001E", NA_character_, NA_character_,
    "computer_access_pct", "Social Factors", "Percentage of households with a computer", "S2801_C01_002E", NA_character_, NA_character_,
    "non_english_home_pct", "Social Factors", "Percentage speaking language other than English at home", "DP02_0113PE", NA_character_, NA_character_,
    
    # Disability
    "disability_pct", "Disability", "Percentage of civilian noninstitutionalized population with a disability", "S1810_C02_001E", NA_character_, NA_character_,
    "disability_under_18_pct", "Disability", "Percentage of population under 18 with a disability", "S1810_C02_002E", NA_character_, NA_character_,
    "disability_18_64_pct", "Disability", "Percentage of population 18 to 64 with a disability", "S1810_C02_003E", NA_character_, NA_character_,
    "disability_65_over_pct", "Disability", "Percentage of population 65 and over with a disability", "S1810_C02_004E", NA_character_, NA_character_,
    "cognitive_disability_pct", "Disability", "Percentage with cognitive difficulty", "S1810_C02_006E", NA_character_, NA_character_,
    "ambulatory_disability_pct", "Disability", "Percentage with ambulatory difficulty", "S1810_C02_007E", NA_character_, NA_character_,
    "independent_living_disability_pct", "Disability", "Percentage with independent living difficulty", "S1810_C02_009E", NA_character_, NA_character_,
    
    # Environmental
    "severe_housing_cost_burden", "Environmental", "Percentage with severe housing cost burden", "B25070_010E", NA_character_, NA_character_,
    "overcrowded_housing_pct", "Environmental", "Percentage of housing units with more than 1 person per room", "DP04_0078PE", NA_character_, NA_character_,
    "housing_no_kitchen_pct", "Environmental", "Percentage of housing units lacking complete kitchen facilities", "DP04_0067PE", NA_character_, NA_character_,
    "housing_no_plumbing_pct", "Environmental", "Percentage of housing units lacking complete plumbing facilities", "DP04_0066PE", NA_character_, NA_character_
  )
  
  # NHGIS variables to add if requested
  nhgis_determinants <- if (include_nhgis) {
    tribble(
      ~variable_name, ~category, ~description, ~acs_var, ~nhgis_var, ~cdc_places_var,
      "population_density", "Demographics", "Population per square mile", NA_character_, "POPDENSM", NA_character_,
      "air_pollution_pm25", "Environmental", "Fine particulate matter levels", NA_character_, "PM25", NA_character_,
      "food_insecurity_pct", "Environmental", "Percentage with food insecurity", NA_character_, "FOODINS", NA_character_,
      "physical_inactivity_pct", "Health Behaviors", "Percentage physically inactive", NA_character_, "INACTIVE", NA_character_,
      "severe_housing_problems", "Housing", "Percentage of households with severe housing problems", NA_character_, "HOUSING", NA_character_
    )
  } else {
    tibble(variable_name = character(), category = character(), description = character(), 
           acs_var = character(), nhgis_var = character(), cdc_places_var = character())
  }
  
  # CDC PLACES variables to add if requested
  places_determinants <- if (include_places) {
    tribble(
      ~variable_name, ~category, ~description, ~acs_var, ~nhgis_var, ~cdc_places_var,
      "uninsured_pct", "Health Insurance", "Percentage without health insurance", "S2701_C05_001E", NA_character_, "ACCESS2",
      "annual_checkup_pct", "Health Access", "Percentage with annual checkup", NA_character_, NA_character_, "CHECKUP",
      "dental_visit_pct", "Health Access", "Percentage with dental visit in past year", NA_character_, NA_character_, "DENTAL",
      "poor_physical_health_pct", "Health Status", "Percentage with poor physical health", NA_character_, NA_character_, "PHLTH",
      "poor_mental_health_pct", "Health Status", "Percentage with poor mental health", NA_character_, NA_character_, "MHLTH",
      "depression_pct", "Health Status", "Percentage with depression", NA_character_, NA_character_, "DEPRESSION",
      "obesity_pct", "Health Status", "Percentage with obesity", NA_character_, NA_character_, "OBESITY",
      "diabetes_pct", "Health Status", "Percentage with diabetes", NA_character_, NA_character_, "DIABETES",
      "high_blood_pressure_pct", "Health Status", "Percentage with high blood pressure", NA_character_, NA_character_, "BPHIGH",
      "high_cholesterol_pct", "Health Status", "Percentage with high cholesterol", NA_character_, NA_character_, "HIGHCHOL",
      "asthma_pct", "Health Status", "Percentage with asthma", NA_character_, NA_character_, "CASTHMA",
      "arthritis_pct", "Health Status", "Percentage with arthritis", NA_character_, NA_character_, "ARTHRITIS",
      "cancer_pct", "Health Status", "Percentage with cancer history", NA_character_, NA_character_, "CANCER",
      "copd_pct", "Health Status", "Percentage with COPD", NA_character_, NA_character_, "COPD",
      "kidney_disease_pct", "Health Status", "Percentage with kidney disease", NA_character_, NA_character_, "KIDNEY",
      "coronary_heart_disease_pct", "Health Status", "Percentage with coronary heart disease", NA_character_, NA_character_, "CHD",
      "stroke_pct", "Health Status", "Percentage with stroke history", NA_character_, NA_character_, "STROKE",
      "smoking_pct", "Health Behaviors", "Percentage who smoke", NA_character_, NA_character_, "CSMOKING",
      "physical_inactivity_pct", "Health Behaviors", "Percentage physically inactive", NA_character_, NA_character_, "LPA",
      "binge_drinking_pct", "Health Behaviors", "Percentage who binge drink", NA_character_, NA_character_, "BINGE",
      "insufficient_sleep_pct", "Health Behaviors", "Percentage with insufficient sleep", NA_character_, NA_character_, "SLEEP"
    )
  } else {
    tibble(variable_name = character(), category = character(), description = character(), 
           acs_var = character(), nhgis_var = character(), cdc_places_var = character())
  }
  
  # Combine all determinants
  social_determinants <- bind_rows(
    base_social_determinants,
    nhgis_determinants,
    places_determinants
  ) %>%
  distinct(variable_name, .keep_all = TRUE)
  
  # Add additional CDC PLACES variables to the crosswalk if requested
  # This complements the existing social_determinants data frame
  if (include_places) {
    additional_places_vars <- cdc_places_vars %>%
      anti_join(social_determinants, by = c("short_name" = "variable_name")) %>%
      mutate(
        variable_name = short_name,
        category = case_when(
          category == "Chronic Disease" ~ "Health Status",
          category == "Cardiovascular" ~ "Health Status",
          category == "Mental Health" ~ "Health Status",
          category == "Prevention" ~ "Health Access",
          category == "Health Risk Behavior" ~ "Health Behaviors",
          category == "Access" ~ "Health Insurance",
          TRUE ~ category
        ),
        acs_var = NA_character_,
        nhgis_var = NA_character_,
        cdc_places_var = var_id
      ) %>%
      select(variable_name, category, description, acs_var, nhgis_var, cdc_places_var)
  } else {
    # Empty tibble if PLACES data is not included
    additional_places_vars <- tibble(
      variable_name = character(),
      category = character(),
      description = character(),
      acs_var = character(),
      nhgis_var = character(),
      cdc_places_var = character()
    )
  }
  
  # Combine with base crosswalk and create a comprehensive variable reference
  extended_crosswalk <- bind_rows(
    # Original demographic variables
    base_crosswalk %>% 
      mutate(
        category = "Demographics",
        description = case_when(
          variable_name == "total_population" ~ "Total population",
          variable_name == "median_age" ~ "Median age (years)",
          variable_name == "male_population" ~ "Male population",
          variable_name == "female_population" ~ "Female population",
          TRUE ~ NA_character_
        ),
        nhgis_var = case_when(
          variable_name == "total_population" ~ "TOTPOP",
          variable_name == "median_age" ~ "MEDAGE",
          variable_name == "male_population" ~ "MALE",
          variable_name == "female_population" ~ "FEMALE",
          TRUE ~ NA_character_
        ),
        cdc_places_var = NA_character_
      ),
    
    # New social determinants variables
    social_determinants %>%
      mutate(
        pep_var = NA_character_,
        dec_2000_var = NA_character_,
        dec_2010_var = NA_character_,
        dec_2020_var = NA_character_
      ),
      
    # Additional CDC PLACES variables
    additional_places_vars %>%
      mutate(
        pep_var = NA_character_,
        dec_2000_var = NA_character_,
        dec_2010_var = NA_character_,
        dec_2020_var = NA_character_
      )
  )
  
  # Add data quality flags and interpolation guidance
  extended_crosswalk <- extended_crosswalk %>%
    mutate(
      # Flag indicating if variable is directly available in each data source
      available_in_acs = !is.na(acs_var),
      available_in_decennial = !is.na(dec_2000_var) | !is.na(dec_2010_var) | !is.na(dec_2020_var),
      available_in_pep = !is.na(pep_var),
      available_in_nhgis = !is.na(nhgis_var),
      available_in_places = !is.na(cdc_places_var),
      
      # Flag if interpolation is recommended for this variable
      interpolate_recommended = case_when(
        category %in% c("Demographics", "Race/Ethnicity", "Housing", "Education") ~ TRUE,
        category %in% c("Health Status", "Health Behaviors") ~ FALSE,  # Health metrics often shouldn't be interpolated
        TRUE ~ TRUE  # Default to TRUE for other categories
      ),
      
      # Preferred source priority
      preferred_source = case_when(
        available_in_places ~ "CDC PLACES",  # Health metrics best from CDC
        available_in_nhgis ~ "IPUMS NHGIS",  # NHGIS for longitudinal consistency
        available_in_acs ~ "ACS 5-Year",     # ACS for most other variables
        available_in_decennial ~ "Decennial Census",
        available_in_pep ~ "PEP",
        TRUE ~ NA_character_
      ),
      
      # Years available (simplified)
      years_available = case_when(
        available_in_acs ~ "2009-2021",  # ACS 5-year
        available_in_decennial ~ "2000, 2010, 2020",
        available_in_nhgis ~ "1990-2020 (varies)",
        available_in_places ~ "2019-2021",
        TRUE ~ NA_character_
      ),
      
      # Add temporal extension flag - should this be extended back in time
      extend_backwards = case_when(
        category %in% c("Health Status", "Health Behaviors", "Health Access") & 
          available_in_places & !available_in_acs & !available_in_nhgis ~ TRUE,
        TRUE ~ FALSE
      ),
      
      # Extension method to use
      extend_method = case_when(
        extend_backwards ~ "constant", # Use constant values for health metrics
        TRUE ~ NA_character_
      )
    )
  
  # Save the extended crosswalk
  write_csv(extended_crosswalk, "variable_crosswalk_extended.csv")
  clean_output("Extended crosswalk saved to 'variable_crosswalk_extended.csv'.")
  
  # Create data dictionary - more detailed than the crosswalk for documentation
  data_dictionary <- extended_crosswalk %>%
    mutate(
      variable_type = case_when(
        str_detect(variable_name, "pct$|rate$") ~ "Percentage (0-100)",
        str_detect(variable_name, "median|mean") ~ "Currency ($) or Count",
        str_detect(variable_name, "population") ~ "Count",
        TRUE ~ "Varies"
      ),
      limitations = case_when(
        !available_in_acs & !available_in_decennial & !available_in_pep & 
          !available_in_nhgis & !available_in_places ~ 
          "Not directly available in standard sources; derived or estimated.",
        str_detect(years_available, "2019") & category %in% c("Health Status", "Health Behaviors") ~
          "Only available in recent years; values for earlier years are extended from earliest available data.",
        TRUE ~ ""
      )
    ) %>%
    select(
      variable_name, description, category, variable_type, 
      preferred_source, years_available, interpolate_recommended, 
      extend_backwards, extend_method, limitations
    )
  
  # Save the data dictionary
  write_csv(data_dictionary, "data_dictionary.csv")
  clean_output("Data dictionary saved to 'data_dictionary.csv'.")
  
  # Return the extended crosswalk
  return(extended_crosswalk)
}

# If this script is run directly, execute the function
if (!interactive()) {
  extended_crosswalk <- build_extended_crosswalk()
}        