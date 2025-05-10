#!/usr/bin/env Rscript

# Basic crosswalk builder for Social Determinants of Health variables
# This creates a mapping between standardized variable names and Census codes

#' Build a basic crosswalk for demographic variables
#' 
#' This function creates a simple crosswalk that maps standardized variable
#' names to their corresponding variables in Census datasets (Decennial, ACS, PEP).
#' 
#' @return A tibble with variable crosswalk information
build_crosswalk <- function() {
  # Define basic demographic variables
  crosswalk <- tibble::tribble(
    ~variable_name, ~acs_var,     ~dec_2020_var, ~dec_2010_var, ~dec_2000_var, ~pep_var,
    "total_population", "B01003_001E", "P1_001N",    "P003001",     "P001001",     "POP",
    "median_age",       "B01002_001E", "P13_001N",   "P013001",     "P013001",     NA,
    "male_population",  "B01001_002E", "P1_002N",    "P012002",     "P012002",     NA,
    "female_population", "B01001_026E", "P1_026N",   "P012026",     "P012026",     NA
  )
  
  return(crosswalk)
}