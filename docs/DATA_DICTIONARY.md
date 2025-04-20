# Data Dictionary for Social Determinants of Health Dataset

This documentation provides comprehensive details about all data sources used in the Social Determinants of Health pipeline. Each table lists the variables available in a specific domain, their source, and time range.

## Summary of Variables by Domain

| Domain | Number of Variables | Primary Data Sources |
|--------|---------------------|----------------------|
| Demographics & Population | 6 | Census Bureau, IPUMS NHGIS, SEER |
| Economic Factors | 34 | Census ACS, BLS, Opportunity Insights |
| Education | 20 | Census ACS, NCES, Stanford Education Data Archive |
| Health Status | 46 | CDC PLACES, CDC WONDER, IHME |
| Healthcare Access | 16 | HRSA Area Health Resources Files, CMS |
| Housing | 24 | Census ACS, HUD CHAS, Eviction Lab |
| Environmental Health | 18 | EPA Air Quality System, EPA TRI, CDC Environmental Public Health Tracking |
| Food Environment | 15 | USDA Food Environment Atlas, Feeding America |
| Transportation | 17 | Census ACS, National Transit Database |
| Traffic Safety | 12 | NHTSA FARS, CDC WONDER |
| Social Cohesion | 11 | Census ACS, County Health Rankings, MIT Election Data |
| Crime & Safety | 5 | FBI Uniform Crime Reports, Bureau of Justice Statistics |
| Built Environment | 10 | EPA Smart Location Database, Trust for Public Land |
| Digital Access | 6 | FCC, Census ACS |
| Climate & Weather | 7 | NOAA, EPA |
| **Total** | **255** | |

## Demographics and Population Data

| Variable Name | Description | Source | Years Available | Units |
|---------------|-------------|--------|-----------------|-------|
| total_population | Total population | Census/NHGIS | 1970-present | count |
| median_age | Median age | Census/NHGIS | 1970-present | years |
| male_population | Male population | Census/NHGIS | 1970-present | count |
| female_population | Female population | Census/NHGIS | 1970-present | count |
| population_under_18 | Population under 18 years | Census/NHGIS | 1970-present | count |
| population_65_over | Population 65 years and over | Census/NHGIS | 1970-present | count |
| white_nonhispanic_pct | White alone, not Hispanic or Latino | Census/NHGIS | 1970-present | percent |
| black_pct | Black or African American alone | Census/NHGIS | 1970-present | percent |
| hispanic_latino_pct | Hispanic or Latino | Census/NHGIS | 1970-present | percent |
| asian_pct | Asian alone | Census/NHGIS | 1970-present | percent |
| native_american_pct | American Indian and Alaska Native alone | Census/NHGIS | 1970-present | percent |
| population_density | Population per square mile | Census/NHGIS | 1970-present | density |
| rural_population_pct | Rural population percentage | Census/NHGIS | 1970-present | percent |
| urban_population_pct | Urban population percentage | Census/NHGIS | 1970-present | percent |
| age_under_5_pct | Population under 5 years | Census/NHGIS | 1970-present | percent |
| age_5_17_pct | Population 5 to 17 years | Census/NHGIS | 1970-present | percent |
| age_18_24_pct | Population 18 to 24 years | Census/NHGIS | 1970-present | percent |
| age_25_44_pct | Population 25 to 44 years | Census/NHGIS | 1970-present | percent |
| age_45_64_pct | Population 45 to 64 years | Census/NHGIS | 1970-present | percent |
| age_85_over_pct | Population 85 years and over | Census/NHGIS | 1970-present | percent |
| dependency_ratio | Age dependency ratio | Census/NHGIS | 1970-present | ratio |
| foreign_born_pct | Foreign born population | Census/NHGIS | 1970-present | percent |
| net_migration_rate | Net migration rate | Census/SEER | 1970-present | rate |
| natural_change_rate | Rate of natural population change | Census/SEER | 1970-present | rate |

## Economic Factors Data

| Variable Name | Description | Source | Years Available | Units |
|---------------|-------------|--------|-----------------|-------|
| median_household_income | Median household income | Census ACS | 1970-present | dollars |
| poverty_rate | Population below poverty line | Census ACS | 1970-present | percent |
| gini_index | Income inequality (Gini Index) | Census ACS | 1990-present | index (0-1) |
| snap_benefits_pct | Households receiving SNAP | Census ACS | 1990-present | percent |
| unemployment_rate | Unemployment rate | BLS | 1990-present | percent |
| labor_force_participation | Labor force participation rate | Census ACS | 1990-present | percent |
| median_earnings | Median earnings for workers | Census ACS | 1990-present | dollars |
| absolute_upward_mobility | Expected income rank for children from low-income families | Opportunity Insights | 2000-2018 | percentile |
| economic_distress_index | Composite index of economic distress | Appalachian Regional Commission | 2000-2023 | index |
| economic_typology | County economic typology | USDA Economic Research Service | 2000-2023 | category |
| employment_volatility_index | Index of employment stability/volatility | USDA Economic Research Service | 2000-2023 | index |
| income_inequality_ratio | Ratio of income at 80th to 20th percentile | ACS | 2010-2023 | ratio |
| income_mobility_index | Measure of intergenerational economic mobility | Opportunity Insights | 2000-2018 | index |
| job_density_index | Number of jobs within typical commute distance | Opportunity Insights | 2000-2018 | index |
| job_growth_rate | Annual job growth rate | Bureau of Labor Statistics | 2000-2023 | percent |
| persistent_child_poverty_county | Flag for counties with persistent child poverty | USDA | 2000-2023 | binary |
| persistent_poverty_county | Flag for counties with persistent poverty | USDA | 2000-2023 | binary |

## Education Data

| Variable Name | Description | Source | Years Available | Units |
|---------------|-------------|--------|-----------------|-------|
| less_than_highschool_pct | Less than high school education | Census ACS | 1970-present | percent |
| highschool_only_pct | High school degree only | Census ACS | 1970-present | percent |
| some_college_pct | Some college or associate's | Census ACS | 1970-present | percent |
| bachelors_or_higher_pct | Bachelor's degree or higher | Census ACS | 1970-present | percent |
| educational_opportunity_index | Measure of educational opportunity | Stanford Education Data Archive | 2009-2018 | index |
| high_school_graduation_rate | Four-year high school graduation rate | NCES | 2000-2022 | percent |
| math_achievement_gap | Achievement gap in math scores by race/ethnicity | Stanford Education Data Archive | 2009-2018 | z-score |
| per_pupil_expenditure | Per-pupil expenditure in public schools | NCES | 2000-2022 | dollars |
| preschool_enrollment_rate | Percentage of 3-4 year-olds enrolled in preschool | NCES | 2000-2022 | percent |
| reading_achievement_gap | Achievement gap in reading scores by race/ethnicity | Stanford Education Data Archive | 2009-2018 | z-score |
| school_funding_equity | Ratio of funding in high vs. low poverty districts | NCES | 2000-2022 | ratio |
| student_teacher_ratio | Student-to-teacher ratio in public schools | NCES | 2000-2022 | ratio |
| associate_degree_pct | Associate's degree | Census ACS | 1990-present | percent |
| graduate_degree_pct | Graduate or professional degree | Census ACS | 1990-present | percent |
| school_enrollment_k12_pct | School enrollment, K-12 | Census ACS | 1990-present | percent |

## Housing Data

| Variable Name | Description | Source | Years Available | Units |
|---------------|-------------|--------|-----------------|-------|
| median_home_value | Median home value | Census ACS | 1970-present | dollars |
| median_gross_rent | Median gross rent | Census ACS | 1970-present | dollars |
| homeownership_rate | Homeownership rate | Census ACS | 1970-present | percent |
| vacant_housing_rate | Vacant housing rate | Census ACS | 1970-present | percent |
| severe_housing_cost_burden | Severe housing cost burden | HUD | 1990-present | percent |
| overcrowded_housing_pct | >1 person per room | Census ACS | 1990-present | percent |
| housing_no_kitchen_pct | Lacking kitchen facilities | Census ACS | 1990-present | percent |
| housing_no_plumbing_pct | Lacking plumbing facilities | Census ACS | 1990-present | percent |
| eviction_filing_rate | Number of eviction filings per 100 renter homes | Eviction Lab | 2000-2018 | rate |
| eviction_rate | Number of evictions per 100 renter homes | Eviction Lab | 2000-2018 | rate |
| foreclosure_rate | Foreclosures per 1,000 housing units | Federal Reserve HMDA | 2007-2023 | rate |
| high_cost_loans_pct | Percentage of loans that are high-cost | Federal Reserve HMDA | 2007-2023 | percent |
| housing_problems_pct | Households with at least one housing problem | HUD CHAS | 2006-2020 | percent |
| low_income_renters_affordable_units_ratio | Ratio of affordable units to low-income renters | HUD CHAS | 2006-2020 | ratio |
| mortgage_denial_rate | Percentage of mortgage applications denied | Federal Reserve HMDA | 2007-2023 | percent |
| rent_burden_pct | Percentage of income spent on rent (median) | Eviction Lab | 2000-2018 | percent |
| severely_cost_burdened_owners_pct | Owner households spending >50% of income on housing | HUD CHAS | 2006-2020 | percent |
| severely_cost_burdened_renters_pct | Renter households spending >50% of income on housing | HUD CHAS | 2006-2020 | percent |

## Transportation Data

| Variable Name | Description | Source | Years Available | Units |
|---------------|-------------|--------|-----------------|-------|
| mean_commute_time | Mean travel time to work | Census ACS | 1990-present | minutes |
| commute_public_transit_pct | Public transit commuters | Census ACS | 1990-present | percent |
| no_vehicle_households_pct | Households with no vehicle | Census ACS | 1990-present | percent |
| commute_carpool_pct | Carpool commuters | Census ACS | 1990-present | percent |
| commute_walking_pct | Walking commuters | Census ACS | 1990-present | percent |
| commute_long_pct | Commute ≥60 minutes | Census ACS | 1990-present | percent |
| public_transit_trips_per_capita | Public transit trips per capita | National Transit Database | 2000-2022 | count |
| transit_access_jobs | Jobs accessible by transit within 30 minutes | All Transit Database | 2012-2022 | count |
| transit_connectivity_index | Measure of transit connectivity | All Transit Database | 2012-2022 | index |
| transit_performance_index | Composite measure of transit performance | All Transit Database | 2012-2022 | index |
| transportation_cost_burden_pct | Transportation costs as percentage of household income | National Household Travel Survey | 2001-2017 | percent |
| vehicle_miles_traveled_per_capita | Annual vehicle miles traveled per capita | National Household Travel Survey | 2001-2017 | miles |
| zero_vehicle_households_pct | Percentage of households with no vehicles | American Community Survey | 2009-2023 | percent |

## Traffic Safety Data

| Variable Name | Description | Source | Years Available | Units |
|---------------|-------------|--------|-----------------|-------|
| traffic_fatality_count | Traffic fatalities | NHTSA FARS | 1975-present | count |
| traffic_fatality_rate_per_100k | Traffic fatality rate | NHTSA FARS | 1975-present | rate per 100k |
| traffic_injury_count | Traffic injuries | NHTSA FARS | 1975-present | count |
| traffic_injury_rate_per_100k | Traffic injury rate | NHTSA FARS | 1975-present | rate per 100k |
| ped_bike_fatality_count | Pedestrian/cyclist fatalities | NHTSA FARS | 1975-present | count |
| ped_bike_fatality_rate_per_100k | Pedestrian/cyclist fatality rate | NHTSA FARS | 1975-present | rate per 100k |
| dui_fatality_count | DUI-related fatalities | NHTSA FARS | 1975-present | count |
| dui_fatality_rate_per_100k | DUI-related fatality rate | NHTSA FARS | 1975-present | rate per 100k |
| speeding_fatality_count | Speeding-related fatalities | NHTSA FARS | 1975-present | count |
| speeding_fatality_rate_per_100k | Speeding-related fatality rate | NHTSA FARS | 1975-present | rate per 100k |
| transport_mortality_count | Transport-related deaths | CDC WONDER | 1999-2021 | count |

## Health Insurance Data

| Variable Name | Description | Source | Years Available | Units |
|---------------|-------------|--------|-----------------|-------|
| uninsured_pct | Without health insurance | Census ACS | 1990-present | percent |
| private_health_insurance_pct | Private health insurance | Census ACS | 1990-present | percent |
| public_health_insurance_pct | Public health insurance | Census ACS | 1990-present | percent |
| medicaid_pct | Medicaid coverage | Census ACS | 1990-present | percent |
| medicare_pct | Medicare coverage | Census ACS | 1990-present | percent |
| no_health_insurance_pct | Current lack of health insurance (adults 18-64) | CDC PLACES | 2019-2021 | percent |

## Health Status Data

| Variable Name | Description | Source | Years Available | Units |
|---------------|-------------|--------|-----------------|-------|
| poor_physical_health_pct | Poor physical health | CDC PLACES | 2016-present | percent |
| poor_mental_health_pct | Poor mental health | CDC PLACES | 2016-present | percent |
| depression_pct | Depression | CDC PLACES | 2016-present | percent |
| obesity_pct | Obesity | CDC PLACES | 2016-present | percent |
| diabetes_pct | Diabetes | CDC PLACES | 2016-present | percent |
| high_blood_pressure_pct | High blood pressure | CDC PLACES | 2016-present | percent |
| high_cholesterol_pct | High cholesterol | CDC PLACES | 2016-present | percent |
| asthma_pct | Asthma | CDC PLACES | 2016-present | percent |
| arthritis_pct | Arthritis | CDC PLACES | 2016-present | percent |
| cancer_pct | Cancer history | CDC PLACES | 2016-present | percent |
| copd_pct | COPD | CDC PLACES | 2016-present | percent |
| kidney_disease_pct | Kidney disease | CDC PLACES | 2016-present | percent |
| coronary_heart_disease_pct | Coronary heart disease | CDC PLACES | 2016-present | percent |
| stroke_pct | Stroke history | CDC PLACES | 2016-present | percent |
| annual_checkup_pct | Annual checkup | CDC PLACES | 2016-present | percent |
| dental_visit_pct | Dental visit in past year | CDC PLACES | 2016-present | percent |
| life_expectancy | Life expectancy at birth (all races/genders combined) | IHME | 2000-2019 | years |
| life_expectancy_female | Female life expectancy at birth | IHME | 2000-2019 | years |
| life_expectancy_male | Male life expectancy at birth | IHME | 2000-2019 | years |
| life_expectancy_latino | Latino life expectancy at birth | IHME | 2000-2019 | years |
| life_expectancy_black | Black life expectancy at birth | IHME | 2000-2019 | years |
| life_expectancy_white | White life expectancy at birth | IHME | 2000-2019 | years |
| life_expectancy_aian | American Indian/Alaska Native life expectancy | IHME | 2000-2019 | years |
| life_expectancy_api | Asian/Pacific Islander life expectancy | IHME | 2000-2019 | years |
| years_potential_life_lost | Years of potential life lost before age 75 | CDC WONDER | 1990-present | years per 100k |
| age_adjusted_mortality | Age-adjusted mortality rate | CDC WONDER | 1990-present | rate per 100k |
| premature_death_rate | Premature death rate | CDC WONDER | 1990-present | rate per 100k |
| infant_mortality_rate | Infant mortality rate | CDC WONDER | 1990-present | rate per 1k |
| low_birthweight_pct | Low birthweight | CDC WONDER | 1990-present | percent |
| teen_birth_rate | Teen birth rate | CDC WONDER | 1990-present | rate per 1k |
| adult_smoking_pct | Adult smoking | CDC PLACES | 2016-present | percent |
| adult_obesity_pct | Adult obesity | CDC PLACES | 2016-present | percent |
| physical_inactivity_pct | Physical inactivity | CDC PLACES | 2016-present | percent |
| excessive_drinking_pct | Excessive drinking | CDC PLACES | 2016-present | percent |
| drug_overdose_mortality | Drug overdose mortality rate | CDC WONDER | 1990-present | rate per 100k |
| suicide_rate | Suicide rate | CDC WONDER | 1990-present | rate per 100k |

## Healthcare Access Data

| Variable Name | Description | Source | Years Available | Units |
|---------------|-------------|--------|-----------------|-------|
| ambulatory_care_sensitive_conditions | Rate of hospitalization for ambulatory care sensitive conditions | CMS Geographic Variation | 2007-2021 | rate |
| dentists_per_100k | Dentists per 100,000 population | HRSA Area Health Resources Files | 2000-2023 | count/100k |
| fqhc_access_pct | Percentage with access to Federally Qualified Health Centers | HRSA Area Health Resources Files | 2000-2023 | percent |
| hospital_beds_per_1000 | Hospital beds per 1,000 population | HRSA Area Health Resources Files | 2000-2023 | count/1000 |
| medicare_spending_per_beneficiary | Medicare spending per beneficiary | CMS Geographic Variation | 2007-2021 | dollars |
| mental_health_providers_per_100k | Mental health providers per 100,000 population | HRSA Area Health Resources Files | 2000-2023 | count/100k |
| pharmacies_per_100k | Pharmacies per 100,000 population | HRSA Area Health Resources Files | 2000-2023 | count/100k |
| preventable_hospital_stays | Preventable hospital stays per 100,000 Medicare enrollees | HRSA Area Health Resources Files | 2000-2023 | count/100k |
| preventive_services_pct | Percentage of Medicare beneficiaries receiving preventive services | CMS Geographic Variation | 2007-2021 | percent |
| primary_care_physicians_per_100k | Primary care physicians per 100,000 population | HRSA Area Health Resources Files | 2000-2023 | count/100k |
| hospital_accessibility_index | Hospital accessibility index | HRSA | 2000-2023 | index |

## Health Behaviors Data

| Variable Name | Description | Source | Years Available | Units |
|---------------|-------------|--------|-----------------|-------|
| smoking_pct | Current smokers | CDC PLACES | 2016-present | percent |
| binge_drinking_pct | Binge drinking | CDC PLACES | 2016-present | percent |
| physical_inactivity_pct | Physical inactivity | CDC PLACES | 2016-present | percent |
| insufficient_sleep_pct | Insufficient sleep | CDC PLACES | 2016-present | percent |

## Environmental Health Data

| Variable Name | Description | Source | Years Available | Units |
|---------------|-------------|--------|-----------------|-------|
| air_pollution_pm25 | PM2.5 concentration | EPA | 1990-present | µg/m³ |
| air_quality_days_unhealthy | Days with unhealthy air quality | EPA Air Quality System | 2000-2023 | days |
| air_toxics_cancer_risk | Air toxics cancer risk | EPA Air Quality System | 2000-2023 | per million |
| diesel_pm_concentration | Diesel particulate matter concentration | EPA Air Quality System | 2000-2023 | µg/m³ |
| drought_severity_index | Average drought severity index | CDC Environmental Public Health Tracking | 2002-2022 | index |
| extreme_heat_days | Annual number of extreme heat days | CDC Environmental Public Health Tracking | 2002-2022 | days |
| extreme_precipitation_events | Annual number of extreme precipitation events | CDC Environmental Public Health Tracking | 2002-2022 | count |
| lead_exposure_risk_index | Index of lead exposure risk | CDC Environmental Public Health Tracking | 2002-2022 | index |
| lead_paint_indicator | Percentage of housing units built pre-1960 | EPA EJSCREEN | 2016-2023 | percent |
| ozone_days_exceeding | Days exceeding ozone standards | EPA Air Quality System | 2000-2023 | days |
| proximity_to_hazardous_waste | Count of hazardous waste facilities within 5km | EPA EJSCREEN | 2016-2023 | count |
| proximity_to_npl_sites | Proximity to National Priorities List (Superfund) sites | EPA EJSCREEN | 2016-2023 | index |
| public_water_violations | Number of public water system violations | CDC Environmental Public Health Tracking | 2002-2022 | count |
| respiratory_hazard_index | Respiratory hazard index from air pollutants | EPA Air Quality System | 2000-2023 | index |

## Food Environment Data

| Variable Name | Description | Source | Years Available | Units |
|---------------|-------------|--------|-----------------|-------|
| food_insecurity_pct | Food insecurity | USDA | 2000-present | percent |
| child_food_insecurity_rate | Percentage of children experiencing food insecurity | Feeding America Map the Meal Gap | 2009-2022 | percent |
| children_low_access_pct | Percentage of children with low access to a grocery store | USDA Food Environment Atlas | 2010-2022 | percent |
| convenience_stores_per_1000 | Number of convenience stores per 1,000 population | USDA Food Environment Atlas | 2010-2022 | count/1000 |
| farmers_markets_per_1000 | Farmers markets per 1,000 population | USDA Food Environment Atlas | 2010-2022 | count/1000 |
| fast_food_restaurants_per_1000 | Fast food restaurants per 1,000 population | USDA Food Environment Atlas | 2010-2022 | count/1000 |
| food_insecurity_cost_per_person | Average cost per person to meet food needs | Feeding America Map the Meal Gap | 2009-2022 | dollars |
| full_service_restaurants_per_1000 | Full-service restaurants per 1,000 population | USDA Food Environment Atlas | 2010-2022 | count/1000 |
| grocery_stores_per_1000 | Number of supermarkets and grocery stores per 1,000 population | USDA Food Environment Atlas | 2010-2022 | count/1000 |
| low_income_low_access_pct | Percentage that is low income with low grocery store access | USDA Food Environment Atlas | 2010-2022 | percent |
| seniors_low_access_pct | Percentage of seniors with low access to a grocery store | USDA Food Environment Atlas | 2010-2022 | percent |
| snap_authorized_stores_per_1000 | SNAP-authorized retailers per 1,000 population | USDA Food Environment Atlas | 2010-2022 | count/1000 |

## Social Cohesion Data

| Variable Name | Description | Source | Years Available | Units |
|---------------|-------------|--------|-----------------|-------|
| single_parent_households_pct | Single-parent households | Census ACS | 1990-present | percent |
| limited_english_pct | Limited English proficiency | Census ACS | 1990-present | percent |
| broadband_access_pct | Broadband internet access | Census ACS | 2013-present | percent |
| grandparents_caregivers_pct | Grandparents as caregivers | Census ACS | 1990-present | percent |
| internet_access_pct | Internet access | Census ACS | 2013-present | percent |
| computer_access_pct | Computer access | Census ACS | 2013-present | percent |
| non_english_home_pct | Non-English at home | Census ACS | 1990-present | percent |
| nonprofit_organizations_per_10k | Nonprofit organizations per 10,000 population | County Health Rankings | 2014-2023 | count/10k |
| political_competition_index | Index measuring political competition | MIT Election Data and Science Lab | 2000-2022 | index |
| religious_congregation_rate | Religious congregations per 10,000 population | County Health Rankings | 2014-2023 | count/10k |
| social_association_rate | Social associations per 10,000 population | County Health Rankings | 2014-2023 | count/10k |
| voter_turnout_rate | Voter turnout rate in general elections | MIT Election Data and Science Lab | 2000-2022 | percent |

## Disability Data

| Variable Name | Description | Source | Years Available | Units |
|---------------|-------------|--------|-----------------|-------|
| disability_pct | Any disability | Census ACS | 1990-present | percent |
| disability_under_18_pct | Disability under 18 | Census ACS | 1990-present | percent |
| disability_18_64_pct | Disability 18-64 | Census ACS | 1990-present | percent |
| disability_65_over_pct | Disability 65+ | Census ACS | 1990-present | percent |
| cognitive_disability_pct | Cognitive disability | Census ACS | 1990-present | percent |
| ambulatory_disability_pct | Ambulatory disability | Census ACS | 1990-present | percent |
| independent_living_disability_pct | Independent living disability | Census ACS | 1990-present | percent |

## Crime & Safety Data

| Variable Name | Description | Source | Years Available | Units |
|---------------|-------------|--------|-----------------|-------|
| violent_crime_rate | Violent crimes per 100,000 population | FBI Uniform Crime Reports | 2000-2021 | count/100k |
| property_crime_rate | Property crimes per 100,000 population | FBI Uniform Crime Reports | 2000-2021 | count/100k |
| homicide_rate | Homicides per 100,000 population | FBI Uniform Crime Reports | 2000-2021 | count/100k |
| jail_incarceration_rate | County jail inmates per 100,000 population | Bureau of Justice Statistics | 2000-2020 | count/100k |
| pretrial_detention_rate | Pretrial detainees per 100,000 population | Bureau of Justice Statistics | 2000-2020 | count/100k |
| crime_rate_index | Overall crime rate index | FBI UCR | 2000-2021 | index |
| drug_arrest_rate | Drug-related arrests per 100,000 | FBI UCR | 2000-2021 | count/100k |
| juvenile_arrest_rate | Juvenile arrests per 100,000 juveniles | FBI UCR | 2000-2021 | count/100k |

## Built Environment Data

| Variable Name | Description | Source | Years Available | Units |
|---------------|-------------|--------|-----------------|-------|
| employment_access_index | Access to employment centers | EPA Smart Location Database | 2010-2021 | index |
| housing_density | Housing units per acre of developed land | EPA Smart Location Database | 2010-2021 | units/acre |
| land_use_diversity | Mix of land uses (entropy index) | EPA Smart Location Database | 2010-2021 | index |
| park_access_pct | Percentage of residents living within 10-minute walk of a park | Trust for Public Land ParkScore | 2012-2022 | percent |
| park_acres_per_1000 | Park acres per 1,000 residents | Trust for Public Land ParkScore | 2012-2022 | acres/1000 |

## Digital Access Data

| Variable Name | Description | Source | Years Available | Units |
|---------------|-------------|--------|-----------------|-------|
| broadband_access_pct | Broadband internet access | Census ACS | 2013-present | percent |
| internet_access_pct | Internet access | Census ACS | 2013-present | percent |
| computer_access_pct | Computer access | Census ACS | 2013-present | percent |
| digital_equity_score | Digital equity composite score | FCC | 2015-present | index |
| high_speed_internet_pct | High-speed internet subscription | FCC/ACS | 2013-present | percent |
| cellular_coverage_pct | Cellular network coverage | FCC | 2015-present | percent |

## Climate & Weather Data

| Variable Name | Description | Source | Years Available | Units |
|---------------|-------------|--------|-----------------|-------|
| drought_severity_index | Average drought severity index | CDC/NOAA | 2002-2022 | index |
| extreme_heat_days | Annual number of extreme heat days | CDC/NOAA | 2002-2022 | days |
| extreme_precipitation_events | Annual number of extreme precipitation events | CDC/NOAA | 2002-2022 | count |
| flood_risk_index | Flood risk index | FEMA/NOAA | 2000-present | index |
| average_temperature | Annual average temperature | NOAA | 1980-present | °F |
| average_precipitation | Annual average precipitation | NOAA | 1980-present | inches |
| natural_disaster_count | Count of declared natural disasters | FEMA | 2000-present | count |

## Data Consistency and Quality

Each variable includes metadata about its:
- Data quality (direct, interpolated, extrapolated, simulated)
- Data source (original source of information)
- Data vintage (original year or time period of collection)

The pipeline attempts to obtain direct data from authoritative sources when available, and uses interpolation, extrapolation, or simulation (when explicitly allowed) to fill gaps in time series data.