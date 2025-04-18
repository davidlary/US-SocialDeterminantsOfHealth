# Data Dictionary for Social Determinants of Health Dataset

This documentation provides comprehensive details about all data sources used in the Social Determinants of Health pipeline. Each table lists the variables available in a specific domain, their source, and time range.

## Summary of Variables by Domain

| Domain | Number of Variables | Primary Data Sources |
|--------|---------------------|--------------------|
| Built Environment | 10 | EPA Smart Location Database, Trust for Public Land ParkScore |
| Crime & Safety | 5 | FBI Uniform Crime Reports, Bureau of Justice Statistics |
| Economic Factors | 11 | Opportunity Insights, USDA Economic Research Service, ACS |
| Educational Resources & Quality | 8 | NCES, Stanford Education Data Archive |
| Environmental Health | 15 | EPA Air Quality System, CDC Environmental Public Health Tracking |
| Food Environment & Access | 15 | USDA Food Environment Atlas, Feeding America |
| Healthcare Access | 10 | HRSA Area Health Resources Files, CMS |
| Housing | 11 | HUD CHAS, Eviction Lab, Federal Reserve HMDA |
| Social Cohesion & Capital | 6 | County Health Rankings, MIT Election Data |
| Transportation | 7 | National Transit Database, All Transit Database |
| **Total** | **98** | |

## Crime & Safety Data

The pipeline reads crime data from the following sources:

| Variable Name | Description | Source | Years Available | Units |
|---------------|-------------|--------|-----------------|-------|
| violent_crime_rate | Violent crimes per 100,000 population | FBI Uniform Crime Reports | 2000-2021 | count/100k |
| property_crime_rate | Property crimes per 100,000 population | FBI Uniform Crime Reports | 2000-2021 | count/100k |
| homicide_rate | Homicides per 100,000 population | FBI Uniform Crime Reports | 2000-2021 | count/100k |
| jail_incarceration_rate | County jail inmates per 100,000 population | Bureau of Justice Statistics | 2000-2020 | count/100k |
| pretrial_detention_rate | Pretrial detainees per 100,000 population | Bureau of Justice Statistics | 2000-2020 | count/100k |

### FBI Uniform Crime Reports (UCR)
The FBI's Uniform Crime Reports provide standardized offense statistics from approximately 18,000 law enforcement agencies nationwide. The pipeline retrieves county-level crime rates for violent crime, property crime, and homicide, normalized per 100,000 population.

### Bureau of Justice Statistics (BJS)
The Bureau of Justice Statistics provides county-level jail incarceration data, including general jail population rates and pretrial detention rates (which measures the number of people held in jail before being convicted of a crime).

## Built Environment Data

| Variable Name | Description | Source | Years Available | Units |
|---------------|-------------|--------|-----------------|-------|
| employment_access_index | Access to employment centers | EPA Smart Location Database | 2010-2021 | index |
| housing_density | Housing units per acre of developed land | EPA Smart Location Database | 2010-2021 | units/acre |
| land_use_diversity | Mix of land uses (entropy index) | EPA Smart Location Database | 2010-2021 | index |
| park_access_pct | Percentage of residents living within 10-minute walk of a park | Trust for Public Land ParkScore | 2012-2022 | percent |
| park_acres_per_1000 | Park acres per 1,000 residents | Trust for Public Land ParkScore | 2012-2022 | acres/1000 |
| park_spending_per_capita | Park system spending per resident | Trust for Public Land ParkScore | 2012-2022 | dollars |
| playgrounds_per_10000 | Playgrounds per 10,000 residents | Trust for Public Land ParkScore | 2012-2022 | count/10000 |
| street_intersection_density | Number of intersections per square mile | EPA Smart Location Database | 2010-2021 | count/sq mile |
| transit_service_density | Transit routes and stops per square mile | EPA Smart Location Database | 2010-2021 | count/sq mile |
| walkability_index | County-level walkability score | EPA Smart Location Database | 2010-2021 | index |

## Economic Factors Data

| Variable Name | Description | Source | Years Available | Units |
|---------------|-------------|--------|-----------------|-------|
| absolute_upward_mobility | Expected income rank for children from low-income families | Opportunity Insights | 2000-2018 | percentile |
| economic_distress_index | Composite index of economic distress | Appalachian Regional Commission | 2000-2023 | index |
| economic_typology | County economic typology | USDA Economic Research Service | 2000-2023 | category |
| employment_volatility_index | Index of employment stability/volatility | USDA Economic Research Service | 2000-2023 | index |
| income_inequality_ratio | Ratio of income at 80th percentile to income at 20th percentile | American Community Survey | 2010-2023 | ratio |
| income_mobility_index | Measure of intergenerational economic mobility | Opportunity Insights | 2000-2018 | index |
| job_density_index | Number of jobs within typical commute distance | Opportunity Insights | 2000-2018 | index |
| job_growth_rate | Annual job growth rate | Bureau of Labor Statistics | 2000-2023 | percent |
| mean_commute_distance | Average commute distance | Opportunity Insights | 2000-2018 | miles |
| persistent_child_poverty_county | Flag for counties with persistent child poverty | USDA Economic Research Service | 2000-2023 | binary |
| persistent_poverty_county | Flag for counties with persistent poverty | USDA Economic Research Service | 2000-2023 | binary |

## Educational Resources & Quality Data

| Variable Name | Description | Source | Years Available | Units |
|---------------|-------------|--------|-----------------|-------|
| educational_opportunity_index | Measure of educational opportunity | Stanford Education Data Archive | 2009-2018 | index |
| high_school_graduation_rate | Four-year high school graduation rate | National Center for Education Statistics | 2000-2022 | percent |
| math_achievement_gap | Achievement gap in math scores by race/ethnicity | Stanford Education Data Archive | 2009-2018 | z-score |
| per_pupil_expenditure | Per-pupil expenditure in public schools | National Center for Education Statistics | 2000-2022 | dollars |
| preschool_enrollment_rate | Percentage of 3-4 year-olds enrolled in preschool | National Center for Education Statistics | 2000-2022 | percent |
| reading_achievement_gap | Achievement gap in reading scores by race/ethnicity | Stanford Education Data Archive | 2009-2018 | z-score |
| school_funding_equity | Ratio of funding in high-poverty vs. low-poverty districts | National Center for Education Statistics | 2000-2022 | ratio |
| student_teacher_ratio | Student-to-teacher ratio in public schools | National Center for Education Statistics | 2000-2022 | ratio |

## Environmental Health Data

| Variable Name | Description | Source | Years Available | Units |
|---------------|-------------|--------|-----------------|-------|
| air_quality_days_unhealthy | Number of days with unhealthy air quality | EPA Air Quality System | 2000-2023 | days |
| air_toxics_cancer_risk | Air toxics cancer risk | EPA Air Quality System | 2000-2023 | per million |
| diesel_pm_concentration | Diesel particulate matter concentration | EPA Air Quality System | 2000-2023 | μg/m³ |
| drought_severity_index | Average drought severity index | CDC Environmental Public Health Tracking | 2002-2022 | index |
| extreme_heat_days | Annual number of extreme heat days | CDC Environmental Public Health Tracking | 2002-2022 | days |
| extreme_precipitation_events | Annual number of extreme precipitation events | CDC Environmental Public Health Tracking | 2002-2022 | count |
| lead_exposure_risk_index | Index of lead exposure risk | CDC Environmental Public Health Tracking | 2002-2022 | index |
| lead_paint_indicator | Percentage of housing units built pre-1960 | EPA EJSCREEN | 2016-2023 | percent |
| ozone_days_exceeding | Days exceeding ozone standards | EPA Air Quality System | 2000-2023 | days |
| pm25_annual_mean | Annual mean PM2.5 concentration | EPA Air Quality System | 2000-2023 | μg/m³ |
| proximity_to_hazardous_waste | Count of hazardous waste facilities within 5km | EPA EJSCREEN | 2016-2023 | count |
| proximity_to_npl_sites | Proximity to National Priorities List (Superfund) sites | EPA EJSCREEN | 2016-2023 | index |
| public_water_violations | Number of public water system violations | CDC Environmental Public Health Tracking | 2002-2022 | count |
| respiratory_hazard_index | Respiratory hazard index from air pollutants | EPA Air Quality System | 2000-2023 | index |
| traffic_proximity | Count of vehicles at major roads within 500m | EPA EJSCREEN | 2016-2023 | count |
| wastewater_discharge | Toxicity-weighted concentrations in stream reach | EPA EJSCREEN | 2016-2023 | concentration |

## Food Environment & Access Data

| Variable Name | Description | Source | Years Available | Units |
|---------------|-------------|--------|-----------------|-------|
| child_food_insecurity_rate | Percentage of children experiencing food insecurity | Feeding America Map the Meal Gap | 2009-2022 | percent |
| children_low_access_pct | Percentage of children with low access to a grocery store | USDA Food Environment Atlas | 2010-2022 | percent |
| convenience_stores_per_1000 | Number of convenience stores per 1,000 population | USDA Food Environment Atlas | 2010-2022 | count/1000 |
| farmers_markets_per_1000 | Farmers markets per 1,000 population | USDA Food Environment Atlas | 2010-2022 | count/1000 |
| fast_food_restaurants_per_1000 | Fast food restaurants per 1,000 population | USDA Food Environment Atlas | 2010-2022 | count/1000 |
| food_insecurity_cost_per_person | Average cost per person to meet food needs | Feeding America Map the Meal Gap | 2009-2022 | dollars |
| food_insecurity_rate | Percentage of overall population experiencing food insecurity | Feeding America Map the Meal Gap | 2009-2022 | percent |
| full_service_restaurants_per_1000 | Full-service restaurants per 1,000 population | USDA Food Environment Atlas | 2010-2022 | count/1000 |
| grocery_stores_per_1000 | Number of supermarkets and grocery stores per 1,000 population | USDA Food Environment Atlas | 2010-2022 | count/1000 |
| low_income_low_access_pct | Percentage of population that is low income and has low access to a grocery store | USDA Food Environment Atlas | 2010-2022 | percent |
| seniors_low_access_pct | Percentage of seniors with low access to a grocery store | USDA Food Environment Atlas | 2010-2022 | percent |
| snap_authorized_stores_per_1000 | SNAP-authorized retailers per 1,000 population | USDA Food Environment Atlas | 2010-2022 | count/1000 |
| snap_benefits_redemption_per_capita | SNAP benefits redemption per capita | USDA Food Environment Atlas | 2010-2022 | dollars |
| supercenters_per_1000 | Number of supercenter and club stores per 1,000 population | USDA Food Environment Atlas | 2010-2022 | count/1000 |
| wic_authorized_stores_per_1000 | WIC-authorized stores per 1,000 population | USDA Food Environment Atlas | 2010-2022 | count/1000 |

## Healthcare Access Data

| Variable Name | Description | Source | Years Available | Units |
|---------------|-------------|--------|-----------------|-------|
| ambulatory_care_sensitive_conditions | Rate of hospitalization for ambulatory care sensitive conditions | CMS Geographic Variation Public Use File | 2007-2021 | rate |
| dentists_per_100k | Dentists per 100,000 population | HRSA Area Health Resources Files | 2000-2023 | count/100k |
| fqhc_access_pct | Percentage of population with access to Federally Qualified Health Centers | HRSA Area Health Resources Files | 2000-2023 | percent |
| hospital_beds_per_1000 | Hospital beds per 1,000 population | HRSA Area Health Resources Files | 2000-2023 | count/1000 |
| medicare_spending_per_beneficiary | Medicare spending per beneficiary | CMS Geographic Variation Public Use File | 2007-2021 | dollars |
| mental_health_providers_per_100k | Mental health providers per 100,000 population | HRSA Area Health Resources Files | 2000-2023 | count/100k |
| pharmacies_per_100k | Pharmacies per 100,000 population | HRSA Area Health Resources Files | 2000-2023 | count/100k |
| preventable_hospital_stays | Preventable hospital stays per 100,000 Medicare enrollees | HRSA Area Health Resources Files | 2000-2023 | count/100k |
| preventive_services_pct | Percentage of Medicare beneficiaries receiving preventive services | CMS Geographic Variation Public Use File | 2007-2021 | percent |
| primary_care_physicians_per_100k | Primary care physicians per 100,000 population | HRSA Area Health Resources Files | 2000-2023 | count/100k |

## Housing Data

| Variable Name | Description | Source | Years Available | Units |
|---------------|-------------|--------|-----------------|-------|
| eviction_filing_rate | Number of eviction filings per 100 renter homes | Eviction Lab | 2000-2018 | rate |
| eviction_rate | Number of evictions per 100 renter homes | Eviction Lab | 2000-2018 | rate |
| foreclosure_rate | Foreclosures per 1,000 housing units | Federal Reserve HMDA | 2007-2023 | rate |
| high_cost_loans_pct | Percentage of loans that are high-cost | Federal Reserve HMDA | 2007-2023 | percent |
| housing_problems_pct | Percentage of households with at least one housing problem | HUD CHAS | 2006-2020 | percent |
| low_income_renters_affordable_units_ratio | Ratio of affordable units to low-income renters | HUD CHAS | 2006-2020 | ratio |
| mortgage_denial_rate | Percentage of mortgage applications denied | Federal Reserve HMDA | 2007-2023 | percent |
| overcrowded_housing_pct | Percentage of housing units with >1 person per room | HUD CHAS | 2006-2020 | percent |
| rent_burden_pct | Percentage of income spent on rent (median) | Eviction Lab | 2000-2018 | percent |
| severely_cost_burdened_owners_pct | Percentage of owner households spending >50% of income on housing | HUD CHAS | 2006-2020 | percent |
| severely_cost_burdened_renters_pct | Percentage of renter households spending >50% of income on housing | HUD CHAS | 2006-2020 | percent |

## Social Cohesion & Capital Data

| Variable Name | Description | Source | Years Available | Units |
|---------------|-------------|--------|-----------------|-------|
| nonprofit_organizations_per_10k | Nonprofit organizations per 10,000 population | County Health Rankings | 2014-2023 | count/10k |
| political_competition_index | Index measuring political competition | MIT Election Data and Science Lab | 2000-2022 | index |
| religious_congregation_rate | Religious congregations per 10,000 population | County Health Rankings | 2014-2023 | count/10k |
| social_association_rate | Social associations per 10,000 population | County Health Rankings | 2014-2023 | count/10k |
| voter_registration_rate | Voter registration as percentage of eligible population | MIT Election Data and Science Lab | 2000-2022 | percent |
| voter_turnout_rate | Voter turnout rate in general elections | MIT Election Data and Science Lab | 2000-2022 | percent |

## Transportation Data

| Variable Name | Description | Source | Years Available | Units |
|---------------|-------------|--------|-----------------|-------|
| public_transit_trips_per_capita | Public transit trips per capita | National Transit Database | 2000-2022 | count |
| transit_access_jobs | Number of jobs accessible by transit within 30 minutes | All Transit Database | 2012-2022 | count |
| transit_connectivity_index | Measure of transit connectivity | All Transit Database | 2012-2022 | index |
| transit_performance_index | Composite measure of transit performance | All Transit Database | 2012-2022 | index |
| transportation_cost_burden_pct | Transportation costs as percentage of household income | National Household Travel Survey | 2001-2017 | percent |
| vehicle_miles_traveled_per_capita | Annual vehicle miles traveled per capita | National Household Travel Survey | 2001-2017 | miles |
| zero_vehicle_households_pct | Percentage of households with no vehicles | American Community Survey | 2009-2023 | percent |

## Data Consistency and Quality

Each variable includes metadata about its:
- Data quality (direct, interpolated, extrapolated, simulated)
- Data source (original source of information)
- Data vintage (original year or time period of collection)

The pipeline attempts to obtain direct data from authoritative sources when available, and uses interpolation, extrapolation, or simulation (when explicitly allowed) to fill gaps in time series data.