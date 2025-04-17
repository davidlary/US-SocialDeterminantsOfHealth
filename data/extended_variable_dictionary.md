# Extended SDOH Variable Dictionary

This document details all new variables added to the Social Determinants of Health county-level dataset, organized by domain.

## 1. Food Environment & Access

| Variable | Units | Description | Source | Years | Notes |
|----------|-------|-------------|--------|-------|-------|
| `grocery_stores_per_1000` | count/1000 | Number of supermarkets and grocery stores per 1,000 population | USDA Food Environment Atlas | 2010-2022 | Updated annually |
| `supercenters_per_1000` | count/1000 | Number of supercenter and club stores per 1,000 population | USDA Food Environment Atlas | 2010-2022 | Updated annually |
| `convenience_stores_per_1000` | count/1000 | Number of convenience stores per 1,000 population | USDA Food Environment Atlas | 2010-2022 | Updated annually |
| `snap_authorized_stores_per_1000` | count/1000 | SNAP-authorized retailers per 1,000 population | USDA Food Environment Atlas | 2010-2022 | Updated annually |
| `wic_authorized_stores_per_1000` | count/1000 | WIC-authorized stores per 1,000 population | USDA Food Environment Atlas | 2010-2022 | Updated annually |
| `farmers_markets_per_1000` | count/1000 | Farmers markets per 1,000 population | USDA Food Environment Atlas | 2010-2022 | Updated annually |
| `fast_food_restaurants_per_1000` | count/1000 | Fast food restaurants per 1,000 population | USDA Food Environment Atlas | 2010-2022 | Updated annually |
| `full_service_restaurants_per_1000` | count/1000 | Full-service restaurants per 1,000 population | USDA Food Environment Atlas | 2010-2022 | Updated annually |
| `low_income_low_access_pct` | percentage | Percentage of population that is low income and has low access to a grocery store | USDA Food Environment Atlas | 2010-2022 | Range: 0-100 |
| `children_low_access_pct` | percentage | Percentage of children with low access to a grocery store | USDA Food Environment Atlas | 2010-2022 | Range: 0-100 |
| `seniors_low_access_pct` | percentage | Percentage of seniors with low access to a grocery store | USDA Food Environment Atlas | 2010-2022 | Range: 0-100 |
| `snap_benefits_redemption_per_capita` | dollars | SNAP benefits redemption per capita | USDA Food Environment Atlas | 2010-2022 | Not adjusted for inflation |
| `food_insecurity_rate` | percentage | Percentage of overall population experiencing food insecurity | Feeding America Map the Meal Gap | 2009-2022 | Range: 0-100, updated annually |
| `child_food_insecurity_rate` | percentage | Percentage of children experiencing food insecurity | Feeding America Map the Meal Gap | 2009-2022 | Range: 0-100, updated annually |
| `food_insecurity_cost_per_person` | dollars | Average cost per person to meet food needs | Feeding America Map the Meal Gap | 2009-2022 | Not adjusted for inflation |

## 2. Built Environment

| Variable | Units | Description | Source | Years | Notes |
|----------|-------|-------------|--------|-------|-------|
| `walkability_index` | index | County-level walkability score | EPA Smart Location Database | 2010, 2013, 2021 | Higher values indicate more walkable areas |
| `land_use_diversity` | index | Mix of land uses (entropy index) | EPA Smart Location Database | 2010, 2013, 2021 | Range: 0-1, higher values indicate more diversity |
| `street_intersection_density` | count/sq mile | Number of intersections per square mile | EPA Smart Location Database | 2010, 2013, 2021 | Measure of street connectivity |
| `employment_access_index` | index | Access to employment centers | EPA Smart Location Database | 2010, 2013, 2021 | Higher values indicate better access |
| `transit_service_density` | routes/sq mile | Transit routes and stops per square mile | EPA Smart Location Database | 2010, 2013, 2021 | Measure of transit service |
| `housing_density` | units/acre | Housing units per acre of developed land | EPA Smart Location Database | 2010, 2013, 2021 | |
| `park_access_pct` | percentage | Percentage of residents living within 10-minute walk of a park | Trust for Public Land ParkScore | 2012-2022 | Range: 0-100, updated annually |
| `park_acres_per_1000` | acres/1000 | Park acres per 1,000 residents | Trust for Public Land ParkScore | 2012-2022 | Updated annually |
| `park_spending_per_capita` | dollars | Park system spending per resident | Trust for Public Land ParkScore | 2012-2022 | Not adjusted for inflation |
| `playgrounds_per_10000` | count/10000 | Playgrounds per 10,000 residents | Trust for Public Land ParkScore | 2012-2022 | Updated annually |

## 3. Environmental Health

| Variable | Units | Description | Source | Years | Notes |
|----------|-------|-------------|--------|-------|-------|
| `air_quality_days_unhealthy` | count | Number of days with unhealthy air quality | EPA Air Quality System | 2000-2023 | Updated annually |
| `pm25_annual_mean` | μg/m³ | Annual mean PM2.5 concentration | EPA Air Quality System | 2000-2023 | Updated annually |
| `ozone_days_exceeding` | count | Days exceeding ozone standards | EPA Air Quality System | 2000-2023 | Updated annually |
| `air_toxics_cancer_risk` | per million | Air toxics cancer risk | EPA Air Quality System | 2000-2023 | Lifetime risk per million people |
| `diesel_pm_concentration` | μg/m³ | Diesel particulate matter concentration | EPA Air Quality System | 2000-2023 | Updated annually |
| `respiratory_hazard_index` | index | Respiratory hazard index from air pollutants | EPA Air Quality System | 2000-2023 | Values >1 indicate potential hazard |
| `extreme_heat_days` | count | Annual number of extreme heat days | CDC Environmental Public Health Tracking | 2002-2022 | Updated annually |
| `extreme_precipitation_events` | count | Annual number of extreme precipitation events | CDC Environmental Public Health Tracking | 2002-2022 | Updated annually |
| `drought_severity_index` | index | Average drought severity index | CDC Environmental Public Health Tracking | 2002-2022 | Higher values indicate more severe drought |
| `public_water_violations` | count | Number of public water system violations | CDC Environmental Public Health Tracking | 2002-2022 | Updated annually |
| `lead_exposure_risk_index` | index | Index of lead exposure risk | CDC Environmental Public Health Tracking | 2002-2022 | Based on housing age and poverty |
| `proximity_to_hazardous_waste` | count | Count of hazardous waste facilities within 5km | EPA EJSCREEN | 2016-2023 | Updated annually |
| `proximity_to_npl_sites` | index | Proximity to National Priorities List (Superfund) sites | EPA EJSCREEN | 2016-2023 | Higher values indicate closer proximity |
| `wastewater_discharge` | toxicity-weighted concentration | Toxicity-weighted concentrations in stream reach | EPA EJSCREEN | 2016-2023 | Updated annually |
| `traffic_proximity` | count | Count of vehicles at major roads within 500m | EPA EJSCREEN | 2016-2023 | Updated annually |
| `lead_paint_indicator` | percentage | Percentage of housing units built pre-1960 | EPA EJSCREEN | 2016-2023 | Range: 0-100, proxy for lead paint risk |

## 4. Economic Factors

| Variable | Units | Description | Source | Years | Notes |
|----------|-------|-------------|--------|-------|-------|
| `employment_volatility_index` | index | Index of employment stability/volatility | USDA Economic Research Service | 2000-2023 | Higher values indicate more volatility |
| `job_growth_rate` | percentage | Annual job growth rate | Bureau of Labor Statistics | 2000-2023 | Updated annually |
| `income_inequality_ratio` | ratio | Ratio of income at 80th percentile to income at 20th percentile | American Community Survey | 2010-2023 | Higher values indicate more inequality |
| `economic_typology` | category | County economic typology | USDA Economic Research Service | 2000-2023 | Farming, manufacturing, etc. |
| `persistent_poverty_county` | binary | Flag for counties with persistent poverty | USDA Economic Research Service | 2000-2023 | 20%+ poverty rate for 30+ years |
| `persistent_child_poverty_county` | binary | Flag for counties with persistent child poverty | USDA Economic Research Service | 2000-2023 | 20%+ child poverty rate for 30+ years |
| `economic_distress_index` | index | Composite index of economic distress | Appalachian Regional Commission | 2000-2023 | Higher values indicate more distress |
| `income_mobility_index` | index | Measure of intergenerational economic mobility | Opportunity Insights | 2000-2018 | Higher values indicate more mobility |
| `absolute_upward_mobility` | percentile | Expected income rank for children from low-income families | Opportunity Insights | 2000-2018 | |
| `mean_commute_distance` | miles | Average commute distance | Opportunity Insights | 2000-2018 | |
| `job_density_index` | index | Number of jobs within typical commute distance | Opportunity Insights | 2000-2018 | Higher values indicate more job access |

## 5. Housing

| Variable | Units | Description | Source | Years | Notes |
|----------|-------|-------------|--------|-------|-------|
| `severely_cost_burdened_owners_pct` | percentage | Percentage of owner households spending >50% of income on housing | HUD CHAS | 2006-2020 | Range: 0-100, updated annually |
| `severely_cost_burdened_renters_pct` | percentage | Percentage of renter households spending >50% of income on housing | HUD CHAS | 2006-2020 | Range: 0-100, updated annually |
| `low_income_renters_affordable_units_ratio` | ratio | Ratio of affordable units to low-income renters | HUD CHAS | 2006-2020 | Values <1 indicate shortage |
| `housing_problems_pct` | percentage | Percentage of households with at least one housing problem | HUD CHAS | 2006-2020 | Range: 0-100, updated annually |
| `overcrowded_housing_pct` | percentage | Percentage of housing units with >1 person per room | HUD CHAS | 2006-2020 | Range: 0-100, updated annually |
| `eviction_rate` | rate | Number of evictions per 100 renter homes | Eviction Lab | 2000-2018 | Updated annually |
| `eviction_filing_rate` | rate | Number of eviction filings per 100 renter homes | Eviction Lab | 2000-2018 | Updated annually |
| `rent_burden_pct` | percentage | Percentage of income spent on rent (median) | Eviction Lab | 2000-2018 | Range: 0-100, updated annually |
| `mortgage_denial_rate` | percentage | Percentage of mortgage applications denied | Federal Reserve HMDA | 2007-2023 | Range: 0-100, updated annually |
| `high_cost_loans_pct` | percentage | Percentage of loans that are high-cost | Federal Reserve HMDA | 2007-2023 | Range: 0-100, updated annually |
| `foreclosure_rate` | rate | Foreclosures per 1,000 housing units | Federal Reserve HMDA | 2007-2023 | Updated annually |

## 6. Healthcare Access

| Variable | Units | Description | Source | Years | Notes |
|----------|-------|-------------|--------|-------|-------|
| `primary_care_physicians_per_100k` | count/100k | Primary care physicians per 100,000 population | HRSA Area Health Resources Files | 2000-2023 | Updated annually |
| `mental_health_providers_per_100k` | count/100k | Mental health providers per 100,000 population | HRSA Area Health Resources Files | 2000-2023 | Updated annually |
| `dentists_per_100k` | count/100k | Dentists per 100,000 population | HRSA Area Health Resources Files | 2000-2023 | Updated annually |
| `hospital_beds_per_1000` | count/1000 | Hospital beds per 1,000 population | HRSA Area Health Resources Files | 2000-2023 | Updated annually |
| `fqhc_access_pct` | percentage | Percentage of population with access to Federally Qualified Health Centers | HRSA Area Health Resources Files | 2000-2023 | Range: 0-100, updated annually |
| `pharmacies_per_100k` | count/100k | Pharmacies per 100,000 population | HRSA Area Health Resources Files | 2000-2023 | Updated annually |
| `preventable_hospital_stays` | count/100k | Preventable hospital stays per 100,000 Medicare enrollees | HRSA Area Health Resources Files | 2000-2023 | Updated annually |
| `medicare_spending_per_beneficiary` | dollars | Medicare spending per beneficiary | CMS Geographic Variation Public Use File | 2007-2021 | Not adjusted for inflation |
| `preventive_services_pct` | percentage | Percentage of Medicare beneficiaries receiving preventive services | CMS Geographic Variation Public Use File | 2007-2021 | Range: 0-100, updated annually |
| `ambulatory_care_sensitive_conditions` | rate | Rate of hospitalization for ambulatory care sensitive conditions | CMS Geographic Variation Public Use File | 2007-2021 | Conditions manageable in outpatient settings |

## 7. Transportation

| Variable | Units | Description | Source | Years | Notes |
|----------|-------|-------------|--------|-------|-------|
| `vehicle_miles_traveled_per_capita` | miles | Annual vehicle miles traveled per capita | National Household Travel Survey | 2001, 2009, 2017 | Requires interpolation between survey years |
| `transportation_cost_burden_pct` | percentage | Transportation costs as percentage of household income | National Household Travel Survey | 2001, 2009, 2017 | Range: 0-100, requires interpolation |
| `zero_vehicle_households_pct` | percentage | Percentage of households with no vehicles | American Community Survey | 2009-2023 | Range: 0-100, updated annually |
| `public_transit_trips_per_capita` | count | Public transit trips per capita | National Transit Database | 2000-2022 | Updated annually |
| `transit_connectivity_index` | index | Measure of transit connectivity | All Transit Database | 2012-2022 | Higher values indicate better connectivity |
| `transit_access_jobs` | count | Number of jobs accessible by transit within 30 minutes | All Transit Database | 2012-2022 | Updated annually |
| `transit_performance_index` | index | Composite measure of transit performance | All Transit Database | 2012-2022 | Higher values indicate better performance |

## 8. Social Cohesion & Capital

| Variable | Units | Description | Source | Years | Notes |
|----------|-------|-------------|--------|-------|-------|
| `voter_turnout_rate` | percentage | Voter turnout rate in general elections | MIT Election Data and Science Lab | 2000-2022 | Range: 0-100, available for election years |
| `voter_registration_rate` | percentage | Voter registration as percentage of eligible population | MIT Election Data and Science Lab | 2000-2022 | Range: 0-100, available for election years |
| `political_competition_index` | index | Index measuring political competition | MIT Election Data and Science Lab | 2000-2022 | Higher values indicate more competition |
| `social_association_rate` | count/10k | Social associations per 10,000 population | County Health Rankings | 2014-2023 | Updated annually |
| `religious_congregation_rate` | count/10k | Religious congregations per 10,000 population | County Health Rankings | 2014-2023 | Updated annually |
| `nonprofit_organizations_per_10k` | count/10k | Nonprofit organizations per 10,000 population | County Health Rankings | 2014-2023 | Updated annually |

## 9. Crime & Safety

| Variable | Units | Description | Source | Years | Notes |
|----------|-------|-------------|--------|-------|-------|
| `violent_crime_rate` | count/100k | Violent crimes per 100,000 population | FBI Uniform Crime Reports | 2000-2021 | Updated annually |
| `property_crime_rate` | count/100k | Property crimes per 100,000 population | FBI Uniform Crime Reports | 2000-2021 | Updated annually |
| `homicide_rate` | count/100k | Homicides per 100,000 population | FBI Uniform Crime Reports | 2000-2021 | Updated annually |
| `jail_incarceration_rate` | count/100k | County jail inmates per 100,000 population | Bureau of Justice Statistics | 2000-2020 | Updated annually |
| `pretrial_detention_rate` | count/100k | Pretrial detainees per 100,000 population | Bureau of Justice Statistics | 2000-2020 | Updated annually |

## 10. Educational Resources & Quality

| Variable | Units | Description | Source | Years | Notes |
|----------|-------|-------------|--------|-------|-------|
| `student_teacher_ratio` | ratio | Student-to-teacher ratio in public schools | National Center for Education Statistics | 2000-2022 | Updated annually |
| `per_pupil_expenditure` | dollars | Per-pupil expenditure in public schools | National Center for Education Statistics | 2000-2022 | Not adjusted for inflation |
| `high_school_graduation_rate` | percentage | Four-year high school graduation rate | National Center for Education Statistics | 2000-2022 | Range: 0-100, updated annually |
| `preschool_enrollment_rate` | percentage | Percentage of 3-4 year-olds enrolled in preschool | National Center for Education Statistics | 2000-2022 | Range: 0-100, updated annually |
| `school_funding_equity` | ratio | Ratio of funding in high-poverty vs. low-poverty districts | National Center for Education Statistics | 2000-2022 | Values <1 indicate inequity |
| `reading_achievement_gap` | z-score | Achievement gap in reading scores by race/ethnicity | Stanford Education Data Archive | 2009-2018 | Standard deviation units |
| `math_achievement_gap` | z-score | Achievement gap in math scores by race/ethnicity | Stanford Education Data Archive | 2009-2018 | Standard deviation units |
| `educational_opportunity_index` | index | Measure of educational opportunity | Stanford Education Data Archive | 2009-2018 | Higher values indicate more opportunity |

## 11. Climate & Natural Disaster Risk

| Variable | Units | Description | Source | Years | Notes |
|----------|-------|-------------|--------|-------|-------|
| `extreme_heat_days` | count | Annual number of days with temperature above 95°F/35°C | NOAA National Centers for Environmental Information | 2000-2023 | Updated annually |
| `extreme_precipitation_events` | count | Annual number of days with precipitation > 2 inches | NOAA National Centers for Environmental Information | 2000-2023 | Updated annually |
| `drought_severity_index` | index | Palmer Drought Severity Index | US Drought Monitor | 2000-2023 | Range: -6 to +6, negative is drought |
| `flood_risk_index` | index | Flood risk score | FEMA National Risk Index | 2000-2023 | Range: 0-10, higher values indicate greater risk |
| `hurricane_risk_index` | index | Hurricane risk score | NOAA Hurricane Center | 2000-2023 | Range: 0-10, higher values indicate greater risk |
| `wildfire_risk_index` | index | Wildfire risk score | USFS Wildfire Risk Assessment | 2000-2023 | Range: 0-10, higher values indicate greater risk |
| `disaster_declarations` | count | FEMA disaster declarations count | FEMA Disaster Declarations | 2000-2023 | Updated annually |
| `climate_vulnerability_index` | index | Composite climate vulnerability score | Derived composite index | 2000-2023 | Range: 0-10, higher values indicate greater vulnerability |

## 12. Mental Health & Substance Use

| Variable | Units | Description | Source | Years | Notes |
|----------|-------|-------------|--------|-------|-------|
| `treatment_facilities_per_100k` | count/100k | Substance use treatment facilities per 100,000 population | SAMHSA Treatment Locator | 2010-2023 | Updated annually |
| `substance_abuse_treatment_capacity_per_100k` | beds/100k | Substance abuse treatment bed capacity per 100,000 population | SAMHSA National Survey of Substance Abuse Treatment Services | 2010-2023 | Updated annually |
| `mental_health_provider_ratio` | persons/provider | Population per mental health provider | County Health Rankings | 2010-2023 | Lower values indicate better access |
| `medication_assisted_treatment_facilities_per_100k` | count/100k | Facilities offering MAT per 100,000 population | SAMHSA OTP Directory | 2010-2023 | Updated annually |
| `recovery_support_services_per_100k` | count/100k | Recovery support services per 100,000 population | SAMHSA Recovery Support Services | 2010-2023 | Updated annually |
| `opioid_treatment_programs_per_100k` | count/100k | Opioid treatment programs per 100,000 population | SAMHSA OTP Directory | 2010-2023 | Updated annually |
| `mental_health_treatment_facilities_per_100k` | count/100k | Mental health treatment facilities per 100,000 population | SAMHSA Mental Health Facilities Survey | 2010-2023 | Updated annually |
| `mental_health_provider_shortage_score` | index | Mental health provider shortage score | HRSA Shortage Areas | 2010-2023 | Range: 0-10, higher values indicate greater shortage |

## 13. Digital Access & Broadband

| Variable | Units | Description | Source | Years | Notes |
|----------|-------|-------------|--------|-------|-------|
| `broadband_availability_pct` | percentage | Percentage of population with access to 25/3 Mbps broadband | FCC Form 477 | 2015-2023 | Range: 0-100, updated annually |
| `broadband_subscription_pct` | percentage | Percentage of households with broadband subscription | American Community Survey | 2015-2023 | Range: 0-100, updated annually |
| `digital_divide_index` | index | Digital divide score | Purdue Digital Divide Index | 2015-2023 | Range: 0-100, higher values indicate greater divide |
| `broadband_competition_index` | index | Broadband provider competition score | FCC Competition Report | 2015-2023 | Range: 0-10, higher values indicate more competition |
| `fixed_broadband_avg_speed_mbps` | Mbps | Average fixed broadband download speed | Ookla Speedtest | 2015-2023 | Updated annually |
| `mobile_broadband_avg_speed_mbps` | Mbps | Average mobile broadband download speed | Ookla Speedtest | 2015-2023 | Updated annually |
| `digital_literacy_score` | index | Digital literacy score | NTIA Digital Nation | 2015-2023 | Range: 0-10, higher values indicate more literacy |
| `telehealth_access_index` | index | Telehealth access score | Connected Health | 2015-2023 | Range: 0-10, higher values indicate better access |
| `avg_monthly_broadband_cost` | dollars | Average monthly broadband subscription cost | BroadbandNow | 2015-2023 | Not adjusted for inflation |

## Data Quality Flags

Each variable will include the same data quality flags as the original pipeline:

- `*_data_quality`: Source of data (direct, estimate, harmonized, interpolated, etc.)
- `*_interpolated`: Flag for interpolated values
- `*_missing_data_interpolated`: Flag for values interpolated from missing data
- `*_data_source`: Original source of the data
- `*_data_vintage`: Year and specific collection the data came from