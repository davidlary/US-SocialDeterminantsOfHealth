# Crime & Safety Data

This directory contains county-level data related to crime and safety factors affecting social determinants of health.

## Data Sources

### FBI Uniform Crime Reports (UCR)
The FBI's Uniform Crime Reports provide standardized offense statistics from approximately 18,000 law enforcement agencies nationwide. The dataset retrieves county-level crime rates for:

- Violent crime rate (violent_crime_rate): Violent crimes per 100,000 population
- Property crime rate (property_crime_rate): Property crimes per 100,000 population  
- Homicide rate (homicide_rate): Homicides per 100,000 population

Data is available for years 2000-2021, with the collection methodology changing in 2021 with the transition to the National Incident-Based Reporting System (NIBRS).

### Bureau of Justice Statistics (BJS)
The Bureau of Justice Statistics provides county-level jail incarceration data:

- Jail incarceration rate (jail_incarceration_rate): County jail inmates per 100,000 population
- Pretrial detention rate (pretrial_detention_rate): Pretrial detainees per 100,000 population

Data is available for years 2000-2020.

## Data Processing

The fetch_crime_data.r script handles:
1. Downloading and caching data from these sources
2. Cleaning and standardizing county-level data
3. Interpolating missing values if needed
4. Generating simulated data when real data is unavailable and simulation is allowed
5. Adding data quality flags to indicate source and quality of each data point

## Contact

For questions or issues related to this data, please contact David Lary (davidlary@me.com).