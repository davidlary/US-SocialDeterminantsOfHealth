# Traffic Safety Data

This document provides detailed information about the traffic safety variables integrated into the Social Determinants of Health (SDOH) database.

## Overview

Traffic safety data is a critical component of the SDOH database, providing county-level metrics related to transportation safety and risk. These variables help researchers and policymakers understand and address the public health impacts of transportation systems and policies.

The integration of traffic safety variables enables cross-domain analysis with other social determinants, allowing for more comprehensive understanding of factors affecting community health and wellbeing.

## Variables

| Variable Name | Description | Type | Years Available | Source |
|-------------|-------------|------|----------------|--------|
| `traffic_fatalities` | Total traffic fatalities in the county | numeric_count | 1975 - 2022 | NHTSA FARS |
| `traffic_fatality_rate` | Traffic fatalities per 100,000 population | numeric_rate | 1975 - 2022 | NHTSA FARS |
| `pedestrian_fatalities` | Pedestrian traffic fatalities | numeric_count | 1975 - 2022 | NHTSA FARS |
| `pedestrian_fatality_rate` | Pedestrian fatalities per 100,000 population | numeric_rate | 1975 - 2022 | NHTSA FARS |
| `bicycle_fatalities` | Bicycle traffic fatalities | numeric_count | 1975 - 2022 | NHTSA FARS |
| `bicycle_fatality_rate` | Bicycle fatalities per 100,000 population | numeric_rate | 1975 - 2022 | NHTSA FARS |
| `motorcycle_fatalities` | Motorcycle traffic fatalities | numeric_count | 1975 - 2022 | NHTSA FARS |
| `motorcycle_fatality_rate` | Motorcycle fatalities per 100,000 population | numeric_rate | 1975 - 2022 | NHTSA FARS |
| `alcohol_impaired_fatalities` | Alcohol-impaired driving fatalities | numeric_count | 1975 - 2022 | NHTSA FARS |
| `alcohol_impaired_fatality_rate` | Alcohol-impaired fatalities per 100,000 population | numeric_rate | 1975 - 2022 | NHTSA FARS |
| `speeding_related_fatalities` | Speeding-related traffic fatalities | numeric_count | 1975 - 2022 | NHTSA FARS |
| `speeding_related_fatality_rate` | Speeding-related fatalities per 100,000 population | numeric_rate | 1975 - 2022 | NHTSA FARS |

## Data Sources

### Primary Data Source: FARS (Fatality Analysis Reporting System)

FARS is a nationwide census providing data on fatal injuries in motor vehicle traffic crashes across the United States.

- **Source Organization**: National Highway Traffic Safety Administration (NHTSA)
- **Source Website**: https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars
- **Years Available**: 1975-present (annually updated)
- **Geographic Coverage**: All U.S. counties
- **Update Frequency**: Annual

### Secondary Data Source: CDC WONDER

The CDC WONDER Mortality Data provides supplementary mortality statistics related to traffic safety.

- **Source Organization**: Centers for Disease Control and Prevention (CDC)
- **Source Website**: https://wonder.cdc.gov/
- **Years Available**: 1999-present
- **Geographic Coverage**: All U.S. counties
- **Update Frequency**: Annual

## Methodology

### Data Collection and Processing

1. **Raw Data Collection**: Traffic fatality data is obtained from FARS, which collects data from state agencies, police crash reports, and other sources.

2. **County-Level Aggregation**: Individual crash records are aggregated to the county level using the county FIPS code provided in the FARS data.

3. **Variable Calculation**:
   - Count variables represent the total number of fatalities in each category.
   - Rate variables are calculated using the formula: (fatality count / county population) * 100,000
   - Population denominators come from Census Bureau annual population estimates.

4. **Data Quality Indicators**: Each data point includes a quality indicator:
   - `direct`: Data directly observed/reported for that year and county
   - `interpolated`: Data estimated using temporal interpolation between known data points
   - `estimated`: Data estimated using related variables or spatial methods
   - `missing`: Data not available

### Temporal Interpolation

For counties with gaps in data across years, temporal interpolation is performed:

1. **Short Gaps (1-2 years)**: Linear interpolation between known data points
2. **Medium Gaps (3-5 years)**: Pattern-based interpolation using trends from similar counties
3. **Long Gaps (6+ years)**: Statistical modeling based on county characteristics and state-level trends

### Data Limitations

1. **Small County Variability**: Counties with small populations may show high year-to-year variability in rates due to small numbers.

2. **Reporting Consistency**: Changes in reporting practices over time may affect trend analysis, particularly for pre-2000 data.

3. **Under-reporting**: Some crashes may be unreported or miscategorized, particularly for non-fatal injuries (not included in this dataset).

4. **Definitional Changes**: NHTSA has modified some variable definitions over time, which are harmonized in this dataset but may affect comparability.

## Integration with SDOH Database

Traffic safety data is fully integrated with other SDOH domains, enabling:

1. **Cross-Domain Analysis**: Correlating traffic safety with socioeconomic factors, healthcare access, etc.

2. **Spatio-Temporal Visualization**: County-level maps showing traffic safety trends over time

3. **Policy Impact Assessment**: Evaluating the impact of traffic safety policies and interventions

## Usage Examples

### Example SQL Queries

```sql
-- Get counties with highest pedestrian fatality rates
SELECT c.geoid, c.name, d.value as pedestrian_fatality_rate
FROM counties c
JOIN sdoh_data d ON c.geoid = d.geoid
WHERE d.variable_name = 'pedestrian_fatality_rate' 
AND d.year = 2020
ORDER BY d.value DESC
LIMIT 20;

-- Compare traffic fatality rates with poverty rates
SELECT c.geoid, c.name, 
       tf.value as traffic_fatality_rate,
       pr.value as poverty_rate
FROM counties c
JOIN sdoh_data tf ON c.geoid = tf.geoid AND tf.variable_name = 'traffic_fatality_rate' AND tf.year = 2020
JOIN sdoh_data pr ON c.geoid = pr.geoid AND pr.variable_name = 'poverty_rate' AND pr.year = 2020
ORDER BY c.name;
```

### Research Applications

1. **Health Equity**: Examining disparities in traffic safety outcomes across different demographic groups

2. **Built Environment Impact**: Analyzing how built environment characteristics correlate with traffic safety outcomes

3. **Policy Evaluation**: Assessing the effectiveness of speed limits, drunk driving laws, and other traffic safety policies

4. **Transportation Planning**: Informing transportation planning to reduce fatalities and injuries

## References

1. National Highway Traffic Safety Administration. (2022). Fatality Analysis Reporting System (FARS). https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars

2. Centers for Disease Control and Prevention. (2022). Motor Vehicle Safety. https://www.cdc.gov/transportationsafety/

3. Sauber-Schatz, E.K., Ederer, D.J., Dellinger, A.M., & Baldwin, G.T. (2016). Vital Signs: Motor Vehicle Injury Prevention — United States and 19 Comparison Countries. MMWR. Morbidity and Mortality Weekly Report, 65(26), 672–677.