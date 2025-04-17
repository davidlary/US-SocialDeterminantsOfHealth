# County Boundary Shapefiles

This directory contains county boundary shapefiles for different Census years, which are essential for mapping and spatial analysis of the Social Determinants of Health data.

## Available Shapefiles

The following county boundary shapefiles are included:

| Year | Description | Files | Source |
|------|-------------|-------|--------|
| 1990 | 1990 Census county boundaries | counties_1990.shp, .dbf, .shx, .prj | US Census Bureau TIGER/Line |
| 2000 | 2000 Census county boundaries | counties_2000.shp, .dbf, .shx, .prj | US Census Bureau TIGER/Line |
| 2010 | 2010 Census county boundaries | counties_2010.shp, .dbf, .shx, .prj | US Census Bureau TIGER/Line |
| 2020 | 2020 Census county boundaries | counties_2020.shp, .dbf, .shx, .prj | US Census Bureau TIGER/Line |

## County Changes Over Time

County boundaries and definitions change over time. Major changes include:

- Creation of new counties
- County mergers
- Boundary adjustments
- Name changes
- FIPS code changes

The most significant changes are documented in the metadata files included in this directory.

## Crosswalks

To facilitate analysis across different time periods, crosswalk files are provided:

- `county_crosswalk_1990_2020.csv`: Maps 1990 counties to 2020 counties
- `county_crosswalk_2000_2020.csv`: Maps 2000 counties to 2020 counties
- `county_crosswalk_2010_2020.csv`: Maps 2010 counties to 2020 counties

Each crosswalk file includes:
- Source county FIPS code
- Target county FIPS code
- Proportion of area in source that maps to target
- Proportion of population in source that maps to target

## Usage in R

The following R code demonstrates how to load and work with these shapefiles:

```r
library(sf)

# Load a specific year's county boundaries
counties_2020 <- st_read("data/shapefiles/counties_2020.shp")

# Plot the counties
plot(st_geometry(counties_2020))

# Join with SDOH data
library(dplyr)
sdoh_data <- read.csv("output/county_sdoh_data_complete.csv")
counties_with_data <- left_join(counties_2020, 
                               filter(sdoh_data, year == 2020),
                               by = c("GEOID" = "geoid"))

# Create a choropleth map
library(ggplot2)
ggplot(counties_with_data) +
  geom_sf(aes(fill = poverty_rate)) +
  scale_fill_viridis_c(name = "Poverty Rate (%)") +
  theme_minimal() +
  labs(title = "County Poverty Rates (2020)")
```

## Simplification for Performance

For web mapping and visualization performance, simplified versions of the county boundaries are also available:

- `counties_2020_simplified.shp`: 2020 boundaries simplified to improve rendering speed
- Topology is preserved to ensure no gaps between counties
- The simplification reduces file size by approximately 90% while maintaining county identifiability

## Alaska, Hawaii, and Territories

The shapefiles include:

- All 50 states (including Alaska and Hawaii)
- District of Columbia
- US territories (Puerto Rico, Guam, US Virgin Islands, American Samoa, Northern Mariana Islands)

For mapping applications, an alternative shapefile with Alaska and Hawaii repositioned is available as `counties_2020_alaska_hawaii_repositioned.shp`.

## Updating Shapefiles

To update or add new shapefiles:

1. Download the latest TIGER/Line shapefiles from the US Census Bureau
2. Process them using the utility script: `R/utilities/process_county_shapefiles.r`
3. Create appropriate crosswalks using the utility script: `R/utilities/create_county_crosswalks.r`
4. Update this README with the new information

## Sources and Attribution

These shapefiles are derived from US Census Bureau TIGER/Line files:
https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html

When using these files in publications, please cite:
"U.S. Census Bureau, Geography Division, TIGER/Line Shapefiles"
EOL < /dev/null