#!/usr/bin/env Rscript

# Traffic Safety Visualization Script
# This script creates visualizations of traffic safety data

# Load required packages
required_packages <- c(
  "tidyverse", 
  "sf", 
  "viridis", 
  "gridExtra",
  "plotly"
)

# Load packages with error handling
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    message(paste("Package", pkg, "is not installed. Some visualizations may not work."))
  }
}

# Create directories for output
output_dir <- "output/visualizations/traffic_safety"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

# Load traffic safety data
message("Loading traffic safety data...")
source("fetch_traffic_safety_data.r")

# Function to create time series plot
create_time_series_plot <- function(data, variable, title = NULL, subtitle = NULL) {
  if (is.null(title)) title <- paste(variable, "Over Time")
  
  # Create quality column name based on variable
  quality_col <- paste0(variable, "_data_quality")
  
  # Check if quality column exists
  if (!quality_col %in% names(data)) {
    quality_col <- NULL
  }
  
  # Get data for the variable
  plot_data <- data %>%
    filter(!is.na(!!sym(variable)))
  
  # Create base plot
  p <- ggplot(plot_data, aes(x = year, y = !!sym(variable), group = fips)) +
    theme_minimal() +
    labs(title = title, subtitle = subtitle, x = "Year", y = variable)
  
  # Add color by quality if available
  if (!is.null(quality_col) && quality_col %in% names(data)) {
    p <- p + 
      geom_line(aes(color = !!sym(quality_col)), alpha = 0.3) +
      scale_color_viridis_d() +
      labs(color = "Data Quality")
  } else {
    p <- p + geom_line(alpha = 0.3, color = "steelblue")
  }
  
  # Add smoother to show trend
  p <- p + 
    geom_smooth(aes(group = 1), method = "loess", span = 1, se = TRUE) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold")
    )
  
  return(p)
}

# Function to create choropleth map
create_county_map <- function(data, variable, year, title = NULL) {
  # Default title
  if (is.null(title)) title <- paste(variable, "by County,", year)
  
  # Check if sf package is available
  if (!require("sf", quietly = TRUE)) {
    message("sf package not available. Cannot create map.")
    return(NULL)
  }
  
  # Try to load US county shapefile
  counties_sf <- tryCatch({
    # Try to load from tigris if available
    if (require("tigris", quietly = TRUE)) {
      message("Loading county boundaries from tigris package...")
      tigris::counties(cb = TRUE, year = min(2020, as.numeric(year)))
    } else {
      # Otherwise, look for local shapefile
      shapefile_path <- "data/shapefiles/counties_2020.rds"
      if (file.exists(shapefile_path)) {
        message("Loading county boundaries from local file...")
        readRDS(shapefile_path)
      } else {
        message("No county boundary file found. Cannot create map.")
        return(NULL)
      }
    }
  }, error = function(e) {
    message("Error loading county boundaries: ", e$message)
    return(NULL)
  })
  
  # If failed to load counties, return NULL
  if (is.null(counties_sf)) return(NULL)
  
  # Filter data to requested year
  year_data <- data %>%
    filter(year == !!year, !is.na(!!sym(variable)))
  
  # Ensure GEOID is properly formatted
  year_data$GEOID <- sprintf("%05d", as.numeric(year_data$fips))
  
  # Join with shapefile
  counties_data <- counties_sf %>%
    left_join(year_data, by = "GEOID")
  
  # Create map
  map <- ggplot(counties_data) +
    geom_sf(aes(fill = !!sym(variable)), color = "white", size = 0.1) +
    scale_fill_viridis_c(option = "plasma", na.value = "gray90") +
    labs(
      title = title,
      subtitle = paste("Data for year", year),
      fill = variable
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      axis.text = element_blank()
    )
  
  return(map)
}

# Function to create fatality type comparison
create_fatality_comparison <- function(data, year_range = NULL) {
  # Filter to year range if specified
  if (!is.null(year_range)) {
    data <- data %>% filter(year >= min(year_range) & year <= max(year_range))
  }
  
  # Get total values by year for each fatality type
  fatality_types <- data %>%
    group_by(year) %>%
    summarize(
      Total = sum(traffic_fatality_count, na.rm = TRUE),
      Pedestrian_and_Cyclist = sum(ped_bike_fatality_count, na.rm = TRUE),
      Alcohol_Related = sum(dui_fatality_count, na.rm = TRUE),
      Speeding_Related = sum(speeding_fatality_count, na.rm = TRUE)
    ) %>%
    pivot_longer(
      cols = c(Total, Pedestrian_and_Cyclist, Alcohol_Related, Speeding_Related),
      names_to = "Fatality_Type",
      values_to = "Count"
    )
  
  # Create the comparison plot
  p <- ggplot(fatality_types, aes(x = year, y = Count, color = Fatality_Type)) +
    geom_line(size = 1) +
    geom_point() +
    scale_color_viridis_d() +
    labs(
      title = "Comparison of Traffic Fatality Types Over Time",
      x = "Year",
      y = "Fatality Count",
      color = "Fatality Type"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )
  
  return(p)
}

# Function to create data quality dashboard
create_data_quality_dashboard <- function(data) {
  # Calculate data quality statistics
  quality_stats <- data %>%
    summarize(
      Total_Records = n(),
      Missing_Fatality_Count = sum(is.na(traffic_fatality_count)),
      Missing_Fatality_Rate = sum(is.na(traffic_fatality_rate_per_100k)),
      Direct_Data = sum(traffic_fatality_count_data_quality == "direct", na.rm = TRUE),
      Interpolated_Data = sum(traffic_fatality_count_data_quality == "interpolated", na.rm = TRUE),
      Simulated_Data = sum(traffic_fatality_count_data_quality == "simulated", na.rm = TRUE),
      Extrapolated_Data = sum(traffic_fatality_count_data_quality == "extrapolated", na.rm = TRUE),
      Other_Quality = sum(!traffic_fatality_count_data_quality %in% 
                           c("direct", "interpolated", "simulated", "extrapolated"), na.rm = TRUE)
    )
  
  # Quality by year
  quality_by_year <- data %>%
    group_by(year) %>%
    summarize(
      Direct = sum(traffic_fatality_count_data_quality == "direct", na.rm = TRUE),
      Interpolated = sum(traffic_fatality_count_data_quality == "interpolated", na.rm = TRUE),
      Simulated = sum(traffic_fatality_count_data_quality == "simulated", na.rm = TRUE),
      Extrapolated = sum(traffic_fatality_count_data_quality == "extrapolated", na.rm = TRUE)
    ) %>%
    pivot_longer(
      cols = c(Direct, Interpolated, Simulated, Extrapolated),
      names_to = "Quality",
      values_to = "Count"
    )
  
  # Quality distribution plot
  p1 <- ggplot(quality_by_year, aes(x = year, y = Count, fill = Quality)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_viridis_d() +
    labs(
      title = "Data Quality Distribution by Year",
      x = "Year",
      y = "Record Count",
      fill = "Data Quality"
    ) +
    theme_minimal()
  
  # Overall quality pie chart
  quality_overall <- data.frame(
    Quality = c("Direct", "Interpolated", "Simulated", "Extrapolated", "Other"),
    Count = c(
      quality_stats$Direct_Data,
      quality_stats$Interpolated_Data,
      quality_stats$Simulated_Data,
      quality_stats$Extrapolated_Data,
      quality_stats$Other_Quality
    )
  )
  
  p2 <- ggplot(quality_overall, aes(x = "", y = Count, fill = Quality)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    scale_fill_viridis_d() +
    labs(
      title = "Overall Data Quality Distribution",
      fill = "Data Quality"
    ) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank()
    )
  
  # Combine plots
  if (require("gridExtra")) {
    combined <- grid.arrange(p1, p2, ncol = 2)
    return(combined)
  } else {
    return(list(quality_by_year = p1, quality_overall = p2))
  }
}

# Main function to generate all visualizations
generate_traffic_safety_visualizations <- function(years = 2015:2020, refresh_cache = FALSE) {
  # Fetch traffic safety data
  message("Fetching traffic safety data for years", min(years), "to", max(years), "...")
  traffic_data <- fetch_traffic_safety_data(
    years = years,
    refresh_cache = refresh_cache,
    allow_interpolation = TRUE
  )
  
  # Time series visualizations
  message("Creating time series visualizations...")
  plots <- list()
  
  # Fatality rate time series
  plots$fatality_rate <- create_time_series_plot(
    traffic_data, 
    "traffic_fatality_rate_per_100k",
    "Traffic Fatality Rate Trends",
    "Fatalities per 100,000 population by county"
  )
  ggsave(file.path(output_dir, "fatality_rate_trend.png"), plots$fatality_rate, width = 10, height = 6)
  
  # DUI fatality rate time series
  plots$dui_rate <- create_time_series_plot(
    traffic_data, 
    "dui_fatality_rate_per_100k",
    "Alcohol-Related Fatality Rate Trends",
    "DUI fatalities per 100,000 population by county"
  )
  ggsave(file.path(output_dir, "dui_fatality_rate_trend.png"), plots$dui_rate, width = 10, height = 6)
  
  # Pedestrian and cyclist fatality rate time series
  plots$ped_bike_rate <- create_time_series_plot(
    traffic_data, 
    "ped_bike_fatality_rate_per_100k",
    "Pedestrian & Cyclist Fatality Rate Trends",
    "Fatalities per 100,000 population by county"
  )
  ggsave(file.path(output_dir, "ped_bike_fatality_rate_trend.png"), plots$ped_bike_rate, width = 10, height = 6)
  
  # Create choropleth maps for most recent year
  recent_year <- max(traffic_data$year)
  message("Creating choropleth maps for", recent_year, "...")
  
  # Total fatality rate map
  plots$fatality_map <- create_county_map(
    traffic_data,
    "traffic_fatality_rate_per_100k",
    recent_year,
    paste("Traffic Fatality Rate by County,", recent_year)
  )
  if (!is.null(plots$fatality_map)) {
    ggsave(file.path(output_dir, paste0("fatality_rate_map_", recent_year, ".png")), 
           plots$fatality_map, width = 12, height = 8)
  }
  
  # DUI fatality rate map
  plots$dui_map <- create_county_map(
    traffic_data,
    "dui_fatality_rate_per_100k",
    recent_year,
    paste("Alcohol-Related Fatality Rate by County,", recent_year)
  )
  if (!is.null(plots$dui_map)) {
    ggsave(file.path(output_dir, paste0("dui_fatality_rate_map_", recent_year, ".png")), 
           plots$dui_map, width = 12, height = 8)
  }
  
  # Create fatality type comparison
  message("Creating fatality type comparison...")
  plots$fatality_comparison <- create_fatality_comparison(traffic_data)
  ggsave(file.path(output_dir, "fatality_type_comparison.png"), 
         plots$fatality_comparison, width = 10, height = 6)
  
  # Create data quality dashboard
  message("Creating data quality dashboard...")
  plots$quality_dashboard <- create_data_quality_dashboard(traffic_data)
  ggsave(file.path(output_dir, "data_quality_dashboard.png"), 
         plots$quality_dashboard, width = 12, height = 8)
  
  # Create interactive visualizations if plotly is available
  if (require("plotly")) {
    message("Creating interactive visualizations...")
    
    # Interactive time series
    p_interactive <- ggplotly(plots$fatality_rate)
    htmlwidgets::saveWidget(p_interactive, file.path(output_dir, "interactive_fatality_trend.html"))
    
    # Interactive map (if available)
    if (!is.null(plots$fatality_map)) {
      map_interactive <- ggplotly(plots$fatality_map)
      htmlwidgets::saveWidget(map_interactive, file.path(output_dir, "interactive_fatality_map.html"))
    }
  }
  
  message("Visualizations created successfully and saved to", output_dir)
  return(plots)
}

# Run the visualization generator if this script is executed directly
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  
  # Parse arguments
  years <- 2015:2020  # Default years
  refresh_cache <- FALSE
  
  if (length(args) >= 1) {
    if (args[1] == "--help" || args[1] == "-h") {
      cat("Traffic Safety Visualization Generator\n")
      cat("Usage: Rscript visualize_traffic_safety.r [start_year-end_year] [--refresh]\n")
      cat("  start_year-end_year: Year range to visualize (e.g., 2015-2020)\n")
      cat("  --refresh: Force refresh of cached data\n")
      quit(status = 0)
    }
    
    # Parse year range
    if (grepl("-", args[1])) {
      year_parts <- strsplit(args[1], "-")[[1]]
      if (length(year_parts) == 2) {
        start_year <- as.numeric(year_parts[1])
        end_year <- as.numeric(year_parts[2])
        if (!is.na(start_year) && !is.na(end_year)) {
          years <- start_year:end_year
        }
      }
    }
  }
  
  # Check for refresh flag
  if ("--refresh" %in% args) {
    refresh_cache <- TRUE
  }
  
  # Generate visualizations
  generate_traffic_safety_visualizations(years, refresh_cache)
}