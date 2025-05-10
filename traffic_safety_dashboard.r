#!/usr/bin/env Rscript

# Transportation Metrics Dashboard
# This script creates an interactive Shiny dashboard for exploring
# traffic safety data integrated with other transportation metrics

# Load required packages
required_packages <- c(
  "shiny", "shinydashboard", "plotly", "leaflet", "DT", "dplyr", 
  "tidyr", "ggplot2", "sf", "scales", "viridis", "tigris", 
  "htmltools", "shinyWidgets", "shinycssloaders", "stringr",
  "lubridate", "readr", "zoo", "forecast"
)

# Install missing packages
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

# Load packages
invisible(sapply(required_packages, library, character.only = TRUE))

# Configuration
config <- list(
  cache_dir = "data/cache",
  traffic_safety_dir = "data/traffic_safety",
  shapefile_dir = "data/shapefiles",
  output_dir = "output/visualizations/traffic_safety",
  default_years = 2015:2022,
  theme_color = "red",
  map_providers = list(
    "Esri.WorldGrayCanvas",
    "CartoDB.Positron",
    "OpenStreetMap",
    "Stamen.Terrain"
  )
)

#' Load traffic safety data from the fetch_traffic_safety_data module
#'
#' @param years Years to include in the dashboard
#' @param refresh_cache Whether to refresh cached data
#' @param allow_interpolation Whether to allow interpolation for missing years
#' @param allow_simulation Whether to allow simulating missing data
#' @param offline_mode Whether to use only cached data (no API calls)
#'
#' @return A dataframe with traffic safety data
load_traffic_safety_data <- function(
  years = config$default_years,
  refresh_cache = FALSE,
  allow_interpolation = TRUE,
  allow_simulation = FALSE,
  offline_mode = FALSE
) {
  # Check if fetch_traffic_safety_data.r exists
  if (!file.exists("fetch_traffic_safety_data.r")) {
    stop("fetch_traffic_safety_data.r not found. Please ensure the file exists in the current directory.")
  }
  
  # Source the traffic safety data fetcher
  source("fetch_traffic_safety_data.r")
  
  # Fetch traffic safety data
  traffic_data <- fetch_traffic_safety_data(
    years = years,
    cache_dir = config$cache_dir,
    refresh_cache = refresh_cache,
    allow_interpolation = allow_interpolation,
    allow_simulation = allow_simulation,
    offline_mode = offline_mode
  )
  
  # If data is NULL or has no rows, show warning and return empty data frame
  if (is.null(traffic_data) || nrow(traffic_data) == 0) {
    warning("No traffic safety data available. Please check your parameters and try again.")
    return(data.frame())
  }
  
  return(traffic_data)
}

#' Load transportation infrastructure data
#'
#' @param years Years to include
#' @param refresh_cache Whether to refresh cached data
#'
#' @return A dataframe with transportation infrastructure data
load_transportation_infrastructure_data <- function(
  years = config$default_years,
  refresh_cache = FALSE
) {
  # Try to find transportation data
  infra_cache_file <- file.path(config$cache_dir, "transportation_infrastructure.rds")
  
  # Check if cache file exists and not refreshing
  if (file.exists(infra_cache_file) && !refresh_cache) {
    return(readRDS(infra_cache_file))
  }
  
  # Check if fetch_transportation_data.r exists
  if (file.exists("fetch_transportation_data.r")) {
    # If the transportation data fetcher exists, use it
    source("fetch_transportation_data.r")
    
    # Try to fetch transportation data
    tryCatch({
      transport_data <- fetch_transportation_data(years = years)
      saveRDS(transport_data, infra_cache_file)
      return(transport_data)
    }, error = function(e) {
      warning("Error fetching transportation data: ", e$message)
      return(create_placeholder_transport_data(years))
    })
  } else {
    # Create placeholder data if no fetcher available
    return(create_placeholder_transport_data(years))
  }
}

#' Create placeholder transportation infrastructure data
#'
#' @param years Years to include
#' @return A dataframe with placeholder transportation data
create_placeholder_transport_data <- function(years) {
  message("Creating placeholder transportation infrastructure data")
  
  # Get county FIPS codes
  counties <- tryCatch({
    # Try to get county data from tigris
    counties <- tigris::counties(cb = TRUE, year = 2020)
    counties$fips <- counties$GEOID
    counties$county_name <- counties$NAME
    counties %>% 
      sf::st_drop_geometry() %>% 
      select(fips, county_name)
  }, error = function(e) {
    # If tigris fails, create a simple template with major counties
    data.frame(
      fips = c("06037", "17031", "36061", "48201", "12086"),
      county_name = c("Los Angeles County", "Cook County", "New York County", "Harris County", "Miami-Dade County"),
      stringsAsFactors = FALSE
    )
  })
  
  # Generate a grid of counties and years
  county_years <- expand.grid(
    fips = counties$fips,
    year = years,
    stringsAsFactors = FALSE
  )
  
  # Join with county names
  county_years <- merge(county_years, counties, by = "fips")
  
  # Set random seed for reproducibility
  set.seed(123)
  
  # Generate placeholder transportation metrics
  transport_data <- county_years %>%
    mutate(
      # Public transportation usage (% of commuters)
      public_transit_pct = pmin(runif(n(), 0, 50), 100),
      
      # Average commute time (minutes)
      avg_commute_time = runif(n(), 15, 60),
      
      # Highway miles per 1000 population
      highway_miles_per_1000 = runif(n(), 0.1, 10),
      
      # Bridge condition (% in good condition)
      bridge_condition_pct = pmin(runif(n(), 30, 95), 100),
      
      # Public transportation accessibility (% population with access)
      transit_accessibility_pct = pmin(runif(n(), 0, 90), 100),
      
      # Vehicle miles traveled per capita
      vehicle_miles_per_capita = runif(n(), 5000, 15000),
      
      # Electric vehicle adoption (% of registered vehicles)
      ev_adoption_pct = pmin(runif(n(), 0, 8) * (year - 2014) / 10, 100),
      
      # Data quality (all simulated)
      data_quality = "simulated"
    )
  
  # Return the placeholder data
  return(transport_data)
}

#' Merge traffic safety and transportation infrastructure data
#'
#' @param traffic_data Traffic safety data
#' @param transport_data Transportation infrastructure data
#'
#' @return A merged dataframe with all transportation metrics
merge_transportation_data <- function(traffic_data, transport_data) {
  # If either dataset is empty, return the other
  if (nrow(traffic_data) == 0) return(transport_data)
  if (nrow(transport_data) == 0) return(traffic_data)
  
  # Ensure FIPS codes are formatted consistently
  traffic_data$fips <- sprintf("%05d", as.numeric(traffic_data$fips))
  transport_data$fips <- sprintf("%05d", as.numeric(transport_data$fips))
  
  # Perform a full outer join on FIPS and year
  merged_data <- full_join(
    traffic_data,
    transport_data,
    by = c("fips", "year"),
    suffix = c("", "_transport")
  )
  
  # Handle duplicate columns (like county_name)
  if ("county_name_transport" %in% names(merged_data)) {
    merged_data <- merged_data %>%
      mutate(
        county_name = coalesce(county_name, county_name_transport)
      ) %>%
      select(-county_name_transport)
  }
  
  # If state_name exists in traffic data but not transport, keep it
  if ("state_name" %in% names(traffic_data) && !"state_name" %in% names(transport_data)) {
    merged_data <- merged_data %>%
      select(fips, year, county_name, state_name, everything())
  } else {
    merged_data <- merged_data %>%
      select(fips, year, county_name, everything())
  }
  
  return(merged_data)
}

#' Calculate derived metrics for analysis
#'
#' @param data Combined transportation data
#'
#' @return Data with additional calculated metrics
calculate_derived_metrics <- function(data) {
  # Skip if data is empty
  if (nrow(data) == 0) return(data)
  
  # Define which metrics to calculate
  metrics_to_calculate <- c(
    # Safety vs. Transit Access
    c("traffic_fatality_rate_per_100k", "public_transit_pct"),
    # Safety vs. Commute Time
    c("traffic_fatality_rate_per_100k", "avg_commute_time"),
    # Safety vs. VMT
    c("traffic_fatality_rate_per_100k", "vehicle_miles_per_capita")
  )
  
  # Check which metrics are available in the data
  available_metrics <- names(data)
  
  # Initialize data frame for derived metrics
  derived_data <- data
  
  # Calculate composite metrics when both components are available
  for (metric_pair in metrics_to_calculate) {
    if (all(metric_pair %in% available_metrics)) {
      # Create composite names
      composite_name <- paste(metric_pair, collapse = "_vs_")
      ratio_name <- paste0(metric_pair[1], "_to_", metric_pair[2], "_ratio")
      
      # Calculate composite value (normalize both metrics and add)
      derived_data[[composite_name]] <- derived_data %>%
        group_by(year) %>%
        mutate(
          metric1_norm = scale(!!sym(metric_pair[1])),
          metric2_norm = scale(!!sym(metric_pair[2]))
        ) %>%
        ungroup() %>%
        transmute(
          composite = (metric1_norm + metric2_norm) / 2
        ) %>%
        pull(composite)
      
      # Calculate ratio
      derived_data[[ratio_name]] <- derived_data[[metric_pair[1]]] / 
        derived_data[[metric_pair[2]]]
      
      # Handle infinite values
      derived_data[[ratio_name]] <- ifelse(
        is.infinite(derived_data[[ratio_name]]) | 
          is.nan(derived_data[[ratio_name]]),
        NA,
        derived_data[[ratio_name]]
      )
    }
  }
  
  # Add a Transportation Safety Index if enough metrics are available
  essential_metrics <- c(
    "traffic_fatality_rate_per_100k", 
    "ped_bike_fatality_rate_per_100k", 
    "dui_fatality_rate_per_100k"
  )
  
  if (all(essential_metrics %in% available_metrics)) {
    derived_data <- derived_data %>%
      group_by(year) %>%
      mutate(
        # Create an index where lower is better (safer)
        # Scale each metric (higher = worse)
        traffic_safety_index = scale(traffic_fatality_rate_per_100k) * 0.5 +
          scale(ped_bike_fatality_rate_per_100k) * 0.3 +
          scale(dui_fatality_rate_per_100k) * 0.2,
        
        # Invert the scale so higher = better (safer)
        traffic_safety_index = -traffic_safety_index,
        
        # Rescale to 0-100 range
        traffic_safety_index = 100 * (traffic_safety_index - min(traffic_safety_index, na.rm = TRUE)) / 
          (max(traffic_safety_index, na.rm = TRUE) - min(traffic_safety_index, na.rm = TRUE))
      ) %>%
      ungroup()
  }
  
  return(derived_data)
}

#' Create time series forecasts for transportation metrics
#'
#' @param data Transportation data
#' @param metric_name The metric to forecast
#' @param forecast_years Number of years to forecast
#'
#' @return A dataframe with forecasts
create_forecast <- function(data, metric_name, forecast_years = 3) {
  # Skip if forecasting package not available
  if (!require("forecast")) {
    warning("forecast package not available. Cannot create forecasts.")
    return(NULL)
  }
  
  # Skip if metric not in data
  if (!metric_name %in% names(data)) {
    warning(paste("Metric", metric_name, "not found in data."))
    return(NULL)
  }
  
  # Calculate national average by year
  yearly_avg <- data %>%
    group_by(year) %>%
    summarize(
      value = mean(!!sym(metric_name), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(value)) %>%
    arrange(year)
  
  # Skip if not enough data points
  if (nrow(yearly_avg) < 3) {
    warning("Not enough data points for forecasting.")
    return(NULL)
  }
  
  # Convert to time series
  ts_data <- ts(yearly_avg$value, start = min(yearly_avg$year), frequency = 1)
  
  # Create forecast with confidence intervals
  forecast_result <- forecast::forecast(forecast::auto.arima(ts_data), h = forecast_years)
  
  # Convert forecast to dataframe
  forecast_df <- data.frame(
    year = seq(from = max(yearly_avg$year) + 1, 
               length.out = forecast_years),
    value = as.numeric(forecast_result$mean),
    lower_80 = as.numeric(forecast_result$lower[, 1]),
    upper_80 = as.numeric(forecast_result$upper[, 1]),
    lower_95 = as.numeric(forecast_result$lower[, 2]),
    upper_95 = as.numeric(forecast_result$upper[, 2])
  )
  
  # Combine historical and forecast data
  combined_df <- bind_rows(
    yearly_avg %>% mutate(type = "historical"),
    forecast_df %>% mutate(type = "forecast")
  )
  
  return(combined_df)
}

#' Load county shapefiles for mapping
#'
#' @param year The year for which to load county boundaries
#' @return An sf object with county boundaries
load_county_shapes <- function(year = 2020) {
  shapefile_path <- file.path(config$shapefile_dir, paste0("counties_", year, ".rds"))
  
  if (file.exists(shapefile_path)) {
    # Use cached shapefile
    shapes <- readRDS(shapefile_path)
  } else {
    # Download from tigris if not available
    tryCatch({
      shapes <- tigris::counties(cb = TRUE, year = year)
      
      # Create directory if it doesn't exist
      if (!dir.exists(config$shapefile_dir)) {
        dir.create(config$shapefile_dir, recursive = TRUE)
      }
      
      # Cache for future use
      saveRDS(shapes, shapefile_path)
    }, error = function(e) {
      warning("Failed to download county shapefiles: ", e$message)
      return(NULL)
    })
  }
  
  return(shapes)
}

#' Launch the transportation metrics dashboard
#'
#' @param traffic_data Optional pre-loaded traffic safety data
#' @param transport_data Optional pre-loaded transportation infrastructure data
#' @param port Port to run the Shiny app on
#' @param host Host to bind the Shiny app to
#' @param launch_browser Whether to open the app in a browser
#'
#' @return The Shiny app object
launch_traffic_safety_dashboard <- function(
  traffic_data = NULL,
  transport_data = NULL,
  port = 3838,
  host = "0.0.0.0",
  launch_browser = TRUE
) {
  
  # ---- UI Components ----
  
  # Header
  header <- dashboardHeader(
    title = "Transportation & Safety Dashboard",
    dropdownMenu(
      type = "notifications", 
      icon = icon("info-circle"),
      badgeStatus = NULL,
      headerText = "About",
      notificationItem(
        text = "Traffic Safety & Transportation Analysis",
        icon = icon("car")
      ),
      notificationItem(
        text = "Data from NHTSA FARS, CDC WONDER and more",
        icon = icon("database")
      ),
      notificationItem(
        text = paste("Last updated:", format(Sys.Date(), "%B %d, %Y")),
        icon = icon("calendar")
      )
    )
  )
  
  # Sidebar
  sidebar <- dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard Overview", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("Safety Metrics", tabName = "safety", icon = icon("shield-alt")),
      menuItem("Transportation Infrastructure", tabName = "infrastructure", icon = icon("road")),
      menuItem("County Explorer", tabName = "county", icon = icon("map-marker-alt")),
      menuItem("Time Series Analysis", tabName = "trends", icon = icon("chart-line")),
      menuItem("Data Quality", tabName = "quality", icon = icon("check-circle")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    # Control panel
    div(
      class = "sidebar-form",
      style = "padding: 10px;",
      h4("Controls"),
      selectInput("selected_years", "Year Range",
                  choices = c("Last 5 years" = "last5",
                             "Last 10 years" = "last10",
                             "All available" = "all"),
                  selected = "last5"),
      checkboxInput("allow_interpolation", "Allow data interpolation", value = TRUE),
      checkboxInput("show_forecast", "Show forecasts", value = TRUE),
      actionButton("refresh_data", "Refresh Data", icon = icon("sync"))
    )
  )
  
  # Body
  body <- dashboardBody(
    # Include custom CSS
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        .box {
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        }
        .small-box {
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        }
        .info-box {
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        }
        .nav-tabs-custom {
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        }
        .leaflet-container {
          background: #f8f9fa;
        }
        .data-quality-direct {
          color: #28a745;
        }
        .data-quality-interpolated {
          color: #fd7e14;
        }
        .data-quality-extrapolated {
          color: #dc3545;
        }
        .data-quality-simulated {
          color: #6c757d;
        }
        .metric-card {
          padding: 15px;
          border-radius: 5px;
          margin-bottom: 15px;
          background-color: white;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        }
        .metric-title {
          font-weight: bold;
          font-size: 16px;
          margin-bottom: 10px;
        }
        .metric-value {
          font-size: 24px;
          font-weight: bold;
        }
        .trend-up {
          color: #dc3545;
        }
        .trend-down {
          color: #28a745;
        }
        .trend-neutral {
          color: #6c757d;
        }
      "))
    ),
    
    tabItems(
      # Dashboard Overview
      tabItem(
        tabName = "dashboard",
        fluidRow(
          # Info boxes
          infoBoxOutput("fatality_rate_box", width = 3),
          infoBoxOutput("injury_rate_box", width = 3),
          infoBoxOutput("dui_rate_box", width = 3),
          infoBoxOutput("ped_bike_rate_box", width = 3)
        ),
        fluidRow(
          # Main dashboard charts
          box(
            title = "Traffic Safety Trends",
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("dashboard_trend_plot", height = "300px") %>% withSpinner()
          ),
          box(
            title = "Key Metrics",
            width = 4,
            status = "info",
            solidHeader = TRUE,
            htmlOutput("key_metrics_summary")
          )
        ),
        fluidRow(
          # Map and forecasting
          box(
            title = "Geographic Distribution",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            leafletOutput("dashboard_map", height = "300px") %>% withSpinner()
          ),
          box(
            title = "Forecast",
            width = 6,
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("dashboard_forecast", height = "300px") %>% withSpinner()
          )
        )
      ),
      
      # Safety Metrics Tab
      tabItem(
        tabName = "safety",
        fluidRow(
          box(
            title = "Traffic Fatality Analysis",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            tabsetPanel(
              tabPanel(
                "Rate Comparison",
                fluidRow(
                  column(
                    width = 3,
                    selectInput("safety_metric", "Safety Metric",
                                choices = c("Traffic Fatality Rate" = "traffic_fatality_rate_per_100k",
                                           "DUI Fatality Rate" = "dui_fatality_rate_per_100k",
                                           "Pedestrian/Cyclist Rate" = "ped_bike_fatality_rate_per_100k",
                                           "Speeding Fatality Rate" = "speeding_fatality_rate_per_100k"),
                                selected = "traffic_fatality_rate_per_100k")
                  ),
                  column(
                    width = 3,
                    selectInput("safety_year", "Year", choices = NULL)
                  ),
                  column(
                    width = 3,
                    selectInput("safety_state", "State", choices = NULL)
                  ),
                  column(
                    width = 3,
                    downloadButton("download_safety_data", "Download Data")
                  )
                ),
                plotlyOutput("safety_bar_chart", height = "400px") %>% withSpinner(),
                DTOutput("safety_data_table") %>% withSpinner()
              ),
              tabPanel(
                "Fatality Types",
                plotlyOutput("fatality_type_comparison", height = "400px") %>% withSpinner(),
                htmlOutput("fatality_type_analysis")
              ),
              tabPanel(
                "Risk Factors",
                selectInput("risk_comparison_type", "Comparison",
                            choices = c("DUI vs. Total Fatalities", "Speeding vs. Total Fatalities", 
                                       "Pedestrian/Cyclist vs. Total Fatalities")),
                plotlyOutput("risk_factor_analysis", height = "400px") %>% withSpinner(),
                htmlOutput("risk_factor_summary")
              )
            )
          )
        )
      ),
      
      # Transportation Infrastructure Tab
      tabItem(
        tabName = "infrastructure",
        fluidRow(
          box(
            title = "Transportation Infrastructure Metrics",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            tabsetPanel(
              tabPanel(
                "Infrastructure Overview",
                fluidRow(
                  column(
                    width = 3,
                    selectInput("infrastructure_metric", "Metric",
                                choices = c("Public Transit Usage (%)" = "public_transit_pct",
                                           "Average Commute Time" = "avg_commute_time",
                                           "Highway Miles per 1000" = "highway_miles_per_1000",
                                           "Bridge Condition (%)" = "bridge_condition_pct"),
                                selected = "public_transit_pct")
                  ),
                  column(
                    width = 3,
                    selectInput("infrastructure_year", "Year", choices = NULL)
                  ),
                  column(
                    width = 3,
                    selectInput("infrastructure_state", "State", choices = NULL)
                  ),
                  column(
                    width = 3,
                    downloadButton("download_infrastructure_data", "Download Data")
                  )
                ),
                plotlyOutput("infrastructure_map", height = "400px") %>% withSpinner(),
                DTOutput("infrastructure_data_table") %>% withSpinner()
              ),
              tabPanel(
                "Infrastructure vs. Safety",
                fluidRow(
                  column(
                    width = 4,
                    selectInput("infra_safety_x", "Infrastructure Metric (X)",
                                choices = c("Public Transit Usage (%)" = "public_transit_pct",
                                           "Average Commute Time" = "avg_commute_time",
                                           "Highway Miles per 1000" = "highway_miles_per_1000"),
                                selected = "public_transit_pct")
                  ),
                  column(
                    width = 4,
                    selectInput("infra_safety_y", "Safety Metric (Y)",
                                choices = c("Traffic Fatality Rate" = "traffic_fatality_rate_per_100k",
                                           "DUI Fatality Rate" = "dui_fatality_rate_per_100k"),
                                selected = "traffic_fatality_rate_per_100k")
                  ),
                  column(
                    width = 4,
                    selectInput("infra_safety_year", "Year", choices = NULL)
                  )
                ),
                plotlyOutput("infra_safety_scatter", height = "400px") %>% withSpinner(),
                htmlOutput("infra_safety_correlation")
              ),
              tabPanel(
                "Time Series Analysis",
                fluidRow(
                  column(
                    width = 6,
                    selectInput("infra_time_metric", "Infrastructure Metric",
                                choices = c("Public Transit Usage (%)" = "public_transit_pct",
                                           "Average Commute Time" = "avg_commute_time",
                                           "Highway Miles per 1000" = "highway_miles_per_1000"),
                                selected = "public_transit_pct")
                  ),
                  column(
                    width = 6,
                    checkboxInput("infra_show_forecast", "Show Forecast", value = TRUE)
                  )
                ),
                plotlyOutput("infra_time_series", height = "400px") %>% withSpinner(),
                htmlOutput("infra_time_analysis")
              )
            )
          )
        )
      ),
      
      # County Explorer Tab
      tabItem(
        tabName = "county",
        fluidRow(
          box(
            title = "County Explorer",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            fluidRow(
              column(
                width = 4,
                selectInput("county_explorer_state", "State", choices = NULL)
              ),
              column(
                width = 4,
                selectInput("county_explorer_county", "County", choices = NULL)
              ),
              column(
                width = 4,
                selectInput("county_explorer_metric", "Metric",
                            choices = c("Traffic Fatality Rate" = "traffic_fatality_rate_per_100k",
                                       "Public Transit Usage (%)" = "public_transit_pct",
                                       "DUI Fatality Rate" = "dui_fatality_rate_per_100k"),
                            selected = "traffic_fatality_rate_per_100k")
              )
            ),
            plotlyOutput("county_time_series", height = "300px") %>% withSpinner()
          )
        ),
        fluidRow(
          box(
            title = "County Metrics Dashboard",
            width = 6,
            status = "info",
            solidHeader = TRUE,
            uiOutput("county_metrics_cards")
          ),
          box(
            title = "County Comparison",
            width = 6,
            status = "warning",
            solidHeader = TRUE,
            selectInput("county_comparison_counties", "Compare With",
                        choices = NULL, multiple = TRUE),
            plotlyOutput("county_comparison_chart", height = "300px") %>% withSpinner()
          )
        )
      ),
      
      # Time Series Analysis Tab
      tabItem(
        tabName = "trends",
        fluidRow(
          box(
            title = "Time Series Analysis",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            tabsetPanel(
              tabPanel(
                "Trend Analysis",
                fluidRow(
                  column(
                    width = 4,
                    selectInput("trend_metric", "Metric",
                                choices = c("Traffic Fatality Rate" = "traffic_fatality_rate_per_100k",
                                           "DUI Fatality Rate" = "dui_fatality_rate_per_100k",
                                           "Public Transit Usage (%)" = "public_transit_pct"),
                                selected = "traffic_fatality_rate_per_100k")
                  ),
                  column(
                    width = 4,
                    sliderInput("trend_span", "Smoothing Span", 
                                min = 0.1, max = 1, value = 0.5, step = 0.1)
                  ),
                  column(
                    width = 4,
                    checkboxInput("trend_show_forecast", "Show Forecast", value = TRUE)
                  )
                ),
                plotlyOutput("trend_analysis_plot", height = "400px") %>% withSpinner(),
                htmlOutput("trend_analysis_summary")
              ),
              tabPanel(
                "Seasonal Patterns",
                selectInput("seasonal_metric", "Metric",
                            choices = c("Traffic Fatality Rate" = "traffic_fatality_rate_per_100k",
                                       "DUI Fatality Rate" = "dui_fatality_rate_per_100k")),
                plotlyOutput("seasonal_analysis_plot", height = "400px") %>% withSpinner(),
                htmlOutput("seasonal_analysis_summary")
              ),
              tabPanel(
                "Multi-Variable Analysis",
                selectInput("multi_metrics", "Metrics to Compare",
                            choices = c("Traffic Fatality Rate" = "traffic_fatality_rate_per_100k",
                                       "DUI Fatality Rate" = "dui_fatality_rate_per_100k",
                                       "Public Transit Usage (%)" = "public_transit_pct",
                                       "Average Commute Time" = "avg_commute_time"),
                            multiple = TRUE,
                            selected = c("traffic_fatality_rate_per_100k", "public_transit_pct")),
                plotlyOutput("multi_variable_plot", height = "400px") %>% withSpinner(),
                htmlOutput("multi_variable_summary")
              )
            )
          )
        )
      ),
      
      # Data Quality Tab
      tabItem(
        tabName = "quality",
        fluidRow(
          box(
            title = "Data Quality Analysis",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            tabsetPanel(
              tabPanel(
                "Quality Overview",
                plotlyOutput("quality_overview_plot", height = "400px") %>% withSpinner(),
                htmlOutput("quality_overview_summary")
              ),
              tabPanel(
                "Data Coverage",
                selectInput("coverage_metric", "Metric",
                            choices = c("Traffic Fatality Rate" = "traffic_fatality_rate_per_100k",
                                       "DUI Fatality Rate" = "dui_fatality_rate_per_100k")),
                plotlyOutput("coverage_map", height = "400px") %>% withSpinner(),
                DTOutput("coverage_summary_table") %>% withSpinner()
              ),
              tabPanel(
                "Interpolation Analysis",
                selectInput("interpolation_metric", "Metric",
                            choices = c("Traffic Fatality Rate" = "traffic_fatality_rate_per_100k",
                                       "DUI Fatality Rate" = "dui_fatality_rate_per_100k")),
                plotlyOutput("interpolation_analysis", height = "400px") %>% withSpinner(),
                htmlOutput("interpolation_summary")
              )
            )
          )
        )
      ),
      
      # About Tab
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            title = "About This Dashboard",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            HTML("
              <h3>Transportation Metrics & Traffic Safety Dashboard</h3>
              <p>This interactive dashboard integrates traffic safety data with transportation infrastructure metrics to provide comprehensive analysis tools for transportation planners, researchers, and policy makers.</p>
              
              <h4>Data Sources</h4>
              <ul>
                <li><strong>NHTSA Fatality Analysis Reporting System (FARS)</strong> - Comprehensive crash data for fatal accidents</li>
                <li><strong>CDC WONDER Database</strong> - Mortality data including transportation-related deaths</li>
                <li><strong>U.S. Census Bureau</strong> - Population and demographic data</li>
                <li><strong>Bureau of Transportation Statistics</strong> - Transportation infrastructure metrics</li>
                <li><strong>Federal Highway Administration (FHWA)</strong> - Highway statistics</li>
              </ul>
              
              <h4>Data Quality Indicators</h4>
              <ul>
                <li><span class='data-quality-direct'>Direct</span> - Data obtained directly from authoritative sources</li>
                <li><span class='data-quality-interpolated'>Interpolated</span> - Data filled using time series interpolation</li>
                <li><span class='data-quality-extrapolated'>Extrapolated</span> - Data projected beyond available years</li>
                <li><span class='data-quality-simulated'>Simulated</span> - Synthetic data based on patterns in similar counties</li>
              </ul>
              
              <h4>Dashboard Features</h4>
              <ul>
                <li><strong>Dashboard Overview</strong> - Summary of key metrics and trends</li>
                <li><strong>Safety Metrics</strong> - Detailed analysis of traffic safety indicators</li>
                <li><strong>Transportation Infrastructure</strong> - Exploration of infrastructure metrics</li>
                <li><strong>County Explorer</strong> - County-level deep dives and comparisons</li>
                <li><strong>Time Series Analysis</strong> - Trend analysis and forecasting</li>
                <li><strong>Data Quality</strong> - Transparency about data sources and quality</li>
              </ul>
              
              <h4>Usage Notes</h4>
              <p>This dashboard supports data interpolation and forecasting to fill gaps in the data. Toggle these features using the control panel in the sidebar.</p>
              
              <h4>Citation</h4>
              <p>Social Determinants of Health: Transportation Metrics & Traffic Safety Dashboard (2025). Data derived from NHTSA FARS, CDC WONDER, and transportation infrastructure sources.</p>
            ")
          )
        )
      )
    )
  )
  
  # ---- Server Function ----
  server <- function(input, output, session) {
    # Reactive values for storing data
    rv <- reactiveValues(
      traffic_data = NULL,
      transport_data = NULL,
      combined_data = NULL,
      filtered_data = NULL,
      selected_state = NULL,
      selected_county = NULL,
      available_years = NULL,
      county_shapes = NULL,
      metrics_list = NULL,
      forecasts = list()
    )
    
    # Load initial data
    observe({
      # Determine if we should use provided data or load new data
      traffic_dataset <- if (!is.null(traffic_data)) {
        traffic_data
      } else {
        withProgress(
          message = "Loading traffic safety data...",
          value = 0.5,
          {
            years <- config$default_years
            load_traffic_safety_data(years = years)
          }
        )
      }
      
      transport_dataset <- if (!is.null(transport_data)) {
        transport_data
      } else {
        withProgress(
          message = "Loading transportation infrastructure data...",
          value = 0.5,
          {
            years <- config$default_years
            load_transportation_infrastructure_data(years = years)
          }
        )
      }
      
      # Store in reactive values
      rv$traffic_data <- traffic_dataset
      rv$transport_data <- transport_dataset
      
      # Merge datasets
      rv$combined_data <- merge_transportation_data(traffic_dataset, transport_dataset)
      
      # Calculate derived metrics
      rv$combined_data <- calculate_derived_metrics(rv$combined_data)
      
      # Set available years
      rv$available_years <- sort(unique(rv$combined_data$year))
      
      # Get county shapes
      rv$county_shapes <- load_county_shapes()
      
      # Build metrics list for UI dropdowns
      metrics <- list()
      
      if (nrow(rv$combined_data) > 0) {
        # Safety metrics
        if ("traffic_fatality_rate_per_100k" %in% names(rv$combined_data)) {
          metrics$traffic_fatality_rate_per_100k <- "Traffic Fatality Rate (per 100k)"
        }
        if ("dui_fatality_rate_per_100k" %in% names(rv$combined_data)) {
          metrics$dui_fatality_rate_per_100k <- "DUI Fatality Rate (per 100k)"
        }
        if ("ped_bike_fatality_rate_per_100k" %in% names(rv$combined_data)) {
          metrics$ped_bike_fatality_rate_per_100k <- "Pedestrian/Cyclist Fatality Rate (per 100k)"
        }
        if ("speeding_fatality_rate_per_100k" %in% names(rv$combined_data)) {
          metrics$speeding_fatality_rate_per_100k <- "Speeding Fatality Rate (per 100k)"
        }
        
        # Infrastructure metrics
        if ("public_transit_pct" %in% names(rv$combined_data)) {
          metrics$public_transit_pct <- "Public Transit Usage (%)"
        }
        if ("avg_commute_time" %in% names(rv$combined_data)) {
          metrics$avg_commute_time <- "Average Commute Time (min)"
        }
        if ("highway_miles_per_1000" %in% names(rv$combined_data)) {
          metrics$highway_miles_per_1000 <- "Highway Miles per 1000 Population"
        }
        if ("bridge_condition_pct" %in% names(rv$combined_data)) {
          metrics$bridge_condition_pct <- "Bridge Condition (% Good)"
        }
        if ("vehicle_miles_per_capita" %in% names(rv$combined_data)) {
          metrics$vehicle_miles_per_capita <- "Vehicle Miles Traveled per Capita"
        }
        
        # Composite metrics
        composite_cols <- grep("_vs_|_ratio|_index", names(rv$combined_data), value = TRUE)
        for (col in composite_cols) {
          metrics[[col]] <- paste0(
            paste0(
              toupper(substr(col, 1, 1)),
              substr(col, 2, nchar(col))
            ),
            " (Composite)"
          )
        }
      }
      
      rv$metrics_list <- metrics
    })
    
    # Update data when refresh button is clicked
    observeEvent(input$refresh_data, {
      # Determine year range
      years <- switch(input$selected_years,
                      "last5" = (as.numeric(format(Sys.Date(), "%Y")) - 4):as.numeric(format(Sys.Date(), "%Y")),
                      "last10" = (as.numeric(format(Sys.Date(), "%Y")) - 9):as.numeric(format(Sys.Date(), "%Y")),
                      "all" = 2000:as.numeric(format(Sys.Date(), "%Y")),
                      config$default_years)
      
      # Load traffic data with user settings
      withProgress(
        message = "Refreshing traffic safety data...",
        value = 0,
        {
          rv$traffic_data <- load_traffic_safety_data(
            years = years,
            refresh_cache = TRUE,
            allow_interpolation = input$allow_interpolation,
            allow_simulation = FALSE
          )
          setProgress(0.5)
          
          rv$transport_data <- load_transportation_infrastructure_data(
            years = years,
            refresh_cache = TRUE
          )
          setProgress(0.8)
          
          # Merge datasets
          rv$combined_data <- merge_transportation_data(rv$traffic_data, rv$transport_data)
          
          # Calculate derived metrics
          rv$combined_data <- calculate_derived_metrics(rv$combined_data)
          
          # Update available years
          rv$available_years <- sort(unique(rv$combined_data$year))
          
          setProgress(1)
        }
      )
    })
    
    # Update UI dropdowns with available years
    observe({
      req(rv$available_years)
      
      years <- rv$available_years
      most_recent <- max(years)
      
      # Update all year dropdowns
      updateSelectInput(session, "safety_year", choices = years, selected = most_recent)
      updateSelectInput(session, "infrastructure_year", choices = years, selected = most_recent)
      updateSelectInput(session, "infra_safety_year", choices = years, selected = most_recent)
    })
    
    # Update state selections in UI
    observe({
      req(rv$combined_data)
      
      if ("state_name" %in% names(rv$combined_data)) {
        states <- sort(unique(rv$combined_data$state_name))
        state_choices <- c("All States" = "all", setNames(states, states))
        
        updateSelectInput(session, "safety_state", choices = state_choices)
        updateSelectInput(session, "infrastructure_state", choices = state_choices)
        updateSelectInput(session, "county_explorer_state", choices = setNames(states, states))
      }
    })
    
    # Update county selections based on selected state
    observe({
      req(rv$combined_data, input$county_explorer_state)
      
      if (all(c("state_name", "county_name") %in% names(rv$combined_data))) {
        counties <- rv$combined_data %>%
          filter(state_name == input$county_explorer_state) %>%
          select(fips, county_name) %>%
          distinct()
        
        county_choices <- setNames(counties$fips, counties$county_name)
        
        updateSelectInput(session, "county_explorer_county", choices = county_choices)
      }
    })
    
    # Update comparison counties dropdown
    observe({
      req(rv$combined_data, input$county_explorer_state, input$county_explorer_county)
      
      if (all(c("state_name", "county_name") %in% names(rv$combined_data))) {
        # Get all counties in the same state except the selected one
        counties <- rv$combined_data %>%
          filter(
            state_name == input$county_explorer_state,
            fips != input$county_explorer_county
          ) %>%
          select(fips, county_name) %>%
          distinct()
        
        county_choices <- setNames(counties$fips, counties$county_name)
        
        # Get five counties with most similar population
        if ("population" %in% names(rv$combined_data) && nrow(counties) > 0) {
          selected_county_pop <- rv$combined_data %>%
            filter(fips == input$county_explorer_county) %>%
            pull(population) %>%
            na.omit() %>%
            mean(na.rm = TRUE)
          
          if (!is.na(selected_county_pop) && selected_county_pop > 0) {
            similar_counties <- rv$combined_data %>%
              filter(fips %in% counties$fips) %>%
              group_by(fips, county_name) %>%
              summarize(
                avg_population = mean(population, na.rm = TRUE),
                pop_diff = abs(avg_population - selected_county_pop),
                .groups = "drop"
              ) %>%
              arrange(pop_diff) %>%
              head(5)
            
            preselected <- similar_counties$fips
          } else {
            preselected <- head(counties$fips, 3)
          }
        } else {
          preselected <- head(counties$fips, 3)
        }
        
        updateSelectInput(session, "county_comparison_counties", 
                          choices = county_choices,
                          selected = preselected)
      }
    })
    
    # Dashboard Overview Tab - Info Boxes
    output$fatality_rate_box <- renderInfoBox({
      req(rv$combined_data)
      
      most_recent_year <- max(rv$combined_data$year, na.rm = TRUE)
      
      # Calculate national average for most recent year
      avg_rate <- rv$combined_data %>%
        filter(year == most_recent_year) %>%
        summarize(avg = mean(traffic_fatality_rate_per_100k, na.rm = TRUE)) %>%
        pull(avg)
      
      # Calculate trend (compared to previous year)
      prev_year_avg <- rv$combined_data %>%
        filter(year == most_recent_year - 1) %>%
        summarize(avg = mean(traffic_fatality_rate_per_100k, na.rm = TRUE)) %>%
        pull(avg)
      
      trend <- if (length(prev_year_avg) > 0 && !is.na(prev_year_avg)) {
        percent_change <- (avg_rate - prev_year_avg) / prev_year_avg * 100
        
        if (percent_change > 1) {
          icon("arrow-up", class = "text-danger")
        } else if (percent_change < -1) {
          icon("arrow-down", class = "text-success")
        } else {
          icon("equals", class = "text-muted")
        }
      } else {
        NULL
      }
      
      # Format the value
      formatted_value <- formatC(avg_rate, digits = 1, format = "f")
      
      infoBox(
        "Traffic Fatality Rate",
        paste0(formatted_value, " per 100k"),
        icon = icon("car-crash"),
        color = "red",
        subtitle = paste("National Avg,", most_recent_year),
        fill = TRUE
      )
    })
    
    output$injury_rate_box <- renderInfoBox({
      req(rv$combined_data)
      
      most_recent_year <- max(rv$combined_data$year, na.rm = TRUE)
      
      # Calculate national average for most recent year if data available
      if ("traffic_injury_rate_per_100k" %in% names(rv$combined_data)) {
        avg_rate <- rv$combined_data %>%
          filter(year == most_recent_year) %>%
          summarize(avg = mean(traffic_injury_rate_per_100k, na.rm = TRUE)) %>%
          pull(avg)
        
        # Format the value
        formatted_value <- formatC(avg_rate, digits = 1, format = "f")
      } else {
        formatted_value <- "N/A"
      }
      
      infoBox(
        "Traffic Injury Rate",
        paste0(formatted_value, " per 100k"),
        icon = icon("ambulance"),
        color = "yellow",
        subtitle = paste("National Avg,", most_recent_year),
        fill = TRUE
      )
    })
    
    output$dui_rate_box <- renderInfoBox({
      req(rv$combined_data)
      
      most_recent_year <- max(rv$combined_data$year, na.rm = TRUE)
      
      if ("dui_fatality_rate_per_100k" %in% names(rv$combined_data)) {
        avg_rate <- rv$combined_data %>%
          filter(year == most_recent_year) %>%
          summarize(avg = mean(dui_fatality_rate_per_100k, na.rm = TRUE)) %>%
          pull(avg)
        
        formatted_value <- formatC(avg_rate, digits = 1, format = "f")
      } else {
        formatted_value <- "N/A"
      }
      
      infoBox(
        "DUI Fatality Rate",
        paste0(formatted_value, " per 100k"),
        icon = icon("wine-bottle"),
        color = "orange",
        subtitle = paste("National Avg,", most_recent_year),
        fill = TRUE
      )
    })
    
    output$ped_bike_rate_box <- renderInfoBox({
      req(rv$combined_data)
      
      most_recent_year <- max(rv$combined_data$year, na.rm = TRUE)
      
      if ("ped_bike_fatality_rate_per_100k" %in% names(rv$combined_data)) {
        avg_rate <- rv$combined_data %>%
          filter(year == most_recent_year) %>%
          summarize(avg = mean(ped_bike_fatality_rate_per_100k, na.rm = TRUE)) %>%
          pull(avg)
        
        formatted_value <- formatC(avg_rate, digits = 1, format = "f")
      } else {
        formatted_value <- "N/A"
      }
      
      infoBox(
        "Pedestrian/Cyclist Rate",
        paste0(formatted_value, " per 100k"),
        icon = icon("walking"),
        color = "purple",
        subtitle = paste("National Avg,", most_recent_year),
        fill = TRUE
      )
    })
    
    # Dashboard Overview Tab - Main Trend Plot
    output$dashboard_trend_plot <- renderPlotly({
      req(rv$combined_data)
      
      # Prepare data for trend plot
      trend_data <- rv$combined_data %>%
        group_by(year) %>%
        summarize(
          fatality_rate = mean(traffic_fatality_rate_per_100k, na.rm = TRUE),
          dui_rate = mean(dui_fatality_rate_per_100k, na.rm = TRUE),
          ped_bike_rate = mean(ped_bike_fatality_rate_per_100k, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        filter(!is.na(fatality_rate))
      
      # Create plot
      plot_ly(trend_data, x = ~year) %>%
        add_trace(
          y = ~fatality_rate,
          name = "Traffic Fatality Rate",
          type = "scatter",
          mode = "lines+markers",
          line = list(color = "#dc3545", width = 3),
          marker = list(color = "#dc3545", size = 8)
        ) %>%
        add_trace(
          y = ~dui_rate,
          name = "DUI Fatality Rate",
          type = "scatter",
          mode = "lines+markers",
          line = list(color = "#fd7e14", width = 3),
          marker = list(color = "#fd7e14", size = 8)
        ) %>%
        add_trace(
          y = ~ped_bike_rate,
          name = "Pedestrian/Cyclist Rate",
          type = "scatter",
          mode = "lines+markers",
          line = list(color = "#6f42c1", width = 3),
          marker = list(color = "#6f42c1", size = 8)
        ) %>%
        layout(
          title = "Traffic Safety Trends Over Time",
          xaxis = list(title = "Year"),
          yaxis = list(title = "Fatalities per 100,000 Population"),
          legend = list(orientation = "h", x = 0.5, xanchor = "center"),
          hovermode = "x unified"
        )
    })
    
    # Dashboard Overview Tab - Key Metrics Summary
    output$key_metrics_summary <- renderUI({
      req(rv$combined_data)
      
      most_recent_year <- max(rv$combined_data$year, na.rm = TRUE)
      prev_year <- most_recent_year - 1
      
      metrics_summary <- rv$combined_data %>%
        filter(year %in% c(prev_year, most_recent_year)) %>%
        group_by(year) %>%
        summarize(
          fatality_rate = mean(traffic_fatality_rate_per_100k, na.rm = TRUE),
          dui_rate = mean(dui_fatality_rate_per_100k, na.rm = TRUE),
          ped_bike_rate = mean(ped_bike_fatality_rate_per_100k, na.rm = TRUE),
          high_fatality_counties = sum(traffic_fatality_rate_per_100k > 
                                     mean(traffic_fatality_rate_per_100k, na.rm = TRUE) +
                                     sd(traffic_fatality_rate_per_100k, na.rm = TRUE),
                                     na.rm = TRUE),
          total_counties = n(),
          .groups = "drop"
        ) %>%
        pivot_wider(
          names_from = year,
          values_from = c(fatality_rate, dui_rate, ped_bike_rate, 
                          high_fatality_counties, total_counties)
        )
      
      # Calculate percent changes if we have both years
      if (ncol(metrics_summary) >= 10) {
        fatality_change <- (metrics_summary[[paste0("fatality_rate_", most_recent_year)]] - 
                           metrics_summary[[paste0("fatality_rate_", prev_year)]]) / 
          metrics_summary[[paste0("fatality_rate_", prev_year)]] * 100
        
        dui_change <- (metrics_summary[[paste0("dui_rate_", most_recent_year)]] - 
                      metrics_summary[[paste0("dui_rate_", prev_year)]]) / 
          metrics_summary[[paste0("dui_rate_", prev_year)]] * 100
        
        ped_bike_change <- (metrics_summary[[paste0("ped_bike_rate_", most_recent_year)]] - 
                           metrics_summary[[paste0("ped_bike_rate_", prev_year)]]) / 
          metrics_summary[[paste0("ped_bike_rate_", prev_year)]] * 100
        
        high_risk_change <- (metrics_summary[[paste0("high_fatality_counties_", most_recent_year)]] - 
                            metrics_summary[[paste0("high_fatality_counties_", prev_year)]]) / 
          metrics_summary[[paste0("high_fatality_counties_", prev_year)]] * 100
        
        # Generate colored icon based on direction of change
        fatality_icon <- if(fatality_change > 1) {
          tags$span(icon("arrow-up"), class = "text-danger")
        } else if(fatality_change < -1) {
          tags$span(icon("arrow-down"), class = "text-success")
        } else {
          tags$span(icon("equals"), class = "text-muted")
        }
        
        dui_icon <- if(dui_change > 1) {
          tags$span(icon("arrow-up"), class = "text-danger")
        } else if(dui_change < -1) {
          tags$span(icon("arrow-down"), class = "text-success")
        } else {
          tags$span(icon("equals"), class = "text-muted")
        }
        
        ped_bike_icon <- if(ped_bike_change > 1) {
          tags$span(icon("arrow-up"), class = "text-danger")
        } else if(ped_bike_change < -1) {
          tags$span(icon("arrow-down"), class = "text-success")
        } else {
          tags$span(icon("equals"), class = "text-muted")
        }
        
        high_risk_icon <- if(high_risk_change > 1) {
          tags$span(icon("arrow-up"), class = "text-danger")
        } else if(high_risk_change < -1) {
          tags$span(icon("arrow-down"), class = "text-success")
        } else {
          tags$span(icon("equals"), class = "text-muted")
        }
        
        # Format summary text
        HTML(paste0(
          "<h4>Year-over-Year Change (", prev_year, " to ", most_recent_year, ")</h4>",
          "<p><strong>Traffic Fatality Rate:</strong> ", 
          round(fatality_change, 1), "% ", fatality_icon, "</p>",
          "<p><strong>DUI Fatality Rate:</strong> ", 
          round(dui_change, 1), "% ", dui_icon, "</p>",
          "<p><strong>Pedestrian/Cyclist Rate:</strong> ", 
          round(ped_bike_change, 1), "% ", ped_bike_icon, "</p>",
          "<p><strong>High-Risk Counties:</strong> ", 
          metrics_summary[[paste0("high_fatality_counties_", most_recent_year)]], " of ",
          metrics_summary[[paste0("total_counties_", most_recent_year)]], " counties ",
          high_risk_icon, "</p>",
          "<p><small>High-risk counties have fatality rates more than 1 standard deviation above the national average.</small></p>"
        ))
      } else {
        # If we don't have both years, show just the most recent data
        HTML(paste0(
          "<h4>Key Metrics for ", most_recent_year, "</h4>",
          "<p><strong>Traffic Fatality Rate:</strong> ", 
          round(metrics_summary[[paste0("fatality_rate_", most_recent_year)]], 1), 
          " per 100,000</p>",
          "<p><strong>DUI Fatality Rate:</strong> ", 
          round(metrics_summary[[paste0("dui_rate_", most_recent_year)]], 1), 
          " per 100,000</p>",
          "<p><strong>Pedestrian/Cyclist Rate:</strong> ", 
          round(metrics_summary[[paste0("ped_bike_rate_", most_recent_year)]], 1), 
          " per 100,000</p>",
          "<p><strong>High-Risk Counties:</strong> ", 
          metrics_summary[[paste0("high_fatality_counties_", most_recent_year)]], " of ",
          metrics_summary[[paste0("total_counties_", most_recent_year)]], " counties</p>",
          "<p><small>High-risk counties have fatality rates more than 1 standard deviation above the national average.</small></p>"
        ))
      }
    })
    
    # Dashboard Overview Tab - Map
    output$dashboard_map <- renderLeaflet({
      req(rv$combined_data, rv$county_shapes)
      
      most_recent_year <- max(rv$combined_data$year, na.rm = TRUE)
      
      # Prepare data for mapping
      map_data <- rv$combined_data %>%
        filter(year == most_recent_year) %>%
        select(fips, traffic_fatality_rate_per_100k) %>%
        filter(!is.na(traffic_fatality_rate_per_100k))
      
      # Join with county shapes
      map_data$GEOID <- map_data$fips
      county_map <- rv$county_shapes %>%
        left_join(map_data, by = "GEOID")
      
      # Create color palette
      pal <- colorNumeric(
        palette = "YlOrRd",
        domain = map_data$traffic_fatality_rate_per_100k,
        na.color = "#CCCCCC"
      )
      
      # Create map
      leaflet(county_map) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          fillColor = ~pal(traffic_fatality_rate_per_100k),
          weight = 0.5,
          opacity = 1,
          color = "white",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            weight = 2,
            color = "#666",
            fillOpacity = 0.9,
            bringToFront = TRUE
          ),
          popup = ~paste0(
            "<strong>", NAME, ", ", STUSPS, "</strong><br/>",
            "Fatality Rate: ", round(traffic_fatality_rate_per_100k, 1), " per 100k"
          ),
          label = ~paste0(NAME, ": ", round(traffic_fatality_rate_per_100k, 1))
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = map_data$traffic_fatality_rate_per_100k,
          title = "Fatalities per 100k",
          opacity = 0.7,
          labFormat = labelFormat(transform = function(x) round(x, 1))
        )
    })
    
    # Dashboard Overview Tab - Forecast
    output$dashboard_forecast <- renderPlotly({
      req(rv$combined_data, input$show_forecast)
      
      # Get data and create forecast
      if (!"traffic_fatality_rate_per_100k_forecast" %in% names(rv$forecasts)) {
        forecast_data <- create_forecast(
          rv$combined_data,
          "traffic_fatality_rate_per_100k",
          forecast_years = 3
        )
        
        rv$forecasts$traffic_fatality_rate_per_100k_forecast <- forecast_data
      } else {
        forecast_data <- rv$forecasts$traffic_fatality_rate_per_100k_forecast
      }
      
      # Skip if forecast failed
      if (is.null(forecast_data)) {
        return(NULL)
      }
      
      # Create plot
      plot_ly() %>%
        add_trace(
          data = subset(forecast_data, type == "historical"),
          x = ~year,
          y = ~value,
          name = "Historical",
          type = "scatter",
          mode = "lines+markers",
          line = list(color = "#dc3545", width = 3),
          marker = list(color = "#dc3545", size = 8)
        ) %>%
        add_trace(
          data = subset(forecast_data, type == "forecast"),
          x = ~year,
          y = ~value,
          name = "Forecast",
          type = "scatter",
          mode = "lines",
          line = list(color = "#fd7e14", width = 3, dash = "dash")
        ) %>%
        add_trace(
          data = subset(forecast_data, type == "forecast"),
          x = ~year,
          y = ~upper_80,
          name = "80% Upper",
          type = "scatter",
          mode = "lines",
          line = list(color = "rgba(255,165,0,0.3)", width = 0),
          showlegend = FALSE
        ) %>%
        add_trace(
          data = subset(forecast_data, type == "forecast"),
          x = ~year,
          y = ~lower_80,
          name = "80% Lower",
          type = "scatter",
          mode = "lines",
          fill = "tonexty",
          fillcolor = "rgba(255,165,0,0.3)",
          line = list(color = "rgba(255,165,0,0.3)", width = 0),
          showlegend = FALSE
        ) %>%
        layout(
          title = "Traffic Fatality Rate Forecast",
          xaxis = list(title = "Year"),
          yaxis = list(title = "Fatalities per 100,000"),
          legend = list(orientation = "h"),
          hovermode = "x"
        )
    })
    
    # County Explorer Tab - County Metrics Cards
    output$county_metrics_cards <- renderUI({
      req(rv$combined_data, input$county_explorer_county)
      
      # Get the selected county's data
      county_data <- rv$combined_data %>%
        filter(fips == input$county_explorer_county) %>%
        arrange(desc(year))
      
      # Skip if no data
      if (nrow(county_data) == 0) {
        return(HTML("<p>No data available for the selected county.</p>"))
      }
      
      # Get most recent year's data
      most_recent <- county_data[1, ]
      county_name <- most_recent$county_name
      year <- most_recent$year
      
      # Create metric cards for available metrics
      cards <- list()
      
      if ("traffic_fatality_rate_per_100k" %in% names(most_recent)) {
        # For fatality rate, calculate trend if we have previous year
        trend_icon <- NULL
        if (nrow(county_data) > 1) {
          prev_rate <- county_data$traffic_fatality_rate_per_100k[2]
          curr_rate <- most_recent$traffic_fatality_rate_per_100k
          
          if (!is.na(prev_rate) && !is.na(curr_rate)) {
            percent_change <- (curr_rate - prev_rate) / prev_rate * 100
            
            trend_text <- paste0(round(abs(percent_change), 1), "% ")
            
            if (percent_change > 1) {
              trend_icon <- tags$span(trend_text, icon("arrow-up"), class = "trend-up")
            } else if (percent_change < -1) {
              trend_icon <- tags$span(trend_text, icon("arrow-down"), class = "trend-down")
            } else {
              trend_icon <- tags$span(trend_text, icon("equals"), class = "trend-neutral")
            }
          }
        }
        
        cards[[1]] <- div(
          class = "metric-card",
          div(class = "metric-title", "Traffic Fatality Rate"),
          div(
            class = "metric-value",
            formatC(most_recent$traffic_fatality_rate_per_100k, digits = 1, format = "f"),
            " per 100k"
          ),
          div(
            "vs previous year: ", trend_icon
          )
        )
      }
      
      if ("dui_fatality_rate_per_100k" %in% names(most_recent)) {
        cards[[2]] <- div(
          class = "metric-card",
          div(class = "metric-title", "DUI Fatality Rate"),
          div(
            class = "metric-value",
            formatC(most_recent$dui_fatality_rate_per_100k, digits = 1, format = "f"),
            " per 100k"
          )
        )
      }
      
      if ("public_transit_pct" %in% names(most_recent)) {
        cards[[3]] <- div(
          class = "metric-card",
          div(class = "metric-title", "Public Transit Usage"),
          div(
            class = "metric-value",
            formatC(most_recent$public_transit_pct, digits = 1, format = "f"),
            "%"
          )
        )
      }
      
      if ("traffic_safety_index" %in% names(most_recent)) {
        # Determine color based on index value
        safety_color <- if (!is.na(most_recent$traffic_safety_index)) {
          if (most_recent$traffic_safety_index >= 75) {
            "green"
          } else if (most_recent$traffic_safety_index >= 50) {
            "orange"
          } else {
            "red"
          }
        } else {
          "gray"
        }
        
        cards[[4]] <- div(
          class = "metric-card",
          div(class = "metric-title", "Safety Index"),
          div(
            class = paste0("metric-value text-", safety_color),
            formatC(most_recent$traffic_safety_index, digits = 1, format = "f"),
            " / 100"
          ),
          div(
            "Higher is safer"
          )
        )
      }
      
      # Header for the section
      header <- h3(paste0(county_name, " (", year, ")"))
      
      # Combine all elements
      tagList(
        header,
        div(style = "display: flex; flex-wrap: wrap; gap: 10px;",
            lapply(cards, function(card) {
              div(style = "flex: 1 1 45%;", card)
            })
        )
      )
    })
    
    # County Explorer Tab - Time Series
    output$county_time_series <- renderPlotly({
      req(rv$combined_data, input$county_explorer_county, input$county_explorer_metric)
      
      # Get data for the selected county
      county_data <- rv$combined_data %>%
        filter(fips == input$county_explorer_county) %>%
        select(year, fips, county_name, !!sym(input$county_explorer_metric)) %>%
        filter(!is.na(!!sym(input$county_explorer_metric))) %>%
        arrange(year)
      
      # Skip if no data
      if (nrow(county_data) == 0) {
        return(NULL)
      }
      
      # Get metric name for display
      metric_name <- names(rv$metrics_list)[names(rv$metrics_list) == input$county_explorer_metric]
      if (length(metric_name) == 0) {
        metric_name <- input$county_explorer_metric
      } else {
        metric_name <- rv$metrics_list[[metric_name]]
      }
      
      # Create line chart
      plot_ly(county_data, x = ~year, y = ~get(input$county_explorer_metric)) %>%
        add_trace(
          type = "scatter",
          mode = "lines+markers",
          line = list(color = "#dc3545", width = 3),
          marker = list(color = "#dc3545", size = 8),
          name = county_data$county_name[1]
        ) %>%
        layout(
          title = paste0(metric_name, " in ", county_data$county_name[1]),
          xaxis = list(title = "Year"),
          yaxis = list(title = metric_name),
          showlegend = FALSE
        )
    })
    
    # County Explorer Tab - County Comparison
    output$county_comparison_chart <- renderPlotly({
      req(rv$combined_data, input$county_explorer_county, 
          input$county_comparison_counties, input$county_explorer_metric)
      
      # Combine selected county with comparison counties
      all_counties <- c(input$county_explorer_county, input$county_comparison_counties)
      
      # Get data for all selected counties
      counties_data <- rv$combined_data %>%
        filter(fips %in% all_counties) %>%
        select(year, fips, county_name, !!sym(input$county_explorer_metric)) %>%
        filter(!is.na(!!sym(input$county_explorer_metric))) %>%
        arrange(year)
      
      # Skip if no data
      if (nrow(counties_data) == 0) {
        return(NULL)
      }
      
      # Get metric name for display
      metric_name <- names(rv$metrics_list)[names(rv$metrics_list) == input$county_explorer_metric]
      if (length(metric_name) == 0) {
        metric_name <- input$county_explorer_metric
      } else {
        metric_name <- rv$metrics_list[[metric_name]]
      }
      
      # Highlight the main county
      main_county <- input$county_explorer_county
      
      # Create line chart with all counties
      p <- plot_ly()
      
      # Add comparison counties first (in gray)
      for (county in input$county_comparison_counties) {
        county_subset <- counties_data %>% filter(fips == county)
        
        if (nrow(county_subset) > 0) {
          p <- p %>% add_trace(
            data = county_subset,
            x = ~year,
            y = ~get(input$county_explorer_metric),
            type = "scatter",
            mode = "lines",
            line = list(color = "#6c757d", width = 2, opacity = 0.6),
            name = county_subset$county_name[1]
          )
        }
      }
      
      # Add main county last (in red, thicker)
      main_subset <- counties_data %>% filter(fips == main_county)
      
      if (nrow(main_subset) > 0) {
        p <- p %>% add_trace(
          data = main_subset,
          x = ~year,
          y = ~get(input$county_explorer_metric),
          type = "scatter",
          mode = "lines+markers",
          line = list(color = "#dc3545", width = 4),
          marker = list(color = "#dc3545", size = 8),
          name = main_subset$county_name[1]
        )
      }
      
      # Add layout
      p %>% layout(
        title = paste0("County Comparison: ", metric_name),
        xaxis = list(title = "Year"),
        yaxis = list(title = metric_name),
        legend = list(orientation = "h", x = 0.5, xanchor = "center"),
        hovermode = "closest"
      )
    })
    
    # When the application is closed
    onSessionEnded(function() {
      # Clean up resources if needed
      message("Closing transportation metrics dashboard")
    })
  }
  
  # Create and run the Shiny app
  app <- shinyApp(
    ui = dashboardPage(header, sidebar, body),
    server = server
  )
  
  # Launch the app
  if (launch_browser) {
    runApp(app, host = host, port = port, launch.browser = TRUE)
  } else {
    return(app)
  }
}

# Run the dashboard if this script is executed directly
if (!interactive()) {
  # Load traffic safety data
  traffic_data <- load_traffic_safety_data()
  
  # Load transportation infrastructure data
  transport_data <- load_transportation_infrastructure_data()
  
  # Launch the dashboard
  launch_traffic_safety_dashboard(
    traffic_data = traffic_data,
    transport_data = transport_data
  )
}