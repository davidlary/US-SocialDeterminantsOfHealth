#!/usr/bin/env Rscript

# Interactive SDOH Visualization Dashboard
# This script creates an interactive Shiny dashboard for exploring
# county-level Social Determinants of Health data

# Load required packages
required_packages <- c(
  "shiny", "shinydashboard", "plotly", "leaflet", "DT", "dplyr", 
  "tidyr", "ggplot2", "sf", "duckdb", "DBI", "scales", "viridis", 
  "tigris", "htmltools", "shinyWidgets", "shinycssloaders", "stringr"
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
  db_path = "output/us_county_sdoh_unified.duckdb",
  shapefile_dir = "data/shapefiles",
  cache_dir = "data/cache",
  theme_color = "blue",
  map_providers = list(
    "Esri.WorldGrayCanvas",
    "CartoDB.Positron",
    "OpenStreetMap",
    "Stamen.Terrain"
  )
)

#' Launch the SDOH interactive dashboard
#'
#' @param db_path Path to the DuckDB database
#' @param port Port to run the Shiny app on
#' @param host Host to bind the Shiny app to
#' @param launch_browser Whether to open the app in a browser
#'
#' @return The Shiny app object
launch_sdoh_dashboard <- function(
  db_path = config$db_path,
  port = 3838, 
  host = "0.0.0.0",
  launch_browser = TRUE
) {
  
  # ---- Data Loading Functions ----
  
  # Connect to the database
  connect_to_db <- function(db_path) {
    if (!file.exists(db_path)) {
      stop("Database file not found at: ", db_path)
    }
    conn <- dbConnect(duckdb::duckdb(), dbdir = db_path)
    return(conn)
  }
  
  # Get the list of available variables
  get_variables <- function(conn) {
    variables <- dbGetQuery(conn, "SELECT * FROM variables ORDER BY domain, variable_name")
    return(variables)
  }
  
  # Get county data for a specific variable and year
  get_county_data <- function(conn, variable_name, year = NULL) {
    if (is.null(year)) {
      # Get the most recent year for this variable
      query <- glue::glue_sql("
        SELECT MAX(year) as max_year
        FROM sdoh_data
        WHERE variable_name = {variable_name}
      ", .con = conn)
      
      result <- dbGetQuery(conn, query)
      year <- result$max_year[1]
      
      if (is.na(year)) {
        return(NULL)
      }
    }
    
    query <- glue::glue_sql("
      SELECT 
        s.geoid, 
        c.name as county_name, 
        c.state_name,
        s.year, 
        s.variable_name, 
        s.value,
        s.data_quality, 
        s.data_source,
        s.interpolation_method
      FROM sdoh_data s
      JOIN counties c ON s.geoid = c.geoid
      WHERE s.variable_name = {variable_name}
        AND s.year = {year}
    ", .con = conn)
    
    data <- dbGetQuery(conn, query)
    
    return(data)
  }
  
  # Get time series data for a specific variable and county
  get_time_series <- function(conn, variable_name, geoid = NULL) {
    if (is.null(geoid)) {
      # Get data for all counties
      query <- glue::glue_sql("
        SELECT 
          s.geoid, 
          c.name as county_name, 
          c.state_name,
          s.year, 
          s.variable_name, 
          s.value,
          s.data_quality, 
          s.data_source
        FROM sdoh_data s
        JOIN counties c ON s.geoid = c.geoid
        WHERE s.variable_name = {variable_name}
        ORDER BY s.year
      ", .con = conn)
    } else {
      # Get data for specific county
      query <- glue::glue_sql("
        SELECT 
          s.geoid, 
          c.name as county_name, 
          c.state_name,
          s.year, 
          s.variable_name, 
          s.value,
          s.data_quality, 
          s.data_source
        FROM sdoh_data s
        JOIN counties c ON s.geoid = c.geoid
        WHERE s.variable_name = {variable_name}
          AND s.geoid = {geoid}
        ORDER BY s.year
      ", .con = conn)
    }
    
    data <- dbGetQuery(conn, query)
    
    return(data)
  }
  
  # Get correlation data for two variables
  get_correlation_data <- function(conn, var1, var2, year = NULL) {
    if (is.null(year)) {
      # Get the most recent common year for both variables
      query <- glue::glue_sql("
        WITH var1_years AS (
          SELECT DISTINCT year FROM sdoh_data WHERE variable_name = {var1}
        ),
        var2_years AS (
          SELECT DISTINCT year FROM sdoh_data WHERE variable_name = {var2}
        )
        SELECT MAX(year) as max_year
        FROM var1_years 
        WHERE year IN (SELECT year FROM var2_years)
      ", .con = conn)
      
      result <- dbGetQuery(conn, query)
      year <- result$max_year[1]
      
      if (is.na(year)) {
        return(NULL)
      }
    }
    
    query <- glue::glue_sql("
      WITH var1_data AS (
        SELECT 
          s.geoid, 
          c.name as county_name, 
          c.state_name,
          s.value as var1_value,
          s.data_quality as var1_quality
        FROM sdoh_data s
        JOIN counties c ON s.geoid = c.geoid
        WHERE s.variable_name = {var1}
          AND s.year = {year}
      ),
      var2_data AS (
        SELECT 
          s.geoid, 
          s.value as var2_value,
          s.data_quality as var2_quality
        FROM sdoh_data s
        WHERE s.variable_name = {var2}
          AND s.year = {year}
      )
      SELECT 
        v1.geoid, 
        v1.county_name, 
        v1.state_name,
        v1.var1_value,
        v2.var2_value,
        v1.var1_quality,
        v2.var2_quality
      FROM var1_data v1
      JOIN var2_data v2 ON v1.geoid = v2.geoid
      WHERE v1.var1_value IS NOT NULL AND v2.var2_value IS NOT NULL
    ", .con = conn)
    
    data <- dbGetQuery(conn, query)
    
    return(data)
  }
  
  # Load counties shapefile
  load_county_shapes <- function(year = 2020) {
    shapefile_path <- file.path(config$shapefile_dir, paste0("counties_", year, ".rds"))
    
    if (file.exists(shapefile_path)) {
      # Use cached shapefile
      shapes <- readRDS(shapefile_path)
    } else {
      # Download from tigris if not available
      shapes <- tigris::counties(cb = TRUE, year = year)
      
      # Create directory if it doesn't exist
      if (!dir.exists(config$shapefile_dir)) {
        dir.create(config$shapefile_dir, recursive = TRUE)
      }
      
      # Cache for future use
      saveRDS(shapes, shapefile_path)
    }
    
    return(shapes)
  }
  
  # ---- UI Components ----
  
  # Header
  header <- dashboardHeader(
    title = "SDOH Explorer",
    dropdownMenu(
      type = "notifications", 
      icon = icon("info-circle"),
      badgeStatus = NULL,
      headerText = "About",
      notificationItem(
        text = "Unified SDOH County-Level Dataset",
        icon = icon("database")
      ),
      notificationItem(
        text = "Data from Census, CDC, NHGIS and more",
        icon = icon("chart-bar")
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
      menuItem("Map Explorer", tabName = "map", icon = icon("map")),
      menuItem("Time Trends", tabName = "trends", icon = icon("chart-line")),
      menuItem("Correlations", tabName = "correlations", icon = icon("project-diagram")),
      menuItem("Data Table", tabName = "table", icon = icon("table")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    conditionalPanel(
      condition = "input.tabs == 'map'",
      selectInput("map_variable", "Variable", choices = NULL),
      selectInput("map_year", "Year", choices = NULL),
      selectInput("map_color_scale", "Color Scale", 
                  choices = c("Viridis" = "viridis", 
                             "Magma" = "magma", 
                             "Plasma" = "plasma",
                             "Inferno" = "inferno",
                             "Cividis" = "cividis",
                             "Blues" = "Blues",
                             "Reds" = "Reds", 
                             "Greens" = "Greens")),
      checkboxInput("map_show_legend", "Show Legend", value = TRUE),
      selectInput("map_provider", "Map Provider", choices = config$map_providers),
      downloadButton("download_map_data", "Download Map Data")
    ),
    conditionalPanel(
      condition = "input.tabs == 'trends'",
      selectInput("trend_variable", "Variable", choices = NULL),
      selectInput("trend_county", "County", choices = NULL),
      checkboxInput("trend_show_all_counties", "Show All Counties", value = FALSE),
      checkboxInput("trend_highlight_selected", "Highlight Selected County", value = TRUE),
      radioButtons("trend_plot_type", "Plot Type", 
                   choices = c("Line" = "line", "Bar" = "bar"),
                   selected = "line", inline = TRUE),
      downloadButton("download_trend_data", "Download Trend Data")
    ),
    conditionalPanel(
      condition = "input.tabs == 'correlations'",
      selectInput("corr_variable_x", "X Variable", choices = NULL),
      selectInput("corr_variable_y", "Y Variable", choices = NULL),
      selectInput("corr_year", "Year", choices = NULL),
      checkboxInput("corr_show_regression", "Show Regression Line", value = TRUE),
      sliderInput("corr_opacity", "Point Opacity", min = 0.1, max = 1, value = 0.7, step = 0.1),
      downloadButton("download_correlation_data", "Download Correlation Data")
    ),
    conditionalPanel(
      condition = "input.tabs == 'table'",
      selectInput("table_variable", "Variable", choices = NULL),
      selectInput("table_year", "Year", choices = NULL),
      selectInput("table_state", "State", choices = NULL),
      textInput("table_search", "Search Counties", ""),
      downloadButton("download_table_data", "Download Table Data")
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
        #map_explorer, #trend_plot, #correlation_plot {
          height: calc(100vh - 230px);
          min-height: 500px;
        }
        .leaflet-container {
          background: #f8f9fa;
        }
        .dataTables_wrapper {
          padding: 10px;
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
      "))
    ),
    
    tabItems(
      # Map Explorer Tab
      tabItem(
        tabName = "map",
        fluidRow(
          box(
            title = "County Map Explorer",
            width = 9,
            status = "primary",
            solidHeader = TRUE,
            leafletOutput("map_explorer") %>% withSpinner(color = "#0275d8")
          ),
          box(
            title = "Variable Information",
            width = 3,
            status = "info",
            solidHeader = TRUE,
            htmlOutput("map_variable_info"),
            uiOutput("map_stats")
          )
        ),
        fluidRow(
          box(
            title = "Selected County Information",
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            htmlOutput("selected_county_info"),
            plotlyOutput("selected_county_trend", height = "200px") %>% withSpinner(color = "#0275d8")
          )
        )
      ),
      
      # Time Trends Tab
      tabItem(
        tabName = "trends",
        fluidRow(
          box(
            title = "Time Trends Explorer",
            width = 9,
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("trend_plot") %>% withSpinner(color = "#0275d8")
          ),
          box(
            title = "Variable Information",
            width = 3,
            status = "info",
            solidHeader = TRUE,
            htmlOutput("trend_variable_info"),
            uiOutput("trend_stats")
          )
        ),
        fluidRow(
          box(
            title = "Data Summary Table",
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            DTOutput("trend_data_table") %>% withSpinner(color = "#0275d8")
          )
        )
      ),
      
      # Correlations Tab
      tabItem(
        tabName = "correlations",
        fluidRow(
          box(
            title = "Variable Correlation Explorer",
            width = 9,
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("correlation_plot") %>% withSpinner(color = "#0275d8")
          ),
          box(
            title = "Correlation Information",
            width = 3,
            status = "info",
            solidHeader = TRUE,
            htmlOutput("correlation_info"),
            uiOutput("correlation_stats")
          )
        ),
        fluidRow(
          box(
            title = "Data Summary Table",
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            DTOutput("correlation_data_table") %>% withSpinner(color = "#0275d8")
          )
        )
      ),
      
      # Data Table Tab
      tabItem(
        tabName = "table",
        fluidRow(
          box(
            title = "Data Explorer",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            DTOutput("data_table") %>% withSpinner(color = "#0275d8")
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
              <h3>Unified Social Determinants of Health County-Level Dataset Explorer</h3>
              <p>This interactive dashboard provides tools to explore county-level data on social determinants of health across the United States.</p>
              
              <h4>Data Sources</h4>
              <ul>
                <li><strong>IPUMS NHGIS</strong> - Primary source for harmonized time series data (1970-present)</li>
                <li><strong>U.S. Census Bureau</strong> - American Community Survey and other Census programs</li>
                <li><strong>CDC PLACES</strong> - County-level health indicators</li>
                <li><strong>USDA Food Environment Atlas</strong> - Food access measures</li>
                <li><strong>EPA Environmental Data</strong> - Environmental quality measures</li>
                <li><strong>NOAA Climate Data</strong> - Climate and disaster risk metrics</li>
                <li><strong>SAMHSA Facility Data</strong> - Mental health and substance use treatment metrics</li>
                <li><strong>FCC Broadband Data</strong> - Digital access and connectivity metrics</li>
                <li><strong>Additional specialized data sources</strong> - For other SDOH domains</li>
              </ul>
              
              <h4>Data Quality Indicators</h4>
              <ul>
                <li><span class='data-quality-direct'>Direct</span> - Data directly from source without modification</li>
                <li><span class='data-quality-interpolated'>Interpolated</span> - Data filled in using interpolation methods</li>
                <li><span class='data-quality-extrapolated'>Extrapolated</span> - Data extended beyond available time range</li>
                <li><span class='data-quality-simulated'>Simulated</span> - Data generated using simulation methods</li>
              </ul>
              
              <h4>Usage Notes</h4>
              <ul>
                <li>Map Explorer - View county-level maps for any variable and year</li>
                <li>Time Trends - Analyze how variables change over time for specific counties</li>
                <li>Correlations - Explore relationships between different variables</li>
                <li>Data Table - View and filter the raw data</li>
              </ul>
              
              <h4>Citation</h4>
              <p>Unified Social Determinants of Health County-Level Dataset (2025). Generated using data from U.S. Census Bureau, CDC PLACES, IPUMS NHGIS, NOAA, SAMHSA, FCC, and other authoritative sources for comprehensive social determinants of health analyses.</p>
            ")
          )
        )
      )
    )
  )
  
  # ---- Server Function ----
  server <- function(input, output, session) {
    # Connect to the database
    conn <- connect_to_db(db_path)
    
    # Load county shapes
    county_shapes <- load_county_shapes()
    
    # Initialize variables list
    variables <- reactive({
      get_variables(conn)
    })
    
    # Update variable selection dropdowns when variables are loaded
    observe({
      vars <- variables()
      
      # Create a named vector for the dropdown
      var_choices <- setNames(
        vars$variable_name,
        paste0(vars$variable_name, " (", vars$domain, ")")
      )
      
      # Update all variable selectors
      updateSelectInput(session, "map_variable", choices = var_choices)
      updateSelectInput(session, "trend_variable", choices = var_choices)
      updateSelectInput(session, "corr_variable_x", choices = var_choices)
      updateSelectInput(session, "corr_variable_y", choices = var_choices)
      updateSelectInput(session, "table_variable", choices = var_choices)
    })
    
    # Get counties for dropdown
    counties <- reactive({
      dbGetQuery(conn, "SELECT geoid, name, state_name FROM counties ORDER BY state_name, name")
    })
    
    # Update county selection dropdown
    observe({
      county_list <- counties()
      
      # Create a named vector for the dropdown
      county_choices <- setNames(
        county_list$geoid,
        paste0(county_list$name, ", ", county_list$state_name)
      )
      
      # Update county selector
      updateSelectInput(session, "trend_county", choices = county_choices)
    })
    
    # Get states for dropdown
    observe({
      states <- dbGetQuery(conn, "SELECT DISTINCT state_name FROM counties ORDER BY state_name")
      
      # Add "All States" option
      state_choices <- c("All States" = "all", setNames(states$state_name, states$state_name))
      
      # Update state selector
      updateSelectInput(session, "table_state", choices = state_choices)
    })
    
    # ---- Map Explorer Tab ----
    
    # Update years dropdown based on selected variable
    observe({
      req(input$map_variable)
      
      query <- glue::glue_sql("
        SELECT DISTINCT year 
        FROM sdoh_data 
        WHERE variable_name = {input$map_variable}
        ORDER BY year DESC
      ", .con = conn)
      
      years <- dbGetQuery(conn, query)$year
      
      updateSelectInput(session, "map_year", 
                        choices = years,
                        selected = years[1])
    })
    
    # Get variable details
    variable_details <- reactive({
      req(input$map_variable)
      
      vars <- variables()
      vars[vars$variable_name == input$map_variable, ]
    })
    
    # Get map data
    map_data <- reactive({
      req(input$map_variable, input$map_year)
      
      data <- get_county_data(conn, input$map_variable, input$map_year)
      
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }
      
      return(data)
    })
    
    # Render variable information
    output$map_variable_info <- renderUI({
      req(variable_details())
      
      var <- variable_details()
      
      HTML(paste0(
        "<h4>", var$variable_name, "</h4>",
        "<p><strong>Domain:</strong> ", var$domain, "</p>",
        "<p><strong>Description:</strong> ", var$description, "</p>",
        "<p><strong>Units:</strong> ", var$units, "</p>",
        "<p><strong>Years Available:</strong> ", var$min_year, " to ", var$max_year, "</p>"
      ))
    })
    
    # Render map statistics
    output$map_stats <- renderUI({
      req(map_data())
      
      data <- map_data()
      
      # Calculate statistics
      stats <- list(
        min = min(data$value, na.rm = TRUE),
        max = max(data$value, na.rm = TRUE),
        mean = mean(data$value, na.rm = TRUE),
        median = median(data$value, na.rm = TRUE),
        sd = sd(data$value, na.rm = TRUE),
        counties = nrow(data),
        missing = sum(is.na(data$value)),
        direct = sum(data$data_quality == "direct", na.rm = TRUE),
        interpolated = sum(data$data_quality == "interpolated", na.rm = TRUE),
        extrapolated = sum(data$data_quality == "extrapolated", na.rm = TRUE),
        simulated = sum(data$data_quality == "simulated", na.rm = TRUE)
      )
      
      HTML(paste0(
        "<h4>Statistics for ", input$map_year, "</h4>",
        "<p><strong>Min:</strong> ", round(stats$min, 2), "</p>",
        "<p><strong>Max:</strong> ", round(stats$max, 2), "</p>",
        "<p><strong>Mean:</strong> ", round(stats$mean, 2), "</p>",
        "<p><strong>Median:</strong> ", round(stats$median, 2), "</p>",
        "<p><strong>Standard Deviation:</strong> ", round(stats$sd, 2), "</p>",
        "<h4>Data Quality</h4>",
        "<p><span class='data-quality-direct'>Direct:</span> ", stats$direct, " counties</p>",
        "<p><span class='data-quality-interpolated'>Interpolated:</span> ", stats$interpolated, " counties</p>",
        "<p><span class='data-quality-extrapolated'>Extrapolated:</span> ", stats$extrapolated, " counties</p>",
        "<p><span class='data-quality-simulated'>Simulated:</span> ", stats$simulated, " counties</p>",
        "<p><strong>Missing:</strong> ", stats$missing, " counties</p>"
      ))
    })
    
    # Render map
    output$map_explorer <- renderLeaflet({
      req(map_data())
      
      data <- map_data()
      var_info <- variable_details()
      
      # Join data to shapefile
      map_sf <- county_shapes %>%
        mutate(GEOID = as.character(GEOID)) %>%
        left_join(data, by = c("GEOID" = "geoid"))
      
      # Set up color palette
      pal_func <- colorNumeric(
        palette = input$map_color_scale,
        domain = data$value,
        na.color = "#CCCCCC"
      )
      
      # Create popup content
      popup_content <- paste0(
        "<strong>", map_sf$NAME, ", ", map_sf$STUSPS, "</strong><br/>",
        "<strong>", var_info$variable_name, " (", input$map_year, "):</strong> ", 
        round(map_sf$value, 2), " ", var_info$units, "<br/>",
        "<strong>Data Quality:</strong> ", map_sf$data_quality, "<br/>",
        "<strong>Data Source:</strong> ", map_sf$data_source
      )
      
      # Create the map
      leaflet(map_sf) %>%
        addProviderTiles(input$map_provider) %>%
        addPolygons(
          fillColor = ~pal_func(value),
          weight = 1,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 3,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          popup = popup_content,
          layerId = ~GEOID
        ) %>%
        {if(input$map_show_legend) addLegend(
          .,
          position = "bottomright",
          pal = pal_func,
          values = data$value,
          title = paste0(var_info$variable_name, "<br>(", var_info$units, ")"),
          opacity = 0.7,
          labFormat = labelFormat(transform = function(x) round(x, 2))
        ) else .}
    })
    
    # Store the selected county
    selected_county <- reactiveVal(NULL)
    
    # Handle map clicks
    observeEvent(input$map_explorer_shape_click, {
      click <- input$map_explorer_shape_click
      selected_county(click$id)
    })
    
    # Render selected county information
    output$selected_county_info <- renderUI({
      req(selected_county(), map_data())
      
      geoid <- selected_county()
      data <- map_data()
      var_info <- variable_details()
      
      county_data <- data[data$geoid == geoid, ]
      
      if (nrow(county_data) == 0) {
        return(HTML("<p>No data available for the selected county.</p>"))
      }
      
      HTML(paste0(
        "<h4>", county_data$county_name, ", ", county_data$state_name, " (FIPS: ", geoid, ")</h4>",
        "<p><strong>", var_info$variable_name, " (", input$map_year, "):</strong> ", 
        round(county_data$value, 2), " ", var_info$units, "</p>",
        "<p><strong>Data Quality:</strong> <span class='data-quality-", tolower(county_data$data_quality), "'>", 
        county_data$data_quality, "</span></p>",
        "<p><strong>Data Source:</strong> ", county_data$data_source, "</p>",
        "<p><strong>Interpolation Method:</strong> ", 
        ifelse(is.na(county_data$interpolation_method), "None", county_data$interpolation_method), "</p>"
      ))
    })
    
    # Get time series data for selected county
    selected_county_ts <- reactive({
      req(selected_county(), input$map_variable)
      
      geoid <- selected_county()
      
      get_time_series(conn, input$map_variable, geoid)
    })
    
    # Render selected county trend
    output$selected_county_trend <- renderPlotly({
      req(selected_county_ts(), variable_details())
      
      ts_data <- selected_county_ts()
      var_info <- variable_details()
      
      if (nrow(ts_data) == 0) {
        return(NULL)
      }
      
      # Create color based on data quality
      quality_colors <- c(
        "direct" = "#28a745",
        "interpolated" = "#fd7e14",
        "extrapolated" = "#dc3545",
        "simulated" = "#6c757d"
      )
      
      ts_data$color <- quality_colors[ts_data$data_quality]
      
      # Create the plot
      p <- plot_ly(
        ts_data,
        x = ~year,
        y = ~value,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#0275d8"),
        marker = list(color = ~color, size = 8),
        hoverinfo = "text",
        text = ~paste0(
          county_name, ", ", state_name, "<br>",
          var_info$variable_name, " (", year, "): ", round(value, 2), " ", var_info$units, "<br>",
          "Data Quality: ", data_quality, "<br>",
          "Data Source: ", data_source
        )
      ) %>%
        layout(
          title = paste0("Time Trend for ", var_info$variable_name, " in ", ts_data$county_name[1]),
          xaxis = list(title = "Year", tickmode = "linear"),
          yaxis = list(title = paste0(var_info$variable_name, " (", var_info$units, ")")),
          margin = list(l = 50, r = 20, b = 50, t = 50, pad = 4),
          showlegend = FALSE
        )
      
      return(p)
    })
    
    # Download map data
    output$download_map_data <- downloadHandler(
      filename = function() {
        paste0("map_data_", input$map_variable, "_", input$map_year, ".csv")
      },
      content = function(file) {
        write.csv(map_data(), file, row.names = FALSE)
      }
    )
    
    # ---- Time Trends Tab ----
    
    # Get trend data
    trend_data <- reactive({
      req(input$trend_variable)
      
      if (input$trend_show_all_counties) {
        # Get data for all counties
        data <- get_time_series(conn, input$trend_variable)
      } else {
        # Get data for selected county
        req(input$trend_county)
        data <- get_time_series(conn, input$trend_variable, input$trend_county)
      }
      
      return(data)
    })
    
    # Render trend variable information
    output$trend_variable_info <- renderUI({
      req(input$trend_variable)
      
      var <- variables()[variables()$variable_name == input$trend_variable, ]
      
      HTML(paste0(
        "<h4>", var$variable_name, "</h4>",
        "<p><strong>Domain:</strong> ", var$domain, "</p>",
        "<p><strong>Description:</strong> ", var$description, "</p>",
        "<p><strong>Units:</strong> ", var$units, "</p>",
        "<p><strong>Years Available:</strong> ", var$min_year, " to ", var$max_year, "</p>"
      ))
    })
    
    # Render trend statistics
    output$trend_stats <- renderUI({
      req(trend_data())
      
      data <- trend_data()
      
      # Calculate statistics
      stats <- list(
        years = length(unique(data$year)),
        min_year = min(data$year),
        max_year = max(data$year),
        counties = if(input$trend_show_all_counties) length(unique(data$geoid)) else 1,
        min = min(data$value, na.rm = TRUE),
        max = max(data$value, na.rm = TRUE),
        mean = mean(data$value, na.rm = TRUE),
        median = median(data$value, na.rm = TRUE),
        sd = sd(data$value, na.rm = TRUE)
      )
      
      HTML(paste0(
        "<h4>Time Series Statistics</h4>",
        "<p><strong>Years:</strong> ", stats$years, " (", stats$min_year, " to ", stats$max_year, ")</p>",
        "<p><strong>Counties:</strong> ", stats$counties, "</p>",
        "<p><strong>Min Value:</strong> ", round(stats$min, 2), "</p>",
        "<p><strong>Max Value:</strong> ", round(stats$max, 2), "</p>",
        "<p><strong>Mean Value:</strong> ", round(stats$mean, 2), "</p>",
        "<p><strong>Median Value:</strong> ", round(stats$median, 2), "</p>",
        "<p><strong>Standard Deviation:</strong> ", round(stats$sd, 2), "</p>"
      ))
    })
    
    # Render trend plot
    output$trend_plot <- renderPlotly({
      req(trend_data(), input$trend_variable)
      
      data <- trend_data()
      var_info <- variables()[variables()$variable_name == input$trend_variable, ]
      
      if (nrow(data) == 0) {
        return(NULL)
      }
      
      # Define colors for data quality
      quality_colors <- c(
        "direct" = "#28a745",
        "interpolated" = "#fd7e14",
        "extrapolated" = "#dc3545",
        "simulated" = "#6c757d"
      )
      
      if (input$trend_show_all_counties) {
        # Summarize by year for all counties
        summary_data <- data %>%
          group_by(year) %>%
          summarize(
            mean_value = mean(value, na.rm = TRUE),
            median_value = median(value, na.rm = TRUE),
            min_value = min(value, na.rm = TRUE),
            max_value = max(value, na.rm = TRUE),
            p25_value = quantile(value, 0.25, na.rm = TRUE),
            p75_value = quantile(value, 0.75, na.rm = TRUE),
            n_counties = n()
          )
        
        # Create the plot
        if (input$trend_plot_type == "line") {
          p <- plot_ly() %>%
            add_trace(
              data = summary_data,
              x = ~year,
              y = ~min_value,
              name = "Min",
              type = "scatter",
              mode = "lines",
              line = list(color = "rgba(200, 200, 200, 0.5)", width = 0),
              showlegend = FALSE
            ) %>%
            add_trace(
              data = summary_data,
              x = ~year,
              y = ~max_value,
              name = "Max",
              type = "scatter",
              mode = "lines",
              fill = "tonexty",
              fillcolor = "rgba(200, 200, 200, 0.5)",
              line = list(color = "rgba(200, 200, 200, 0.5)", width = 0),
              showlegend = FALSE
            ) %>%
            add_trace(
              data = summary_data,
              x = ~year,
              y = ~p25_value,
              name = "25th Percentile",
              type = "scatter",
              mode = "lines",
              line = list(color = "rgba(100, 100, 100, 0.5)", width = 0),
              showlegend = FALSE
            ) %>%
            add_trace(
              data = summary_data,
              x = ~year,
              y = ~p75_value,
              name = "75th Percentile",
              type = "scatter",
              mode = "lines",
              fill = "tonexty",
              fillcolor = "rgba(100, 100, 100, 0.5)",
              line = list(color = "rgba(100, 100, 100, 0.5)", width = 0),
              showlegend = FALSE
            ) %>%
            add_trace(
              data = summary_data,
              x = ~year,
              y = ~median_value,
              name = "Median",
              type = "scatter",
              mode = "lines+markers",
              line = list(color = "#0275d8", width = 2),
              marker = list(color = "#0275d8", size = 8),
              hoverinfo = "text",
              text = ~paste0(
                "Year: ", year, "<br>",
                "Median: ", round(median_value, 2), " ", var_info$units, "<br>",
                "Mean: ", round(mean_value, 2), " ", var_info$units, "<br>",
                "Range: ", round(min_value, 2), " - ", round(max_value, 2), " ", var_info$units, "<br>",
                "Counties: ", n_counties
              )
            )
          
          # If a specific county is selected to highlight
          if (input$trend_highlight_selected && !is.null(input$trend_county)) {
            county_data <- data[data$geoid == input$trend_county, ]
            
            if (nrow(county_data) > 0) {
              county_name <- paste0(county_data$county_name[1], ", ", county_data$state_name[1])
              
              p <- p %>%
                add_trace(
                  data = county_data,
                  x = ~year,
                  y = ~value,
                  name = county_name,
                  type = "scatter",
                  mode = "lines+markers",
                  line = list(color = "#dc3545", width = 2),
                  marker = list(color = ~quality_colors[data_quality], size = 8, line = list(color = "#dc3545", width = 2)),
                  hoverinfo = "text",
                  text = ~paste0(
                    county_name, "<br>",
                    "Year: ", year, "<br>",
                    var_info$variable_name, ": ", round(value, 2), " ", var_info$units, "<br>",
                    "Data Quality: ", data_quality, "<br>",
                    "Data Source: ", data_source
                  )
                )
            }
          }
          
        } else {
          # Bar plot for all counties
          p <- plot_ly(
            data = summary_data,
            x = ~year,
            y = ~median_value,
            type = "bar",
            name = "Median",
            marker = list(color = "#0275d8"),
            error_y = list(
              type = "data",
              symmetric = FALSE,
              array = summary_data$max_value - summary_data$median_value,
              arrayminus = summary_data$median_value - summary_data$min_value,
              color = "#888"
            ),
            hoverinfo = "text",
            text = ~paste0(
              "Year: ", year, "<br>",
              "Median: ", round(median_value, 2), " ", var_info$units, "<br>",
              "Mean: ", round(mean_value, 2), " ", var_info$units, "<br>",
              "Range: ", round(min_value, 2), " - ", round(max_value, 2), " ", var_info$units, "<br>",
              "Counties: ", n_counties
            )
          )
        }
      } else {
        # Plot for single county
        if (input$trend_plot_type == "line") {
          p <- plot_ly(
            data,
            x = ~year,
            y = ~value,
            type = "scatter",
            mode = "lines+markers",
            line = list(color = "#0275d8"),
            marker = list(color = ~quality_colors[data_quality], size = 8),
            hoverinfo = "text",
            text = ~paste0(
              county_name, ", ", state_name, "<br>",
              "Year: ", year, "<br>",
              var_info$variable_name, ": ", round(value, 2), " ", var_info$units, "<br>",
              "Data Quality: ", data_quality, "<br>",
              "Data Source: ", data_source
            )
          )
        } else {
          # Bar plot for single county
          p <- plot_ly(
            data,
            x = ~year,
            y = ~value,
            type = "bar",
            marker = list(color = ~quality_colors[data_quality]),
            hoverinfo = "text",
            text = ~paste0(
              county_name, ", ", state_name, "<br>",
              "Year: ", year, "<br>",
              var_info$variable_name, ": ", round(value, 2), " ", var_info$units, "<br>",
              "Data Quality: ", data_quality, "<br>",
              "Data Source: ", data_source
            )
          )
        }
      }
      
      # Add layout
      p <- p %>%
        layout(
          title = paste0("Time Trend for ", var_info$variable_name),
          xaxis = list(title = "Year", tickmode = "linear"),
          yaxis = list(title = paste0(var_info$variable_name, " (", var_info$units, ")")),
          hovermode = "closest",
          showlegend = input$trend_highlight_selected
        )
      
      return(p)
    })
    
    # Render trend data table
    output$trend_data_table <- renderDT({
      req(trend_data())
      
      data <- trend_data()
      
      if (input$trend_show_all_counties) {
        # Summarize by year for all counties
        summary_data <- data %>%
          group_by(year) %>%
          summarize(
            `Mean Value` = mean(value, na.rm = TRUE),
            `Median Value` = median(value, na.rm = TRUE),
            `Min Value` = min(value, na.rm = TRUE),
            `Max Value` = max(value, na.rm = TRUE),
            `25th Percentile` = quantile(value, 0.25, na.rm = TRUE),
            `75th Percentile` = quantile(value, 0.75, na.rm = TRUE),
            `Standard Deviation` = sd(value, na.rm = TRUE),
            `Counties` = n()
          ) %>%
          ungroup()
        
        # Round numeric columns
        summary_data <- summary_data %>%
          mutate(across(where(is.numeric) & !matches("year|Counties"), ~round(., 2)))
        
        datatable(
          summary_data,
          options = list(
            pageLength = 10,
            order = list(list(0, 'desc')),
            scrollX = TRUE
          ),
          rownames = FALSE
        )
      } else {
        # Format the data for a single county
        formatted_data <- data %>%
          select(year, value, data_quality, data_source) %>%
          rename(
            Year = year,
            Value = value,
            `Data Quality` = data_quality,
            `Data Source` = data_source
          ) %>%
          mutate(Value = round(Value, 2))
        
        datatable(
          formatted_data,
          options = list(
            pageLength = 10,
            order = list(list(0, 'desc')),
            scrollX = TRUE
          ),
          rownames = FALSE
        )
      }
    })
    
    # Download trend data
    output$download_trend_data <- downloadHandler(
      filename = function() {
        if (input$trend_show_all_counties) {
          paste0("trend_data_all_counties_", input$trend_variable, ".csv")
        } else {
          paste0("trend_data_", gsub("[^a-zA-Z0-9]", "_", input$trend_county), "_", input$trend_variable, ".csv")
        }
      },
      content = function(file) {
        write.csv(trend_data(), file, row.names = FALSE)
      }
    )
    
    # ---- Correlations Tab ----
    
    # Update years dropdown based on selected variables
    observe({
      req(input$corr_variable_x, input$corr_variable_y)
      
      # Get years for both variables
      query_x <- glue::glue_sql("
        SELECT DISTINCT year 
        FROM sdoh_data 
        WHERE variable_name = {input$corr_variable_x}
        ORDER BY year DESC
      ", .con = conn)
      
      query_y <- glue::glue_sql("
        SELECT DISTINCT year 
        FROM sdoh_data 
        WHERE variable_name = {input$corr_variable_y}
        ORDER BY year DESC
      ", .con = conn)
      
      years_x <- dbGetQuery(conn, query_x)$year
      years_y <- dbGetQuery(conn, query_y)$year
      
      # Find common years
      common_years <- intersect(years_x, years_y)
      
      updateSelectInput(session, "corr_year", 
                        choices = common_years,
                        selected = max(common_years))
    })
    
    # Get correlation data
    correlation_data <- reactive({
      req(input$corr_variable_x, input$corr_variable_y, input$corr_year)
      
      data <- get_correlation_data(conn, input$corr_variable_x, input$corr_variable_y, input$corr_year)
      
      return(data)
    })
    
    # Render correlation information
    output$correlation_info <- renderUI({
      req(correlation_data(), input$corr_variable_x, input$corr_variable_y)
      
      var_x <- variables()[variables()$variable_name == input$corr_variable_x, ]
      var_y <- variables()[variables()$variable_name == input$corr_variable_y, ]
      
      data <- correlation_data()
      
      # Calculate correlation coefficient
      cor_value <- cor(data$var1_value, data$var2_value, use = "complete.obs")
      
      HTML(paste0(
        "<h4>Correlation Analysis</h4>",
        "<p><strong>X Variable:</strong> ", var_x$variable_name, "</p>",
        "<p><strong>X Description:</strong> ", var_x$description, "</p>",
        "<p><strong>X Units:</strong> ", var_x$units, "</p>",
        "<p><strong>Y Variable:</strong> ", var_y$variable_name, "</p>",
        "<p><strong>Y Description:</strong> ", var_y$description, "</p>",
        "<p><strong>Y Units:</strong> ", var_y$units, "</p>",
        "<p><strong>Year:</strong> ", input$corr_year, "</p>",
        "<p><strong>Correlation (Pearson):</strong> ", round(cor_value, 3), "</p>"
      ))
    })
    
    # Render correlation statistics
    output$correlation_stats <- renderUI({
      req(correlation_data())
      
      data <- correlation_data()
      
      # Calculate statistics
      stats <- list(
        counties = nrow(data),
        x_min = min(data$var1_value, na.rm = TRUE),
        x_max = max(data$var1_value, na.rm = TRUE),
        x_mean = mean(data$var1_value, na.rm = TRUE),
        x_sd = sd(data$var1_value, na.rm = TRUE),
        y_min = min(data$var2_value, na.rm = TRUE),
        y_max = max(data$var2_value, na.rm = TRUE),
        y_mean = mean(data$var2_value, na.rm = TRUE),
        y_sd = sd(data$var2_value, na.rm = TRUE),
        direct_x = sum(data$var1_quality == "direct", na.rm = TRUE),
        interpolated_x = sum(data$var1_quality == "interpolated", na.rm = TRUE),
        direct_y = sum(data$var2_quality == "direct", na.rm = TRUE),
        interpolated_y = sum(data$var2_quality == "interpolated", na.rm = TRUE)
      )
      
      HTML(paste0(
        "<h4>Data Statistics</h4>",
        "<p><strong>Counties:</strong> ", stats$counties, "</p>",
        "<p><strong>X Range:</strong> ", round(stats$x_min, 2), " to ", round(stats$x_max, 2), "</p>",
        "<p><strong>X Mean (SD):</strong> ", round(stats$x_mean, 2), " (", round(stats$x_sd, 2), ")</p>",
        "<p><strong>Y Range:</strong> ", round(stats$y_min, 2), " to ", round(stats$y_max, 2), "</p>",
        "<p><strong>Y Mean (SD):</strong> ", round(stats$y_mean, 2), " (", round(stats$y_sd, 2), ")</p>",
        "<h4>Data Quality</h4>",
        "<p><strong>X Variable:</strong> ", stats$direct_x, " direct, ", stats$interpolated_x, " interpolated</p>",
        "<p><strong>Y Variable:</strong> ", stats$direct_y, " direct, ", stats$interpolated_y, " interpolated</p>"
      ))
    })
    
    # Render correlation plot
    output$correlation_plot <- renderPlotly({
      req(correlation_data(), input$corr_variable_x, input$corr_variable_y)
      
      data <- correlation_data()
      var_x <- variables()[variables()$variable_name == input$corr_variable_x, ]
      var_y <- variables()[variables()$variable_name == input$corr_variable_y, ]
      
      if (nrow(data) == 0) {
        return(NULL)
      }
      
      # Create the plot
      p <- plot_ly(
        data,
        x = ~var1_value,
        y = ~var2_value,
        type = "scatter",
        mode = "markers",
        marker = list(
          size = 10,
          opacity = input$corr_opacity,
          line = list(
            color = "white",
            width = 0.5
          )
        ),
        color = I("#0275d8"),
        hoverinfo = "text",
        text = ~paste0(
          county_name, ", ", state_name, "<br>",
          var_x$variable_name, ": ", round(var1_value, 2), " ", var_x$units, "<br>",
          var_y$variable_name, ": ", round(var2_value, 2), " ", var_y$units, "<br>",
          "X Data Quality: ", var1_quality, "<br>",
          "Y Data Quality: ", var2_quality
        )
      )
      
      # Add regression line if requested
      if (input$corr_show_regression) {
        # Fit linear model
        fit <- lm(var2_value ~ var1_value, data = data)
        
        # Create prediction data
        x_range <- seq(min(data$var1_value, na.rm = TRUE), max(data$var1_value, na.rm = TRUE), length.out = 100)
        pred_data <- data.frame(var1_value = x_range)
        pred_data$var2_value <- predict(fit, newdata = pred_data)
        
        # Add line to plot
        p <- p %>%
          add_trace(
            data = pred_data,
            x = ~var1_value,
            y = ~var2_value,
            type = "scatter",
            mode = "lines",
            line = list(color = "#dc3545", width = 2),
            name = paste0("R² = ", round(summary(fit)$r.squared, 3)),
            hoverinfo = "none"
          )
      }
      
      # Add layout
      p <- p %>%
        layout(
          title = paste0("Correlation between ", var_x$variable_name, " and ", var_y$variable_name, " (", input$corr_year, ")"),
          xaxis = list(title = paste0(var_x$variable_name, " (", var_x$units, ")")),
          yaxis = list(title = paste0(var_y$variable_name, " (", var_y$units, ")")),
          hovermode = "closest",
          showlegend = input$corr_show_regression
        )
      
      return(p)
    })
    
    # Render correlation data table
    output$correlation_data_table <- renderDT({
      req(correlation_data(), input$corr_variable_x, input$corr_variable_y)
      
      data <- correlation_data()
      var_x <- variables()[variables()$variable_name == input$corr_variable_x, ]
      var_y <- variables()[variables()$variable_name == input$corr_variable_y, ]
      
      # Format the data
      formatted_data <- data %>%
        select(county_name, state_name, var1_value, var2_value, var1_quality, var2_quality) %>%
        rename(
          County = county_name,
          State = state_name,
          !!paste0(var_x$variable_name) := var1_value,
          !!paste0(var_y$variable_name) := var2_value,
          !!paste0(var_x$variable_name, " Quality") := var1_quality,
          !!paste0(var_y$variable_name, " Quality") := var2_quality
        ) %>%
        mutate(across(where(is.numeric), ~round(., 2)))
      
      datatable(
        formatted_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    })
    
    # Download correlation data
    output$download_correlation_data <- downloadHandler(
      filename = function() {
        paste0("correlation_data_", input$corr_variable_x, "_", input$corr_variable_y, "_", input$corr_year, ".csv")
      },
      content = function(file) {
        write.csv(correlation_data(), file, row.names = FALSE)
      }
    )
    
    # ---- Data Table Tab ----
    
    # Update years dropdown based on selected variable
    observe({
      req(input$table_variable)
      
      query <- glue::glue_sql("
        SELECT DISTINCT year 
        FROM sdoh_data 
        WHERE variable_name = {input$table_variable}
        ORDER BY year DESC
      ", .con = conn)
      
      years <- dbGetQuery(conn, query)$year
      
      updateSelectInput(session, "table_year", 
                        choices = years,
                        selected = years[1])
    })
    
    # Get table data
    table_data <- reactive({
      req(input$table_variable, input$table_year)
      
      data <- get_county_data(conn, input$table_variable, input$table_year)
      
      # Filter by state if selected
      if (!is.null(input$table_state) && input$table_state != "all") {
        data <- data %>%
          filter(state_name == input$table_state)
      }
      
      # Filter by search term if provided
      if (!is.null(input$table_search) && input$table_search != "") {
        search_term <- tolower(input$table_search)
        data <- data %>%
          filter(grepl(search_term, tolower(county_name)))
      }
      
      return(data)
    })
    
    # Render data table
    output$data_table <- renderDT({
      req(table_data(), input$table_variable)
      
      data <- table_data()
      var_info <- variables()[variables()$variable_name == input$table_variable, ]
      
      # Format the data
      formatted_data <- data %>%
        select(geoid, county_name, state_name, value, data_quality, data_source) %>%
        rename(
          FIPS = geoid,
          County = county_name,
          State = state_name,
          !!paste0(var_info$variable_name, " (", var_info$units, ")") := value,
          `Data Quality` = data_quality,
          `Data Source` = data_source
        ) %>%
        mutate(across(where(is.numeric), ~round(., 2)))
      
      datatable(
        formatted_data,
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          order = list(list(3, 'desc')) # Order by value descending
        ),
        rownames = FALSE
      )
    })
    
    # Download table data
    output$download_table_data <- downloadHandler(
      filename = function() {
        paste0("table_data_", input$table_variable, "_", input$table_year, ".csv")
      },
      content = function(file) {
        write.csv(table_data(), file, row.names = FALSE)
      }
    )
    
    # Clean up when the app closes
    onSessionEnded(function() {
      dbDisconnect(conn)
    })
  }
  
  # Create and launch the Shiny app
  app <- shinyApp(
    ui = dashboardPage(header, sidebar, body),
    server = server
  )
  
  if (launch_browser) {
    runApp(app, host = host, port = port, launch.browser = TRUE)
  } else {
    return(app)
  }
}

# If script is run directly, launch the app
if (!interactive()) {
  launch_sdoh_dashboard()
}