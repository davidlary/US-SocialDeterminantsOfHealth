#!/usr/bin/env Rscript

# Traffic Safety Geospatial Module
# This module provides spatial analysis capabilities for traffic safety data

# Required packages
required_packages <- c(
  "tidyverse",
  "sf",
  "spdep",
  "tigris",
  "tmap",
  "mapview"
)

# Load required packages
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    message(paste("Required package", pkg, "is not installed."))
    message("Please run 'Rscript R/install_packages.r' first.")
    # Don't stop execution, just warn and continue with reduced functionality
  }
}

# Try to load additional visualization packages if available
has_leaflet <- require("leaflet", quietly = TRUE)
has_plotly <- require("plotly", quietly = TRUE)
has_ggmap <- require("ggmap", quietly = TRUE)

#' Analyze spatial patterns in traffic safety data
#'
#' @param traffic_data Traffic safety data frame
#' @param variable_name Variable to analyze
#' @param year Year to analyze
#' @param shapefile_path Optional path to county shapefile
#' @param method Spatial analysis method
#' @param queen Whether to use queen contiguity
#' @param significance_level Significance level for tests
#'
#' @return A list with spatial analysis results
#' @export
analyze_traffic_safety_spatial <- function(traffic_data,
                                         variable_name = "traffic_fatality_rate_per_100k",
                                         year = NULL,
                                         shapefile_path = NULL,
                                         method = c("moran", "geary", "getis-ord", "all"),
                                         queen = TRUE,
                                         significance_level = 0.05) {
  
  # Check method
  method <- match.arg(method)
  
  # Make sure we have the data variable
  if (!variable_name %in% names(traffic_data)) {
    stop("Variable '", variable_name, "' not found in data")
  }
  
  # Choose the latest year if not specified
  if (is.null(year)) {
    year <- max(traffic_data$year, na.rm = TRUE)
  }
  
  # Filter to the selected year
  year_data <- traffic_data %>%
    filter(year == !!year)
  
  if (nrow(year_data) == 0) {
    stop("No data available for year ", year)
  }
  
  # Get county geometry
  county_sf <- get_county_geometry(shapefile_path, year_data)
  
  if (is.null(county_sf)) {
    stop("Could not get county geometry for spatial analysis")
  }
  
  # Join the data to the geometry
  spatial_data <- county_sf %>%
    left_join(year_data, by = c("GEOID" = "fips"))
  
  # Remove counties with missing data
  spatial_data <- spatial_data %>%
    filter(!is.na(!!sym(variable_name)))
  
  # Check that we have enough data
  if (nrow(spatial_data) < 5) {
    stop("Not enough data for spatial analysis (minimum 5 counties with data)")
  }
  
  # Create spatial weights matrix
  nb <- poly2nb(spatial_data, queen = queen)
  
  # Handle islands (counties with no neighbors)
  if (any(card(nb) == 0)) {
    message("Some counties have no neighbors. Adding nearest neighbors for these.")
    # Find counties with no neighbors
    islands <- which(card(nb) == 0)
    # Create a knn=1 neighbor list for islands
    coords <- st_centroid(st_geometry(spatial_data))
    island_nb <- knn2nb(knearneigh(coords[islands], k=1), row.names = row.names(spatial_data)[islands])
    # Update the neighbor list
    nb <- include.isolates(nb, islands)
    for (i in 1:length(islands)) {
      nb[[islands[i]]] <- island_nb[[i]]
      # Add reverse links
      for (j in island_nb[[i]]) {
        nb[[j]] <- sort(unique(c(nb[[j]], islands[i])))
      }
    }
  }
  
  # Create spatial weights
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  
  # Initialize results list
  results <- list(
    data = spatial_data,
    variable = variable_name,
    year = year,
    method = method,
    global = list(),
    local = list(),
    neighbors = nb,
    weights = lw
  )
  
  # Perform global spatial autocorrelation tests
  if (method %in% c("moran", "all")) {
    # Moran's I test
    moran_test <- tryCatch({
      moran.test(spatial_data[[variable_name]], lw, zero.policy = TRUE)
    }, error = function(e) {
      message("Error calculating Moran's I: ", e$message)
      return(NULL)
    })
    
    if (!is.null(moran_test)) {
      results$global$moran <- list(
        statistic = moran_test$statistic,
        p.value = moran_test$p.value,
        estimate = moran_test$estimate
      )
    }
  }
  
  if (method %in% c("geary", "all")) {
    # Geary's C test
    geary_test <- tryCatch({
      geary.test(spatial_data[[variable_name]], lw, zero.policy = TRUE)
    }, error = function(e) {
      message("Error calculating Geary's C: ", e$message)
      return(NULL)
    })
    
    if (!is.null(geary_test)) {
      results$global$geary <- list(
        statistic = geary_test$statistic,
        p.value = geary_test$p.value,
        estimate = geary_test$estimate
      )
    }
  }
  
  # Perform local spatial autocorrelation analysis
  if (method %in% c("moran", "all")) {
    # Local Moran's I (LISA)
    localmoran_results <- tryCatch({
      localmoran(spatial_data[[variable_name]], lw, zero.policy = TRUE)
    }, error = function(e) {
      message("Error calculating Local Moran's I: ", e$message)
      return(NULL)
    })
    
    if (!is.null(localmoran_results)) {
      # Generate LISA cluster categories
      variable_std <- scale(spatial_data[[variable_name]])
      variable_lag <- lag.listw(lw, variable_std, zero.policy = TRUE)
      
      # Determine significance
      significant <- localmoran_results[, 5] <= significance_level
      
      # Create cluster types
      clusters <- rep("Not Significant", nrow(spatial_data))
      clusters[significant & variable_std > 0 & variable_lag > 0] <- "High-High"
      clusters[significant & variable_std < 0 & variable_lag < 0] <- "Low-Low"
      clusters[significant & variable_std > 0 & variable_lag < 0] <- "High-Low"
      clusters[significant & variable_std < 0 & variable_lag > 0] <- "Low-High"
      
      # Add results to the spatial data
      spatial_data$lisa_i <- localmoran_results[, 1]
      spatial_data$lisa_p_value <- localmoran_results[, 5]
      spatial_data$lisa_cluster <- clusters
      
      # Store in results
      results$local$moran <- list(
        values = localmoran_results,
        clusters = clusters,
        significance_level = significance_level
      )
    }
  }
  
  if (method %in% c("getis-ord", "all")) {
    # Getis-Ord G*
    g_star_results <- tryCatch({
      localG(spatial_data[[variable_name]], lw, zero.policy = TRUE)
    }, error = function(e) {
      message("Error calculating Getis-Ord G*: ", e$message)
      return(NULL)
    })
    
    if (!is.null(g_star_results)) {
      # Determine hotspots/coldspots
      g_star_p_values <- 2 * pnorm(abs(g_star_results), lower.tail = FALSE)
      significant <- g_star_p_values <= significance_level
      
      # Create cluster types
      clusters <- rep("Not Significant", nrow(spatial_data))
      clusters[significant & g_star_results > 0] <- "Hotspot"
      clusters[significant & g_star_results < 0] <- "Coldspot"
      
      # Add results to the spatial data
      spatial_data$g_star <- g_star_results
      spatial_data$g_star_p_value <- g_star_p_values
      spatial_data$g_star_cluster <- clusters
      
      # Store in results
      results$local$getis_ord <- list(
        values = g_star_results,
        p_values = g_star_p_values,
        clusters = clusters,
        significance_level = significance_level
      )
    }
  }
  
  # Update the spatial data
  results$data <- spatial_data
  
  return(results)
}

#' Identify traffic safety hotspots
#'
#' @param traffic_data Traffic safety data frame
#' @param variable_name Variable to analyze
#' @param year Year to analyze
#' @param shapefile_path Optional path to county shapefile
#' @param method Hotspot analysis method
#' @param significance_level Significance level for tests
#'
#' @return A data frame with hotspot results
#' @export
identify_traffic_safety_hotspots <- function(traffic_data,
                                           variable_name = "traffic_fatality_rate_per_100k",
                                           year = NULL,
                                           shapefile_path = NULL,
                                           method = c("lisa", "getis-ord"),
                                           significance_level = 0.05) {
  
  # Check method
  method <- match.arg(method)
  
  # Run spatial analysis
  spatial_results <- analyze_traffic_safety_spatial(
    traffic_data, 
    variable_name = variable_name,
    year = year,
    shapefile_path = shapefile_path,
    method = ifelse(method == "lisa", "moran", "getis-ord"),
    significance_level = significance_level
  )
  
  # Format results based on method
  if (method == "lisa") {
    hotspot_data <- spatial_results$data %>%
      st_drop_geometry() %>%
      select(fips = GEOID, county_name, 
             value = !!sym(variable_name),
             local_moran = lisa_i, 
             p_value = lisa_p_value, 
             cluster_type = lisa_cluster)
    
    # Add attributes
    attr(hotspot_data, "variable") <- variable_name
    attr(hotspot_data, "year") <- year
    attr(hotspot_data, "method") <- "lisa"
    attr(hotspot_data, "significance_level") <- significance_level
    
  } else if (method == "getis-ord") {
    hotspot_data <- spatial_results$data %>%
      st_drop_geometry() %>%
      select(fips = GEOID, county_name, 
             value = !!sym(variable_name),
             g_star = g_star, 
             p_value = g_star_p_value, 
             cluster_type = g_star_cluster)
    
    # Add attributes
    attr(hotspot_data, "variable") <- variable_name
    attr(hotspot_data, "year") <- year
    attr(hotspot_data, "method") <- "getis-ord"
    attr(hotspot_data, "significance_level") <- significance_level
  }
  
  return(hotspot_data)
}

#' Identify persistent problem areas across multiple years
#'
#' @param traffic_data Traffic safety data frame
#' @param variable_name Variable to analyze
#' @param years Years to analyze
#' @param threshold_percentile Percentile threshold for problematic values
#' @param persistence_pct Percentage of years needed to be considered persistent
#'
#' @return A data frame with persistent problem areas
#' @export
identify_persistent_problem_areas <- function(traffic_data,
                                            variable_name = "traffic_fatality_rate_per_100k",
                                            years = NULL,
                                            threshold_percentile = 75,
                                            persistence_pct = 50) {
  
  # Check that we have the needed data
  if (!all(c("fips", "year", variable_name) %in% names(traffic_data))) {
    stop("Required columns not found in data")
  }
  
  # Use all available years if not specified
  if (is.null(years)) {
    years <- sort(unique(traffic_data$year))
  } else {
    # Filter to only include years that exist in the data
    years <- years[years %in% unique(traffic_data$year)]
    if (length(years) == 0) {
      stop("None of the specified years exist in the data")
    }
  }
  
  # Initialize a data frame to track problem areas
  problem_areas <- data.frame(
    fips = unique(traffic_data$fips),
    years_analyzed = length(years),
    years_exceeding = 0,
    pct_years_exceeding = 0,
    avg_value = NA_real_,
    max_value = NA_real_,
    latest_value = NA_real_,
    latest_year = max(years),
    persistently_problematic = FALSE
  )
  
  # Get county names
  county_names <- traffic_data %>%
    select(fips, county_name) %>%
    distinct()
  
  # Add county names to problem_areas
  problem_areas <- problem_areas %>%
    left_join(county_names, by = "fips")
  
  # Calculate threshold for each year
  thresholds <- numeric(length(years))
  names(thresholds) <- as.character(years)
  
  for (yr in years) {
    year_data <- traffic_data %>%
      filter(year == yr)
    
    if (nrow(year_data) > 0) {
      year_values <- year_data[[variable_name]]
      year_values <- year_values[!is.na(year_values)]
      
      if (length(year_values) > 0) {
        thresholds[as.character(yr)] <- quantile(year_values, threshold_percentile/100, na.rm = TRUE)
      }
    }
  }
  
  # For each county, count how many years it exceeds the threshold
  for (i in 1:nrow(problem_areas)) {
    county_fips <- problem_areas$fips[i]
    county_data <- traffic_data %>%
      filter(fips == county_fips & year %in% years)
    
    if (nrow(county_data) > 0) {
      # Count years exceeding threshold
      years_exceeding <- 0
      for (yr in years) {
        year_data <- county_data %>%
          filter(year == yr)
        
        if (nrow(year_data) > 0 && !is.na(year_data[[variable_name]])) {
          if (year_data[[variable_name]] >= thresholds[as.character(yr)]) {
            years_exceeding <- years_exceeding + 1
          }
        }
      }
      
      # Update problem_areas
      problem_areas$years_exceeding[i] <- years_exceeding
      problem_areas$pct_years_exceeding[i] <- (years_exceeding / length(years)) * 100
      problem_areas$avg_value[i] <- mean(county_data[[variable_name]], na.rm = TRUE)
      problem_areas$max_value[i] <- max(county_data[[variable_name]], na.rm = TRUE)
      
      # Get latest value
      latest_year_data <- county_data %>%
        filter(year == max(years))
      if (nrow(latest_year_data) > 0) {
        problem_areas$latest_value[i] <- latest_year_data[[variable_name]]
      }
      
      # Determine if persistently problematic
      problem_areas$persistently_problematic[i] <- problem_areas$pct_years_exceeding[i] >= persistence_pct
    }
  }
  
  # Filter to only include counties with data
  problem_areas <- problem_areas %>%
    filter(!is.na(avg_value))
  
  # Add attributes
  attr(problem_areas, "variable") <- variable_name
  attr(problem_areas, "years") <- years
  attr(problem_areas, "threshold_percentile") <- threshold_percentile
  attr(problem_areas, "persistence_pct") <- persistence_pct
  attr(problem_areas, "thresholds") <- thresholds
  
  return(problem_areas)
}

#' Create a choropleth map of traffic safety data
#'
#' @param traffic_data Traffic safety data frame
#' @param variable_name Variable to map
#' @param year Year to map
#' @param shapefile_path Optional path to county shapefile
#' @param state_borders Whether to show state borders
#' @param interactive Whether to create an interactive map
#' @param output_file Path to save the map
#'
#' @return A map object or path to saved map
#' @export
create_choropleth_map <- function(traffic_data,
                                variable_name = "traffic_fatality_rate_per_100k",
                                year = NULL,
                                shapefile_path = NULL,
                                state_borders = TRUE,
                                interactive = TRUE,
                                output_file = NULL) {
  
  # Check if we have the required mapping packages
  if (!requireNamespace("tmap", quietly = TRUE)) {
    stop("Package 'tmap' is required for mapping")
  }
  
  # Choose the latest year if not specified
  if (is.null(year)) {
    year <- max(traffic_data$year, na.rm = TRUE)
  }
  
  # Filter to the selected year
  year_data <- traffic_data %>%
    filter(year == !!year)
  
  if (nrow(year_data) == 0) {
    stop("No data available for year ", year)
  }
  
  # Get county geometry
  county_sf <- get_county_geometry(shapefile_path, year_data)
  
  if (is.null(county_sf)) {
    stop("Could not get county geometry for mapping")
  }
  
  # Join the data to the geometry
  map_data <- county_sf %>%
    left_join(year_data, by = c("GEOID" = "fips"))
  
  # Set the map mode based on interactive preference
  tmap::tmap_mode(ifelse(interactive, "view", "plot"))
  
  # Create the map
  tm <- tmap::tm_shape(map_data) +
    tmap::tm_fill(col = variable_name, 
                 title = variable_name,
                 palette = "viridis",
                 style = "jenks",
                 id = "county_name") +
    tmap::tm_borders(col = "white", lwd = 0.1) +
    tmap::tm_layout(main.title = paste(variable_name, "by County,", year),
                   frame = FALSE,
                   legend.outside = TRUE)
  
  # Add state borders if requested
  if (state_borders) {
    states_sf <- tigris::states(cb = TRUE, year = year)
    tm <- tm + 
      tmap::tm_shape(states_sf) +
      tmap::tm_borders(col = "black", lwd = 0.5)
  }
  
  # Save the map if output_file is provided
  if (!is.null(output_file)) {
    # Create output directory if it doesn't exist
    dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
    
    # Save the map
    tmap::tmap_save(tm, output_file)
    return(output_file)
  }
  
  # Return the map object
  return(tm)
}

#' Create a map of traffic safety hotspots
#'
#' @param hotspot_data Hotspot data from identify_traffic_safety_hotspots
#' @param shapefile_path Optional path to county shapefile
#' @param state_borders Whether to show state borders
#' @param interactive Whether to create an interactive map
#' @param output_file Path to save the map
#'
#' @return A map object or path to saved map
#' @export
create_hotspot_map <- function(hotspot_data,
                             shapefile_path = NULL,
                             state_borders = TRUE,
                             interactive = TRUE,
                             output_file = NULL) {
  
  # Check if we have the required mapping packages
  if (!requireNamespace("tmap", quietly = TRUE)) {
    stop("Package 'tmap' is required for mapping")
  }
  
  # Check if hotspot_data has the needed attributes
  if (is.null(attr(hotspot_data, "method")) || 
      is.null(attr(hotspot_data, "variable")) || 
      is.null(attr(hotspot_data, "year"))) {
    stop("Hotspot data missing required attributes")
  }
  
  # Set up color palette and labels based on method
  method <- attr(hotspot_data, "method")
  if (method == "lisa") {
    palette <- c("red", "blue", "lightpink", "lightblue", "white")
    labels <- c("High-High", "Low-Low", "High-Low", "Low-High", "Not Significant")
    title <- "LISA Clusters"
  } else if (method == "getis-ord") {
    palette <- c("red", "blue", "white")
    labels <- c("Hotspot", "Coldspot", "Not Significant")
    title <- "Getis-Ord G* Hotspots"
  } else {
    stop("Unknown hotspot method")
  }
  
  # Get county geometry
  county_sf <- get_county_geometry(shapefile_path, hotspot_data)
  
  if (is.null(county_sf)) {
    stop("Could not get county geometry for mapping")
  }
  
  # Join the data to the geometry
  map_data <- county_sf %>%
    left_join(hotspot_data, by = c("GEOID" = "fips"))
  
  # Set the map mode based on interactive preference
  tmap::tmap_mode(ifelse(interactive, "view", "plot"))
  
  # Create the map
  variable_name <- attr(hotspot_data, "variable")
  year <- attr(hotspot_data, "year")
  
  tm <- tmap::tm_shape(map_data) +
    tmap::tm_fill(col = "cluster_type", 
                 title = title,
                 palette = palette,
                 labels = labels,
                 id = "county_name") +
    tmap::tm_borders(col = "white", lwd = 0.1) +
    tmap::tm_layout(main.title = paste(variable_name, "Hotspots,", year),
                   frame = FALSE,
                   legend.outside = TRUE)
  
  # Add state borders if requested
  if (state_borders) {
    states_sf <- tigris::states(cb = TRUE, year = year)
    tm <- tm + 
      tmap::tm_shape(states_sf) +
      tmap::tm_borders(col = "black", lwd = 0.5)
  }
  
  # Save the map if output_file is provided
  if (!is.null(output_file)) {
    # Create output directory if it doesn't exist
    dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
    
    # Save the map
    tmap::tmap_save(tm, output_file)
    return(output_file)
  }
  
  # Return the map object
  return(tm)
}

#' Helper function to get county geometry
#'
#' @param shapefile_path Optional path to county shapefile
#' @param data Data to join to geometry
#'
#' @return A sf object with county geometry
get_county_geometry <- function(shapefile_path = NULL, data = NULL) {
  # Try using a provided shapefile first
  if (!is.null(shapefile_path) && file.exists(shapefile_path)) {
    tryCatch({
      county_sf <- sf::st_read(shapefile_path, quiet = TRUE)
      return(county_sf)
    }, error = function(e) {
      message("Error reading shapefile: ", e$message)
      # Fall back to tigris if shapefile fails
    })
  }
  
  # If no shapefile or it failed, use tigris
  if (requireNamespace("tigris", quietly = TRUE)) {
    # Get the year to use for tigris
    if (!is.null(data) && "year" %in% names(data)) {
      year <- max(data$year, na.rm = TRUE)
      # Limit to years available in tigris
      year <- min(max(year, 1990), 2022)
    } else {
      year <- 2022  # Use the most recent year as default
    }
    
    tryCatch({
      county_sf <- tigris::counties(cb = TRUE, year = year)
      return(county_sf)
    }, error = function(e) {
      message("Error getting county geometry from tigris: ", e$message)
      return(NULL)
    })
  }
  
  # If we get here, we couldn't get county geometry
  message("Could not get county geometry. Please provide a valid shapefile or install the tigris package.")
  return(NULL)
}

# Let the pipeline know the module is loaded
cat("[INFO] Traffic safety geospatial module loaded successfully\n")