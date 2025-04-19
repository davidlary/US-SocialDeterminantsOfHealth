#!/usr/bin/env Rscript

# Traffic Safety Integration Module
# This script integrates all traffic safety enhancements into the unified pipeline
# Including geospatial analysis, data validation, forecasting, and optimized caching

# Required packages
required_packages <- c(
  "tidyverse",
  "magrittr",
  "R6"
)

# Load required packages
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    message(paste("Required package", pkg, "is not installed."))
    message("Please run 'Rscript R/install_packages.r' first.")
    # Don't stop execution, just warn and continue with reduced functionality
  }
}

#' Check and load all traffic safety enhancement modules
#' @return List of loaded module statuses
load_traffic_safety_modules <- function() {
  module_statuses <- list()
  
  # Define the modules to load
  modules <- c(
    "traffic_safety_cache.r",
    "traffic_safety_validation.r",
    "traffic_safety_forecasting.r",
    "traffic_safety_geospatial.r"
  )
  
  # Try to load each module with proper error handling (no timeouts)
  for (module in modules) {
    # Try multiple possible module locations
    possible_paths <- c(
      file.path(getwd(), module),                             # Current working directory
      file.path(dirname(getwd()), module),                    # Parent directory
      file.path(getwd(), "R", module),                        # R subdirectory
      file.path(dirname(getwd()), "R", module)                # Parent's R subdirectory
    )
    
    # Try each possible path
    result <- FALSE
    for (module_path in possible_paths) {
      if (file.exists(module_path)) {
        # Attempt to load the module without timeout (timeout was causing hanging issues)
        result <- tryCatch({
          # Log loading attempt
          message(paste("Loading module:", module, "from", module_path))
          
          # Create a special environment for sourcing to prevent namespace conflicts
          temp_env <- new.env(parent = .GlobalEnv)
          
          # Source the module in the special environment
          sys.source(module_path, envir = temp_env)
          
          # Copy necessary objects from temp environment to global environment
          for (obj_name in ls(temp_env)) {
            if (!exists(obj_name, envir = .GlobalEnv)) {
              assign(obj_name, get(obj_name, envir = temp_env), envir = .GlobalEnv)
            }
          }
          
          # Log success
          message(paste("Successfully loaded module:", module, "from", module_path))
          TRUE
        }, error = function(e) {
          message(paste("Failed to load module:", module, "from", module_path, "-", e$message))
          FALSE
        }, warning = function(w) {
          # Handle warnings but continue
          message(paste("Warning loading module:", module, "-", w$message))
          TRUE
        })
        
        # If successfully loaded, break the loop
        if (result) break
      }
    }
    
    # If we went through all paths and none worked
    if (!result) {
      message(paste("Could not find module:", module, "in any expected location"))
    }
    
    # Store the result
    module_statuses[[module]] <- result
  }
  
  return(module_statuses)
}

#' Enhanced traffic safety data fetch with all improvements integrated
#'
#' @param years Years to fetch data for
#' @param cache_dir Directory for caching data
#' @param refresh_cache Whether to refresh cache
#' @param allow_interpolation Whether to allow data interpolation
#' @param allow_simulation Whether to allow data simulation
#' @param use_validation Whether to apply validation hooks
#' @param use_optimized_cache Whether to use the enhanced caching system
#' @param generate_forecasts Whether to generate forecasts
#' @param spatial_analysis Whether to perform spatial analysis
#' @param ... Additional parameters passed to the underlying functions
#'
#' @return Enhanced traffic safety dataset with additional attributes
#' @export
fetch_enhanced_traffic_safety_data <- function(
    years = NULL,
    cache_dir = "data/cache",
    refresh_cache = FALSE,
    allow_interpolation = TRUE,
    allow_simulation = FALSE,
    use_validation = TRUE,
    use_optimized_cache = TRUE,
    generate_forecasts = FALSE,
    spatial_analysis = FALSE,
    ...
) {
  # Load required modules
  module_statuses <- load_traffic_safety_modules()
  
  # Set up default years if not provided
  if (is.null(years)) {
    years <- (as.numeric(format(Sys.Date(), "%Y")) - 10):as.numeric(format(Sys.Date(), "%Y"))
  }
  
  # Check if fetch_traffic_safety_data exists
  if (!exists("fetch_traffic_safety_data", mode = "function")) {
    # Try to load it from multiple possible locations
    possible_paths <- c(
      file.path(getwd(), "fetch_traffic_safety_data.r"),
      file.path(dirname(getwd()), "fetch_traffic_safety_data.r"),
      file.path(getwd(), "R", "fetch_traffic_safety_data.r"),
      file.path(dirname(getwd()), "R", "fetch_traffic_safety_data.r")
    )
    
    fetch_loaded <- FALSE
    for (fetch_file_path in possible_paths) {
      if (file.exists(fetch_file_path)) {
        # Load without timeout to prevent hanging
        tryCatch({
          message(paste("Loading fetch_traffic_safety_data from", fetch_file_path))
          
          # Create a special environment for sourcing to prevent namespace conflicts
          temp_env <- new.env(parent = .GlobalEnv)
          
          # Source the file in the special environment
          sys.source(fetch_file_path, envir = temp_env)
          
          # Copy necessary objects from temp environment to global environment
          for (obj_name in ls(temp_env)) {
            if (!exists(obj_name, envir = .GlobalEnv)) {
              assign(obj_name, get(obj_name, envir = temp_env), envir = .GlobalEnv)
            }
          }
          
          fetch_loaded <- TRUE
          message(paste("Successfully loaded fetch_traffic_safety_data from", fetch_file_path))
          break
        }, error = function(e) {
          message(paste("Error loading fetch_traffic_safety_data from", fetch_file_path, "-", e$message))
        })
      }
    }
    
    if (!fetch_loaded) {
      stop("fetch_traffic_safety_data.r not found in any expected location. Cannot proceed.")
    }
  }
  
  # Base function to fetch data
  base_fetch_func <- function() {
    fetch_traffic_safety_data(
      years = years,
      cache_dir = cache_dir,
      refresh_cache = refresh_cache,
      allow_interpolation = allow_interpolation,
      allow_simulation = allow_simulation,
      ...
    )
  }
  
  # Apply optimized cache if requested
  if (use_optimized_cache && module_statuses[["traffic_safety_cache.r"]]) {
    # Fetch data with optimized cache
    traffic_data <- with_optimized_cache(
      base_fetch_func,
      cache_dir = cache_dir
    )
  } else {
    # Use regular fetch
    traffic_data <- base_fetch_func()
  }
  
  # Apply validation if requested
  if (use_validation && module_statuses[["traffic_safety_validation.r"]]) {
    tryCatch({
      # Check if the validation functions exist
      if (exists("with_validation_hooks", mode = "function") && 
          exists("validate_traffic_safety_data", mode = "function")) {
        
        message("Applying validation hooks to traffic safety data...")
        
        # Apply validation hooks without timeout
        traffic_data <- with_validation_hooks(
          function() { traffic_data },
          on_validation_fail = "warn"
        )
        
        # Add validation attribute
        validator <- validate_traffic_safety_data(traffic_data)
        attr(traffic_data, "validation") <- validator
        
        message("Validation completed successfully")
      } else {
        message("Validation functions not found. Skipping validation.")
      }
    }, error = function(e) {
      message(paste("Error during validation:", e$message, "- Continuing without validation"))
    })
  }
  
  # Add forecasts if requested
  if (generate_forecasts && module_statuses[["traffic_safety_forecasting.r"]]) {
    tryCatch({
      # Check if forecasting functions exist
      if (exists("prepare_timeseries_data", mode = "function") && 
          exists("generate_forecast", mode = "function")) {
        
        # Generate national level forecast
        message("Generating traffic safety forecasts...")
        national_ts_data <- prepare_timeseries_data(
          traffic_data = traffic_data,
          variable = "traffic_fatality_rate_per_100k",
          region_type = "national"
        )
        
        national_forecast <- generate_forecast(
          ts_data = national_ts_data,
          forecast_years = 5,
          method = "ensemble"
        )
        
        # Add national forecast as attribute
        attr(traffic_data, "forecasts") <- list(
          national = national_forecast
        )
        
        message("Forecasting completed successfully")
      } else {
        message("Forecasting functions not found. Skipping forecasting.")
      }
    }, error = function(e) {
      message(paste("Error during forecasting:", e$message, "- Continuing without forecasts"))
    })
  }
  
  # Add spatial analysis if requested
  if (spatial_analysis && module_statuses[["traffic_safety_geospatial.r"]]) {
    tryCatch({
      # Check if spatial functions exist
      if (exists("prepare_spatial_data", mode = "function") && 
          exists("identify_spatial_clusters", mode = "function")) {
        
        message("Running traffic safety spatial analysis...")
        # Use the most recent year for spatial analysis
        latest_year <- max(traffic_data$year, na.rm = TRUE)
        
        # Prepare spatial data
        spatial_data <- prepare_spatial_data(
          traffic_data = traffic_data,
          year = latest_year,
          variable = "traffic_fatality_rate_per_100k"
        )
        
        # Identify clusters
        cluster_data <- identify_spatial_clusters(
          spatial_data = spatial_data,
          method = "lisa"
        )
        
        # Add spatial data as attribute
        attr(traffic_data, "spatial") <- list(
          year = latest_year,
          data = cluster_data
        )
        
        # Identify problem corridors over multiple years if function exists
        if (exists("identify_problem_corridors", mode = "function") && 
            length(unique(traffic_data$year)) >= 3) {
          
          problem_areas <- identify_problem_corridors(
            traffic_data = traffic_data,
            variable = "traffic_fatality_rate_per_100k",
            min_years = 3
          )
          
          # Add to spatial attributes
          attr(traffic_data, "spatial")$problem_areas <- problem_areas
        }
        
        message("Spatial analysis completed successfully")
      } else {
        message("Spatial analysis functions not found. Skipping spatial analysis.")
      }
    }, error = function(e) {
      message(paste("Error during spatial analysis:", e$message, "- Continuing without spatial analysis"))
    })
  }
  
  # Add metadata about enhancements
  attr(traffic_data, "enhancements") <- list(
    optimized_cache = use_optimized_cache && module_statuses[["traffic_safety_cache.r"]],
    validation = use_validation && module_statuses[["traffic_safety_validation.r"]],
    forecasting = generate_forecasts && module_statuses[["traffic_safety_forecasting.r"]],
    spatial_analysis = spatial_analysis && module_statuses[["traffic_safety_geospatial.r"]],
    modules_loaded = module_statuses
  )
  
  return(traffic_data)
}

#' Generate and save traffic safety visualizations
#'
#' @param traffic_data Enhanced traffic safety dataset
#' @param output_dir Directory to save visualizations
#' @param create_maps Whether to create maps
#' @param create_forecast_plots Whether to create forecast plots
#' @param create_animation Whether to create spatial animation
#'
#' @return List of paths to created visualizations
#' @export
create_traffic_safety_visualizations <- function(
    traffic_data,
    output_dir = "output/visualizations/traffic_safety",
    create_maps = TRUE,
    create_forecast_plots = TRUE,
    create_animation = FALSE
) {
  # Make sure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Track all created files
  created_files <- list()
  
  # Create maps if requested
  if (create_maps && !is.null(attr(traffic_data, "spatial"))) {
    if (requireNamespace("tmap", quietly = TRUE)) {
      # Create hotspot map
      cluster_map <- create_hotspot_map(
        attr(traffic_data, "spatial")$data,
        title = paste("Traffic Fatality Clusters", attr(traffic_data, "spatial")$year)
      )
      
      # Save the map
      map_file <- file.path(output_dir, "traffic_fatality_clusters.png")
      tmap::tmap_save(cluster_map, map_file, width = 10, height = 8)
      created_files$cluster_map <- map_file
    }
  }
  
  # Create forecast plots if requested
  if (create_forecast_plots && !is.null(attr(traffic_data, "forecasts"))) {
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      # Get national forecast
      national_forecast <- attr(traffic_data, "forecasts")$national
      
      # Create and save plot
      if (!is.null(national_forecast)) {
        p <- plot_forecast(
          national_forecast,
          title = "U.S. Traffic Fatality Rate Forecast",
          y_label = "Fatalities per 100,000 Population"
        )
        
        # Save plot
        forecast_file <- file.path(output_dir, "national_fatality_forecast.png")
        ggplot2::ggsave(forecast_file, p, width = 10, height = 6)
        created_files$forecast_plot <- forecast_file
      }
    }
  }
  
  # Create animation if requested
  if (create_animation) {
    if (requireNamespace("tmap", quietly = TRUE) && 
        requireNamespace("gifski", quietly = TRUE)) {
      
      # Create spatial animation
      animation_file <- file.path(output_dir, "traffic_fatality_animation.gif")
      
      # Get years with data
      years_with_data <- sort(unique(traffic_data$year))
      
      # Only create animation if we have 3+ years of data
      if (length(years_with_data) >= 3) {
        animation_path <- create_spatial_animation(
          traffic_data = traffic_data,
          years = tail(years_with_data, min(8, length(years_with_data))),
          variable = "traffic_fatality_rate_per_100k",
          title = "Traffic Fatality Rates Over Time",
          output_file = animation_file
        )
        
        created_files$animation <- animation_path
      }
    }
  }
  
  return(created_files)
}

#' Add traffic safety data to the main SDOH pipeline database
#'
#' @param traffic_data Enhanced traffic safety dataset
#' @param db_path Path to DuckDB database
#' @param add_forecasts Whether to add forecasts to database
#' @param add_spatial Whether to add spatial data to database
#'
#' @return TRUE if successful
#' @export
add_traffic_safety_to_database <- function(
    traffic_data,
    db_path,
    add_forecasts = TRUE,
    add_spatial = TRUE
) {
  # Check if DBI and duckdb packages are available
  if (!requireNamespace("DBI", quietly = TRUE) || 
      !requireNamespace("duckdb", quietly = TRUE)) {
    stop("DBI and duckdb packages are required to add data to database.")
  }
  
  # Connect to database
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  
  # Scope to ensure connection is closed even if an error occurs
  tryCatch({
    # Prepare traffic data for database
    db_traffic_data <- traffic_data %>%
      # Standardize county identifiers
      mutate(
        geoid = sprintf("%05d", as.numeric(fips)),
        year = as.integer(year)
      ) %>%
      # Convert to long format for the SDOH database structure
      tidyr::pivot_longer(
        cols = c(
          traffic_fatality_count, traffic_fatality_rate_per_100k,
          dui_fatality_count, dui_fatality_rate_per_100k,
          ped_bike_fatality_count, ped_bike_fatality_rate_per_100k,
          speeding_fatality_count, speeding_fatality_rate_per_100k
        ),
        names_to = "variable_name",
        values_to = "value"
      ) %>%
      # Add data quality and source columns
      mutate(
        data_quality = "direct",
        data_source = "traffic_safety_module",
        data_vintage = as.character(year),
        last_updated = as.character(Sys.time())
      )
    
    # Check if associated quality flags exist and use them
    if (any(grepl("_data_quality$", names(traffic_data)))) {
      db_traffic_data <- db_traffic_data %>%
        rowwise() %>%
        mutate(
          # Get the quality flag column for this variable if it exists
          quality_col = paste0(variable_name, "_data_quality"),
          # Use the flag if it exists, otherwise keep the default
          data_quality = ifelse(
            quality_col %in% names(traffic_data),
            traffic_data[[quality_col]][match(
              paste(geoid, year, variable_name),
              paste(traffic_data$geoid, traffic_data$year, quality_col)
            )],
            data_quality
          )
        ) %>%
        select(-quality_col) %>%
        ungroup()
    }
    
    # Check if sdoh_data table exists, create if not
    if (!DBI::dbExistsTable(con, "sdoh_data")) {
      DBI::dbExecute(con, "
        CREATE TABLE sdoh_data (
          geoid VARCHAR,
          year INTEGER,
          variable_name VARCHAR,
          value DOUBLE,
          data_quality VARCHAR,
          data_source VARCHAR,
          data_vintage VARCHAR,
          interpolation_method VARCHAR,
          ci_lower DOUBLE,
          ci_upper DOUBLE,
          confidence_level DOUBLE,
          last_updated TIMESTAMP,
          PRIMARY KEY (geoid, year, variable_name)
        )
      ")
    }
    
    # Check if variables table exists, create if not
    if (!DBI::dbExistsTable(con, "variables")) {
      DBI::dbExecute(con, "
        CREATE TABLE variables (
          variable_name VARCHAR PRIMARY KEY,
          domain VARCHAR,
          description VARCHAR,
          type VARCHAR,
          units VARCHAR,
          min_year INTEGER,
          max_year INTEGER,
          extended_only BOOLEAN
        )
      ")
    }
    
    # Add or update traffic safety variables
    traffic_vars <- data.frame(
      variable_name = c(
        "traffic_fatality_count", "traffic_fatality_rate_per_100k",
        "dui_fatality_count", "dui_fatality_rate_per_100k",
        "ped_bike_fatality_count", "ped_bike_fatality_rate_per_100k", 
        "speeding_fatality_count", "speeding_fatality_rate_per_100k"
      ),
      domain = "traffic_safety",
      description = c(
        "Traffic fatalities", "Traffic fatality rate per 100k population",
        "Alcohol-involved fatalities", "Alcohol-involved fatality rate per 100k",
        "Pedestrian/cyclist fatalities", "Pedestrian/cyclist fatality rate per 100k",
        "Speed-related fatalities", "Speed-related fatality rate per 100k"
      ),
      type = c(
        "count", "rate", "count", "rate", "count", "rate", "count", "rate"
      ),
      units = c(
        "fatalities", "per 100k", "fatalities", "per 100k",
        "fatalities", "per 100k", "fatalities", "per 100k"
      ),
      min_year = min(traffic_data$year, na.rm = TRUE),
      max_year = max(traffic_data$year, na.rm = TRUE),
      extended_only = TRUE
    )
    
    # Insert traffic safety variables
    for (i in 1:nrow(traffic_vars)) {
      var <- traffic_vars[i, ]
      
      # Check if variable exists
      if (DBI::dbGetQuery(con, glue::glue_sql(
        "SELECT COUNT(*) as count FROM variables WHERE variable_name = {var$variable_name}",
        .con = con
      ))$count > 0) {
        # Update existing variable
        DBI::dbExecute(con, glue::glue_sql(
          "UPDATE variables
           SET domain = {var$domain},
               description = {var$description},
               type = {var$type},
               units = {var$units},
               min_year = {var$min_year},
               max_year = {var$max_year},
               extended_only = {var$extended_only}
           WHERE variable_name = {var$variable_name}",
          .con = con
        ))
      } else {
        # Insert new variable
        DBI::dbExecute(con, glue::glue_sql(
          "INSERT INTO variables (
             variable_name, domain, description, type, units, 
             min_year, max_year, extended_only
           ) VALUES (
             {var$variable_name}, {var$domain}, {var$description}, {var$type}, {var$units}, 
             {var$min_year}, {var$max_year}, {var$extended_only}
           )",
          .con = con
        ))
      }
    }
    
    # Insert traffic safety data using UPSERT pattern
    # First create a temporary table
    DBI::dbExecute(con, "CREATE TEMPORARY TABLE temp_traffic_data AS SELECT * FROM sdoh_data LIMIT 0")
    
    # Insert into temp table
    DBI::dbWriteTable(con, "temp_traffic_data", db_traffic_data, append = TRUE)
    
    # Update existing records
    DBI::dbExecute(con, "
      UPDATE sdoh_data AS t1
      SET 
        value = t2.value,
        data_quality = t2.data_quality,
        data_source = t2.data_source,
        data_vintage = t2.data_vintage,
        last_updated = t2.last_updated
      FROM temp_traffic_data AS t2
      WHERE 
        t1.geoid = t2.geoid AND
        t1.year = t2.year AND
        t1.variable_name = t2.variable_name
    ")
    
    # Insert new records
    DBI::dbExecute(con, "
      INSERT INTO sdoh_data
      SELECT t2.*
      FROM temp_traffic_data t2
      LEFT JOIN sdoh_data t1 ON
        t1.geoid = t2.geoid AND
        t1.year = t2.year AND
        t1.variable_name = t2.variable_name
      WHERE t1.geoid IS NULL
    ")
    
    # Drop temp table
    DBI::dbExecute(con, "DROP TABLE temp_traffic_data")
    
    # Add forecasts if requested
    if (add_forecasts && !is.null(attr(traffic_data, "forecasts"))) {
      # Check if forecast table exists
      if (!DBI::dbExistsTable(con, "forecasts")) {
        DBI::dbExecute(con, "
          CREATE TABLE forecasts (
            geoid VARCHAR,
            variable_name VARCHAR,
            year INTEGER,
            forecast_value DOUBLE,
            lower_bound DOUBLE,
            upper_bound DOUBLE,
            method VARCHAR,
            created_date DATE,
            PRIMARY KEY (geoid, variable_name, year)
          )
        ")
      }
      
      # Process national forecasts
      if (!is.null(attr(traffic_data, "forecasts")$national)) {
        national_forecast <- attr(traffic_data, "forecasts")$national
        
        # Convert to database format
        forecast_db_data <- national_forecast %>%
          filter(type == "forecast") %>%
          mutate(
            geoid = "00000", # Use special code for national level
            variable_name = attr(national_forecast, "variable"),
            forecast_value = forecast,
            lower_bound = lower,
            upper_bound = upper,
            method = attr(national_forecast, "method"),
            created_date = Sys.Date()
          ) %>%
          select(geoid, variable_name, year, forecast_value, 
                 lower_bound, upper_bound, method, created_date)
        
        # Add to database using upsert pattern
        if (nrow(forecast_db_data) > 0) {
          # Create temp table
          DBI::dbExecute(con, "CREATE TEMPORARY TABLE temp_forecasts AS SELECT * FROM forecasts LIMIT 0")
          
          # Insert into temp table
          DBI::dbWriteTable(con, "temp_forecasts", forecast_db_data, append = TRUE)
          
          # Update existing records
          DBI::dbExecute(con, "
            UPDATE forecasts AS f
            SET 
              forecast_value = t.forecast_value,
              lower_bound = t.lower_bound,
              upper_bound = t.upper_bound,
              method = t.method,
              created_date = t.created_date
            FROM temp_forecasts AS t
            WHERE 
              f.geoid = t.geoid AND
              f.variable_name = t.variable_name AND
              f.year = t.year
          ")
          
          # Insert new records
          DBI::dbExecute(con, "
            INSERT INTO forecasts
            SELECT t.*
            FROM temp_forecasts t
            LEFT JOIN forecasts f ON
              f.geoid = t.geoid AND
              f.variable_name = t.variable_name AND
              f.year = t.year
            WHERE f.geoid IS NULL
          ")
          
          # Drop temp table
          DBI::dbExecute(con, "DROP TABLE temp_forecasts")
        }
      }
    }
    
    # Add spatial data if requested
    if (add_spatial && !is.null(attr(traffic_data, "spatial"))) {
      # Check if spatial_clusters table exists
      if (!DBI::dbExistsTable(con, "spatial_clusters")) {
        DBI::dbExecute(con, "
          CREATE TABLE spatial_clusters (
            geoid VARCHAR,
            variable_name VARCHAR,
            year INTEGER,
            cluster_type VARCHAR,
            analysis_method VARCHAR,
            analysis_date DATE,
            PRIMARY KEY (geoid, variable_name, year)
          )
        ")
      }
      
      # Get spatial data
      spatial_data <- attr(traffic_data, "spatial")$data
      
      if (!is.null(spatial_data) && "cluster_type" %in% names(spatial_data)) {
        # Convert to database format
        spatial_db_data <- spatial_data %>%
          st::st_drop_geometry() %>%
          filter(!is.na(cluster_type)) %>%
          mutate(
            geoid = GEOID,
            variable_name = "traffic_fatality_rate_per_100k",
            year = attr(traffic_data, "spatial")$year,
            analysis_method = "LISA",
            analysis_date = Sys.Date()
          ) %>%
          select(geoid, variable_name, year, cluster_type, analysis_method, analysis_date)
        
        # Add to database using upsert pattern
        if (nrow(spatial_db_data) > 0) {
          # Create temp table
          DBI::dbExecute(con, "CREATE TEMPORARY TABLE temp_clusters AS SELECT * FROM spatial_clusters LIMIT 0")
          
          # Insert into temp table
          DBI::dbWriteTable(con, "temp_clusters", spatial_db_data, append = TRUE)
          
          # Update existing records
          DBI::dbExecute(con, "
            UPDATE spatial_clusters AS sc
            SET 
              cluster_type = tc.cluster_type,
              analysis_method = tc.analysis_method,
              analysis_date = tc.analysis_date
            FROM temp_clusters AS tc
            WHERE 
              sc.geoid = tc.geoid AND
              sc.variable_name = tc.variable_name AND
              sc.year = tc.year
          ")
          
          # Insert new records
          DBI::dbExecute(con, "
            INSERT INTO spatial_clusters
            SELECT tc.*
            FROM temp_clusters tc
            LEFT JOIN spatial_clusters sc ON
              sc.geoid = tc.geoid AND
              sc.variable_name = tc.variable_name AND
              sc.year = tc.year
            WHERE sc.geoid IS NULL
          ")
          
          # Drop temp table
          DBI::dbExecute(con, "DROP TABLE temp_clusters")
        }
        
        # Add problem areas if available
        if (!is.null(attr(traffic_data, "spatial")$problem_areas)) {
          # Check if problem_areas table exists
          if (!DBI::dbExistsTable(con, "problem_areas")) {
            DBI::dbExecute(con, "
              CREATE TABLE problem_areas (
                geoid VARCHAR,
                county_name VARCHAR,
                variable_name VARCHAR,
                years_analyzed INTEGER,
                years_exceeding INTEGER,
                pct_years_exceeding DOUBLE,
                avg_value DOUBLE,
                max_value DOUBLE,
                latest_value DOUBLE,
                latest_year INTEGER,
                persistently_problematic BOOLEAN,
                analysis_date DATE,
                PRIMARY KEY (geoid, variable_name)
              )
            ")
          }
          
          # Get problem areas
          problem_areas <- attr(traffic_data, "spatial")$problem_areas
          
          if (nrow(problem_areas) > 0) {
            # Convert to database format
            problem_db_data <- problem_areas %>%
              mutate(
                geoid = sprintf("%05d", as.numeric(fips)),
                variable_name = "traffic_fatality_rate_per_100k",
                analysis_date = Sys.Date()
              ) %>%
              select(geoid, county_name, variable_name, years_analyzed, years_exceeding,
                     pct_years_exceeding, avg_value, max_value, latest_value, latest_year,
                     persistently_problematic, analysis_date)
            
            # Add to database using upsert pattern
            # Create temp table
            DBI::dbExecute(con, "CREATE TEMPORARY TABLE temp_problems AS SELECT * FROM problem_areas LIMIT 0")
            
            # Insert into temp table
            DBI::dbWriteTable(con, "temp_problems", problem_db_data, append = TRUE)
            
            # Update existing records
            DBI::dbExecute(con, "
              UPDATE problem_areas AS pa
              SET 
                county_name = tp.county_name,
                years_analyzed = tp.years_analyzed,
                years_exceeding = tp.years_exceeding,
                pct_years_exceeding = tp.pct_years_exceeding,
                avg_value = tp.avg_value,
                max_value = tp.max_value,
                latest_value = tp.latest_value,
                latest_year = tp.latest_year,
                persistently_problematic = tp.persistently_problematic,
                analysis_date = tp.analysis_date
              FROM temp_problems AS tp
              WHERE 
                pa.geoid = tp.geoid AND
                pa.variable_name = tp.variable_name
            ")
            
            # Insert new records
            DBI::dbExecute(con, "
              INSERT INTO problem_areas
              SELECT tp.*
              FROM temp_problems tp
              LEFT JOIN problem_areas pa ON
                pa.geoid = tp.geoid AND
                pa.variable_name = tp.variable_name
              WHERE pa.geoid IS NULL
            ")
            
            # Drop temp table
            DBI::dbExecute(con, "DROP TABLE temp_problems")
          }
        }
      }
    }
    
    # Create views if they don't exist
    # Traffic safety view
    DBI::dbExecute(con, "
      CREATE OR REPLACE VIEW traffic_safety_variables AS
      SELECT 
        c.geoid,
        c.name as county_name,
        c.state_name,
        d.variable_name,
        v.description as variable_description,
        v.units,
        d.year,
        d.value,
        d.data_quality,
        d.data_source
      FROM counties c
      JOIN sdoh_data d ON c.geoid = d.geoid
      JOIN variables v ON d.variable_name = v.variable_name
      WHERE v.domain = 'traffic_safety'
      ORDER BY c.geoid, d.variable_name, d.year
    ")
    
  }, finally = {
    # Always close the connection
    DBI::dbDisconnect(con)
  })
  
  return(TRUE)
}

# Example usage when run directly
if (!interactive()) {
  # Parse command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) > 0 && args[1] == "--test") {
    # Test the integration
    cat("Testing traffic safety integration...\n")
    
    # Load all modules
    module_statuses <- load_traffic_safety_modules()
    
    # Print module status
    for (module in names(module_statuses)) {
      status <- if (module_statuses[[module]]) "loaded" else "failed"
      cat(paste(module, ":", status, "\n"))
    }
    
    # Fetch enhanced data
    cat("Fetching enhanced traffic safety data...\n")
    enhanced_data <- fetch_enhanced_traffic_safety_data(
      years = 2018:2021,
      use_validation = TRUE,
      use_optimized_cache = TRUE,
      generate_forecasts = TRUE,
      spatial_analysis = TRUE
    )
    
    # Create visualizations
    cat("Creating visualizations...\n")
    vis_files <- create_traffic_safety_visualizations(
      enhanced_data,
      create_maps = TRUE,
      create_forecast_plots = TRUE
    )
    
    # Print results
    cat("\nTest complete.\n")
    cat("Data dimensions:", nrow(enhanced_data), "rows,", ncol(enhanced_data), "columns\n")
    cat("Years:", paste(sort(unique(enhanced_data$year)), collapse = ", "), "\n")
    cat("Enhancements applied:", paste(names(attr(enhanced_data, "enhancements")), collapse = ", "), "\n")
    
    if (length(vis_files) > 0) {
      cat("Visualizations created:\n")
      for (name in names(vis_files)) {
        cat(" -", name, ":", vis_files[[name]], "\n")
      }
    }
  }
}