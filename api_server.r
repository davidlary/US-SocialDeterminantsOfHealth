#!/usr/bin/env Rscript

# API Server for SDOH County-Level Dataset
# This script creates a RESTful API for accessing the SDOH county-level dataset
# providing programmatic access to the data for researchers and applications.

# Required packages
if (!require("plumber")) install.packages("plumber", repos = "https://cloud.r-project.org")
if (!require("tidyverse")) install.packages("tidyverse", repos = "https://cloud.r-project.org")
if (!require("DBI")) install.packages("DBI", repos = "https://cloud.r-project.org")
if (!require("duckdb")) install.packages("duckdb", repos = "https://cloud.r-project.org")
if (!require("jsonlite")) install.packages("jsonlite", repos = "https://cloud.r-project.org")
if (!require("lubridate")) install.packages("lubridate", repos = "https://cloud.r-project.org")
if (!require("glue")) install.packages("glue", repos = "https://cloud.r-project.org")
if (!require("memoise")) install.packages("memoise", repos = "https://cloud.r-project.org")
if (!require("cachem")) install.packages("cachem", repos = "https://cloud.r-project.org")

library(plumber)
library(tidyverse)
library(DBI)
library(duckdb)
library(jsonlite)
library(lubridate)
library(glue)
library(memoise)
library(cachem)

# Configuration
config <- list(
  db_path = "output/us_county_sdoh_unified.duckdb",
  port = 8000,
  host = "0.0.0.0",
  enable_docs = TRUE,
  cache_size = 512,  # Cache size in MB
  cache_timeout = 3600,  # Cache timeout in seconds (1 hour)
  row_limit = 10000,  # Maximum rows to return in a single request
  enable_cors = TRUE,
  api_keys = NULL,  # Leave NULL for no authentication
  log_requests = TRUE
)

#' Initialize the API server connection pool and cache
#'
#' @param db_path Path to the DuckDB database
#' @param cache_size Cache size in MB
#' @param cache_timeout Cache timeout in seconds
#'
#' @return List containing the connection and cache objects
initialize_api <- function(
  db_path = config$db_path,
  cache_size = config$cache_size,
  cache_timeout = config$cache_timeout
) {
  # Create connection to DuckDB
  tryCatch({
    con <- dbConnect(duckdb::duckdb(), dbdir = db_path)
    message("Connected to database: ", db_path)
  }, error = function(e) {
    stop("Error connecting to database: ", e$message)
  })
  
  # Set up query cache
  cache <- cachem::cache_mem(
    max_size = cache_size * 1024^2,
    evict = "lru",
    max_age = cache_timeout
  )
  
  # Memoize database functions with cache
  query_db <- memoise::memoise(
    function(query, params = NULL) {
      if (is.null(params)) {
        dbGetQuery(con, query)
      } else {
        dbGetQuery(con, glue_sql(query, .con = con, .envir = params))
      }
    },
    cache = cache
  )
  
  # Load basic database info
  db_info <- list(
    version = try(dbGetQuery(con, "SELECT version() AS version")$version, silent = TRUE),
    tables = try(dbListTables(con), silent = TRUE),
    variables = try(dbGetQuery(con, "SELECT * FROM variables"), silent = TRUE),
    counties = try(dbGetQuery(con, "SELECT COUNT(*) AS count FROM counties")$count, silent = TRUE),
    years = try(dbGetQuery(con, "SELECT MIN(year) AS min_year, MAX(year) AS max_year FROM sdoh_data"), silent = TRUE),
    variables_count = try(dbGetQuery(con, "SELECT COUNT(*) AS count FROM variables")$count, silent = TRUE),
    data_points = try(dbGetQuery(con, "SELECT COUNT(*) AS count FROM sdoh_data")$count, silent = TRUE)
  )
  
  # Create API context
  api_context <- list(
    con = con,
    cache = cache,
    query_db = query_db,
    db_info = db_info,
    start_time = Sys.time()
  )
  
  return(api_context)
}

#' Clean up the API server resources
#'
#' @param api_context API context from initialize_api
#'
#' @return NULL
cleanup_api <- function(api_context) {
  tryCatch({
    dbDisconnect(api_context$con)
    message("Database connection closed")
  }, error = function(e) {
    warning("Error closing database connection: ", e$message)
  })
  
  invisible(NULL)
}

#' Validate an API key
#'
#' @param api_key API key to validate
#' @param api_keys List of valid API keys
#'
#' @return TRUE if valid, FALSE otherwise
validate_api_key <- function(api_key, api_keys = config$api_keys) {
  # If no API keys are configured, allow all access
  if (is.null(api_keys)) {
    return(TRUE)
  }
  
  # Check if the provided key is in the list of valid keys
  api_key %in% api_keys
}

#' Format API response
#'
#' @param data Data to include in response
#' @param status Status code
#' @param message Message to include
#' @param meta Additional metadata
#'
#' @return List for JSON response
format_response <- function(
  data = NULL,
  status = 200,
  message = "Success",
  meta = NULL
) {
  # Create standard response structure
  response <- list(
    status = status,
    message = message,
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
    meta = if (is.null(meta)) list() else meta
  )
  
  # Add data if provided
  if (!is.null(data)) {
    response$data <- data
  }
  
  return(response)
}

#' Log API request
#'
#' @param req Request object
#' @param endpoint Endpoint accessed
#' @param status Status code
#' @param duration Duration in seconds
#'
#' @return NULL
log_request <- function(req, endpoint, status, duration) {
  if (!config$log_requests) {
    return(invisible(NULL))
  }
  
  log_entry <- data.frame(
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
    ip = req$REMOTE_ADDR,
    method = req$REQUEST_METHOD,
    endpoint = endpoint,
    query = ifelse(is.null(req$QUERY_STRING), "", req$QUERY_STRING),
    status = status,
    duration = round(duration, 4),
    user_agent = req$HTTP_USER_AGENT
  )
  
  message(glue::glue(
    "{log_entry$timestamp} | {log_entry$method} {log_entry$endpoint} | ",
    "Status: {log_entry$status} | Duration: {log_entry$duration}s"
  ))
  
  return(invisible(NULL))
}

#' Create and launch the SDOH API server
#'
#' @param db_path Path to the DuckDB database
#' @param port Port number to run on
#' @param host Host to bind to
#' @param enable_docs Whether to enable Swagger documentation
#' @param enable_cors Whether to enable CORS
#' @param api_keys List of valid API keys, or NULL for no authentication
#' @param cache_size Cache size in MB
#' @param cache_timeout Cache timeout in seconds
#' @param row_limit Maximum rows to return in a single request
#' @param log_requests Whether to log requests
#'
#' @return The plumber API object
launch_sdoh_api <- function(
  db_path = config$db_path,
  port = config$port,
  host = config$host,
  enable_docs = config$enable_docs,
  enable_cors = config$enable_cors,
  api_keys = config$api_keys,
  cache_size = config$cache_size,
  cache_timeout = config$cache_timeout,
  row_limit = config$row_limit,
  log_requests = config$log_requests
) {
  # Store configuration
  config$db_path <- db_path
  config$port <- port
  config$host <- host
  config$enable_docs <- enable_docs
  config$enable_cors <- enable_cors
  config$api_keys <- api_keys
  config$cache_size <- cache_size
  config$cache_timeout <- cache_timeout
  config$row_limit <- row_limit
  config$log_requests <- log_requests
  
  # Initialize API
  api_context <- initialize_api(
    db_path = db_path,
    cache_size = cache_size,
    cache_timeout = cache_timeout
  )
  
  # Create plumber API
  api <- plumber::pr()
  
  # Configure API
  api <- api %>%
    pr_set_docs(enable_docs)
  
  if (enable_cors) {
    api <- api %>%
      pr_set_header("Access-Control-Allow-Origin", "*") %>%
      pr_set_header("Access-Control-Allow-Methods", "GET, OPTIONS") %>%
      pr_set_header("Access-Control-Allow-Headers", "Content-Type, Authorization")
  }
  
  # Register filters
  api <- api %>%
    pr_filter("logger", function(req, res) {
      # Record start time
      req$start_time <- Sys.time()
      
      # Pass to next handler
      forward()
    }) %>%
    pr_filter("auth", function(req, res) {
      # Skip authentication if API keys are not configured
      if (is.null(api_keys)) {
        return(forward())
      }
      
      # Check for API key in header or query parameter
      api_key <- req$HTTP_AUTHORIZATION
      
      if (is.null(api_key)) {
        api_key <- req$args$api_key
      } else {
        # Remove "Bearer " prefix if present
        api_key <- sub("^Bearer\\s+", "", api_key)
      }
      
      # Validate API key
      if (is.null(api_key) || !validate_api_key(api_key)) {
        res$status <- 401
        return(format_response(
          status = 401,
          message = "Unauthorized: Invalid or missing API key"
        ))
      }
      
      # Pass to next handler
      forward()
    })
  
  # Register endpoints
  
  # Health Check
  api <- api %>%
    pr_get("/api/v1/health", function(req, res) {
      # Calculate uptime
      uptime <- difftime(Sys.time(), api_context$start_time, units = "secs")
      
      # Get database info
      db_info <- api_context$db_info
      
      # Create response
      health_data <- list(
        status = "healthy",
        uptime = as.numeric(uptime),
        database = list(
          connected = !is.null(api_context$con),
          path = db_path,
          tables = length(db_info$tables),
          variables = db_info$variables_count,
          counties = db_info$counties,
          years = list(
            min = db_info$years$min_year,
            max = db_info$years$max_year
          ),
          data_points = db_info$data_points
        ),
        cache = list(
          enabled = TRUE,
          size_mb = cache_size,
          timeout_sec = cache_timeout
        ),
        version = "1.0.0"
      )
      
      # Log request
      duration <- as.numeric(difftime(Sys.time(), req$start_time, units = "secs"))
      log_request(req, "/api/v1/health", 200, duration)
      
      return(format_response(
        data = health_data,
        message = "System is healthy"
      ))
    }, comments = "Check API health status")
  
  # Variables List
  api <- api %>%
    pr_get("/api/v1/variables", function(req, res, domain = NULL) {
      # Build query
      query <- "SELECT * FROM variables"
      
      # Filter by domain if provided
      if (!is.null(domain) && domain != "") {
        query <- paste0(query, " WHERE domain = '", domain, "'")
      }
      
      # Add order by
      query <- paste0(query, " ORDER BY domain, variable_name")
      
      # Execute query
      variables <- api_context$query_db(query)
      
      # Format response
      response_data <- list(
        count = nrow(variables),
        variables = variables
      )
      
      # Log request
      duration <- as.numeric(difftime(Sys.time(), req$start_time, units = "secs"))
      log_request(req, "/api/v1/variables", 200, duration)
      
      return(format_response(
        data = response_data,
        message = paste("Retrieved", nrow(variables), "variables")
      ))
    }, comments = "Get list of all variables")
  
  # Domains List
  api <- api %>%
    pr_get("/api/v1/domains", function(req, res) {
      # Build query
      query <- "SELECT DISTINCT domain, COUNT(*) as variable_count FROM variables GROUP BY domain ORDER BY domain"
      
      # Execute query
      domains <- api_context$query_db(query)
      
      # Format response
      response_data <- list(
        count = nrow(domains),
        domains = domains
      )
      
      # Log request
      duration <- as.numeric(difftime(Sys.time(), req$start_time, units = "secs"))
      log_request(req, "/api/v1/domains", 200, duration)
      
      return(format_response(
        data = response_data,
        message = paste("Retrieved", nrow(domains), "domains")
      ))
    }, comments = "Get list of all variable domains")
  
  # Counties List
  api <- api %>%
    pr_get("/api/v1/counties", function(req, res, state = NULL) {
      # Build query
      query <- "SELECT * FROM counties"
      
      # Filter by state if provided
      if (!is.null(state) && state != "") {
        # Check if it's a FIPS code (2 digits) or state name
        if (nchar(state) == 2 && grepl("^[0-9]{2}$", state)) {
          query <- paste0(query, " WHERE state_fips = '", state, "'")
        } else {
          query <- paste0(query, " WHERE state_name = '", state, "'")
        }
      }
      
      # Add order by
      query <- paste0(query, " ORDER BY state_name, name")
      
      # Execute query
      counties <- api_context$query_db(query)
      
      # Format response
      response_data <- list(
        count = nrow(counties),
        counties = counties
      )
      
      # Log request
      duration <- as.numeric(difftime(Sys.time(), req$start_time, units = "secs"))
      log_request(req, "/api/v1/counties", 200, duration)
      
      return(format_response(
        data = response_data,
        message = paste("Retrieved", nrow(counties), "counties")
      ))
    }, comments = "Get list of all counties")
  
  # Years List
  api <- api %>%
    pr_get("/api/v1/years", function(req, res, variable = NULL) {
      # Build query
      if (!is.null(variable) && variable != "") {
        query <- paste0(
          "SELECT DISTINCT year FROM sdoh_data WHERE variable_name = '", 
          variable, "' ORDER BY year DESC"
        )
      } else {
        query <- "SELECT DISTINCT year FROM sdoh_data ORDER BY year DESC"
      }
      
      # Execute query
      years <- api_context$query_db(query)
      
      # Format response
      response_data <- list(
        count = nrow(years),
        years = years$year
      )
      
      # Log request
      duration <- as.numeric(difftime(Sys.time(), req$start_time, units = "secs"))
      log_request(req, "/api/v1/years", 200, duration)
      
      return(format_response(
        data = response_data,
        message = paste("Retrieved", nrow(years), "years")
      ))
    }, comments = "Get list of all available years")
  
  # Data Query
  api <- api %>%
    pr_get("/api/v1/data", function(
      req, res, 
      variable = NULL, 
      year = NULL, 
      county = NULL, 
      state = NULL,
      quality = NULL,
      limit = NULL,
      offset = 0
    ) {
      # Validate inputs
      if (is.null(variable) || variable == "") {
        res$status <- 400
        
        # Log request
        duration <- as.numeric(difftime(Sys.time(), req$start_time, units = "secs"))
        log_request(req, "/api/v1/data", 400, duration)
        
        return(format_response(
          status = 400,
          message = "Variable parameter is required"
        ))
      }
      
      # Set row limit
      if (is.null(limit) || limit == "") {
        limit <- row_limit
      } else {
        limit <- as.numeric(limit)
        if (is.na(limit) || limit <= 0) {
          limit <- row_limit
        } else {
          limit <- min(limit, row_limit)
        }
      }
      
      # Convert offset to numeric
      offset <- as.numeric(offset)
      if (is.na(offset) || offset < 0) {
        offset <- 0
      }
      
      # Build query
      query <- "
        SELECT 
          d.geoid, 
          c.name as county_name, 
          c.state_name,
          d.year, 
          d.variable_name, 
          d.value,
          d.data_quality, 
          d.data_source,
          d.interpolation_method,
          d.ci_lower,
          d.ci_upper
        FROM sdoh_data d
        JOIN counties c ON d.geoid = c.geoid
        WHERE d.variable_name = {variable}
      "
      
      # Add filters
      params <- list(variable = variable)
      
      if (!is.null(year) && year != "") {
        if (grepl("-", year)) {
          # Range of years (e.g., 2010-2020)
          year_parts <- strsplit(year, "-")[[1]]
          if (length(year_parts) == 2) {
            query <- paste0(query, " AND d.year BETWEEN {year_start} AND {year_end}")
            params$year_start <- as.numeric(year_parts[1])
            params$year_end <- as.numeric(year_parts[2])
          }
        } else {
          # Single year
          query <- paste0(query, " AND d.year = {year}")
          params$year <- as.numeric(year)
        }
      }
      
      if (!is.null(county) && county != "") {
        # Check if it's a FIPS code (5 digits) or county name
        if (nchar(county) == 5 && grepl("^[0-9]{5}$", county)) {
          query <- paste0(query, " AND d.geoid = {county}")
          params$county <- county
        } else {
          query <- paste0(query, " AND c.name LIKE {county_pattern}")
          params$county_pattern <- paste0("%", county, "%")
        }
      }
      
      if (!is.null(state) && state != "") {
        # Check if it's a FIPS code (2 digits) or state name
        if (nchar(state) == 2 && grepl("^[0-9]{2}$", state)) {
          query <- paste0(query, " AND c.state_fips = {state}")
          params$state <- state
        } else {
          query <- paste0(query, " AND c.state_name = {state}")
          params$state <- state
        }
      }
      
      if (!is.null(quality) && quality != "") {
        query <- paste0(query, " AND d.data_quality = {quality}")
        params$quality <- quality
      }
      
      # Count total rows (for pagination info)
      count_query <- paste0("SELECT COUNT(*) as total FROM (", query, ") as subquery")
      total_count <- tryCatch({
        api_context$query_db(count_query, params)$total
      }, error = function(e) {
        0
      })
      
      # Add order by, limit, and offset
      query <- paste0(query, " ORDER BY d.year DESC, c.state_name, c.name LIMIT {limit} OFFSET {offset}")
      params$limit <- limit
      params$offset <- offset
      
      # Execute query
      result <- tryCatch({
        api_context$query_db(query, params)
      }, error = function(e) {
        res$status <- 500
        
        # Log request
        duration <- as.numeric(difftime(Sys.time(), req$start_time, units = "secs"))
        log_request(req, "/api/v1/data", 500, duration)
        
        return(format_response(
          status = 500,
          message = paste("Database query error:", e$message)
        ))
      })
      
      # Get variable info
      var_info <- api_context$query_db(
        "SELECT * FROM variables WHERE variable_name = {variable}",
        list(variable = variable)
      )
      
      # Format response
      if (nrow(result) == 0) {
        response_data <- list(
          count = 0,
          total = total_count,
          limit = limit,
          offset = offset,
          variable = if(nrow(var_info) > 0) var_info else NULL,
          data = list()
        )
        
        response_message <- "No data found matching the criteria"
      } else {
        response_data <- list(
          count = nrow(result),
          total = total_count,
          limit = limit,
          offset = offset,
          variable = if(nrow(var_info) > 0) var_info else NULL,
          data = result
        )
        
        response_message <- paste("Retrieved", nrow(result), "data points")
      }
      
      # Log request
      duration <- as.numeric(difftime(Sys.time(), req$start_time, units = "secs"))
      log_request(req, "/api/v1/data", 200, duration)
      
      return(format_response(
        data = response_data,
        message = response_message
      ))
    }, comments = "Query data based on variable, year, county, and state")
  
  # Time Series Query
  api <- api %>%
    pr_get("/api/v1/timeseries", function(
      req, res, 
      variable = NULL, 
      county = NULL, 
      state = NULL,
      start_year = NULL,
      end_year = NULL,
      quality = NULL
    ) {
      # Validate inputs
      if (is.null(variable) || variable == "") {
        res$status <- 400
        
        # Log request
        duration <- as.numeric(difftime(Sys.time(), req$start_time, units = "secs"))
        log_request(req, "/api/v1/timeseries", 400, duration)
        
        return(format_response(
          status = 400,
          message = "Variable parameter is required"
        ))
      }
      
      if (is.null(county) || county == "") {
        res$status <- 400
        
        # Log request
        duration <- as.numeric(difftime(Sys.time(), req$start_time, units = "secs"))
        log_request(req, "/api/v1/timeseries", 400, duration)
        
        return(format_response(
          status = 400,
          message = "County parameter is required"
        ))
      }
      
      # Build query
      query <- "
        SELECT 
          d.geoid, 
          c.name as county_name, 
          c.state_name,
          d.year, 
          d.variable_name, 
          d.value,
          d.data_quality, 
          d.data_source,
          d.interpolation_method,
          d.ci_lower,
          d.ci_upper
        FROM sdoh_data d
        JOIN counties c ON d.geoid = c.geoid
        WHERE d.variable_name = {variable}
      "
      
      # Add filters
      params <- list(variable = variable)
      
      # Check if county is a FIPS code (5 digits) or county name
      if (nchar(county) == 5 && grepl("^[0-9]{5}$", county)) {
        query <- paste0(query, " AND d.geoid = {county}")
        params$county <- county
      } else {
        query <- paste0(query, " AND c.name LIKE {county_pattern}")
        params$county_pattern <- paste0("%", county, "%")
      }
      
      if (!is.null(state) && state != "") {
        # Check if it's a FIPS code (2 digits) or state name
        if (nchar(state) == 2 && grepl("^[0-9]{2}$", state)) {
          query <- paste0(query, " AND c.state_fips = {state}")
          params$state <- state
        } else {
          query <- paste0(query, " AND c.state_name = {state}")
          params$state <- state
        }
      }
      
      if (!is.null(start_year) && start_year != "") {
        query <- paste0(query, " AND d.year >= {start_year}")
        params$start_year <- as.numeric(start_year)
      }
      
      if (!is.null(end_year) && end_year != "") {
        query <- paste0(query, " AND d.year <= {end_year}")
        params$end_year <- as.numeric(end_year)
      }
      
      if (!is.null(quality) && quality != "") {
        query <- paste0(query, " AND d.data_quality = {quality}")
        params$quality <- quality
      }
      
      # Add order by
      query <- paste0(query, " ORDER BY d.year")
      
      # Execute query
      result <- tryCatch({
        api_context$query_db(query, params)
      }, error = function(e) {
        res$status <- 500
        
        # Log request
        duration <- as.numeric(difftime(Sys.time(), req$start_time, units = "secs"))
        log_request(req, "/api/v1/timeseries", 500, duration)
        
        return(format_response(
          status = 500,
          message = paste("Database query error:", e$message)
        ))
      })
      
      # Get variable info
      var_info <- api_context$query_db(
        "SELECT * FROM variables WHERE variable_name = {variable}",
        list(variable = variable)
      )
      
      # Format response
      if (nrow(result) == 0) {
        response_data <- list(
          count = 0,
          variable = if(nrow(var_info) > 0) var_info else NULL,
          county = county,
          state = state,
          data = list()
        )
        
        response_message <- "No time series data found matching the criteria"
      } else {
        response_data <- list(
          count = nrow(result),
          variable = if(nrow(var_info) > 0) var_info else NULL,
          county = if(nrow(result) > 0) result$county_name[1] else county,
          state = if(nrow(result) > 0) result$state_name[1] else state,
          data = result
        )
        
        response_message <- paste("Retrieved time series with", nrow(result), "data points")
      }
      
      # Log request
      duration <- as.numeric(difftime(Sys.time(), req$start_time, units = "secs"))
      log_request(req, "/api/v1/timeseries", 200, duration)
      
      return(format_response(
        data = response_data,
        message = response_message
      ))
    }, comments = "Get time series data for a specific variable and county")
  
  # Correlation Query
  api <- api %>%
    pr_get("/api/v1/correlation", function(
      req, res, 
      variable1 = NULL, 
      variable2 = NULL, 
      year = NULL, 
      state = NULL
    ) {
      # Validate inputs
      if (is.null(variable1) || variable1 == "" || is.null(variable2) || variable2 == "") {
        res$status <- 400
        
        # Log request
        duration <- as.numeric(difftime(Sys.time(), req$start_time, units = "secs"))
        log_request(req, "/api/v1/correlation", 400, duration)
        
        return(format_response(
          status = 400,
          message = "Both variable1 and variable2 parameters are required"
        ))
      }
      
      if (is.null(year) || year == "") {
        # Get the most recent common year for both variables
        year_query <- "
          WITH var1_years AS (
            SELECT DISTINCT year FROM sdoh_data WHERE variable_name = {variable1}
          ),
          var2_years AS (
            SELECT DISTINCT year FROM sdoh_data WHERE variable_name = {variable2}
          )
          SELECT MAX(year) as max_year
          FROM var1_years 
          WHERE year IN (SELECT year FROM var2_years)
        "
        
        year_result <- api_context$query_db(
          year_query, 
          list(variable1 = variable1, variable2 = variable2)
        )
        
        if (nrow(year_result) == 0 || is.na(year_result$max_year)) {
          res$status <- 404
          
          # Log request
          duration <- as.numeric(difftime(Sys.time(), req$start_time, units = "secs"))
          log_request(req, "/api/v1/correlation", 404, duration)
          
          return(format_response(
            status = 404,
            message = "No common year found for both variables"
          ))
        }
        
        year <- year_result$max_year
      } else {
        year <- as.numeric(year)
      }
      
      # Build query
      query <- "
        WITH var1_data AS (
          SELECT 
            d.geoid, 
            c.name as county_name, 
            c.state_name,
            d.value as value1,
            d.data_quality as quality1
          FROM sdoh_data d
          JOIN counties c ON d.geoid = c.geoid
          WHERE d.variable_name = {variable1}
            AND d.year = {year}
        ),
        var2_data AS (
          SELECT 
            d.geoid, 
            d.value as value2,
            d.data_quality as quality2
          FROM sdoh_data d
          WHERE d.variable_name = {variable2}
            AND d.year = {year}
        )
        SELECT 
          v1.geoid, 
          v1.county_name, 
          v1.state_name,
          v1.value1,
          v2.value2,
          v1.quality1,
          v2.quality2
        FROM var1_data v1
        JOIN var2_data v2 ON v1.geoid = v2.geoid
      "
      
      # Add state filter if provided
      params <- list(variable1 = variable1, variable2 = variable2, year = year)
      
      if (!is.null(state) && state != "") {
        # Check if it's a FIPS code (2 digits) or state name
        if (nchar(state) == 2 && grepl("^[0-9]{2}$", state)) {
          query <- paste0(query, " WHERE v1.state_name IN (SELECT state_name FROM counties WHERE state_fips = {state})")
          params$state <- state
        } else {
          query <- paste0(query, " WHERE v1.state_name = {state}")
          params$state <- state
        }
      }
      
      # Add order by
      query <- paste0(query, " ORDER BY v1.state_name, v1.county_name")
      
      # Execute query
      result <- tryCatch({
        api_context$query_db(query, params)
      }, error = function(e) {
        res$status <- 500
        
        # Log request
        duration <- as.numeric(difftime(Sys.time(), req$start_time, units = "secs"))
        log_request(req, "/api/v1/correlation", 500, duration)
        
        return(format_response(
          status = 500,
          message = paste("Database query error:", e$message)
        ))
      })
      
      # Get variable info
      var1_info <- api_context$query_db(
        "SELECT * FROM variables WHERE variable_name = {variable}",
        list(variable = variable1)
      )
      
      var2_info <- api_context$query_db(
        "SELECT * FROM variables WHERE variable_name = {variable}",
        list(variable = variable2)
      )
      
      # Calculate correlation if we have enough data points
      correlation <- NA
      if (nrow(result) >= 3) {
        correlation <- cor(result$value1, result$value2, use = "complete.obs")
      }
      
      # Format response
      if (nrow(result) == 0) {
        response_data <- list(
          count = 0,
          year = year,
          variable1 = if(nrow(var1_info) > 0) var1_info else NULL,
          variable2 = if(nrow(var2_info) > 0) var2_info else NULL,
          correlation = NA,
          data = list()
        )
        
        response_message <- "No correlation data found matching the criteria"
      } else {
        response_data <- list(
          count = nrow(result),
          year = year,
          variable1 = if(nrow(var1_info) > 0) var1_info else NULL,
          variable2 = if(nrow(var2_info) > 0) var2_info else NULL,
          correlation = correlation,
          data = result
        )
        
        response_message <- paste(
          "Correlation between", variable1, "and", variable2, 
          "in", year, "is", round(correlation, 3)
        )
      }
      
      # Log request
      duration <- as.numeric(difftime(Sys.time(), req$start_time, units = "secs"))
      log_request(req, "/api/v1/correlation", 200, duration)
      
      return(format_response(
        data = response_data,
        message = response_message
      ))
    }, comments = "Get correlation between two variables for a specific year")
  
  # Summary Statistics
  api <- api %>%
    pr_get("/api/v1/summary", function(
      req, res, 
      variable = NULL, 
      year = NULL, 
      state = NULL
    ) {
      # Validate inputs
      if (is.null(variable) || variable == "") {
        res$status <- 400
        
        # Log request
        duration <- as.numeric(difftime(Sys.time(), req$start_time, units = "secs"))
        log_request(req, "/api/v1/summary", 400, duration)
        
        return(format_response(
          status = 400,
          message = "Variable parameter is required"
        ))
      }
      
      if (is.null(year) || year == "") {
        # Get the most recent year for this variable
        year_query <- "
          SELECT MAX(year) as max_year
          FROM sdoh_data
          WHERE variable_name = {variable}
        "
        
        year_result <- api_context$query_db(year_query, list(variable = variable))
        
        if (nrow(year_result) == 0 || is.na(year_result$max_year)) {
          res$status <- 404
          
          # Log request
          duration <- as.numeric(difftime(Sys.time(), req$start_time, units = "secs"))
          log_request(req, "/api/v1/summary", 404, duration)
          
          return(format_response(
            status = 404,
            message = "No data found for this variable"
          ))
        }
        
        year <- year_result$max_year
      } else {
        year <- as.numeric(year)
      }
      
      # Build query
      query <- "
        SELECT 
          d.value,
          d.data_quality,
          c.state_name
        FROM sdoh_data d
        JOIN counties c ON d.geoid = c.geoid
        WHERE d.variable_name = {variable}
          AND d.year = {year}
      "
      
      # Add state filter if provided
      params <- list(variable = variable, year = year)
      
      if (!is.null(state) && state != "") {
        # Check if it's a FIPS code (2 digits) or state name
        if (nchar(state) == 2 && grepl("^[0-9]{2}$", state)) {
          query <- paste0(query, " AND c.state_fips = {state}")
          params$state <- state
        } else {
          query <- paste0(query, " AND c.state_name = {state}")
          params$state <- state
        }
      }
      
      # Execute query
      result <- tryCatch({
        api_context$query_db(query, params)
      }, error = function(e) {
        res$status <- 500
        
        # Log request
        duration <- as.numeric(difftime(Sys.time(), req$start_time, units = "secs"))
        log_request(req, "/api/v1/summary", 500, duration)
        
        return(format_response(
          status = 500,
          message = paste("Database query error:", e$message)
        ))
      })
      
      # Get variable info
      var_info <- api_context$query_db(
        "SELECT * FROM variables WHERE variable_name = {variable}",
        list(variable = variable)
      )
      
      # Calculate summary statistics
      if (nrow(result) > 0) {
        stats <- list(
          count = nrow(result),
          min = min(result$value, na.rm = TRUE),
          max = max(result$value, na.rm = TRUE),
          mean = mean(result$value, na.rm = TRUE),
          median = median(result$value, na.rm = TRUE),
          sd = sd(result$value, na.rm = TRUE),
          q1 = quantile(result$value, 0.25, na.rm = TRUE),
          q3 = quantile(result$value, 0.75, na.rm = TRUE)
        )
        
        # Calculate counts by data quality
        quality_counts <- table(result$data_quality)
        quality_stats <- list()
        
        for (quality in names(quality_counts)) {
          quality_stats[[quality]] <- quality_counts[[quality]]
        }
        
        # Calculate state summaries if no state filter was applied
        state_summaries <- NULL
        
        if (is.null(state) || state == "") {
          state_summaries <- result %>%
            group_by(state_name) %>%
            summarize(
              count = n(),
              mean = mean(value, na.rm = TRUE),
              median = median(value, na.rm = TRUE),
              min = min(value, na.rm = TRUE),
              max = max(value, na.rm = TRUE)
            ) %>%
            arrange(state_name) %>%
            as.data.frame()
        }
        
        response_data <- list(
          variable = if(nrow(var_info) > 0) var_info else NULL,
          year = year,
          stats = stats,
          quality = quality_stats,
          state_summaries = state_summaries
        )
        
        response_message <- paste(
          "Summary statistics for", variable, "in", year
        )
      } else {
        response_data <- list(
          variable = if(nrow(var_info) > 0) var_info else NULL,
          year = year,
          stats = NULL,
          quality = NULL,
          state_summaries = NULL
        )
        
        response_message <- "No data found matching the criteria"
      }
      
      # Log request
      duration <- as.numeric(difftime(Sys.time(), req$start_time, units = "secs"))
      log_request(req, "/api/v1/summary", 200, duration)
      
      return(format_response(
        data = response_data,
        message = response_message
      ))
    }, comments = "Get summary statistics for a variable in a specific year")
  
  # Download Data Endpoint
  api <- api %>%
    pr_get("/api/v1/download", function(
      req, res, 
      variable = NULL, 
      year = NULL, 
      county = NULL, 
      state = NULL,
      format = "csv"
    ) {
      # Validate inputs
      if (is.null(variable) || variable == "") {
        res$status <- 400
        
        # Log request
        duration <- as.numeric(difftime(Sys.time(), req$start_time, units = "secs"))
        log_request(req, "/api/v1/download", 400, duration)
        
        return(format_response(
          status = 400,
          message = "Variable parameter is required"
        ))
      }
      
      # Build query
      query <- "
        SELECT 
          d.geoid, 
          c.name as county_name, 
          c.state_name,
          d.year, 
          d.variable_name, 
          d.value,
          d.data_quality, 
          d.data_source,
          d.interpolation_method,
          d.ci_lower,
          d.ci_upper
        FROM sdoh_data d
        JOIN counties c ON d.geoid = c.geoid
        WHERE d.variable_name = {variable}
      "
      
      # Add filters
      params <- list(variable = variable)
      
      if (!is.null(year) && year != "") {
        if (grepl("-", year)) {
          # Range of years (e.g., 2010-2020)
          year_parts <- strsplit(year, "-")[[1]]
          if (length(year_parts) == 2) {
            query <- paste0(query, " AND d.year BETWEEN {year_start} AND {year_end}")
            params$year_start <- as.numeric(year_parts[1])
            params$year_end <- as.numeric(year_parts[2])
          }
        } else {
          # Single year
          query <- paste0(query, " AND d.year = {year}")
          params$year <- as.numeric(year)
        }
      }
      
      if (!is.null(county) && county != "") {
        # Check if it's a FIPS code (5 digits) or county name
        if (nchar(county) == 5 && grepl("^[0-9]{5}$", county)) {
          query <- paste0(query, " AND d.geoid = {county}")
          params$county <- county
        } else {
          query <- paste0(query, " AND c.name LIKE {county_pattern}")
          params$county_pattern <- paste0("%", county, "%")
        }
      }
      
      if (!is.null(state) && state != "") {
        # Check if it's a FIPS code (2 digits) or state name
        if (nchar(state) == 2 && grepl("^[0-9]{2}$", state)) {
          query <- paste0(query, " AND c.state_fips = {state}")
          params$state <- state
        } else {
          query <- paste0(query, " AND c.state_name = {state}")
          params$state <- state
        }
      }
      
      # Add order by
      query <- paste0(query, " ORDER BY d.year DESC, c.state_name, c.name")
      
      # Execute query
      result <- tryCatch({
        api_context$query_db(query, params)
      }, error = function(e) {
        res$status <- 500
        
        # Log request
        duration <- as.numeric(difftime(Sys.time(), req$start_time, units = "secs"))
        log_request(req, "/api/v1/download", 500, duration)
        
        return(format_response(
          status = 500,
          message = paste("Database query error:", e$message)
        ))
      })
      
      # Get variable info
      var_info <- api_context$query_db(
        "SELECT * FROM variables WHERE variable_name = {variable}",
        list(variable = variable)
      )
      
      # Format filename
      file_desc <- variable
      if (!is.null(year) && year != "") {
        file_desc <- paste0(file_desc, "_", year)
      }
      if (!is.null(state) && state != "") {
        file_desc <- paste0(file_desc, "_", state)
      }
      
      # Generate output based on format
      if (tolower(format) == "json") {
        # JSON format
        output <- jsonlite::toJSON(
          list(
            variable = if(nrow(var_info) > 0) var_info else NULL,
            data = result
          ),
          auto_unbox = TRUE, 
          pretty = TRUE
        )
        
        res$setHeader("Content-Type", "application/json")
        res$setHeader("Content-Disposition", paste0('attachment; filename="', file_desc, '.json"'))
      } else {
        # Default to CSV format
        output <- format_csv(result)
        
        res$setHeader("Content-Type", "text/csv")
        res$setHeader("Content-Disposition", paste0('attachment; filename="', file_desc, '.csv"'))
      }
      
      # Log request
      duration <- as.numeric(difftime(Sys.time(), req$start_time, units = "secs"))
      log_request(req, "/api/v1/download", 200, duration)
      
      return(output)
    }, comments = "Download data for a variable in CSV or JSON format")
  
  # Add shutdown hook
  pr_hooks <- list(
    preroute = NULL,
    postroute = NULL,
    exit = function() {
      message("API server shutting down...")
      cleanup_api(api_context)
    }
  )
  api$hooks <- pr_hooks
  
  # Launch API
  message(paste0("Launching SDOH API server on http://", host, ":", port, "/"))
  
  # Create startup message with available endpoints
  startup_message <- paste0(
    "API documentation available at: http://", host, ":", port, "/__docs__/\n",
    "Available endpoints:\n",
    "- GET /api/v1/health              - Check API health status\n",
    "- GET /api/v1/variables           - Get list of all variables\n",
    "- GET /api/v1/domains             - Get list of all variable domains\n",
    "- GET /api/v1/counties            - Get list of all counties\n",
    "- GET /api/v1/years               - Get list of all available years\n",
    "- GET /api/v1/data                - Query data based on variable, year, county, and state\n",
    "- GET /api/v1/timeseries          - Get time series data for a specific variable and county\n",
    "- GET /api/v1/correlation         - Get correlation between two variables for a specific year\n",
    "- GET /api/v1/summary             - Get summary statistics for a variable in a specific year\n",
    "- GET /api/v1/download            - Download data for a variable in CSV or JSON format\n"
  )
  
  message(startup_message)
  
  # Run the API
  plumber::pr_run(
    api,
    host = host,
    port = port
  )
}

#' Create and run the SDOH API server
#'
#' @param db_path Path to the DuckDB database
#' @param port Port number to run on
#' @param host Host to bind to
#' @param ... Additional parameters to pass to launch_sdoh_api
#'
#' @return NULL (runs until interrupted)
run_api_server <- function(
  db_path = "output/us_county_sdoh_unified.duckdb",
  port = 8000,
  host = "0.0.0.0",
  ...
) {
  launch_sdoh_api(
    db_path = db_path,
    port = port,
    host = host,
    ...
  )
}

# If script is run directly, launch the API server
if (!interactive()) {
  # Check if command line arguments were provided
  args <- commandArgs(trailingOnly = TRUE)
  
  # Parse command line arguments
  if (length(args) > 0) {
    for (i in 1:length(args)) {
      if (args[i] == "--port" && i < length(args)) {
        config$port <- as.numeric(args[i + 1])
      } else if (args[i] == "--host" && i < length(args)) {
        config$host <- args[i + 1]
      } else if (args[i] == "--db" && i < length(args)) {
        config$db_path <- args[i + 1]
      } else if (args[i] == "--no-docs") {
        config$enable_docs <- FALSE
      } else if (args[i] == "--no-cors") {
        config$enable_cors <- FALSE
      } else if (args[i] == "--cache-size" && i < length(args)) {
        config$cache_size <- as.numeric(args[i + 1])
      } else if (args[i] == "--cache-timeout" && i < length(args)) {
        config$cache_timeout <- as.numeric(args[i + 1])
      }
    }
  }
  
  # Launch API server
  run_api_server(
    db_path = config$db_path,
    port = config$port,
    host = config$host,
    enable_docs = config$enable_docs,
    enable_cors = config$enable_cors,
    cache_size = config$cache_size,
    cache_timeout = config$cache_timeout
  )
}