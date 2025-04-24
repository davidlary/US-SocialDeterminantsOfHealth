#!/usr/bin/env Rscript

# test_database_optimizations.r
# Script to test the performance of database optimizations

# Load required packages
library(dplyr)
library(DBI)
library(duckdb)
library(data.table)
library(yaml)
library(microbenchmark)

# Source utility functions
source("pipeline_modules/module_core.r")

# Load configuration
config <- yaml::read_yaml("config.yaml")
db_path <- config$database$db_path

# Connect to the database
log_message("Connecting to database...", level = "INFO", show_console = TRUE)
con <- dbConnect(duckdb::duckdb(), dbdir = db_path)

# Define a function to run a query and measure its performance
run_query_benchmark <- function(query, name, repeat_count = 10) {
  cat(paste("\nBenchmarking query:", name, "\n"))
  cat(paste("Query:", query, "\n"))
  
  # Run the query once to warm up any caches
  result <- dbGetQuery(con, query)
  cat(paste("  Result rows:", nrow(result), "\n"))
  
  # Benchmark the query
  times <- NULL
  for (i in 1:repeat_count) {
    start_time <- Sys.time()
    dbGetQuery(con, query)
    end_time <- Sys.time()
    times <- c(times, as.numeric(difftime(end_time, start_time, units = "secs")))
  }
  
  # Calculate statistics
  mean_time <- mean(times)
  median_time <- median(times)
  min_time <- min(times)
  max_time <- max(times)
  
  cat(paste("  Mean execution time:", round(mean_time * 1000, 2), "ms\n"))
  cat(paste("  Median execution time:", round(median_time * 1000, 2), "ms\n"))
  cat(paste("  Min execution time:", round(min_time * 1000, 2), "ms\n"))
  cat(paste("  Max execution time:", round(max_time * 1000, 2), "ms\n"))
  
  return(list(
    query = query,
    name = name,
    mean_time = mean_time,
    median_time = median_time,
    min_time = min_time,
    max_time = max_time
  ))
}

# Test queries
cat("\n=== RUNNING DATABASE OPTIMIZATION TESTS ===\n\n")

# 1. Get database information
cat("Database Information:\n")
tables <- dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='table'")
cat(paste("Tables in database:", paste(tables$name, collapse = ", "), "\n"))

total_rows <- dbGetQuery(con, "SELECT COUNT(*) as count FROM sdoh_data")
cat(paste("Total data rows:", total_rows$count, "\n"))

# 2. Test regular views vs materialized views
view_exists <- tryCatch({
  dbGetQuery(con, "SELECT 1 FROM latest_data LIMIT 1")
  TRUE
}, error = function(e) FALSE)

materialized_view_exists <- tryCatch({
  dbGetQuery(con, "SELECT 1 FROM latest_data_materialized LIMIT 1")
  TRUE
}, error = function(e) FALSE)

cat("\nView availability:\n")
cat(paste("Regular view 'latest_data':", ifelse(view_exists, "Available", "Not available"), "\n"))
cat(paste("Materialized view 'latest_data_materialized':", 
         ifelse(materialized_view_exists, "Available", "Not available"), "\n"))

# 3. Run benchmark tests

# Test 1: Basic query using standard indices
test1 <- run_query_benchmark(
  "SELECT * FROM sdoh_data WHERE geoid = '01001' AND year = 2020",
  "Basic query with primary key filtering"
)

# Test 2: Query using composite indices
test2 <- run_query_benchmark(
  "SELECT * FROM sdoh_data WHERE geoid = '01001' AND variable_name = 'traffic_fatality_rate'",
  "Query using composite index (geoid + variable)"
)

# Test 3: Aggregation query
test3 <- run_query_benchmark(
  "SELECT year, COUNT(*) as count, AVG(value) as avg_value 
   FROM sdoh_data 
   WHERE variable_name = 'traffic_fatality_rate' 
   GROUP BY year 
   ORDER BY year",
  "Aggregation query with filtering and grouping"
)

# Test 4: Join query
test4 <- run_query_benchmark(
  "SELECT c.name, c.state_name, d.year, d.value 
   FROM counties c 
   JOIN sdoh_data d ON c.geoid = d.geoid 
   WHERE d.variable_name = 'traffic_fatality_rate' AND d.year = 2020 
   ORDER BY d.value DESC 
   LIMIT 10",
  "Join query with filtering and sorting"
)

# Test 5: View vs direct query (if view exists)
if (view_exists) {
  test5a <- run_query_benchmark(
    "SELECT * FROM latest_data WHERE variable_name = 'traffic_fatality_rate' LIMIT 100",
    "Query using regular view"
  )
  
  test5b <- run_query_benchmark(
    "SELECT c.geoid, c.name, c.state_fips, c.state_name, d.year, d.variable_name, d.value, d.data_quality, d.data_source
     FROM counties c
     JOIN sdoh_data d ON c.geoid = d.geoid
     WHERE (d.geoid, d.variable_name, d.year) IN (
       SELECT geoid, variable_name, MAX(year) 
       FROM sdoh_data 
       GROUP BY geoid, variable_name
     )
     AND d.variable_name = 'traffic_fatality_rate'
     LIMIT 100",
    "Equivalent direct query without view"
  )
  
  speedup_factor <- test5b$median_time / test5a$median_time
  cat(paste("\nView speedup factor:", round(speedup_factor, 2), 
           "x (view is", round(speedup_factor * 100 - 100), "% faster)\n"))
}

# Test 6: Materialized view vs regular view (if both exist)
if (view_exists && materialized_view_exists) {
  test6a <- run_query_benchmark(
    "SELECT * FROM latest_data_materialized WHERE variable_name = 'traffic_fatality_rate' LIMIT 100",
    "Query using materialized view"
  )
  
  test6b <- run_query_benchmark(
    "SELECT * FROM latest_data WHERE variable_name = 'traffic_fatality_rate' LIMIT 100",
    "Same query using regular view"
  )
  
  speedup_factor <- test6b$median_time / test6a$median_time
  cat(paste("\nMaterialized view speedup factor:", round(speedup_factor, 2), 
           "x (materialized view is", round(speedup_factor * 100 - 100), "% faster)\n"))
}

# Test 7: Complex query with multiple joins and aggregations
test7 <- run_query_benchmark(
  "WITH county_data AS (
     SELECT 
       SUBSTRING(geoid, 1, 2) as state_fips,
       variable_name,
       AVG(value) as county_avg
     FROM sdoh_data
     WHERE year = 2020
     GROUP BY SUBSTRING(geoid, 1, 2), variable_name
   )
   SELECT 
     c.state_name,
     cd.variable_name,
     cd.county_avg,
     RANK() OVER (PARTITION BY cd.variable_name ORDER BY cd.county_avg DESC) as state_rank
   FROM county_data cd
   JOIN counties c ON SUBSTRING(c.geoid, 1, 2) = cd.state_fips
   WHERE cd.variable_name IN ('traffic_fatality_rate', 'census_median_household_income')
   GROUP BY c.state_name, cd.variable_name, cd.county_avg
   ORDER BY cd.variable_name, state_rank
   LIMIT 20",
  "Complex query with window functions and aggregations"
)

# Disconnect from the database
dbDisconnect(con)

cat("\n=== DATABASE OPTIMIZATION TESTS COMPLETED ===\n")