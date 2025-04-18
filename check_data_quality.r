#\!/usr/bin/env Rscript

# Data Quality Checking Utility
# This script analyzes a Social Determinants of Health database to report on data quality flags

# Load required packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(duckdb)
})

#' Check Data Quality in SDOH Database
#'
#' This function connects to a SDOH database and analyzes the quality flags
#' to provide a comprehensive report on data quality, including interpolation
#' and extension statistics.
#'
#' @param db_path Path to the DuckDB database
#' @param output_csv Whether to export results to CSV files
#' @param output_dir Directory to save output files
#'
#' @return A list with data quality analysis results
#'
check_data_quality <- function(db_path = "us_county_sdoh_data.duckdb",
                              output_csv = TRUE,
                              output_dir = "output") {
  # Check if database exists
  if (\!file.exists(db_path)) {
    stop("Database file does not exist: ", db_path)
  }
  
  # Create output directory if it doesn't exist
  if (output_csv && \!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Connect to the database
  message("Connecting to database: ", db_path)
  con <- dbConnect(duckdb(), db_path)
  
  # Check if the main table exists
  if (\!dbExistsTable(con, "county_sdoh_data")) {
    dbDisconnect(con)
    stop("Main table 'county_sdoh_data' does not exist in the database")
  }
  
  # Get column names to understand available quality flags
  columns <- dbListFields(con, "county_sdoh_data")
  message("Found ", length(columns), " columns in the database")
  
  # Check for key quality columns
  quality_columns <- c(
    "data_quality", "data_source", "data_vintage", "data_quality_score",
    "interpolation_used", "interpolation_count", "extension_used", "extension_count"
  )
  
  available_quality_columns <- intersect(quality_columns, columns)
  message("Available quality columns: ", paste(available_quality_columns, collapse = ", "))
  
  # Initialize results list
  results <- list()
  
  # 1. Overall data quality summary
  if ("data_quality" %in% available_quality_columns) {
    message("Analyzing overall data quality...")
    quality_summary <- dbGetQuery(con, "
      SELECT 
        data_quality, 
        COUNT(*) as record_count,
        COUNT(DISTINCT GEOID) as county_count,
        COUNT(DISTINCT year) as year_count,
        ROUND(100.0 * COUNT(*) / (SELECT COUNT(*) FROM county_sdoh_data), 2) as percentage
      FROM county_sdoh_data
      GROUP BY data_quality
      ORDER BY record_count DESC
    ")
    
    results$quality_summary <- quality_summary
    
    message("Data quality summary:")
    print(quality_summary)
    
    # Export to CSV if requested
    if (output_csv) {
      write_csv(quality_summary, file.path(output_dir, "data_quality_summary.csv"))
    }
  }
  
  # 2. Quality by year
  message("Analyzing data quality by year...")
  quality_by_year_query <- "
    SELECT 
      year,
      COUNT(*) as total_records,
      COUNT(DISTINCT GEOID) as county_count
  "
  
  # Add conditional counts for each quality type if available
  if ("data_quality" %in% available_quality_columns) {
    quality_by_year_query <- paste0(quality_by_year_query, "
      ,SUM(CASE WHEN data_quality = 'direct' THEN 1 ELSE 0 END) as direct_count
      ,SUM(CASE WHEN data_quality = 'estimate' THEN 1 ELSE 0 END) as estimate_count
      ,SUM(CASE WHEN data_quality = 'harmonized' THEN 1 ELSE 0 END) as harmonized_count
      ,SUM(CASE WHEN data_quality = 'interpolated' THEN 1 ELSE 0 END) as interpolated_count
      ,SUM(CASE WHEN data_quality = 'extended' THEN 1 ELSE 0 END) as extended_count
    ")
  }
  
  # Add interpolation and extension flags if available
  if ("interpolation_used" %in% available_quality_columns) {
    quality_by_year_query <- paste0(quality_by_year_query, "
      ,SUM(CASE WHEN interpolation_used = TRUE THEN 1 ELSE 0 END) as records_with_interpolation
      ,ROUND(100.0 * SUM(CASE WHEN interpolation_used = TRUE THEN 1 ELSE 0 END) / COUNT(*), 2) as pct_with_interpolation
    ")
  }
  
  if ("extension_used" %in% available_quality_columns) {
    quality_by_year_query <- paste0(quality_by_year_query, "
      ,SUM(CASE WHEN extension_used = TRUE THEN 1 ELSE 0 END) as records_with_extension
      ,ROUND(100.0 * SUM(CASE WHEN extension_used = TRUE THEN 1 ELSE 0 END) / COUNT(*), 2) as pct_with_extension
    ")
  }
  
  # Complete the query
  quality_by_year_query <- paste0(quality_by_year_query, "
    FROM county_sdoh_data
    GROUP BY year
    ORDER BY year
  ")
  
  # Execute the query
  quality_by_year <- dbGetQuery(con, quality_by_year_query)
  results$quality_by_year <- quality_by_year
  
  message("Data quality by year (first few rows):")
  print(head(quality_by_year))
  
  # Export to CSV if requested
  if (output_csv) {
    write_csv(quality_by_year, file.path(output_dir, "data_quality_by_year.csv"))
  }
  
  # 3. Variable-specific quality analysis
  message("Analyzing variable-specific data quality...")
  
  # Get variable names (excluding metadata columns)
  metadata_columns <- c(
    "GEOID", "NAME", "year", "source", "data_quality", "data_source", "data_vintage",
    "data_quality_score", "interpolation_used", "interpolation_count", "extension_used", 
    "extension_count"
  )
  
  variable_columns <- setdiff(columns, metadata_columns)
  message("Found ", length(variable_columns), " data variables")
  
  # Check for variable-specific interpolation flags (pattern: variable_name_interpolated)
  interpolation_flags <- columns[grep("_interpolated$", columns)]
  
  if (length(interpolation_flags) > 0) {
    # Extract base variable names
    base_variables <- sub("_interpolated$", "", interpolation_flags)
    message("Found ", length(interpolation_flags), " variable-specific interpolation flags")
    
    # Create query to analyze variable-specific interpolation
    var_query_parts <- c()
    
    for (var in base_variables) {
      # Check if both base variable and interpolation flag exist
      if (var %in% columns && paste0(var, "_interpolated") %in% columns) {
        var_query_parts <- c(var_query_parts, sprintf("
          SELECT 
            '%s' as variable_name,
            COUNT(*) as total_records,
            SUM(CASE WHEN \"%s\" IS NOT NULL THEN 1 ELSE 0 END) as non_null_records,
            SUM(CASE WHEN \"%s_interpolated\" = TRUE THEN 1 ELSE 0 END) as interpolated_records,
            ROUND(100.0 * SUM(CASE WHEN \"%s_interpolated\" = TRUE THEN 1 ELSE 0 END) / 
              NULLIF(SUM(CASE WHEN \"%s\" IS NOT NULL THEN 1 ELSE 0 END), 0), 2) as pct_interpolated
          FROM county_sdoh_data
        ", var, var, var, var, var))
      }
    }
    
    # Combine all variable queries with UNION ALL
    if (length(var_query_parts) > 0) {
      var_quality_query <- paste(var_query_parts, collapse = " UNION ALL ")
      var_quality_query <- paste0(var_quality_query, " ORDER BY interpolated_records DESC")
      
      # Execute the query
      variable_interpolation <- dbGetQuery(con, var_quality_query)
      results$variable_interpolation <- variable_interpolation
      
      message("Variable-specific interpolation (top 10):")
      print(head(variable_interpolation, 10))
      
      # Export to CSV if requested
      if (output_csv) {
        write_csv(variable_interpolation, file.path(output_dir, "variable_interpolation_summary.csv"))
      }
    }
  } else {
    message("No variable-specific interpolation flags found")
  }
  
  # Check for variable-specific extension flags (pattern: variable_name_extended)
  extension_flags <- columns[grep("_extended$", columns)]
  
  if (length(extension_flags) > 0) {
    # Extract base variable names
    base_variables <- sub("_extended$", "", extension_flags)
    message("Found ", length(extension_flags), " variable-specific extension flags")
    
    # Create query to analyze variable-specific extension
    var_query_parts <- c()
    
    for (var in base_variables) {
      # Check if both base variable and extension flag exist
      if (var %in% columns && paste0(var, "_extended") %in% columns) {
        var_query_parts <- c(var_query_parts, sprintf("
          SELECT 
            '%s' as variable_name,
            COUNT(*) as total_records,
            SUM(CASE WHEN \"%s\" IS NOT NULL THEN 1 ELSE 0 END) as non_null_records,
            SUM(CASE WHEN \"%s_extended\" = TRUE THEN 1 ELSE 0 END) as extended_records,
            ROUND(100.0 * SUM(CASE WHEN \"%s_extended\" = TRUE THEN 1 ELSE 0 END) / 
              NULLIF(SUM(CASE WHEN \"%s\" IS NOT NULL THEN 1 ELSE 0 END), 0), 2) as pct_extended
          FROM county_sdoh_data
        ", var, var, var, var, var))
      }
    }
    
    # Combine all variable queries with UNION ALL
    if (length(var_query_parts) > 0) {
      var_quality_query <- paste(var_query_parts, collapse = " UNION ALL ")
      var_quality_query <- paste0(var_quality_query, " ORDER BY extended_records DESC")
      
      # Execute the query
      variable_extension <- dbGetQuery(con, var_quality_query)
      results$variable_extension <- variable_extension
      
      message("Variable-specific extension (top 10):")
      print(head(variable_extension, 10))
      
      # Export to CSV if requested
      if (output_csv) {
        write_csv(variable_extension, file.path(output_dir, "variable_extension_summary.csv"))
      }
    }
  } else {
    message("No variable-specific extension flags found")
  }
  
  # 4. Data source analysis
  if ("data_source" %in% available_quality_columns) {
    message("Analyzing data sources...")
    source_summary <- dbGetQuery(con, "
      SELECT 
        data_source, 
        COUNT(*) as record_count,
        COUNT(DISTINCT GEOID) as county_count,
        COUNT(DISTINCT year) as year_count,
        MIN(year) as min_year,
        MAX(year) as max_year,
        ROUND(100.0 * COUNT(*) / (SELECT COUNT(*) FROM county_sdoh_data), 2) as percentage
      FROM county_sdoh_data
      GROUP BY data_source
      ORDER BY record_count DESC
    ")
    
    results$source_summary <- source_summary
    
    message("Data source summary:")
    print(source_summary)
    
    # Export to CSV if requested
    if (output_csv) {
      write_csv(source_summary, file.path(output_dir, "data_source_summary.csv"))
    }
    
    # Cross-tabulate data source and quality
    if ("data_quality" %in% available_quality_columns) {
      source_quality_cross <- dbGetQuery(con, "
        SELECT 
          data_source,
          data_quality,
          COUNT(*) as record_count,
          ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER (PARTITION BY data_source), 2) as source_percentage
        FROM county_sdoh_data
        GROUP BY data_source, data_quality
        ORDER BY data_source, record_count DESC
      ")
      
      results$source_quality_cross <- source_quality_cross
      
      message("Data source by quality (first few rows):")
      print(head(source_quality_cross))
      
      # Export to CSV if requested
      if (output_csv) {
        write_csv(source_quality_cross, file.path(output_dir, "data_source_quality_cross.csv"))
      }
    }
  }
  
  # Create a comprehensive report file
  if (output_csv) {
    report_file <- file.path(output_dir, "data_quality_report.md")
    message("Creating comprehensive report at ", report_file)
    
    report_content <- c(
      "# Social Determinants of Health Data Quality Report",
      "",
      paste("Generated on:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      "",
      "## Overall Data Quality",
      ""
    )
    
    # Add quality summary
    if (\!is.null(results$quality_summary) && nrow(results$quality_summary) > 0) {
      report_content <- c(report_content,
        "### Quality Type Distribution",
        "",
        "| Quality Type | Record Count | County Count | Year Count | Percentage |",
        "| ------------ | -----------: | -----------: | ---------: | ---------: |"
      )
      
      for (i in 1:nrow(results$quality_summary)) {
        row <- results$quality_summary[i, ]
        report_content <- c(report_content,
          paste("|", row$data_quality, "|", 
                format(row$record_count, big.mark = ","), "|",
                format(row$county_count, big.mark = ","), "|",
                row$year_count, "|",
                paste0(row$percentage, "%"), "|")
        )
      }
      
      report_content <- c(report_content, "")
    }
    
    # Add quality by year
    if (\!is.null(results$quality_by_year) && nrow(results$quality_by_year) > 0) {
      report_content <- c(report_content,
        "### Quality by Year",
        "",
        "The table below shows the distribution of data quality by year. Only first and last 3 years are shown.",
        ""
      )
      
      # Extract column names
      year_columns <- names(results$quality_by_year)
      
      # Create markdown table header
      header_row <- paste("|", paste(year_columns, collapse = " | "), "|")
      divider_row <- paste("|", paste(rep("---:", length(year_columns)), collapse = " | "), "|")
      
      report_content <- c(report_content, header_row, divider_row)
      
      # Add first 3 years
      first_years <- head(results$quality_by_year, 3)
      for (i in 1:nrow(first_years)) {
        row_values <- as.character(unlist(first_years[i, ]))
        row_values <- ifelse(is.na(row_values), "N/A", row_values)
        row_content <- paste("|", paste(row_values, collapse = " | "), "|")
        report_content <- c(report_content, row_content)
      }
      
      # Add indicator for skipped rows if more than 6 years
      if (nrow(results$quality_by_year) > 6) {
        report_content <- c(report_content, "| ... | ... | ... | ... | ... | ... |")
      }
      
      # Add last 3 years
      last_years <- tail(results$quality_by_year, 3)
      for (i in 1:nrow(last_years)) {
        row_values <- as.character(unlist(last_years[i, ]))
        row_values <- ifelse(is.na(row_values), "N/A", row_values)
        row_content <- paste("|", paste(row_values, collapse = " | "), "|")
        report_content <- c(report_content, row_content)
      }
      
      report_content <- c(report_content, "")
    }
    
    # Add variable-specific interpolation
    if (\!is.null(results$variable_interpolation) && nrow(results$variable_interpolation) > 0) {
      report_content <- c(report_content,
        "## Variable-Specific Interpolation",
        "",
        "The table below shows the variables with the highest percentage of interpolated values.",
        "",
        "| Variable | Total Records | Non-Null Records | Interpolated Records | % Interpolated |",
        "| -------- | ------------: | ---------------: | ------------------: | -------------: |"
      )
      
      # Show top 10 variables by percentage interpolated
      top_vars <- head(results$variable_interpolation[order(-results$variable_interpolation$pct_interpolated), ], 10)
      for (i in 1:nrow(top_vars)) {
        row <- top_vars[i, ]
        report_content <- c(report_content,
          paste("|", row$variable_name, "|", 
                format(row$total_records, big.mark = ","), "|",
                format(row$non_null_records, big.mark = ","), "|",
                format(row$interpolated_records, big.mark = ","), "|",
                paste0(row$pct_interpolated, "%"), "|")
        )
      }
      
      report_content <- c(report_content, "")
    }
    
    # Add variable-specific extension
    if (\!is.null(results$variable_extension) && nrow(results$variable_extension) > 0) {
      report_content <- c(report_content,
        "## Variable-Specific Extension",
        "",
        "The table below shows the variables with the highest percentage of extended values.",
        "",
        "| Variable | Total Records | Non-Null Records | Extended Records | % Extended |",
        "| -------- | ------------: | ---------------: | ---------------: | ---------: |"
      )
      
      # Show top 10 variables by percentage extended
      top_vars <- head(results$variable_extension[order(-results$variable_extension$pct_extended), ], 10)
      for (i in 1:nrow(top_vars)) {
        row <- top_vars[i, ]
        report_content <- c(report_content,
          paste("|", row$variable_name, "|", 
                format(row$total_records, big.mark = ","), "|",
                format(row$non_null_records, big.mark = ","), "|",
                format(row$extended_records, big.mark = ","), "|",
                paste0(row$pct_extended, "%"), "|")
        )
      }
      
      report_content <- c(report_content, "")
    }
    
    # Add data source information
    if (\!is.null(results$source_summary) && nrow(results$source_summary) > 0) {
      report_content <- c(report_content,
        "## Data Sources",
        "",
        "### Source Distribution",
        "",
        "| Data Source | Record Count | County Count | Year Count | Min Year | Max Year | Percentage |",
        "| ----------- | -----------: | -----------: | ---------: | -------: | -------: | ---------: |"
      )
      
      for (i in 1:nrow(results$source_summary)) {
        row <- results$source_summary[i, ]
        report_content <- c(report_content,
          paste("|", row$data_source, "|", 
                format(row$record_count, big.mark = ","), "|",
                format(row$county_count, big.mark = ","), "|",
                row$year_count, "|",
                row$min_year, "|",
                row$max_year, "|",
                paste0(row$percentage, "%"), "|")
        )
      }
      
      report_content <- c(report_content, "")
    }
    
    # Add data source by quality
    if (\!is.null(results$source_quality_cross) && nrow(results$source_quality_cross) > 0) {
      report_content <- c(report_content,
        "### Source by Quality Type",
        "",
        "| Data Source | Quality Type | Record Count | Source Percentage |",
        "| ----------- | ------------ | -----------: | ----------------: |"
      )
      
      # Get unique sources to organize the report
      unique_sources <- unique(results$source_quality_cross$data_source)
      
      for (source in unique_sources) {
        source_rows <- results$source_quality_cross[results$source_quality_cross$data_source == source, ]
        
        for (i in 1:nrow(source_rows)) {
          row <- source_rows[i, ]
          report_content <- c(report_content,
            paste("|", row$data_source, "|", 
                  row$data_quality, "|",
                  format(row$record_count, big.mark = ","), "|",
                  paste0(row$source_percentage, "%"), "|")
          )
        }
      }
      
      report_content <- c(report_content, "")
    }
    
    # Add conclusion and recommendations
    report_content <- c(report_content,
      "## Conclusion and Recommendations",
      "",
      "This report provides an overview of the data quality in the Social Determinants of Health dataset.",
      "Based on the analysis, the following recommendations can be made:",
      "",
      "1. **Direct and Estimated Data**: Prefer direct and estimated data for critical analyses when available.",
      "2. **Interpolated Data**: Use interpolated data with caution, especially for variables with high interpolation rates.",
      "3. **Extended Data**: Be particularly cautious with extended data, as these represent extrapolations beyond known data points.",
      "4. **Time Series Analysis**: When analyzing trends over time, be aware of the quality type for each data point.",
      "5. **Data Filtering**: Consider filtering data based on quality needs using the quality flags provided.",
      "",
      "## How to Filter Data by Quality",
      "",
      "The following SQL examples show how to filter data based on quality requirements:",
      "",
      "```sql",
      "-- Get only direct data",
      "SELECT * FROM county_sdoh_data WHERE data_quality = 'direct';",
      "",
      "-- Get direct or estimated data (exclude interpolated and extended)",
      "SELECT * FROM county_sdoh_data WHERE data_quality IN ('direct', 'estimate', 'harmonized');",
      "",
      "-- Get only records with no interpolation",
      "SELECT * FROM county_sdoh_data WHERE interpolation_used = FALSE;",
      "",
      "-- Get only records with no extension",
      "SELECT * FROM county_sdoh_data WHERE extension_used = FALSE;",
      "```"
    )
    
    # Write the report file
    writeLines(report_content, report_file)
  }
  
  # Close database connection
  dbDisconnect(con)
  message("Database connection closed")
  
  # Return the results
  return(results)
}

# Execute as script if run directly
if (\!interactive()) {
  # Process command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  
  # Default values
  db_path <- "us_county_sdoh_data.duckdb"
  output_csv <- TRUE
  output_dir <- "output"
  
  # Parse arguments
  i <- 1
  while (i <= length(args)) {
    if (args[i] == "--db" && i < length(args)) {
      db_path <- args[i + 1]
      i <- i + 2
    } else if (args[i] == "--no-csv") {
      output_csv <- FALSE
      i <- i + 1
    } else if (args[i] == "--output-dir" && i < length(args)) {
      output_dir <- args[i + 1]
      i <- i + 2
    } else if (args[i] == "--help" || args[i] == "-h") {
      cat("Usage: Rscript check_data_quality.r [options]\n")
      cat("\nOptions:\n")
      cat("  --db PATH           Path to DuckDB database (default: us_county_sdoh_data.duckdb)\n")
      cat("  --no-csv            Do not export results to CSV files\n")
      cat("  --output-dir DIR    Directory to save output files (default: output)\n")
      cat("  --help, -h          Show this help message\n")
      quit(save = "no", status = 0)
    } else {
      i <- i + 1
    }
  }
  
  # Check database path
  if (\!file.exists(db_path)) {
    cat("Database file does not exist:", db_path, "\n")
    cat("Please provide a valid database path using --db\n")
    quit(save = "no", status = 1)
  }
  
  # Run the check
  check_data_quality(db_path, output_csv, output_dir)
}
EOL < /dev/null