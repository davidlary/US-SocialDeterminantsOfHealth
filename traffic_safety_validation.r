#!/usr/bin/env Rscript

# Traffic Safety Data Validation Module
# This module provides validation hooks and tools for traffic safety data

# Load required packages
required_packages <- c(
  "tidyverse",
  "assertthat",
  "jsonlite",
  "openxlsx",
  "glue"
)

# Load packages with error handling
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    if (pkg == "openxlsx") {
      message("Package openxlsx is not installed. Some validation features may not be available.")
    } else {
      message(paste("Package", pkg, "is not installed. Some validation features may not be available."))
    }
  }
}

#' Traffic Safety Data Validator Class
#' 
#' A comprehensive framework for validating traffic safety data
#' with customizable rules, reporting, and auto-fixing capabilities
#' @export
TrafficDataValidator <- R6::R6Class(
  "TrafficDataValidator",
  
  public = list(
    #' @field data The traffic safety data to validate
    data = NULL,
    
    #' @field rules List of validation rules to apply
    rules = list(),
    
    #' @field validation_results Results of validation checks
    validation_results = list(),
    
    #' @field auto_fix Whether to automatically fix issues when possible
    auto_fix = FALSE,
    
    #' @description Create a new TrafficDataValidator
    #' @param data Traffic safety data frame
    #' @param auto_fix Whether to automatically fix issues when possible
    initialize = function(data, auto_fix = FALSE) {
      self$data <- data
      self$auto_fix <- auto_fix
      self$validation_results <- list()
      self$init_standard_rules()
    },
    
    #' @description Initialize standard validation rules
    init_standard_rules = function() {
      # General data structure rules
      self$add_rule(
        name = "required_columns",
        description = "Check for required columns",
        check_function = function(data) {
          required_cols <- c("fips", "year", "traffic_fatality_count", "traffic_fatality_rate_per_100k")
          missing_cols <- setdiff(required_cols, names(data))
          
          if (length(missing_cols) > 0) {
            return(list(
              passed = FALSE,
              message = paste("Missing required columns:", paste(missing_cols, collapse = ", ")),
              locations = NULL,
              fixable = FALSE
            ))
          }
          
          return(list(
            passed = TRUE,
            message = "All required columns present",
            locations = NULL,
            fixable = FALSE
          ))
        },
        fix_function = NULL  # Can't fix missing columns automatically
      )
      
      # Data type rules
      self$add_rule(
        name = "data_types",
        description = "Check column data types",
        check_function = function(data) {
          type_issues <- list()
          
          # Check fips is character
          if ("fips" %in% names(data) && !is.character(data$fips)) {
            type_issues$fips <- "fips should be character type"
          }
          
          # Check year is numeric
          if ("year" %in% names(data) && !is.numeric(data$year)) {
            type_issues$year <- "year should be numeric type"
          }
          
          # Check traffic metrics are numeric
          rate_cols <- grep("rate|count", names(data), value = TRUE)
          for (col in rate_cols) {
            if (!is.numeric(data[[col]])) {
              type_issues[[col]] <- paste(col, "should be numeric type")
            }
          }
          
          if (length(type_issues) > 0) {
            return(list(
              passed = FALSE,
              message = paste("Data type issues found in", length(type_issues), "columns"),
              locations = type_issues,
              fixable = TRUE
            ))
          }
          
          return(list(
            passed = TRUE,
            message = "All columns have correct data types",
            locations = NULL,
            fixable = FALSE
          ))
        },
        fix_function = function(data) {
          # Convert fips to character
          if ("fips" %in% names(data) && !is.character(data$fips)) {
            data$fips <- as.character(data$fips)
            # Ensure proper formatting with leading zeros
            data$fips <- sprintf("%05d", as.numeric(data$fips))
          }
          
          # Convert year to numeric
          if ("year" %in% names(data) && !is.numeric(data$year)) {
            data$year <- as.numeric(data$year)
          }
          
          # Convert rate columns to numeric
          rate_cols <- grep("rate|count", names(data), value = TRUE)
          for (col in rate_cols) {
            if (!is.numeric(data[[col]])) {
              data[[col]] <- as.numeric(data[[col]])
            }
          }
          
          return(data)
        }
      )
      
      # FIPS code format
      self$add_rule(
        name = "fips_format",
        description = "Check FIPS code format (5 digits)",
        check_function = function(data) {
          if (!"fips" %in% names(data)) {
            return(list(
              passed = FALSE,
              message = "FIPS column not found",
              locations = NULL,
              fixable = FALSE
            ))
          }
          
          invalid_fips <- which(!grepl("^[0-9]{5}$", data$fips))
          
          if (length(invalid_fips) > 0) {
            return(list(
              passed = FALSE,
              message = paste(length(invalid_fips), "records with invalid FIPS format"),
              locations = invalid_fips,
              fixable = TRUE
            ))
          }
          
          return(list(
            passed = TRUE,
            message = "All FIPS codes have correct format",
            locations = NULL,
            fixable = FALSE
          ))
        },
        fix_function = function(data) {
          # Convert all FIPS to standard 5-digit format
          data$fips <- sprintf("%05d", as.numeric(data$fips))
          return(data)
        }
      )
      
      # Year range check
      self$add_rule(
        name = "year_range",
        description = "Check year values are in a reasonable range",
        check_function = function(data) {
          if (!"year" %in% names(data)) {
            return(list(
              passed = FALSE,
              message = "Year column not found",
              locations = NULL,
              fixable = FALSE
            ))
          }
          
          current_year <- as.numeric(format(Sys.Date(), "%Y"))
          invalid_years <- which(data$year < 1970 | data$year > current_year)
          
          if (length(invalid_years) > 0) {
            return(list(
              passed = FALSE,
              message = paste(length(invalid_years), "records with invalid year (before 1970 or in the future)"),
              locations = invalid_years,
              fixable = FALSE
            ))
          }
          
          return(list(
            passed = TRUE,
            message = "All years are within valid range",
            locations = NULL,
            fixable = FALSE
          ))
        },
        fix_function = NULL  # No auto-fix for invalid years
      )
      
      # Negative values check
      self$add_rule(
        name = "negative_values",
        description = "Check for negative values in count/rate fields",
        check_function = function(data) {
          # Find all count/rate columns
          metric_cols <- grep("count$|rate|per_100k", names(data), value = TRUE)
          
          negative_locations <- list()
          for (col in metric_cols) {
            # Skip non-numeric columns
            if (!is.numeric(data[[col]])) next
            
            negatives <- which(data[[col]] < 0)
            if (length(negatives) > 0) {
              negative_locations[[col]] <- negatives
            }
          }
          
          if (length(negative_locations) > 0) {
            total_negative_values <- sum(sapply(negative_locations, length))
            return(list(
              passed = FALSE,
              message = paste("Found", total_negative_values, "negative values across", 
                              length(negative_locations), "columns"),
              locations = negative_locations,
              fixable = TRUE
            ))
          }
          
          return(list(
            passed = TRUE,
            message = "No negative values found in metrics",
            locations = NULL,
            fixable = FALSE
          ))
        },
        fix_function = function(data) {
          # Find all count/rate columns
          metric_cols <- grep("count$|rate|per_100k", names(data), value = TRUE)
          
          # Set negative values to NA
          for (col in metric_cols) {
            # Skip non-numeric columns
            if (!is.numeric(data[[col]])) next
            
            data[[col]] <- ifelse(data[[col]] < 0, NA, data[[col]])
          }
          
          return(data)
        }
      )
      
      # Extreme outlier check
      self$add_rule(
        name = "extreme_outliers",
        description = "Check for extreme outliers in metrics",
        check_function = function(data) {
          # Find all rate columns
          rate_cols <- grep("rate|per_100k", names(data), value = TRUE)
          
          outlier_locations <- list()
          for (col in rate_cols) {
            # Skip non-numeric columns
            if (!is.numeric(data[[col]])) next
            
            # Calculate outlier threshold (3x IQR)
            q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
            q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
            iqr <- q3 - q1
            upper_bound <- q3 + 3 * iqr
            
            # Identify extreme outliers
            outliers <- which(data[[col]] > upper_bound)
            if (length(outliers) > 0) {
              outlier_locations[[col]] <- outliers
            }
          }
          
          if (length(outlier_locations) > 0) {
            total_outliers <- sum(sapply(outlier_locations, length))
            return(list(
              passed = FALSE,
              message = paste("Found", total_outliers, "extreme outliers across", 
                               length(outlier_locations), "columns"),
              locations = outlier_locations,
              fixable = FALSE  # We flag but don't auto-fix outliers
            ))
          }
          
          return(list(
            passed = TRUE,
            message = "No extreme outliers found",
            locations = NULL,
            fixable = FALSE
          ))
        },
        fix_function = NULL  # No auto-fix for outliers, requires manual review
      )
      
      # Data quality flag consistency
      self$add_rule(
        name = "data_quality_flags",
        description = "Check consistency of data quality flags",
        check_function = function(data) {
          # Find all data quality flag columns
          quality_cols <- grep("_data_quality$", names(data), value = TRUE)
          
          # If no quality columns, skip this check
          if (length(quality_cols) == 0) {
            return(list(
              passed = TRUE,
              message = "No data quality flag columns found",
              locations = NULL,
              fixable = FALSE
            ))
          }
          
          # Valid quality flags
          valid_flags <- c("direct", "interpolated", "extrapolated", "simulated", "calculated", "imputed")
          
          invalid_locations <- list()
          for (col in quality_cols) {
            # For character flags, check if they're in the valid set
            if (is.character(data[[col]])) {
              invalids <- which(!is.na(data[[col]]) & !data[[col]] %in% valid_flags)
              if (length(invalids) > 0) {
                invalid_locations[[col]] <- invalids
              }
            }
          }
          
          if (length(invalid_locations) > 0) {
            total_invalids <- sum(sapply(invalid_locations, length))
            return(list(
              passed = FALSE,
              message = paste("Found", total_invalids, "invalid data quality flags across", 
                              length(invalid_locations), "columns"),
              locations = invalid_locations,
              fixable = TRUE
            ))
          }
          
          return(list(
            passed = TRUE,
            message = "All data quality flags are consistent",
            locations = NULL,
            fixable = FALSE
          ))
        },
        fix_function = function(data) {
          # Find all data quality flag columns
          quality_cols <- grep("_data_quality$", names(data), value = TRUE)
          
          # Valid quality flags
          valid_flags <- c("direct", "interpolated", "extrapolated", "simulated", "calculated", "imputed")
          
          # Fix invalid flags
          for (col in quality_cols) {
            # For character flags, replace invalid values with NA
            if (is.character(data[[col]])) {
              data[[col]] <- ifelse(!is.na(data[[col]]) & !data[[col]] %in% valid_flags, 
                                    NA, data[[col]])
            }
          }
          
          return(data)
        }
      )
      
      # Count-rate consistency 
      self$add_rule(
        name = "count_rate_consistency",
        description = "Check consistency between counts and rates",
        check_function = function(data) {
          # Find paired count and rate columns
          count_cols <- grep("count$", names(data), value = TRUE)
          consistency_issues <- list()
          
          for (count_col in count_cols) {
            # Look for corresponding rate column
            base_name <- sub("_count$", "", count_col)
            rate_col <- paste0(base_name, "_rate_per_100k")
            
            # Skip if corresponding rate column doesn't exist
            if (!rate_col %in% names(data)) next
            
            # Check if both are numeric
            if (!is.numeric(data[[count_col]]) || !is.numeric(data[[rate_col]])) next
            
            # Check if population column exists
            if ("population" %in% names(data) && is.numeric(data$population)) {
              # Calculate expected rates from counts and compare
              inconsistent_rows <- which(
                !is.na(data[[count_col]]) & !is.na(data[[rate_col]]) & !is.na(data$population) &
                data$population > 0 &
                abs((data[[count_col]] / data$population * 100000) - data[[rate_col]]) > 0.1
              )
              
              if (length(inconsistent_rows) > 0) {
                consistency_issues[[paste(count_col, "vs", rate_col)]] <- inconsistent_rows
              }
            }
          }
          
          if (length(consistency_issues) > 0) {
            total_issues <- sum(sapply(consistency_issues, length))
            return(list(
              passed = FALSE,
              message = paste("Found", total_issues, "count-rate inconsistencies"),
              locations = consistency_issues,
              fixable = TRUE
            ))
          }
          
          return(list(
            passed = TRUE,
            message = "Counts and rates are consistent",
            locations = NULL,
            fixable = FALSE
          ))
        },
        fix_function = function(data) {
          # Find paired count and rate columns
          count_cols <- grep("count$", names(data), value = TRUE)
          
          for (count_col in count_cols) {
            # Look for corresponding rate column
            base_name <- sub("_count$", "", count_col)
            rate_col <- paste0(base_name, "_rate_per_100k")
            
            # Skip if corresponding rate column doesn't exist
            if (!rate_col %in% names(data)) next
            
            # Check if both are numeric
            if (!is.numeric(data[[count_col]]) || !is.numeric(data[[rate_col]])) next
            
            # Check if population column exists
            if ("population" %in% names(data) && is.numeric(data$population)) {
              # Recalculate rates from counts where population exists
              data[[rate_col]] <- ifelse(
                !is.na(data[[count_col]]) & !is.na(data$population) & data$population > 0,
                data[[count_col]] / data$population * 100000,
                data[[rate_col]]
              )
              
              # Update corresponding data quality flag if it exists
              quality_col <- paste0(rate_col, "_data_quality")
              if (quality_col %in% names(data)) {
                data[[quality_col]] <- ifelse(
                  !is.na(data[[count_col]]) & !is.na(data$population) & data$population > 0,
                  "calculated",
                  data[[quality_col]]
                )
              }
            }
          }
          
          return(data)
        }
      )
    },
    
    #' @description Add a custom validation rule
    #' @param name Rule name
    #' @param description Rule description
    #' @param check_function Function that checks data and returns result list
    #' @param fix_function Function that fixes issues (or NULL if not fixable)
    add_rule = function(name, description, check_function, fix_function = NULL) {
      self$rules[[name]] <- list(
        name = name,
        description = description,
        check = check_function,
        fix = fix_function
      )
    },
    
    #' @description Run a specific validation rule
    #' @param rule_name Name of the rule to run
    #' @return Validation result for the specified rule
    run_rule = function(rule_name) {
      if (!rule_name %in% names(self$rules)) {
        stop(paste("Rule", rule_name, "not found"))
      }
      
      rule <- self$rules[[rule_name]]
      result <- rule$check(self$data)
      
      # Store validation result
      self$validation_results[[rule_name]] <- result
      
      # Apply fix if requested and available
      if (self$auto_fix && !result$passed && result$fixable && !is.null(rule$fix)) {
        self$data <- rule$fix(self$data)
        
        # Re-validate after fix
        result_after_fix <- rule$check(self$data)
        result$fixed <- !identical(result, result_after_fix)
        result$result_after_fix <- result_after_fix
      } else {
        result$fixed <- FALSE
      }
      
      return(result)
    },
    
    #' @description Run all validation rules
    #' @return List of validation results for all rules
    validate = function() {
      results <- list()
      
      for (rule_name in names(self$rules)) {
        results[[rule_name]] <- self$run_rule(rule_name)
      }
      
      return(results)
    },
    
    #' @description Get the validated data
    #' @return Data frame with fixes applied (if auto_fix=TRUE)
    get_data = function() {
      return(self$data)
    },
    
    #' @description Generate a validation report
    #' @param format Report format (markdown, json, or html)
    #' @param file_path Optional file path to save the report
    #' @return Report content
    generate_report = function(format = "markdown", file_path = NULL) {
      # Ensure we have validation results
      if (length(self$validation_results) == 0) {
        self$validate()
      }
      
      # Count issues
      total_issues <- 0
      fixable_issues <- 0
      fixed_issues <- 0
      
      for (result in self$validation_results) {
        if (!result$passed) {
          issue_count <- if (is.list(result$locations)) {
            sum(sapply(result$locations, length))
          } else if (!is.null(result$locations)) {
            length(result$locations)
          } else {
            1  # At least one issue if the test failed
          }
          
          total_issues <- total_issues + issue_count
          
          if (result$fixable) {
            fixable_issues <- fixable_issues + issue_count
          }
          
          if (result$fixed) {
            fixed_issues <- fixed_issues + issue_count
          }
        }
      }
      
      # Create report based on format
      if (format == "markdown") {
        report <- self$create_markdown_report(total_issues, fixable_issues, fixed_issues)
      } else if (format == "json") {
        report <- self$create_json_report(total_issues, fixable_issues, fixed_issues)
      } else if (format == "html") {
        report <- self$create_html_report(total_issues, fixable_issues, fixed_issues)
      } else {
        stop(paste("Unsupported report format:", format))
      }
      
      # Save to file if requested
      if (!is.null(file_path)) {
        writeLines(report, file_path)
      }
      
      return(report)
    },
    
    #' @description Create a markdown validation report
    #' @param total_issues Total number of issues found
    #' @param fixable_issues Number of fixable issues
    #' @param fixed_issues Number of issues fixed
    #' @return Markdown report as string
    create_markdown_report = function(total_issues, fixable_issues, fixed_issues) {
      # Start building the report
      report_lines <- c(
        "# Traffic Safety Data Validation Report",
        "",
        paste("Report generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        "",
        "## Summary",
        "",
        paste("Total records:", nrow(self$data)),
        paste("Total issues found:", total_issues),
        paste("Automatically fixable issues:", fixable_issues),
        if (self$auto_fix) paste("Issues fixed:", fixed_issues) else "Auto-fix disabled",
        "",
        "## Validation Results",
        ""
      )
      
      # Add results for each rule
      for (rule_name in names(self$validation_results)) {
        result <- self$validation_results[[rule_name]]
        rule <- self$rules[[rule_name]]
        
        status <- if (result$passed) "✅ PASSED" else "❌ FAILED"
        if (!result$passed && result$fixed) {
          status <- "🔧 FIXED"
        }
        
        report_lines <- c(
          report_lines,
          paste("###", rule_name, "-", status),
          "",
          paste("Description:", rule$description),
          paste("Result:", result$message),
          ""
        )
        
        # Add details about issues if any
        if (!result$passed && !is.null(result$locations)) {
          if (is.list(result$locations)) {
            for (location_name in names(result$locations)) {
              locations <- result$locations[[location_name]]
              if (length(locations) > 10) {
                # Too many issues to list, show summary
                report_lines <- c(
                  report_lines,
                  paste("Issues in", location_name, ":", length(locations), "records"),
                  paste("First few affected rows:", paste(head(locations, 5), collapse = ", "), "...")
                )
              } else {
                # List all issues
                report_lines <- c(
                  report_lines,
                  paste("Issues in", location_name, ":", paste(locations, collapse = ", "))
                )
              }
            }
          } else {
            if (length(result$locations) > 10) {
              # Too many issues to list, show summary
              report_lines <- c(
                report_lines,
                paste("Issues found in", length(result$locations), "records"),
                paste("First few affected rows:", paste(head(result$locations, 5), collapse = ", "), "...")
              )
            } else {
              # List all issues
              report_lines <- c(
                report_lines,
                paste("Affected rows:", paste(result$locations, collapse = ", "))
              )
            }
          }
          report_lines <- c(report_lines, "")
        }
        
        # Add fix status
        if (!result$passed) {
          fix_status <- if (result$fixable) {
            if (self$auto_fix) {
              if (result$fixed) "Issues were automatically fixed" else "Failed to fix issues"
            } else {
              "Issues can be fixed automatically (auto_fix=TRUE)"
            }
          } else {
            "Issues require manual review"
          }
          
          report_lines <- c(
            report_lines,
            paste("Fix status:", fix_status),
            ""
          )
        }
      }
      
      # Add recommendations
      report_lines <- c(
        report_lines,
        "## Recommendations",
        ""
      )
      
      if (total_issues == 0) {
        report_lines <- c(
          report_lines,
          "✅ No data quality issues found. Data is valid for further analysis."
        )
      } else {
        if (fixed_issues == total_issues) {
          report_lines <- c(
            report_lines,
            "✅ All issues have been automatically fixed. Data is now valid for further analysis."
          )
        } else {
          unfixed_issues <- total_issues - fixed_issues
          report_lines <- c(
            report_lines,
            paste("❗", unfixed_issues, "issues still need to be addressed:"),
            ""
          )
          
          # List unfixed issues by category
          for (rule_name in names(self$validation_results)) {
            result <- self$validation_results[[rule_name]]
            if (!result$passed && !result$fixed) {
              report_lines <- c(
                report_lines,
                paste("-", rule_name, ":", result$message)
              )
            }
          }
        }
      }
      
      # Return the complete report
      return(paste(report_lines, collapse = "\n"))
    },
    
    #' @description Create a JSON validation report
    #' @param total_issues Total number of issues found
    #' @param fixable_issues Number of fixable issues
    #' @param fixed_issues Number of issues fixed
    #' @return JSON report as string
    create_json_report = function(total_issues, fixable_issues, fixed_issues) {
      # Create report structure
      report <- list(
        metadata = list(
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          record_count = nrow(self$data),
          total_issues = total_issues,
          fixable_issues = fixable_issues,
          fixed_issues = fixed_issues,
          auto_fix_enabled = self$auto_fix
        ),
        validation_results = list()
      )
      
      # Add results for each rule
      for (rule_name in names(self$validation_results)) {
        result <- self$validation_results[[rule_name]]
        rule <- self$rules[[rule_name]]
        
        # Simplify locations to make them JSON-friendly
        locations_json <- NULL
        if (!is.null(result$locations)) {
          if (is.list(result$locations)) {
            locations_json <- lapply(result$locations, function(loc) {
              if (length(loc) > 100) {
                list(
                  count = length(loc),
                  sample = head(loc, 20)
                )
              } else {
                loc
              }
            })
          } else {
            if (length(result$locations) > 100) {
              locations_json <- list(
                count = length(result$locations),
                sample = head(result$locations, 20)
              )
            } else {
              locations_json <- result$locations
            }
          }
        }
        
        report$validation_results[[rule_name]] <- list(
          name = rule_name,
          description = rule$description,
          passed = result$passed,
          message = result$message,
          locations = locations_json,
          fixable = result$fixable,
          fixed = result$fixed
        )
      }
      
      # Convert to JSON
      return(jsonlite::toJSON(report, pretty = TRUE, auto_unbox = TRUE))
    },
    
    #' @description Create an HTML validation report
    #' @param total_issues Total number of issues found
    #' @param fixable_issues Number of fixable issues
    #' @param fixed_issues Number of issues fixed
    #' @return HTML report as string
    create_html_report = function(total_issues, fixable_issues, fixed_issues) {
      # Start building the HTML report
      html_lines <- c(
        "<!DOCTYPE html>",
        "<html>",
        "<head>",
        "  <title>Traffic Safety Data Validation Report</title>",
        "  <style>",
        "    body { font-family: Arial, sans-serif; margin: 20px; }",
        "    h1 { color: #333366; }",
        "    h2 { color: #333366; margin-top: 30px; }",
        "    h3 { margin-top: 20px; }",
        "    .summary { background-color: #f5f5f5; padding: 15px; border-radius: 5px; }",
        "    .passed { color: green; font-weight: bold; }",
        "    .failed { color: red; font-weight: bold; }",
        "    .fixed { color: orange; font-weight: bold; }",
        "    .rule { border: 1px solid #ddd; margin: 10px 0; padding: 15px; border-radius: 5px; }",
        "    .locations { font-family: monospace; margin-left: 20px; }",
        "  </style>",
        "</head>",
        "<body>",
        "  <h1>Traffic Safety Data Validation Report</h1>",
        "  <p>Report generated: " %+% format(Sys.time(), "%Y-%m-%d %H:%M:%S") %+% "</p>",
        "  <div class='summary'>",
        "    <h2>Summary</h2>",
        "    <p>Total records: " %+% nrow(self$data) %+% "</p>",
        "    <p>Total issues found: " %+% total_issues %+% "</p>",
        "    <p>Automatically fixable issues: " %+% fixable_issues %+% "</p>",
        if (self$auto_fix) "    <p>Issues fixed: " %+% fixed_issues %+% "</p>" else "    <p>Auto-fix disabled</p>",
        "  </div>",
        "  <h2>Validation Results</h2>"
      )
      
      # Add results for each rule
      for (rule_name in names(self$validation_results)) {
        result <- self$validation_results[[rule_name]]
        rule <- self$rules[[rule_name]]
        
        status_class <- if (result$passed) "passed" else "failed"
        status_text <- if (result$passed) "✅ PASSED" else "❌ FAILED"
        
        if (!result$passed && result$fixed) {
          status_class <- "fixed"
          status_text <- "🔧 FIXED"
        }
        
        html_lines <- c(
          html_lines,
          "  <div class='rule'>",
          "    <h3>" %+% rule_name %+% " - <span class='" %+% status_class %+% "'>" %+% status_text %+% "</span></h3>",
          "    <p><strong>Description:</strong> " %+% rule$description %+% "</p>",
          "    <p><strong>Result:</strong> " %+% result$message %+% "</p>"
        )
        
        # Add details about issues if any
        if (!result$passed && !is.null(result$locations)) {
          html_lines <- c(html_lines, "    <div class='locations'>")
          
          if (is.list(result$locations)) {
            for (location_name in names(result$locations)) {
              locations <- result$locations[[location_name]]
              if (length(locations) > 10) {
                # Too many issues to list, show summary
                html_lines <- c(
                  html_lines,
                  "      <p>Issues in <strong>" %+% location_name %+% "</strong>: " %+% length(locations) %+% " records</p>",
                  "      <p>First few affected rows: " %+% paste(head(locations, 5), collapse = ", ") %+% "...</p>"
                )
              } else {
                # List all issues
                html_lines <- c(
                  html_lines,
                  "      <p>Issues in <strong>" %+% location_name %+% "</strong>: " %+% paste(locations, collapse = ", ") %+% "</p>"
                )
              }
            }
          } else {
            if (length(result$locations) > 10) {
              # Too many issues to list, show summary
              html_lines <- c(
                html_lines,
                "      <p>Issues found in " %+% length(result$locations) %+% " records</p>",
                "      <p>First few affected rows: " %+% paste(head(result$locations, 5), collapse = ", ") %+% "...</p>"
              )
            } else {
              # List all issues
              html_lines <- c(
                html_lines,
                "      <p>Affected rows: " %+% paste(result$locations, collapse = ", ") %+% "</p>"
              )
            }
          }
          
          html_lines <- c(html_lines, "    </div>")
        }
        
        # Add fix status
        if (!result$passed) {
          fix_status <- if (result$fixable) {
            if (self$auto_fix) {
              if (result$fixed) "Issues were automatically fixed" else "Failed to fix issues"
            } else {
              "Issues can be fixed automatically (auto_fix=TRUE)"
            }
          } else {
            "Issues require manual review"
          }
          
          html_lines <- c(
            html_lines,
            "    <p><strong>Fix status:</strong> " %+% fix_status %+% "</p>"
          )
        }
        
        html_lines <- c(html_lines, "  </div>")
      }
      
      # Add recommendations
      html_lines <- c(
        html_lines,
        "  <h2>Recommendations</h2>"
      )
      
      if (total_issues == 0) {
        html_lines <- c(
          html_lines,
          "  <p class='passed'>✅ No data quality issues found. Data is valid for further analysis.</p>"
        )
      } else {
        if (fixed_issues == total_issues) {
          html_lines <- c(
            html_lines,
            "  <p class='passed'>✅ All issues have been automatically fixed. Data is now valid for further analysis.</p>"
          )
        } else {
          unfixed_issues <- total_issues - fixed_issues
          html_lines <- c(
            html_lines,
            "  <p class='failed'>❗ " %+% unfixed_issues %+% " issues still need to be addressed:</p>",
            "  <ul>"
          )
          
          # List unfixed issues by category
          for (rule_name in names(self$validation_results)) {
            result <- self$validation_results[[rule_name]]
            if (!result$passed && !result$fixed) {
              html_lines <- c(
                html_lines,
                "    <li><strong>" %+% rule_name %+% "</strong>: " %+% result$message %+% "</li>"
              )
            }
          }
          
          html_lines <- c(html_lines, "  </ul>")
        }
      }
      
      # Close the HTML
      html_lines <- c(
        html_lines,
        "</body>",
        "</html>"
      )
      
      # Return the complete HTML report
      return(paste(html_lines, collapse = "\n"))
    }
  )
)

#' Create validation hooks for the traffic safety data pipeline
#'
#' @param auto_fix Whether to automatically fix issues when possible
#' @return List of hook functions for the pipeline
#' 
#' @export
create_validation_hooks <- function(auto_fix = FALSE) {
  # Define hooks
  hooks <- list(
    pre_fetch = function(years, cache_dir, ...) {
      # Verify years are valid
      current_year <- as.numeric(format(Sys.Date(), "%Y"))
      if (any(years > current_year)) {
        warning("Future years requested, capping at current year")
        years <- years[years <= current_year]
      }
      
      # Verify cache directory exists
      if (!dir.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
      }
      
      return(list(years = years, cache_dir = cache_dir))
    },
    
    post_fetch = function(traffic_data, ...) {
      # Skip validation if data is empty
      if (is.null(traffic_data) || nrow(traffic_data) == 0) {
        warning("No traffic data to validate")
        return(traffic_data)
      }
      
      # Create validator
      validator <- TrafficDataValidator$new(traffic_data, auto_fix = auto_fix)
      
      # Run validation
      results <- validator$validate()
      
      # Get possibly fixed data
      validated_data <- validator$get_data()
      
      # Generate validation report
      report_path <- file.path(dirname(tempdir()), "traffic_safety_validation_report.md")
      validator$generate_report(format = "markdown", file_path = report_path)
      
      # Print report path for reference
      message("Validation report saved to: ", report_path)
      
      # Check if we should return the fixed data or original
      issue_count <- sum(sapply(results, function(r) !r$passed))
      if (issue_count > 0) {
        if (auto_fix) {
          fixed_count <- sum(sapply(results, function(r) r$fixed))
          message(paste("Fixed", fixed_count, "of", issue_count, "validation issues"))
          if (fixed_count < issue_count) {
            warning(paste(issue_count - fixed_count, "issues remain - see validation report"))
          }
        } else {
          warning(paste("Found", issue_count, "validation issues - see validation report"))
        }
      } else {
        message("Data validation passed with no issues")
      }
      
      return(validated_data)
    },
    
    pre_save = function(traffic_data, output_file, ...) {
      # Check if data exists
      if (is.null(traffic_data) || nrow(traffic_data) == 0) {
        warning("No traffic data to save")
        return(NULL)
      }
      
      # Create directory if it doesn't exist
      output_dir <- dirname(output_file)
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      }
      
      # Run a final validation check
      validator <- TrafficDataValidator$new(traffic_data, auto_fix = auto_fix)
      
      # Run critical validation rules only
      validator$run_rule("required_columns")
      validator$run_rule("data_types")
      validator$run_rule("fips_format")
      
      # Get the validated data
      validated_data <- validator$get_data()
      
      # Return data and file path
      return(list(data = validated_data, file_path = output_file))
    }
  )
  
  return(hooks)
}

#' Validate traffic safety data with an optional validation report
#'
#' @param traffic_data Traffic safety data frame
#' @param auto_fix Whether to automatically fix issues when possible
#' @param report_format Format for validation report (markdown, json, html)
#' @param report_file Optional file path to save the report
#' @return List containing validated data and validation results
#' 
#' @export
validate_traffic_safety_data <- function(traffic_data, 
                                       auto_fix = FALSE,
                                       report_format = "markdown",
                                       report_file = NULL) {
  # Skip validation if data is empty
  if (is.null(traffic_data) || nrow(traffic_data) == 0) {
    warning("No traffic data to validate")
    return(list(
      data = traffic_data,
      valid = FALSE,
      message = "No data to validate",
      report = NULL
    ))
  }
  
  # Create validator
  validator <- TrafficDataValidator$new(traffic_data, auto_fix = auto_fix)
  
  # Run validation
  results <- validator$validate()
  
  # Get possibly fixed data
  validated_data <- validator$get_data()
  
  # Generate validation report
  if (is.null(report_file) && report_format != "none") {
    report_file <- tempfile(pattern = "traffic_safety_validation", 
                           fileext = switch(report_format,
                                           "markdown" = ".md",
                                           "json" = ".json",
                                           "html" = ".html",
                                           ".txt"))
  }
  
  if (report_format != "none") {
    report <- validator$generate_report(format = report_format, file_path = report_file)
  } else {
    report <- NULL
  }
  
  # Check if all validations passed
  all_passed <- all(sapply(results, function(r) r$passed))
  
  # Count issues
  issue_count <- sum(sapply(results, function(r) !r$passed))
  fixed_count <- sum(sapply(results, function(r) r$fixed))
  
  # Create summary message
  if (all_passed) {
    message <- "Data validation passed with no issues"
  } else if (fixed_count == issue_count) {
    message <- paste("All", issue_count, "validation issues were fixed")
  } else {
    message <- paste(issue_count - fixed_count, "of", issue_count, 
                    "validation issues remain unfixed")
  }
  
  # Return results
  return(list(
    data = validated_data,
    valid = all_passed || fixed_count == issue_count,
    message = message,
    report = report,
    report_file = if (report_format != "none") report_file else NULL,
    results = results
  ))
}

# String concatenation helper to avoid lots of paste() calls in report generation
"%+%" <- function(a, b) paste0(a, b)

# Run validation if this script is executed directly
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) > 0 && (args[1] == "--help" || args[1] == "-h")) {
    cat("Traffic Safety Data Validation\n")
    cat("Usage: Rscript traffic_safety_validation.r [data_file] [auto_fix] [report_format] [report_file]\n")
    cat("  data_file: Path to traffic safety data file (CSV, RDS)\n")
    cat("  auto_fix: Whether to automatically fix issues (TRUE/FALSE, default: FALSE)\n")
    cat("  report_format: Format for validation report (markdown, json, html, default: markdown)\n")
    cat("  report_file: Output file for validation report\n")
    quit(status = 0)
  }
  
  # Parse arguments
  data_file <- if (length(args) >= 1) args[1] else NULL
  auto_fix <- if (length(args) >= 2) as.logical(args[2]) else FALSE
  report_format <- if (length(args) >= 3) args[3] else "markdown"
  report_file <- if (length(args) >= 4) args[4] else NULL
  
  if (is.null(data_file)) {
    cat("Error: No data file specified\n")
    cat("Run with --help for usage information\n")
    quit(status = 1)
  }
  
  # Read data file
  traffic_data <- NULL
  
  if (grepl("\\.csv$", data_file, ignore.case = TRUE)) {
    traffic_data <- read.csv(data_file, stringsAsFactors = FALSE)
  } else if (grepl("\\.rds$", data_file, ignore.case = TRUE)) {
    traffic_data <- readRDS(data_file)
  } else {
    cat("Error: Unsupported file format. Use CSV or RDS files.\n")
    quit(status = 1)
  }
  
  # Run validation
  result <- validate_traffic_safety_data(
    traffic_data = traffic_data,
    auto_fix = auto_fix,
    report_format = report_format,
    report_file = report_file
  )
  
  # Print result
  cat(result$message, "\n")
  
  if (!is.null(result$report_file)) {
    cat("Validation report saved to:", result$report_file, "\n")
  }
  
  # Exit with status based on validation result
  quit(status = if (result$valid) 0 else 1)
}