#!/usr/bin/env Rscript

# Traffic Safety Validation Module
# This module provides data validation capabilities for traffic safety data
# including automated checks, fixes, and reporting

# Required packages
required_packages <- c(
  "R6",
  "tidyverse",
  "jsonlite"
)

# Load required packages
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    message(paste("Required package", pkg, "is not installed."))
    message("Please run 'Rscript R/install_packages.r' first.")
    # Don't stop execution, just warn and continue with reduced functionality
  }
}

# String concatenation helper
"%+%" <- function(a, b) paste0(a, b)

#' R6 class for validating traffic safety data
#' 
#' @description Validates traffic safety data against a set of rules.
#'   Can automatically fix issues when possible.
#'
#' @field data The traffic safety data frame
#' @field rules List of validation rules
#' @field validation_results Results of validation
#' @field auto_fix Whether to automatically fix issues
#'
#' @export
TrafficDataValidator <- R6::R6Class(
  "TrafficDataValidator",
  
  public = list(
    data = NULL,
    rules = list(),
    validation_results = list(),
    auto_fix = FALSE,
    
    #' @description Initialize a new TrafficDataValidator
    #' @param data The traffic safety data frame to validate
    #' @param auto_fix Whether to automatically fix issues (default FALSE)
    initialize = function(data, auto_fix = FALSE) {
      self$data <- data
      self$auto_fix <- auto_fix
      self$validation_results <- list()
      self$init_standard_rules()
    },
    
    #' @description Initialize standard validation rules
    init_standard_rules = function() {
      # Required columns rule
      self$rules$required_columns <- list(
        name = "required_columns",
        description = "Check for required columns",
        check = function(data) {
          required <- c("fips", "year", "traffic_fatality_count")
          missing <- required[!required %in% names(data)]
          
          if (length(missing) > 0) {
            return(list(
              passed = FALSE,
              message = "Missing required columns: " %+% paste(missing, collapse = ", "),
              locations = missing,
              fixable = FALSE
            ))
          }
          
          return(list(passed = TRUE, message = "All required columns present"))
        },
        fix = NULL  # Not fixable
      )
      
      # Data types rule
      self$rules$data_types <- list(
        name = "data_types",
        description = "Check column data types",
        check = function(data) {
          issues <- list()
          
          # Check fips is character
          if ("fips" %in% names(data) && !is.character(data$fips)) {
            issues$fips <- "fips should be character"
          }
          
          # Check year is numeric
          if ("year" %in% names(data) && !is.numeric(data$year)) {
            issues$year <- "year should be numeric"
          }
          
          # Check count columns are numeric
          count_cols <- grep("count$", names(data), value = TRUE)
          for (col in count_cols) {
            if (!is.numeric(data[[col]])) {
              issues[[col]] <- col %+% " should be numeric"
            }
          }
          
          # Check rate columns are numeric
          rate_cols <- grep("rate", names(data), value = TRUE)
          for (col in rate_cols) {
            if (!is.numeric(data[[col]])) {
              issues[[col]] <- col %+% " should be numeric"
            }
          }
          
          if (length(issues) > 0) {
            return(list(
              passed = FALSE,
              message = "Data type issues found: " %+% paste(unlist(issues), collapse = "; "),
              locations = names(issues),
              fixable = TRUE
            ))
          }
          
          return(list(passed = TRUE, message = "All data types are correct"))
        },
        fix = function(data) {
          # Convert fips to character with proper formatting
          if ("fips" %in% names(data)) {
            data$fips <- as.character(data$fips)
            # Ensure standard 5-digit FIPS format
            data$fips <- sprintf("%05d", as.numeric(data$fips))
          }
          
          # Convert year to numeric
          if ("year" %in% names(data)) {
            data$year <- as.numeric(data$year)
          }
          
          # Convert count columns to numeric
          count_cols <- grep("count$", names(data), value = TRUE)
          for (col in count_cols) {
            data[[col]] <- as.numeric(data[[col]])
          }
          
          # Convert rate columns to numeric
          rate_cols <- grep("rate", names(data), value = TRUE)
          for (col in rate_cols) {
            data[[col]] <- as.numeric(data[[col]])
          }
          
          return(data)
        }
      )
      
      # FIPS code format rule
      self$rules$fips_format <- list(
        name = "fips_format",
        description = "Check FIPS code format",
        check = function(data) {
          if (!"fips" %in% names(data)) {
            return(list(
              passed = FALSE,
              message = "Missing FIPS column",
              locations = NULL,
              fixable = FALSE
            ))
          }
          
          # Check if FIPS codes are in the standard 5-digit format
          invalid_fips <- which(!grepl("^[0-9]{5}$", data$fips))
          
          if (length(invalid_fips) > 0) {
            sample_invalid <- head(data$fips[invalid_fips], 5)
            return(list(
              passed = FALSE,
              message = "Invalid FIPS format found for " %+% length(invalid_fips) %+% 
                " entries. Examples: " %+% paste(sample_invalid, collapse = ", "),
              locations = invalid_fips,
              fixable = TRUE
            ))
          }
          
          return(list(passed = TRUE, message = "All FIPS codes have valid format"))
        },
        fix = function(data) {
          # Try to fix FIPS codes by standardizing to 5 digits
          data$fips <- tryCatch({
            sprintf("%05d", as.numeric(data$fips))
          }, error = function(e) {
            data$fips  # Keep original if conversion fails
          })
          
          return(data)
        }
      )
      
      # Value range rule
      self$rules$value_ranges <- list(
        name = "value_ranges",
        description = "Check value ranges for reasonableness",
        check = function(data) {
          issues <- list()
          
          # Check year ranges
          if ("year" %in% names(data)) {
            current_year <- as.numeric(format(Sys.Date(), "%Y"))
            future_years <- which(data$year > current_year)
            if (length(future_years) > 0) {
              issues$future_years <- "Found " %+% length(future_years) %+% 
                " entries with years in the future"
            }
            
            too_old_years <- which(data$year < 1970)
            if (length(too_old_years) > 0) {
              issues$too_old_years <- "Found " %+% length(too_old_years) %+% 
                " entries with years before 1970"
            }
          }
          
          # Check count values are non-negative
          count_cols <- grep("count$", names(data), value = TRUE)
          for (col in count_cols) {
            if (is.numeric(data[[col]])) {
              negative_counts <- which(data[[col]] < 0)
              if (length(negative_counts) > 0) {
                issues[[col %+% "_negative"]] <- "Found " %+% length(negative_counts) %+% 
                  " negative values for " %+% col
              }
            }
          }
          
          # Check rate values are reasonable
          rate_cols <- grep("rate", names(data), value = TRUE)
          for (col in rate_cols) {
            if (is.numeric(data[[col]])) {
              negative_rates <- which(data[[col]] < 0)
              if (length(negative_rates) > 0) {
                issues[[col %+% "_negative"]] <- "Found " %+% length(negative_rates) %+% 
                  " negative values for " %+% col
              }
              
              very_high_rates <- which(data[[col]] > 100)
              if (length(very_high_rates) > 0) {
                issues[[col %+% "_high"]] <- "Found " %+% length(very_high_rates) %+% 
                  " potentially unrealistic high values for " %+% col %+% " (>100 per 100k)"
              }
            }
          }
          
          if (length(issues) > 0) {
            return(list(
              passed = FALSE,
              message = "Value range issues found: " %+% paste(unlist(issues), collapse = "; "),
              locations = names(issues),
              fixable = TRUE
            ))
          }
          
          return(list(passed = TRUE, message = "All values are within reasonable ranges"))
        },
        fix = function(data) {
          # Fix future years
          if ("year" %in% names(data)) {
            current_year <- as.numeric(format(Sys.Date(), "%Y"))
            data$year <- pmin(data$year, current_year)
          }
          
          # Fix negative counts
          count_cols <- grep("count$", names(data), value = TRUE)
          for (col in count_cols) {
            if (is.numeric(data[[col]])) {
              data[[col]] <- pmax(0, data[[col]], na.rm = TRUE)
            }
          }
          
          # Fix negative rates
          rate_cols <- grep("rate", names(data), value = TRUE)
          for (col in rate_cols) {
            if (is.numeric(data[[col]])) {
              data[[col]] <- pmax(0, data[[col]], na.rm = TRUE)
            }
          }
          
          return(data)
        }
      )
      
      # Duplicate check rule
      self$rules$duplicates <- list(
        name = "duplicates",
        description = "Check for duplicate rows by fips and year",
        check = function(data) {
          if (!all(c("fips", "year") %in% names(data))) {
            return(list(
              passed = FALSE,
              message = "Missing fips or year columns needed for duplicate check",
              locations = NULL,
              fixable = FALSE
            ))
          }
          
          # Count occurrences of each fips-year combination
          counts <- table(data$fips, data$year)
          duplicates <- which(counts > 1, arr.ind = TRUE)
          
          if (nrow(duplicates) > 0) {
            dup_fips <- rownames(counts)[duplicates[, 1]]
            dup_years <- colnames(counts)[duplicates[, 2]]
            dup_pairs <- paste(dup_fips, dup_years, sep = "-")
            
            return(list(
              passed = FALSE,
              message = "Found " %+% nrow(duplicates) %+% " duplicate fips-year combinations. Examples: " %+% 
                paste(head(dup_pairs, 5), collapse = ", "),
              locations = duplicates,
              fixable = TRUE
            ))
          }
          
          return(list(passed = TRUE, message = "No duplicates found"))
        },
        fix = function(data) {
          # Keep only the first occurrence of each fips-year combination
          data <- data %>%
            dplyr::group_by(fips, year) %>%
            dplyr::slice(1) %>%
            dplyr::ungroup()
          
          return(data)
        }
      )
      
      # Missing value check
      self$rules$missing_values <- list(
        name = "missing_values",
        description = "Check for missing values in key columns",
        check = function(data) {
          key_columns <- c("fips", "year", "traffic_fatality_count")
          key_columns <- key_columns[key_columns %in% names(data)]
          
          missing_counts <- sapply(data[key_columns], function(x) sum(is.na(x)))
          columns_with_missing <- names(missing_counts[missing_counts > 0])
          
          if (length(columns_with_missing) > 0) {
            message <- "Missing values found in key columns: "
            for (col in columns_with_missing) {
              message <- message %+% col %+% " (" %+% missing_counts[col] %+% " missing), "
            }
            message <- substr(message, 1, nchar(message) - 2)
            
            return(list(
              passed = FALSE,
              message = message,
              locations = columns_with_missing,
              fixable = FALSE  # Not auto-fixable without data imputation
            ))
          }
          
          return(list(passed = TRUE, message = "No missing values in key columns"))
        },
        fix = NULL
      )
      
      # Data quality flags check
      self$rules$data_quality_flags <- list(
        name = "data_quality_flags",
        description = "Check data quality flags for consistency",
        check = function(data) {
          # Find columns that should have quality flags
          data_columns <- grep("count$|rate", names(data), value = TRUE)
          data_columns <- data_columns[!grepl("_data_quality$", data_columns)]
          
          # Check for corresponding quality flag columns
          missing_flags <- character(0)
          for (col in data_columns) {
            flag_col <- paste0(col, "_data_quality")
            if (!flag_col %in% names(data)) {
              missing_flags <- c(missing_flags, flag_col)
            }
          }
          
          if (length(missing_flags) > 0) {
            return(list(
              passed = FALSE,
              message = "Missing data quality flag columns: " %+% paste(missing_flags, collapse = ", "),
              locations = missing_flags,
              fixable = TRUE
            ))
          }
          
          return(list(passed = TRUE, message = "All data columns have corresponding quality flags"))
        },
        fix = function(data) {
          # Add missing data quality flag columns
          data_columns <- grep("count$|rate", names(data), value = TRUE)
          data_columns <- data_columns[!grepl("_data_quality$", data_columns)]
          
          for (col in data_columns) {
            flag_col <- paste0(col, "_data_quality")
            if (!flag_col %in% names(data)) {
              # Create the missing flag column with "unknown" as default
              data[[flag_col]] <- "unknown"
            }
          }
          
          return(data)
        }
      )
    },
    
    #' @description Add a custom validation rule
    #' @param name Rule name
    #' @param description Rule description
    #' @param check_function Function that performs the check
    #' @param fix_function Function that fixes issues (optional)
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
    #' @return Validation result
    run_rule = function(rule_name) {
      if (!rule_name %in% names(self$rules)) {
        result <- list(
          passed = FALSE,
          message = "Rule not found: " %+% rule_name,
          locations = NULL,
          fixable = FALSE,
          fixed = FALSE
        )
        self$validation_results[[rule_name]] <- result
        return(result)
      }
      
      rule <- self$rules[[rule_name]]
      
      # Run the check
      check_result <- rule$check(self$data)
      check_result$fixed <- FALSE
      
      # Apply fix if needed and auto_fix is enabled
      if (!check_result$passed && 
          check_result$fixable && 
          self$auto_fix && 
          !is.null(rule$fix)) {
        
        # Apply the fix
        self$data <- rule$fix(self$data)
        
        # Re-run the check to see if it's fixed
        recheck_result <- rule$check(self$data)
        
        # Update the result
        check_result$fixed <- recheck_result$passed
        if (recheck_result$passed) {
          check_result$message <- check_result$message %+% " (Auto-fixed)"
        } else {
          check_result$message <- check_result$message %+% " (Auto-fix attempted but failed)"
        }
      }
      
      # Store and return the result
      self$validation_results[[rule_name]] <- check_result
      return(check_result)
    },
    
    #' @description Run all validation rules
    #' @return List of validation results
    validate = function() {
      for (rule_name in names(self$rules)) {
        self$run_rule(rule_name)
      }
      
      return(self$validation_results)
    },
    
    #' @description Get the validated data
    #' @return Validated data frame
    get_data = function() {
      return(self$data)
    },
    
    #' @description Generate a validation report
    #' @param format Report format (markdown, json, or html)
    #' @param file_path Optional file path to save the report
    #' @return Validation report as a string
    generate_report = function(format = "markdown", file_path = NULL) {
      # Make sure validation has been run
      if (length(self$validation_results) == 0) {
        self$validate()
      }
      
      # Count issues
      passed <- sapply(self$validation_results, function(r) r$passed)
      total_issues <- sum(!passed)
      
      fixable <- sapply(self$validation_results, function(r) {
        !r$passed && r$fixable
      })
      fixable_issues <- sum(fixable)
      
      fixed <- sapply(self$validation_results, function(r) {
        r$fixed
      })
      fixed_issues <- sum(fixed)
      
      # Generate the report based on format
      report <- switch(
        format,
        "markdown" = self$create_markdown_report(total_issues, fixable_issues, fixed_issues),
        "json" = self$create_json_report(total_issues, fixable_issues, fixed_issues),
        "html" = self$create_html_report(total_issues, fixable_issues, fixed_issues),
        # Default to markdown
        self$create_markdown_report(total_issues, fixable_issues, fixed_issues)
      )
      
      # Save the report if requested
      if (!is.null(file_path)) {
        dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
        writeLines(report, file_path)
      }
      
      return(report)
    },
    
    #' @description Create a markdown validation report
    #' @param total_issues Total number of issues found
    #' @param fixable_issues Number of fixable issues
    #' @param fixed_issues Number of fixed issues
    #' @return Markdown report as a string
    create_markdown_report = function(total_issues, fixable_issues, fixed_issues) {
      report <- "# Traffic Safety Data Validation Report\n\n"
      
      # Summary
      report <- report %+% "## Summary\n\n"
      report <- report %+% "- **Total Issues Found**: " %+% total_issues %+% "\n"
      report <- report %+% "- **Fixable Issues**: " %+% fixable_issues %+% "\n"
      report <- report %+% "- **Issues Fixed**: " %+% fixed_issues %+% "\n\n"
      
      # Results by rule
      report <- report %+% "## Validation Results\n\n"
      
      for (rule_name in names(self$rules)) {
        result <- self$validation_results[[rule_name]]
        status <- if (result$passed) "✅ PASSED" else "❌ FAILED"
        
        report <- report %+% "### " %+% rule_name %+% " - " %+% status %+% "\n\n"
        report <- report %+% "**Description**: " %+% self$rules[[rule_name]]$description %+% "\n\n"
        report <- report %+% "**Result**: " %+% result$message %+% "\n\n"
        
        if (!result$passed) {
          if (result$fixable) {
            if (result$fixed) {
              report <- report %+% "**Action**: Issue was automatically fixed.\n\n"
            } else if (self$auto_fix) {
              report <- report %+% "**Action**: Auto-fix was attempted but failed.\n\n"
            } else {
              report <- report %+% "**Action**: Issue is fixable but auto-fix is disabled.\n\n"
            }
          } else {
            report <- report %+% "**Action**: Issue requires manual intervention.\n\n"
          }
        }
      }
      
      # Add timestamp
      report <- report %+% "---\n\n"
      report <- report %+% "Report generated on " %+% format(Sys.time(), "%Y-%m-%d %H:%M:%S") %+% "\n"
      
      return(report)
    },
    
    #' @description Create a JSON validation report
    #' @param total_issues Total number of issues found
    #' @param fixable_issues Number of fixable issues
    #' @param fixed_issues Number of fixed issues
    #' @return JSON report as a string
    create_json_report = function(total_issues, fixable_issues, fixed_issues) {
      # Build the report structure
      report <- list(
        summary = list(
          total_issues = total_issues,
          fixable_issues = fixable_issues,
          fixed_issues = fixed_issues,
          overall_status = if (total_issues == 0 || total_issues == fixed_issues) "pass" else "fail"
        ),
        results = list(),
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
      
      # Add results for each rule
      for (rule_name in names(self$rules)) {
        result <- self$validation_results[[rule_name]]
        report$results[[rule_name]] <- list(
          status = if (result$passed) "pass" else "fail",
          description = self$rules[[rule_name]]$description,
          message = result$message,
          fixable = if (!result$passed) result$fixable else NULL,
          fixed = if (!result$passed) result$fixed else NULL
        )
      }
      
      # Convert to JSON
      json_report <- jsonlite::toJSON(report, auto_unbox = TRUE, pretty = TRUE)
      return(json_report)
    },
    
    #' @description Create an HTML validation report
    #' @param total_issues Total number of issues found
    #' @param fixable_issues Number of fixable issues
    #' @param fixed_issues Number of fixed issues
    #' @return HTML report as a string
    create_html_report = function(total_issues, fixable_issues, fixed_issues) {
      # Simple HTML report with embedded styling
      report <- "<!DOCTYPE html>\n<html>\n<head>\n<title>Traffic Safety Data Validation Report</title>\n"
      report <- report %+% "<style>\n"
      report <- report %+% "body { font-family: Arial, sans-serif; margin: 20px; }\n"
      report <- report %+% "h1 { color: #333366; }\n"
      report <- report %+% "h2 { color: #333366; margin-top: 20px; }\n"
      report <- report %+% "h3 { margin-top: 15px; }\n"
      report <- report %+% ".pass { color: green; }\n"
      report <- report %+% ".fail { color: red; }\n"
      report <- report %+% ".summary { background-color: #f0f0f0; padding: 10px; border-radius: 5px; }\n"
      report <- report %+% ".rule { margin-bottom: 20px; padding: 10px; border: 1px solid #ddd; border-radius: 5px; }\n"
      report <- report %+% ".timestamp { color: #666; font-size: 0.8em; margin-top: 30px; }\n"
      report <- report %+% "</style>\n</head>\n<body>\n"
      
      # Heading
      report <- report %+% "<h1>Traffic Safety Data Validation Report</h1>\n"
      
      # Summary
      report <- report %+% "<div class='summary'>\n"
      report <- report %+% "<h2>Summary</h2>\n"
      report <- report %+% "<p><strong>Total Issues Found</strong>: " %+% total_issues %+% "</p>\n"
      report <- report %+% "<p><strong>Fixable Issues</strong>: " %+% fixable_issues %+% "</p>\n"
      report <- report %+% "<p><strong>Issues Fixed</strong>: " %+% fixed_issues %+% "</p>\n"
      report <- report %+% "</div>\n"
      
      # Results by rule
      report <- report %+% "<h2>Validation Results</h2>\n"
      
      for (rule_name in names(self$rules)) {
        result <- self$validation_results[[rule_name]]
        status_class <- if (result$passed) "pass" else "fail"
        status_text <- if (result$passed) "PASSED" else "FAILED"
        
        report <- report %+% "<div class='rule'>\n"
        report <- report %+% "<h3>" %+% rule_name %+% " - <span class='" %+% status_class %+% "'>" %+% status_text %+% "</span></h3>\n"
        report <- report %+% "<p><strong>Description</strong>: " %+% self$rules[[rule_name]]$description %+% "</p>\n"
        report <- report %+% "<p><strong>Result</strong>: " %+% result$message %+% "</p>\n"
        
        if (!result$passed) {
          if (result$fixable) {
            if (result$fixed) {
              report <- report %+% "<p><strong>Action</strong>: Issue was automatically fixed.</p>\n"
            } else if (self$auto_fix) {
              report <- report %+% "<p><strong>Action</strong>: Auto-fix was attempted but failed.</p>\n"
            } else {
              report <- report %+% "<p><strong>Action</strong>: Issue is fixable but auto-fix is disabled.</p>\n"
            }
          } else {
            report <- report %+% "<p><strong>Action</strong>: Issue requires manual intervention.</p>\n"
          }
        }
        
        report <- report %+% "</div>\n"
      }
      
      # Timestamp
      report <- report %+% "<div class='timestamp'>\n"
      report <- report %+% "<p>Report generated on " %+% format(Sys.time(), "%Y-%m-%d %H:%M:%S") %+% "</p>\n"
      report <- report %+% "</div>\n"
      
      report <- report %+% "</body>\n</html>"
      
      return(report)
    }
  )
)

#' Create validation hooks for traffic safety data
#'
#' @param auto_fix Whether to automatically fix issues
#' @return List of validation hooks
#' @export
create_validation_hooks <- function(auto_fix = FALSE) {
  hooks <- list(
    pre_fetch = function(years, cache_dir, ...) {
      # Validate input parameters
      issues <- character(0)
      
      # Check years
      if (!is.null(years)) {
        if (!is.numeric(years) && !all(grepl("^\\d{4}$", as.character(years)))) {
          issues <- c(issues, "Years must be numeric or character representations of 4-digit years")
        }
        
        current_year <- as.numeric(format(Sys.Date(), "%Y"))
        if (any(as.numeric(years) > current_year)) {
          years <- years[as.numeric(years) <= current_year]
          message("Removed years beyond current year")
        }
        
        if (any(as.numeric(years) < 1970)) {
          message("Warning: Some years are earlier than 1970, data may be limited")
        }
      }
      
      # Check cache_dir
      if (!is.null(cache_dir)) {
        if (!is.character(cache_dir) || length(cache_dir) != 1) {
          issues <- c(issues, "cache_dir must be a single character string")
        } else {
          # Create the cache directory if it doesn't exist
          if (!dir.exists(cache_dir)) {
            dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
            message("Created cache directory: ", cache_dir)
          }
        }
      }
      
      # Report issues if any
      if (length(issues) > 0) {
        warning("Validation issues with input parameters: ", paste(issues, collapse = "; "))
      }
      
      # Return validated parameters
      return(list(years = years, cache_dir = cache_dir))
    },
    
    post_fetch = function(traffic_data, ...) {
      # Skip validation if no data
      if (is.null(traffic_data) || !is.data.frame(traffic_data) || nrow(traffic_data) == 0) {
        warning("No data to validate")
        return(traffic_data)
      }
      
      # Create validator and run validation
      validator <- TrafficDataValidator$new(traffic_data, auto_fix = auto_fix)
      results <- validator$validate()
      
      # Check for critical issues
      critical_issues <- !sapply(results, function(r) r$passed || r$fixed)
      if (any(critical_issues)) {
        warning("Critical validation issues found: ", 
                paste(names(results)[critical_issues], collapse = ", "))
      }
      
      # Return validated data
      return(validator$get_data())
    },
    
    pre_save = function(traffic_data, output_file, ...) {
      # Check if output directory exists
      if (!is.null(output_file)) {
        output_dir <- dirname(output_file)
        if (!dir.exists(output_dir)) {
          dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
          message("Created output directory: ", output_dir)
        }
      }
      
      return(list(data = traffic_data, file_path = output_file))
    }
  )
  
  return(hooks)
}

#' Validate traffic safety data
#'
#' @param traffic_data Traffic safety data frame
#' @param auto_fix Whether to automatically fix issues
#' @param report_format Format of validation report
#' @param report_file Path to save validation report
#' @return List with validation results and fixed data
#' @export
validate_traffic_safety_data <- function(traffic_data, 
                                     auto_fix = FALSE,
                                     report_format = "markdown",
                                     report_file = NULL) {
  # Create validator
  validator <- TrafficDataValidator$new(traffic_data, auto_fix = auto_fix)
  
  # Run validation
  results <- validator$validate()
  
  # Generate report
  report <- validator$generate_report(format = report_format, file_path = report_file)
  
  # Get validated data
  validated_data <- validator$get_data()
  
  # Check overall validation status
  passed <- sapply(results, function(r) r$passed || r$fixed)
  valid <- all(passed)
  
  # Prepare result summary
  return(list(
    data = validated_data,
    valid = valid,
    message = if (valid) "Data validation passed" else "Data validation failed",
    report = report,
    report_file = report_file,
    results = results
  ))
}

# Let the pipeline know the module is loaded
cat("[INFO] Traffic safety validation module loaded successfully\n")