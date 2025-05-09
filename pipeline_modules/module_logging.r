#!/usr/bin/env Rscript

# module_logging.r
# Comprehensive logging module for SDOH pipeline
# Created: 2025-05-08

# Create logs directory if it doesn't exist
if (!dir.exists("logs")) {
  dir.create("logs", recursive = TRUE)
}

# Global log file path - will be initialized based on script name
LOG_FILE <- NULL
CONSOLE_LOGGING <- TRUE
FILE_LOGGING <- TRUE
LOG_LEVEL <- "INFO"  # Default log level: DEBUG, INFO, WARN, ERROR

# Initialize logging for a specific script
initialize_logging <- function(script_name, log_level = NULL, console = TRUE, file = TRUE) {
  global_log_dir <- "logs"
  if (!dir.exists(global_log_dir)) {
    dir.create(global_log_dir, recursive = TRUE)
  }
  
  # Format script name for the log file
  script_base <- gsub("\\.r$", "", basename(script_name))
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  log_file <- file.path(global_log_dir, sprintf("%s_%s.log", script_base, timestamp))
  
  # Assign to global variable
  LOG_FILE <<- log_file
  CONSOLE_LOGGING <<- console
  FILE_LOGGING <<- file
  
  if (!is.null(log_level)) {
    LOG_LEVEL <<- toupper(log_level)
  }
  
  # Create initial log entry
  log_message(paste("LOGGING INITIALIZED FOR", script_name))
  log_message(paste("Log file:", log_file))
  log_message(paste("Log level:", LOG_LEVEL))
  log_message(paste("System info: R", R.version.string))
  log_message(paste("Working directory:", getwd()))
  log_message(paste("Date and time:", Sys.time()))
  
  # Return the log file path
  return(log_file)
}

# Function to determine if message should be logged based on level
should_log <- function(level) {
  level <- toupper(level)
  levels <- c("DEBUG", "INFO", "WARN", "ERROR")
  level_idx <- match(level, levels)
  current_idx <- match(LOG_LEVEL, levels)
  
  if (is.na(level_idx) || is.na(current_idx)) {
    return(TRUE)  # Log by default if levels are invalid
  }
  
  return(level_idx >= current_idx)
}

# Main logging function
log_message <- function(message, level = "INFO", show_console = CONSOLE_LOGGING, write_to_file = FILE_LOGGING) {
  if (!should_log(level)) {
    return(invisible(NULL))
  }
  
  level <- toupper(level)
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  formatted_msg <- sprintf("[%s] [%s] %s", timestamp, level, message)
  
  # Print to console if requested
  if (show_console) {
    # Format based on level
    if (level == "ERROR") {
      cat("\033[31m", formatted_msg, "\033[0m\n", sep="")  # Red for errors
    } else if (level == "WARN") {
      cat("\033[33m", formatted_msg, "\033[0m\n", sep="")  # Yellow for warnings
    } else if (level == "DEBUG") {
      cat("\033[36m", formatted_msg, "\033[0m\n", sep="")  # Cyan for debug
    } else {
      cat(formatted_msg, "\n", sep="")  # Default for INFO
    }
  }
  
  # Write to log file if requested
  if (write_to_file && !is.null(LOG_FILE)) {
    write(formatted_msg, file = LOG_FILE, append = TRUE)
  }
  
  # Return invisibly
  invisible(NULL)
}

# Specialized logging functions
log_debug <- function(message, show_console = CONSOLE_LOGGING, write_to_file = FILE_LOGGING) {
  log_message(message, level = "DEBUG", show_console = show_console, write_to_file = write_to_file)
}

log_info <- function(message, show_console = CONSOLE_LOGGING, write_to_file = FILE_LOGGING) {
  log_message(message, level = "INFO", show_console = show_console, write_to_file = write_to_file)
}

log_warn <- function(message, show_console = CONSOLE_LOGGING, write_to_file = FILE_LOGGING) {
  log_message(message, level = "WARN", show_console = show_console, write_to_file = write_to_file)
}

log_error <- function(message, show_console = CONSOLE_LOGGING, write_to_file = FILE_LOGGING) {
  log_message(message, level = "ERROR", show_console = show_console, write_to_file = write_to_file)
}

# Start a timer for a specific task
start_task_timer <- function(task_name) {
  timer_name <- paste0("timer_", gsub("[^a-zA-Z0-9]", "_", task_name))
  assign(timer_name, Sys.time(), envir = .GlobalEnv)
  log_info(paste("Starting task:", task_name))
  invisible(timer_name)
}

# End a timer for a specific task and log the elapsed time
end_task_timer <- function(task_name) {
  timer_name <- paste0("timer_", gsub("[^a-zA-Z0-9]", "_", task_name))
  if (exists(timer_name, envir = .GlobalEnv)) {
    start_time <- get(timer_name, envir = .GlobalEnv)
    end_time <- Sys.time()
    elapsed <- end_time - start_time
    
    # Format elapsed time in a readable way
    if (as.numeric(elapsed, units = "secs") < 60) {
      elapsed_str <- sprintf("%.2f seconds", as.numeric(elapsed, units = "secs"))
    } else if (as.numeric(elapsed, units = "mins") < 60) {
      elapsed_str <- sprintf("%.2f minutes", as.numeric(elapsed, units = "mins"))
    } else {
      elapsed_str <- sprintf("%.2f hours", as.numeric(elapsed, units = "hours"))
    }
    
    log_info(paste("Completed task:", task_name, "in", elapsed_str))
    rm(list = timer_name, envir = .GlobalEnv)
    return(invisible(elapsed))
  } else {
    log_warn(paste("No timer found for task:", task_name))
    return(invisible(NULL))
  }
}

# Log package loading with error handling
log_package_loading <- function(package_name) {
  tryCatch({
    if (!requireNamespace(package_name, quietly = TRUE)) {
      log_info(paste("Installing package:", package_name))
      install.packages(package_name, repos = "https://cloud.r-project.org")
    }
    library(package_name, character.only = TRUE)
    log_debug(paste("Successfully loaded package:", package_name))
  }, error = function(e) {
    log_error(paste("Failed to load package:", package_name, "-", conditionMessage(e)))
  })
}

# Log system information
log_system_info <- function() {
  log_info("=== SYSTEM INFORMATION ===")
  log_info(paste("R version:", R.version.string))
  log_info(paste("Platform:", R.version$platform))
  log_info(paste("Working directory:", getwd()))
  log_info(paste("User:", Sys.info()["user"]))
  log_info(paste("Date and time:", Sys.time()))
  
  # Log package versions if available
  if (requireNamespace("sessioninfo", quietly = TRUE)) {
    pkg_info <- sessioninfo::package_info(dependencies = FALSE)
    if (nrow(pkg_info) > 0) {
      log_info("Attached packages:")
      for (i in 1:nrow(pkg_info)) {
        if (pkg_info$attached[i]) {
          log_info(paste("  -", pkg_info$package[i], pkg_info$loadedversion[i]))
        }
      }
    }
  } else {
    # Fallback if sessioninfo is not available
    pkgs <- sessionInfo()$otherPkgs
    if (length(pkgs) > 0) {
      log_info("Attached packages:")
      for (i in 1:length(pkgs)) {
        log_info(paste("  -", names(pkgs)[i], pkgs[[i]]$Version))
      }
    }
  }
  log_info("===========================")
}

# Initialize error handling to log all errors
setup_error_logging <- function() {
  options(error = function() {
    log_error(paste("ERROR:", geterrmessage()))
    if (interactive()) stop(geterrmessage()) else quit(status = 1)
  })
}

# Log a section header to make logs more readable
log_section <- function(section_name) {
  section_line <- paste(rep("=", 50), collapse = "")
  log_info(section_line)
  log_info(paste("SECTION:", toupper(section_name)))
  log_info(section_line)
}

# Example usage:
# source("pipeline_modules/module_logging.r")
# initialize_logging("my_script.r")
# log_system_info()
# setup_error_logging()
# 
# log_section("Data Loading")
# timer <- start_task_timer("data loading")
# # ... do work ...
# end_task_timer("data loading")