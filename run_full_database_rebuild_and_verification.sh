#!/bin/bash

# Master script to create a unified database with all 255+ variables, verify it, and generate maps
# This script orchestrates the complete process of building a comprehensive SDOH database and visualization
# Updated with comprehensive logging - 2025-05-08

# Setup logging
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
LOG_DIR="logs"
MASTER_LOG="${LOG_DIR}/full_rebuild_${TIMESTAMP}.log"

# Create logs directory if it doesn't exist
mkdir -p "${LOG_DIR}"

# Function to log messages to both console and log file
log() {
  echo "$1"
  echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" >> "${MASTER_LOG}"
}

# Ensure our logging module exists
LOGGING_MODULE="pipeline_modules/module_logging.r"
if [ ! -f "${LOGGING_MODULE}" ]; then
  mkdir -p "pipeline_modules"
  log "Creating R logging module at ${LOGGING_MODULE}..."
  
  # Create the logging module content
  cat > "${LOGGING_MODULE}" << 'EOL'
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
EOL
fi

log "=== Starting Full Database Rebuild, Verification, and Map Generation ==="
log "Starting time: $(date)"
log "Master log file: ${MASTER_LOG}"

# First check for existing maps and database files
log "Checking for existing database and map files..."

DB_PATH="output/us_county_sdoh_unified.duckdb"
if [ -f "${DB_PATH}" ]; then
  DB_SIZE=$(du -h "${DB_PATH}" | cut -f1)
  log "Found existing database: ${DB_PATH} (Size: ${DB_SIZE})"
  log "Will be replaced during rebuild"
fi

MAP_DIR="output/maps"
# Create map directories if they don't exist
for DIR in "${MAP_DIR}" "${MAP_DIR}/by_year" "${MAP_DIR}/by_variable" "${MAP_DIR}/combined"; do
  if [ ! -d "$DIR" ]; then
    mkdir -p "$DIR"
    log "Created directory: $DIR"
  fi
done

# Clear existing map files
for SUBDIR in "by_year" "by_variable" "combined"; do
  FULLPATH="${MAP_DIR}/${SUBDIR}"
  if [ -d "$FULLPATH" ]; then
    COUNT=$(find "$FULLPATH" -name "*.png" | wc -l)
    if [ "$COUNT" -gt 0 ]; then
      log "Clearing ${COUNT} existing map files from ${FULLPATH}..."
      rm -f "${FULLPATH}"/*.png
    fi
  fi
done

# Create enhanced R scripts with logging
log "Setting up enhanced logging for R scripts..."

# Update generate_county_maps.r to use our logging module
cat > "generate_county_maps_with_logging.r" << 'EOL'
#!/usr/bin/env Rscript

# NOTE: This script is a wrapper for the enhanced generate_conus_maps.r script
# It provides proper logging and enhanced functionality

# First, load the logging module
if (file.exists("pipeline_modules/module_logging.r")) {
  source("pipeline_modules/module_logging.r")
  initialize_logging("generate_county_maps.r")
  log_system_info()
  setup_error_logging()
} else {
  cat("WARNING: Could not find logging module. Proceeding without detailed logging.\n")
}

# Load the main map generation script
log_section("INITIALIZING MAP GENERATION")
log_info("Loading map generation module: generate_conus_maps.r")
source("generate_conus_maps.r")

# Log the map generation configuration
log_section("MAP GENERATION CONFIGURATION")
log_info("Map configuration:")
log_info("  - Output directory: output/maps")
log_info("  - Database path: output/us_county_sdoh_unified.duckdb")  
log_info("  - Geographic coverage: Continental US with Alaska, excluding Hawaii")
log_info("  - State filtering: Include Alaska, exclude Hawaii (FIPS 15)")

# Start timing the map generation process
task_timer <- start_task_timer("Map Generation")

# Call the function with parameters for Continental US + Alaska - Hawaii
generate_conus_maps(
  output_dir = "output/maps",
  db_path = "output/us_county_sdoh_unified.duckdb",
  conus_only = FALSE,  # Include all states
  exclude_states = c("15"),  # Exclude Hawaii (FIPS code 15)
  include_alaska = TRUE  # Explicitly include Alaska
)

# Report completion and timing
end_task_timer("Map Generation")
log_section("MAP GENERATION COMPLETE")
log_info("Maps have been generated in output/maps directory")
log_info("Geographic coverage: Continental US with Alaska, excluding Hawaii")
EOL

log "Enhanced R script created with proper logging"

# Create logging wrapper for other scripts
for SCRIPT in "create_unified_database_with_all_variables.r" "verify_all_variables.r" "verify_traffic_safety_database.r" "verify_traffic_safety_data.r"; do
  if [ -f "$SCRIPT" ]; then
    WRAPPER="${SCRIPT%.r}_with_logging.r"
    log "Creating logging wrapper for $SCRIPT"
    
    cat > "$WRAPPER" << EOL
#!/usr/bin/env Rscript

# Logging wrapper for $SCRIPT
# Created: $(date)

# Load logging module
if (file.exists("pipeline_modules/module_logging.r")) {
  source("pipeline_modules/module_logging.r")
  initialize_logging("$SCRIPT")
  log_system_info()
  setup_error_logging()
  
  log_section("STARTING ${SCRIPT%.r} WITH ENHANCED LOGGING")
  log_info("Running original script with detailed logging...")
  
  # Start task timer
  task_timer <- start_task_timer("${SCRIPT%.r}")
  
  # Source the original script
  tryCatch({
    source("$SCRIPT")
    log_info("Script completed successfully")
  }, error = function(e) {
    log_error(paste("Script failed:", conditionMessage(e)))
    quit(status = 1)
  })
  
  # End task timer
  end_task_timer("${SCRIPT%.r}")
  log_section("COMPLETED ${SCRIPT%.r}")
} else {
  cat("ERROR: Logging module not found. Falling back to original script.\n")
  source("$SCRIPT")
}
EOL
  else
    log "WARNING: Script $SCRIPT not found, cannot create wrapper"
  fi
done

# Step 1: Create the comprehensive database with all 255+ variables
log "Step 1: Creating unified database with all variables..."
time Rscript create_unified_database_with_all_variables_with_logging.r 2>&1 | tee -a "${MASTER_LOG}"

# Check if database creation was successful
if [ $? -ne 0 ]; then
    log "ERROR: Database creation failed. Exiting."
    exit 1
fi

log "Database creation completed successfully."

# Check database file exists and report size
if [ -f "${DB_PATH}" ]; then
  DB_SIZE=$(du -h "${DB_PATH}" | cut -f1)
  log "Database created: ${DB_PATH} (Size: ${DB_SIZE})"
else
  log "WARNING: Expected database file ${DB_PATH} not found after creation step."
fi

# Step 2: Verify all variables in the database
log "Step 2: Verifying all variables in the database..."
time Rscript verify_all_variables_with_logging.r 2>&1 | tee -a "${MASTER_LOG}"

# Check if verification was successful
if [ $? -ne 0 ]; then
    log "ERROR: Variable verification failed."
    exit 1
fi

log "Variable verification completed successfully."

# Step 3: Enhanced verification for traffic safety data
log "Step 3: Performing enhanced verification for traffic safety data..."
time Rscript verify_traffic_safety_database_with_logging.r 2>&1 | tee -a "${MASTER_LOG}"

# Also run the standard verification for compatibility
log "Running additional traffic safety verification..."
time Rscript verify_traffic_safety_data_with_logging.r 2>&1 | tee -a "${MASTER_LOG}"

# Step 4: Generate maps for visualization
log "Step 4: Generating maps for all variables..."
log "Note: Maps will include Continental US with Alaska but exclude Hawaii"
time Rscript generate_county_maps_with_logging.r 2>&1 | tee -a "${MASTER_LOG}"

# Check if map generation was successful
if [ $? -ne 0 ]; then
    log "WARNING: Map generation had issues, but the database was created successfully."
else
    # Count generated maps
    BY_YEAR_COUNT=$(find "${MAP_DIR}/by_year" -name "*.png" 2>/dev/null | wc -l)
    BY_VAR_COUNT=$(find "${MAP_DIR}/by_variable" -name "*.png" 2>/dev/null | wc -l)
    COMBINED_COUNT=$(find "${MAP_DIR}/combined" -name "*.png" 2>/dev/null | wc -l)
    TOTAL_MAPS=$((BY_YEAR_COUNT + BY_VAR_COUNT + COMBINED_COUNT))
    
    log "Map generation completed successfully."
    log "Maps generated:"
    log "  - By year: ${BY_YEAR_COUNT}"
    log "  - By variable: ${BY_VAR_COUNT}"
    log "  - Combined: ${COMBINED_COUNT}"
    log "  - Total: ${TOTAL_MAPS}"
fi

# List all log files created
log "Log files generated in the logs directory:"
find "${LOG_DIR}" -type f -name "*.log" -mmin -60 | while read -r logfile; do
  log_size=$(du -h "${logfile}" | cut -f1)
  log "  - $(basename "${logfile}") (Size: ${log_size})"
done

# Display completion message
log "=== Full Database Rebuild, Verification, and Map Generation Completed Successfully ==="
log "Completion time: $(date)"
log "The database now contains all 255+ variables including traffic safety data"
log "You can access the database at: output/us_county_sdoh_unified.duckdb"
log "Maps have been generated in: output/maps/"
log "Detailed logs available in: logs/ directory"
log "Master log file: ${MASTER_LOG}"