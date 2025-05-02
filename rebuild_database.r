#!/usr/bin/env Rscript

# rebuild_database.r
# Rebuilds the SDOH database from scratch by removing the existing database
# and running the pipeline with overwrite_db=TRUE

# Set working directory to the project root
setwd(dirname(getwd()))

# Source the core functions
source("R/pipeline_modules/module_core.r")

# Initialize logging
log_file <- file.path("R/logs", paste0("rebuild_database_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
log_message("Starting database rebuild...", log_file = log_file, level = "INFO", show_console = TRUE)

# Define paths
db_path <- "R/output/us_county_sdoh_unified.duckdb"
backup_path <- paste0(db_path, ".bak_", format(Sys.time(), "%Y%m%d_%H%M%S"))

# Back up the existing database if it exists
if (file.exists(db_path)) {
  log_message(paste("Backing up existing database to", backup_path), 
              log_file = log_file, level = "INFO", show_console = TRUE)
  file.copy(db_path, backup_path)
  
  # Remove the existing database
  log_message("Removing existing database...", 
              log_file = log_file, level = "INFO", show_console = TRUE)
  file.remove(db_path)
}

# Load the cached processed data
cache_path <- "R/data/cache/processed_sdoh_data.rds"
if (!file.exists(cache_path)) {
  log_message("Processed data cache not found. Please run the pipeline first.", 
              log_file = log_file, level = "ERROR", show_console = TRUE)
  stop("Processed data cache not found at ", cache_path)
}

log_message("Loading processed data from cache...", 
            log_file = log_file, level = "INFO", show_console = TRUE)
processed_data <- readRDS(cache_path)

# Load the variable crosswalk
crosswalk_path <- "R/output/variable_crosswalk_consolidated.csv"
if (!file.exists(crosswalk_path)) {
  log_message("Variable crosswalk not found. Please run the pipeline first.", 
              log_file = log_file, level = "ERROR", show_console = TRUE)
  stop("Variable crosswalk not found at ", crosswalk_path)
}

log_message("Loading variable crosswalk...", 
            log_file = log_file, level = "INFO", show_console = TRUE)
crosswalk <- readr::read_csv(crosswalk_path, show_col_types = FALSE)

# Source the database module
source("R/pipeline_modules/module_database.r")

# Create the database with overwrite = TRUE
log_message("Creating database with overwrite = TRUE...", 
            log_file = log_file, level = "INFO", show_console = TRUE)
result <- create_unified_database(
  processed_data = processed_data,
  crosswalk = crosswalk,
  db_path = db_path,
  overwrite = TRUE,
  incremental = FALSE,
  force_full_rebuild = TRUE
)

if (result) {
  log_message("Database rebuild completed successfully!", 
              log_file = log_file, level = "INFO", show_console = TRUE)
  
  # Run the map generation script
  log_message("Generating maps...", 
              log_file = log_file, level = "INFO", show_console = TRUE)
  source("R/generate_conus_maps.r")
  
  # Generate maps using the new database
  generate_conus_maps(
    output_dir = "R/output/maps",
    db_path = db_path,
    conus_only = TRUE
  )
  
  log_message("Map generation completed.", 
              log_file = log_file, level = "INFO", show_console = TRUE)
  log_message("Database rebuild and map generation process completed successfully!", 
              log_file = log_file, level = "INFO", show_console = TRUE)
} else {
  log_message("Database rebuild failed. Please check the logs for details.", 
              log_file = log_file, level = "ERROR", show_console = TRUE)
  
  # Restore from backup if available
  if (file.exists(backup_path)) {
    log_message("Restoring database from backup...", 
                log_file = log_file, level = "INFO", show_console = TRUE)
    file.copy(backup_path, db_path, overwrite = TRUE)
    log_message("Database restored from backup.", 
                log_file = log_file, level = "INFO", show_console = TRUE)
  }
}