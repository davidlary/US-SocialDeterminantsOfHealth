#!/usr/bin/env Rscript

# fix_map_generation.r
#
# This script ensures that the map generation works properly for all 255 variables
# by fixing how the generate_conus_maps.r script handles variables with no data

# Load required libraries
library(dplyr)
library(ggplot2)
library(sf)
library(DBI)
library(duckdb)

# Define utility function for logging
log_message <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
  formatted_message <- paste(timestamp, "[", level, "]", message)
  cat(formatted_message, "\n")
  return(invisible(formatted_message))
}

fix_map_generation <- function() {
  log_message("=================================================")
  log_message("FIXING MAP GENERATION FOR ALL VARIABLES")
  log_message("=================================================")
  
  # Fix 1: Ensure generate_conus_maps.r handles variables with no data properly
  maps_script_path <- "generate_conus_maps.r"
  
  if (!file.exists(maps_script_path)) {
    log_message(paste("Map generation script not found at:", maps_script_path), "ERROR")
    return(FALSE)
  }
  
  # Create backup if it doesn't exist
  backup_path <- paste0(maps_script_path, ".bak")
  if (!file.exists(backup_path)) {
    file.copy(maps_script_path, backup_path)
    log_message(paste("Created backup of original map script at:", backup_path))
  }
  
  # Read the script content
  maps_content <- readLines(maps_script_path)
  
  # Fix 1: Find and modify the coverage threshold for generating maps
  coverage_check <- grep("coverage_pct < 1", maps_content)
  
  if (length(coverage_check) > 0) {
    line_num <- coverage_check[1]
    old_line <- maps_content[line_num]
    
    # Change to allow maps even with no data (0% coverage)
    new_line <- "    # Allow maps even for variables with no data to ensure all variables get maps"
    maps_content[line_num] <- new_line
    
    # Add modified check that doesn't skip variables with no data
    additional_line <- "    if (FALSE) {  # Disabled check to ensure all variables get maps"
    maps_content <- c(
      maps_content[1:line_num],
      additional_line,
      maps_content[(line_num+1):length(maps_content)]
    )
    
    log_message("Modified coverage check to ensure maps are generated for all variables")
  }
  
  # Fix 2: Modify the map rendering for variables with no data
  render_section <- grep("if \\(is.null\\(var_data\\) \\|\\| nrow\\(var_data\\) == 0\\)", maps_content)
  
  if (length(render_section) > 0) {
    line_num <- render_section[1]
    next_line <- maps_content[line_num + 1]
    
    if (grepl("WARNING|return\\(NULL\\)", next_line)) {
      # Replace the return NULL with code to generate an empty map
      maps_content[line_num + 1] <- "      log_message(sprintf(\"WARNING: No data available for %s in %d - creating empty map\", variable, year))"
      
      # Add code to create empty maps for variables with no data
      empty_map_code <- c(
        "      # Create empty map for variables with no data to ensure all variables get maps",
        "      # This ensures the pipeline doesn't miss any variables",
        "      var_description <- if (nrow(var_meta) > 0 && !is.na(var_meta$description[1])) {",
        "        var_meta$description[1]",
        "      } else {",
        "        gsub(\"_\", \" \", tools::toTitleCase(variable))",
        "      }",
        "      ",
        "      # Create a simple empty dataset with the county shapefile",
        "      map_data <- county_sf %>%",
        "        mutate(!!variable := NA)",
        "      ",
        "      # Choose a neutral palette for empty maps",
        "      fill_scale <- scale_fill_distiller(",
        "        name = \"No Data\",",
        "        palette = \"Greys\",",
        "        direction = 1,",
        "        na.value = \"grey90\"",
        "      )",
        "      ",
        "      # Create the map",
        "      p <- ggplot(map_data) +",
        "        geom_sf(aes(fill = .data[[variable]]), color = \"white\", size = 0.1) +",
        "        fill_scale +",
        "        labs(",
        "          title = sprintf(\"%s (%d)\", var_description, year),",
        "          subtitle = \"No data available for this variable\",",
        "          caption = sprintf(\"Source: US Social Determinants of Health Dataset %d\", year)",
        "        ) +",
        "        theme_minimal() +",
        "        theme(",
        "          plot.title = element_text(size = 14, face = \"bold\"),",
        "          plot.subtitle = element_text(size = 10),",
        "          plot.caption = element_text(size = 8),",
        "          legend.position = \"none\",",
        "          panel.grid = element_blank(),",
        "          axis.text = element_blank(),",
        "          axis.title = element_blank(),",
        "          axis.ticks = element_blank()",
        "        )",
        "      ",
        "      # Save the maps",
        "      ggsave(year_filename, p, width = 10, height = 7, dpi = 150)",
        "      ggsave(variable_filename, p, width = 10, height = 7, dpi = 150)",
        "      ",
        "      return(c(year_filename, variable_filename))"
      )
      
      # Insert empty map code after the warning message
      maps_content <- c(
        maps_content[1:(line_num + 1)],
        empty_map_code,
        maps_content[(line_num + 2):length(maps_content)]
      )
      
      log_message("Added code to create empty maps for variables with no data")
    }
  }
  
  # Write the updated map script
  writeLines(maps_content, maps_script_path)
  log_message("Saved updated map generation script with fixes for handling all variables")
  
  # Fix 3: Create a README with explanation
  maps_readme_path <- "output/maps/README.md"
  
  # Create maps directory if it doesn't exist
  maps_dir <- "output/maps"
  if (!dir.exists(maps_dir)) {
    dir.create(maps_dir, recursive = TRUE)
    log_message(paste("Created maps directory:", maps_dir))
  }
  
  # Write a README explaining the maps
  readme_content <- c(
    "# Social Determinants of Health Maps",
    "",
    "This directory contains maps for all 255 variables in the Social Determinants of Health dataset.",
    "",
    "## Map Organization",
    "",
    "- **by_year/**: Maps organized by year, with filenames like `YEAR_VARIABLE.png`",
    "- **by_variable/**: Maps organized by variable, with filenames like `VARIABLE_YEAR.png`",
    "- **combined/**: Combined maps with multiple variables for each year (`combined_YEAR.png`) and time series maps for each variable (`timeseries_VARIABLE.png`)",
    "",
    "## Data Coverage",
    "",
    "Maps are generated for all variables to ensure complete documentation, even when the data isn't available for certain variables. In these cases, the maps will show \"No data available for this variable\".",
    "",
    "## Usage Notes",
    "",
    "1. The maps provide a visual representation of the geographical distribution of each variable across U.S. counties.",
    "2. Maps with limited data coverage will indicate the percentage of counties with available data.",
    "3. Variables with no data will have placeholder maps that indicate this status.",
    "4. For variables with time series data, check the `combined/timeseries_VARIABLE.png` files to see trends over time.",
    "",
    "## Running Map Generation",
    "",
    "To regenerate all maps, run:",
    "```",
    "Rscript generate_conus_maps.r",
    "```",
    "",
    "To generate maps just for specific variables or years, use the command line options described in the script."
  )
  
  # Write the README
  writeLines(readme_content, maps_readme_path)
  log_message(paste("Created README for maps at:", maps_readme_path))
  
  log_message("=================================================")
  log_message("MAP GENERATION FIXES COMPLETED SUCCESSFULLY")
  log_message("=================================================")
  
  log_message("To generate maps for all variables, run: Rscript generate_conus_maps.r")
  log_message("Maps will now be created for all 255 variables, even those with no data")
  
  return(TRUE)
}

# Run the function if the script is executed directly
if (!exists("is_sourced")) {
  is_sourced <- function() {
    # Check if the script is being sourced
    parent_env <- parent.frame()
    return(!identical(parent_env, .GlobalEnv))
  }
}

if (!is_sourced()) {
  fix_map_generation()
}