#!/usr/bin/env Rscript

# This script cleans up the unsafe is_sourced pattern in all fetch files

# Get list of files to fix
files <- list.files("/Users/davidlary/Dropbox/Environments/Code/GetData/US-SocialDeterminantsOfHealth/R/", 
                   pattern = "fetch_.*\\.r$", 
                   full.names = TRUE)

# Add build_extended_crosswalk_v2.r
files <- c(files, "/Users/davidlary/Dropbox/Environments/Code/GetData/US-SocialDeterminantsOfHealth/R/build_extended_crosswalk_v2.r")

# Function to fix a file
fix_file <- function(file_path) {
  # Read the file content
  if (!file.exists(file_path)) {
    cat(paste("File not found:", file_path, "\n"))
    return(invisible(NULL))
  }
  
  content <- readLines(file_path, warn = FALSE)
  
  # First look for the corrupted pattern with duplicated text
  corrupted_pattern <- "is_interactive_run <- !exists\\(\"is_sourced\"\\) \\|\\| \\(is\\.logical\\(is_sourced\\) is_interactive_run <- !exists\\(\"is_sourced\"\\) \\|\\| \\(exists\\(\"is_sourced\"\\) && !is_sourced\\(\\)\\)is_interactive_run <- !exists\\(\"is_sourced\"\\) \\|\\| \\(exists\\(\"is_sourced\"\\) && !is_sourced\\(\\)\\) !is_sourced\\)"
  
  # Check for the corrupted pattern
  corrupted_lines <- grep(corrupted_pattern, content)
  
  if (length(corrupted_lines) > 0) {
    cat(paste("Fixing corrupted pattern in:", file_path, "\n"))
    # Replace corrupted pattern with the safe pattern
    content[corrupted_lines] <- "    is_interactive_run <- !exists(\"is_sourced\") || (is.logical(is_sourced) && !is_sourced)"
    # Write back the fixed content
    writeLines(content, file_path)
    return(invisible(NULL))
  }
  
  # Check for the unsafe pattern with is_sourced()
  unsafe_pattern <- "is_interactive_run <- !exists\\(\"is_sourced\"\\) \\|\\| \\(exists\\(\"is_sourced\"\\) && !is_sourced\\(\\)\\)"
  
  # Find lines with the unsafe pattern
  unsafe_lines <- grep(unsafe_pattern, content)
  
  if (length(unsafe_lines) > 0) {
    cat(paste("Fixing unsafe is_sourced pattern in:", file_path, "\n"))
    # Replace the unsafe pattern with the safe pattern
    content[unsafe_lines] <- gsub(
      "is_interactive_run <- !exists\\(\"is_sourced\"\\) \\|\\| \\(exists\\(\"is_sourced\"\\) && !is_sourced\\(\\)\\)",
      "is_interactive_run <- !exists(\"is_sourced\") || (is.logical(is_sourced) && !is_sourced)",
      content[unsafe_lines]
    )
    # Write back the fixed content
    writeLines(content, file_path)
  } else {
    cat(paste("No unsafe patterns found in:", file_path, "\n"))
  }
  
  return(invisible(NULL))
}

# Fix each file
for (file in files) {
  fix_file(file)
}

cat("Clean-up complete!\n")