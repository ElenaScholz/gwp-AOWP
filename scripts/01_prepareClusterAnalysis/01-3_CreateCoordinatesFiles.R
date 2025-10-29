## === Setup ===
rm(list = ls())
renv::activate()
#setwd("C:/Users/elena/Documents/RProjects/globalLakeVariability")
source("scripts/utils/helperfunctions.R")

# ---- Configuration ----
config <- list(
  ROOT              = "T:/DLR/PCA-Analysis",
  coord_subfolder   = "Input/00_coordinates_8247",
  ts_subfolder      = "Input/05_timeseries_8247_rm2902",
  coord_suffix      = "_coordinates.txt",
  ts_suffix_pattern = "_SGV-timeseries.*\\.txt$",   # more general than hardcoding "rm2902"
  output_folder     = "ClaDat"
)
output_folder_path <- paste0(config$ROOT, "/", config$output_folder)

## === Functions ===

# Get file prefixes (generalized)
get_prefixes <- function(files, suffix_pattern) {
  gsub(suffix_pattern, "", files)
}

# Find best matches between coordinate and timeseries files
find_matches <- function(coord_prefixes, ts_prefixes) {
  matches <- list()
  for (coord in coord_prefixes) {
    if (coord %in% ts_prefixes) {
      matches[[coord]] <- coord
    } else {
      coord_base <- gsub("_[0-9]+$", "", coord)
      possible_matches <- ts_prefixes[grepl(paste0("^", coord_base, "($|_)"), ts_prefixes)]
      if (length(possible_matches) > 0) {
        matches[[coord]] <- possible_matches[1]
      }
    }
  }
  matches
}

# Read matched coordinate files into one df
read_and_combine_coordinates <- function(matches, coord_folder_path, coord_suffix) {
  all_data_list <- lapply(names(matches), function(coord_prefix) {
    coord_filename <- paste0(coord_prefix, coord_suffix)
    coord_filepath <- file.path(coord_folder_path, coord_filename)
    if (file.exists(coord_filepath)) {
      df <- read.table(coord_filepath, sep = ";", header = TRUE, stringsAsFactors = FALSE)
      df$prefix_id <- coord_prefix
      return(df)
    } else {
      message("File not found: ", coord_filepath)
      return(NULL)
    }
  })
  combined <- do.call(rbind, all_data_list)
  rownames(combined) <- NULL
  combined
}

# Save files (unchanged, but parametric)
save_coordinate_files <- function(combined, output_folder_path) {
  if (!dir.exists(output_folder_path)) dir.create(output_folder_path, recursive = TRUE)
  
  # 1. Save complete
  complete_filepath <- file.path(output_folder_path, "all_coordinates_complete.txt")
  write.table(combined, complete_filepath, sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE)
  
  # 2. Save lat/lon splits (auto-detect)
  lat_cols <- names(combined)[grepl("lat|Lat|LAT|latitude|Latitude", names(combined))]
  lon_cols <- names(combined)[grepl("lon|Lon|LON|longitude|Longitude", names(combined))]
  
  if (length(lat_cols) > 0) {
    write.table(combined[, c("prefix_id", lat_cols), drop = FALSE],
                file.path(output_folder_path, "coordinates_latitude_only.txt"),
                sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE)
  }
  
  if (length(lon_cols) > 0) {
    write.table(combined[, c("prefix_id", lon_cols), drop = FALSE],
                file.path(output_folder_path, "coordinates_longitude_only.txt"),
                sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE)
  }
}


## === Workflow ===

# Define folders
coord_folder <- file.path(config$ROOT, config$coord_subfolder)
ts_folder    <- file.path(config$ROOT, config$ts_subfolder)

# List files
coord_files <- list.files(coord_folder)
ts_files    <- list.files(ts_folder)

# Extract prefixes
coord_prefixes <- get_prefixes(coord_files, config$coord_suffix)
ts_prefixes    <- get_prefixes(ts_files, config$ts_suffix_pattern)

# Match coord ↔ ts
matches <- find_matches(coord_prefixes, ts_prefixes)

# Read and combine coords
all_coords <- read_and_combine_coordinates(matches, coord_folder, config$coord_suffix)

# Save results
save_coordinate_files(all_coords, output_folder_path)


