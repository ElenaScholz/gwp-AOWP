#' Script for checking whether lakes lie in multiple MODIS tiles
#' 
#' 
#' @author Patrick Sogno, Igor Klein
#' 
#' Date: September 23, 2019
#' 
#' 
#' RStudio version: 1.2.1335
#' R version: 3.5.3 
#' 
#' System: Windows 7 Enterprise Version 6.1
#' 
#' 
#' Updated: October 2025
#' @author Elena Scholz
#' 
#' 
#' RStudio version: RStudio-2025.05.0-496
#' System: Windows 11
#'------------------------------------------------------------------------------
#'
#'Workflow & Information:
#'
#' 
#'
#'

###----- 0. Set environment ----------------------------------------------------

# Packages:
#   Load and install function.
rm(list = ls())
renv::activate()
source("scripts/utils/99_TSA_customfunctions.R")


#   List of needed packages.
#packagelist <-as.list(c('stringr'))
#   Call function for each element of list.
#lapply(packagelist, function(x) loadandinstall(x))


config <- list(
  folder_information = list(
    timeseries_input_folder = "T:/DLR-DFD/PCA-Analysis/Input/01_timeseries_8247/",
    timeseries_preliminary_results = "T:/DLR-DFD/PCA-Analysis/Input/02_timeseries_8247_preliminary/",
    output_folder_for_corrected_ts = "T:/DLR-DFD/PCA-Analysis/Input/03_timeseries_8247_cor/"
  ),
  file_pattern = "h",  # pattern for input timeseries files
  sep = ";",
  summary_log_file = "T:/DLR-DFD/PCA-Analysis/Input/00-1_RemoveDuplicates_summary_log.txt",
  
  analysis_information = list(
    start_year = 2003,
    end_year   = 2024
  )
)


# Paths and file names:
#   Set input directory:
ipath <- config$folder_information$timeseries_input_folder
opath <- config$folder_information$output_folder_for_corrected_ts

ifelse(!dir.exists(file.path(config$folder_information$timeseries_preliminary_results)),
       dir.create(file.path(config$folder_information$timeseries_preliminary_results)),
       "Directory Exists")

ifelse(!dir.exists(file.path(opath)),
       dir.create(file.path(opath)),
       "Directory Exists")
       
#   Set pattern of input files:
pattern <- config$file_pattern




sep = config$sep
# Quarantine
jail <- config$folder_information$timeseries_preliminary_results  # Zwischenfolder - die mehrfach da sind ??? 
#"D:/00_GWP/02_Reservoirs/02_TimeSat-Reservoirs/01_Results/Seasonality/JAIL/"

# Define output summary log file
summary_file <- config$summary_log_file
sink(summary_file, split = TRUE)  # Redirect output to both file and console
###----- 1. Input --------------------------------------------------------------
# Get files
fl <- list.files(path = ipath, pattern = pattern)
fl

print(length(fl)) # 8247

# read all files as dataframes  
files <- lapply(fl, function(l) read.delim(paste0(ipath, l), header = T, sep = sep))
#files


# Check if length more than 8037 

add_day    <- 1  
years <- config$analysis_information$start_year :config$analysis_information$end_year

# Leap years are divisible by 4 (but centuries only if divisible by 400)
is_leap <- function(year) {
  (year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0)
}

num_leaps <- sum(is_leap(years))
num_years <- length(years)
expected_length <- num_years * 365 + num_leaps + add_day

expected_length
# 8037

files_processed <- 0
files_with_duplicates <- 0
for (l in 1:length(files)) {
  current_length <- length(files[[l]][[1]])
  
  if (current_length > expected_length) { # 2003-2024: 22 Years with 6 leap years + 1 (as it was before)
    # Count duplicates before removal
    duplicate_count <- sum(duplicated(files[[l]][[1]]))
    
    if (duplicate_count > 0) {
      cat("File", l, "- Found", duplicate_count, "duplicates in", current_length, "entries\n")
      
      # Find and remove duplicates and Na values
      files[[l]][duplicated(files[[l]][[1]]),] <- NA
      files[[l]] <- na.omit(files[[l]])
      
      final_length <- length(files[[l]][[1]])
      cat("  -> Cleaned to", final_length, "entries\n")
      files_with_duplicates <- files_with_duplicates + 1
    } else {
      cat("File", l, "- No duplicates found (", current_length, "entries) but length exceeds threshold\n")
    }
  }
  
  files_processed <- files_processed + 1
  
  # Progress indicator every 10 files
  if (files_processed %% 10 == 0) {
    cat("Processed", files_processed, "of", length(files), "files...\n")
  }
}

# Summary
cat("\n=== SUMMARY ===\n")
cat("Total files processed:", files_processed, "\n")
cat("Files with duplicates found and cleaned:", files_with_duplicates, "\n")
cat("Files exceeding 8037 entries:", sum(sapply(files, function(x) length(x[[1]])) > 8037), "\n")



# rewrite cleaned data to files
for (l in 1:length(files)) {
  write.table(files[[l]], file = paste0(opath, fl[l]), row.names = F,
              quote = F, append = F, sep = sep)
}
fl <- list.files(path = opath, pattern = pattern)  # <- opath statt ipath

#===================== PART 2 and 3  ==============================
# PART 2: Find duplicate Lakes between Modis Tiles
# ==============================================================================
library(stringr)

# Improved extraction of lake identifier from filenames
extract_lake_id <- function(filename) {
  match <- str_match(filename, "^h\\d{2}v\\d{2}_(.*?)_SGV-timeseries")
  if (!is.na(match[2])) {
    return(match[2])
  } else {
    return(NA)
  }
}

lake_ids <- sapply(fl, extract_lake_id)

# Find lakes that are present in multiple tiles
troublemakers <- unique(lake_ids[duplicated(lake_ids)])

# For each troublemaker lake, find all files that belong to it
name_list <- lapply(troublemakers, function(lake_id) {
  fl[which(lake_ids == lake_id)]
})

# Read the data for each troublemaker lake
tr_df <- lapply(name_list, function(file_list) {
  lapply(file_list, function(filename) {
    read.delim(paste0(opath, filename), header = TRUE, sep = sep)
  })
})

# Use troublemakers as your lake identifier list for output
tr_lakes <- troublemakers


#===================== PART 4 ==============================
# Clean duplicates in lakes that are within two tiles
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. REMOVE DUPLICATES FROM LAKE DATA
# ------------------------------------------------------------------------------

cat("=== DUPLICATE REMOVAL ===\n")

for (l in 1:length(tr_df)) {
  for (i in 1:length(tr_df[[l]])) {
    current_length <- length(tr_df[[l]][[i]][[1]])
    
    if (current_length > 8037) {
      cat("Lake", l, "File", i, ": Too many entries (", current_length, ")\n")
      
      # Check for duplicates
      duplicates <- duplicated(tr_df[[l]][[i]][[1]])
      if (any(duplicates)) {
        cat("  Found duplicates:\n")
        print(tr_df[[l]][[i]][duplicates, ])
        
        # Remove duplicates
        tr_df[[l]][[i]] <- tr_df[[l]][[i]][!duplicates, ]
        new_length <- length(tr_df[[l]][[i]][[1]])
        cat("  Duplicates removed. New length:", new_length, "\n")
      }
    }
  }
}

# ------------------------------------------------------------------------------
# 2. IDENTIFY LAKES WITH DIFFERENT ROW COUNTS
# ------------------------------------------------------------------------------

cat("\n=== ROW COUNT ANALYSIS ===\n")

# Check row counts for each lake
for (l in 1:length(tr_df)) {
  cat("Lake", l, ":\n")
  for (i in 1:length(tr_df[[l]])) {
    row_count <- length(tr_df[[l]][[i]][[1]])
    cat("  File", i, "has", row_count, "rows\n")
  }
  cat("\n")
}

# Find lakes with different row counts
lakes_different_length <- c()
for (l in 1:length(tr_df)) {
  row_counts <- sapply(tr_df[[l]], function(x) length(x[[1]]))
  
  if (length(unique(row_counts)) > 1) {
    cat("Lake", l, "has different row counts:", paste(row_counts, collapse = ", "), "\n")
    lakes_different_length <- c(lakes_different_length, l)
  }
}

# ------------------------------------------------------------------------------
# 3. ANALYZE DATE RANGES FOR PROBLEMATIC LAKES
# ------------------------------------------------------------------------------

if (length(lakes_different_length) > 0) {
  cat("\n=== DATE RANGE ANALYSIS ===\n")
  
  for (l in lakes_different_length) {
    cat("Lake", l, ":\n")
    
    all_dates <- list()
    for (i in 1:length(tr_df[[l]])) {
      dates <- tr_df[[l]][[i]][[1]]  # Assuming first column contains dates
      all_dates[[i]] <- dates
      
      cat("  File", i, ": ", length(dates), "entries from", 
          as.character(dates[1]), "to", as.character(dates[length(dates)]), "\n")
    }
    
    # Find missing dates between files (for 2-file comparison)
    if (length(all_dates) == 2) {
      missing_in_1 <- setdiff(all_dates[[2]], all_dates[[1]])
      missing_in_2 <- setdiff(all_dates[[1]], all_dates[[2]])
      
      if (length(missing_in_1) > 0) {
        cat("  Missing in File 1:", length(missing_in_1), "data points\n")
        cat("    Examples:", paste(head(as.character(missing_in_1), 3), collapse = ", "), "\n")
      }
      
      if (length(missing_in_2) > 0) {
        cat("  Missing in File 2:", length(missing_in_2), "data points\n")
        cat("    Examples:", paste(head(as.character(missing_in_2), 3), collapse = ", "), "\n")
      }
    }
    cat("\n")
  }
}

# ------------------------------------------------------------------------------
# 4. HARMONIZE LAKES WITH DIFFERENT LENGTHS
# ------------------------------------------------------------------------------

harmonize_lake_data <- function(lake_index) {
  cat("=== Harmonizing Lake", lake_index, "===\n")
  
  num_files <- length(tr_df[[lake_index]])
  cat("Number of files:", num_files, "\n")
  
  # Show initial state
  for (i in 1:num_files) {
    cat("File", i, ":", length(tr_df[[lake_index]][[i]][[1]]), "rows\n")
  }
  
  # Find common dates across ALL files
  all_dates <- lapply(tr_df[[lake_index]], function(df) df[[1]])
  common_dates <- all_dates[[1]]
  
  for (i in 2:length(all_dates)) {
    common_dates <- intersect(common_dates, all_dates[[i]])
  }
  
  cat("Common data points across all", num_files, "files:", length(common_dates), "\n")
  
  # Filter and sort all DataFrames to common dates
  filtered_dfs <- list()
  for (i in 1:num_files) {
    df_filtered <- tr_df[[lake_index]][[i]][tr_df[[lake_index]][[i]][[1]] %in% common_dates, ]
    df_filtered <- df_filtered[order(df_filtered[[1]]), ]
    filtered_dfs[[i]] <- df_filtered
  }
  
  # Verify harmonization
  final_lengths <- sapply(filtered_dfs, function(df) length(df[[1]]))
  
  if (length(unique(final_lengths)) == 1) {
    # Update original data
    for (i in 1:num_files) {
      tr_df[[lake_index]][[i]] <<- filtered_dfs[[i]]
    }
    cat("Successfully harmonized to", final_lengths[1], "rows for all files\n")
    return(TRUE)
  } else {
    cat("ERROR: Harmonization failed! Final lengths:", paste(final_lengths, collapse = ", "), "\n")
    return(FALSE)
  }
}

# Apply harmonization to problematic lakes
if (length(lakes_different_length) > 0) {
  cat("\n=== HARMONIZATION PROCESS ===\n")
  
  for (l in lakes_different_length) {
    if (length(tr_df[[l]]) >= 2) {
      harmonize_lake_data(l)
      cat("\n")
    }
  }
}


# ------------------------------------------------------------------------------
# 5. FINAL VALIDATION
# ------------------------------------------------------------------------------

cat("=== FINAL VALIDATION ===\n")
validation_passed <- TRUE

for (l in 1:length(tr_df)) {
  row_counts <- sapply(tr_df[[l]], function(x) length(x[[1]]))
  
  if (length(unique(row_counts)) > 1) {
    cat("WARNING: Lake", l, "still has different row counts:", paste(row_counts, collapse = ", "), "\n")
    validation_passed <- FALSE
  }
}

if (validation_passed) {
  cat("All lakes successfully validated!\n")
} else {
  cat("Some lakes still need attention.\n")
}

# ------------------------------------------------------------------------------
# 6. APPLY AddRowVal FUNCTION
# ------------------------------------------------------------------------------

cat("\n=== APPLYING AddRowVal ===\n")

tr_add <- list()
for (l in 1:length(tr_df)) {
  tr_add[[l]] <- AddRowVal(tr_df[[l]])
  cat("Lake", l, ": AddRowVal applied\n")
}

# ------------------------------------------------------------------------------
# 7. FILE MANAGEMENT - MOVE OLD FILES TO JAIL
# ------------------------------------------------------------------------------

cat("\n=== FILE MANAGEMENT ===\n")

# Create jail directory if it doesn't exist
if (!dir.exists(jail)) {
  dir.create(jail)
  cat("Created jail directory:", jail, "\n")
}

# Move old files to jail
lapply(name_list, function(l) {
  lapply(l, function(x) {
    old_path <- paste0(opath, x) 
    #old_path <- paste0(ipath, x)
    new_path <- paste0(jail, x)
    
    if (file.exists(old_path)) {
      file.rename(old_path, new_path)
      cat("Moved", x, "to jail\n")
    }
  })
})

# ------------------------------------------------------------------------------
# 8. EXTRACT TILES AND WRITE OUTPUT FILES
# ------------------------------------------------------------------------------

extract_tile_from_filename <- function(filename) {
  # Try to extract hXXvXX pattern
  tile_match <- str_extract(filename, "h\\d{2}v\\d{2}")
  
  if (!is.na(tile_match)) {
    return(tile_match)
  }
  
  # Alternative method: check if filename starts with tile pattern
  if (str_detect(filename, "^h\\d{2}v\\d{2}")) {
    return(str_extract(filename, "^h\\d{2}v\\d{2}"))
  }
  
  return(NA)
}

cat("\n=== WRITING OUTPUT FILES ===\n")

for (l in 1:length(tr_lakes)) {
  cat("Processing Lake", l, "(", tr_lakes[[l]], ")\n")
  cat("Files:", paste(name_list[[l]], collapse = ", "), "\n")
  
  # Extract tiles from all filenames
  tiles <- c()
  for (filename in name_list[[l]]) {
    tile <- extract_tile_from_filename(filename)
    if (!is.na(tile)) {
      tiles <- c(tiles, tile)
    } else {
      cat("WARNING: No tile pattern found in:", filename, "\n")
    }
  }
  
  # Get unique tiles and choose naming strategy
  unique_tiles <- unique(tiles)
  cat("Found tiles:", paste(unique_tiles, collapse = ", "), "\n")
  
  if (length(unique_tiles) > 0) {
    # Use first tile for filename (or combine multiple tiles if needed)
    chosen_tile <- unique_tiles[1]
    # Alternative: chosen_tile <- paste(unique_tiles, collapse = "AND")
    
    output_filename <- paste0(opath, chosen_tile, "_", tr_lakes[[l]], "_SGV-timeseries.txt")
  } else {
    # Fallback naming
    cat("ERROR: No tiles found. Using fallback name.\n")
    output_filename <- paste0(opath, "UNKNOWN_", tr_lakes[[l]], "_SGV-timeseries.txt")
  }
  
  # Write output file
  write.table(tr_add[[l]], output_filename,
              row.names = FALSE, quote = FALSE, append = FALSE, sep = sep)
  
  cat("File written:", basename(output_filename), "\n\n")
}

cat("=== PROCESSING COMPLETE ===\n")

sink()
