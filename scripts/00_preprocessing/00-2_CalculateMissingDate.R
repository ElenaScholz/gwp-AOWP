# =============================
# Fill missing dates in timeseries
# =============================

rm(list = ls())
renv::activate()
library(tidyverse)
library(tools)   # for file_path_sans_ext
#source("R/Code/helperfunctions.R")  # for read_data() and save_datasets()
source("scripts/utils/helperfunctions.R")
# ---- Configuration ----
config <- list(
  input_folder   = "T:/DLR/PCA-Analysis/Input/03_timeseries_8247_cor/",
  output_folder  = "T:/DLR/PCA-Analysis/Input/04_timeseries_8247_allDates",
  start_date     = "2003-01-01",
  end_date       = "2024-12-31",
  output_suffix  = "_allDates",
  csv_sep        = ";",
  file_format    = "txt"
)

# ---- Functions ----

# 1. Fill missing dates for a single dataframe
fill_missing_dates <- function(df, full_dates) {
  df$Date <- as.Date(df$Date)
  
  if(!all(full_dates %in% df$Date)) {
    df_complete <- df %>%
      complete(Date = full_dates) %>%
      arrange(Date) %>%
      mutate(
        Area = ifelse(is.na(Area),
                      (lag(Area) + lead(Area)) / 2,
                      Area)
      )
    return(df_complete)
  } else {
    return(df)
  }
}

# 2. Process all files in a folder
process_all_files <- function(input_folder, full_dates, csv_sep = ";") {
  all_files <- read_data(input_folder, csv_sep = csv_sep, add_ID_from_filename = FALSE)
  
  processed <- lapply(names(all_files), function(fname) {
    df <- all_files[[fname]]
    fill_missing_dates(df, full_dates)
  })
  
  # Keep names without file extensions
  names(processed) <- file_path_sans_ext(names(all_files))
  
  return(processed)
}

# ---- Workflow ----

# Generate full date sequence
full_date_seq <- seq(from = as.Date(config$start_date),
                     to   = as.Date(config$end_date),
                     by   = "day")

# Process all timeseries files
processed_files <- process_all_files(config$input_folder, full_date_seq, csv_sep = config$csv_sep)

# Save processed datasets
save_datasets(processed_files,
              directory = config$output_folder,
              name_extension = config$output_suffix,
              file_format = config$file_format,
              separator = config$csv_sep)

message("All timeseries processed and saved to: ", config$output_folder)

