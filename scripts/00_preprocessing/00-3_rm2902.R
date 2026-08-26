# =============================
# Remove Feb 29 from leap years
# =============================
rm(list = ls())
renv::activate()
library(tidyverse)
library(tools)          # for file_path_sans_ext
source('scripts/utils/helperfunctions.R')  # read_data() & save_datasets()

# ---- Configuration ----
config <- list(
  # EDIT THIS: set to your local project root
  ROOT           = "path-to-your-gwpAOWP-folder/",
  input_folder   = "Input/04_timeseries_8247_allDates/",
  output_folder  = "Input/05_timeseries_8247_rm2902",
  output_suffix  = "_rm2902",
  csv_sep        = ";",
  file_format    = "txt"
)

config$input_folder  <- paste0(config$ROOT, config$input_folder)
config$output_folder <- paste0(config$ROOT, config$output_folder)

# ---- Functions ----

# Remove all Feb 29 from a single dataframe
remove_feb29 <- function(df, date_col = "Date") {
  df[[date_col]] <- as.Date(df[[date_col]])
  df_filtered <- df[format(df[[date_col]], "%m-%d") != "02-29", ]
  return(df_filtered)
}

# Apply to all files in a folder
remove_feb29_from_folder <- function(input_folder, csv_sep = ";") {
  all_files <- read_data(input_folder, csv_sep = csv_sep, add_ID_from_filename = FALSE)
  
  processed <- lapply(names(all_files), function(fname) {
    df <- all_files[[fname]]
    remove_feb29(df)
  })
  
  names(processed) <- file_path_sans_ext(names(all_files))
  return(processed)
}

# ---- Workflow ----

processed_files <- remove_feb29_from_folder(config$input_folder, config$csv_sep)

# Save processed datasets
save_datasets(processed_files,
              directory = config$output_folder,
              name_extension = config$output_suffix,
              sep = config$csv_sep,
              file_format = config$file_format)

message("All timeseries processed and Feb 29 removed. Saved to: ", config$output_folder)
