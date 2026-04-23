# =============================
# Area Combination Generator
# =============================
# 
# Purpose:
#   Reads multiple SGV timeseries files, reshapes them into year × day-of-year
#   matrices (daily) and year × month matrices (monthly average), and exports
#   four output files — each in a version with and without metadata (prefix_id, Year).
#
# Input:
#   - Directory of tab/semicolon-separated timeseries files matching the suffix
#     pattern defined in config$ts_suffix_pattern
#   - Each file must contain at least two columns: `Date` (YYYY-MM-DD) and `Area`
#
# Output (written to config$output_folder):
#   - area_combination.txt              : Daily matrix, no metadata
#   - area_combination_withPrefix.txt   : Daily matrix, with prefix_id and Year
#   - area_combination_month.txt        : Monthly average matrix, no metadata
#   - area_combination_month_withPrefix.txt : Monthly average matrix, with prefix_id and Year
#
# Dependencies:
#   - scripts/utils/helperfunctions.R  → read_data()
#
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
#' Updated: April 2026
#' @author Elena Scholz
#' 
#' 
#' RStudio version: RStudio-2025.05.0-496
#' System: Windows 11
# =============================

rm(list = ls())
renv::activate()
source("scripts/utils/helperfunctions.R")

# ---- Configuration ----
# Central config list — adjust paths and patterns here before running.
config <- list(
  ROOT              = "T:/DLR-DFD/PCA-Analysis/",
  ts_subfolder      = "input/05_timeseries_8247_rm2902/",
  ts_suffix_pattern = "_SGV-timeseries_allDates_rm2902\\.txt$", # used to extract prefix_id
  output_folder     = "CluDat",
  daily_output      = "area_combination.txt",
  daily_output_with_id = "area_combination_withPrefix.txt",
  monthly_output    = "area_combination_month.txt",
  monthly_output_with_id = "area_combination_month_withPrefix.txt"
)

# ---- Functions ----

# 1. split_by_year()
# Reshapes a single timeseries dataframe into a matrix of shape [years × days].
# Each row = one year, each column = one observation (day-of-year order).
# Note: assumes equal number of observations per year (warns via workflow check).
split_by_year <- function(df) {
  df$Date <- as.Date(df$Date)
  df$Year <- format(df$Date, "%Y")
  years_list <- split(df$Area, df$Year)
  area_matrix <- do.call(rbind, years_list)
  return(area_matrix)
}

# 2. split_by_month()
# Aggregates a single timeseries to monthly averages, then reshapes to
# a wide dataframe of shape [years × 12 months].
# Uses arithmetic mean (na.rm = TRUE) within each year-month group.
split_by_month <- function(df) {
  df$Date  <- as.Date(df$Date)
  df$Year  <- format(df$Date, "%Y")
  df$Month <- format(df$Date, "%m")
  
  summary_df <- aggregate(Area ~ Year + Month, data = df, FUN = mean, na.rm = TRUE)
  reshaped   <- reshape(summary_df, idvar = "Year", timevar = "Month", direction = "wide")
  colnames(reshaped) <- gsub("Area\\.", "", colnames(reshaped))  # e.g. "Area.01" → "01"
  reshaped   <- reshaped[order(reshaped$Year), ]
  return(reshaped)
}

# 3. process_timeseries_files()
# Applies a split function (split_by_year or split_by_month) to every file
# in files_list, extracts the prefix_id from the filename using suffix_pattern,
# and row-binds all results into one combined dataframe.
#
# Args:
#   files_list     : Named list of dataframes (names = filenames)
#   split_function : Function to apply to each dataframe (split_by_year or split_by_month)
#   suffix_pattern : Regex pattern to remove from filename to get prefix_id
#
# Returns: Combined dataframe with columns [prefix_id, Year, <value columns>]
process_timeseries_files <- function(files_list, split_function, suffix_pattern) {
  result_list <- lapply(names(files_list), function(filename) {
    df        <- files_list[[filename]]
    mat       <- split_function(df)
    result_df <- as.data.frame(mat)
    
    prefix_id        <- gsub(suffix_pattern, "", filename)
    result_df$prefix_id <- prefix_id
    result_df$Year   <- rownames(mat)
    rownames(result_df) <- NULL
    return(result_df)
  })
  
  combined <- do.call(rbind, result_list)
  return(combined)
}

# 4. save_area_combination()
# Writes two versions of a combined dataframe to disk:
#   - Full version (with prefix_id and Year columns)
#   - Clean version (without metadata columns, no column headers)
# Both are space-separated with "." as decimal separator.
#
# Args:
#   df            : Dataframe to save
#   output_folder : Directory to write files into
#   with_id_file  : Filename for the version with metadata
#   no_meta_file  : Filename for the version without metadata
save_area_combination <- function(df, output_folder, with_id_file, no_meta_file) {
  write.table(df,
              file      = file.path(output_folder, with_id_file),
              sep       = " ", dec = ".", row.names = FALSE, col.names = FALSE)
  
  df_no_meta         <- df[, !(names(df) %in% c("prefix_id", "Year"))]
  colnames(df_no_meta) <- NULL
  write.table(df_no_meta,
              file      = file.path(output_folder, no_meta_file),
              sep       = " ", dec = ".", row.names = FALSE)
  
  message("Saved files: ", with_id_file, " and ", no_meta_file)
}

# ---- Workflow ----

# Load all timeseries files from input folder
ts_folder    <- file.path(config$ROOT, config$ts_subfolder)
all_ts_files <- read_data(paste0(ts_folder, "/"), csv_sep = ";", add_ID_from_filename = FALSE)

# Sanity check: all files should have the same number of rows (= same date range)
file_lengths <- sapply(all_ts_files, nrow)
if (length(unique(file_lengths)) != 1) {
  warning("Files have different number of rows — results may be misaligned:")
  print(table(file_lengths))
}

output_folder_path <- paste0(config$ROOT, config$output_folder)

# Daily aggregation: reshape each file into year × day matrix
area_daily <- process_timeseries_files(all_ts_files, split_by_year, config$ts_suffix_pattern)

save_area_combination(area_daily, output_folder_path,
                      config$daily_output_with_id, config$daily_output)

# Monthly aggregation: average within each month, reshape to year × 12 matrix
area_monthly <- process_timeseries_files(all_ts_files, split_by_month, config$ts_suffix_pattern)
month_cols   <- sprintf("%02d", 1:12)  # ensure columns are ordered Jan–Dec
area_monthly <- area_monthly[, c("prefix_id", "Year", month_cols)]
save_area_combination(area_monthly, output_folder_path,
                      config$monthly_output_with_id, config$monthly_output)






