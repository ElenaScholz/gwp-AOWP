# =============================
# Area combination generator
# =============================
rm(list = ls())
renv::activate()
library(dplyr)
source("scripts/utils/helperfunctions.R")  # for read_data()

# ---- Configuration ----
config <- list(
  ROOT              = "T:/DLR/PCA-Analysis/",
  ts_subfolder      = "input/05_timeseries_8247_rm2902/",
  ts_suffix_pattern = "_SGV-timeseries_allDates_rm2902\\.txt$", # used to extract prefix_id
  output_folder     = "ClaDat",
  daily_output      = "area_combination.txt",
  daily_output_with_id = "area_combination_withPrefix.txt",
  monthly_output    = "area_combination_month.txt",
  monthly_output_with_id = "area_combination_month_withPrefix.txt"
) 

# ---- Functions ----

# 1. Split timeseries by year
split_by_year <- function(df) {
  df$Date <- as.Date(df$Date)
  df$Year <- format(df$Date, "%Y")
  years_list <- split(df$Area, df$Year)
  area_matrix <- do.call(rbind, years_list)
  return(area_matrix)
}

# 2. Split timeseries by month (average within month)
split_by_month <- function(df) {
  df$Date <- as.Date(df$Date)
  df$Year <- format(df$Date, "%Y")
  df$Month <- format(df$Date, "%m")
  
  summary_df <- aggregate(Area ~ Year + Month, data = df, FUN = mean, na.rm = TRUE)
  reshaped <- reshape(summary_df, idvar = "Year", timevar = "Month", direction = "wide")
  colnames(reshaped) <- gsub("Area\\.", "", colnames(reshaped))  # clean colnames
  reshaped <- reshaped[order(reshaped$Year), ]
  return(reshaped)
}

# 3. Process all files and combine
process_timeseries_files <- function(files_list, split_function, suffix_pattern) {
  result_list <- lapply(names(files_list), function(filename) {
    df <- files_list[[filename]]
    mat <- split_function(df)
    result_df <- as.data.frame(mat)
    
    # Extract prefix_id
    prefix_id <- gsub(suffix_pattern, "", filename)
    result_df$prefix_id <- prefix_id
    result_df$Year <- rownames(mat)
    rownames(result_df) <- NULL
    return(result_df)
  })
  
  combined <- do.call(rbind, result_list)
  return(combined)
}

# 4. Save both versions (with and without metadata)
save_area_combination <- function(df, output_folder, with_id_file, no_meta_file) {
  # Save with prefix_id
  write.table(df,
              file = file.path(output_folder, with_id_file),
              sep = " ", dec = ".", row.names = FALSE, col.names = FALSE)
  
  # Save without metadata (remove prefix_id and Year)
  df_no_meta <- df[, !(names(df) %in% c("prefix_id", "Year"))]
  colnames(df_no_meta) <- NULL
  write.table(df_no_meta,
              file = file.path(output_folder, no_meta_file),
              sep = " ", dec = ".", row.names = FALSE)
  
  message("Saved files: ", with_id_file, " and ", no_meta_file)
}

# ---- Workflow ----

# Load all timeseries
ts_folder <- file.path(config$ROOT, config$ts_subfolder)
all_ts_files <- read_data(paste0(ts_folder,"/"), csv_sep = ";", add_ID_from_filename = FALSE)

# Optional: check file lengths
file_lengths <- sapply(all_ts_files, nrow)
if(length(unique(file_lengths)) != 1) {
  warning("Files have different number of rows:")
    print(table(file_lengths))
}

# ---- Daily aggregation ----
output_folder_path = paste0(config$ROOT, config$output_folder)
area_daily <- process_timeseries_files(all_ts_files, split_by_year, config$ts_suffix_pattern)
save_area_combination(area_daily, output_folder_path,
                      config$daily_output_with_id, config$daily_output)

# ---- Monthly aggregation ----
area_monthly <- process_timeseries_files(all_ts_files, split_by_month, config$ts_suffix_pattern)

# Sort columns: prefix_id, Year, Jan–Dec
month_cols <- sprintf("%02d", 1:12)
area_monthly <- area_monthly[, c("prefix_id", "Year", month_cols)]
save_area_combination(area_monthly, output_folder_path,
                      config$monthly_output_with_id, config$monthly_output)


