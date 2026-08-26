
#' Check if a directory path is valid
#'
#' This function verifies that the input directory path is a valid character string and exists in the filesystem.
#'
#' @param dir A character string representing the path to the directory.
#' @return Throws an error if the directory does not exist or if the input is not a character string.

check_path <- function(dir) {
  if (!is.character(dir)) {
    stop("The directory path should be a string.")
  }
  if (!dir.exists(dir)) {
    stop(paste("The directory", dir, "does not exist."))
  }
}

#' Check if a file path is valid
#'
#' This function verifies that the input file path is a valid character string and exists in the filesystem.
#'
#' @param file A character string representing the path to the file.
#' @return Throws an error if the file does not exist or if the input is not a character string.

check_file <- function(file) {
  if (!is.character(file)) {
    stop("The file path should be a string.")
  }
  if (!file.exists(file)) {
    stop(paste("The file", file, "does not exist."))
  }
}

#' Check file format and load data
#'
#' This function reads a dataset from a CSV, TXT, or XLSX file, with options to specify skipped lines and separator for CSV/TXT.
#'
#' @param input_dataset A character string representing the path to the dataset file (CSV, TXT, or XLSX).
#' @param skip_lines An integer indicating the number of lines to skip (for CSV/TXT).
#' @param csv_sep A character string for the CSV/TXT separator. Defaults to ",".
#' @param comment.char A character to specify the comment character in the CSV/TXT. Defaults to '#'.
#' @return A dataframe loaded from the specified file.

check_format <- function(input_dataset, skip_lines, csv_sep = ",", comment.char = '#') {
  extension <- tools::file_ext(input_dataset)
  
  if (extension %in% c("csv", "txt")) {
    dataframe <- utils::read.csv(input_dataset, skip = skip_lines, sep = csv_sep, comment.char = comment.char, header = TRUE)
  } else if (extension == "xlsx") {
    dataframe <- readxl::read_excel(input_dataset)
  } else {
    stop("Unsupported file format. Please provide a CSV, TXT, or XLSX file.")
  }
  
  return(dataframe)
}

#' read_data
#'
#' Reads in all files from a specified directory and returns dataframes. 
#' The function can handle single or multiple files and offers customization through optional parameters.
#'
#' @param input_directory **MANDATORY** The directory where time series files are stored. All files should have the same structure.
#' @param file_pattern **OPTIONAL** A file pattern (e.g., "_daily") to filter files in the directory. Default is NULL (reads all files).
#' @param skip_lines **OPTIONAL** Number of lines to skip before the actual data starts. Default is 0.
#' @param csv_sep **OPTIONAL** Separator used in CSV/TXT files (e.g., ",", ";", "\t"). Default is ",".
#' @param csv_comment_character **OPTIONAL** Character used to denote comments in the CSV/TXT file. Default is '#'.
#' @param add_ID_from_filename **OPTIONAL** Boolean indicating whether to add an ID from the filename. Default is TRUE.
#' @param index_id **OPTIONAL** Vector defining the start and end characters for extracting ID from filenames. Default is c(0, 6).
#'
#' @return A dataframe if only one file is found; a list of dataframes if multiple files are found.
#'

read_data <- function(input_directory, file_pattern = NULL, skip_lines = 0, csv_sep = ",", csv_comment_character = '#', add_ID_from_filename = TRUE, index_id = c(0, 6)) {
  
  if (is.null(file_pattern)){
    files <- list.files(input_directory)
  }else {
    pattern = file_pattern
    files <- list.files(input_directory, pattern = pattern)
    
  }
  
  
  check_path(input_directory)
  
  logger_dataframes <- list()
  # # 
  if (length(files) == 1) {
    # check_file(paste0(input_directory, files))
    dataframe <- check_format(paste0(input_directory, files), skip_lines = skip_lines, csv_sep = csv_sep, comment.char = csv_comment_character)
    if (add_ID_from_filename) {
      id_logger <- substr(files, index_id[1], index_id[2])
      dataframe$Logger_ID <- id_logger
    }
    return(dataframe)
  } else {
    for (i in files) {
      # check_file(paste0(input_directory, i))
      dataframe <- check_format(paste0(input_directory, i), skip_lines = skip_lines, csv_sep = csv_sep, comment.char = csv_comment_character)
      if (add_ID_from_filename) {
        id_logger <- substr(i, index_id[1], index_id[2])
        dataframe$Logger_ID <- id_logger
      }
      logger_dataframes[[i]] <- dataframe
    }
    return(logger_dataframes)
  }
}

#' save_datasets
#'
#' Function to save datasets. It checks for empty dataframes and skips them.
#' Can save as CSV or TXT format depending on user input.
#'
#' @param data **MANDATORY** the list of datasets you want to save
#' @param directory **MANDATORY** the output directory
#' @param name_extension **MANDATORY** the extension for the saved file e.g. "daily_" or "_monthly"
#' @param file_format **OPTIONAL** the file format to save ("csv" or "txt"). Default is "csv".
#' @param separator **OPTIONAL** the separator to use for CSV/TXT files. Default is "," for CSV and "\t" for TXT.
#' 

save_datasets <- function(data, directory, name_extension, file_format = "csv", separator = NULL) {
  
  # Validate file format
  if (!file_format %in% c("csv", "txt")) {
    stop("file_format must be either 'csv' or 'txt'")
  }
  
  # Set default separator based on format if not provided
  if (is.null(separator)) {
    separator <- ifelse(file_format == "csv", ",", "\t")
  }
  
  # Create directory if it doesn't exist
  if (!dir.exists(directory)) {
    dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  }
  
  for (i in seq_along(data)) {
    df <- data[[i]]
    
    # Check if the dataframe is empty
    if (nrow(df) > 0) {
      file_name <- file.path(directory, paste0(names(data)[i], name_extension, ".", file_format))
      
      if (file_format == "csv") {
        write.csv(df, file = file_name, row.names = FALSE)
      } else if (file_format == "txt") {
        write.table(df, file = file_name, row.names = FALSE, sep = separator, quote = FALSE)
      }
      
      message(paste("Saved:", file_name))
    } else {
      message(paste("Skipping empty dataframe for:", names(data)[i]))
    }
  }
}

#' Check and Clean Invalid Data Records
#'
#' This function identifies and corrects invalid values in a raw data matrix or
#' data frame containing GWP lake extent data.  
#' It replaces negative values, `"NA"` strings, and actual `NA` entries with `0`.
#' After cleaning, the function reports the number of records corrected and
#' ensures the returned dataset has no missing or negative values.
#'
#' @param raw_data_full A numeric matrix or data frame containing raw values.
#'   May include negative numbers, `"NA"` strings, or missing values (`NA`).
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Detects and replaces negative values with `0`.
#'   \item Detects and replaces `"NA"` string entries with `0`.
#'   \item Detects and replaces actual `NA` values with `0`.
#'   \item Prints a summary of how many corrections were applied.
#'   \item Prints a final verification of remaining invalid values.
#' }
#'
#' @return A cleaned numeric matrix or data frame with the same dimensions
#'   as the input, where all invalid values have been replaced by `0`.
#'

check_for_invalid_data <- function(raw_data_full) {
  ## identify bad data records
  
  # Handle negative values
  neg_recs <- which(raw_data_full < 0, arr.ind = TRUE)
  print(paste("Found",
              length(which(apply(raw_data_full, 1, min, na.rm = TRUE) < 0)),
              "rows with negative lake extent which were set to 0!"))
  raw_data_full[neg_recs] <- 0
  
  # Handle "NA" strings
  na_string_recs <- which(raw_data_full == "NA", arr.ind = TRUE)
  print(paste("Found", length(na_string_recs),
              "NA string records which were set to 0!"))
  raw_data_full[na_string_recs] <- 0
  
  # Handle actual NA values
  actual_na_recs <- which(is.na(raw_data_full), arr.ind = TRUE)
  print(paste("Found", length(actual_na_recs),
              "actual NA records which were set to 0!"))
  raw_data_full[actual_na_recs] <- 0
  
  # Verify your data is clean
  print(paste("Remaining NA values:", sum(is.na(raw_data_full))))
  print(paste("Remaining negative values:", sum(raw_data_full < 0, na.rm = TRUE)))
  
  return(raw_data_full)
}


#' Normalize Data by Row
#'
#' This function scales each row of a numeric matrix or data frame by dividing
#' all values in that row by the row's maximum value. This results in each row
#' being normalized to a maximum of 1.
#'
#' @param raw_data A numeric matrix or data frame. Rows typically represent
#'   observations (e.g., lakes, years) and columns represent measurements
#'   (e.g., time steps).
#'
#' @details
#' For each row:
#' \enumerate{
#'   \item Compute the maximum value of the row.
#'   \item Divide all entries in that row by this maximum value.
#'   \item If the row maximum equals 0, the entire row is set to 0 (to avoid
#'         division by zero).
#' }
#'
#' **Important:** If the input contains `NA` values, `max()` will return `NA`,
#' which may cause an error when evaluating the `if` condition. To handle such
#' cases safely, consider pre-cleaning the data (e.g., with
#' \code{check_for_invalid_data()}) or adding `na.rm = TRUE` to the
#' \code{max()} calls.
#'
#' @return A numeric matrix of the same dimensions as the input, where each row
#'   is normalized such that its maximum value is 1 (unless the row maximum was
#'   0, in which case all entries are 0).
#'

normalize_data <- function(raw_data){
  out_dat_byYear <- matrix(NA, ncol = ncol(raw_data), nrow = nrow(raw_data))
  
  for(i in 1:nrow(raw_data)){
    out_dat_byYear[i,] <- raw_data[i,] / max(raw_data[i,])
    
    if(max(raw_data[i,])==0){out_dat_byYear[i,] <- 0}
  }
  
  return(out_dat_byYear)
}


