
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


#' Load Input Matrix
#' 
#' Loads and converts a data file into a numeric matrix for cluster analysis.
#' 
#' @param path Character string specifying the file path to the input data file
#' @return A numeric matrix with observations as rows and features as columns
load_input_matrix <- function(path, sep) {
  as.matrix(read.table(file = path, header = FALSE, sep = sep, dec = "."))
}

#' Run K-means Clustering Evaluation
#' 
#' Performs K-means clustering for a specified number of clusters and calculates
#' multiple clustering quality metrics including WSS, explained variance, 
#' silhouette index, and Davies-Bouldin index.
#' 
#' @param CLA_DAT Numeric matrix containing the input data for clustering 
#'   (observations x features)
#' @param k Integer specifying the number of clusters to create
#' @param iter.max Integer specifying the maximum number of iterations allowed 
#'   for the K-means algorithm
#' @param nstart Integer specifying the number of random starts for K-means 
#'   (helps find global optimum)
#' @param folder_cnt Character string specifying the path to folder containing 
#'   cluster center files (format: "k.cnt")
#' @param folder_cla Character string specifying the path to folder containing 
#'   cluster assignment files (format: "k.cla")
#' 
#' @return A list containing:
#'   \item{cluster_assignments}{Integer vector of cluster assignments for each observation}
#'   \item{wss}{Numeric value of total within-cluster sum of squares}
#'   \item{exvs}{Numeric value of explained variance as percentage}
#'   \item{fsil}{Numeric value of Faster Silhouette Index score}
#'   \item{db}{Numeric value of Davies-Bouldin Index score}
#' 
#' @details The function reads pre-computed cluster centers from .cnt files and 
#' uses them as starting points for K-means. Multiple clustering validation 
#' metrics are calculated: lower Davies-Bouldin scores and higher Silhouette 
#' scores indicate better clustering.

run_kmeans_evaluation <- function(CLA_DAT, k, iter.max, nstart, folder_cnt, folder_cla) {
  cnts <- read.table(paste0(folder_cnt, k, ".cnt"))
  clas <- read.table(paste0(folder_cla, k, ".cla"))
  
  kmn_res <- kmeans(CLA_DAT, centers = t(cnts), iter.max = iter.max, nstart = nstart)
  cluster_assignment <- kmn_res$cluster
  # cluster_assignment = clas[,2]
  list(
    cluster_assignments = cluster_assignment, #ACHTUNG - Hier nur der schritt gewählt, weil noch keine echte cluster vorhanden
    wss = sum(kmn_res$withinss),
    exvs = kmn_res$betweenss / kmn_res$totss * 100,
    fsil = calc_FSIL_faster(mat=CLA_DAT, cla=cluster_assignment, cnt=cnts),
    db = index.DB(CLA_DAT, cl=cluster_assignment)$DB
  )
}

#' Compute Krzanowski-Lai Index
#' 
#' Computes the Krzanowski-Lai (KL) index for determining the optimal number 
#' of clusters across a range of K values. The KL index helps identify the 
#' best clustering solution by comparing the quality of clustering for 
#' consecutive values of K.
#' 
#' @param CLA_DAT Numeric matrix containing the input data used for clustering
#' @param clas_all Numeric matrix where each column contains cluster assignments 
#'   for different K values
#' 
#' @return Numeric vector of KL index values for each K. Returns NaN for edge 
#'   cases where calculation isn't possible (first two and last positions).
#' 
#' @details The KL index is calculated for K values from 3 to (ncol(clas_all)-1).
#' It requires cluster assignments for K-1, K, and K+1 to calculate the index 
#' for K. Higher KL values or local maxima may indicate better clustering solutions.
#' 
#' @examples
#' kl_scores <- compute_KL(data_matrix, cluster_assignments_matrix)
compute_KL <- function(CLA_DAT, clas_all) {
  kl <- rep(NaN, ncol(clas_all))
  for(i in 3:(ncol(clas_all)-1)) {
    kl[i] <- index.KL(CLA_DAT, clall=clas_all[,(i-1):(i+1)])
  }
  kl
}

#' Plot Cluster Metrics
#' 
#' Creates a comprehensive 2x2 grid visualization of clustering quality metrics
#' across different numbers of clusters. Saves the plot as a PDF file for 
#' evaluation of optimal cluster numbers.
#' 
#' @param exvs Numeric vector of explained variance percentages for each K
#' @param fsil Numeric vector of Faster Silhouette Index scores for each K
#' @param db Numeric vector of Davies-Bouldin Index scores for each K
#' @param kl Numeric vector of Krzanowski-Lai index scores for each K
#' @param filename Character string specifying the output PDF filename
#' 
#' @return No return value. Creates a PDF file as a side effect.
#' 
#' @details Creates four line plots showing:
#' \itemize{
#'   \item Explained Variance: Higher values indicate better cluster separation
#'   \item Silhouette Index: Values closer to 1 indicate better clustering
#'   \item Davies-Bouldin Index: Values closer to 0 indicate better clustering  
#'   \item Krzanowski-Lai Index: Local maxima may indicate optimal K values
#' }
#' The plot dimensions are set to 10x10 inches and points are connected with lines.

plot_cluster_metrics <- function(exvs, fsil, db, kl, filename) {
  pdf(filename, width=10, height=10)
  par(mfrow=c(2,2))
  plot(1:length(exvs), exvs, type="b", xlab="Number of Clusters",
       ylab="Explained Variance [%]", main="Explained Variance")
  plot(1:length(fsil), fsil, type="b", xlab="Number of Clusters",
       ylab="Faster Silhouette Index")
  plot(1:length(db), db, type="b", xlab="Number of Clusters",
       ylab="Davies-Bouldin Index")
  plot(1:length(kl), kl, type="b", xlab="Number of Clusters",
       ylab="Krzanowski-Lai index")
  dev.off()
}