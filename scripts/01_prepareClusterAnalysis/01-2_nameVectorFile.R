rm(list = ls())
renv::activate()

source("scripts/utils/helperfunctions.R")  

config <- list(
  ROOT =  "T:/DLR/PCA-Analysis/Input/",
  input_data_folder = "03_timeseries_8247_cor",
  start_year = 2003,
  end_year = 2024
)

ROOT <-  config$ROOT

filepath <-  paste0(ROOT, config$input_data_folder)
start_year <- config$start_year
end_year <- config$end_year

#all_files = read_data(filepath, csv_sep = ";", add_ID_from_filename = FALSE)
filepaths <- list.files(filepath, pattern = ".txt$", full.names = TRUE)
nr_years = length(seq(start_year,end_year))

nrobjects =  length(filepaths)
nrlines = nrobjects * nr_years


filepaths <- list.files(filepath, pattern = ".txt$", full.names = TRUE)

name_vec <- c()
#match <- str_match(filename, "^h\\d{2}v\\d{2}_(.*?)_SGV-timeseries")
for (c in 1:length(filepaths)) {
  specific_basename <- basename(filepaths[c])
  #id <- sub("^h\\d{2}v\\d{2}_(.*?)", "\\1", specific_basename)
  id <- sub("^[^_]+_([^_]+)_.*", "\\1", specific_basename)
  name_vec_specific_aoi <- rep(id, times = nr_years)  
  
  name_vec <- c(name_vec, name_vec_specific_aoi)
}

date_vec <- seq(start_year, end_year, by = 1)
date_vec_rep <- seq(1, nrlines, by = 1)  #<- rep(x = date_vec, times = -> 16 jahre mal anzahl der lakes)
coord_vec <- seq(1, nrlines, by = 1) # verstehe ich nicht wo der hin soll


complete_name_vec <- seq(1, nrlines, by = 1)

for (d in 1:length(date_vec_rep)){
  complete_name_vec[d] <- paste0(name_vec[d], "_", date_vec_rep[d])
}

complete_name_vec <- as.data.frame(complete_name_vec)


print(paste0("Number of Rows: ", length(complete_name_vec[, 1]), "   Number of Columns: ", length(complete_name_vec[1, ]), "   Number of AOIs: ", length(filepaths)))

write.table(complete_name_vec, file = paste0("T:/DLR/Analysis2/PCA/indata/complete_name_vec.txt"), sep = " ", dec = ".", col.names = FALSE, row.names = FALSE)


