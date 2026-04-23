rm(list=ls())
renv::activate()



inpath <- "T:/DLR-DFD/PCA-Analysis/CluDatOutput/data/KMN_LakeClass_resultsKopie.txt"


library(readr)

load_input_matrix <- function(path) {
  df <- read_table(path, col_names = TRUE)
  as.matrix(df)
}

LakeClass_kmns <- load_input_matrix(inpath) 
