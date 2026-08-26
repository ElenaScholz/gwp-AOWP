rm(list=ls())
renv::activate()
library(abind)
library(maps)
library(sp)
library(rworldmap)
library(fields)
library(clusterSim)


# ---- Configuration ----
config <- list(
  # EDIT THIS: set to your local project root
  ROOT = "path-to-your-gwpAOWP-folder",
  folder_and_files = list(
    input_normalized_lake_mat = "CluDatOutput/CLA_DAT_full.dat",
    folder_for_cnts = "CluDat/cla_runs/kmn_",
    output_folder_for_cla = "CluDatOutput/kmns_cla/kmns_cla_",
    data_separator = ""
  )
)
load_input_matrix <- function(path) {
  as.matrix(read.table(path))
}

folder_for_cnts = paste0(config$ROOT, "/", config$folder_and_files$folder_for_cnts)

source("scripts/utils/helperfunctions.R")
cla_dat_path = paste0(config$ROOT,"/", config$folder_and_files$input_normalized_lake_mat)
CLA_DAT <- load_input_matrix(cla_dat_path, sep = "")
results <- vector("list", 15)
clas_all <- matrix(NaN, nrow=nrow(CLA_DAT), ncol=15)

library(ClusterR)



calculate_cluster_assignment <- function(CLA_DAT, k, folder_cnt, 
                                         method = c("euclidean", "manhattan", "euclidean_kmeans"),
                                         iter.max = 100, nstart = 1
                                         ) {
  method <- match.arg(method)
  
  cnts <- as.matrix(read.table(paste0(folder_cnt, k, ".cnt")))
  CLA_DAT <- as.matrix(CLA_DAT)
  
  if (method == "euclidean") {
    dist_mat <- sapply(1:ncol(cnts), function(j) {
      rowSums((CLA_DAT - cnts[, j])^2)
    })
    assignments <- max.col(-dist_mat, ties.method = "first")
    
  } else if (method == "manhattan") {
    dist_mat <- sapply(1:ncol(cnts), function(j) {
      rowSums(abs(CLA_DAT - cnts[, j]))
    })
    assignments <- max.col(-dist_mat, ties.method = "first")
    
  } else if (method == "euclidean_kmeans") {
    kmn_res <- kmeans(CLA_DAT, centers = t(cnts), iter.max = iter.max, nstart = nstart, algorithm = "Lloyd")
    assignments <- kmn_res$cluster

  }
  
  return(assignments)
}


eukl_dist <- vector("list", 15)
output_folder_for_cla <- paste0(config$ROOT, "/", config$folder_and_files$output_folder_for_cla)

for (l in 2:15) {

  eucl_kmeans_assignments <- calculate_cluster_assignment(
    CLA_DAT,
    l,
    folder_for_cnts,
    method = "euclidean_kmeans" ,
    iter.max = 200, nstart = 10  )
  eukl_dist[[l]] <- eucl_kmeans_assignments

  write.table(eucl_kmeans_assignments, paste0(output_folder_for_cla, l, ".txt"), sep = "",
              row.names = FALSE, col.names = FALSE)
 }
