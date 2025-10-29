rm(list=ls())

library(abind)
library(maps)
library(sp)
library(rworldmap)
library(fields)
library(clusterSim)


# ---- Configuration ---- 
config <- list(
  ROOT = "T:/DLR/PCA-Analysis",
  #path_to_helper = "C:/Users/elena/Documents/RProjects/globalLakeVariability/R/Code/helperfunctions.R",
  folder_and_files = list(
    
    input_normalized_lake_mat = "CluDatOutput/CLA_DAT_full.dat",
    folder_for_cnts = "CluDat/cla_runs/kmn_",
    data_separator = ""
  )
)
load_input_matrix <- function(path) {
  as.matrix(read.table(path))
}

folder_for_cnts = paste0(config$ROOT, "/", config$folder_and_files$folder_for_cnts)

#setwd(config$working_directory)
source("scripts/utils/helperfunctions.R")
cla_dat_path = paste0(config$ROOT,"/", config$folder_and_files$input_normalized_lake_mat)
CLA_DAT <- load_input_matrix(cla_dat_path, sep = "")
# Vorbereitung
results <- vector("list", 15)
clas_all <- matrix(NaN, nrow=nrow(CLA_DAT), ncol=15)

# 
# calculate_cluster_assignment <- function(CLA_DAT, k, folder_cnt, method = c("euclidean", "manhattan")) {
#   # match.arg ensures only valid method is used
#   method <- match.arg(method)
#   
#   # Load centroids (365 × k)
#   cnts <- as.matrix(read.table(paste0(folder_cnt, k, ".cnt")))
#   
#   # Make sure CLA_DAT is a matrix for faster ops
#   CLA_DAT <- as.matrix(CLA_DAT)
#   # Initialize distance matrix: rows = data points, cols = centroids
#   dist_mat <- matrix(0, nrow = nrow(CLA_DAT), ncol = ncol(cnts))
#   
#   # Debugging: Schauen Sie sich die Dimensionen an
#   cat("CLA_DAT dimensions:", dim(CLA_DAT), "\n")
#   cat("cnts dimensions:", dim(cnts), "\n")
#   cat("Distance matrix dimensions:", dim(dist_mat), "\n")
# 
#   # Compute distances:
#   if (method == "euclidean") {
#     # squared Euclidean distances: (x - y)^2
#     # dist[i,j] = distance of row i to centroid j
#     dist_mat <- sapply(1:ncol(cnts), function(j) {
#       rowSums((CLA_DAT - cnts[, j])^2)
#     })
#   
#   } else if (method == "manhattan") {
#     dist_mat <- sapply(1:ncol(cnts), function(j) {
#       rowSums(abs(CLA_DAT - cnts[, j]))
#     })
#   } 
# 
#   # Assign cluster: index of minimum distance for each row
#   assignments <- max.col(-dist_mat, ties.method = "first")
# 
#   return(assignments)
# }

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
      # Direkter kmeans-Aufruf
    kmn_res <- kmeans(CLA_DAT, centers = t(cnts), iter.max = iter.max, nstart = nstart, algorithm = "Lloyd")
    assignments <- kmn_res$cluster  # Direkt die Zuweisungen zurückgeben  
  
  }
  
  return(assignments)
}


eukl_dist <- vector("list", 15)

for (l in 2:15) {
  # eucl_assignments <- calculate_cluster_assignment(
  #   CLA_DAT,
  #   l,
  #   config$folder_and_files$folder_for_cnts,
  #   method = "euclidean"   # "euclidean" or "manhattan" or "euclidean_kmeans"
  # )
  # eukl_dist[[l]] <- eucl_assignments
  # 
  # write.table(eucl_assignments, paste0("T:/DLR/Analysis2/PCA/output/02_eucl_cla/eucl_cla_",l,".txt"), sep = "",
  #             row.names = FALSE, col.names = FALSE)
  # 
  
  
  eucl_kmeans_assignments <- calculate_cluster_assignment(
    CLA_DAT,
    l,
    folder_for_cnts,
    method = "euclidean_kmeans" ,
    iter.max = 200, nstart = 10  )
  eukl_dist[[l]] <- eucl_kmeans_assignments
  
  write.table(eucl_kmeans_assignments, paste0("T:/DLR/PCA-Analysis/CluDatOutput/kmns_cla/kmns_cla_",l,".txt"), sep = "",
              row.names = FALSE, col.names = FALSE)
  
  
  # mnh_assignments <- calculate_cluster_assignment(
  #   CLA_DAT,
  #   l,
  #   config$folder_and_files$folder_for_cnts,
  #   method = "manhattan"   # "euclidean" or "manhattan" or "euclidean_kmeans"
  # )
  # eukl_dist[[l]] <- mnh_assignments
  
#   write.table(mnh_assignments, paste0("T:/DLR/Analysis2/PCA/output/02_mnh_cla/mnh_cla_",l,".txt"), sep = "",
#               row.names = FALSE, col.names = FALSE)
 }



k <- 10

# drei Cluster-Zuweisungen laden
eucl_assignments <- as.numeric(read.table(paste0("T:/DLR/Analysis2/PCA/output/02_eucl_cla/eucl_cla_", k, ".txt"))[,1])
kmns_assignments <- as.numeric(read.table(paste0("T:/DLR/Analysis2/PCA/output/02_kmns_cla/kmns_cla_v4_", k, ".txt"))[,1])
mnh_assignments <- as.numeric(read.table(paste0("T:/DLR/Analysis2/PCA/output/02_mnh_cla/mnh_cla_", k, ".txt"))[,1])

# Unterschiede berechnen (wie im Original)
print("Unterschiede zwischen den Methoden:")
print(paste("Euclidean vs k-means:", length(which(eucl_assignments != kmns_assignments))/length(eucl_assignments)*100, "%"))
print(paste("Euclidean vs Manhattan:", length(which(eucl_assignments != mnh_assignments))/length(eucl_assignments)*100, "%"))
print(paste("k-means vs Manhattan:", length(which(kmns_assignments != mnh_assignments))/length(kmns_assignments)*100, "%"))


library(ggplot2)
# Plots (wie im Original)
par(mfrow=c(1,3))

# Euclidean vs k-means
# Dann plotten:
plot(eucl_assignments, kmns_assignments, pch=19, col="blue")
abline(0,1, col="red")

title("Euclidean vs k-means")

# Euclidean vs Manhattan  
plot(x=eucl_assignments, y=mnh_assignments, type="p", pch=5, col="green",
     main="Euclidean vs Manhattan", xlab="Euclidean", ylab="Manhattan")
abline(0,1, col="red")

# k-means vs Manhattan
plot(x=kmns_assignments, y=mnh_assignments, type="p", pch=5, col="orange",
     main="k-means vs Manhattan", xlab="k-means", ylab="Manhattan")
abline(0,1, col="red")
