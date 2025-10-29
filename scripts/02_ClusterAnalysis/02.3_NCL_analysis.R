## clear workspace
rm(list=ls())
renv::activate()
## load libraries
library(abind)
library(maps)
library(sp)
library(rworldmap)
library(fields)
library(clusterSim)


# ---- Configuration ---- 
config <- list(
  ROOT = "T:/DLR/PCA-Analysis",
  distance_metric = "euclidean", # or manhattan
  folder_and_files = list(
    
    input_normalized_lake_mat = "CluDatOutput/CLA_DAT_full.dat",
    folder_for_cnts = "CluDat/cla_runs/kmn_",
    folder_for_cla = "CluDatOutput/kmns_cla/kmns_cla_", # kmeans 
    data_separator = ""
  ),
  kmeans_parameter = list(  # number of iterations within k-means algorithm
    iter.max = 500,#15
    nstart = 25)#10
  
)

# --- notes regarding changes in kmeans:
'This warning indicates that the K-means algorithm is having trouble converging within the maximum number of Quick-TRANSfer steps allowed. 
This typically happens when:
The data is difficult to cluster (overlapping clusters, high dimensionality)
Poor initialization of cluster centers
The algorithm gets stuck in local optima
'
# ---- Functions ----

## silhouette funktions
## Faster silhouette index after Rousseeuw, 1984. See Beck et al.
calc_FSIL <- function(mat,cla,cnt){

  ## calculate distance matrices
  d_mat <- matrix(NaN,nrow=nrow(mat),ncol=max(cla))
  for(cl in 1:max(cla)){
  d_mat[,cl] <- rowMeans(abs(mat-matrix(rep(cnt[,cl],nrow(mat)),ncol=nrow(cnt),byrow=TRUE)))
  }

  ais <- c()
  bis <- c()
  for(i in 1:nrow(d_mat)){
  ais[i] <- d_mat[i,cla[i]]

  bis_recs <- which(match(d_mat[i,],ais[i],nomatch=1878)==1878)
  bis[i] <- min(d_mat[i,bis_recs])
  }

  FSIL_val <- mean((bis-ais)/apply(cbind(ais,bis),1,max))
  return(FSIL_val)
}

calc_FSIL_faster <- function(mat, cla, cnt) {
  # Berechne Distanzmatrix zu allen Clusterzentren
  d_mat <- sapply(1:ncol(cnt), function(cl) {
    rowMeans(abs(mat - matrix(cnt[,cl], nrow=nrow(mat), ncol=ncol(mat), byrow=TRUE)))
  })
  
  # a(i): Distanz zum eigenen Cluster
  ais <- d_mat[cbind(1:nrow(mat), cla)]
  
  # b(i): Minimale Distanz zu einem anderen Cluster (NICHT dem eigenen)
  bis <- numeric(nrow(mat))
  for(i in 1:nrow(mat)) {
    # Alle Distanzen außer der zum eigenen Cluster
    other_distances <- d_mat[i, -cla[i]]
    bis[i] <- min(other_distances)
  }
  
  # Silhouette-Berechnung
  mean((bis - ais) / pmax(ais, bis))
}

## Original silhoutte sccore
silhouette_score <- function(dat,cluster){
  ss <- silhouette(cluster, dist(dat))
  return(mean(ss[, 3]))
}

lsf <- function(dat){
  
  ssd <- c()
  for(i in 2:ncol(dat)){
  ssd[i] <- sum((dummy_dat[,i]-dummy_dat[,1])**2)
  }
  return(ssd[2:ncol(dat)])
}


# ---- Start Processing ---- 

#setwd(config$working_directory)
wd <- getwd()


source("scripts/utils/helperfunctions.R")

#' Load Input Matrix
#'
#' Loads and converts a data file into a numeric matrix for cluster analysis.
#'
#' @param path Character string specifying the file path to the input data file
#' @return A numeric matrix with observations as rows and features as columns

load_input_matrix <- function(path) {
  as.matrix(read.table(path))
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
  clas <- read.table(paste0(folder_cla, k, ".txt"))
  
  
  kmn_res <- kmeans(CLA_DAT, centers = t(cnts), iter.max = iter.max, nstart = nstart)
  
  cluster_assignment <- clas[,1]
  print(head(cluster_assignment))  
  print(dim(cluster_assignment))
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

compute_KL <- function(CLA_DAT, clas_all) {
  kl <- rep(NA, ncol(clas_all))
  
  for(i in 3:(ncol(clas_all)-1)) {
    cl_subset <- clas_all[ , (i-1):(i+1)]
    
    # Prüfen, ob irgendein Cluster leer oder zu klein ist
    cluster_sizes <- table(as.vector(cl_subset))
    if(any(cluster_sizes < 2)) {
      warning(sprintf("KL-Index für Spalte %d übersprungen: mindestens ein Cluster hat <2 Punkte", i))
      kl[i] <- NA
      next
    }
    
    # Sicherheits-Wrapper für index.KL
    safe_index_KL <- tryCatch(
      {
        index.KL(CLA_DAT, clall = cl_subset)
      },
      error = function(e) {
        warning(sprintf("Fehler bei KL-Berechnung für Spalte %d: %s", i, e$message))
        return(NA)
      }
    )
    
    kl[i] <- safe_index_KL
  }
  
  return(kl)
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

plot_cluster_metrics_noKL <- function(exvs, fsil, db, filename) {
  pdf(filename, width=10, height=10)
  par(mfrow=c(2,2))
  plot(1:length(exvs), exvs, type="b", xlab="Number of Clusters",
       ylab="Explained Variance [%]", main="Explained Variance")
  plot(1:length(fsil), fsil, type="b", xlab="Number of Clusters",
       ylab="Faster Silhouette Index")
  plot(1:length(db), db, type="b", xlab="Number of Clusters",
       ylab="Davies-Bouldin Index")

  dev.off()
}



# =========================================
# Main-Skript
# =========================================
cla_dat_path = paste0(config$ROOT,"/", config$folder_and_files$input_normalized_lake_mat)
CLA_DAT <- load_input_matrix(cla_dat_path, sep = "")

# Vorbereitung
results <- vector("list", 15)
clas_all <- matrix(NaN, nrow=nrow(CLA_DAT), ncol=15)


folder_for_cnts = paste0(config$ROOT, "/", config$folder_and_files$folder_for_cnts)
folder_for_cla = paste0(config$ROOT, "/", config$folder_and_files$folder_for_cla)
# Schleife über Clusterzahlen
for(k in 2:15){
  cat("Running K =", k, "\n")
  res <- run_kmeans_evaluation(CLA_DAT, k, config$kmeans_parameter$iter.max, config$kmeans_parameter$nstart, folder_for_cnts, folder_for_cla)
  results[[k]] <- res
  clas_all[,k] <- res$cluster_assignments
  # Debug: Clusterverteilung anzeigen
  print(table(res$cluster_assignments))
}



# KL lässt sich aktuell nicht berechnen, da einige cluster nicht besetzt (manhattan)
# KL Index berechnen
KL <- compute_KL(CLA_DAT, clas_all[,2:15])

# Erstelle Plots aller Kennzahlen
# Extract from indices 2 to 15 (skip the NULL at index 1)
wss <- as.numeric(sapply(results[2:15], `[[`, "wss"))
exvs <- as.numeric(sapply(results[2:15], `[[`, "exvs"))
fsil <- as.numeric(sapply(results[2:15], `[[`, "fsil"))
db <- as.numeric(sapply(results[2:15], `[[`, "db"))
# Plots speichern


if (config$distance_metric == "euclidean"){
  filename <- paste0("T:/DLR/PCA-Analysis/CluDatOutput/plots/NCL_analysis_eucl_cla-", config$kmeans_parameter$iter.max, ".pdf")
  plot_cluster_metrics_noKL(exvs, fsil, db,filename)
  
}else {
  filename <- paste0("T:/DLR/PCA-Analysis/CluDatOutput/plots/NCL_analysis_eucl_cla-", config$kmeans_parameter$iter.max, ".pdf")
  plot_cluster_metrics(exvs, fsil, db, KL,filename)}

cat("Alle Berechnungen abgeschlossen. PDF gespeichert:", filename, "\n")
