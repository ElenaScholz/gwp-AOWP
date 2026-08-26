#' ---- Aim ----
#' This Script generates a normalized matrix of the GWP time series data 
#' 
#' Part 1 - INPUT:
#' raw_dat -> area_combination.txt = file where each row represents a year for one lake (so it has numberOfYear*NumberOfLakes = Rows)
#' 
#' ---- Workflow ----
#' Step 1: Read in Data
#' Step 2: Identify "bad-records" and assign them to 0
#' Step 3: Calculate number of years and create an output matrix for the normalized data by year
#' Step 4: normalize data by annual maximum lake extent
#' Step 5: Save the matrix
#' 
#' ---- Output ----
#' 
#' The output is a matrix with the same dimensions as the input file, but containing normalized lake data
#' 
#' Part 2 - INPUT: 
#' coordinate files -> merged together as one file with lon,lat structure (length = number of lakes)
#' 
#' ---- Workflow ----
#' Step 6: Create latitude bins: 10° steps between -55° to 65° - counts the number of lake per bin
#' Step 7: Identify the minimum number of lakes per latitude bin - this is the sampling number
#' Step 8: Generate multiple randomized datasets
#' Step 9: save subsets
#' 
#' 
#'--- OUTPUT ---- 
#' Files with the lake information for each latitude band - can be used as datasets 
#' 

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

source("scripts/utils/helperfunctions.R")
# ---- Configuration ----

config <- list(
  # EDIT THIS: set to your local project root
  ROOT = "path-to-your-gwpAOWP-folder/",
  # The overall input folder containing all datasets for the cluster analysis - (all files generated in the 00.X_Scripts)
  input_folder   = "CluDat",

  # Information needed to read the lake area file and write the output
  normalizing_lakes = list(
    # The file name of the Area file generated in Script 00.4 -> IMPORTANT use the one with the sample information (called _withPrefix.txt)
    area_file = "area_combination_withPrefix.txt",
    # the folder where the final normalized dataset will ne stored
    output_folder  = "CluDatOutput",
    # the name of the files (one with and one without the Metadata / Prefixes)
    normalized_lake_area = "CLA_DAT_full.dat",
    normalized_lake_area_with_MetaData = "CLA_DAT_full_withMetaData.dat",
    # Defining the csv seperator
    csv_sep_raw_data = " "
  ),
  # Information needed to generate the sample datasets for each latitude nand
  lat_sample_lakes = list(
    # filename of the coodinated file with latitude and longitude information
    coordinate_file =  "all_coordinates_complete.txt",
    # where the files will be saved
    output_folder = "07_latitude_bins/CLA_DAT_",
    # Defining the csv seperator
    csv_sep_coordinates  = ";"
  )
)

config$input_folder <- paste0(config$ROOT, config$input_folder)
config$normalizing_lakes$output_folder <- paste0(config$ROOT, config$normalizing_lakes$output_folder)
config$lat_sample_lakes$output_folder <- paste0(config$ROOT, config$lat_sample_lakes$output_folder)

# ---- Start Processing ----

#== STEP 1
## read raw data, rows: 18 years * x-lakes, columns: 365 days
raw_data_path = file.path(config$input_folder, config$normalizing_lakes$area_file )
raw_dat_full <- as.matrix(read.delim(raw_data_path, header = FALSE, sep = config$normalizing_lakes$csv_sep_raw_data, dec=".",na.strings="NA"))


#== STEP 2
raw_dat <- check_for_invalid_data(raw_data_full = raw_dat_full)
## Separate info columns (last 2 columns) from data columns
info_cols <- raw_dat[, (ncol(raw_dat)-1):ncol(raw_dat)]
raw_dat <- raw_dat[, 1:(ncol(raw_dat)-2)]

raw_dat <- apply(raw_dat, 2, as.numeric)


#== STEP 3 -  normalize data by annual maximum lake extent
out_dat_byYear <- normalize_data(raw_dat)
out_dat_byYear <- round(out_dat_byYear,6)
#== STEP 4

## Create final output matrix with normalized data + info columns
final_output <- cbind(out_dat_byYear, info_cols)



print(paste("Original data dimensions:", nrow(raw_dat_full), "x", ncol(raw_dat_full)))
print(paste("Normalized data dimensions:", nrow(out_dat_byYear), "x", ncol(out_dat_byYear)))
print(paste("Final output dimensions:", nrow(final_output), "x", ncol(final_output)))
print("Info columns successfully preserved and reattached!")
#== STEP 5- save
## write data
path_cla_dat_full <- file.path(config$normalizing_lakes$output_folder, config$normalizing_lakes$normalized_lake_area )
path_cla_dat_meta <- file.path(config$normalizing_lakes$output_folder, config$normalizing_lakes$normalized_lake_area_with_MetaData)
write.table(out_dat_byYear,file=path_cla_dat_full,col.names=FALSE,row.names=FALSE,quote=FALSE)
write.table(final_output,file=path_cla_dat_meta,col.names=FALSE,row.names=FALSE,quote=FALSE)


 #==== not used

#== STEP 6
## read coordinates
coordinates_path = file.path(config$input_folder, config$lat_sample_lakes$coordinate_file)
coordinates = read.delim(coordinates_path, sep = config$csv_sep_coordinates, header = TRUE)

coords <- coordinates[,1:2]

rm(coordinates)



nyears <- nrow(raw_data)/nrow(coords)

lat_bins <- seq(-55,65,10)
nlakes <- c()
for(i in 1:length(lat_bins)){

  nlakes[i] <- length(which(coords[,2]>=lat_bins[i] & coords[,2]<lat_bins[i]+10))
  if(i==1){
    nlakes[i] <- length(which(coords[,2]>=lat_bins[i]-10 & coords[,2]<lat_bins[i]+10))
  }
}

#== STEP 7-9

for(j in 1:ceiling(max(nlakes)/min(nlakes))){
  CLA_DAT <- matrix(NaN,nrow=length(lat_bins)*min(nlakes)*nyears,ncol=365)
  for(i in 1:length(lat_bins)){
    recs_oi <- which(coords[,2]>=lat_bins[i] & coords[,2]<lat_bins[i]+10)
    sub_recs <- runif(min(nlakes),min=1,max=length(recs_oi))

    lake_recs <- c()
    for(lake in recs_oi[sub_recs]){
      # print(lake)
      start_lake <- (lake*nyears+1)-nyears
      end_lake <- (lake*nyears)
      lake_recs <- c(lake_recs,seq(start_lake,end_lake,1))
    }

    start_ind <- (i*(nyears*min(nlakes))+1)-(nyears*min(nlakes))
    end_ind <- (i*(nyears*min(nlakes)))
    CLA_DAT[start_ind:end_ind,] <- out_dat_byYear[lake_recs,]
  }
  write.table(round(CLA_DAT,6),file=paste(config$lat_sample_lakes$output_folder,j,".dat",sep=config$lat_sample_lakes$csv_sep_coordinates),col.names=FALSE,row.names=FALSE,quote=FALSE)
}

