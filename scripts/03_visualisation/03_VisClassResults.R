## clear workspace
rm(list=ls())
renv::activate()
library(abind)
library(maps)
library(sp)
library(rworldmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(fields)
library(clusterSim)
library(ggsci)

# ---- Configuration ----
source("scripts/utils/helperfunctions.R")
source("scripts/utils/vis.R")
config <- list(
  ROOT = "T:/DLR-DFD/PCA-Analysis/",
  input_datasets = list(
    climate_dataset =  "CluDat/Points_Climate.txt",
    continents_dataset = "CluDat/Points_Cont.txt",
    coordinate_dataset = "CluDat/all_coordinates_complete.txt",
    normalized_laked_data = "CluDatOutput/CLA_DAT_full.dat",
    sep = ";"
  ),
  cluster_centroids = list(
    folder_for_cnts = "CluDat/cla_runs/kmn_",
    folder_for_cla = "CluDatOutput/kmns_cla/kmns_cla_", #"output/02_kmn_res_cla/kmn_res_cla_K", #"output/02_mnh_cla/mnh_cla", #"output/02_eucl_cla/eucl_cla", #"indata/cla_runs/kmn_", 
    sep = "",
    number_of_cluster = 10
  ),
  plotting_information = list(
    start_year = 2003,
    end_year = 2024,
    interpolate_colors = FALSE,
    pdf_size = list(
      width = 25,
      height_large = 20.42,
      height_small = 10.42
    )
  ),
  output_directories = list(
    for_plots = "CluDatOutput/plots_NewLayout",
    for_data = "CluDatOutput/data"
  )
)


#source(config$path_to_helpers)
#setwd(config$working_directory)

# 
# # ---- Define color palette
library(colorBlindness)
# 
# # 10 Basisfarben
# color_ramp <- pal_flatui("aussie")(10)   # 10 Farben aus der FlatUI-Palette
# ramp_dev <- colorRamp(color_ramp)
# if (config$plotting_information$interpolate_colors){
#   # können interpoliert werden
#   
#   lake_cols <- rgb(ramp_dev(seq(0, 1, length.out = ncl)), max = 255)  
# } else{
#   lake_cols <- color_ramp
# }

# ---- Reading Data ----
normalized_lake_dat <- load_input_matrix(
  path = paste0(config$ROOT, "/", config$input_datasets$normalized_laked_data), 
  sep = config$input_datasets$seperator
)

climate = read.delim(
  file =paste0(config$ROOT, "/",config$input_datasets$climate_dataset), 
  header = TRUE, 
  sep = config$input_datasets$sep, 
  dec = "."
)

continents <- read.delim(
  paste0(config$ROOT, "/",config$input_datasets$continents_dataset),
  header=TRUE, 
  sep =  config$input_datasets$sep, 
  dec = "."
)

coords <- read.delim(
  paste0(config$ROOT, "/",config$input_datasets$coordinate_dataset),
  header=TRUE, 
  sep =  config$input_datasets$sep,
  dec= "."
)

cnts <- read.table(
  paste0(config$ROOT, "/", config$cluster_centroids$folder_for_cnts,config$cluster_centroids$number_of_cluster,".cnt"), 
  sep = config$cluster_centroids$sep
) 

clas <- read.table(
  paste0(config$ROOT, config$cluster_centroids$folder_for_cla,config$cluster_centroids$number_of_cluster,".txt"), 
  sep = config$cluster_centroids$sep
)

# ---- Start Code - Define important variables ----

# create_color_palette
lake_cols_list <- create_lake_color_palette(config = config,
                                            color_ramp = c("forestgreen","green","cyan","blue","navy",
                                                           "yellow","gold","red","brown","darkviolet")#pal_flatui("aussie")(10)
)

heatmap_colors <- create_lake_color_palette(config, color_ramp = (colorBlindness::Blue2Orange10Steps))
# matrix_cols <- colorRampPalette(c("#5A6B7B", "#8A97A6", "#C7CDD4", "#F0EAE2", "#F4C17A", "#E8963A"))(100)
# matrix_cols <- colorRampPalette(c("#EAF0F4", "#D2DCE4", "#B4C4D0", "#DCE0DE", "#F0E8DC", "#F4C17A", "#E8963A"))(100)
matrix_cols <- colorRampPalette(c("#EAF0F4", "#D2DCE4", "#B4C4D0", "#B1C6D9","#ffdfb0", "#F4C17A", "#E8963A"))(100)
lake_cols <- lake_cols_list$lake_cols
# Calculate number of years per lake
number_of_years <- nrow(normalized_lake_dat) / nrow(coords)

if (number_of_years != length(seq(config$plotting_information$start_year:config$plotting_information$end_year))){
  print("Check Start and End year - The number of Years calculated out of input data does not match the plotting information")
}


start_year <- config$plotting_information$start_year
end_year <- config$plotting_information$end_year


ncl = config$cluster_centroids$number_of_cluster
# Plot cluster centers


# Reorder clusters according to subjective interpretation
cluster_order <- c(7, 10, 9, 3, 1, 6, 2, 5, 4, 8)
reordered <- reorder_clusters(cnts, clas, cluster_order)
cnts <- reordered$cnts
clas <- reordered$clas
plot_cluster_centers(cnts, ncl)

# ---- Write Annual Classification Results ----
# Reorganize cluster assignments by time periods
# cluster_dist information
cluster_matrices <- reorganize_cluster_assignments(clas, nrow(coords), number_of_years)


# Save results (using the reorganized data)
write.table(cluster_matrices$full,
            file = file.path(config$ROOT, "/" ,config$output_directories$for_data,"03_AnnualClassResults.txt"),
            quote = FALSE, row.names = FALSE,
            col.names = config$plotting_information$start_year:config$plotting_information$end_year, append = FALSE)



##### identify most frequent class of a lake
# cluster_char infromation
lake_frequencies <- get_most_frequent_lake_Cluster(cluster_matrices = cluster_matrices)

time_axis <- get_time_axis()

source('scripts/utils/vis.R')
 

# ---- NEW LAYOUT: Create plot for Class Results whole time series ---- 
# create pdf
source('scripts/utils/vis.R')


tiff(
  paste0(
    config$ROOT, "/", 
    config$output_directories$for_plots,
    "/03.1.NL_White_Class_results_full_textSize_legendOutside.tif"
  ),
  width = config$plotting_information$pdf_size$width,
  height = config$plotting_information$pdf_size$height_large,
  units = "in",
  res = 300
)



# ------------------- Base canvas -------------------


plot(
  0,0,
  type="n",
  xlim=c(0,1),
  ylim=c(0,1),
  axes=FALSE,
  xlab="",
  ylab=""
)


# ------------------- A heading -------------------


par(fig=c(0,1,0.92,1), new=TRUE, mar=c(0,0,0,0))

plot(0,0, type="n", xlim=c(0,1), ylim=c(0,1),
     xaxs="i", yaxs="i",
     axes=FALSE, xlab="", ylab="")

text(0.03, 0.5,
     "(a) Annual Open-Surface-Water Patterns (AOWPs)",
     adj=c(0,0.5), cex=2.25, font=2)
# ----------------------------------------------------
# Top row: clusters 1-5
# ----------------------------------------------------

for (cla in 1:5) {
  
  pos <- c(0.03 + 0.19*(cla-1), 0.03 + 0.19*cla, 0.74, 0.94)
  
  plot_class_panel(
    cla = cla,
    position = pos,
    x_recs = time_axis$x_records,
    x_ticks = time_axis$x_ticks,
    x_labs = time_axis$x_labs,
    clas = clas,
    dominant_cluster=lake_frequencies$cluster_char_total[,1],
    normalized_lake_dat = normalized_lake_dat,
    centroids = cnts,
    lake_colors = lake_cols,
    bg_color="white",
    mar=c(4.5,2.5,5,1),
    show_x_ticks=TRUE,
    show_x_title=FALSE,
    show_y_label=(cla==1),
    show_class_percentage=TRUE
  )
}




# ------------------- Bottom row: clusters 6-10 -------------------

for (cla in 6:10) {
  
  pos <- c(0.03 + 0.19*(cla-6),
           0.03 + 0.19*(cla-5),
           0.54, 0.74)
  
  plot_class_panel(
    cla = cla,
    position = pos,
    x_recs = time_axis$x_records,
    x_ticks = time_axis$x_ticks,
    x_labs = time_axis$x_labs,
    clas = clas,
    dominant_cluster=lake_frequencies$cluster_char_total[,1],
    normalized_lake_dat = normalized_lake_dat,
    centroids = cnts,
    lake_colors = lake_cols,
    bg_color="white",
    mar=c(4.5,2.5,5,1),
    show_x_ticks=TRUE,
    show_x_title=TRUE,
    show_y_label=(cla==6),
    show_class_percentage=TRUE
  )
}




# ------------------- B heading -------------------


par(fig=c(0,1,0.49,0.53), new=TRUE, mar=c(0,0,0,0))

plot(0,0, type="n", xlim=c(0,1), ylim=c(0,1),
     xaxs="i", yaxs="i",
     axes=FALSE, xlab="", ylab="")

text(0.03, 0.5,
     "(b) Global distribution of dominant AOWPs",
     adj=c(0,0.5), cex=2.25, font=2)


# ------------------- Map -------------------

pos_map <- c(
  0,
  1,
  0,
  0.49
)


plot_world_map_rob(
  coords = coords,
  cluster_vals = lake_frequencies$cluster_char_total[,1],
  lake_cols = lake_cols_list$lake_cols,
  year_label="Dominant AOWP",
  number_of_cluster=ncl,
  position=pos_map,
  white_world = TRUE
)



dev.off()



# ------ PLOT ANNUAL RESULTS ------
tiff(paste0(config$ROOT ,"/", config$output_directories$for_plots,"/03_NL_Class_results_annual.tiff"),
     width=config$plotting_information$pdf_size$width,
     height=config$plotting_information$pdf_size$height_large, units = "in", res = 300)

for (year in seq(start_year, end_year)) {
  
  # Fresh page + reset fig region for each year
  par(fig = c(0,1,0,1), new = FALSE, mar = c(0,0,0,0))
  plot(0,0, type="n", xlim=c(0,1), ylim=c(0,1),
       axes=FALSE, xlab="", ylab="")
  
  # ---- A heading ----
  par(fig=c(0,1,0.92,1), new=TRUE, mar=c(0,0,0,0))
  plot(0,0, type="n", xlim=c(0,1), ylim=c(0,1),
       xaxs="i", yaxs="i", axes=FALSE, xlab="", ylab="")
  text(0.03, 0.5,
       "A",
       adj=c(0,0.5), cex=1.8, font=2)
  
  # ---- Top row: clusters 1-5 ----
  for (cla in 1:5) {
    pos <- c(0.03 + 0.19*(cla-1), 0.03 + 0.19*cla, 0.74, 0.92)
    plot_class_panel(
      cla = cla, position = pos,
      x_recs = time_axis$x_records, x_ticks = time_axis$x_ticks, x_labs = time_axis$x_labs,
      clas = clas, normalized_lake_dat = normalized_lake_dat, centroids = cnts,
      lake_colors = lake_cols,
      bg_color="white", mar=c(3.5,2.5,2.5,1),
      show_x_ticks=TRUE, show_x_title=FALSE, show_y_label=(cla==1), show_class_percentage=TRUE
    )
  }
  
  # ---- Bottom row: clusters 6-10 ----
  for (cla in 6:10) {
    pos <- c(0.03 + 0.19*(cla-6), 0.03 + 0.19*(cla-5), 0.54, 0.72)
    plot_class_panel(
      cla = cla, position = pos,
      x_recs = time_axis$x_records, x_ticks = time_axis$x_ticks, x_labs = time_axis$x_labs,
      clas = clas, normalized_lake_dat = normalized_lake_dat, centroids = cnts,
      lake_colors = lake_cols,
      bg_color="white", mar=c(4.5,2.5,2.5,1),
      show_x_ticks=TRUE, show_x_title=TRUE, show_y_label=(cla==6), show_class_percentage=TRUE
    )
  }
  
  # ---- B heading (year-specific) ----
  par(fig=c(0,1,0.49,0.53), new=TRUE, mar=c(0,0,0,0))
  plot(0,0, type="n", xlim=c(0,1), ylim=c(0,1),
       xaxs="i", yaxs="i", axes=FALSE, xlab="", ylab="")
  text(0.03, 0.5,
       paste0("B"),
       adj=c(0,0.5), cex=1.8, font=2)
  
  # ---- Map ----
  pos_map <- c(0, 1, 0, 0.49)
  plot_world_map_rob(
    coords,
    cluster_vals = cluster_matrices$full[, year-start_year+1],
    lake_cols = lake_cols_list$lake_cols,
    year_label = paste("AOWP:", year),
    number_of_cluster = ncl,
    position = pos_map,
    white_world = TRUE
  )
}


dev.off()


source('scripts/utils/vis.R')

# ---- Plot Lake Variability ----


source('scripts/utils/vis.R')

breaks_var <- seq(min(lake_frequencies$cluster_char_total[,2], na.rm=TRUE)-.5,
                  max(lake_frequencies$cluster_char_total[,2], na.rm=TRUE)+.5)
lake_cols_var <- rgb(lake_cols_list$ramp_dev(seq(0, 1, length = length(breaks_var)-1)), max = 255)
lake_cols_var_ylgnbu <- colorRampPalette(RColorBrewer::brewer.pal(9, "YlGnBu"))(10)
lake_cols_var_lajolla <- scico::scico(10, palette = "lajolla", direction = -1)
lake_cols_var_oslo <- scico::scico(10, palette = "oslo", direction = -1)

tiff(paste0(config$ROOT, "/", config$output_directories$for_plots, "/03.2.NL_White_Class_variability_full_textSize_legendOutside.tiff"),
     width = config$plotting_information$pdf_size$width,
     height = config$plotting_information$pdf_size$height_small, units = "in", res = 300)

plot(0,0,type="n",xlim=c(-180,180),ylim=c(-60,90),axes=FALSE,
     yaxs="i",xaxs="i",xlab="",ylab="",cex.axis=1.5)

pos_map <- c(0, 1, 0, 1)

var_vals <- sort(unique(lake_frequencies$cluster_char_total[,2]))

# Percentage of water bodies per variability value
var_col   <- lake_frequencies$cluster_char_total[,2]
var_perc  <- sapply(var_vals, function(v) 100 * sum(var_col == v, na.rm = TRUE) / length(var_col))

# Labels: value + percentage in parentheses
custom_labels <- paste0(var_vals, " (", round(var_perc, 2), "%)")

custom_legend_var <- list(
  labels = custom_labels,
  colors = lake_cols_var_lajolla[seq_along(var_vals)]
)


title_var <- paste0(
  "<span style='font-size:24pt'>",
  "**Number of additional different<br>",
  "AOWPs per water body**",
  "</span><br>",
  "<span style='font-size:18pt;font-weight:normal'>",
  "(beyond the dominant AOWP<br>",
  "over the 22-year period)",
  "</span>"
)
# 
# title_var <- paste0(
#   "<span style='font-size:24pt'>",
#   "**Additional AOWPS<br>",
#   "per water body**",
#   "</span><br>",
#   "<span style='font-size:18pt;font-weight:normal'>",
#   "(beyond the dominant AOWP<br>",
#   "over the 22-year period)",
#   "</span>"
# )

# title_var <- "**Additional AOWPs per water body**</span><br>
# <span style='font-size:24pt;font-weight:normal'>(beyond the dominant AOWP, 2003–2024)"

plot_world_map_rob(coords = coords,
                   cluster_vals = lake_frequencies$cluster_char_total[,2],
                   lake_cols = lake_cols_var_lajolla,
                   year_label = title_var,
                   number_of_cluster = length(var_vals),
                   custom_legend = custom_legend_var,
                   position = pos_map,
                   white_world = TRUE,
                   textsize_legend = 24,
                   legend_outside = TRUE)

dev.off()


# ---- Write Classification Summary ----

# 20.04.2026 the file does not contain all information , therefore new function added (_new)
classification_summary_df <- create_classification_summary(cluster_char = lake_frequencies$cluster_char_total,
                                                           cluster_char_first = lake_frequencies$cluster_char_first,
                                                           cluster_char_last = lake_frequencies$cluster_char_last,
                                                           coords = coords,
                                                           climate = climate)

classification_summary_df_new <- create_classification_summary_new(cluster_char = lake_frequencies$cluster_char_total,
                                                           cluster_char_first = lake_frequencies$cluster_char_first,
                                                           cluster_char_last = lake_frequencies$cluster_char_last,
                                                           coords = coords,
                                                           climate = climate)

write.table(classification_summary_df,
            file=paste0(config$ROOT , "/", config$output_directories$for_data,"/KMN_LakeClass_results.txt"),
            row.names=FALSE,col.names=TRUE,quote=FALSE, append = FALSE)

write.table(classification_summary_df_new,
            file=paste0(config$ROOT , "/", config$output_directories$for_data,"/KMN_LakeClass_results_new.txt"),
            row.names=FALSE,col.names=TRUE,quote=FALSE, append = FALSE)


# ---- Plot Lake Changes first and second period ----
# 
# pdf(paste0(config$ROOT, "/", config$output_directories$for_plots,"/Class_change_1vs2.pdf"),
#     width=config$plotting_information$pdf_size$width, 
#     height=config$plotting_information$pdf_size$height_small)

source('scripts/utils/vis.R')



# ================= Shared data prep =================
base_cols <- colorRampPalette(heatmap_colors$lake_cols)(100)
base_cols <- base_cols[15:100]
colors_lightened <- colorRampPalette(base_cols)(100)

changed <- lake_frequencies$cluster_char_first[,1] != lake_frequencies$cluster_char_last[,1]
target_cluster <- lake_frequencies$cluster_char_last[,1]
cluster_vals_for_map <- ifelse(changed, target_cluster, 0)

ord <- order(cluster_vals_for_map != 0)
coords_all       <- coords[ord, ]
cluster_vals_all <- cluster_vals_for_map[ord]

map_cols <- c("grey80", lake_cols_list$lake_cols)
custom_legend <- list(
  levels = 0:10,
  labels = c("No transition", paste("AOWP", 1:10)),
  colors = map_cols
)

tiff(paste0(config$ROOT, "/", config$output_directories$for_plots,"/03.3_NL_white_Class_change_target_changedTextSize.tiff"),
     width=config$plotting_information$pdf_size$width,
     height=config$plotting_information$pdf_size$height_large,
     units = "in", res = 300)

plot(0,0, type="n", xlim=c(0,1), ylim=c(0,1), axes=FALSE, xlab="", ylab="")
n_changed <- sum(changed)
n_total   <- length(changed)
pct_changed <- 100 * n_changed / n_total
change_subtitle <- sprintf("%d of %d water bodies changed (%.1f%%)",
                           n_changed, n_total, pct_changed)
# # A heading

par(fig=c(0,1,0.88,1), new=TRUE, mar=c(0,0,0,0))
plot(0,0, type="n", xlim=c(0,1), ylim=c(0,1), xaxs="i", yaxs="i",
     axes=FALSE, xlab="", ylab="")

text(0.03, 0.72, "(a) Global distribution of dominant AOWP transitions",
     adj=c(0,0.5), cex=2.25, font=2)
text(0.03, 0.52, change_subtitle,
     adj=c(0,0.5), cex=2.0, font=1, col="grey25")


# 
# Karte
pos_map <- c(0, 1, 0.47, 0.90)
plot_world_map_rob(coords = coords_all,
                   cluster_vals = cluster_vals_all,
                   lake_cols = map_cols,
                   year_label = "",                     # kein Legendentitel
                   number_of_cluster = length(custom_legend$labels),
                   custom_legend = custom_legend,
                   position = pos_map,
                   white_world = TRUE,
                   legend_outside = FALSE)

# B heading
par(fig=c(0,1,0.42,0.46), new=TRUE, mar=c(0,0,0,0))

plot(0,0, type="n", xlim=c(0,1), ylim=c(0,1), xaxs="i", yaxs="i", axes=FALSE, xlab="", ylab="")
text(0.03, 0.5, "(b) Global transition matrix", adj=c(0,0.5), cex=2.25, font=2)

# par(fig=c(0.3, 0.70, 0.00, 0.42), new=TRUE)
# plot_change_matrix(
#   cluster_char_first = lake_frequencies$cluster_char_first,
#   cluster_char_last  = lake_frequencies$cluster_char_last,
#   ncl = ncl, colors = matrix_cols,
#   normalize = "row", include_diagonal = TRUE, percent_on_top = TRUE,
#   cex_values = 1.8, cex_lab = 1.8, cex_axis = 1.8, mar = c(10, 4.8, 1,1), legend_mar = 8
# )
# Matrix — Breite so berechnet, dass die Box quadratisch wird
csi <- par("csi"); W <- 25; H <- config$plotting_information$pdf_size$height_large
m <- c(10, 4.8, 1, 1); fig_h <- 0.42

box_h <- fig_h * H - (m[1] + m[3]) * csi
fig_w <- (box_h + (m[2] + m[4]) * csi) / W

par(fig = c(0.5 - fig_w/2, 0.5 + fig_w/2, 0, fig_h), new = TRUE)

plot_change_matrix(
  cluster_char_first = lake_frequencies$cluster_char_first,
  cluster_char_last  = lake_frequencies$cluster_char_last,
  ncl = ncl, colors = matrix_cols,
  normalize = "row", include_diagonal = TRUE, percent_on_top = TRUE,
  cex_values = 1.4, cex_lab = 1.7, cex_axis = 1.8,
  mar = m, legend_mar = 8.5
)
# 
dev.off()


tiff(paste0(config$ROOT, "/", config$output_directories$for_plots,"/03.3_NL_white_Class_change_target_MAP.tiff"),
     width=config$plotting_information$pdf_size$width, 
     height=config$plotting_information$pdf_size$height_small,
     units = "in", res = 300)

plot(0,0, type="n", xlim=c(0,1), ylim=c(0,1), axes=FALSE, xlab="", ylab="")

# Heading + grey subtitle
par(fig=c(0,1,0.88,1), new=TRUE, mar=c(0,0,0,0))
plot(0,0, type="n", xlim=c(0,1), ylim=c(0,1), xaxs="i", yaxs="i", axes=FALSE, xlab="", ylab="")
text(0.03, 0.68, "Global distribution of dominant AOWP transitions",
     adj=c(0,0.5), cex=2.25, font=2)
text(0.03, 0.42, change_subtitle,
     adj=c(0,0.5), cex=2.0, font=1, col="grey30")

# Map below the heading
pos_map <- c(0, 1, 0, 0.87)
plot_world_map_rob(coords = coords_all,
                   cluster_vals = cluster_vals_all,
                   lake_cols = map_cols,
                   year_label = "",
                   number_of_cluster = length(custom_legend$labels),
                   custom_legend = custom_legend,
                   position = pos_map,
                   white_world = TRUE)
dev.off()

# tiff(paste0(config$ROOT, "/", config$output_directories$for_plots,"/03.3_NL_white_Class_change_target_MATRIX.tiff"),
#      width=12, height=12, units = "in", res = 300, compression = "lzw")
# 
# plot(0,0, type="n", xlim=c(0,1), ylim=c(0,1), axes=FALSE, xlab="", ylab="")
# par(fig=c(0.08, 0.92, 0.06, 0.94), new=TRUE)   # Matrix füllt nicht das ganze Blatt
# 
# plot_change_matrix(
#   cluster_char_first = lake_frequencies$cluster_char_first,
#   cluster_char_last  = lake_frequencies$cluster_char_last,
#   ncl = ncl, colors = matrix_cols,
#   normalize = "row", include_diagonal = TRUE, percent_on_top = TRUE,
#   cex_values = 1.3, cex_lab = 1.5, cex_axis = 1.5,
#   mar = c(9, 6, 1, 1), legend_mar = 1
# )
# dev.off()
tiff(paste0(config$ROOT, "/", config$output_directories$for_plots,"/03.3_NL_white_Class_change_target_MATRIX.tiff"),
     width=12, height=12, units = "in", res = 300, compression = "lzw")

plot(0,0, type="n", xlim=c(0,1), ylim=c(0,1), axes=FALSE, xlab="", ylab="")

# Breite so berechnet, dass die Box quadratisch wird
csi <- par("csi"); W <- 12; H <- 12
m <- c(11, 6, 1, 1); fig_b <- 0.06; fig_t <- 0.94

box_h <- (fig_t - fig_b) * H - (m[1] + m[3]) * csi
fig_w <- (box_h + (m[2] + m[4]) * csi) / W

par(fig = c(0.5 - fig_w/2, 0.5 + fig_w/2, fig_b, fig_t), new = TRUE)

plot_change_matrix(
  cluster_char_first = lake_frequencies$cluster_char_first,
  cluster_char_last  = lake_frequencies$cluster_char_last,
  ncl = ncl, colors = matrix_cols,
  normalize = "row", include_diagonal = TRUE, percent_on_top = TRUE,
  cex_values = 1.3, cex_lab = 1.5, cex_axis = 1.5,
  mar = m, legend_mar = 4.5
)
dev.off()
# ---- Pie Charts changes of climate regions between first and second period ----

source("scripts/utils/vis.R")

# Palette wie gehabt (ggf. deine aufgehellte Variante hier einsetzen)

# ---- Transition matrices per climate zone ----
plot_change_matrix_by_group(
  group_vector       = climate$description_of_zone,
  cluster_char_first = lake_frequencies$cluster_char_first,
  cluster_char_last  = lake_frequencies$cluster_char_last,
  ncl                = ncl,
  colors             = matrix_cols,
  # group_name         = "Transition matrices per climate zone",
  output_file        = paste0(config$ROOT, "/", config$output_directories$for_plots,
                              "/03.4_NL_ChangeMatrix_ClimateZones.tiff"),
  height = 17,
  width = 21,
  cb_bottom          = 0.08,      # schmaler Streifen unten (NICHT 0.8)
  panel_mar = c(3.5, 4.0, 3.0, 0.5),
  legend_width       = 4,       # Dicke der Bar (NICHT 10)
  legend_mar         = 3,         # Abstand nach unten (NICHT 10)
  ncol               = 3,
  cex_values         = 1.3,
  cex_lab            = 1.5,
  cex_axis           = 1.3
)


# ---- Transition matrices per continent ----
plot_change_matrix_by_group(
  group_vector       = continents$continent,
  cluster_char_first = lake_frequencies$cluster_char_first,
  cluster_char_last  = lake_frequencies$cluster_char_last,
  ncl                = ncl,
  colors             = matrix_cols,
  # group_name         = "Transition matrices per continent",
  output_file        = paste0(config$ROOT, "/", config$output_directories$for_plots,
                              "/03.4_NL_ChangeMatrix_Continents.tiff"),
  height = 16,
  cb_bottom          = 0.08,      # schmaler Streifen unten (NICHT 0.8)
  panel_mar = c(3.5, 4.0, 3.0, 0.5),
  legend_width       = 4,       # Dicke der Bar (NICHT 10)
  legend_mar         = 3,         # Abstand nach unten (NICHT 10)
  ncol               = 3,
  cex_values         = 1.3,
  cex_lab            = 1.5,
  cex_axis           = 1.3
)

# 
# # 
source("scripts/utils/vis.R")
# ---- Plot Annual Distribution Heatmaps ----
plot_annual_distribution(
  group_vector = climate$description_of_zone,
  group_name   = "Climate Zones",
  cluster_dist = cluster_matrices$full,
  ncl          = ncl,
  start_year   = start_year,
  end_year     = end_year,
  output_file  = paste0(config$ROOT, "/", config$output_directories$for_plots,"/03_AnnualLakeDistribution_Climates.tif")
)

plot_annual_distribution(
  group_vector = continents$continent,
  group_name   = "Continents",
  cluster_dist = cluster_matrices$full,
  ncl          = ncl,
  start_year   = start_year,
  end_year     = end_year,
  output_file  = paste0(config$ROOT, "/", config$output_directories$for_plots,"/03_AnnualLakeDistribution_Continents.tif")
)


