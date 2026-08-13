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
            file = file.path(config$ROOT, "/" ,config$output_directories$for_data,"AnnualClassResults.txt"),
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
    "/03.NL_White_Class_results_full.tif"
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
     "A",
     adj=c(0,0.5), cex=1.8, font=2)
# ----------------------------------------------------
# Top row: clusters 1-5
# ----------------------------------------------------

for (cla in 1:5) {
  
  pos <- c(0.03 + 0.19*(cla-1), 0.03 + 0.19*cla, 0.74, 0.92)
  
  plot_class_panel(
    cla = cla,
    position = pos,
    x_recs = time_axis$x_records,
    x_ticks = time_axis$x_ticks,
    x_labs = time_axis$x_labs,
    clas = clas,
    normalized_lake_dat = normalized_lake_dat,
    centroids = cnts,
    lake_colors = lake_cols,
    bg_color="white",
    mar=c(3.5,2.5,2.5,1),
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
           0.54, 0.72)
  
  plot_class_panel(
    cla = cla,
    position = pos,
    x_recs = time_axis$x_records,
    x_ticks = time_axis$x_ticks,
    x_labs = time_axis$x_labs,
    clas = clas,
    normalized_lake_dat = normalized_lake_dat,
    centroids = cnts,
    lake_colors = lake_cols,
    bg_color="white",
    mar=c(4.5,2.5,2.5,1),
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
     "B",
     adj=c(0,0.5), cex=1.8, font=2)


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

# Adjustments of Colorbar
breaks_var <- seq(min(lake_frequencies$cluster_char_total[,2], na.rm=TRUE)-.5,
                  max(lake_frequencies$cluster_char_total[,2], na.rm=TRUE)+.5)

lake_cols_var <- rgb(lake_cols_list$ramp_dev(seq(0, 1, length = length(breaks_var)-1)), max = 255)
lake_cols_var_ylgnbu <- colorRampPalette(RColorBrewer::brewer.pal(9, "YlGnBu"))(10)
lake_cols_var_lajolla <- scico::scico(10, palette = "lajolla", direction = -1)
lake_cols_var_oslo <- scico::scico(10, palette = "oslo", direction = -1)
tiff(paste0(config$ROOT, "/", config$output_directories$for_plots, "/03.NL_White_Class_variability_full.tiff"),
     width = config$plotting_information$pdf_size$width,
     height = config$plotting_information$pdf_size$height_small, units = "in", res = 300)

plot(0,0,type="n",xlim=c(-180,180),ylim=c(-60,90),axes=FALSE,
     yaxs="i",xaxs="i",xlab="",ylab="",cex.axis=1.5)

pos_map <- c(0, 1, 0, 1)

var_vals <- sort(unique(lake_frequencies$cluster_char_total[,2]))

# Kurzer Titel + schlichte Zahlen-Labels (Details gehen in die Caption)
custom_legend_var <- list(
  labels = as.character(var_vals),
  colors = lake_cols_var_lajolla[seq_along(var_vals)]
)

plot_world_map_rob(coords = coords,
                   cluster_vals = lake_frequencies$cluster_char_total[,2],
                   lake_cols = lake_cols_var_lajolla,
                   year_label = "AOWP variability",
                   number_of_cluster = length(var_vals),
                   custom_legend = custom_legend_var,
                   position = pos_map,
                   white_world = TRUE)

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


# tiff(paste0(config$ROOT, "/", config$output_directories$for_plots,"/03_NL_white_Class_change_target.tiff"),
#      width=config$plotting_information$pdf_size$width, 
#      height=config$plotting_information$pdf_size$height_large,   # large! (Karte + Matrix brauchen Höhe)
#      units = "in", res = 300)

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

tiff(paste0(config$ROOT, "/", config$output_directories$for_plots,"/03_NL_white_Class_change_target.tiff"),
     width=config$plotting_information$pdf_size$width, 
     height=config$plotting_information$pdf_size$height_large,
     units = "in", res = 300)

plot(0,0, type="n", xlim=c(0,1), ylim=c(0,1), axes=FALSE, xlab="", ylab="")

# A heading
par(fig=c(0,1,0.90,0.94), new=TRUE, mar=c(0,0,0,0))
plot(0,0, type="n", xlim=c(0,1), ylim=c(0,1), xaxs="i", yaxs="i", axes=FALSE, xlab="", ylab="")
text(0.03, 0.5, "A", adj=c(0,0.5), cex=1.8, font=2)

# Map
pos_map <- c(0, 1, 0.47, 0.90)
plot_world_map_rob(coords = coords_all,
                   cluster_vals = cluster_vals_all,
                   lake_cols = map_cols,
                   year_label = "",                     # kein Legendentitel
                   number_of_cluster = length(custom_legend$labels),
                   custom_legend = custom_legend,
                   position = pos_map,
                   white_world = TRUE)

# B heading
par(fig=c(0,1,0.42,0.46), new=TRUE, mar=c(0,0,0,0))
plot(0,0, type="n", xlim=c(0,1), ylim=c(0,1), xaxs="i", yaxs="i", axes=FALSE, xlab="", ylab="")
text(0.03, 0.5, "B", adj=c(0,0.5), cex=1.8, font=2)

# Matrix
par(fig=c(0.30, 0.70, 0.00, 0.42), new=TRUE)
plot_change_matrix(
  cluster_char_first = lake_frequencies$cluster_char_first,
  cluster_char_last  = lake_frequencies$cluster_char_last,
  ncl = ncl, colors = colors_lightened,
  normalize = "row", include_diagonal = TRUE, percent_on_top = TRUE,
  cex_values = 1.3, cex_lab = 1.5, cex_axis = 1.5
)

dev.off()

tiff(paste0(config$ROOT, "/", config$output_directories$for_plots,"/03_NL_white_Class_change_target_MAP.tiff"),
     width=config$plotting_information$pdf_size$width, 
     height=config$plotting_information$pdf_size$height_small,
     units = "in", res = 300)

plot(0,0, type="n", xlim=c(0,1), ylim=c(0,1), axes=FALSE, xlab="", ylab="")

pos_map <- c(0, 1, 0, 1)
plot_world_map_rob(coords = coords_all,
                   cluster_vals = cluster_vals_all,
                   lake_cols = map_cols,
                   year_label = "",
                   number_of_cluster = length(custom_legend$labels),
                   custom_legend = custom_legend,
                   position = pos_map,
                   white_world = TRUE)

dev.off()

tiff(paste0(config$ROOT, "/", config$output_directories$for_plots,"/03_NL_white_Class_change_target_MATRIX.tiff"),
     width=12, height=11, units = "in", res = 300, compression = "lzw")

plot_change_matrix(
  cluster_char_first = lake_frequencies$cluster_char_first,
  cluster_char_last  = lake_frequencies$cluster_char_last,
  ncl = ncl, colors = colors_lightened,
  normalize = "row", include_diagonal = TRUE, percent_on_top = TRUE,
  cex_values = 1.3, cex_lab = 1.5, cex_axis = 1.5
)

dev.off()

# ---- Pie Charts changes of climate regions between first and second period ----

source("scripts/utils/vis.R")

# Palette wie gehabt (ggf. deine aufgehellte Variante hier einsetzen)
matrix_cols <- colorRampPalette(heatmap_colors$lake_cols)(100)

# ---- Transition matrices per climate zone ----
plot_change_matrix_by_group(
  group_vector       = climate$description_of_zone,
  cluster_char_first = lake_frequencies$cluster_char_first,
  cluster_char_last  = lake_frequencies$cluster_char_last,
  ncl                = ncl,
  colors             = colors_lightened,
  group_name         = "Transition matrices per climate zone",
  output_file        = paste0(config$ROOT, "/", config$output_directories$for_plots,
                              "/03_NL_ChangeMatrix_ClimateZones.tiff"),
  cb_left = 0.9,
  legend_width = 9,
  legend_mar =5,
  ncol               = 3,
  cex_values = 1.0,
  cex_lab = 1.5,
  cex_axis = 1.5
)


# ---- Transition matrices per continent ----
plot_change_matrix_by_group(
  group_vector       = continents$continent,
  cluster_char_first = lake_frequencies$cluster_char_first,
  cluster_char_last  = lake_frequencies$cluster_char_last,
  ncl                = ncl,
  colors             = colors_lightened,
  group_name         = "Transition matrices per continent",
  output_file        = paste0(config$ROOT, "/", config$output_directories$for_plots,
                              "/03_NL_ChangeMatrix_Continents.tiff"),
  cb_left = 0.9,
  legend_width = 9,
  legend_mar =5,
  ncol               = 3,
  cex_values = 1.0,
  cex_lab = 1.5,
  cex_axis = 1.5
)

# 
# # ---- Plot settings ----
# # Climate zones: use defaults 
# # Continents: adjust these values as needed
# # ---- Plot settings ----
# # Climate zones: use function defaults (already tuned)
# 
# # Continents: custom values to account for different number of rows/cell sizes
# cont_pie_settings <- list(
#   cex_main = 1.0,        # Font size of the title (e.g., "Europe (42 lakes)")
#   cex_legend = 1.7,      # Font size of the legend text
#   cex_label = 1.3,       # Font size of the subplot label (e.g., "(a)")
#   radius = 0.95,         # Pie chart radius (0-1, fraction of plot area)
#   title_adj = 0.6,       # Horizontal position of title (0=left, 0.5=center, 1=right)
#   title_line = -0.25,    # Vertical position of title (higher value = further from plot)
#   label_adj = -0.25,     # Horizontal position of subplot label (negative = further left)
#   label_line = -0.05,    # Vertical position of subplot label
#   legend_x = -2.75,       # Legend x-position in plot coordinates (pie ranges from -1 to 1)
#   legend_y = 0.8,        # Legend y-position: top edge of legend box
#   legend_pt_cex = 1.7,   # Size of colored dots in legend
#   plt = c(0.25, 0.95, 0.1, 0.9),  # Plot region within figure: c(left, right, bottom, top) as fractions
#   mar = c(0, 1, 3, 1)   # Margins in lines: c(bottom, left, top, right)
# )
# 
# cont_heatmap_settings <- list(
#   cex_axis = 0.8,        # Font size of axis tick labels (1-10)
#   cex_lab = 0.8,         # Font size of axis titles and colorbar label
#   cex_values = 1.0,      # Font size of percentage values inside heatmap cells
#   mar = c(4, 4, 1, 4)   # Margins in lines: c(bottom, left, top, right)
# )
# 
# # ---- Pie Charts changes of climate regions between first and second period ----
# pdf_climate <- setup_tiff(paste0(config$ROOT, "/", config$output_directories$for_plots, "/03_PieCharts_LakeChanges_ClimateZones.tiff"), 
#                           nrows = length(unique(climate$description_of_zone)), ncols = 2)
# state <- list(sum_change = 0, n = 1)
# all_shifts <- list()
# for (i in unique(climate$description_of_zone)){
#   lakes_oi <- which(climate$description_of_zone == i)
#   all_shifts[[i]] <- calc_shift_heatmap(lakes_oi, ncl, mode="period")
# }
# global_zlim <- range(unlist(all_shifts), na.rm = TRUE)
# 
# for (i in unique(climate$description_of_zone)){
#   lakes_oi <- which(climate$description_of_zone == i)
#   plot_pie(lakes_oi, i, state$n, ncl, lake_cols)
#   plot_shift_heatmap(all_shifts[[i]], ncl, 
#                      colors = colorRampPalette(heatmap_colors$lake_cols)(100), 
#                      "Cluster type, 2014 - 2024", "Cluster type, 2003 - 2013",
#                      zlim = global_zlim)
#   state$n <- state$n + 1
# }
# dev.off()
# 
# # ---- Pie Charts changes of continents between first and second period ----
# pdf_continent <- setup_tiff(paste0(config$ROOT, "/", config$output_directories$for_plots, "/03_PieCharts_LakeChanges_Continents.tiff"), 
#                             nrows = length(unique(continents$continent)), ncols = 2)
# state <- list(sum_change = 0, n = 1)
# all_shifts <- list()
# for (i in unique(continents$continent)){
#   lakes_oi <- which(continents$continent == i)
#   all_shifts[[i]] <- calc_shift_heatmap(lakes_oi, ncl, mode="period")
# }
# global_zlim <- range(unlist(all_shifts), na.rm = TRUE)
# for (i in unique(continents$continent)){
#   lakes_oi <- which(continents$continent == i)
#   plot_pie(lakes_oi, i, state$n, ncl, lake_cols,
#            cex_main = cont_pie_settings$cex_main,
#            cex_legend = cont_pie_settings$cex_legend,
#            cex_label = cont_pie_settings$cex_label,
#            radius = cont_pie_settings$radius,
#            title_adj = cont_pie_settings$title_adj,
#            title_line = cont_pie_settings$title_line,
#            label_adj = cont_pie_settings$label_adj,
#            label_line = cont_pie_settings$label_line,
#            legend_x = cont_pie_settings$legend_x,
#            legend_y = cont_pie_settings$legend_y,
#            legend_pt_cex = cont_pie_settings$legend_pt_cex,
#            plt = cont_pie_settings$plt,
#            mar = cont_pie_settings$mar)
#   plot_shift_heatmap(all_shifts[[i]], ncl, 
#                      colors = colorRampPalette(heatmap_colors$lake_cols)(100), 
#                      "Cluster type, 2014 - 2024", "Cluster type, 2003 - 2013",
#                      zlim = global_zlim,
#                      cex_axis = cont_heatmap_settings$cex_axis,
#                      cex_lab = cont_heatmap_settings$cex_lab,
#                      cex_values = cont_heatmap_settings$cex_values,
#                      mar = cont_heatmap_settings$mar)
#   state$n <- state$n + 1
# }
#  dev.off()
# 
# # ---- Pie Chart Climate Zones annual ----
# year1 <- 2003
# year2 <- 2004
# tiff_pie_annual <- setup_tiff(paste(config$ROOT, "/", config$output_directories$for_plots, "/03_PieCharts_LakeChangesAnnual_",year1,"vs",year2,"_climates.tiff",sep=""), 
#                               nrows = length(unique(climate$description_of_zone)), ncols = 2)
# state <- list(sum_change = 0, n = 1)
# all_shifts <- list()
# for (i in unique(climate$description_of_zone)){
#   lakes_oi <- which(climate$description_of_zone == i)
#   all_shifts[[i]] <- calc_shift_heatmap(lakes_oi, ncl, mode="annual", year1=year1, year2=year2)
# }
# global_zlim <- range(unlist(all_shifts), na.rm = TRUE)
# for (i in unique(climate$description_of_zone)){
#   lakes_oi <- which(climate$description_of_zone == i)
#   plot_pie(lakes_oi, i, state$n, ncl, lake_cols)
#   plot_shift_heatmap(all_shifts[[i]], ncl, 
#                      colors = colorRampPalette(heatmap_colors$lake_cols)(100), 
#                      paste("Cluster type,", year2), paste("Cluster type,", year1),
#                      zlim = global_zlim)
#   state$n <- state$n + 1
# }
# dev.off()
# 
# # ---- Pie Chart Continents Annual ----
# pdf_continent_annual <- setup_tiff(paste(config$ROOT, "/", config$output_directories$for_plots, "/03_PieCharts_LakeChangesAnnual_",year1,"vs",year2,"_continents.tiff",sep=""), 
#                                    nrows = length(unique(continents$continent)), ncols = 2)
# state <- list(sum_change = 0, n = 1)
# all_shifts <- list()
# for (i in unique(continents$continent)){
#   lakes_oi <- which(continents$continent == i)
#   all_shifts[[i]] <- calc_shift_heatmap(lakes_oi, ncl, mode="annual", year1=year1, year2=year2)
# }
# global_zlim <- range(unlist(all_shifts), na.rm = TRUE)
# for (i in unique(continents$continent)){
#   lakes_oi <- which(continents$continent == i)
#   plot_pie(lakes_oi, i, state$n, ncl, lake_cols,
#            cex_main = cont_pie_settings$cex_main,
#            cex_legend = cont_pie_settings$cex_legend,
#            cex_label = cont_pie_settings$cex_label,
#            radius = cont_pie_settings$radius,
#            title_adj = cont_pie_settings$title_adj,
#            title_line = cont_pie_settings$title_line,
#            label_adj = cont_pie_settings$label_adj,
#            label_line = cont_pie_settings$label_line,
#            legend_x = cont_pie_settings$legend_x,
#            legend_y = cont_pie_settings$legend_y,
#            legend_pt_cex = cont_pie_settings$legend_pt_cex,
#            plt = cont_pie_settings$plt,
#            mar = cont_pie_settings$mar)
#   plot_shift_heatmap(all_shifts[[i]], ncl, 
#                      colors = colorRampPalette(heatmap_colors$lake_cols)(100), 
#                      paste("Cluster type,", year2), paste("Cluster type,", year1),
#                      zlim = global_zlim,
#                      cex_axis = cont_heatmap_settings$cex_axis,
#                      cex_lab = cont_heatmap_settings$cex_lab,
#                      cex_values = cont_heatmap_settings$cex_values,
#                      mar = cont_heatmap_settings$mar)
#   state$n <- state$n + 1
# }
# dev.off()
# 
# # 
# 
# # ---- Plot Annual Distribution Heatmaps ----
# plot_annual_distribution(
#   group_vector = climate$description_of_zone,
#   group_name   = "Climate Zones",
#   cluster_dist = cluster_matrices$full,
#   ncl          = ncl,
#   start_year   = start_year,
#   end_year     = end_year,
#   output_file  = paste0(config$ROOT, "/", config$output_directories$for_plots,"/03_AnnualLakeDistribution_Climates.tif")
# )
# 
# plot_annual_distribution(
#   group_vector = continents$continent,
#   group_name   = "Continents",
#   cluster_dist = cluster_matrices$full,
#   ncl          = ncl,
#   start_year   = start_year,
#   end_year     = end_year,
#   output_file  = paste0(config$ROOT, "/", config$output_directories$for_plots,"/03_AnnualLakeDistribution_Continents.tif")
# )
# 
# 
