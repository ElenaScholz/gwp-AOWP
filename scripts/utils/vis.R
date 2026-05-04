# ---- Functions  ----
library(abind)
library(maps)
library(sp)
library(rworldmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(fields)
library(clusterSim)
library(ggsci)

# --- Color Palette Setup 
create_lake_color_palette <- function(config, color_ramp = pal_flatui("aussie")(10) ) {
  # 10 Basisfarben
  color_ramp <- color_ramp  
  ramp_dev <- colorRamp(color_ramp)
  
  if (config$plotting_information$interpolate_colors) {
    # Interpolate colors based on number of clusters
    lake_cols <- rgb(ramp_dev(seq(0, 1, length.out = config$cluster_centroids$number_of_cluster)), max = 255)  
  } else {
    # Use base colors, repeat if necessary
    if (config$cluster_centroids$number_of_cluster <= length(color_ramp)) {
      lake_cols <- color_ramp[1:config$cluster_centroids$number_of_cluster]
    } else {
      lake_cols <- rep(color_ramp, ceiling(config$cluster_centroids$number_of_cluster / length(color_ramp)))[1:config$cluster_centroids$number_of_cluster]
    }
  }
  
  return(list(lake_cols=lake_cols,
              ramp_dev = ramp_dev))
}

#' Plot Cluster Centers
#' 
#' Creates a multi-panel plot showing the temporal patterns of cluster centers.
#' 
#' @param cnts Matrix of cluster centers (time points x clusters)
#' @param number_of_cluster Number of clusters to plot
#' @param nrows Number of rows in the plot grid (default: 2)
#' @param ncols Number of columns in the plot grid (default: 5)
#' @param xlim Vector of x-axis limits (default: c(0, 365) for days)
#' @param ylim Vector of y-axis limits (default: c(0, 1))
plot_cluster_centers <- function(cnts, number_of_cluster, nrows = 2, ncols = 5, xlim = c(0,365), ylim = c(0,1)) {
  par(mfrow = c(nrows, ncols))
  for(cla in 1:number_of_cluster) {
    plot(cnts[, cla], type = "l", lwd = 2, lend = 2, 
         xlim = xlim, ylim = ylim,
         main = paste("Cluster", cla),
         xlab = "Day of Year", ylab = "Value")
  }
}

reorder_clusters <- function(cnts, clas, new_order){
  if (length(new_order) != ncol(cnts)) {
    stop("new_order length must match number of clusters")
  }
  
  # Reorder cluster centers
  cnts_reordered <- cnts[, new_order]
  
  # Create new cluster assignments
  clas_new <- rep(NA, length(clas))
  for (j in seq_along(new_order)) {
    old_cluster <- new_order[j]
    lakes_in_cluster <- which(clas == old_cluster)
    clas_new[lakes_in_cluster] <- j
  }
  
  return(list(cnts = cnts_reordered, clas = clas_new))
}

# cluster_dist
reorganize_cluster_assignments <- function(clas, n_lakes, n_years) {
  if (length(clas) != n_lakes * n_years) {
    stop("Length of clas must equal n_lakes * n_years")
  }
  
  half_years <- floor(n_years / 2)
  
  # Initialize matrices
  cluster_dist <- matrix(NA, nrow = n_lakes, ncol = n_years)
  cluster_dist_first <- matrix(NA, nrow = n_lakes, ncol = half_years)
  cluster_dist_last <- matrix(NA, nrow = n_lakes, ncol = half_years)
  
  # Fill matrices for each lake
  for (i in 1:n_lakes) {
    # Calculate indices for this lake's time series
    start_ind <- (i * n_years + 1) - n_years
    end_ind <- i * n_years
    
    # Full period
    cluster_dist[i, ] <- clas[start_ind:end_ind]
    
    # First period
    start_ind_first <- start_ind
    end_ind_first <- end_ind - half_years
    cluster_dist_first[i, ] <- clas[start_ind_first:end_ind_first]
    
    # Second period
    start_ind_last <- start_ind + half_years
    end_ind_last <- end_ind
    cluster_dist_last[i, ] <- clas[start_ind_last:end_ind_last]
  }
  
  return(list(
    full = cluster_dist,
    first_period = cluster_dist_first,
    second_period = cluster_dist_last
  ))
}

# cluster_char
get_most_frequent_lake_class <- function(cluster_matrices){
  cluster_char <- matrix(NaN,nrow=nrow(cluster_matrices$full),ncol=2)
  cluster_char_first <- matrix(NaN,nrow=nrow(cluster_matrices$full),ncol=2)
  cluster_char_last <- matrix(NaN,nrow=nrow(cluster_matrices$full),ncol=2)  
  
  
  for(i in 1:nrow(cluster_matrices$full)){
    ## frequency analysis using table funtion for entire, first and last period
    # Häufigste Klassenzuordnung (Cluster)
    # 
    # Anzahl unterschiedlicher Klassen, die dieser See jemals hatte
    lake_count_total <- as.data.frame(table(cluster_matrices$full[i,]))
    lake_count_first <- as.data.frame(table(cluster_matrices$first_period[i,]))
    lake_count_last <- as.data.frame(table(cluster_matrices$second_period[i,]))
    
    ## classify lakes by max class assignement 
    ## Hier wird die am häufigsten auftretende Klasse (Cluster) gewählt → „Dominant Class“ eines Sees.
    cluster_char[i,1] <- as.integer(as.vector(lake_count_total$Var1[which(lake_count_total$Freq==max(lake_count_total$Freq))[1]]))
    cluster_char_first[i,1] <- as.integer(as.vector(lake_count_first$Var1[which(lake_count_first$Freq==max(lake_count_first$Freq))[1]]))
    cluster_char_last[i,1] <- as.integer(as.vector(lake_count_last$Var1[which(lake_count_last$Freq==max(lake_count_last$Freq))[1]]))
    ## number of different cluster assignments
    ## Zusätzlich wird gezählt, wie viele unterschiedliche Cluster ein See über die Zeit „durchlaufen“ hat.
    cluster_char[i,2] <- length(unique(cluster_matrices$full[i,]))
    cluster_char_first[i,2] <- length(unique(cluster_matrices$first_period[i,]))
    cluster_char_last[i,2] <- length(unique(cluster_matrices$second_period[i,]))
  }
  
  ## reduce Number of different lake types by 1.
  ## Offensichtlich wird die Anzahl „Seen-Typen“ um 1 reduziert 
  ##– vermutlich um den Fall „nur ein Cluster“ als „0 Wechsel“ zu zählen.
  cluster_char[,2] <- cluster_char[,2]-1
  
  # 20.04.2026 die 2 Zeilen unten eingefügt - es könnte sein, dass hier ein Fehler in den Seezuordnungen stattgefunden hat. 
  cluster_char_first[,2] <- cluster_char_first[,2]-1
  cluster_char_last[,2] <- cluster_char_last[,2]-1
  
  return(list(
    cluster_char_total = cluster_char,
    cluster_char_first = cluster_char_first,
    cluster_char_last = cluster_char_last
  ))
}

get_time_axis <- function(){
  x_recs <- seq(ISOdate(2001,01,01),ISOdate(2001,12,31),by="days")
  x_ticks <- seq(ISOdate(2001,01,01),ISOdate(2001,12,31),by="months")
  x_labs <- seq(ISOdate(2001,01,15),ISOdate(2001,12,15),by="months")
  
  
  return(list(x_records = x_recs,
              x_ticks = x_ticks,
              x_labs = x_labs))
}

plot_class_panel <- function(cla, position, x_recs, x_ticks, x_labs, clas, 
                             normalized_lake_dat, centroids, lake_colors,
                             bg_color = NULL, bg_alpha = 1){
  
  par(fig=position, mar=c(4,4,3,1), new=TRUE)
  
  # Empty plot
  plot(0,0,type="n", xlim=range(x_recs), ylim=c(0,1),
       xlab="", ylab="", axes=FALSE, main=paste("Cluster #", cla, sep=""),
       cex.main=2, cex.axis=1.5)
  
  # Hintergrundfarbe hinzufügen (falls gewünscht)
  if (!is.null(bg_color)) {
    # Erstelle Farbe mit Transparenz
    bg_col_with_alpha <- adjustcolor(bg_color, alpha.f = bg_alpha)
    
    # Zeichne Rechteck über gesamte Plot-Fläche
    rect(xleft = par("usr")[1], 
         ybottom = par("usr")[3],
         xright = par("usr")[2], 
         ytop = par("usr")[4],
         col = bg_col_with_alpha, 
         border = NA)
  }
  
  # Axes
  axis(1, at=x_ticks, labels=FALSE, lwd=1.5, lend=2, tcl=0.6)
  axis(3, at=x_ticks, labels=FALSE, lwd=1.5, lend=2, tcl=0.6)
  axis(1, at=x_labs, labels=format(x_labs, "%m"), las=3,
       mgp=c(3,0.5,0), tcl=0, cex.axis=1.1, font.axis=2)
  
  axis(2, at=seq(0,1,.25), labels=TRUE, lwd=1.5, lend=2, tcl=0.6,
       cex.axis=1.1, font.axis=2)
  axis(4, at=seq(0,1,.25), labels=FALSE, lwd=1.5, lend=2, tcl=0.6)
  
  mtext("Month of year", side=1, line=2.5, cex=1.5, font=2)
  mtext("rel. lake extent", side=2, line=2.5, cex=1.5, font=2)
  box(lwd=1.5)
  
  # Shaded ±SD band and mean line
  lakes_oi <- which(clas == cla)
  lakes_sd <- apply(normalized_lake_dat[lakes_oi,], 2, sd)
  
  lake_range_pos <- pmin(cnts[,cla] + lakes_sd, 1)
  lake_range_neg <- pmax(cnts[,cla] - lakes_sd, 0)
  
  polygon(x=c(x_recs, rev(x_recs)),
          y=c(lake_range_pos, rev(lake_range_neg)),
          col="grey", border=NA)
  lines(x=x_recs, y=cnts[,cla], col=lake_cols[cla], lwd=4, lend=2)
}

plot_world_map <- function(coords, cluster_vals, lake_cols, year_label,
                           number_of_cluster, custom_par = NULL,
                           custom_legend = NULL) {
  if(!is.null(custom_par)) {
    par(custom_par)
  } else {
    par(fig=c(0,1, 5/20.42, 15.42/20.42), mar=rep(2,4), new=TRUE)
  }
  
  plot(0,0,type="n", xlim=c(-180,180), ylim=c(-60,90), axes=FALSE,
       yaxs="i", xaxs="i", xlab="", ylab="")
  map("world", add=TRUE, fill=TRUE, col="gray80")
  
  points(x=coords[,"Lat"], y=coords[,"Lon"],
         col=lake_cols[cluster_vals], cex=1, pch=16)
  
  # Legende
  if(!is.null(custom_legend)) {
    legend(x=-170, y=20, legend=custom_legend$labels, pch=16, pt.cex=3,
           col=custom_legend$colors, ncol=2, cex=2,
           title=year_label, title.adj=.5, text.font=2, bg="ivory")
  } else {
    # Original Legende
    legend(x=-170, y=20, legend=1:number_of_cluster, pch=16, pt.cex=3,
           col=lake_cols, ncol=2, cex=2,
           title=year_label, title.adj=.5, text.font=2, bg="ivory")
  }
}

plot_world_map_rob <- function(coords, cluster_vals, lake_cols, year_label, 
                               number_of_cluster, custom_par = NULL, 
                               custom_legend = NULL, position = NULL,
                               label_start = 1) {
  library(sf)
  library(ggplot2)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(grid)
  
  # Get world map
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  # Entferne Antarktis
  world <- world[world$name != "Antarctica", ]
  # Transform world to Robinson
  world_robin <- sf::st_transform(world, crs = "+proj=robin")
  
  # Convert coords to sf points (Lat=x, Lon=y as per your note)
  points_sf <- sf::st_as_sf(
    data.frame(
      x = coords[, "Lat"],
      y = coords[, "Lon"],
      cluster = cluster_vals
    ),
    coords = c("x", "y"),
    crs = 4326
  )
  
  # Transform points to Robinson
  points_robin <- sf::st_transform(points_sf, crs = "+proj=robin")
  
  # Create plot
  p <- ggplot() +
    geom_sf(data = world_robin, fill = "gray80", color = "gray90", linewidth = 0.2) +
    geom_sf(data = points_robin, aes(color = factor(cluster)), size = 3) +
    coord_sf(expand = FALSE)+
    theme_void() +
    theme(
      legend.position = c(0.05, 0.5),
      legend.background = element_rect(fill = "white", color = "white", linewidth = 1),
      legend.title = element_text(face = "bold", hjust = 0.5, size = 20),
      legend.text = element_text(face = "bold", size = 18),
      legend.key.size = unit(2.75, "lines"),
      legend.key = element_rect(fill = "white")
    )
  
  # Add color scale
  if (!is.null(custom_legend)) {
    p <- p + scale_color_manual(
      values = custom_legend$colors,
      labels = custom_legend$labels,
      name = year_label
    ) +
      guides(color = guide_legend(ncol = 1, override.aes = list(size = 6)))
  } else {
    p <- p + scale_color_manual(
      values = lake_cols,
      labels = paste("#", label_start:(label_start + number_of_cluster - 1)),
      name = year_label
    ) +
      guides(color = guide_legend(ncol = 1, override.aes = list(size = 6)))
  }
  
  # Use position if provided, otherwise use default
  if (!is.null(position)) {
    vp <- grid::viewport(x = (position[1] + position[2])/2,
                         y = (position[3] + position[4])/2,
                         width = position[2] - position[1],
                         height = position[4] - position[3])
  } else {
    vp <- grid::viewport(x = 0.5, y = (3.5/20.42 + 12.5/20.42)/2,
                         width = 1, height = (12.5 - 3.5)/20.42)
  }
  
  print(p, vp = vp, newpage = FALSE)
}


create_classification_summary <- function(cluster_char, cluster_char_first, cluster_char_last, coords, climate){
  change_id <- rep(0,nrow(cluster_char_first))
  recs_oi <- which(cluster_char_first[,1] != cluster_char_last[,1])
  change_id[recs_oi] <- 1
  
  classification_summary_df <- data.frame(x = coords['Lat'], y = coords['Lon'], ID = 1:nrow(coords),
                                          LakeClass_full=cluster_char[,1],
                                          LakeClass_first=cluster_char_first[,1],
                                          LakeClass_last=cluster_char_last[,1],
                                          LakeClass_variability=cluster_char_first,
                                          change_id=change_id,
                                          KG_climate=climate$description_of_zone
  )
  
  return(classification_summary_df)
}

create_classification_summary_new <- function(cluster_char, cluster_char_first, cluster_char_last, coords, climate){
  change_id <- rep(0, nrow(cluster_char_first))
  recs_oi <- which(cluster_char_first[,1] != cluster_char_last[,1])
  change_id[recs_oi] <- 1
  
  classification_summary_df <- data.frame(
    x = coords['Lat'], 
    y = coords['Lon'], 
    ID = 1:nrow(coords),
    LakeClass_full = cluster_char[,1],
    LakeClass_first = cluster_char_first[,1],
    LakeClass_last = cluster_char_last[,1],
    LakeClass_variability_full = cluster_char[,2],
    LakeClass_variability_first = cluster_char_first[,2],
    LakeClass_variability_last = cluster_char_last[,2],
    change_id = change_id,
    KG_climate = climate$description_of_zone
  )
  
  return(classification_summary_df)
}

### 1. PDF + layout setup
setup_pdf <- function(filename, nrows=5, ncols=2) {
  pdf(filename, paper="a4", width=9, height=13)
  par(mfrow=c(nrows,ncols), mar=rep(2,4))
  return(list(sum_change=0, n=1))
}

setup_tiff <- function(filename, nrows = 5, ncols = 2, width = 9, height = 13, res = 300) {
  # Open a TIFF graphics device
  tiff(filename, width = width, height = height, units = "in", res = res, compression = "lzw")

  # Layout setup (same as before)
  par(mfrow = c(nrows, ncols), mar = rep(2, 4))

  # Return same structure for compatibility
  return(list(sum_change = 0, n = 1))
}
plot_pie <- function(lakes_oi, region_name, n, ncl, lake_cols, 
                     cluster_char = lake_frequencies$cluster_char_total,
                     cex_main = 1.0, cex_legend = 1.5, cex_label = 1.5,
                     radius = 0.85,
                     title_adj = 0.6, title_line = -0.25,
                     label_adj = -0.25, label_line = -0.05,
                     legend_x = -2.2, legend_y = 0.7, legend_pt_cex = 2,
                     plt = c(0.25, 0.95, 0.1, 0.9),
                     mar = c(0, 1, 3, 1)) {
  
  dummy_table <- data.frame(Var1=1:ncl, Freq=rep(0,ncl))
  pieDAT_full <- dummy_table
  for(cl in 1:ncl){
    pieDAT_full$Freq[cl] <- length(which(cluster_char[lakes_oi,1]==cl))
  }
  
  par(mar=mar, xpd=TRUE, plt=plt)
  pie(pieDAT_full$Freq, col=lake_cols, labels="",
      cex.main=cex_main, font.main=4,
      radius=radius)
  
  mtext(paste(region_name, " (", length(lakes_oi), " lakes)", sep=""), 
        side=3, adj=title_adj, line=title_line, cex=cex_main, font=2)
  mtext(paste("(", letters[n], ")", sep=""), 
        side=3, adj=label_adj, cex=cex_label, font=2, line=label_line)
  
  legend(x=legend_x, y=legend_y, legend=1:ncl, pch=16, pt.cex=legend_pt_cex, 
         col=lake_cols, ncol=2, title="Class number", title.adj=.5, 
         text.font=2, bg="ivory", xpd=TRUE, cex=cex_legend)
}

plot_shift_heatmap <- function(shift_heat_map, ncl, colors, xlab, ylab, 
                               zlim = NULL, cex_axis = 1.0, cex_lab = 1.0, 
                               cex_values = 1.2, mar = c(4, 4, 1, 4)) {
  par(mar=mar)
  if (is.null(zlim)) {
    zlim <- range(shift_heat_map, na.rm = TRUE)
  }
  image.plot(x=1:ncl, y=1:ncl, z=t(shift_heat_map), col=colors, 
             xlab="", ylab="", axes=F, zlim=zlim,
             xlim=c(0.5, ncl+0.5), ylim=c(0.5, ncl+0.5))
  axis(1, at=1:ncl, lwd=2, lend=2, cex.axis=cex_axis, font=2, mgp=c(3,.85,0))
  axis(2, at=1:ncl, lwd=2, lend=2, cex.axis=cex_axis, font=2, mgp=c(3,.85,0))
  mtext("class changes [%]", side=4, line=.75, cex=cex_lab, font=2)
  mtext(xlab, side=1, line=2.2, cex=cex_lab, font=2)
  mtext(ylab, side=2, line=2.5, cex=cex_lab, font=2)
  box(lwd=2)
  for(i in 1:nrow(shift_heat_map)){
    for(j in 1:ncol(shift_heat_map)){
      text(x=j, y=i, labels=shift_heat_map[i,j], cex=cex_values)
    }
  }
}

calc_shift_heatmap <- function(lakes_oi, ncl, mode = c("period", "annual"), 
                               year1 = NULL, year2 = NULL, 
                               cluster_char_list = lake_frequencies, 
                               cluster_dist_list = cluster_matrices) {
  shift_heat_map <- matrix(0, ncol = ncl, nrow = ncl)
  cluster_char_first <- cluster_char_list$cluster_char_first
  cluster_char_last <- cluster_char_list$cluster_char_last
  cluster_dist <- cluster_dist_list$full
  
  for (i in 1:ncl) {
    for (j in 1:ncl) {
      if (mode == "period") {
        last_oi <- which(cluster_char_first[lakes_oi, 1] == i &
                           cluster_char_last[lakes_oi, 1] == j)
      } else if (mode == "annual") {
        last_oi <- which(cluster_dist[lakes_oi, year1 - 2002] == i &
                           cluster_dist[lakes_oi, year2 - 2002] == j)
      }
      shift_heat_map[i, j] <- shift_heat_map[i, j] + length(last_oi)
      if (i == j) { shift_heat_map[i, j] <- NA }
    }
  }
  shift_heat_map <- round(shift_heat_map / length(lakes_oi) * 100, 1)
  return(shift_heat_map)
}
# 
# plot_pie <- function(lakes_oi, region_name, n, ncl, lake_cols, 
#                      cluster_char = lake_frequencies$cluster_char_total,
#                      cex_main = 1.5, cex_legend = 1.5, cex_label = 1.5) {
#   dummy_table <- data.frame(Var1=1:ncl, Freq=rep(0,ncl))
#   
#   pieDAT_full <- dummy_table
#   
#   for(cl in 1:ncl){
#     pieDAT_full$Freq[cl] <- length(which(cluster_char[lakes_oi,1]==cl))
#   }
#   
#   # Erweitere Plotbereich für Legende
#   #par(mar = c(bottom, left, top, right), xpd = allows to draw outside the plot region, plt=c(left, right, bottom, top in %))
#   par(mar=c(0, 1, 3, 1), xpd=TRUE, plt = c(0.25, 0.95, 0.1, 0.9))
# 
#   pie(pieDAT_full$Freq, col=lake_cols, labels="",
#       #main=paste(region_name, " (", length(lakes_oi), " lakes)", sep=""),
#       cex.main=cex_main, font.main=4, # title font size
#       radius = 0.85)  # adjust radius to change pie size
#  
#    # for pie
#   mtext(paste(region_name, " (", length(lakes_oi), " lakes)", sep=""), 
#         side=3, adj = 0.6, line=-0.25, cex=cex_main, font=2)
#   # adds subplot labels
#   mtext(paste("(", letters[n], ")", sep=""), side=3, adj=-0.25, cex=cex_label, font=2, line = -0.05) # side = top margin, adj = 0 means left aligned
#   
#   # absolute positions in plot coordinates 
#   legend(x=-2.2, y=0.7, legend=1:ncl, pch=16, pt.cex=2, col=lake_cols,
#          ncol=2, title="Class number", title.adj=.5, text.font=3,
#          bg="ivory", xpd=TRUE, cex=cex_legend)
#   
# 
# }
# 
# 
# ### 3. Shift heatmap calculation
# calc_shift_heatmap <- function(lakes_oi, ncl, mode=c("period","annual"), 
#                                year1=NULL, year2=NULL, 
#                                cluster_char_list = lake_frequencies, cluster_dist_list = cluster_matrices) {
#   shift_heat_map <- matrix(0, ncol=ncl, nrow=ncl)
#   cluster_char_first <-  cluster_char_list$cluster_char_first
#   cluster_char_last <-  cluster_char_list$cluster_char_last
#   cluster_dist <-  cluster_dist_list$full
#   for(i in 1:ncl){
#     for(j in 1:ncl){
#       if(mode=="period"){
#         last_oi <- which(cluster_char_first[lakes_oi,1]==i &
#                            cluster_char_last[lakes_oi,1]==j)
#       } else if(mode=="annual"){
#         last_oi <- which(cluster_dist[lakes_oi, year1-2002]==i &
#                            cluster_dist[lakes_oi, year2-2002]==j)
#       }
#       shift_heat_map[i,j] <- shift_heat_map[i,j] + length(last_oi)
#       if(i==j){ shift_heat_map[i,j] <- NA }
#     }
#   }
#   shift_heat_map <- round(shift_heat_map/length(lakes_oi)*100,1)
#   return(shift_heat_map)
# }
# 
# ### 4. Heatmap plotting
# plot_shift_heatmap <- function(shift_heat_map, ncl, colors, xlab, ylab, 
#                                zlim = NULL, cex_axis = 1.0, cex_lab = 1.0, 
#                                cex_values = 1.2) {
#   par(mar=c(4,4,1,4))
#   #ar(mar=c(5,5,1,4))
#   if (is.null(zlim)) {
#     zlim <- range(shift_heat_map, na.rm = TRUE)
#   }
#   # image.plot(x=1:ncl, y=1:ncl, z=t(shift_heat_map), col=colors, 
#   #            xlab="", ylab="", axes=F, zlim=zlim)
#   image.plot(x=1:ncl, y=1:ncl, z=t(shift_heat_map), col=colors, 
#              xlab="", ylab="", axes=F, zlim=zlim,
#              xlim=c(0.5, ncl+0.5), ylim=c(0.5, ncl+0.5))
#   axis(1, at=1:ncl, lwd=2, lend=2, cex.axis=cex_axis, font=2, mgp=c(3,.85,0))
#   axis(2, at=1:ncl, lwd=2, lend=2, cex.axis=cex_axis, font=2, mgp=c(3,.85,0))
#   mtext("class changes [%]", side=4, line=.75, cex=cex_lab, font=2)
#   mtext(xlab, side=1, line=2.2, cex=cex_lab, font=2)
#   mtext(ylab, side=2, line=2.5, cex=cex_lab, font=2)
#   box(lwd=2)
#   for(i in 1:nrow(shift_heat_map)){
#     for(j in 1:ncol(shift_heat_map)){
#       text(x=j, y=i, labels=shift_heat_map[i,j], cex=cex_values)
#     }
#   }
# }

### Function to plot annual frequency anomalies for lake types
# group vecotr = climate or coontinents
plot_annual_distribution <- function(group_vector, group_name, 
                                     cluster_dist, ncl, 
                                     start_year, end_year, 
                                     output_file, 
                                     color_ramp = colorBlindness::Blue2Orange10Steps){
  
  years <- seq(start_year, end_year)
  nyears <- length(years)
  unique_groups <- unique(group_vector)
  
  tiff(output_file, width=10, height=12.5, units="in", res=300)
  par(mfrow = c(length(unique_groups), 1), mar=c(4,4,2,2))

  
  
  for(i in seq_along(unique_groups)){
    
    lakes_oi <- which(group_vector == unique_groups[i])
    
    # Annual frequencies
    annual_freqs <- matrix(NaN, ncol=nyears, nrow=ncl)
    for(j in 1:nyears){
      annual_freqs[, j] <- hist(cluster_dist[lakes_oi, j],
                                breaks=seq(0.5, ncl+0.5),
                                plot=FALSE)$counts
    }
    
    # Relative anomalies (deviation from mean)
    annual_freqs_rel <- matrix(NaN, ncol=nyears, nrow=ncl)
    for(cla in 1:ncl){
      annual_freqs_rel[cla, ] <- annual_freqs[cla, ] - mean(annual_freqs[cla, ])
    }
    
    # # Color scale - GEÄNDERT
    # minmax <- ceiling(max(abs(range(annual_freqs_rel)))/10)*10
    # hist_anom_breaks <- seq(-minmax, minmax, length=50)
    # 
    # # Verwende Blue2Orange10Steps Palette
    # color_ramp <- color_ramp
    # hist_anom_ramp <- colorRamp(color_ramp)
    # hist_anom_cols <- rgb(hist_anom_ramp(seq(0, 1, length=length(hist_anom_breaks)-1)), max=255)
    # 
    
        # Color scale
    minmax <- ceiling(max(abs(range(annual_freqs_rel)))/10)*10
    hist_anom_breaks <- seq(-minmax, minmax, length=50)
    hist_anom_cols <- c("#FE650B","#FED474","white","#A1EFFE","#0053FE")
    hist_anom_ramp <- colorRamp(hist_anom_cols)
    hist_anom_cols <- rgb(hist_anom_ramp(seq(0, 1, length=length(hist_anom_breaks)-1)), max=255)
    # Heatmap
    image.plot(x=years, y=1:ncl, z=t(annual_freqs_rel),
               breaks=hist_anom_breaks, col=hist_anom_cols,
               xlab="", ylab="")
    
    title(paste(unique_groups[i], " (", length(lakes_oi), " lakes)", sep=""), cex.main=1)
    mtext("year", side=1, line=2.2, cex=.8, font=2)
    mtext("frequency anomaly", side=4, line=.9, cex=.8, font=2)
    mtext("class #", side=2, line=2.5, cex=.8, font=2)
    
    # Add counts to heatmap
    for(n in 1:ncl){
      for(m in 1:nyears){
        text(x=years[m], y=n, adj=c(0.5,0.5), labels=annual_freqs[n,m], cex=.8)
      }
    }
    
    # Grid lines
    for(n in 1:ncl){
      lines(x=c(start_year-3, end_year), y=rep(n,2)-.5, lwd=1, col="grey")
    }
    for(y in (start_year+0.5):(end_year+0.5)){
      lines(x=rep(y,2), y=c(0,ncl+1), lwd=1, col="grey")
    }
    box(lwd=2, col="black")
  }
  
  dev.off()
  message("✅ Plot saved to: ", output_file)
}