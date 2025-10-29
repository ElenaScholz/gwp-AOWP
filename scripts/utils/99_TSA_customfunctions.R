#' Auxiliary script for custom functions for TSA project
#' 
#'
#' Start date: August 30, 2019
#' 
#' RStudio version: 1.2.1335
#' R version: 3.5.3 
#' 
#' System: Windows 7 Enterprise Version 6.1
#'------------------------------------------------------------------------------
#'
#'Overwiew:
#'
#' A. Pre-processing (before Timesat)
#' B. Post-processing (after Timesat)
#' C. Miscellaneous
#' 
#'------------------------------------------------------------------------------
#
#'------------------------------------------------------------------------------


################################################################################
#'------------------------------------------------------------------------------
#' A. Pre-processing:
#'------------------------------------------------------------------------------
################################################################################


#'------------------------------------------------------------------------------
#' *ConvertSGV*
#' -----------------------------------------------------------------------------
#'
#' This function takes SGV timeseries and converts them into the right format,
#' then saves them into the respective folders with the correct name.


ConvertSGV <- function(entry, obs, ipath, sep) {
  name <- strsplit(entry, "/")[[1]][length(strsplit(entry, "/")[[1]])]
  rl <- read.delim(entry, header = T, sep = sep)
  rl[which(find_leap(rl$Date)),] <- NA
  rl <- na.omit(rl)
  
  name <- paste0(strsplit(name, "SGV-timeseries.txt"), "SGV4TS-timeseries.txt")
  folder <- strsplit(strsplit(name, "01_Surface-Water-")[[1]][2], "_")[[1]][1]
  
  yl <- ceiling(length(rl$Area)/obs)
  no_missingData <- yl*obs-length(rl$Area)
  filler <- rep(NA, no_missingData)
  
  ol <- append(rl$Area, filler)
  ol <- na_interpolation(ol)
  
  if (dir.exists(paste0(ipath, folder)) == F) {
    dir.create(paste0(ipath, folder))
  }
  
  write.table(paste0(yl, " ", obs, " ", 1),
              file = paste0(ipath, folder, "/", name),
              row.names = F, col.names = F, append = F, quote = F)
  write.table(ol, file = paste0(ipath, folder, "/", name),
              row.names = F, col.names = F, append = T)
}


#'------------------------------------------------------------------------------
#' *AggregateTS*
#' -----------------------------------------------------------------------------
#'
#' This function aggregates the existing timeseries in a folder

AggregateTS <- function(ipath, folder, pattern, obs) {
  fl <- list.files(path = paste0(ipath, "/", folder), pattern = pattern,
                   full.names = T)
  fl <- fl[grep(pattern = paste0(folder, "/", folder), fl, invert = T)]
  ll <- lapply(fl, function(x) read.delim(file = x, header = T))
  yl <- length(ll[[1]]$X16.365.1)/obs
  obs_pattern <- strsplit(pattern, "-")[[1]][1]
  obs_pattern <- strsplit(obs_pattern, "\\.")[[1]][1]
  obs_pattern
  write.table(paste0(yl, " ", obs, " ", length(ll)),
              file = paste0(ipath, "/", folder, "/", folder, "_", obs_pattern,
                            ".txt"),
              row.names = F, col.names = F, append = F, quote = F)
  lapply(ll, function(x) {
    write.table(x,
                file = paste0(ipath, "/", folder, "/", folder, "_", obs_pattern,
                              ".txt"),
                row.names = F, col.names = F, append = T)
  })
}



#'------------------------------------------------------------------------------
#' *BuildSet*
#' -----------------------------------------------------------------------------
#'
#' This function builds the setting file for Timesat

BuildSet <- function(ipath, folder, pattern, yl, obs, season, method) {
  ats <- list.files(path = paste0(ipath, "/", folder, "/"),
                    pattern = paste0(folder, "_", pattern), full.names = T)
  nts <- list.files(path = paste0(ipath, "/", folder, "/"),
                    pattern = paste0(folder, "_", pattern), full.names = F)
  nts <- lapply(nts, function(x) strsplit(x, "\\.")[[1]][1])
  for (i in 1:length(ats)) {
    # build .set file content
    set_content <-  paste0("Settings file version: 3.3
",nts[[i]] ,"      %Job_name (no blanks)
0               %Image /series mode (1/0)
0               %Trend (1/0)
0               %Use quality data (1/0)
", ats[[i]]," %Data file list/name
none  %Mask file list/name
1               %Image file type
0               %Byte order (1/0)
0 0      %Image dimension (nrow ncol)
0 0 0 0      %Processing window (start row end row start col end col)
",yl," ",obs,"           %No. years and no. points per year
-100 10000         %Valid data range (lower upper)
0 0 0      %Quality range 1 and weight
0 0 0      %Quality range 2 and weight
0 0 0      %Quality range 3 and weight
0.0             %Amplitude cutoff value
0               %Debug flag (3/2/1/0)
1 1 1           %Output files (1/0 1/0 1/0)
0               %Use land cover (1/0)
none  %Name of landcover file
1               %Spike method (3/2/1/0)
0.5             %Spike value
3.0             %STL stiffness value (1-10)
1               %No. of landcover classes
************
1               %Land cover code for class  1
",season,"             %Seasonality parameter (0-1)
1               %No. of envelope iterations (3/2/1)
2               %Adaptation strength (1-10)
0 -99999             %Force minimum (1/0) and value
1               %Fitting method (3/2/1)
1               %Weight update method
10               %Window size for Sav-Gol.
0               %Reserved
0               %Reserved
",method,"               %Season start / end method (4/3/2/1)
0.5 0.5         %Season start / end values ")
    # Write as .set
    write.table(set_content, file = paste0(ipath, "/", folder, "/", nts[[i]],
                                           ".set"),
                row.names = F, col.names = F, append = F)
  }
}


#'------------------------------------------------------------------------------
#' *FillMissingFieldData*
#' -----------------------------------------------------------------------------
#'
#' This function fills data gaps in ts of field data


FillMissingFieldData <- function(entry, years, obs, ipath, folder) {
  name <- strsplit(entry, "/")[[1]][length(strsplit(entry, "/")[[1]])]
  fi <- read.delim(entry, header = T)
  missingyears <- years*obs - length(fi[[1]])
  filler <- rep(NA, missingyears)
  ol <- append(fi[[1]], filler)
  ol <- na_interpolation(ol)
  if (dir.exists(paste0(ipath, folder)) == F) {
    dir.create(paste0(ipath, folder))
  }
  write.table(paste0(years, " ", obs, " ", 1),
              file = paste0(ipath, folder, "/", name),
              row.names = F, col.names = F, append = F, quote = F)
  write.table(ol, file = paste0(ipath, folder, "/", name),
              row.names = F, col.names = F, append = T)
}


################################################################################
#'------------------------------------------------------------------------------
#' B. Post-processing:
#'------------------------------------------------------------------------------
################################################################################


#'------------------------------------------------------------------------------
#' *ReformatTS*
#' -----------------------------------------------------------------------------
#'
#' This function changes the format of TS-outputs to a nice data frame


ReformatTS <- function(entry, lakeList) {
  df_plot <- read.delim(entry, header = T, sep = "", dec = ".") # for header
  helper <- read.delim(entry, header = F, sep = "", dec = ".") # for data
  delvec <- c(5,7,11,13,20,22) # vector of column names to be deleted
  newNames <- as.list(names(df_plot)) # extract names from df_plot
  newNames[delvec] <- NULL # set according names to NULL
  newNames <- as.character(newNames) # change to character
  helper[1,] <- NA # set col.names in temporary df to NA
  helper <- na.omit(helper) # delete NAs
  df_plot <- as.data.frame(helper[,c(1:16)]) # bring cleaned data to df_plot
  names(df_plot) <- newNames # change col.names fo df_plot
  df_plot$SoS <- DateFromFactor(df_plot$Beg., origin = "2003-01-01")
  df_plot$Peak <- DateFromFactor(df_plot$Max, origin = "2003-01-01")
  df_plot$EoS <- DateFromFactor(df_plot$End, origin = "2003-01-01")
  df_plot$Peak2EoS <- as.numeric(df_plot$EoS - df_plot$Peak)
  
  df_plot$Year <- format(df_plot$Peak, format = "%Y")
  df_plot$Seas. <- as.numeric.factor(df_plot$Seas.)  
  df_plot$Max.1 <- as.numeric.factor(df_plot$Max.1)
  df_plot$Length <- as.numeric.factor(df_plot$Length)
  df_plot$Row <- as.numeric.factor(df_plot$Row)
  ldf <- list()
  for(i in 1:max(df_plot$Row)) {
    ldf[[i]] <- df_plot[which(df_plot$Row == i),]
    ldf[[i]]$ID <- lakeList[[i]]
    ldf[[i]]$LoS_rM <- rollmean(ldf[[i]]$Length, 5, fill = NA)
    ldf[[i]]$SoS_rM <- rollmean(ldf[[i]]$SoS, 5, fill = NA)
    ldf[[i]]$EoS_rM <- rollmean(ldf[[i]]$EoS, 5, fill = NA)
    ldf[[i]]$Peak_rM <- rollmean(ldf[[i]]$Peak, 5, fill = NA)
    ldf[[i]]$Max.1_rM <- rollmean(ldf[[i]]$Max.1, 5, fill = NA)
  }
  return(ldf)
}


#'------------------------------------------------------------------------------
#' *Export2Excel_Fi*
#' -----------------------------------------------------------------------------
#' 
#' This function exports the processed Field dfs to a common excel sheet


Export2Excel_Fi <- function(inputLake, opath, folder) {
  write.xlsx(inputLake, file = paste0(opath, "/", folder, "/", folder, 
                                      "_Field.xlsx"), 
             sheetName = inputLake$ID[1], append = T)
}


#'------------------------------------------------------------------------------
#' *Export2Excel_RS*
#' -----------------------------------------------------------------------------
#' 
#' This function exports the processed GWP dfs to a common excel sheet


Export2Excel_RS <- function(inputLake, opath, folder) {
  write.xlsx(inputLake, file = paste0(opath, "/", folder, "/", folder, 
                                      "_GWP.xlsx"), 
             sheetName = inputLake$ID[1], append = T)
}


#'------------------------------------------------------------------------------
#' *plotGaugeWithPoints*
#' -----------------------------------------------------------------------------
#' 
#' This function plots timeseries of gauge values + SoS/EoS and exports to png
 

plotGaugeWithPoints <- function(q, ipath, folder, FieldPattern, unit) {
  fl <- list.files(path = paste0(ipath, folder), 
                   pattern = paste0(q$ID , "_", FieldPattern), 
                   full.names = T)
  l <- read.delim(fl)
  Lake_df <- as.data.frame(l)
  v <- c(1:lengths(Lake_df))
  Lake_df$I <- as.character(as.Date(v, origin = "2003-01-01"))
  p <- ggplot(data = Lake_df, aes(x = as.Date(I), y = X16.365.1)) +
    geom_ribbon(data = Lake_df, aes(ymin = X16.365.1 - sd(X16.365.1), 
                                    ymax = X16.365.1 + sd(X16.365.1)),
                alpha = 0.6, colour = "darkgrey", fill = "darkgrey",
                linetype = "dashed") +
    geom_line(colour = "steelblue2", size = 0.9) +
    geom_point(data = q, aes(x = SoS, y = as.numeric(as.character(Start))),
               colour = "deeppink4") +
    geom_point(data = q, aes(x = EoS, y = as.numeric(as.character(End.1))),
               colour = "#EE9E5C") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    xlab("Year") +
    ylab(paste0("Station [", unit,"]")) +
    coord_cartesian(xlim = c(as.Date("2003-01-01"), as.Date("2018-12-31"))) +
    theme(axis.text.x = element_text(size = 10)) +
    theme(axis.text.y.left = element_text(size = 10)) +
    #theme(axis.text.y.right = element_text(size = 6)) +
    theme(axis.title.x = element_text(size = 12)) +
    theme(axis.title.y = element_text(size = 12))
}


#'------------------------------------------------------------------------------
#' *plotGWPWithPoints*
#' -----------------------------------------------------------------------------
#' 
#' This function plots timeseries of GWP area + SoS/EoS and exports to png


plotGWPWithPoints <- function(q, ipath, folder, GWPPattern, unit) {
  fl <- list.files(path = paste0(ipath, folder), 
                   pattern = paste0(q$ID , "_", GWPPattern), 
                   full.names = T)
  l <- read.delim(fl)
  Lake_df <- as.data.frame(l)
  v <- c(1:lengths(Lake_df))
  Lake_df$I <- as.character(as.Date(v, origin = "2003-01-01"))
  p <- ggplot(data = Lake_df, aes(x = as.Date(I), y = X16.365.1)) +
    geom_ribbon(data = Lake_df, aes(ymin = X16.365.1 - sd(X16.365.1), 
                                    ymax = X16.365.1 + sd(X16.365.1)),
                alpha = 0.6, colour = "darkgrey", fill = "darkgrey",
                linetype = "dashed") +
    geom_line(colour = "steelblue4", size = 0.9) +
    geom_point(data = q, aes(x = SoS, y = as.numeric(as.character(Start))),
               colour = "deeppink4") +
    geom_point(data = q, aes(x = EoS, y = as.numeric(as.character(End.1))),
               colour = "#EE9E5C") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    xlab("Year") +
    ylab(paste0("Area [",unit,"]")) +
    coord_cartesian(xlim = c(as.Date("2003-01-01"), as.Date("2018-12-31"))) +
    theme(axis.text.x = element_text(size = 10)) +
    theme(axis.text.y.left = element_text(size = 10)) +
    #theme(axis.text.y.right = element_text(size = 6)) +
    theme(axis.title.x = element_text(size = 12)) +
    theme(axis.title.y = element_text(size = 12)) 
}


################################################################################
#'------------------------------------------------------------------------------
#' C. Miscellaneous:
#'------------------------------------------------------------------------------
################################################################################


#'------------------------------------------------------------------------------
#' *loadandinstall*
#' -----------------------------------------------------------------------------
#'
#' This function automatically loads all provided packages and installs those 
#' that were not installed already.


loadandinstall <- function(mypkg) {
  if (!is.element(mypkg, installed.packages()[,1])){
    install.packages(mypkg)}; 
  library(mypkg, character.only=TRUE)  
}


#'------------------------------------------------------------------------------
#' *find_leap*
#' -----------------------------------------------------------------------------
#'
#' This function searches for February 29 in a given list of dates.


find_leap <- function(i) {
  day(i) == 29 & month(i) == 2
}


#'------------------------------------------------------------------------------
#' *rm_empty*
#' -----------------------------------------------------------------------------
#'
#' This function removes empty entries in lists.


rm_empty <- function(inputList) {
  outputList <- list()
  for (i in 1:length(inputList)) {
    if((!length(inputList[[i]])) == F) {
      outputList[[i]] <- inputList[[i]]
    }
  }
  outputList <- outputList[!sapply(outputList, is.null)]
  return(outputList)
}


#'------------------------------------------------------------------------------
#' *RelationPlot*
#' -----------------------------------------------------------------------------
#' 
#' This function plots the relation of Field obs to GWP


RelationPlot <- function(q, x_label, y_label) {
  p <- ggplot(data = q, aes(x = rs, y = fi)) +
    stat_density2d(aes(fill = ..density..^0.2), geom = "tile", contour = F, 
                   n = 200) +
    scale_fill_continuous(low = "#00000000", high = "steelblue4") +
    geom_point(alpha = 0.6, shape = 20, colour = "black") +
    geom_smooth(method = 'lm', formula = y~x) +
    labs(x = x_label, y = y_label) +
    stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label..,
                                                    sep = "~~~")), parse = T) +
    coord_cartesian(xlim = c(min(q[[1]]), max(q[[1]])), 
                    ylim = c(min(q[[2]]), max(q[[2]])))
  return(p)
}


#'------------------------------------------------------------------------------
#' *RelationPrep*
#' -----------------------------------------------------------------------------
#' 
#' This function prepares the dara for printing a RelationPlot


RelationPrep <- function(ipath, folder, lake) {
  fl <- list.files(ipath, pattern = paste0(folder, "$"), full.names = T)
  rs <- list.files(fl, pattern = paste0(lake, "_SGV4TS-timeseries.txt"), 
                   full.names = T)
  fi <- list.files(fl, pattern = paste0(lake, "_ts_field.txt"), full.names = T)
  rm(fl)
  rs <- lapply(rs, function(x) read.delim(x))
  fi <- lapply(fi, function(x) read.delim(x))

  rs <- as.data.frame(rs)
  fi <- as.data.frame(fi)
  names(rs) <- "rs"
  names(fi) <- "fi"
  for (i in 1:lengths(rs)) {
    rs$Index[i] <- i
  }
  if (length(rs$Index) != 5840) {
    print(paste0("GWP Observations found: ", length(rs$Index)))
    rs$Index <- as.Date(rs$Index, origin = "2003-01-01")
    rs[which(find_leap(rs$Index)),] <- NA
    rs <- na.omit(rs)
    print(paste0("Corrected to: ", length(rs$Index)))
    temp <- rs$rs
    rm(rs)
    rs <- as.data.frame(temp)
    names(rs) <- "rs"
    for (i in 1:lengths(rs)) {
      rs$Index[i] <- i
    }
  } else {
    print(paste0("GWP Observations found: ", length(rs$Index)))
  }
  print(paste0("Field observations found: ", lengths(fi)))
  rs$Index
  if(length(rs$Index) != lengths(fi)) {
    rs <- rs %>% filter(Index <= lengths(fi))
    print(paste0("Corrected unequal data amount: GWP = ", length(rs$Index), 
                 " Field = ", lengths(fi)))
  } 
  both <- cbind(rs$rs, fi$fi)
  both <- as.data.frame(both)
  names(both) <- c("rs", "fi")
  return(both)
}


#'------------------------------------------------------------------------------
#' *RelationPrint*
#' -----------------------------------------------------------------------------
#' 
#'This function prints the RelationPlot from given folder + lake names


RelationPrint <- function(ipath, folder, lake, x_label, y_label, both) {
  png(filename = paste0(ipath, "/", folder, "/", lake, "_", x_label, "vs", 
                        y_label, ".png"),
      units = "cm", width = 20.23, height = 10.5, res = 300)
  print(RelationPlot(both, x_label, y_label))
  dev.off()
}


#'------------------------------------------------------------------------------
#' *PlotWithOutliers*
#' -----------------------------------------------------------------------------
#' 
#' This function plots a comparing boxplot of GWP and field data


PlotWithOutliers <- function(rsm, fim, y_label1, y_label2) {
  p_rs <- ggplot(data = rsm, aes(x = L1, y = value, group = L1)) +
    geom_boxplot(colour = "steelblue4", outlier.shape = "cross", 
                 outlier.size = 0.7) +
    labs(x = "Bucket", y = y_label1) +
    theme(axis.text.x = element_text(size = 6.5)) +
    theme(axis.text.y.left = element_text(size = 6.5)) +
    theme(axis.title.x = element_text(size = 8)) +
    theme(axis.title.y = element_text(size = 8))
  p_fi <- p <- ggplot(data = fim, aes(x = L1, y = value, group = L1)) +
    geom_boxplot(colour = "steelblue2", outlier.shape = "cross", 
                 outlier.size = 0.7) +
    labs(x = "Bucket", y = y_label2) +
    theme(axis.text.x = element_text(size = 6.5)) +
    theme(axis.text.y.left = element_text(size = 6.5)) +
    theme(axis.title.x = element_text(size = 8)) +
    theme(axis.title.y = element_text(size = 8))
  p <- gridExtra::grid.arrange(p_rs, p_fi, ncol = 1)
  return(p)
}


#'------------------------------------------------------------------------------
#' *PlotNoOutliers*
#' -----------------------------------------------------------------------------
#' 
#' This function plots a comparing boxplot of GWP and field data


PlotNOOutliers <- function(rsm, fim, y_label1, y_label2) {
  p_rs <- ggplot(data = rsm, aes(x = L1, y = value, group = L1)) +
    geom_boxplot(colour = "steelblue4", outlier.shape = NA) +
    coord_cartesian(ylim = c(mean(rsm$value)-0.1*mean(rsm$value), 
                             mean(rsm$value)+0.1*mean(rsm$value))) +
    labs(x = "Bucket", y = y_label1) +
    theme(axis.text.x = element_text(size = 6.5)) +
    theme(axis.text.y.left = element_text(size = 6.5)) +
    theme(axis.title.x = element_text(size = 8)) +
    theme(axis.title.y = element_text(size = 8))
  p_fi <- p <- ggplot(data = fim, aes(x = L1, y = value, group = L1)) +
    geom_boxplot(colour = "steelblue2", outlier.shape = NA) +
    coord_cartesian(ylim = c(mean(fim$value)-0.1*mean(fim$value), 
                             mean(fim$value)+0.1*mean(fim$value))) +
    labs(x = "Bucket", y = y_label2) +
    theme(axis.text.x = element_text(size = 6.5)) +
    theme(axis.text.y.left = element_text(size = 6.5)) +
    theme(axis.title.x = element_text(size = 8)) +
    theme(axis.title.y = element_text(size = 8))
  p2 <- gridExtra::grid.arrange(p_rs, p_fi, ncol = 1)
  return(p2)
}


#'------------------------------------------------------------------------------
#' *Df2df4Boxplot*
#' -----------------------------------------------------------------------------
#' 
#' This function formats a list of lists of data frames with 1 column to the
#' format needed for plotting.


Df2df4Boxplot <- function(df) {
  t <- names(df)
  t
  df_y <- split(df, (seq(nrow(df))-1) %/% 365) 
  df_y_chunks <- lapply(df_y, function(x) split(x, (seq(nrow(x))-1) %/% 5))
  df_boxplot_df <- list()
  for (l in 1:length(df_y_chunks)) {
    for (i in 1:length(df_y_chunks[[l]])) {
      if (l == 1) {
        df_boxplot_df <- list()
        df_boxplot_df[[i]] <- df_y_chunks[[l]][[i]][[1]]
      } else {
        df_boxplot_df[[i]] <- append(df_boxplot_df[[i]], 
                                     df_y_chunks[[l]][[i]][[1]])
      }
    }
  }
  dfm <- melt(df_boxplot_df, variable.names(t))
  return(dfm)
}


#'------------------------------------------------------------------------------
#' *FindLakeName*
#' -----------------------------------------------------------------------------
#' 
#' This function finds the lake name from the given folders 
#' (will return a ch.vector)


FindLakeName <- function(opath, folder, obs_pattern) {
  lake <- list.files(paste0(opath, folder), pattern = obs_pattern)
  lake <- lapply(lake, function(x) strsplit(x, paste0(folder, "_"))[[1]][2])
  lake <- lapply(lake, function(x) strsplit(x, paste0("_"))[[1]][1])
  return(lake)
}


#'------------------------------------------------------------------------------
#' *DateFromFactor*
#' -----------------------------------------------------------------------------
#' 
#' This function takes a factor and gives a date


DateFromFactor <- function(x, origin) {
  as.Date.numeric(as.numeric(as.character(x)), origin = origin)
}


#'------------------------------------------------------------------------------
#' *as.numeric.factor*
#' -----------------------------------------------------------------------------
#' 
#' This function takes a factor and gives a numeric


as.numeric.factor <- function(x) {
  as.numeric(as.character(x))
}


#'------------------------------------------------------------------------------
#' *NameCheck*
#' -----------------------------------------------------------------------------
#' 
#' This function checks for instances in two lists, in which the name checks out


NameCheck <- function(ldf_rs, n_fi) { #
  name_check <- list()
  for (i in 1:length(ldf_rs)) {
    name_check[i] <- ldf_rs[[i]]$ID[1]
  }
  
  name_check <- lapply(n_fi,function(x) str_detect(name_check, x)) 
  return(name_check)
}


#'------------------------------------------------------------------------------
#' *DeleteMatch*
#' -----------------------------------------------------------------------------
#' 
#' This function deletes a match of a string object in a list


DeleteMatch <- function(list_obj, string_obj) {
  sieve <- match(list_obj, string_obj)
  for (i in 1:length(sieve)) {
    if (is.na(sieve[i]) == F) {
      list_obj[i] <- NULL
    }
  }
  return(list_obj)
}


#'------------------------------------------------------------------------------
#' *PlotCompareObs*
#' -----------------------------------------------------------------------------
#' 
#' This function compares the two observation modes for a lake in one plot


PlotCompareObs <- function(Fi, RS) {
  result <- ggplot(data = Fi, aes(x = Peak, y = Length, colour = "Field")) +
    geom_point() +
    stat_smooth() +
    geom_point(data = RS, aes(x = Peak, y = Length, colour = "MODIS")) +
    stat_smooth(data = RS, aes(x = Peak, y = Length, colour = "MODIS")) +
    labs(colour = "Type of observation")
  
  return(result)
}


#'------------------------------------------------------------------------------
#' *SIFinder*
#' -----------------------------------------------------------------------------
#' 
#' This function looks for files that end with *SI.txt in all folders of one dir


SIFinder <- function(entry) {
  entry <- list.files(entry, full.names = T)
  entry <- entry[grep(".xlsx", entry, invert = T)]
  list.files(entry, pattern = "*SI.txt", full.names = T)
}


#'------------------------------------------------------------------------------
#' *RemoveLeap*
#' -----------------------------------------------------------------------------
#' 
#' This function looks for Feb.29. in a data frame with a date column and
#' removes them


RemoveLeap <- function(df) {
  df[find_leap(df$Date),] <- NA
  df <- na.omit(df)
  return(df)
}


#'------------------------------------------------------------------------------
#' *subYear*
#' -----------------------------------------------------------------------------
#' 
#' This function iterates through a df and finds the SI for each year


subYear <- function(table, obs, y) {
  SI <- list()
  y_seq <- seq(1, obs*y, obs)
  k <- 1
  for (i in 1:length(y_seq)) {
    t <- table[[2]][y_seq[i]:(y_seq[i]+(obs-1))]
    tmin <- min(t)
    tmax <- max(t)
    SI[[i]] <- tmax/tmin
  }
  return(SI)
}


#'------------------------------------------------------------------------------
#' *AddRowVal*
#' -----------------------------------------------------------------------------
#' 
#' This function takes a list of dfs and combines the second column in one df
#' it returns only the df where the values have been added


AddRowVal <- function(df) {
  for (l in 1:length(df)) {
    if (l == 1) {
      df[[l]][[2]] <- df[[l]][[2]]
    } else {
      df[[1]][[2]] <- df[[1]][[2]] + df[[l]][[2]]
    }
  }
  return(df[[1]])
}

#'------------------------------------------------------------------------------
#' *FolderFinder*
#' -----------------------------------------------------------------------------
#' 
#' This function finds all child folders for a given path


FolderFinder <- function(ipath) {
  ipath <- ipath
  t <- list.dirs(ipath)
  t[1] <- NA
  t <- na.omit(t)
  f <- unlist(lapply(t, function(x) strsplit(x, "//")[[1]][2]))
  return(f)
}


###----- End of script. --------------------------------------------------------


