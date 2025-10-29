# =============================
# Climate zone + location joiner



# NOTE: h09v07_9798_1 lies in the waterarea of koeppen-geiger and therefore needs to be classified manually
# =============================
rm(list=ls())


library(sf)
library(dplyr)

# ---- Configuration ----
config <- list(
  gpkg_file        = "T:/DLR/PCA-Analysis/gwp_HylaksWithClimateZone.gpkg",
  coords_file      = "T:DLR/PCA-Analysis/CluDat/all_coordinates_complete.txt",
  output_climate   = "T:DLR/PCA-Analysis/CluDat/Points_Climate.txt",
  output_location  = "T:DLR/PCA-Analysis/CluDat/Points_Cont.txt",
  coord_id_column  = "prefix_id",   # ID column in coords file
  gpkg_id_column   = "id",          # ID column in gpkg
  gpkg_lat_column  = "latitude",    # may adapt if naming changes
  gpkg_lon_column  = "longitude"
)


# ---- Functions ----

# 1. Build Köppen-Geiger mapping
build_koeppengeiger_mapping <- function() {
  data.frame(
    PixelValue = 1:30,
    Class = c(
      "Af","Am","Aw","BWh","BWk","BSh","BSk",
      "Csa","Csb","Csc","Cwa","Cwb","Cwc",
      "Cfa","Cfb","Cfc","Dsa","Dsb","Dsc","Dsd",
      "Dwa","Dwb","Dwc","Dwd","Dfa","Dfb","Dfc","Dfd",
      "ET","EF"
    ),
    Description = c(
      rep("Equatorial", 3),
      rep("Arid", 4),
      rep("Warm temperate", 9),
      rep("Snow", 12),
      rep("Polar", 2)
    ),
    stringsAsFactors = FALSE
  )
}

# 2. Enrich gpkg points with climate info
enrich_with_climate <- function(points, mapping, gpkg_id_col) {
  points$id_clean <- gsub("_coordinates$", "", points[[gpkg_id_col]])
  points %>%
    left_join(mapping, by = c("climateZone1" = "PixelValue"))
}

# 3. Create climate zones dataframe (for export)
make_climateZones <- function(points, config) {
  points %>%
    st_drop_geometry() %>%
    select(
      id = id_clean,
      lat = all_of(config$gpkg_lat_column),
      lon = all_of(config$gpkg_lon_column),
      climate_zone_code = climateZone1,
      description_of_zone = Description
    )
}

# 4. Create location dataframe (for export)
make_locations <- function(points, config) {
  points %>%
    st_drop_geometry() %>%
    select(
      id = id_clean,
      lat = all_of(config$gpkg_lat_column),
      lon = all_of(config$gpkg_lon_column),
      country = Country,
      continent = Continent
    )
}

# 5. Merge with coordinate file and save
merge_and_save <- function(base_coords, df, id_col, keep_cols, output_path) {
  merged <- merge(base_coords, df, by = "id", all.x = TRUE)
  subset <- merged[keep_cols]
  write.table(subset, file = output_path,
              sep = ";", row.names = FALSE, quote = FALSE)
  message("Saved: ", output_path, " (", nrow(subset), " rows)")
  invisible(subset)
}


# ---- Workflow ----

# Load gpkg
points <- st_read(config$gpkg_file)

# Build mapping and enrich
kg_mapping <- build_koeppengeiger_mapping()
points_enriched <- enrich_with_climate(points, kg_mapping, config$gpkg_id_column)

# Load coordinates file
coords <- read.delim(config$coords_file, header = TRUE, sep = ";", dec = ".")
coords$id <- coords[[config$coord_id_column]]

# Create exports
climateZones <- make_climateZones(points_enriched, config)
locations    <- make_locations(points_enriched, config)

# Merge + Save
subset_climateZones <- merge_and_save(coords, climateZones, "id",
                                      c("id", "Lat", "Lon", "climate_zone_code", "description_of_zone"),
                                      config$output_climate)

subset_locations <- merge_and_save(coords, locations, "id",
                                   c("id", "Lat", "Lon", "country", "continent"),
                                   config$output_location)

# Quick checks
head(subset_climateZones)
head(subset_locations)




#' #' Prerequesites: A gpkg file containing 
#' #' 1. the gwp ids, 
#' #' 2. the hylak ids
#' #' 3. Country and Continent Information
#' #' 4. and the assigned values to the Koeppen Geiger raster map
#' #' 
#' #' TODO: To create such a file run Python Skript X
#' #' 
#' 
#' 
#' 
#' 
#' 
#' gwp_gpkg = "T:/DLR/Analysis2/PCA/gwp_HylaksWithClimateZone.gpkg"
#' 
#' library(sf)
#' library(dplyr)
#' 
#' points = st_read(gwp_gpkg)
#' 
#' 
#' 
#' # Hylak ID 9798 = ClimateZOne 1: 1
#' points$climateZone1[points$Hylak_id == 9798] <- 1
#' colnames(points)
#' str(points)
#' 
#' # 3. Köppen-Geiger Mapping erstellen
#' 
#' # Create dataframe in R
#' koeppengeiger_mapping <- data.frame(
#'   PixelValue = 1:30,
#'   Class = c(
#'     "Af", 
#'     "Am", 
#'     "Aw", 
#'     "BWh", 
#'     "BWk", 
#'     "BSh",
#'     "BSk",
#'     "Csa",
#'     "Csb",
#'     "Csc", 
#'     "Cwa",
#'     "Cwb", 
#'     "Cwc",
#'     "Cfa",
#'     "Cfb", 
#'     "Cfc",
#'     "Dsa",
#'     "Dsb",
#'     "Dsc", 
#'     "Dsd",
#'     "Dwa",
#'     "Dwb",
#'     "Dwc",
#'     "Dwd",
#'     "Dfa",
#'     "Dfb",
#'     "Dfc",
#'     "Dfd",
#'     "ET",
#'     "EF"
#'   ),
#'   Description = c(
#'     "Equatorial",
#'     "Equatorial",
#'     "Equatorial",
#'     "Arid",
#'     "Arid",
#'     "Arid",
#'     "Arid",
#'     "Warm temperate",
#'     "Warm temperate",
#'     "Warm temperate",
#'     "Warm temperate",
#'     "Warm temperate",
#'     "Warm temperate",
#'     "Warm temperate",
#'     "Warm temperate",
#'     "Warm temperate",
#'     "Snow",
#'     "Snow",
#'     "Snow",
#'     "Snow",
#'     "Snow",
#'     "Snow",
#'     "Snow",
#'     "Snow",
#'     "Snow",
#'     "Snow",
#'     "Snow",
#'     "Snow",
#'     "Polar",
#'     "Polar"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#' 
#' 
#' 
#' # Mapping anzeigen
#' print(koeppengeiger_mapping)
#' 
#' # Join durchführen
#' points_mit_klima <- points %>%
#'   left_join(koeppengeiger_mapping, by = c("climateZone1" = "PixelValue"))
#' 
#' # Ergebnis überprüfen
#' colnames(points_mit_klima)
#' table(points_mit_klima$klasse)
#' 
#' # Neue Spalten anzeigen
#' head(points_mit_klima[, c("climateZone1", "PixelValue", "Class")])
#' 
#' 
#' # 1. ID umformen - "_coordinates" entfernen
#' points_mit_klima$id_clean <- gsub("_coordinates$", "", points_mit_klima$id)
#' 
#' # Kontrolle der Transformation
#' head(points_mit_klima[, c("id", "id_clean")])
#' 
#' # 2a. Climate Zones Datei erstellen und speichern
#' climateZones <- points_mit_klima %>%
#'   st_drop_geometry() %>%  # Geometrie entfernen für txt export
#'   dplyr::select(id = id_clean, 
#'          lat = latitude, 
#'          long = longitude, 
#'          climate_zone_code = climateZone1, 
#'          description_of_zone = Description)
#' # read in the coordinates, that are actually needed:
#' 
#' coords = read.delim("T:/DLR/Analysis2/PCA/indata/all_coordinates_complete.txt",header=TRUE, sep = ";", dec= ".")
#' coords['id'] = coords['prefix_id']
#' prefixes= coords$prefix_id
#' 
#' merged = merge( x = coords, y = climateZones, by = "id", all.x = TRUE)
#' 
#' columns_to_keep = c("id", "Lat", "Lon", "climate_zone_code", "description_of_zone")
#' 
#' subset_climateZones = merged[columns_to_keep]
#' anyNA(subset_climateZones)
#' 
#' # Als txt speichern
#' write.table(subset_climateZones, 
#'             file = "T:/DLR/Analysis2/PCA/indata/Points_Climate.txt", 
#'             sep = ";", 
#'             row.names = FALSE, 
#'             quote = FALSE)
#' 
#' # 2b. Location Datei erstellen und speichern
#' locations <- points_mit_klima %>%
#'   st_drop_geometry() %>%
#'   dplyr::select(id = id_clean,
#'          lat = latitude,
#'          lon = longitude,
#'          country = Country,
#'          continent = Continent)
#' 
#' 
#' merged = merge( x = coords, y = locations, by = "id", all.x = TRUE)
#' 
#' columns_to_keep = c("id", "Lat", "Lon", "country", "continent")
#' subset_locations = merged[columns_to_keep]
#' 
#' 
#' # Als txt speichern
#' write.table(subset_locations, 
#'             file = "T:/DLR/Analysis2/PCA/input/Points_Cont.txt", 
#'             sep = ";", 
#'             row.names = FALSE, 
#'             quote = FALSE)
#' 
#' # Kontrolle der ersten Zeilen
#' head(subset_climateZones)
#' head(subset_locations)
