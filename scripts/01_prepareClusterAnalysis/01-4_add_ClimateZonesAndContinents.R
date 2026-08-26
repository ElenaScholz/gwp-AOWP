# =============================
# Climate zone + location joiner



# NOTE: h09v07_9798_1 lies in the water-area of koeppen-geiger and therefore needs to be classified manually
# =============================
rm(list=ls())
renv::activate()
library(sf)
library(dplyr)

# ---- Configuration ----
config <- list(
  # EDIT THIS: set to your local project root
  ROOT             = "path-to-your-gwpAOWP-folder/",
  gpkg_file        = "Input/gwp_HylaksWithClimateZone.gpkg",
  coords_file      = "CluDat/all_coordinates_complete.txt",
  output_climate   = "CluDat/Points_Climate.txt",
  output_location  = "CluDat/Points_Cont.txt",
  coord_id_column  = "prefix_id",   # ID column in coords file
  gpkg_id_column   = "id",          # ID column in gpkg
  gpkg_lat_column  = "latitude",    # may adapt if naming changes
  gpkg_lon_column  = "longitude"
)

config$gpkg_file       <- paste0(config$ROOT, config$gpkg_file)
config$coords_file     <- paste0(config$ROOT, config$coords_file)
config$output_climate  <- paste0(config$ROOT, config$output_climate)
config$output_location <- paste0(config$ROOT, config$output_location)


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
