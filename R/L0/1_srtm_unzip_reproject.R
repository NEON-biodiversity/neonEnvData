# TITLE:            Geodiversity Data Cleaning
# PROJECT:          NEON Geodiversity Analysis
# AUTHORS:          Kelly Kapsar, Pat Bills, Phoebe Zarnetske 
# COLLABORATORS:    Lala Kounta
# DATA INPUT:       SRTMGl3_v003 data downloaded from NASA EarthData 
# DATA OUTPUT:      Processed SRTM tiles in specified projection (.R/config.R)
# DATE:             August 2024
# OVERVIEW:         Script for cleaning and reprojecting geodiversity raster data
# REQUIRES:         R libraries: terra, sf, dplyr
# NOTES:            Ensure proper directory structure and data availability

# Load required libraries
library(terra)  # For raster data manipulation
library(sf)     # For spatial data manipulation
library(dplyr)  # For data wrangling

# Source configuration settings (e.g., custom projections or paths)
source("./R/config.R")

# Load shapefiles of polygons with geodiversity data
spat_data <- list.files(
  neon_dir,
  full.names = TRUE
) %>%
  grep("\\.shp$", ., value = TRUE)

# Load and transform SRTM tile grid shapefile to the desired projection
srtm_tiles <- st_read(elev_tiles) %>%
  st_transform(prj)

################################################################################
# Identify and process intersecting tiles

# Read and unionize the polygons (e.g., study regions or domains)
all_doms <- st_read(spat_data[2]) %>% st_union()

# Filter SRTM tiles that intersect with the union of all domains
all_tiles <- srtm_tiles[st_intersects(srtm_tiles, all_doms, sparse = FALSE), ]

# Loop over each intersecting tile to reproject and save it
for (i in 1:length(all_tiles$id)) {
  # Start timer for performance tracking
  start_time <- proc.time()
  
  # Print progress message
  print(paste0("Started tile ", all_tiles$id[i]))
  
  # Locate the specific SRTM tile file based on the tile ID
  tile <- list.files(raw_elev_dir, full.names = TRUE) %>%
    grep(paste(all_tiles$id[i], collapse = "|"), ., value = TRUE) %>%
    grep("\\.zip$", ., value = TRUE)
  
  # Specify the target resolution (e.g., 30 meters)
  target_resolution <- 30  # Adjust as needed
  
  # Unzip and load the raster tile
  raster_tile <- unzip(
    tile, 
    exdir = elev_hgt
  ) %>% rast()
  
  # Create a template raster for reprojection (to EPSG:5070 with target resolution)
  template <- project(raster_tile, prj, res = target_resolution)
  
  # Reproject the raster tile using the template
  reprojected_tile <- project(raster_tile, template)
  
  # Save the reprojected tile to a specified directory
  terra::writeRaster(
    reprojected_tile,
    paste0(
      elev_tif,
      all_tiles$id[i], "_", prj_name, ".tif"
    )
  )
  
  # Print completion message for the current tile
  print(paste0("Tile ", all_tiles$id[i], " reprojected and saved."))
  
  # Print the time taken to process the current tile
  print(proc.time() - start_time)
}
