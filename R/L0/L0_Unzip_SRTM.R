# TITLE:            Geodiversity Data Cleaning
# PROJECT:          NEON Geodiversity Analysis
# AUTHORS:          Kelly Kapsar, Pat Bills, Phoebe Zarnetske 
# COLLABORATORS:    Lala Kounta
# DATA INPUT:       SRTMGl3_v003 data downloaded from NASA EarthData 
# DATA OUTPUT:       
# DATE:             August 2024
# OVERVIEW:          
# REQUIRES:         
# NOTES:             

library(terra)
library(sf)
library(dplyr)

source("./R/config.R")

spat_data <- list.files("/mnt/scratch/plz-lab/geodiversity/spatial_data/polys_EPSG5070/", full.names=T) %>% grep("\\.shp$", ., value=TRUE)

srtm_tiles <- st_read("/mnt/scratch/plz-lab/geodiversity/spatial_data/SRTM_tiles/srtm_grid_1deg.shp") %>% st_transform(prj)

################################################################################

all_doms <- st_read(spat_data[2]) %>% st_union()
all_tiles <- srtm_tiles[st_intersects(srtm_tiles, all_doms, sparse=F),]

for(i in 1:length(all_tiles$id)){
  start_time <- proc.time()
  
  print(paste0("Started tile ", all_tiles$id[i]))
  
  tile <- list.files("/mnt/scratch/plz-lab/geodiversity/SRTM_gl1_v003", full.names=TRUE) %>%
    grep( paste(all_tiles$id[i], collapse = "|"), ., value = TRUE) %>%
    grep("\\.zip$", ., value = TRUE)
  
  
  # Specify the common resolution (e.g., in meters)
  target_resolution <- 30  # Example: 1 km resolution
  
  raster_tile <- unzip(tile, exdir = "/mnt/scratch/plz-lab/geodiversity/SRTM_gl1_v003/unzipped_hgt") %>% 
    rast() 
  
  # Create a template raster for reprojection
  template <- project(raster_tile, "EPSG:5070", res=target_resolution)
  
  # Reproject all tiles using the template resolution and extent
  reprojected_tile <- project(raster_tile, template)
  
  terra::writeRaster(reprojected_tile, paste0("/mnt/scratch/plz-lab/geodiversity/SRTM_gl1_v003/tiles_EPSG5070/", all_tiles$id[i], "_EPSG5070.tif"))
  
  print(paste0("Tile ", all_tiles$id[i], " reprojected and saved."))
  
  print(proc.time() - start_time )
}