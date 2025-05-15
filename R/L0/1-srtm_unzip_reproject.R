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
library(doParallel) # For parallel processing
library(foreach)   # For parallel iteration

# Source configuration settings (e.g., custom projections or paths)
# source("./R/config.R")
source("../config.R")

# Load and transform SRTM tile grid shapefile to the desired projection
srtm_tiles <- st_read(elev_tiles) %>%
  st_transform(prj)

# Convert extent to sf object
extent_sf <- st_as_sf(as.polygons(extent))
st_crs(extent_sf) <- "EPSG:4326"
extent_sf <- st_transform(extent_sf, prj)


################################################################################
# Identify and process intersecting tiles

# Filter SRTM tiles that intersect with the extent of the data set
all_tiles <- srtm_tiles[st_intersects(srtm_tiles, extent_sf, sparse = FALSE), ]


# Set up parallel processing backend
# Use the number of cores available on the MSU HPCC cluster
registerDoParallel(cores = as.numeric(Sys.getenv("SLURM_CPUS_ON_NODE")[1]))

# Process each climate data file in parallel
cropped_projected <- foreach(i = 1:length(all_tiles$id), .packages = c("terra", "dplyr")) %dopar% {
  
# Loop over each intersecting tile to reproject and save it
# for (i in 1:length(all_tiles$id)) {
  # Start timer for performance tracking
  start_time <- proc.time()
  
  # Print progress message
  print(paste0("Started tile ", all_tiles$id[i]))
  
  # Locate the specific SRTM tile file based on the tile ID
  tile <- list.files(raw_elev_dir, full.names = TRUE) %>%
    grep(paste(all_tiles$id[i], collapse = "|"), ., value = TRUE) %>%
    grep("\\.zip$", ., value = TRUE)
  
  # Specify the target resolution (e.g., 30 meters)
  target_resolution <- 30 
  
  # Unzip and load the raster tile
  raster_tile <- unzip(
    tile, 
    exdir = elev_hgt
  ) %>% rast()
  
  # Create a template raster for reprojection (to EPSG:5070 with target resolution)
  template <- project(raster_tile, prj, res = target_resolution)
  
  # Reproject the raster tile using the template
  reprojected_tile <- project(raster_tile, template)
  
  # Convert all values <-20 to 0
  reprojected_tile[reprojected_tile < -20] <- 0
  
  # Save the reprojected tile to a specified directory
  terra::writeRaster(
    reprojected_tile,
    paste0(
      elev_tile_tif,
      "/",
      all_tiles$id[i], "_", prj_name, ".tif"
    ), overwrite=T
  )
  
  # Print completion message for the current tile
  print(paste0("Tile ", all_tiles$id[i], " reprojected and saved."))
  
  # Print the time taken to process the current tile
  print(proc.time() - start_time)
}

