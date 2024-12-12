# TITLE:            Geodiversity Metric Calculations
# PROJECT:          NEON Geodiversity Analysis
# AUTHORS:          Kelly Kapsar, Pat Bills, Phoebe Zarnetske 
# COLLABORATORS:    Lala Kounta
# DATA INPUT:       SRTMGl3_v003 data downloaded from NASA EarthData 
# DATA OUTPUT:       
# DATE:             November 2024
# OVERVIEW:          
# REQUIRES:         
# NOTES:

library(geodiv)
library(terra)
library(sf)
library(dplyr)
library(foreach)
library(doParallel)

source("./R/L1/L1-functions.R")

# Example usage:
srtm_tiles <- st_read("/mnt/scratch/plz-lab/geodiversity/spatial_data/SRTM_tiles/srtm_grid_1deg.shp") %>%
  st_transform(crs = 5070)
srtm_tile_files <- list.files("/mnt/scratch/plz-lab/geodiversity/SRTM_gl1_v003/tiles_EPSG5070", full.names = TRUE)
spatial_polys <- lapply(grep(".shp", list.files("/mnt/scratch/plz-lab/geodiversity/spatial_data/polys_EPSG5070/", full.names = TRUE), value = TRUE), st_read)
spatial_poly_names <- grep(".shp", list.files("/mnt/scratch/plz-lab/geodiversity/spatial_data/polys_EPSG5070/"), value=T)
metrics_list <- c("sq", "sdq", "sbi", "ssk", "sku", "sfd", "sds", "std2")

# Define output directory
output_dir <- "/mnt/scratch/plz-lab/geodiversity/output/intersected_rasters"

# Set up parallel backend
## MSU HPCC: https://wiki.hpcc.msu.edu/display/ITH/R+workshop+tutorial#Rworkshoptutorial-Submittingparalleljobstotheclusterusing{doParallel}:singlenode,multiplecores
# Request a single node (this uses the "multicore" functionality)
# registerDoParallel(cores=as.numeric(Sys.getenv("SLURM_CPUS_ON_NODE")[1]))

# Loop through sets of polygons and process
for (i in 2){
  polygons <- spatial_polys[[i]]
  
  # Ensure save directory exists if specified
  if (!is.null(output_dir) && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # create a blank list to store the results 
  # rasters_intersected=list()
  metrics_values <- list()
  
  # Process each file in parallel
  for(j in 20) {
  # for(j in 1:length(polygons$geometry)) {
  # metrics_values <- foreach(j=1:length(polygons$geometry), .packages = c("terra", "sf")) %dopar% {
  # rasters_intersected <- foreach(j=1:length(polygons$geometry), .packages = c("terra", "sf")) %dopar% {

      polygon <- polygons[j, ]
      
      # out_path <- if (!is.null(output_dir)) {
      #   file.path(output_dir, paste0("intersected_raster_", j, ".tif"))
      # } else {
      #   NULL
      # }
      
      temp <- srtm_intersection(srtm_tiles, srtm_tile_files, polygon, out_path)
      output <- calculate_geodiversity_metrics(temp, metrics_list)
      metrics_values[[j]] <- output
    }

  # Calculate geodiversity metrics
  # metrics_values <- lapply(rasters_intersected, function(x){calculate_geodiversity_metrics(x, metrics_list)})
  
  metrics_df <- as.data.frame(do.call(rbind, metrics_values))
  
  # Add metrics to polygon
  out_polys <- cbind(polygons, metrics_df)
  
  # Save updated polygon
  output_path <- paste0("/mnt/scratch/plz-lab/geodiversity/output/polys_EPSG5070_intersected/", spatial_poly_names[[i]])
  # st_write(out_polys, output_path, overwrite = TRUE)
  print(paste0("Saved processed polygon ", i, " to ", output_path))
}
