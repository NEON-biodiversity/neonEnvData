# TITLE:            Geodiversity Metric Calculations
# PROJECT:          NEON Geodiversity Analysis
# AUTHORS:          Kelly Kapsar, Pat Bills, Phoebe Zarnetske 
# COLLABORATORS:    Lala Kounta
# DATA INPUT:       SRTMGl3_v003 data processed in ./R/L0/1-srtm_unzip_reproject.R
# DATA OUTPUT:      Shapefiles with geodiversity metrics
# DATE:             November 2024
# OVERVIEW:         Processes spatial polygons to calculate geodiversity metrics
# REQUIRES:         R packages: geodiv, terra, sf, dplyr, foreach, doParallel
# NOTES:            Ensure input directories contain required data files

library(geodiv)
library(terra)
library(sf)
library(dplyr)
library(foreach)
library(doParallel)

# Load custom functions
source("./R/L1/L1-functions.R")

#' Main Geodiversity Metric Calculation Script
#'
#' This script processes spatial polygons and calculates geodiversity metrics using SRTM raster data.
#' 
#' @param srtm_tiles An `sf` object representing the SRTM tile grid.
#' @param srtm_tile_files A character vector of file paths to SRTM raster files.
#' @param spatial_polys A list of `sf` polygon objects to be processed.
#' @param spatial_poly_names A character vector of polygon file names.
#' @param metrics_list A character vector of geodiversity metrics to calculate.
#' @param output_dir The directory where intersected rasters and updated polygons are saved.
#' @return None. Saves updated polygons with geodiversity metrics to the specified directory.
#' @examples
#' metrics_list <- c("sq", "sdq", "sbi", "ssk", "sku", "sfd", "sds", "std2")
#' process_polygons(srtm_tiles, srtm_tile_files, spatial_polys, spatial_poly_names, metrics_list, output_dir)
process_polygons <- function(srtm_tiles, srtm_tile_files, spatial_polys, spatial_poly_names, metrics_list, output_dir) {
  
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Loop through each set of polygons
  for (i in 1:length(spatial_polys)) {
    polygons <- spatial_polys[[i]]
    print(paste0("Processing polygon set ", i, " of ", length(spatial_polys)))
    
    # Placeholder to store metrics for each polygon
    metrics_values <- list()
    
    # Process each polygon
    for (j in 1:length(polygons$geometry)) {
      polygon <- polygons[j, ]
      
      # Intersect SRTM raster with the polygon
      temp <- srtm_intersection(srtm_tiles, srtm_tile_files, polygon)
      
      # Calculate geodiversity metrics for the intersected raster
      output <- calculate_geodiversity_metrics(temp, metrics_list)
      metrics_values[[j]] <- output
    }
    
    # Combine metrics into a data frame
    metrics_df <- as.data.frame(do.call(rbind, metrics_values))
    
    # Add metrics to the polygon data
    out_polys <- cbind(polygons, metrics_df)
    
    # Generate the output file path
    output_path <- file.path(output_dir, paste0("processed_", spatial_poly_names[i]))
    
    # Save the updated polygons with metrics
    st_write(out_polys, output_path, overwrite = TRUE)
    print(paste0("Saved processed polygons to ", output_path))
  }
}

# Example data setup (ensure paths and data files exist as expected)
srtm_tiles <- st_read("/mnt/scratch/plz-lab/geodiversity/spatial_data/SRTM_tiles/srtm_grid_1deg.shp") %>%
  st_transform(crs = 5070)
srtm_tile_files <- list.files("/mnt/scratch/plz-lab/geodiversity/SRTM_gl1_v003/tiles_EPSG5070", full.names = TRUE)
spatial_polys <- lapply(
  grep(".shp", list.files("/mnt/scratch/plz-lab/geodiversity/spatial_data/polys_EPSG5070/", full.names = TRUE), value = TRUE),
  st_read
)
spatial_poly_names <- grep(".shp", list.files("/mnt/scratch/plz-lab/geodiversity/spatial_data/polys_EPSG5070/"), value = TRUE)
metrics_list <- c("sq", "sdq", "sbi", "ssk", "sku", "sfd", "sds", "std2")
output_dir <- "/mnt/scratch/plz-lab/geodiversity/output/polys_EPSG5070_intersected/"

# Run the process
process_polygons(srtm_tiles, srtm_tile_files, spatial_polys, spatial_poly_names, metrics_list, output_dir)
