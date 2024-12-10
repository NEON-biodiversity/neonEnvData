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



library(terra)
library(sf)
library(dplyr)


intersect_raster_with_polygon <- function(polygon, raster){
  out_ras <- terra::crop(raster, st_bbox(polygon)) %>%
    terra::mask(polygon)
  out_ras
}

# Function to intersect a raster with a single polygon
srtm_intersection <- function(srtm_tiles, srtm_tile_files, polygon, save_path = NULL) {
  # Find intersecting SRTM tiles
  intersecting_tiles <- srtm_tiles[st_intersects(polygon, srtm_tiles, sparse = FALSE), ]
  
  if (length(intersecting_tiles$id) == 0) {
    stop("No intersecting tiles found.")
  }
  
  # Get the list of relevant SRTM tile files
  tiles <- srtm_tile_files %>%
    grep(paste(unique(intersecting_tiles$id), collapse = "|"), ., value = TRUE)
  
  # Load and mosaic the relevant SRTM tiles
  temp <- lapply(tiles, rast)
  if (length(temp) > 1) {
    raster_intersected <- do.call(terra::mosaic, temp) %>% 
      intersect_raster_with_polygon(., polygon)
  } else {
    raster_intersected <- temp[[1]] %>%
      intersect_raster_with_polygon(., polygon)
  }
  
  # Optionally save the intersected raster
  if (!is.null(save_path)) {
    writeRaster(raster_intersected, save_path, overwrite = TRUE)
  }
  
  return(raster_intersected)
}

# Wrapper function to apply `srtm_intersection` to all polygons in a spatial dataframe
srtm_intersection_wrapper <- function(srtm_tiles, srtm_tile_files, spatial_polys, save_dir = NULL) {
  # Ensure save directory exists if specified
  if (!is.null(save_dir) && !dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  # Iterate over each row of the spatial dataframe
  intersected_rasters <- lapply(1:nrow(spatial_polys), function(i) {
    polygon <- spatial_polys[i, ]
    save_path <- if (!is.null(save_dir)) {
      file.path(save_dir, paste0("intersected_raster_", i, ".tif"))
    } else {
      NULL
    }
    srtm_intersection(srtm_tiles, srtm_tile_files, polygon, save_path)
  })
  
  return(intersected_rasters)
}

# Function 2: Calculate geodiversity metrics
calculate_geodiversity_metrics <- function(raster, metrics_list) {
  metrics_values <- sapply(metrics_list, function(metric) {
    if (metric == "std") {
      stop("Error: Please specify 'std1' or 'std2' as the metric name.")
    }
    if (metric == "std1") {
      return(tryCatch({
        metric_function <- get("std", envir = asNamespace("geodiv"))
        as.numeric(metric_function(raster)[1])
      }, error = function(e) {
        message(sprintf("Geodiv metric '%s' does not exist.", metric))
        NA
      }))
    }
    if (metric == "std2") {
      return(tryCatch({
        metric_function <- get("std", envir = asNamespace("geodiv"))
        as.numeric(metric_function(raster)[2])
      }, error = function(e) {
        message(sprintf("Geodiv metric '%s' does not exist.", metric))
        NA
      }))
    }
    tryCatch({
      metric_function <- get(metric, envir = asNamespace("geodiv"))
      as.numeric(metric_function(raster))
    }, error = function(e) {
      message(sprintf("Geodiv metric '%s' does not exist.", metric))
      NA
    })
  })
  
  return(metrics_values)
}




# Example usage:
srtm_tiles <- st_read("/mnt/scratch/plz-lab/geodiversity/spatial_data/SRTM_tiles/srtm_grid_1deg.shp") %>%
  st_transform(crs = 5070)
srtm_tile_files <- list.files("/mnt/scratch/plz-lab/geodiversity/SRTM_gl1_v003/tiles_EPSG5070", full.names = TRUE)
spatial_polys <- lapply(grep(".shp", list.files("/mnt/scratch/plz-lab/geodiversity/spatial_data/polys_EPSG5070/", full.names = TRUE), value = TRUE), st_read)
metrics_list <- c("sq", "sdq", "sbi", "ssk", "sku", "sfd", "sds", "std2")

# Define output directory
output_dir <- "/mnt/scratch/plz-lab/geodiversity/output/intersected_rasters"

# Loop through polygons and process
for (i in 1:length(spatial_polys)) {
  polygons <- spatial_polys[[i]]
  print(paste0("Processing polygon ", i, " of ", length(spatial_polys), "."))
  
  # Intersect raster with polygon
  raster_path <- paste0("/mnt/scratch/plz-lab/geodiversity/output/raster_intersected_polygon_", i, ".tif")
  raster_intersected <- srtm_intersection_wrapper(srtm_tiles, srtm_tile_files, polygons, save_dir = output_dir)
  
  # Calculate geodiversity metrics
  metrics_values <- calculate_geodiversity_metrics(raster_intersected, metrics_list)
  
  # Add metrics to polygon
  for (j in seq_along(metrics_list)) {
    polygon[[metrics_list[j]]] <- metrics_values[j]
  }
  
  # Save updated polygon
  output_path <- paste0("/mnt/scratch/plz-lab/geodiversity/output/polys_EPSG5070_intersected/polygon_", i, ".shp")
  st_write(polygon, output_path, overwrite = TRUE)
  print(paste0("Saved processed polygon ", i, " to ", output_path))
}
