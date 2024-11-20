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

calculate_geodiv_metrics <- function(srtm_tiles, srtm_tile_files, spatial_polys, metrics_list) {
  # Loop through each polygon
  for (i in 1:nrow(spatial_polys)) {
    cutter <- spatial_polys[i, ]
    
    print(paste0("Processing polygon ", i, " of ", nrow(spatial_polys), "."))
    
    # Find intersecting SRTM tiles
    intersecting_tiles <- srtm_tiles[st_intersects(cutter, srtm_tiles, sparse = FALSE), ]
    
    if(length(intersecting_tiles$id) == 0){
      metrics_values <- rep(NA, length(metrics_list))
    }else{
      # Get the list of relevant SRTM tile files
      tiles <- srtm_tile_files %>%
        grep(paste(unique(intersecting_tiles$id), collapse = "|"), ., value = TRUE)
      
      # Load and mosaic the relevant SRTM tiles
      temp <- lapply(tiles, rast)
      
      if (length(temp) > 1) {
        ras1 <- do.call(terra::mosaic, temp) %>% terra::crop(., st_bbox(cutter)) %>% terra::mask(cutter)
      } else {
        ras1 <- temp[[1]] %>% terra::crop(., st_bbox(cutter)) %>% terra::mask(cutter)
      }
      
      # Calculate metrics for the polygon
      metrics_values <- sapply(metrics_list, function(metric) {
        if (metric == "std") {
          stop("Error: Please specify 'std1' or 'std2' as the metric name.")
        }
        if (metric == "std1") {
          return(tryCatch({
            metric_function <- get("std", envir = asNamespace("geodiv"))
            as.numeric(metric_function(ras1)[1])
          }, error = function(e) {
            message(sprintf("Geodiv metric '%s' does not exist.", metric))
            NA
          }))
        }
        if (metric == "std2") {
          return(tryCatch({
            metric_function <- get("std", envir = asNamespace("geodiv"))
            as.numeric(metric_function(ras1)[2])
          }, error = function(e) {
            message(sprintf("Geodiv metric '%s' does not exist.", metric))
            NA
          }))
        }
        tryCatch({
          metric_function <- get(metric, envir = asNamespace("geodiv"))
          as.numeric(metric_function(ras1))
        }, error = function(e) {
          message(sprintf("Geodiv metric '%s' does not exist.", metric))
          NA
        })
      })
  }
  print(metrics_values)

  # Add the metric values as new columns to the spatial_polys object
  for (j in seq_along(metrics_list)) {
    spatial_polys[[metrics_list[j]]][i] <- as.numeric(metrics_values[j])
  }
  rm(metrics_values)
}
return(spatial_polys)
}

# Example usage (load shapefiles externally)
srtm_tiles <- st_read("/mnt/scratch/plz-lab/geodiversity/spatial_data/SRTM_tiles/srtm_grid_1deg.shp") %>% st_transform(crs = 5070)
srtm_tile_files <- list.files("/mnt/scratch/plz-lab/geodiversity/SRTM_gl1_v003/tiles_EPSG5070", full.names = TRUE)
spatial_polys <- lapply(grep(".shp", list.files("/mnt/scratch/plz-lab/geodiversity/spatial_data/polys_EPSG5070/", full.names = T), value=T), st_read)
metrics_list <- c("sq", "sdq", "sbi", "ssk", "sku", "sfd", "sds", "std2")

# Call function with pre-loaded objects
# result <- calculate_geodiv_metrics(srtm_tiles, srtm_tile_files, spatial_polys[[6]], metrics_list)
# result <- calculate_geodiv_metrics(srtm_tiles, srtm_tile_files, spatial_polys[[6]], metrics_list)
# print(result)


for(i in 1:length(spatial_polys)){

  results <- calculate_geodiv_metrics(srtm_tiles, srtm_tile_files, spatial_polys[[i]], metrics_list)
  st_write(results, paste0("/mnt/scratch/plz-lab/geodiversity/output/polys_EPSG5070_intersected/", 
                           grep(".shp", list.files("/mnt/scratch/plz-lab/geodiversity/spatial_data/polys_EPSG5070/"), value=T)[i]))
  print(  grep(".shp", list.files("/mnt/scratch/plz-lab/geodiversity/spatial_data/polys_EPSG5070/"), value=T)[i])
}
