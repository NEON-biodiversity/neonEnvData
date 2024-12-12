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


intersect_raster_with_polygon <- function(pol, raster, save_path = NULL){
  out_ras <- terra::crop(raster, ext(pol)) %>%
    terra::mask(pol)
  
  # Optionally save the intersected raster
  if (!is.null(save_path)) {
    writeRaster(out_ras, save_path, overwrite = TRUE)
  }
  out_ras
}

# Function to intersect a raster with a single polygon
srtm_intersection <- function(srtm_tiles, srtm_tile_files, polygon, save_path = NULL) {
  # Find intersecting SRTM tiles
  intersecting_tiles <- srtm_tiles[st_intersects(polygon, srtm_tiles, sparse = FALSE), ]
  
  # Get the list of relevant SRTM tile files
  tiles <- srtm_tile_files %>%
    grep(paste(unique(intersecting_tiles$id), collapse = "|"), ., value = TRUE)
  
  # Load and mosaic the relevant SRTM tiles
  temp <- lapply(tiles, rast)
  if (length(temp) > 1) {
    raster_intersected <- do.call(terra::mosaic, temp) %>% 
      intersect_raster_with_polygon(polygon, .)
  } else {
    raster_intersected <- 
      intersect_raster_with_polygon(polygon, temp[[1]])
  }
  return(raster_intersected)
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
