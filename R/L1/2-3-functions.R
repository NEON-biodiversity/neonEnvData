# TITLE:            Functions for Geodiversity and Climate Data Processing
# PROJECT:          NEON Geodiversity Analysis
# AUTHORS:          Kelly Kapsar, Pat Bills, Phoebe Zarnetske 
# COLLABORATORS:    Lala Kounta
# DATE:             November 2024
# OVERVIEW:         This script provides utility functions for processing raster and polygon data
# REQUIRES:         R packages: terra, geodiv
# NOTES:            Ensure the 'geodiv' package and input data are correctly configured

#' Intersect a raster with a polygon
#'
#' This function crops a raster to the extent of a polygon and masks it to the polygon shape.
#'
#' @param pol An `sf` polygon object.
#' @param raster A `SpatRaster` object to be intersected.
#' @param save_path Optional file path to save the intersected raster.
#' @return A `SpatRaster` object of the intersected raster.
#' @examples
#' intersected <- intersect_raster_with_polygon(my_polygon, my_raster)
#' intersected <- intersect_raster_with_polygon(my_polygon, my_raster, "output.tif")
intersect_raster_with_polygon <- function(pol, raster, save_path = NULL) {
  out_ras <- terra::crop(raster, st_bbox(pol)) %>%
    terra::mask(pol)
  
  if (!is.null(save_path)) {
    writeRaster(out_ras, save_path, overwrite = TRUE)
  }
  
  return(out_ras)
}

#' Intersect SRTM tiles with a polygon
#'
#' This function identifies SRTM tiles that intersect with a polygon AOI and generates a raster of the intersection area.
#'
#' @param srtm_tiles An `sf` object representing the SRTM tile grid.
#' @param srtm_tile_files A character vector of file paths to SRTM raster files.
#' @param polygon An `sf` polygon object.
#' @param save_path Optional file path to save the intersected raster.
#' @param ID identifying name for virtual raster 
#' @return A `SpatRaster` object of the intersected and mosaicked raster.
#' @examples
#' intersected <- srtm_intersection(srtm_tiles, srtm_tile_files, my_polygon)
#' intersected <- srtm_intersection(srtm_tiles, srtm_tile_files, my_polygon, "output.tif")
srtm_intersection <- function(srtm_tiles, srtm_tile_files, polygon, save_path, ID = NULL) {
    # Find intersecting tiles
    intersecting_tiles <- srtm_tiles[st_intersects(polygon, srtm_tiles, sparse = FALSE), ]
    
    # Return NA if no intersecting tiles are found
    if (length(intersecting_tiles$id) == 0) {
      return(NA)
    }
    
    # Filter tile files that match intersecting tiles
    tiles <- srtm_tile_files %>%
      grep(paste(unique(intersecting_tiles$id), collapse = "|"), ., value = TRUE)
    
    output_vrt <- paste0(save_path, "/", ID, ".vrt")

    # Create the virtual raster
    vr <- vrt(tiles, filename = output_vrt, overwrite=T)

    # Mosaic and intersect rasters
    # if (length(tiles) > 1) {
    raster_intersected <- intersect_raster_with_polygon(polygon, vr)
    # } else {
    #   raster_intersected <- intersect_raster_with_polygon(polygon, temp[[1]])
    # }
    
    output_ras <- paste0(save_path, "/masked_rasters/", ID, ".tif" )
    
    terra::writeRaster(raster_intersected, output_ras)

  # Return the resulting raster
  return(raster_intersected)
}
#' Calculate geodiversity metrics
#'
#' This function calculates a list geodiversity metrics for a given raster.
#'
#' @param raster A `SpatRaster` object for which metrics are calculated.
#' @param metrics_list A character vector of metric names (e.g., `std1`, `std2`, etc.).
#' @return A numeric vector of calculated metric values.
#' @examples
#' metrics <- calculate_geodiversity_metrics(my_raster, c("std1", "std2", "roughness"))
calculate_geodiversity_metrics <- function(raster, metrics_list) {
  metrics_values <- sapply(metrics_list, function(metric) {
    print(metric)
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
