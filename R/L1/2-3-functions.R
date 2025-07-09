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
#' @param elev_res Character string specifying resolution for raster output
#' @param ID identifying name for virtual raster 
#' @return A `SpatRaster` object of the intersected and mosaicked raster.
#' @examples
#' intersected <- srtm_intersection(srtm_tiles, srtm_tile_files, my_polygon)
#' intersected <- srtm_intersection(srtm_tiles, srtm_tile_files, my_polygon, "output.tif")
srtm_intersection <- function(srtm_tiles, srtm_tile_files, polygon, tif_path, vrt_path, elev_res, ID = NULL) {
  # Find intersecting tiles
  intersecting_tiles <- srtm_tiles[st_intersects(polygon, srtm_tiles, sparse = FALSE), ]
  
  # Return NA if no intersecting tiles are found
  if (length(intersecting_tiles$id) == 0) {
    return(NA)
  }
  
  ## Step 1: Filter tiles that intersect
  tiles <- srtm_tile_files %>%
    grep(paste(unique(intersecting_tiles$id), collapse = "|"), ., value = TRUE)
  
  # Create file paths
  output_vrt <- file.path(vrt_path, paste0(ID, "_30.vrt"))
  output_vrt_agg <- file.path(vrt_path, paste0(ID, "_", elev_res, ".vrt"))
  
  # Step 2: Build the original high-resolution VRT
  if (!file.exists(output_vrt)) {
    gdalbuild_cmd <- paste(
      "gdalbuildvrt",
      shQuote(output_vrt),
      paste(shQuote(tiles), collapse = " ")
    )
    system(gdalbuild_cmd)
  }
  
  # Step 3: Resample VRT to lower resolution
  if (!file.exists(output_vrt_agg)) {
    res <- as.numeric(elev_res)
    gdalwarp_cmd <- paste(
      "gdalwarp",
      "-tr", res, res,          # target resolution
      "-r average",             # resampling method
      "-of VRT",                # output format
      shQuote(output_vrt),      # input VRT
      shQuote(output_vrt_agg)   # output VRT
    )
    system(gdalwarp_cmd)
  }
  
  # Step 4: Load the resampled VRT
  vr <- terra::rast(output_vrt_agg)

  # Intersect rasters
  output_ras <- paste0(tif_path, "/", ID, "_", elev_res, ".tif" )
  
  raster_intersected <- intersect_raster_with_polygon(polygon, vr, save_path = output_ras)

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

#' Geodiversity Metric Calculation for a Single Spatial Polygon File
#'
#' This function processes a single spatial polygon file and calculates geodiversity metrics using SRTM raster data.
#' 
#' @param srtm_tiles An `sf` object representing the SRTM tile grid.
#' @param srtm_tile_files A character vector of file paths to SRTM raster files.
#' @param spatial_poly An `sf` polygon object to be processed.
#' @param spatial_poly_name A character string representing the name of the polygon file.
#' @param metrics_list A character vector of geodiversity metrics to calculate.
#' @param output_dir The directory where intersected rasters and updated polygons are saved.
#' @return None. Saves the updated polygon with geodiversity metrics to the specified directory.
#' @examples
#' metrics_list <- c("sq", "sdq", "sbi", "ssk", "sku", "sfd", "sds", "std2")
#' spatial_poly <- st_read("path/to/polygon_file.gpkg")
#' process_polygon(srtm_tiles, srtm_tile_files, spatial_poly, "polygon_file.gpkg", metrics_list, output_dir)
process_polygons <- function(srtm_tiles, srtm_tile_files, spatial_poly, spatial_poly_name, id_col, metrics_list, output_dir, vrt_dir, tif_dir, elev_res) {

  print(paste0("Processing polygon set: ", spatial_poly_name))
  
  # Placeholder to store metrics for each polygon
  metrics_values <- list()
  
  # Process each polygon
  for (j in 1:length(spatial_poly$geom)) {
    
    print(paste0("ROUND: ", j))
    polygon <- spatial_poly[j, ]
    
    # Generate name for virtual raster 
    nm <- paste0(spatial_poly_name, "_", polygon[[id_col]])
    
    print(paste0(nm, ": Processing."))
    
    # Intersect SRTM raster with the polygon
    temp <- srtm_intersection(srtm_tiles, srtm_tile_files, polygon, ID=nm, vrt_path=vrt_dir, tif_path=tif_dir, elev_res=elev_res)
    
    print(paste0(nm, ": Raster intersection complete."))
    
    # Convert metric list to character if only one value 
    if (class(metrics_list) == "factor") {
      metrics_list <- as.character(metrics_list)
    }
    
    # Calculate geodiversity metrics for the intersected raster    
    output <- calculate_geodiversity_metrics(temp, metrics_list)
    
    if(class(temp) == "SpatRaster"){
      output[["mean"]] <- global(temp, "mean", na.rm=T)[,1]
      # output[["sd"]] <- global(temp, "sd", na.rm=T)[,1]
    }else{
      output[["mean"]] <- NA
      # output[["sd"]] <- NA
    }
    
    # Reorder so mean and sd are first 
    output <- output[c("mean", setdiff(names(output), c("mean")))]
    
    names(output) <- paste0("srtm_", names(output))
    
    metrics_values[[j]] <- output
    
    # write.csv(output, paste0("/mnt/scratch/kapsarke/geodiversity/output/geodiv_metric_csv_domains/geodiv_metrics_", polygon$domainNumb, ".csv"))
    # write.csv(output, paste0("/mnt/home/kapsarke/Documents/geodiversity/geodiv_metrics_", polygon$domainNumb, ".csv"))
    
    print(paste0(nm, ": Metrics calculated."))
  }
  
  # Combine metrics into a data frame
  metrics_df <- as.data.frame(do.call(rbind, metrics_values))
  
  # Add metrics to the polygon data
  out_polys <- cbind(spatial_poly, metrics_df)
  
  # Generate the output file path
  if(length(metrics_list) == 1){
    output_path <- file.path(output_dir, paste0(spatial_poly_name,"_", metrics_list, ".gpkg"))
  }else(
    output_path <- file.path(output_dir, paste0(spatial_poly_name, ".gpkg"))
  )
  # Save the updated polygons with metrics
  st_write(out_polys, output_path, append=FALSE)
  print(paste0("Saved processed polygons to ", output_path))
}
