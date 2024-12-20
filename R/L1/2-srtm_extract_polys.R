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
library(ggplot2)

# Load custom functions
source("./R/L1/2-functions.R")

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
#' spatial_poly <- st_read("path/to/polygon_file.shp")
#' process_polygon(srtm_tiles, srtm_tile_files, spatial_poly, "polygon_file.shp", metrics_list, output_dir)
process_polygon <- function(srtm_tiles, srtm_tile_files, spatial_poly, spatial_poly_name, metrics_list, output_dir) {
  
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  print(paste0("Processing polygon set: ", spatial_poly_name))
  
  # Placeholder to store metrics for each polygon
  metrics_values <- list()
  
  # Process each polygon
  for (j in 1:length(spatial_poly$geometry)) {
    polygon <- spatial_poly[j, ]
    print(polygon$domainName)
    
    # Intersect SRTM raster with the polygon
    temp <- srtm_intersection(srtm_tiles, srtm_tile_files, polygon)
    
    # Calculate geodiversity metrics for the intersected raster
    output <- calculate_geodiversity_metrics(temp, metrics_list)
    metrics_values[[j]] <- output
  }
  
  # Combine metrics into a data frame
  metrics_df <- as.data.frame(do.call(rbind, metrics_values))
  
  # Add metrics to the polygon data
  out_polys <- cbind(spatial_poly, metrics_df)
  
  # Generate the output file path
  output_path <- file.path(output_dir, paste0("processed_", spatial_poly_name))
  
  # Save the updated polygons with metrics
  st_write(out_polys, output_path, overwrite = TRUE)
  print(paste0("Saved processed polygons to ", output_path))
}


# Example data setup (ensure paths and data files exist as expected)
srtm_tiles <- st_read("/mnt/scratch/plz-lab/geodiversity/spatial_data/SRTM_tiles/srtm_grid_1deg.shp") %>%
  st_transform(crs = 5070)
srtm_tile_files <- list.files("/mnt/scratch/plz-lab/geodiversity/SRTM_gl1_v003/tiles_EPSG5070", full.names = TRUE)
spatial_poly_paths <- grep(".shp", list.files("/mnt/scratch/plz-lab/geodiversity/spatial_data/polys_EPSG5070/", full.names = TRUE), value = TRUE)
spatial_poly_names <- grep(".shp", list.files("/mnt/scratch/plz-lab/geodiversity/spatial_data/polys_EPSG5070/"), value = TRUE)
metrics_list <- c("sq", "sdq", "sbi", "ssk", "sku", "sfd", "sds", "std2")
output_dir <- "/mnt/scratch/plz-lab/geodiversity/output/polys_EPSG5070_intersected/"


process_polygon(srtm_tiles = srtm_tiles,
                srtm_tile_files = srtm_tile_files,
                spatial_poly = st_read(spatial_poly_paths[[2]]),
                spatial_poly_name = spatial_poly_names[[2]],
                metrics_list = metrics_list,
                output_dir = output_dir)

# Apply the updated function to each file
# lapply(seq_along(spatial_poly_paths), function(i) {
#   spatial_poly <- st_read(spatial_poly_paths[i])
#   process_polygon(
#     srtm_tiles = srtm_tiles,
#     srtm_tile_files = srtm_tile_files,
#     spatial_poly = spatial_poly,
#     spatial_poly_name = spatial_poly_names[i],
#     metrics_list = metrics_list,
#     output_dir = output_dir
#   )
# })


#################################################################################
# Time trials for number of tiles to mosaic
# Initialize an empty data frame to store results
results <- data.frame(
  num_rasters = integer(),
  time_seconds = numeric()
)

polygon <- st_read(spatial_poly_paths[[2]])[1,]
intersecting_tiles <- srtm_tiles[st_intersects(polygon, srtm_tiles, sparse = FALSE), ]

if(length(intersecting_tiles$id) == 0){return(NA)}
tiles <- srtm_tile_files %>%
  grep(paste(unique(intersecting_tiles$id), collapse = "|"), ., value = TRUE)
# print(length(tiles))
temp <- lapply(tiles, rast)

# Loop through increasing numbers of rasters
for (i in 13:20) {
  # Subset the raster list
  raster_subset <- temp[1:i]

  # Time the mosaicking process
  start_time <- Sys.time()
  raster_intersected <- do.call(terra::mosaic, raster_subset)
  test <-
    terra::crop(raster_intersected, st_bbox(polygon)) %>%
    terra::mask(., polygon)
  end_time <- Sys.time()

  # Calculate the elapsed time in seconds
  elapsed_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # Append results to the data frame
  results <- rbind(results, data.frame(num_rasters = i, time_seconds = elapsed_time, method = "Original (All at once)"))
}

# Plot the results
ggplot(results, aes(x = num_rasters, y = time_seconds)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Time to Mosaic Rasters vs Number of Rasters",
    x = "Number of Rasters",
    y = "Time to Mosaic (seconds)"
  ) +
  theme_minimal()

#### Mosaic of mosaics trial 
# Loop through increasing numbers of rasters
# new_results <- data.frame(
#   num_rasters = integer(),
#   time_seconds = numeric()
# )


for (i in 13:20) {
  # Subset the raster list
  raster_subset <- temp[1:i]
  
  # Split the raster subset into two halves
  midpoint <- ceiling(length(raster_subset) / 2)
  raster_half1 <- raster_subset[1:midpoint]
  raster_half2 <- raster_subset[(midpoint + 1):length(raster_subset)]
  
  # Time the mosaicking process
  start_time <- Sys.time()
  
  # Create the two intermediate mosaics
  mosaic1 <- do.call(terra::mosaic, raster_half1)
  mosaic2 <- do.call(terra::mosaic, raster_half2)
  
  # Create the final mosaic from the two intermediate mosaics
  raster_intersected <- terra::mosaic(mosaic1, mosaic2)
  
  # Crop and mask with the polygon
  test <-
    terra::crop(raster_intersected, st_bbox(polygon)) %>%
    terra::mask(., polygon)
  
  end_time <- Sys.time()
  
  # Calculate the elapsed time in seconds
  elapsed_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Append results to the data frame
  new_results <- rbind(new_results, data.frame(num_rasters = i, time_seconds = elapsed_time, method =  "Mosaic of Mosaics (In chunks)"))
  
  # Log progress
  print(paste0("Processed ", i, " rasters in ", elapsed_time, " seconds."))
}


# Combine results from both methods into one data frame
results$method <- "Original (All at once)"
new_results$method <- "Mosaic of Mosaics (In chunks)"

combined_results <- rbind(results, new_results)

# Create the ggplot object
comparison_plot <- ggplot(combined_results, aes(x = num_rasters, y = time_seconds, color = method)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Performance Comparison: Original vs Mosaic of Mosaics",
    x = "Number of Rasters",
    y = "Time (seconds)",
    color = "Method"
  ) +
  theme_minimal(base_size = 14)

# Display the plot
print(comparison_plot)

