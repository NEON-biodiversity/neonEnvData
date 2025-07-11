# =============================================================================
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
# =============================================================================

# -----------------------------------------------------------------------------
# Load Custom Functions and Configuration
# -----------------------------------------------------------------------------
# Load custom functions
# source("./R/L1/2-3-functions.R")
source("2-3-functions.R") # HPCC

# Load configuration file
# source("./R/config.R")
source("../config.R") # HPCC

# -----------------------------------------------------------------------------
# Setup Inputs: Raster Tiles, Domain Polygons, and Metric List
# -----------------------------------------------------------------------------
# Example data setup (ensure paths and data files exist as expected)
srtm_tiles <- st_read(elev_tiles) %>%
  st_transform(crs = prj)

spatial_poly_paths <- grep(".gpkg", list.files(output_dir_clim, full.names = TRUE), value = TRUE)
spatial_poly_names <- grep(".gpkg", list.files(output_dir_clim), value = TRUE) %>% gsub("\\.gpkg$", "", .)


# -----------------------------------------------------------------------------
# Run Processing Function
# -----------------------------------------------------------------------------
# process_polygons(srtm_tiles = srtm_tiles,
#                  srtm_tile_files = list.files(elev_tile_tif, full.names = TRUE) ,
#                  spatial_poly = st_read(spatial_poly_paths[[1]]),
#                  spatial_poly_name = spatial_poly_names[[1]],
#                  id_col = ifelse(grepl("plot", spatial_poly_names[[1]]), "plotID",
#                           ifelse(grepl("site", spatial_poly_names[[1]]), "siteID",
#                           ifelse(grepl("domain_radii", spatial_poly_names[[1]]), "siteID",
#                           ifelse(grepl("domain_footprint", spatial_poly_names[[1]]), "domainNumb", id_col)))),
#                  metrics_list = metrics_list,
#                  vrt_dir = elev_vrt,
#                  tif_dir = elev_tif,
#                  output_dir = output_dir_clim_elev,
#                  elev_res = elev_res)


# Apply the updated function to each file
# lapply(c(2:8), function(i) {
#   process_polygons(srtm_tiles = srtm_tiles,
#                    srtm_tile_files = list.files(elev_tile_tif, full.names = TRUE) ,
#                    spatial_poly = st_read(spatial_poly_paths[[i]]),
#                    spatial_poly_name = spatial_poly_names[[i]],
#                    id_col = ifelse(grepl("plot", spatial_poly_names[[i]]), "plotID",
#                              ifelse(grepl("site", spatial_poly_names[[i]]), "siteID",
#                              ifelse(grepl("domain_radii", spatial_poly_names[[i]]), "siteID",
#                              ifelse(grepl("domain_footprint", spatial_poly_names[[i]]), "domainNumb", NA)))),
#                    metrics_list = metrics_list,
#                    vrt_dir = elev_vrt,
#                    tif_dir = elev_tif,
#                    output_dir = output_dir_clim_elev, 
#                    elev_res = elev_res)
# })

lapply(c(7:8), function(i) {
  process_polygons(srtm_tiles = srtm_tiles,
                   srtm_tile_files = list.files(elev_tile_tif, full.names = TRUE) ,
                   spatial_poly = st_read(spatial_poly_paths[[i]]),
                   spatial_poly_name = spatial_poly_names[[i]],
                   id_col = "siteID",
                   metrics_list = metrics_list,
                   vrt_dir = elev_vrt,
                   tif_dir = elev_tif,
                   output_dir = output_dir_clim_elev,
                   elev_res = elev_res)
})

###
# For testing
# srtm_tile_files = list.files(elev_tile_tif, full.names = TRUE)
# spatial_poly = st_read(spatial_poly_paths[[3]])
# spatial_poly_name = spatial_poly_names[[3]]
# id_col = "plotID"
#   # ifelse(grepl("plot", spatial_poly_name), "plotID",
#   #        ifelse(grepl("site", spatial_poly_name), "siteID",
#   #               ifelse(grepl("domain_radii", spatial_poly_name), "siteID",
#   #                      ifelse(grepl("domain_footprint", spatial_poly_name), "domainNumb", NA))))
# vrt_dir = elev_vrt
# tif_dir = elev_tif
# output_dir = output_dir_clim_elev
###


# -----------------------------------------------------------------------------
# Time Trials
# -----------------------------------------------------------------------------
# #################################################################################
# # Time trials for number of tiles to mosaic
# # Initialize an empty data frame to store results
# results <- data.frame(
#   num_rasters = integer(),
#   time_seconds = numeric()
# )
# 
# polygon <- st_read(spatial_poly_paths[[2]])[1,]
# intersecting_tiles <- srtm_tiles[st_intersects(polygon, srtm_tiles, sparse = FALSE), ]
# 
# if(length(intersecting_tiles$id) == 0){return(NA)}
# tiles <- srtm_tile_files %>%
#   grep(paste(unique(intersecting_tiles$id), collapse = "|"), ., value = TRUE)
# # print(length(tiles))
# temp <- lapply(tiles, rast)
# 
# # Loop through increasing numbers of rasters
# for (i in 13:20) {
#   # Subset the raster list
#   raster_subset <- temp[1:i]
# 
#   # Time the mosaicking process
#   start_time <- Sys.time()
#   raster_intersected <- do.call(terra::mosaic, raster_subset)
#   test <-
#     terra::crop(raster_intersected, st_bbox(polygon)) %>%
#     terra::mask(., polygon)
#   end_time <- Sys.time()
# 
#   # Calculate the elapsed time in seconds
#   elapsed_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
# 
#   # Append results to the data frame
#   results <- rbind(results, data.frame(num_rasters = i, time_seconds = elapsed_time, method = "Original (All at once)"))
# }
# 
# # Plot the results
# ggplot(results, aes(x = num_rasters, y = time_seconds)) +
#   geom_line() +
#   geom_point() +
#   labs(
#     title = "Time to Mosaic Rasters vs Number of Rasters",
#     x = "Number of Rasters",
#     y = "Time to Mosaic (seconds)"
#   ) +
#   theme_minimal()
# 
# #### Mosaic of mosaics trial 
# # Loop through increasing numbers of rasters
# # new_results <- data.frame(
# #   num_rasters = integer(),
# #   time_seconds = numeric()
# # )
# 
# 
# for (i in 13:20) {
#   # Subset the raster list
#   raster_subset <- temp[1:i]
#   
#   # Split the raster subset into two halves
#   midpoint <- ceiling(length(raster_subset) / 2)
#   raster_half1 <- raster_subset[1:midpoint]
#   raster_half2 <- raster_subset[(midpoint + 1):length(raster_subset)]
#   
#   # Time the mosaicking process
#   start_time <- Sys.time()
#   
#   # Create the two intermediate mosaics
#   mosaic1 <- do.call(terra::mosaic, raster_half1)
#   mosaic2 <- do.call(terra::mosaic, raster_half2)
#   
#   # Create the final mosaic from the two intermediate mosaics
#   raster_intersected <- terra::mosaic(mosaic1, mosaic2)
#   
#   # Crop and mask with the polygon
#   test <-
#     terra::crop(raster_intersected, st_bbox(polygon)) %>%
#     terra::mask(., polygon)
#   
#   end_time <- Sys.time()
#   
#   # Calculate the elapsed time in seconds
#   elapsed_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
#   
#   # Append results to the data frame
#   new_results <- rbind(new_results, data.frame(num_rasters = i, time_seconds = elapsed_time, method =  "Mosaic of Mosaics (In chunks)"))
#   
#   # Log progress
#   print(paste0("Processed ", i, " rasters in ", elapsed_time, " seconds."))
# }
# 
# 
# # Combine results from both methods into one data frame
# results$method <- "Original (All at once)"
# new_results$method <- "Mosaic of Mosaics (In chunks)"
# 
# combined_results <- rbind(results, new_results)
# 
# # Create the ggplot object
# comparison_plot <- ggplot(combined_results, aes(x = num_rasters, y = time_seconds, color = method)) +
#   geom_line(size = 1) +
#   geom_point(size = 2) +
#   labs(
#     title = "Performance Comparison: Original vs Mosaic of Mosaics",
#     x = "Number of Rasters",
#     y = "Time (seconds)",
#     color = "Method"
#   ) +
#   theme_minimal(base_size = 14)
# 
# # Display the plot
# print(comparison_plot)
# 
