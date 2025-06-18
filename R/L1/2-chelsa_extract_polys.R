# TITLE:            Climate Data Extraction 
# PROJECT:          NEON Geodiversity Analysis
# AUTHORS:          Kelly Kapsar, Lala Kounta, Pat Bills, Phoebe Zarnetske 
# DATA INPUT:       NEON spatial data and chelsa climate rasters (reprojected from 1-chelsa_crop_reproject.R)
# DATA OUTPUT:      Shapefiles with integrated climate and elevation data
# DATE:             December 2024
# OVERVIEW:         This script takes reprojected climate rasters for each of the
#                   19 biovars, intersects them with NEON spatial data, and calculates
#                   the mean value for each biovar for each spatial object. 
# REQUIRES:         R libraries: sf, terra, stringr
# NOTES:            Ensure input directories contain required data files

# Load necessary libraries
library(sf)        # For spatial data handling
library(terra)     # For raster data manipulation
library(stringr)   # For string operations

# Load configuration file
source("./R/L1/2-3-functions.R")
source("./R/config.R")
# source("./2-3-functions.R") # HPCC
# source("../config.R") #HPCC

# Load shapefiles from the specified directory
spatial_names <- grep(
  ".shp", 
  list.files(neon_dir, full.names = TRUE), 
  value = TRUE
)

# Read spatial polygon files into a list
spatial_polys <- lapply(spatial_names, st_read)

# Load climate rasters into a list
clim_ras <- list.files(
  clim_tif,
  full.names = TRUE
) %>% 
  lapply(terra::rast)

# Loop through each set of polygons for processing
for (i in 1:length(spatial_polys)) {
  polygons <- spatial_polys[[i]]  # Select the current polygon set
  
  print(paste0("Processing ", spatial_names[[i]]))
  
  id_col <-ifelse(grepl("tower", spatial_names[[i]]), "siteID", 
            ifelse(grepl("plot", spatial_names[[i]]), "plotID",
            ifelse(grepl("site", spatial_names[[i]]), "siteID",
            ifelse(grepl("domain_radii", spatial_names[[i]]), "siteID",
            ifelse(grepl("domain_footprint", spatial_names[[i]]), "domainNumb", id_col)))))

  # Loop through each polygon in the set
  for (j in 1:length(polygons$geometry)) {
    poly <- polygons[j, ]  # Select the current polygon
    
    id_val <- st_drop_geometry(poly[1, id_col])[[1]]
    
    # Process each climate raster for the current polygon
    for (m in 1:length(clim_ras)) {
      if(st_is_empty(poly$geometry)){
        next
      }else(
        ras <- clim_ras[[m]] %>% intersect_raster_with_polygon(st_buffer(poly, 10), .)
      )
      # Extract variable name from raster name
      parts <- strsplit(names(ras), "_")[[1]]
      num <- sub("bio", "", parts[2])
      var_name <- paste0("bio", sprintf("%02d", as.integer(num)))
      
      # Save output raster for later visualizations
      ras_name <- paste0(tools::file_path_sans_ext(basename(spatial_names[i])), "_", id_val, "_", var_name, ".tif")
      terra::writeRaster(ras, file.path(clim_tif_neon, ras_name), overwrite=T)
      
      # Calculate the mean value of the raster for the polygon
      val <- mean(terra::values(ras), na.rm = TRUE)
      
      # Assign the mean value to the respective column in the polygons data
      polygons[j, paste0(var_name, "_mean")] <- val
      
      if(var_name %in% c("bio01", "bio12")){
        t <- calculate_geodiversity_metrics(ras, metrics_list)
        names(t) <- paste0(var_name, "_", names(t))
        polygons[j, names(t)] <- t
      }
    }
  }
  
  # Generate a cleaned file name for output
  file_name <- sub(".*/([^/]+)\\.[^\\.]+$", "\\1", spatial_names[i])
  
  # Save the updated polygons as a new shapefile
  st_write(polygons, paste0(output_dir_clim, "/", file_name, ".shp"))
  
  # Replace the processed polygons in the list
  spatial_polys[[i]] <- polygons
}
