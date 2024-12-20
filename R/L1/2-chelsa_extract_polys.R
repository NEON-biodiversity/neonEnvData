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

# Source custom functions (e.g., intersect_raster_with_polygon)
source("./R/L1/2-functions.R")

# Load shapefiles from the specified directory
spatial_names <- grep(
  ".shp", 
  list.files("/mnt/scratch/plz-lab/geodiversity/output/polys_EPSG5070_intersected/", 
             full.names = TRUE), 
  value = TRUE
)

# Read spatial polygon files into a list
spatial_polys <- lapply(spatial_names, st_read)

# Output directory for processed shapefiles
out_dir <- "/mnt/scratch/plz-lab/geodiversity/output/polys_EPSG5070_clim_elev/"

# Load climate rasters into a list
clim_ras <- list.files(
  "/mnt/scratch/plz-lab/geodiversity/spatial_data/climate_EPSG5070",
  full.names = TRUE
) %>% 
  lapply(terra::rast)

# Loop through each set of polygons for processing
for (i in 1:length(spatial_polys)) {
  polygons <- spatial_polys[[i]]  # Select the current polygon set
  
  # Add columns for each climate variable to the polygon data
  for (k in 1:length(clim_ras)) {
    parts <- strsplit(names(clim_ras[[k]]), "_")[[1]]
    var_name <- parts[2]  # Extract variable name (e.g., "bio1")
    polygons[[var_name]] <- NA  # Initialize with NA values
  }
  
  # Loop through each polygon in the set
  for (j in 1:length(polygons$geometry)) {
    poly <- polygons[j, ]  # Select the current polygon
    print(paste0("Working on ", poly$siteID))  # Display progress
    
    # Process each climate raster for the current polygon
    for (m in 1:length(clim_ras)) {
      ras <- clim_ras[[m]] %>% intersect_raster_with_polygon(st_buffer(poly, 10), .)
      
      # Extract variable name from raster name
      parts <- strsplit(names(ras), "_")[[1]]
      var_name <- parts[2]
      
      print(var_name)  # Display the variable being processed
      
      # Calculate the mean value of the raster for the polygon
      val <- mean(terra::values(ras), na.rm = TRUE)
      
      # Assign the mean value to the respective column in the polygons data
      polygons[j, var_name] <- val
    }
  }
  
  # Generate a cleaned file name for output
  file_name <- sub(".*/([^/]+)\\.[^\\.]+$", "\\1", spatial_names[i])
  
  # Save the updated polygons as a new shapefile
  st_write(polygons, paste0(out_dir, file_name, "_clim_elev.shp"))
  
  # Replace the processed polygons in the list
  spatial_polys[[i]] <- polygons
}

# Example of reading the output for testing (optional)
# test <- st_read("/mnt/scratch/plz-lab/geodiversity/output/polys_EPSG5070_clim_elev/plot_radii_clim_elev.shp")
