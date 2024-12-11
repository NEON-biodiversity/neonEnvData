# Load necessary libraries
library(sf)
library(terra)
library(stringr)

source("./R/L1/L1-functions.R")
spatial_names <- grep(".shp", list.files("/mnt/scratch/plz-lab/geodiversity/output/polys_EPSG5070_intersected/", full.names = TRUE), value=TRUE)
# spatial_names <- grep(".shp", list.files("/mnt/scratch/plz-lab/geodiversity/spatial_data/polys_EPSG5070/", full.names = TRUE), value=TRUE)
spatial_polys <- lapply(spatial_names, st_read)

out_dir <- "/mnt/scratch/plz-lab/geodiversity/output/polys_EPSG5070_clim_elev/"

clim_ras <- list.files("/mnt/scratch/plz-lab/geodiversity/spatial_data/climate_EPSG5070", full.names = T) %>% lapply(., terra::rast)

# Loop through sets of polygons and process
for (i in 1:length(spatial_polys)) {
  polygons <- spatial_polys[[i]]
  
  # Create a placeholder for new columns for each climate variable
  for (k in 1:length(clim_ras)) {
    parts <- strsplit(names(clim_ras[[k]]), "_")[[1]]
    var_name <- parts[2]  # Extract variable name (e.g., "bio1")
    polygons[[var_name]] <- NA  # Initialize column with NA
  }
  
  for (j in 1:length(polygons$geometry)) {
    poly <- polygons[j,]
    print(paste0("Working on ", poly$siteID))
    
    for (m in 1:length(clim_ras)) {rm 
      ras <- clim_ras[[m]] %>% intersect_raster_with_polygon(st_buffer(poly, 10), .)
      
      # Extract variable name from raster name
      parts <- strsplit(names(ras), "_")[[1]]
      var_name <- parts[2]
      
      print(var_name)
      
      # Calculate mean value for the polygon
      val <- mean(terra::values(ras), na.rm = TRUE)
      
      # Assign the mean value to the respective column in the polygons data frame
      polygons[j, var_name] <- val
    }
  }
  file_name <- sub(".*/([^/]+)\\.[^\\.]+$", "\\1", spatial_names[i])
  
  st_write(polygons, paste0(out_dir, file_name, "_clim_elev.shp"))
  # Replace the processed polygons in the spatial_polys list
  spatial_polys[[i]] <- polygons
}


# test <- st_read("/mnt/scratch/plz-lab/geodiversity/output/polys_EPSG5070_clim_elev/plot_radii_clim_elev.shp")
