# Load necessary libraries
library(sf)
library(terra)

spatial_polys <- lapply(grep(".shp", list.files("/mnt/scratch/plz-lab/geodiversity/spatial_data/polys_EPSG5070/", full.names = TRUE), value = TRUE), st_read)

clim_ras <- list.files("/mnt/scratch/plz-lab/geodiversity/spatial_data/clim_EPSG5070")


intersect_raster_with_polygon <- function(polygon, raster){
  out_ras <- terra::crop(raster, st_bbox(polygon)) %>%
    terra::mask(polygon)
  out_ras
}

