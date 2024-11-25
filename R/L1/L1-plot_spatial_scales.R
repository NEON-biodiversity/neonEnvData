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

source("./R/config.R")

site <- st_read("../spatial_scales/data/L0/NEON_spatial/EPSG5070/NEON_sites.shp", quiet = TRUE)

site_radii <- st_read("../spatial_scales/data/L0/NEON_spatial/EPSG5070/site_radii.shp", quiet = TRUE)

plt_radii <- st_read("../spatial_scales/data/L0/NEON_spatial/EPSG5070/plot_radii.shp", quiet = TRUE)

srtm_tiles <- st_read("/mnt/scratch/plz-lab/geodiversity/spatial_data/SRTM_tiles/srtm_grid_1deg.shp") %>% st_transform(crs = 5070)

srtm_tile_files <- list.files("/mnt/scratch/plz-lab/geodiversity/SRTM_gl1_v003/tiles_EPSG5070", full.names = TRUE)

plot_site_elevations <- function(
    srtm_tiles, 
    srtm_tile_files, 
    site_radii, 
    site, 
    plot_data) {
  
  # Loop through each polygon
  for (i in 1:nrow(site)) {
    
    cutter <- site_radii[i, ]
    
    site_code <- site_radii$siteID[i]
    
    plots_data <- plt_circle_center[plt_circle_center$siteID == site_code,]
    
    radii_data <- site_radii[site_radii$siteID == site_code,]
    
    print(paste0("Processing polygon ", i, " of ", nrow(site_radii), "."))
    
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
        ras1 <- 
          do.call(terra::mosaic, temp) %>% 
          terra::crop(., st_bbox(cutter)) %>% 
          terra::mask(cutter)
      } else {
        ras1 <- 
          temp[[1]] %>% 
          terra::crop(., st_bbox(cutter)) %>% 
          terra::mask(cutter)
      }
      
      fig_site_radii <- ggplot() +
        geom_sf(data = site_radii, fill = "#44aea3") +
        geom_sf(data = site_data, fill = NA, color = "white", lwd=1) +
        geom_sf(data = plots_data, shape = 21, fill = "#b8e6a5", color = "black", size = 3) +
        theme_minimal() +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.title = element_blank()
        )
      
    }}}
    


for(i in 1:length(site_radii)){

  results <- calculate_geodiv_metrics(srtm_tiles, srtm_tile_files, site_radii[[i]], metrics_list)
  st_write(results, paste0("/mnt/scratch/plz-lab/geodiversity/output/polys_EPSG5070_intersected/", 
                           grep(".shp", list.files("/mnt/scratch/plz-lab/geodiversity/spatial_data/polys_EPSG5070/"), value=T)[i]))
  print(  grep(".shp", list.files("/mnt/scratch/plz-lab/geodiversity/spatial_data/polys_EPSG5070/"), value=T)[i])
}
