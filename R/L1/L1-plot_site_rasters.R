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
library(ggplot2)

source("./R/config.R")

site <- st_read("/mnt/scratch/plz-lab/geodiversity/spatial_data/polys_EPSG5070/NEON_sites.shp", quiet = TRUE)

site_radii <- st_read("/mnt/scratch/plz-lab/geodiversity/spatial_data/polys_EPSG5070/site_radii.shp", quiet = TRUE)

domain_radii <- st_read("/mnt/scratch/plz-lab/geodiversity/spatial_data/polys_EPSG5070/domain_radii.shp", quiet = TRUE)

srtm_tiles <- st_read("/mnt/scratch/plz-lab/geodiversity/spatial_data/SRTM_tiles/srtm_grid_1deg.shp") %>% st_transform(crs = 5070)

srtm_tile_files <- list.files("/mnt/scratch/plz-lab/geodiversity/SRTM_gl1_v003/tiles_EPSG5070", full.names = TRUE)

plot_site_elevations <- function(
    srtm_tiles, 
    srtm_tile_files, 
    site_radii, 
    site) {
  
  # Loop through each polygon
  for (i in 1:nrow(site_radii)) {
    
    cutter <- site_radii[i, ]
    
    site_code <- site_radii$siteID[i]
    
    radii_data <- site_radii[site_radii$siteID == site_code,]
    
    print(paste0("Processing polygon ", i, " of ", nrow(site_radii), "."))
    
    # Find intersecting SRTM tiles
    intersecting_tiles <- srtm_tiles[st_intersects(cutter, srtm_tiles, sparse = FALSE), ]
    
    if(length(intersecting_tiles$id) == 0){
      next
    }
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
    # Convert the raster to a data frame for ggplot
    ras1_df <- as.data.frame(ras1, xy = TRUE, na.rm = TRUE)
    colnames(ras1_df) <- c("x", "y", "value")  # Ensure columns are named correctly
    
    # Generate the plot
    fig_site_radii <- ggplot() +
      geom_raster(data = ras1_df, aes(x = x, y = y, fill = value)) +
      scale_fill_viridis_c(option = "viridis") +  # Apply the viridis color scale
      # geom_sf(data = site[site$siteID == site_code,], fill = NA, color = "white", lwd = 1) +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_blank(), 
        legend.position = "none"
      )
    ggsave(paste0("/mnt/scratch/plz-lab/geodiversity/output/figures/", site_code, "_plain.png" ), 
           fig_site_radii, width = 4, height = 4, units = "in", dpi = 300)
  }
}

  plot_site_elevations(
    srtm_tiles, 
    srtm_tile_files, 
    site_radii, 
    site)
