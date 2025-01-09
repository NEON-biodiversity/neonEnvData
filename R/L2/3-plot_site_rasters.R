# TITLE:            Geodiversity Metric Calculations
# PROJECT:          NEON Geodiversity Analysis
# AUTHORS:          Kelly Kapsar, Pat Bills, Phoebe Zarnetske 
# COLLABORATORS:    Lala Kounta
# DATA INPUT:       Reprojected SRTM_gl1_v003 from ./R/L0/1-srtm_unzip_reproject.R
# DATA OUTPUT:      Elevation plots for NEON sites
# DATE:             November 2024
# OVERVIEW:         Script to generate elevation plots
# REQUIRES:         R libraries: geodiv, terra, sf, dplyr, ggplot2
# NOTES:            Ensure SRTM data and shapefiles are correctly organized

# Load required libraries
library(geodiv)    # For geodiversity calculations
library(terra)     # For raster data manipulation
library(sf)        # For spatial data manipulation
library(dplyr)     # For data wrangling
library(ggplot2)   # For plotting

# Source configuration settings (e.g., projections or paths)
source("./R/config.R")

# Load spatial data (e.g., NEON sites and radii polygons)
site <- st_read(
  "/mnt/scratch/plz-lab/geodiversity/spatial_data/polys_EPSG5070/NEON_sites.shp",
  quiet = TRUE
)

site_radii <- st_read(
  "/mnt/scratch/plz-lab/geodiversity/spatial_data/polys_EPSG5070/site_radii.shp",
  quiet = TRUE
)

domain_radii <- st_read(
  "/mnt/scratch/plz-lab/geodiversity/spatial_data/polys_EPSG5070/domain_radii.shp",
  quiet = TRUE
)

# Load and transform SRTM tile grid shapefile to the desired projection
srtm_tiles <- st_read(
  "/mnt/scratch/plz-lab/geodiversity/spatial_data/SRTM_tiles/srtm_grid_1deg.shp"
) %>%
  st_transform(crs = 5070)

# List of processed SRTM raster files in the target projection
srtm_tile_files <- list.files(
  "/mnt/scratch/plz-lab/geodiversity/SRTM_gl1_v003/tiles_EPSG5070",
  full.names = TRUE
)

# Function to generate elevation plots for NEON sites
plot_site_elevations <- function(srtm_tiles, srtm_tile_files, site_radii, site) {
  
  # Loop through each polygon in site_radii
  for (i in 1:nrow(site_radii)) {
    # Define the current polygon and site code
    cutter <- site_radii[i, ]
    site_code <- site_radii$siteID[i]
    radii_data <- site_radii[site_radii$siteID == site_code, ]
    
    # Print progress message
    print(paste0("Processing polygon ", i, " of ", nrow(site_radii), "."))
    
    # Identify SRTM tiles intersecting with the current polygon
    intersecting_tiles <- srtm_tiles[st_intersects(cutter, srtm_tiles, sparse = FALSE), ]
    
    # Skip to the next polygon if no intersecting tiles are found
    if (length(intersecting_tiles$id) == 0) {
      next
    }
    
    # Get the list of raster files corresponding to the intersecting tiles
    tiles <- srtm_tile_files %>%
      grep(paste(unique(intersecting_tiles$id), collapse = "|"), ., value = TRUE)
    
    # Load and mosaic the intersecting tiles
    temp <- lapply(tiles, rast)
    
    # Mosaic and process the raster data
    if (length(temp) > 1) {
      ras1 <- do.call(terra::mosaic, temp) %>%
        terra::crop(., st_bbox(cutter)) %>%  # Crop to the bounding box of the polygon
        terra::mask(cutter)  # Mask to the shape of the polygon
    } else {
      ras1 <- temp[[1]] %>%
        terra::crop(., st_bbox(cutter)) %>%
        terra::mask(cutter)
    }
    
    # Convert the processed raster to a data frame for plotting
    ras1_df <- as.data.frame(ras1, xy = TRUE, na.rm = TRUE)
    colnames(ras1_df) <- c("x", "y", "value")  # Ensure proper column names
    
    # Create a plot using ggplot
    fig_site_radii <- ggplot() +
      geom_raster(data = ras1_df, aes(x = x, y = y, fill = value)) +
      scale_fill_viridis_c(option = "viridis") +  # Use the viridis color scale
      theme_minimal() +  # Minimalistic plot theme
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_blank(),
        legend.position = "none"
      )
    
    # Save the plot to a file
    ggsave(
      paste0(
        "/mnt/scratch/plz-lab/geodiversity/output/figures/", 
        site_code, "_plain.png"
      ), 
      fig_site_radii, 
      width = 4, height = 4, units = "in", dpi = 300
    )
  }
}

# Call the function to generate plots
plot_site_elevations(
  srtm_tiles, 
  srtm_tile_files, 
  site_radii, 
  site
)
