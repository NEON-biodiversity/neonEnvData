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
site <- st_read(paste0(neon_dir,
  "/NEON_site_footprint.shp"),
  quiet = TRUE
)

site_radii <- st_read(paste0(neon_dir,
  "/NEON_site_radii.shp"),
  quiet = TRUE
)

domain_radii <- st_read(paste0(neon_dir,
  "/NEON_domain_radii.shp"),
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
    site_footprint <- site[site$siteID == site_code,]
    
    # Print progress message
    print(paste0("Processing polygon ", i, " of ", nrow(site_radii), "."))
    
    r <- paste0(elev_vrt, "/NEON_site_radii_", site_code, ".vrt")
    
    if(file.exists(r)){               
      t <- terra::vrt(r) %>% 
        terra::mask(radii_data) %>% 
        terra::crop(radii_data)
      
      # Convert the processed raster to a data frame for plotting
      ras1_df <- as.data.frame(t, xy = TRUE, na.rm = TRUE)
      colnames(ras1_df) <- c("x", "y", "value")  # Ensure proper column names
      
      # Create a plot using ggplot
      fig_site_radii <- ggplot() +
        geom_raster(data = ras1_df, aes(x = x, y = y, fill = value)) +
        scale_fill_viridis_c(option = "viridis") +  # Use the viridis color scale
        geom_sf(data = site_footprint, color = "white", fill = NA, size = 0.5) +  # Added white outline
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
          figures, "/",
          site_code, "_plain.png"
        ), 
        fig_site_radii, 
        width = 4, height = 4, units = "in", dpi = 300
      )
  }else(next)
    }
}

# Call the function to generate plots
plot_site_elevations(
  srtm_tiles, 
  srtm_tile_files, 
  site_radii, 
  site
)
