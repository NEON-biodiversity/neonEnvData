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
# Load Required Libraries
# -----------------------------------------------------------------------------
library(geodiv)
library(terra)
library(sf)
library(dplyr)


# -----------------------------------------------------------------------------
# Load Custom Functions and Configuration
# -----------------------------------------------------------------------------
source("./R/L1/2-3-functions.R")       # Custom geodiv functions
# source("2-3-functions.R")            # Alternate HPCC path if needed

source("./R/config.R")                # Project configuration (paths, CRS, etc.)
# source("../config.R")               # Alternate HPCC path if needed


# -----------------------------------------------------------------------------
# Read Command Line Argument (for batch/array job index)
# -----------------------------------------------------------------------------
i <- commandArgs(trailingOnly = TRUE) %>% as.numeric()
# i <- 1                             # Uncomment to manually test locally


# -----------------------------------------------------------------------------
# Setup Inputs: Raster Tiles, Domain Polygons, and Metric List
# -----------------------------------------------------------------------------

# Read and crop tile index shapefile
srtm_tiles <- st_read(elev_tiles) %>%
  st_crop(xmin = -180, xmax = -50, ymin = 0, ymax = 90) %>% 
  st_transform(crs = prj)

# Get list of SRTM raster tile files
srtm_tile_files <- list.files(elev_tile_tif, full.names = TRUE)

# Locate the spatial domain polygon shapefile(s)
# spatial_poly_paths <- grep("domain_footprint.shp", list.files(output_dir_clim, full.names = TRUE), value = TRUE)
# spatial_poly_names <- grep("domain_footprint.shp", list.files(output_dir_clim), value = TRUE) %>% gsub("\\.shp$", "", .)
spatial_poly_paths <- grep("site_radii.shp", list.files(output_dir_clim, full.names = TRUE), value = TRUE)
spatial_poly_names <- grep("site_radii.shp", list.files(output_dir_clim), value = TRUE) %>% gsub("\\.shp$", "", .)

ras_paths <- grep("domain", list.files(elev_vrt, full.names = TRUE), value = TRUE) %>% grep("_footprint", ., value = T)

# Define geodiversity metrics to calculate
metrics_list <- c("sq", "sdq", "sbi", "ssk", "sku", "sfd", "std2", "sds")


# -----------------------------------------------------------------------------
# Prepare Domain–Metric Combinations
# -----------------------------------------------------------------------------

# Read all domain polygons
spatial_polys <- st_read(spatial_poly_paths)

# Generate all combinations of domain numbers and metrics
combos <- expand.grid(spatial_polys$domainNumb, metrics_list)

# Select the polygon and metric for this index (i)
spatial_poly <- spatial_polys[spatial_polys$domainNumb == combos$Var1[i], ]
metric <- combos$Var2[i]

print(paste0("i = ", i))
print(head(combos))


# -----------------------------------------------------------------------------
# Run Processing Function
# -----------------------------------------------------------------------------
# ras <- terra::vrt(ras_paths[1])
ras <- terra::rast("/mnt/scratch/kapsarke/neonEnvData/L1/elev_vrt/NEON_site_radii_ABBY.vrt")

# mask 
m <- st_read(spatial_poly_paths) %>% filter(siteID == "ABBY") %>% vect()

ras_mask <- ras %>% terra::crop(m) %>%  terra::mask(m)

# test <- focal_metrics(ras, window = matrix(1,101,101), metrics = list("sa"), progress=T)
test <- texture_image(ras_mask, window_type = "circle", size = 5, in_meters=F, metric = "sds", parallel=T, ncores = 5, nclumps=100)


terra::writeRaster(test, "test_output.tif")

process_polygons(
  srtm_tiles = srtm_tiles,
  srtm_tile_files = srtm_tile_files,
  spatial_poly = spatial_poly,
  spatial_poly_name = combos$Var1[i],
  metrics_list = metric,
  vrt_dir = elev_vrt,
  tif_dir = elev_tif,
  output_dir = output_dir_domain_footprint_downscale300m
)
