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
# i <- commandArgs(trailingOnly = TRUE) %>% as.numeric()
# i <- 1                             # Uncomment to manually test locally


# -----------------------------------------------------------------------------
# Setup Inputs: Raster Tiles, Domain Polygons, and Metric List
# -----------------------------------------------------------------------------


# Locate the spatial domain polygon shapefile(s)
# spatial_poly_paths <- grep("domain_footprint.shp", list.files(output_dir_clim, full.names = TRUE), value = TRUE)
# spatial_poly_names <- grep("domain_footprint.shp", list.files(output_dir_clim), value = TRUE) %>% gsub("\\.shp$", "", .)
spatial_poly_paths <- grep("plot_radii.shp", list.files(output_dir_clim, full.names = TRUE), value = TRUE)
spatial_poly_names <- grep("plot_radii.shp", list.files(output_dir_clim), value = TRUE) %>% gsub("\\.shp$", "", .)

ras_paths <- grep("plot", list.files(elev_vrt, full.names = TRUE), value = TRUE)

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
# ras <- terra::rast("/mnt/scratch/kapsarke/neonEnvData/L1/elev_vrt_300m/NEON_site_radii_ABBY_300.vrt")
ras <- terra::rast("/mnt/scratch/kapsarke/neonEnvData/L1/elev_vrt_300m/NEON_plot_radii_STEI_019.vrt")

# mask 
m <- st_read(spatial_poly_paths) %>% filter(plotID == "STEI_019") # %>% vect()

ras_mask <- ras %>% terra::crop(st_bbox(st_buffer(m, 1000))) # %>%  terra::mask(m)

# test <- focal_metrics(ras, window = matrix(1,101,101), metrics = list("sa"), progress=T)
test_sq <- texture_image(ras_mask, window_type = "square", size = 3, in_meters=F, metric = "sq", parallel=F) 
test_sdq <- texture_image(ras_mask, window_type = "square", size = 3, in_meters=F, metric = "sdq", parallel=F) 
test_sbi <- texture_image(ras_mask, window_type = "square", size = 3, in_meters=F, metric = "sbi", parallel=F) 
test_ssk <- texture_image(ras_mask, window_type = "square", size = 3, in_meters=F, metric = "ssk", parallel=F) 
test_sku <- texture_image(ras_mask, window_type = "square", size = 3, in_meters=F, metric = "sku", parallel=F) 
test_sfd <- texture_image(ras_mask, window_type = "square", size = 3, in_meters=F, metric = "sfd", parallel=F) 
test_sds <- texture_image(ras_mask, window_type = "square", size = 3, in_meters=F, metric = "sds", parallel=F) # Longest metric to run 

test_sq
test_sdq
test_sbi
test_ssk
test_sku
test_sfd
test_sds

# TEST results from square window, 100 x 100 raster (Oregon - vignette data), window size = 3 
# sq works  
# sdq 0
# sbi works but has 90% correlation with sq... 
# ssk NaN
# sku max Inf 
# sfd NaN
# sds 0 value  

# TEST results from square window, 73x73 raster, window size = 3 
# sq works 
# sdq 0 value
# sbi works but has 99.99% correlation with sq... 
# ssk NaN
# sku max Inf
# sfd matrix too small (size = 3) 
# sds 0 value 


# TEST results from square window, 7x7 raster, window size = 3 
# sq works 
# sdq 0 value
# sbi works but has 99.99% correlation with sq... 
# ssk NaN
# sku max Inf
# sfd matrix too small (size = 3) 
# sds 0 value 

# TEST results from circle window, 7x7 raster, window size = 3 
# sq works 
# sdq 0 value
# sbi works but has 99.99% correlation with sq... 
# ssk NaN
# sku max Inf
# sfd matrix too small (size = 3) 
# sds 0 value 


# test <- texture_image(ras, window_type = "circle", size = 5, in_meters=F, metric = "sds", parallel=T, ncores = 5, nclumps=100)


# Generating subset of data from geodiv vignette for state of Oregon to test on 

library(terra)

# Load your raster
r <- ras_mask

# Get raster dimensions
ncol_r <- ncol(r)
nrow_r <- nrow(r)

# Center cell indices
center_col <- ceiling(ncol_r / 2)
center_row <- ceiling(nrow_r / 2)
half_size <- 50  # Half of 100

# Define bounds in row/col
start_col <- center_col - half_size + 1
end_col   <- center_col + half_size
start_row <- center_row - half_size + 1
end_row   <- center_row + half_size

# Convert row/col bounds to spatial coordinates
ul <- xyFromCell(r, cellFromRowCol(r, start_row, start_col))  # upper-left
lr <- xyFromCell(r, cellFromRowCol(r, end_row, end_col))      # lower-right

# Create extent from coordinates
sub_ext <- ext(ul[1], lr[1], lr[2], ul[2])

# Crop raster to this extent
test_raster <- crop(r, sub_ext)

ras_mask <- test_raster %>% remove_plane()

# Plot to confirm
plot(test_raster)