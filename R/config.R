# TITLE:            Configuration Setup
# PROJECT:          NEON Environmental Data 
# AUTHORS:          Kelly Kapsar, Pat Bills, Phoebe Zarnetske 
# COLLABORATORS:    Lala Kounta
# DATA INPUT:       NA
# DATA OUTPUT:      File structure for neonEnvData project
# DATE:             September 2025
# OVERVIEW:         Script for generating file structure for project data and 
#                   specifying parameters (e.g. projection, radii distances). 

print(version)

list.of.packages <- c("geodiv", "terra", "sf", "dplyr", "tidyr", "lwgeom", "doParallel", "foreach", "stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='http://cran.us.r-project.org')


# Libraries 
library(terra)
library(sf)
library(tidyr)
library(dplyr)
library(lwgeom)
library(doParallel) # For parallel processing
library(foreach)   # For parallel iteration

################################################################################
#### USER SPECIFIED INPUTS ####

## Spatial Projection 
prj <- "EPSG:5070" # For input to projection functions
prj_name <- "EPSG5070" # For naming folders and files

## Buffer distance for radii around domain, site, and plot (in m)
dom_buff_dist = 100000
site_buff_dist = 15000
plt_buff_dist = 100

# Resolution for srtm data (if coarsened). Starting resolution = 30m 
elev_res <- "30"

## Project Directory 
proj_dir <- "PATH/TO/YOUR/PROJ/DIR/HERE"  # Change to relative path

# Specify geographic extent of the analysis 
# (so that not all data are loaded in -- saves memory)
# xmin = Minimum longitude
# ymin = Minimum latitude
# xmax = Maximum longitude
# ymax = Maximum latitude
# Format = ext(xmin, xmax, ymin, ymax)

# Define the extent for cropping
extent <- ext(-180, 14, -60, 90)

extent_sf <- c(xmin = -180, xmax = -60, ymax = 90, ymin = 14) %>%
  st_bbox(crs = st_crs(4326)) %>%
  st_as_sfc() %>%          
  st_transform(prj) %>% 
  st_sf()     

# Shapefile ID column (unique value per row in each shapefile for which 
# values are being calculated) 
id_col <- NA
# id_col <- "id" 

# Geodiversity metrics to be extracted 
metrics_list <- c("sq", "sdq", "sbi", "ssk", "sku", "std2", "sds")

################################################################################
#### AUTO-GENERATED FILE STRUCTURE ####

## Elevation
# L0
raw_elev_dir <- paste0(proj_dir, "L0/elev_srtm_gl1_v003/raw") # srtm raw data here
elev_hgt <- paste0(proj_dir, "L0/elev_srtm_gl1_v003/unzipped_hgt")
elev_tile_dir <- paste0(proj_dir, "L0/elev_srtm_gl1_v003/tiles")
elev_tiles <- paste0(proj_dir, "L0/elev_srtm_gl1_v003/tiles/srtm_grid_1deg.shp")


# L1
elev_tile_tif <- paste0(proj_dir, "L1/elev_EPSG5070")
# elev_tile_tif_300m <- paste0(proj_dir, "L1/elev_EPSG5070_300m")
elev_tif <- paste0(proj_dir, "L1/elev_tif")
# elev_tif_300m <- paste0(proj_dir, "L1/elev_tif_300m")
elev_vrt <- paste0(proj_dir, "L1/elev_vrt")
# elev_vrt_300m <- paste0(proj_dir, "L1/elev_vrt_300m")

## Climate
# L0
raw_clim_dir <- paste0(proj_dir, "L0/climate_chelsa_1981-2009")
# L1
clim_tif <- paste0(proj_dir, "L1/climate_", prj_name)
clim_tif_polygon <- paste0(proj_dir, "L1/climate_tif")

## NEON
# L0
polygon_raw <- paste0(proj_dir, "L0/polygon_spatial")
# L1
polygon_dir <- paste0(proj_dir, "L1/polygon_", prj_name)

## Output Directories
output_dir <- paste0(proj_dir, "L2")
output_dir_clim <- paste0(proj_dir, "L2/clim_only")
output_dir_clim_elev <- paste0(proj_dir, "L2/clim_elev")
# output_dir_clim_elev_300m <- paste0(proj_dir, "L2/clim_elev_300m")
figures <- paste0(proj_dir, "L2/figures")

# Add in changes to resolution of srtm if needed 
if(!is.na(elev_res)){
  elev_tif <- paste0(elev_tif, "_", elev_res, "m")
  output_dir_clim_elev <- paste0(output_dir_clim_elev, "_", elev_res, "m")
}


setup_project_directories <- function(proj_dir_name) {
  # Construct the full path for the project directory
  proj_dir <- file.path(proj_dir_name)
  
  # Create the project directory if it doesn't exist
  if (!dir.exists(proj_dir)) {
    dir.create(proj_dir, recursive = TRUE)
    message(paste("Created project directory:", proj_dir))
  } else {
    message(paste("Project directory already exists:", proj_dir))
  }
  
  # Define the full paths of required subfolders within the project directory
  required_dirs <- c(
    raw_elev_dir,
    elev_hgt,
    elev_tile_dir,
    elev_tile_tif,
    elev_vrt,
    elev_tif,
    raw_clim_dir,
    clim_tif,
    clim_tif_polygon,
    polygon_raw,
    polygon_dir,
    output_dir, 
    output_dir_clim, 
    output_dir_clim_elev,
    figures
  )
  
  # Loop through each directory and create it if it does not exist
  for (dir in required_dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      message(paste("Created directory:", dir))
    } else {
      message(paste("Directory already exists:", dir))
    }
  }
  
  message("All directories are set up!")
}

setup_project_directories(proj_dir_name = proj_dir)
