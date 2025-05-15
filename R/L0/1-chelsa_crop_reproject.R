# TITLE:            Climate Data Preprocessing
# PROJECT:          NEON Geodiversity Analysis
# AUTHORS:          Kelly Kapsar, Lala Kounta, Pat Bills, Phoebe Zarnetske 
# DATA INPUT:       CHELSA bioclimatic variables (annual mean values 1981-2009)
# DATA OUTPUT:      CHELSA bioclimatic rasters projected to EPSG:5070 and cropped to North America
# DATE:             December 2024
# OVERVIEW:         This script crops and reprojects climate raster data in parallel
# REQUIRES:         R libraries: sf, terra, doParallel, foreach
# NOTES:            Ensure input data files and output directories are properly configured

# Load necessary libraries
library(sf)        # For spatial data handling
library(terra)     # For raster data manipulation
library(doParallel) # For parallel processing
library(foreach)   # For parallel iteration

# Source configuration settings (e.g., custom projections or paths)
# source("./R/config.R")
source("../config.R")

# List climate data files and their names
clim_data <- list.files(
  raw_clim_dir,
  full.names = TRUE
)
clim_names <- list.files(raw_clim_dir)

# Set up parallel processing backend
# Use the number of cores available on the MSU HPCC cluster
registerDoParallel(cores = as.numeric(Sys.getenv("SLURM_CPUS_ON_NODE")[1]))

# Process each climate data file in parallel
cropped_projected <- foreach(i = 1:length(clim_data), .packages = c("terra")) %dopar% {
  
  # Print progress message for the current file
  print(paste0("Loading ", clim_names[[i]]))
  
  # Load raster file
  r <- rast(clim_data[[i]])
  
  # Crop raster to the defined extent
  cropped <- crop(r, extent)
  print(paste0(clim_names[[i]], " cropped."))
  
  # Reproject cropped raster to the desired CRS (EPSG:5070)
  projected <- project(cropped, "EPSG:5070")
  
  # Save the processed raster to the output directory
  writeRaster(
    projected,
    paste0(clim_tif, "/", clim_names[[i]]),
    overwrite = TRUE
  )
  
  # Return the processed raster (optional, for the result list)
  return(projected)
}

# The result is a list of cropped and projected rasters
