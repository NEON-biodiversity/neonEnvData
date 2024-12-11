# Load necessary libraries
library(sf)
library(terra)
library(doParallel)
library(foreach)

# Define the bounding box
xmin <- -180
ymin <- 14
xmax <- -60
ymax <- 90

extent <- ext(xmin, xmax, ymin, ymax)  # Define extent for cropping

# List climate data files
clim_data <- list.files("/mnt/research/ibeem/climate_NEON/biovars/1981-2009", full.names = TRUE)
clim_names <- list.files("/mnt/research/ibeem/climate_NEON/biovars/1981-2009")

# Set up parallel backend
## MSU HPCC: https://wiki.hpcc.msu.edu/display/ITH/R+workshop+tutorial#Rworkshoptutorial-Submittingparalleljobstotheclusterusing{doParallel}:singlenode,multiplecores
# Request a single node (this uses the "multicore" functionality)
registerDoParallel(cores=as.numeric(Sys.getenv("SLURM_CPUS_ON_NODE")[1]))

# Process each file in parallel
cropped_projected <- foreach(i=1:length(clim_data), .packages = c("terra")) %dopar% {
  print(paste0("Loading ", clim_names[[i]]))
  # Load raster
  r <- rast(clim_data[[i]])
    # Crop raster
  cropped <- crop(r, extent)
  print(paste0(clim_names[[i]], " cropped."))
  # Reproject raster
  projected <- project(cropped, "EPSG:5070")
  # Save processed raster
  writeRaster(projected, paste0("/mnt/scratch/plz-lab/geodiversity/spatial_data/climate_EPSG5070/", clim_names[[i]]), overwrite = TRUE)
}

# The result is a list of cropped and projected rasters
