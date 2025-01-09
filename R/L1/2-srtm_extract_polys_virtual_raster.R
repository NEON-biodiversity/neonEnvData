
# Directory containing the raster files
input_directory <- "/mnt/scratch/kapsarke/geodiversity/SRTM_gl1_v003/tiles_EPSG5070"

# Get the list of .tif files
raster_files <- list.files(input_directory, pattern = "\\.tif$", full.names = TRUE)

# Output file path for the virtual raster
output_vrt <- "/mnt/scratch/kapsarke/geodiversity/virtual_raster.vrt"

# Create the virtual raster
vrt(raster_files[1:100], filename = output_vrt, overwrite=T)

# Load the virtual raster
vr <- rast(output_vrt)

# Print basic information about the virtual raster
print(vr)

# Plot the virtual raster (optional)
plot(vr)

library(geodiv)

geodiv::sq(vr)
