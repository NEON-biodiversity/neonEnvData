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

spat_data <- list.files("/mnt/scratch/plz-lab/geodiversity/spatial_data/polys_EPSG5070/", full.names=T) %>% grep("\\.shp$", ., value=TRUE)

srtm_tiles <- st_read("/mnt/scratch/plz-lab/geodiversity/spatial_data/SRTM_tiles/srtm_grid_1deg.shp") %>% st_transform(prj)

################################################################################
###### Method using raw hgt files and cropping 
start_time <- Sys.time()

cutters <- st_read(spat_data[3])
cutter <- cutters[cutters$siteID == "ABBY",]

test <- srtm_tiles[st_intersects(cutter, srtm_tiles, sparse=F),]

tiles <- list.files("/mnt/scratch/plz-lab/geodiversity/SRTM_gl1_v003/tiles_EPSG5070", full.names=TRUE) %>%
  grep( paste(unique(test$id), collapse = "|"), ., value = TRUE)

# temp <- lapply(tiles, function(x){unzip(x, exdir = "/mnt/scratch/plz-lab/geodiversity/SRTM_gl1_v003/unzipped_hgt") %>% rast})
temp <- lapply(tiles, rast)

if(length(temp) > 1){
  ras1 <- do.call(terra::mosaic, temp) %>% terra::crop(., st_bbox(cutter) %>% terra::mask(cutter))
}else(ras1 <- temp[[1]] %>% terra::crop(., st_bbox(cutter)) %>% terra::mask(cutter))

plot(ras1)

end_time <- Sys.time() - start_time
print(end_time)

################################################################################
###### Method using output from duckdb 
start_time2 <- Sys.time()

ls <- list.files("/mnt/scratch/plz-lab/geodiversity/output", full.names=TRUE) %>%
  grep("intersected", ., value = TRUE)
df <- read.csv(ls[23])

# Isolate one spatial unit
d <- df[df$siteID == "ABBY",]

# Convert csv back to raster
coordinates <- vect(d, geom = c("x", "y"), crs = "EPSG:4326") %>% terra::project("EPSG:5070")

r_template <- rast(ext=ext(coordinates), resolution = 30, crs = "EPSG:5070")
ras2 <- rasterize(coordinates, r_template, field = "elevation")
plot(ras2)

end_time2 <- Sys.time() - start_time2
print(end_time2)
################################################################################

mets <- c("sq", "sdq", "sbi", "ssk", "sku", "sfd", "sds", "std")
# NOTE stdi is the actual metric we're using, but it comes out of calling std in the function call 

geodiv::sq(test)
geodiv::sdq(test)
geodiv::sbi(test)
geodiv::ssk(test)
geodiv::sku(test)
geodiv::sfd(test)
geodiv::sds(test)
geodiv::std(test)[2]

win <- matrix(1, nrow = 7, ncol = 7)

geodiv::focal_metrics(test, window = win, metrics = "sq", progress=TRUE)


