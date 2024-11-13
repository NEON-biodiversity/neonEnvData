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

spat_data <- list.files("/mnt/scratch/plz-lab/geodiversity/spatial_data/polys_EPSG5070/") %>% grep("\\.shp$", ., value=TRUE)
tiles <- 


ls <- list.files("/mnt/scratch/plz-lab/geodiversity/output", full.names=TRUE) %>%
  grep("intersected", ., value = TRUE)
df <- read.csv(ls[23])

# Isolate one spatial unit
d <- df[df$siteID == "DSNY",]

# Convert csv back to raster
coordinates <- vect(d, geom = c("x", "y"), crs = "EPSG:4326") 

tiles <- list.files("/mnt/scratch/plz-lab/geodiversity/SRTM_gl1_v003", full.names=TRUE) %>% 
  grep( paste(unique(d$tile_id), collapse = "|"), ., value = TRUE) %>% 
  grep("\\.zip$", ., value = TRUE)
  
temp <- lapply(tiles, function(x){unzip(x, exdir = "/mnt/scratch/plz-lab/geodiversity/SRTM_gl1_v003/unzipped_hgt") %>% rast})

t <- do.call(terra::mosaic, temp) %>% terra::crop(., st_bbox(coordinates))

ras <- rasterize(coordinates, t, field = "elevation")


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


