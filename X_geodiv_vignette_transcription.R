# TITLE:            Geodiversity Vignette
# PROJECT:          NEON Geodiversity Analysis
# AUTHORS:          Kelly Kapsar, Pat Bills, Phoebe Zarnetske 
# COLLABORATORS:    Lala Kounta
# DATA INPUT:       
# DATA OUTPUT:      
# DATE:             
# OVERVIEW:        
# REQUIRES:         
# NOTES:            

library(geodiv)
library(raster)
library(rastervis)
library(mapdata)
library(maptools)
library(rgeos)
library(ggplot2)
library(tidyverse)
library(parallel)
library(sf)
library(rasterVis)
library(ggmap)
library(corrplot )
library(gridExtra)
library(cowplot)
library(cluster)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

fs_data <- list("https://ndownloader.figshare.com/files/24366086", 
                "https://ndownloader.figshare.com/files/28259166")

options(timeout = 1000)

get_raster <- function(rasts){
  tf <- tempfile()
  tryCatch(download.file(rasts, destfile = tf, mode = 'wb'), 
  error = function(e) 'File download unsuccessful.')
outrast <- terra::rast(tf)
return(outrast)
}

evi <- get_raster(fs_data[[1]])*0.0001
evi <- raster::aggregate(evi, fact=4) # 1 km resolution, 440,640 cells 

# state <- maps::map(database = 'state', regions = 'oregon', 
# fill = TRUE, plot = FALSE)

# Download US states shapefile (you can set cb = TRUE for a simplified version)
states <- states(cb = TRUE)

# Filter for Oregon
statePoly <- states[states$NAME == "Oregon", ] %>% st_transform(., st_crs(evi))

# Mask 
evi_masked <- mask(x = evi, mask = statePoly)

rasterVis::levelplot(evi_masked, margin = F, par.settings = eviTheme, 
                     ylab = NULL, xlab = NULL, 
                     main = 'Maximum Growing Season EVI')

evi_masked <- remove_plane(evi_masked)

rasterVis::levelplot(evi_masked, margin = F, par.settings = eviTheme, 
                     ylab = NULL, xlab = NULL, 
                     main = 'EVI without Trend')

system.time(outrast <- texture_image(evi_masked, window_type = 'square', 
                                     size = 5, in_meters = FALSE, metric = 'sa', 
                                     parallel = FALSE, nclumps = 100))

# 200 GB RAM took 64 seconds on 440640 pixels 

data_evi <- data.frame(x = terra::crds(outrast)[,1], 
                       y = terra::crds(outrast)[,2])

data_evi[,3] <- outrast[]




