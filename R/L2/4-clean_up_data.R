# =============================================================================
# TITLE:            Geopackage data cleaning
# PROJECT:          NEON Geodiversity Analysis
# AUTHORS:          Kelly Kapsar, Pat Bills, Phoebe Zarnetske 
# COLLABORATORS:    Lala Kounta
# DATA INPUT:       geopackage files output from 3-srtm_extract_polys.R
# DATA OUTPUT:      Clean geopackage feils with geodiversity metrics
# DATE:             July 2025
# OVERVIEW:         Adds srtm resolution to file name and converts columns with 
#                   all 0 values to NAs. 
# =============================================================================



library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(tools)
library(stringr)
library(ggplot2)

# Directories for 30m and 300m resolution shapefiles
dirs <- list(
  "clim_elev_30m" = "/mnt/research/neon/neonEnvData/L2/clim_elev_30m/",
  "clim_elev_300m" = "/mnt/research/neon/neonEnvData/L2/clim_elev_300m/"
)

out_path <- "/mnt/research/neon/neonEnvData/L2/clean_gpkg_files/"

# Load in functions
source("./R/L2/4-functions.R")


# Iterate through gpkg files to clean 
for (label in names(dirs)) {
  dir_path <- dirs[[label]]
  shapefiles <- list.files(dir_path, pattern = "\\.gpkg$", full.names = TRUE)
  
  for (shp_file in shapefiles) {
    
    shp_file <- clean_zero_na_columns(shp_file) %>% 
    tag_and_copy_gpkg(shp_file, out_path)
  }
}
