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
# library(DBI)
library(RSQLite)
library(glue)
library(readr)

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



################################################################################

template <- read_file("./metadata/metadata_template.XML")  # your template above

nm <- list.files(out_path)

titles <-  str_replace(nm, ".gpkg", "") %>% 
  str_replace(., "_", " ") %>% 
  str_replace(., "_", " ") %>% 
  str_replace(., "_", " ") %>% 
  str_replace(., "_", " ")

meta_df <- data.frame(
  file_id = 1:13,
  title = titles,
  date = rep(Sys.Date(), 13),
  abstract = rep(c("Temperature, precipitation, and elevation are important environmental drivers of biodiversity. Understanding the roles these drivers play across multiple scales will improve the ability to predict biodiversity and how changes in environmental conditions are likely to impact it. However, accessing and using large, high resolution data sets is often challenging for ecologists not trained in supercomputing and big data analysis. To help overcome this barrier, we present a reproducible, open workflow and associated ready-to-use data set of climate and elevation geodiversity (surface variability) metrics across multiple spatial scales for National Ecological Observatory Network (NEON) sites across the United States. We derived these metrics from “Climatologies at high resolution for the earth’s land surface areas” (CHELSA) and NASA’s Shuttle Radar Topography Mission (SRTM). Spatial scales of the data include plot, site, and domain. We calculated metrics for both the original site and domain footprints as well as at equally sized buffers around NEON sites (15 km) and domains (100 km). This radius-based approach mitigates some bias induced by size and location differences among NEON sites and domains. The final data set contains over 20,400 values derived from 19 bioclimatic variables and high resolution elevation data calculated across 3 spatial extents and multiple spatial resolutions. These analysis-ready data, along with the associated open and reproducible R workflow used to generate them, will offer opportunities to examine the multiscalar effects of climate and elevation geodiversity on ecological processes. 
"), 13),
  keywords = rep(c("precipitation, temperature, elevation, geodiversity, National Ecological Observatory Network (NEON)"), 13),
  license = rep("CC BY", 13)
)

for (i in 1:nrow(meta_df)) {
  print(i)
  xml <- glue_data(meta_df[i, ], template, .open = "{", .close = "}")
  meta_path <- paste0("./metadata/file_", i, ".xml")
  write_file(xml, meta_path)
  tbl <- list.files(out_path, full.names = T)[[i]]
  tbl_name <- str_replace(basename(tbl), ".gpkg", "")
  
  insert_metadata(gpkg_path = tbl, 
                  xml_path = meta_path, 
                  file_id = 1, 
                  table_name = tbl_name)
  
}

# TESTING 
# gpkg_path = tbl
# xml_path = meta_path
# file_id = 1
# table_name = tbl_name

con <- dbConnect(SQLite(), tbl)
dbGetQuery(con, "SELECT * FROM gpkg_metadata")
dbGetQuery(con, "SELECT * FROM gpkg_metadata_reference")
dbGetQuery(con, "
  SELECT r.reference_scope, r.table_name, m.metadata
  FROM gpkg_metadata_reference r
  JOIN gpkg_metadata m ON r.md_file_id = m.id
")
dbDisconnect(con)
