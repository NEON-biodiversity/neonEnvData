
# Load packages
library(tidyverse)
library(sf)

# Load in data_dir location
source("./R/config.R")

# Specify projection 
prj <- "EPSG:5070" # Albers equal area 

# Import NEON data 
dom <- st_read(paste0(data_dir, "NEON_spatial/NEON_domains/NEON_Domains.shp"), quiet=T) %>% 
  st_transform(prj) %>% 
  st_write(paste0(out_dir,"/EPSG5070/NEON_domains/NEON_domains.shp"), append=F)

site <- st_read(paste0(data_dir, "NEON_spatial/NEON_sites/terrestrialSamplingBoundaries.shp"), quiet=T)  %>% 
  st_transform(prj) %>% 
  st_write(paste0(out_dir,"/EPSG5070/NEON_sites/NEON_sites.shp"), append=F)

plt <- st_read(paste0(data_dir, "NEON_spatial/NEON_TOS_Plot_Points/NEON_TOS_Plot_Points.shp"), quiet=T)  %>% 
  filter(subtype == "mammalGrid") %>% 
  st_transform(prj)  %>% 
  st_write(paste0(out_dir, "/EPSG5070/NEON_small_mammal_plots/NEON_small_mammal_plots.shp"), append=F)


# Define functions 
id_circle_center <- function(d){
  du <- st_union(d)
  b <- lwgeom::st_minimum_bounding_circle(du)
  c <- st_centroid(b)
  return(c)
}

max_dist_to_circle_center <- function(d){
  du <- st_union(d)
  b <- lwgeom::st_minimum_bounding_circle(du)
  c <- st_centroid(b)
  dp <- d %>% st_cast("POINT")
  dist <- st_distance(dp, c)
  return(as.numeric(max(dist)))
}

################################################################################
##### PLOT SCALE (MAMMALS) ##### 

##### SITE SCALE ##### 
plt_nested <- plt %>% 
  st_make_valid() %>% 
  nest(.by = plotID) %>% 
  mutate(circle_center = lapply(data, function(x) id_circle_center(x)), 
         max_dist_circ = lapply(data, function(x) max_dist_to_circle_center(x))) %>% 
  unnest(cols = c(max_dist_circ, circle_center)) %>% 
  mutate(plot_poly = st_buffer(circle_center, dist = 100))
# proc.time()-start

# start <- proc.time()
site_nested <- plt %>% 
  st_make_valid() %>% 
  nest(.by = siteID) %>% 
  mutate(circle_center = lapply(data, function(x) id_circle_center(x)), 
         max_dist_circ = lapply(data, function(x) max_dist_to_circle_center(x))) %>% 
  unnest(cols = c(max_dist_circ, circle_center)) %>% 
  mutate(buff_dist =15000) %>% 
  # mutate(buff_dist = max(max_dist_circ)) %>% 
  mutate(site_poly = st_buffer(circle_center, dist = buff_dist))
# proc.time()-start


plt_circle_center <- plt_nested %>% 
  dplyr::select(plotID, circle_center) %>% 
  rename(geometry = circle_center) %>% 
  st_as_sf() %>% 
  st_write(paste0(out_dir, "/EPSG5070/NEON_radii_centers/plot_circle_centers.shp"))

plt_radii <- plt_nested %>% 
  dplyr::select(plotID, plot_poly) %>% 
  rename(geometry = plot_poly) %>% 
  st_as_sf() %>% 
  st_write(paste0(out_dir, "/EPSG5070/NEON_radii_centers/plot_radii.shp"))

site_circle_center <- site_nested %>% 
  dplyr::select(siteID, circle_center) %>% 
  rename(geometry = circle_center) %>% 
  st_as_sf() %>% 
  st_write(paste0(out_dir, "/EPSG5070/NEON_radii_centers/site_circle_centers.shp"))

site_radii <- site_nested %>% 
  dplyr::select(siteID, site_poly) %>% 
  rename(geometry = site_poly) %>% 
  st_as_sf() %>% 
  st_write(paste0(out_dir, "/EPSG5070/NEON_radii_centers/site_radii.shp"))

dom_radii <- site_nested %>% 
  dplyr::select(siteID, circle_center) %>% 
  rename(geometry = circle_center) %>% # 100 km centroid around 
  st_as_sf() %>% 
  st_buffer(100000) %>% 
  st_write(paste0(out_dir, "/EPSG5070/NEON_radii_centers/domain_radii.shp"))
################################################################################
# Modify site data to indicate whether small mammal plots are present & add climate data siteID

site_with_mamm <- site %>% st_make_valid() %>% st_intersects(plt, ., sparse=FALSE) %>% colSums()
site$mamm_pres <- ifelse(site_with_mamm == 0, FALSE, TRUE)

site$bioclim_id <- site$siteID
site$bioclim_id[site$siteName == "Blandy Experimental Farm Additional TOS Boundary at Casey Tree"] <- "BLAN_TOS"
site$bioclim_id[site$siteHost == "University of Alaska, Fairbanks" & site$siteID == "BONA"] <- "BONA_UA"
site$bioclim_id[site$siteHost == "Alaska Department of Natural Resources" & site$siteID == "BONA"] <- "BONA_ADNR"
site$bioclim_id[site$siteName == "Dakota Coteau Field School Additional TOS Boundary"] <- "DCFS_TOS"
site$bioclim_id[site$siteName == "Harvard Forest at Quabbin Reservoir"] <- "HARV_DCR"
site$bioclim_id[site$siteName == "Lenoir Landing Additional TOS Boundary at Choctaw National Wildlife Refuge"] <- "LENO_TOS"
site$bioclim_id[site$siteName == "Mountain Lake Biological Station Additional TOS Boundary"] <- "MLBS_TOS"
site$bioclim_id[site$siteName == "Northern Great Plains Research Laboratory Additional TOS Boundary"] <- "NOGP_TOS"
site$bioclim_id[site$siteName == "Rocky Mountain National Park CASTNET Additional TOS at Roosevelt National Forest"] <- "RMNP_TOS"
site$bioclim_id[site$siteName == "Smithsonian Environmental Research Center Additional TOS Boundary"] <- "SERC_TOS"
site$bioclim_id[site$siteName == "Steigerwaldt Additional TOS Boundary at Chequamegon National Forest"] <- "STEI_TOS"
site$bioclim_id[site$siteName == "North Sterling, CO Additional TOS Boundary"] <- "STER_TOS"
site$bioclim_id[site$siteName == "Treehaven Additional TOS Boundary"] <- "TREE_TOS"

st_write(site, paste0(out_dir,"/EPSG5070/NEON_sites/NEON_field_sites_mamm.shp"))
