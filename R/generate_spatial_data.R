
# Load packages
library(tidyverse)
library(sf)
library(lwgeom)
library(ggspatial)

# Load in data_dir location
source("./R/config.R")

# Specify projection 
prj <- "EPSG:5070" # Albers equal area 

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

###############################################################################
# Load and clean data 

# Import NEON data 
dom <- st_read(paste0(data_dir, "NEON_spatial/NEON_Domains/NEON_Domains.shp"), quiet=T) %>% 
  st_transform(prj) %>%
  # Standardize naming with plot and site-level 
  rename(domainNumb = DomainID, 
         domainName = DomainName) %>% 
  mutate(domainNumb = factor(sprintf("D%02d", domainNumb))) %>% 
  select(domainName, domainNumb) %>% 
  # Join together multiple polygons within same domain
  group_by(domainNumb) %>% 
  summarize(
    domainName = first(domainName), 
    geometry = st_union(geometry)
  ) # %>% 
  # st_write(paste0(out_dir,"EPSG5070/NEON_domains.shp"), append=F)

site <- st_read(paste0(data_dir, "NEON_spatial/NEON_sites/terrestrialSamplingBoundaries.shp"), quiet=T)  %>% 
  st_transform(prj) %>% 
  # Join multiple polygons with same siteID
  group_by(siteID) %>%
  summarize(
    domainName = first(domainName),
    domainNumb = first(domainNumb),
    geometry = st_union(geometry)
  ) # %>%
  st_write(paste0(out_dir,"EPSG5070/NEON_sites.shp"), append=F)


plt <- st_read(paste0(data_dir, "NEON_spatial/NEON_TOS_Plot_Points/NEON_TOS_Plot_Points.shp"), quiet=T)  %>% 
  filter(subtype == "mammalGrid") %>% 
  st_transform(prj) %>%
  # Add in domain information for plots 
  left_join(., st_drop_geometry(site %>% select(siteID, domainName, domainNumb)), by = c("siteID")) # %>% 
# st_write(paste0(out_dir, "EPSG5070/NEON_small_mammal_plots.shp"), append=F)

# Determine small mammal trapping presence at each site
site <- site %>% 
  mutate(mamm_pres = ifelse(
    colSums(st_intersects(plt, ., sparse=FALSE)) > 0, 
    TRUE, 
    FALSE
  ))


################################################################################
# Modify site data to indicate whether small mammal plots are present & add climate data siteID

# site$bioclim_id <- site$siteID
# site$bioclim_id[site$siteName == "Blandy Experimental Farm Additional TOS Boundary at Casey Tree"] <- "BLAN_TOS"
# site$bioclim_id[site$siteHost == "University of Alaska, Fairbanks" & site$siteID == "BONA"] <- "BONA_UA"
# site$bioclim_id[site$siteHost == "Alaska Department of Natural Resources" & site$siteID == "BONA"] <- "BONA_ADNR"
# site$bioclim_id[site$siteName == "Dakota Coteau Field School Additional TOS Boundary"] <- "DCFS_TOS"
# site$bioclim_id[site$siteName == "Harvard Forest at Quabbin Reservoir"] <- "HARV_DCR"
# site$bioclim_id[site$siteName == "Lenoir Landing Additional TOS Boundary at Choctaw National Wildlife Refuge"] <- "LENO_TOS"
# site$bioclim_id[site$siteName == "Mountain Lake Biological Station Additional TOS Boundary"] <- "MLBS_TOS"
# site$bioclim_id[site$siteName == "Northern Great Plains Research Laboratory Additional TOS Boundary"] <- "NOGP_TOS"
# site$bioclim_id[site$siteName == "Rocky Mountain National Park CASTNET Additional TOS at Roosevelt National Forest"] <- "RMNP_TOS"
# site$bioclim_id[site$siteName == "Smithsonian Environmental Research Center Additional TOS Boundary"] <- "SERC_TOS"
# site$bioclim_id[site$siteName == "Steigerwaldt Additional TOS Boundary at Chequamegon National Forest"] <- "STEI_TOS"
# site$bioclim_id[site$siteName == "North Sterling, CO Additional TOS Boundary"] <- "STER_TOS"
# site$bioclim_id[site$siteName == "Treehaven Additional TOS Boundary"] <- "TREE_TOS"

# st_write(site, paste0(out_dir,"EPSG5070/NEON_sites/NEON_field_sites_mamm.shp"))


################################################################################

# Add bioclim id to plot level data 

# # Find the intersection between plots and sites
# intersection_indices <- st_intersects(plt, site)
# 
# # Create a new bioclim_id column for each plot
# plt$bioclim_id <- sapply(seq_along(intersection_indices), function(i) {
#   # Get the intersecting site(s) for the given plot
#   intersecting_sites <- intersection_indices[[i]]
#   
#   # If there is exactly one intersecting site, return its bioclim_id
#   if (length(intersecting_sites) == 1) {
#     return(site$bioclim_id[intersecting_sites])
#   } else {
#     # Handle cases where there are no intersecting sites or multiple intersections
#     return(NA) # or any other way you prefer to handle this case
#   }
# })


################################################################################


##### PLOT SCALE (MAMMALS) ##### 

##### SITE SCALE ##### 
plt_nested <- plt %>% 
  st_make_valid() %>% 
  nest(.by = c(plotID, plotType, siteID, domainName, domainNumb)) %>% 
  mutate(circle_center = lapply(data, function(x) id_circle_center(x)), 
         max_dist_circ = lapply(data, function(x) max_dist_to_circle_center(x))) %>% 
  unnest(cols = c(max_dist_circ, circle_center)) %>% 
  mutate(plot_poly = st_buffer(circle_center, dist = 100))
# proc.time()-start

# start <- proc.time()
site_nested <- plt %>% 
  st_make_valid() %>% 
  nest(.by = c(siteID, domainName, domainNumb)) %>%
  mutate(circle_center = lapply(data, function(x) id_circle_center(x)), 
         max_dist_circ = lapply(data, function(x) max_dist_to_circle_center(x))) %>% 
  unnest(cols = c(max_dist_circ, circle_center)) %>%
  mutate(buff_dist =15000) %>% 
  # mutate(buff_dist = max(max_dist_circ)) %>% 
  mutate(site_poly = st_buffer(circle_center, dist = buff_dist))
# proc.time()-start

plt_circle_center <- plt_nested %>% 
  dplyr::select(plotID, siteID, domainName, domainNumb, circle_center) %>% 
  rename(geometry = circle_center) %>% 
  st_as_sf() # %>%
  # st_write(paste0(out_dir, "EPSG5070/plot_circle_centers.shp"), append=F)

plt_radii <- plt_nested %>% 
  dplyr::select(plotID, siteID, domainName, domainNumb, plot_poly) %>% 
  rename(geometry = plot_poly) %>% 
  st_as_sf() # %>% 
  # st_write(paste0(out_dir, "EPSG5070/plot_radii.shp"), append=F)

site_circle_center <- site_nested %>% 
  dplyr::select(siteID, siteID, domainName, domainNumb, circle_center) %>% 
  rename(geometry = circle_center) %>% 
  st_as_sf() # %>%  
  # st_write(paste0(out_dir, "EPSG5070/site_circle_centers.shp"), append=F)

site_radii <- site_nested %>% 
  dplyr::select(siteID, domainName, domainNumb, site_poly) %>% 
  rename(geometry = site_poly) %>% 
  st_as_sf() # %>%  
  # st_write(paste0(out_dir, "EPSG5070/site_radii.shp"), append=F)

dom_radii <- site_nested %>% 
  dplyr::select(siteID, domainName, domainNumb, circle_center) %>% 
  rename(geometry = circle_center) %>% # 100 km centroid around 
  st_as_sf() %>% 
  st_buffer(100000) # %>% 
  # st_write(paste0(out_dir, "EPSG5070/domain_radii.shp"), append=F)