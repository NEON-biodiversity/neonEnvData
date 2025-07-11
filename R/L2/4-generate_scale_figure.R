# Load necessary libraries
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)

source("./R/config.R")

# Load data into memory
site <- st_read(paste0(neon_dir, "/NEON_site_footprint.shp"), quiet = TRUE)
plt <- st_read(paste0(neon_dir, "/NEON_mammal_plot_footprint.shp"), quiet=TRUE)
plt_radii <- st_read(paste0(neon_dir, "/NEON_mammal_plot_radii.shp"), quiet = TRUE)
site_radii <- st_read(paste0(neon_dir, "/NEON_mammal_site_radii.shp"), quiet = TRUE)
dom_radii <- st_read(paste0(neon_dir, "/NEON_mammal_domain_radii.shp"), quiet = TRUE)
dom <- st_read(paste0(neon_dir, "/NEON_domain_footprint.shp"), quiet = TRUE)


# Function to make a square centered on a point
make_centered_square <- function(center, size) {
  half_size <- size / 2
  # Create square coordinates centered at (0,0)
  square_coords <- matrix(c(
    -half_size, -half_size,
    half_size, -half_size,
    half_size,  half_size,
    -half_size,  half_size,
    -half_size, -half_size
  ), ncol = 2, byrow = TRUE)
  
  # Create polygon and shift it to center
  square <- st_polygon(list(square_coords))
  square_sfc <- st_sfc(square, crs = st_crs(center))
  
  # Translate by adding coordinates
  center_coords <- st_coordinates(center)
  square_translated <- square_sfc + center_coords
  st_sf(geometry = square_translated, crs = st_crs(center))
}


# Refactor function
generate_scale_plots <- function(site_code, site, plt, plt_radii, site_radii, dom_radii, dom) {
  # Subset data based on the input site_code
  site_data <- site[site$siteID == site_code,]
  traps_data <- plt[grep(site_code, plt$plotID),]
  plot_radii_data <- plt_radii[plt_radii$siteID == site_code,]
  
  radii_data <- site_radii[site_radii$siteID == site_code,]
  dom_radii_data <- dom_radii[dom_radii$siteID == site_code,]
  dom_data <- dom[dom$domainNumb == site_data$domainNumb,]
  
  
  
  ######################## PLOT SCALE FIGURE   ######################## 
  
  # Create grid for background
  # Pick a reference point (e.g., center of site)
  center_point <- st_centroid(st_union(plot_radii_data[plot_radii_data$plotID == traps_data$plotID[1],]))
  
  # Create a single 30m and 300m grid cell at the center
  grid_300_plot <- make_centered_square(center_point, 300)
  grid_30_plot <- st_make_grid(grid_300_plot, cellsize=30, square=T)
  

  fig_plot <- ggplot() +
    geom_sf(data = grid_300_plot, fill = "NA", color="black", lwd = 1) +
    geom_sf(data = plot_radii_data[plot_radii_data$plotID == traps_data$plotID[1],], fill = "#b8e6a5") +
    geom_sf(data = grid_30_plot, fill="NA", color = "black", size = 0.3) +
    # geom_sf(data = traps_data[traps_data$plotID == traps_data$plotID[1], ], color = "black", size = 3) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.title = element_blank()
    )
  
  ggsave(filename = paste0(figures,"/scale_figure/", site_code, "_plot.png"), plot = fig_plot, width = 8, height = 6, dpi = 300)
  
  ######################## SITE FOOTPRINT SCALE FIGURE   ######################## 
  
  # # Create grid for background
  site_bounds <- st_bbox(radii_data)
  grid <- st_make_grid(
    st_as_sfc(site_bounds),
    cellsize = 300,
    square = TRUE
  )
  grid_sf <- st_sf(geometry = grid)
  grid_site_foot <- st_intersection(grid_sf, site_data)
  grid_site_rad <- st_intersection(grid_sf, radii_data)
  
  # Second plot
  fig_site_footprint <- ggplot() +
    geom_sf(data = site_data, fill = "#44aea3") +
    geom_sf(data = grid_site_foot, fill = "NA", color = "black", size = 0.3) +
    # geom_sf(data = grid_300_plot, fill = "#f7a680") +
    # geom_sf(data = plot_radii_data, shape = 21, fill = "#b8e6a5",size = 6) +
    geom_sf(data = st_centroid(plot_radii_data), color = "#b8e6a5",size = 6) +
    # geom_sf(data = grid_300_sf, fill = NA, color = "blue", size = 0.8, linetype = "solid") +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.title = element_blank()
    )
  
  ggsave(filename = paste0(figures,"/scale_figure/", site_code, "_site_footprint.png"), plot = fig_site_footprint, width = 8, height = 6, dpi = 300)
  
  ######################## SITE RADIUS SCALE FIGURE   ######################## 
  # Third plot
  fig_site_radii <- ggplot() +
    geom_sf(data = radii_data, fill = "#44aea3") +
    geom_sf(data = grid_site_rad, fill = "NA", color = "black", size = 0.3) +
    geom_sf(data = site_data, fill = NA, color = "white", lwd=2) +
    geom_sf(data = st_centroid(plot_radii_data), color = "#b8e6a5",size = 6) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.title = element_blank()
    )

  ggsave(filename = paste0("./figures/", site_code, "_site_radius.png"), plot = fig_site_radii, width = 8, height = 6, dpi = 300)
  
  ######################## DOMAIN RADIUS SCALE FIGURE   ######################## 
  
  # Fourth plot (dom_radii)
  buffer_extent <- st_buffer(dom_radii_data, dist = 100000)
  
  fig_dom_radii <- ggplot() +
    geom_sf(data = dom_data, color = "black", alpha = 0.5) +
    geom_sf(data = dom_radii_data, fill = "#05718b") +
    geom_sf(data = radii_data, fill = "#44aea3", color = "black", size = 0.7) +
    coord_sf(xlim = st_bbox(buffer_extent)[c("xmin", "xmax")],
             ylim = st_bbox(buffer_extent)[c("ymin", "ymax")]) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.title = element_blank()
    )
  
  # ggsave(filename = paste0("./figures/", site_code, "_dom_radius.png"), plot = fig_dom_radii, width = 8, height = 6, dpi = 300)
  
  ######################## DOMAIN FOOTPRINT SCALE FIGURE   ######################## 
  
  # Fifth plot (dom_footprint)
  fig_dom_footprint <- ggplot() +
    geom_sf(data = dom_data, fill = "#05718b") +
    geom_sf(data = site_data, fill = "#44aea3", color = "#44aea3", size = 0.7, alpha = 0.5) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.title = element_blank()
    )
  
  # ggsave(filename = paste0("./figures/", site_code, "_dom_footprint.png"), plot = fig_dom_footprint, width = 8, height = 6, dpi = 300)

  text_plot <- ggplot() + 
    theme_void() +  # A blank theme
    geom_text(aes(x = 0.5, y = 0.5, label = site_code), size = 10)
  
  # Now combine the plots
  # Arrange the plots in a grid layout
  combined_plot <- plot_grid(
    plot_grid(text_plot, fig_plot, ncol = 2, rel_widths = c(1, 1)),  # First row
    plot_grid(fig_site_footprint, fig_site_radii, ncol = 2),  # Second row
    plot_grid(fig_dom_footprint, fig_dom_radii, ncol = 2),  # Third row
    nrow = 3,
    align = 'v'
  )
  
  # ggsave(filename = paste0("./figures/", site_code, "_combined_plot.png"), plot = combined_plot, width = 6, height = 9, dpi = 300)

    
}

# Example usage:
lapply(unique(site$siteID), function(x){
  generate_scale_plots(x, site, plt, plt_radii, plt_circle_center, site_radii, site_circle_center, dom_radii, dom)
})
