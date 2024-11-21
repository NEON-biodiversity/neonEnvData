# Load necessary libraries
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)

source("./R/config.R")

# Load data into memory
site <- st_read(paste0(data_dir, "NEON_spatial/EPSG5070/NEON_sites.shp"), quiet = TRUE)
plt <- st_read(paste0(data_dir, "NEON_spatial/EPSG5070/NEON_small_mammal_plots.shp"), quiet=TRUE)
plt_radii <- st_read(paste0(data_dir, "NEON_spatial/EPSG5070/plot_radii.shp"), quiet = TRUE)
plt_circle_center <- st_read(paste0(data_dir, "NEON_spatial/EPSG5070/plot_circle_centers.shp"), quiet = TRUE)
site_radii <- st_read(paste0(data_dir, "NEON_spatial/EPSG5070/site_radii.shp"), quiet = TRUE)
site_circle_center <- st_read(paste0(data_dir, "NEON_spatial/EPSG5070/site_circle_centers.shp"), quiet = TRUE)
dom_radii <- st_read(paste0(data_dir, "NEON_spatial/EPSG5070/domain_radii.shp"), quiet = TRUE)
dom <- st_read(paste0(data_dir, "NEON_spatial/EPSG5070/NEON_domains.shp"), quiet = TRUE)

# Refactor function
generate_scale_plots <- function(site_code, site, plt, plt_radii, plt_circle_center, site_radii, site_circle_center, dom_radii, dom) {
  # Subset data based on the input site_code
  site_data <- site[site$siteID == site_code,]
  traps_data <- plt[grep(site_code, plt$plotID),]
  plot_radii_data <- plt_radii[plt_radii$siteID == site_code,]
  focal_plot_data <- plt_circle_center[plt_circle_center$plotID == traps_data$plotID[1],]
  plots_data <- plt_circle_center[plt_circle_center$siteID == site_code,]
  
  radii_data <- site_radii[site_radii$siteID == site_code,]
  circle_center_data <- site_circle_center[site_radii$siteID == site_code,]
  dom_radii_data <- dom_radii[dom_radii$siteID == site_code,]
  dom_data <- dom[dom$domainNumb == site_data$domainNumb,]
  
  # First plot
  fig_plot <- ggplot() +
    geom_sf(data = plot_radii_data[plot_radii_data$plotID == traps_data$plotID[1],], fill = "#b8e6a5") +
    geom_sf(data = traps_data[traps_data$plotID == traps_data$plotID[1], ], color = "black", size = 3) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.title = element_blank()
    )
  
  # ggsave(filename = paste0("./figures/", site_code, "_plot.png"), plot = fig_plot, width = 8, height = 6, dpi = 300)
  
  # Second plot
  fig_site_footprint <- ggplot() +
    geom_sf(data = site_data, fill = "#44aea3") +
    geom_sf(data = plots_data, shape = 21, fill = "#b8e6a5", color = "black", size = 7) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.title = element_blank()
    )
  
  # ggsave(filename = paste0("./figures/", site_code, "_site_footprint.png"), plot = fig_site_footprint, width = 8, height = 6, dpi = 300)
  
  # Third plot
  fig_site_radii <- ggplot() +
    geom_sf(data = radii_data, fill = "#44aea3") +
    geom_sf(data = site_data, fill = NA, color = "white", lwd=1) +
    geom_sf(data = plots_data, shape = 21, fill = "#b8e6a5", color = "black", size = 3) +
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
