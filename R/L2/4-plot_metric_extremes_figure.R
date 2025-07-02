library(sf)
library(ggplot2)
library(terra)
library(viridis)
library(dplyr)
library(tidyr)
library(stringr)

# Helper: convert raster to df
raster_to_df <- function(r) {
  as.data.frame(r, xy = TRUE, na.rm = TRUE) %>%
    rename(value = 3)
}

# Helper: plot raster
plot_raster <- function(df, title, legend_position = "right", main = NULL) {
  ggplot(df, aes(x = x, y = y, fill = value)) +
    geom_raster() +
    scale_fill_viridis(name = title, option = "D", na.value = NA) +    
    coord_fixed() + 
    theme_void() +
    # labs(title = main) +
    theme(
      legend.position = legend_position,
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 16, hjust = 0.5),
      panel.border = element_blank()
    )
}

# ✅ Main workflow function
process_neon_variable <- function(variable, shapefile_path, output_dir) {
  
  # Read shapefile
  df <- st_read(shapefile_path, quiet = TRUE)
  
  # Extract resolution from path (e.g., 30 from "clim_elev_30m")
  resolution <- str_extract(shapefile_path, "\\d+(?=m)")
  
  # Extract base filename (e.g., "NEON_tower_site_radii")
  base_name <- tools::file_path_sans_ext(basename(shapefile_path))
  
  # Create long data for plotting
  df_long <- df %>%
    st_drop_geometry() %>%
    select(siteID, all_of(variable)) %>%
    pivot_longer(cols = -siteID, names_to = "variable", values_to = "value") %>%
    filter(!is.na(value))
  
  # Violin + jitter plot
  p <- ggplot(df_long, aes(x = value, y = 1)) +
    geom_violin(aes(group = 1), fill = "lightgray", color = NA, alpha = 0.5, width = 1) +
    geom_jitter(height = 0, alpha = 0.6, size = 2) +
    theme_minimal(base_size = 12) +
    labs(x = NULL, y = NULL, title = NULL) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 12)
    )
  
  # Construct violin plot filename
  violin_file <- file.path(output_dir, paste0(base_name, "_", resolution, "m_", variable, ".png"))
  ggsave(filename = violin_file, plot = p, width = 3.5, height = 2, dpi = 300, units = "in")
  
  # Identify min and max siteIDs
  max_site <- df$siteID[which.max(df[[variable]])]
  min_site <- df$siteID[which.min(df[[variable]])]
  
  # Construct raster paths
  if(grepl("bio", variable)){
    raster_dir <- str_replace(shapefile_path, "L2/clim_elev_\\d+m/.*", paste0("L1/climate_tif"))
    max_rast_path <- file.path(raster_dir, paste0(base_name, "_", max_site, "_", substr(variable, 1, 5), ".tif"))
    min_rast_path <- file.path(raster_dir, paste0(base_name, "_", min_site, "_", substr(variable, 1, 5), ".tif"))
  }
  else{
    raster_dir <- str_replace(shapefile_path, "L2/clim_elev_\\d+m/.*", paste0("L1/elev_tif_", resolution, "m"))
    max_rast_path <- file.path(raster_dir, paste0(base_name, "_", max_site, "_", resolution, ".tif"))
    min_rast_path <- file.path(raster_dir, paste0(base_name, "_", min_site, "_", resolution, ".tif"))
  }
  
  # Read rasters
  max_rast <- rast(max_rast_path)
  min_rast <- rast(min_rast_path)
  
  # Convert to df and plot
  ttl <- ifelse(substr(variable, 1, 5) == "bio01", "Temperature", 
                ifelse(substr(variable, 1, 5) == "bio12", "Precipitation", 
                       "Elevation"))
  
  plot_max <- plot_raster(raster_to_df(max_rast), ttl, main = max_site)
  plot_min <- plot_raster(raster_to_df(min_rast), ttl, legend_position = "left", main = min_site)
  
  # Save raster plots
  ggsave(file.path(output_dir, paste0(base_name, "_", resolution, "m_", variable, "_", max_site, "_max_map.png")), 
         plot_max, width = 2.75, height = 2, dpi = 300)
  ggsave(file.path(output_dir, paste0(base_name, "_", resolution, "m_", variable, "_", min_site, "_min_map.png")), 
         plot_min, width = 2.75, height = 2, dpi = 300)
}




process_neon_variable(
  variable = "bio12_sq",
  shapefile_path = "/mnt/research/neon/neonEnvData/L2/clim_elev_30m/NEON_tower_site_radii.shp",
  output_dir = "/mnt/home/kapsarke/Documents/neonEnvData/R/L2"
)

