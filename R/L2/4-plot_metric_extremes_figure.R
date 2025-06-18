
library(sf)
library(ggplot2)
library(terra)
library(viridis)
library(ggplotify)  # For converting terra plots to ggplot-style

list.files("/mnt/research/neon/neonEnvData/L2/clim_elev_30m/")
df <- st_read("/mnt/research/neon/neonEnvData/L2/clim_elev_30m/NEON_tower_site_radii.shp")


df$siteID[which.max(df$srtm_sq)] #TEAK - 560
df$siteID[which.min(df$srtm_sq)] #JERC - 5

max_sq <- rast("/mnt/research/neon/neonEnvData/L1/elev_tif_30m/NEON_tower_site_radii_TEAK_30.tif")
min_sq <- rast("/mnt/research/neon/neonEnvData/L1/elev_tif_30m/NEON_tower_site_radii_JERC_30.tif")

df$siteID[which.max(df$srtm_std2)] #ORNL - 0.0547
df$siteID[which.min(df$srtm_std2)] #PUUM - 0.0121

max_std2 <- rast("/mnt/research/neon/neonEnvData/L1/elev_tif_30m/NEON_tower_site_radii_ORNL_30.tif")
min_std2 <- rast("/mnt/research/neon/neonEnvData/L1/elev_tif_30m/NEON_tower_site_radii_PUUM_30.tif")


df$siteID[which.max(df$srtm_sku)] #JORN - 9.568
df$siteID[which.min(df$srtm_sku)] #SERC - -1.378

max_sku <- rast("/mnt/research/neon/neonEnvData/L1/elev_tif_300m/NEON_tower_site_radii_JORN_300.tif")
min_sku <- rast("/mnt/research/neon/neonEnvData/L1/elev_tif_300m/NEON_tower_site_radii_SERC_300.tif")



########################### VIOLIN PLOT ########################### 
# Reshape the data to long format
df_long <- df %>%
  st_drop_geometry() %>% 
  select(siteID, srtm_std2) %>%
  pivot_longer(cols = -siteID, names_to = "variable", values_to = "value")

# Remove NA values
df_long <- df_long %>% filter(!is.na(value))

# Create plot
p <- ggplot(df_long, aes(x = value, y = 1)) +
  geom_violin(aes(group = 1), fill = "lightgray", color = NA, alpha = 0.5, width = 1) +
  geom_jitter(height = 0, alpha = 0.6, size = 2) +
  theme_minimal(base_size = 12) +  # 12 pt font equivalent
  labs(x = "Elevation - surface texture direction index (std2)", y = NULL, title = NULL) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 12),  # Optional: title size
    axis.text.x = element_text(size = 12)  # Make x-axis text larger
  )

# Save the figure
ggsave(
  filename = "/mnt/research/neon/neonEnvData/L2/figures/tower_site_radii_std2_30m.png",
  plot = p,
  width = 4,
  height = 2,
  dpi = 300,
  units = "in"
)

########################### RASTER PLOT ########################### 

# Function to convert terra raster to data frame
raster_to_df <- function(r) {
  as.data.frame(r, xy = TRUE, na.rm = TRUE) %>%
    rename(value = 3)  # assumes only one layer
}

# Convert rasters to data frames
max_df <- raster_to_df(max_std2)
min_df <- raster_to_df(min_std2)

# Plotting function
plot_raster <- function(df, title, legend_position = "right", main = NULL) {
  ggplot(df, aes(x = x, y = y, fill = value)) +
    geom_raster() +
    scale_fill_viridis(name = title, option = "D", na.value = NA) +    
    coord_fixed() + 
    theme_void() +
    labs(title = main) +
    theme(
      legend.position = legend_position,
      legend.title = element_text(size = 14),  # Increase legend title size
      legend.text = element_text(size = 12),   # Increase legend label size
      plot.title = element_text(size = 16, hjust = 0.5),  # Larger & centered
      panel.border = element_blank()
    )
}

# Generate plots
plot_max <- plot_raster(max_df, "Elevation", main = "ORNL")
plot_min <- plot_raster(min_df, "Elevation", legend_position = "left", main = "PUUM")

# Optionally display them
print(plot_max)
print(plot_min)



ggsave("/mnt/research/neon/neonEnvData/L2/figures/max_std2_map.png", plot_max, width = 6, height = 4, dpi = 300)
ggsave("/mnt/research/neon/neonEnvData/L2/figures/min_std2_map.png", plot_min, width = 6, height = 4, dpi = 300)


ggsave("./R/L2/max_std2_map.png", plot_max, width = 6, height = 4, dpi = 300)
ggsave("./R/L2/min_std2_map.png", plot_min, width = 6, height = 4, dpi = 300)
