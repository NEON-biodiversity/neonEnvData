# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(terra)
library(ggpubr)
library(ggcorrplot)

# Aggregate srtm_tile to 300m resolution (instead of orig 30m) 
# for(i in 1:length(srtm_tile_files)){
#   print(i)
#   t <- terra::rast(srtm_tile_files[[i]])
#   u <- terra::aggregate(t, fact = 10)
#   terra::writeRaster(u, paste0("/mnt/scratch/kapsarke/neonEnvData/L1/elev_EPSG5070_300m/", basename(srtm_tile_files[[i]])))
# }

# Compare domain_radii metrics between 30m and 300m resolutions
d30 <- st_read("/mnt/scratch/kapsarke/neonEnvData/L2/clim_elev/NEON_domain_radii.shp")
d300 <- st_read("/mnt/scratch/kapsarke/neonEnvData/L2/clim_elev_300m/NEON_domain_radii.shp")


# 1. Select only siteID and srtm_ columns, and remove all-NA columns
geo_vars_30 <- d30 %>%
  select(siteID, starts_with("srtm_")) %>%
  select(where(~ !all(is.na(.)))) %>% 
  st_drop_geometry()

geo_vars_300 <- d300 %>%
  select(siteID, starts_with("srtm_")) %>%
  select(where(~ !all(is.na(.)))) %>% 
  st_drop_geometry()

# 2. Make sure we only keep common metrics between 30m and 300m datasets
common_metrics <- intersect(
  str_remove(names(geo_vars_30)[-1], "srtm_"),
  str_remove(names(geo_vars_300)[-1], "srtm_")
)

geo_vars_30 <- geo_vars_30 %>%
  select(siteID, all_of(paste0("srtm_", common_metrics)))

geo_vars_300 <- geo_vars_300 %>%
  select(siteID, all_of(paste0("srtm_", common_metrics)))

# 3. Rename columns to reflect resolution
names(geo_vars_30)[-1] <- paste0(names(geo_vars_30)[-1], "_30m")
names(geo_vars_300)[-1] <- paste0(names(geo_vars_300)[-1], "_300m")

# 4. Join dataframes by siteID
geo_combined <- left_join(geo_vars_30, geo_vars_300, by = "siteID")

# 5. Reshape to long format safely
geo_long <- geo_combined %>%
  pivot_longer(
    cols = -siteID,
    names_to = "metric_res",
    values_to = "value"
  ) %>%
  separate(metric_res, into = c("metric", "resolution"), sep = "_(?=30m|300m$)", remove = FALSE) %>%
  dplyr::select(-metric_res) %>% 
  pivot_wider(names_from = resolution, values_from = value)

# 6. Plot: 30m vs 300m for each geodiversity metric
ggplot(geo_long, aes(x = `30m`, y = `300m`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_cor(method = "pearson",
           aes(label = paste0("r = ", ..r..)),
           label.x.npc = "left", label.y.npc = "top",
           size = 5) +  # ≈ 12 pt
  facet_wrap(~metric, scales = "free") +
  theme_minimal(base_size = 12) +  # This sets base font size to ~12 pt
  theme(
    strip.text = element_text(size = 18),       # facet labels
    axis.title = element_text(size = 18),       # axis titles
    axis.text = element_text(size = 18),        # axis tick labels
    plot.title = element_text(size = 18, face = "bold")  # plot title
  ) +
  labs(
    title = "30m vs 300m Geodiversity Metrics by Site",
    x = "30m resolution",
    y = "300m resolution"
  )

# 7. Correlation per metric
cor_results <- geo_long %>%
  group_by(metric) %>%
  summarize(correlation = cor(`30m`, `300m`, use = "complete.obs"))

print(cor_results)
