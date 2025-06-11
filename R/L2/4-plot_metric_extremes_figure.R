


list.files("/mnt/research/neon/neonEnvData/L2/clim_elev_300m/")
df <- st_read("/mnt/research/neon/neonEnvData/L2/clim_elev_300m/NEON_tower_site_radii.shp")


df$siteID[which.max(df$srtm_sq)] #TEAK - 560
df$siteID[which.min(df$srtm_sq)] #JERC - 5

max_sq <- rast("/mnt/research/neon/neonEnvData/L1/elev_tif_300m/NEON_tower_site_radii_TEAK_300.tif")
min_sq <- rast("/mnt/research/neon/neonEnvData/L1/elev_tif_300m/NEON_tower_site_radii_JERC_300.tif")

df$siteID[which.max(df$srtm_std2)] #ORNL - 0.0547
df$siteID[which.min(df$srtm_std2)] #PUUM - 0.0121

max_std2 <- rast("/mnt/research/neon/neonEnvData/L1/elev_tif_300m/NEON_tower_site_radii_ORNL_300.tif")
min_std2 <- rast("/mnt/research/neon/neonEnvData/L1/elev_tif_300m/NEON_tower_site_radii_PUUM_300.tif")


df$siteID[which.max(df$srtm_sku)] #JORN - 9.568
df$siteID[which.min(df$srtm_sku)] #SERC - -1.378

max_sku <- rast("/mnt/research/neon/neonEnvData/L1/elev_tif_300m/NEON_tower_site_radii_JORN_300.tif")
min_sku <- rast("/mnt/research/neon/neonEnvData/L1/elev_tif_300m/NEON_tower_site_radii_SERC_300.tif")
