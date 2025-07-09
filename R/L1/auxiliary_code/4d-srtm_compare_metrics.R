
d30 <- st_read("/mnt/scratch/kapsarke/neonEnvData/L2/clim_elev/NEON_domain_radii.shp")
d300 <- st_read("/mnt/scratch/kapsarke/neonEnvData/L2/clim_elev_300m/NEON_domain_radii.shp")

desired_order <- c(
  "srtm_mean", "srtm_sd", "srtm_sq", "srtm_sbi",
  "srtm_ssk", "srtm_sku", "srtm_sdq", "srtm_sds"
)

# 1. Extract 30m and 300m geodiversity metric columns
metrics_30m <- d30 %>%
  select(any_of(desired_order)) %>%
  select(where(~ !all(is.na(.)))) %>%   # Remove all-NA columns
  st_drop_geometry()
  
metrics_300m <- d300 %>%
  select(any_of(desired_order)) %>%
  select(where(~ !all(is.na(.)))) %>% 
  st_drop_geometry()

# 2. Compute correlation matrices
cor_30m <- cor(metrics_30m, use = "pairwise.complete.obs")
cor_300m <- cor(metrics_300m, use = "pairwise.complete.obs")

# 3. Compute p-values for significance (optional)
cor_pvals <- function(df) {
  mat <- as.matrix(df)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  for (i in 1:n) {
    for (j in 1:n) {
      test <- cor.test(mat[, i], mat[, j], use = "pairwise.complete.obs")
      p.mat[i, j] <- test$p.value
    }
  }
  return(p.mat)
}

pval_30m <- cor_pvals(metrics_30m)
pval_300m <- cor_pvals(metrics_300m)

# 4. Plot correlation matrices
ggcorrplot(cor_30m, 
           hc.order = FALSE,
           type = "lower", 
           lab = TRUE, 
           lab_size = 5,  # Larger numbers in cells
           p.mat = pval_30m, 
           sig.level = 0.05,
           title = "Pairwise Correlations (30m Geodiversity Metrics)\nDomain Radii",
           ggtheme = theme_minimal(base_size = 18) +
             theme(
               axis.text.x = element_text(size = 18, face = "bold", angle = 45, hjust = 1),
               axis.text.y = element_text(size = 18, face = "bold")
             ))

ggsave("/mnt/scratch/kapsarke/neonEnvData/L2/figures/correlation_plot_domain_radii_30m.png",
       width = 10, height = 8, dpi = 600,  # High-res and good screen size
       units = "in")

ggcorrplot(cor_300m, 
           hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           p.mat = pval_300m, 
           sig.level = 0.05,
           title = "Pairwise Correlations (300m Geodiversity Metrics)",
           ggtheme = theme_minimal(base_size = 12))