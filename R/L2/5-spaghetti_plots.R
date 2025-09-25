library(dplyr)
library(tidyr)
library(ggplot2)
library(rlang)

# ---------- data builder: returns ALL sites for the given plot_loc/metric ----------
# chain ∈ {"radius30","radius300","footprint300"}
build_chain_df <- function(data_long,
                           metric,
                           plot_loc_sel,
                           chain) {
  chain <- match.arg(chain, c("radius30","radius300","footprint300"))
  
  if (chain == "radius30") {
    site_type <- "radii"; dom_type <- "radii"; elev_needed <- 30
    x_levels  <- c("plot", "site radius 30m", "domain radius 30m")
  } else if (chain == "radius300") {
    site_type <- "radii"; dom_type <- "radii"; elev_needed <- 300
    x_levels  <- c("plot", "site radius 300m", "domain radius 300m")
  } else { # footprint300
    site_type <- "footprint"; dom_type <- "footprint"; elev_needed <- 300
    x_levels  <- c("plot", "site footprint 300m", "domain footprint 300m")
  }
  
  base <- data_long %>% filter(metric == !!metric)
  
  # sites to draw = all sites present at this plot_loc (used for broadcasting)
  sites_tbl <- base %>%
    filter(!is.na(siteID), plot_loc == plot_loc_sel) %>%
    distinct(domainID, siteID, plot_loc)
  
  # 1) PLOT
  df_plot <- base %>%
    filter(extent == "plot", plot_loc == plot_loc_sel) %>%
    group_by(domainID, siteID, plot_loc) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    semi_join(sites_tbl, by = c("domainID","siteID","plot_loc")) %>%
    mutate(scale_label = x_levels[1])
  
  # 2) SITE
  if (site_type == "radii") {
    df_site <- base %>%
      filter(extent == "site",
             extent_type == "radii",
             elev_res == elev_needed,
             plot_loc == plot_loc_sel) %>%
      group_by(domainID, siteID, plot_loc) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups="drop") %>%
      semi_join(sites_tbl, by = c("domainID","siteID","plot_loc")) %>%
      mutate(scale_label = x_levels[2])
  } else {
    # site footprint has siteID: compute and attach plot_loc/domain from sites_tbl
    df_site <- base %>%
      filter(extent == "site",
             extent_type == "footprint",
             elev_res == elev_needed) %>%
      group_by(siteID) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups="drop") %>%
      inner_join(sites_tbl, by = "siteID") %>%
      transmute(domainID, siteID, plot_loc,
                scale_label = x_levels[2], value)
  }
  
  # 3) DOMAIN
  if (dom_type == "radii") {
    df_dom <- base %>%
      filter(extent == "domain",
             extent_type == "radii",
             elev_res == elev_needed,
             plot_loc == plot_loc_sel) %>%
      group_by(domainID, siteID, plot_loc) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups="drop") %>%
      semi_join(sites_tbl, by = c("domainID","siteID","plot_loc")) %>%
      mutate(scale_label = x_levels[3])
  } else {
    # domain footprint: per-domain value, broadcast to ALL sites in those domains at this plot_loc
    dom_fp <- base %>%
      filter(extent == "domain",
             extent_type == "footprint",
             elev_res == elev_needed) %>%
      group_by(domainID) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups="drop")
    
    df_dom <- dom_fp %>%
      inner_join(sites_tbl %>% distinct(domainID, siteID, plot_loc), by = "domainID") %>%
      transmute(domainID, siteID, plot_loc,
                scale_label = x_levels[3], value)
  }
  
  bind_rows(df_plot, df_site, df_dom) %>%
    mutate(scale_label = factor(scale_label, levels = x_levels)) %>%
    arrange(domainID, siteID, scale_label)
}

# ---------- plotter: optional highlight of a subset ----------
# color/legend can be by "domainID" or "siteID"; lines always grouped by siteID
plot_chain <- function(chain_df, ylab,
                       label_by = c("domainID","siteID"),
                       site_ids = NULL) {
  label_by <- match.arg(label_by)
  group_var <- "siteID"
  
  if (is.null(site_ids) || length(site_ids) == 0L) {
    # Normal mode: color & legend for all
    ggplot(chain_df, aes(x = scale_label, y = value,
                         group = !!sym(group_var),
                         color = !!sym(label_by))) +
      geom_line(alpha = 0.6) +
      geom_point(size = 1.4) +
      labs(x = NULL, y = ylab, color = label_by) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom",
            panel.grid.minor.x = element_blank())
  } else {
    sel <- chain_df %>% filter(siteID %in% site_ids)
    bkg <- chain_df %>% filter(!siteID %in% site_ids)
    
    ggplot() +
      # background: all non-selected sites in gray, no legend
      geom_line(data = bkg,
                aes(x = scale_label, y = value, group = !!sym(group_var)),
                alpha = 0.35, color = "grey70") +
      geom_point(data = bkg,
                 aes(x = scale_label, y = value),
                 size = 1.2, alpha = 0.35, color = "grey70") +
      # foreground: selected sites, colored & in legend
      geom_line(data = sel,
                aes(x = scale_label, y = value,
                    group = !!sym(group_var),
                    color = !!sym(label_by)),
                alpha = 0.85) +
      geom_point(data = sel,
                 aes(x = scale_label, y = value,
                     color = !!sym(label_by)),
                 size = 1.6) +
      labs(x = NULL, y = ylab, color = label_by) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom",
            panel.grid.minor.x = element_blank())
  }
}

make_three_plots <- function(data_long,
                             metric,
                             plot_loc_sel,
                             site_ids = NULL,
                             label_by = c("domainID","siteID")) {
  label_by <- match.arg(label_by)
  
  # Build data once for each chain
  df_radius30     <- build_chain_df(data_long, metric, plot_loc_sel, "radius30")
  df_radius300    <- build_chain_df(data_long, metric, plot_loc_sel, "radius300")
  df_footprint300 <- build_chain_df(data_long, metric, plot_loc_sel, "footprint300")
  
  list(
    radius30 = list(
      data = df_radius30,
      plot = plot_chain(df_radius30, ylab = metric, label_by = label_by, site_ids = site_ids)
    ),
    radius300 = list(
      data = df_radius300,
      plot = plot_chain(df_radius300, ylab = metric, label_by = label_by, site_ids = site_ids)
    ),
    footprint300 = list(
      data = df_footprint300,
      plot = plot_chain(df_footprint300, ylab = metric, label_by = label_by, site_ids = site_ids)
    )
  )
}




# assuming `neon_df_long` already exists
# and contains plot_loc, extent, extent_type, elev_res, metric, value, siteID, domainID

# NEED TO FIX LACK OF SITE FOOTPRINT AND DOMAIN RADII HAVING SAME VALUE AS SITE RADII

# TOWERS
# tower_plots <- make_three_plots(neon_df_long, metric = "srtm_sdq", plot_loc_sel = "tower")
# tower_30m <- build_chain_df(neon_df_long, metric = "srtm_ssk", plot_loc_sel = "tower", chain="radius30")
# 
# tower_plots$radius30
# tower_plots$radius300
# tower_plots$footprint300
# 
# # MAMMALS
# mammal_plots <- make_three_plots(neon_df_long, metric = "srtm_sq", plot_loc_sel = "mammal")
# mammal_plots$radius30
# mammal_plots$radius300
# mammal_plots$footprint300


# TOWERS
tower_plots <- make_three_plots(
  neon_df_long, metric = "srtm_ssk", plot_loc_sel = "mammal",
  site_ids = c("TEAK","DSNY"), label_by = "siteID"
)

tower_plots$radius30$plot
tower_plots$radius300$plot
tower_plots$footprint300$plot


mammal_plots <- make_three_plots(
  neon_df_long, metric = "srtm_sq", plot_loc_sel = "mammal",
  label_by = "domainID"
)

mammal_plots$radius30$plot
mammal_plots$radius300$plot
mammal_plots$footprint300$plot

mammal_plots <- make_three_plots(
  neon_df_long, metric = "srtm_sq", plot_loc_sel = "mammal",
  site_ids = c("TEAK","DSNY"), label_by = "siteID"
)

mammal_plots$radius30$plot
mammal_plots$radius300$plot
mammal_plots$footprint300$plot



mammal_plots <- make_three_plots(
  neon_df_long, metric = "srtm_mean", plot_loc_sel = "mammal",
  site_ids = c("TEAK","DSNY"), label_by = "siteID"
)

mammal_plots$radius30$plot
mammal_plots$radius300$plot
mammal_plots$footprint300$plot


mammal_plots <- make_three_plots(
  neon_df_long, metric = "srtm_sq", plot_loc_sel = "mammal",
  site_ids = c("TEAK","DSNY"), label_by = "siteID"
)

mammal_plots$radius30$plot
mammal_plots$radius300$plot
mammal_plots$footprint300$plot


mammal_plots <- make_three_plots(
  neon_df_long, metric = "srtm_sds", plot_loc_sel = "mammal",
  site_ids = c("TEAK","DSNY"), label_by = "siteID"
)

mammal_plots$radius30$plot
mammal_plots$radius300$plot
mammal_plots$footprint300$plot



t <- plot_raster(raster_to_df(terra::rast("/mnt/research/neon/neonEnvData/L1/elev_tif_30m/NEON_mammal_domain_radii_TEAK_30.tif")), title = "TEAK")

polygon_dir <- "/mnt/research/neon/neonEnvData/L1/neon_EPSG5070"
plt_radii <- st_read(paste0(polygon_dir, "/NEON_mammal_plot_radii.gpkg"), quiet = TRUE) %>% filter(siteID == "TEAK") %>% st_write("teak_plt.gpkg")
site_radii <- st_read(paste0(polygon_dir, "/NEON_mammal_site_radii.gpkg"), quiet = TRUE) %>% filter(siteID == "TEAK")%>% st_write("teak_site.gpkg")

u <- t + geom_sf(data = site_radii, fill = NA, color = "white",
        linewidth = 1, inherit.aes = FALSE)

