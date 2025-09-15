library(dplyr)
library(tidyr)
library(ggplot2)

build_chain_df <- function(data_long,
                           metric,
                           plot_loc_sel,
                           chain,
                           site_ids = NULL) {
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
  if (!is.null(site_ids)) {
    base <- base %>% filter(is.na(siteID) | siteID %in% site_ids)
  }
  
  sites_tbl <- base %>%
    filter(!is.na(siteID), plot_loc == plot_loc_sel) %>%
    distinct(domainID, siteID, plot_loc)
  
  if (!is.null(site_ids) && nrow(sites_tbl) == 0L) {
    return(tibble(
      domainID = character(),
      siteID = character(),
      plot_loc = character(),
      scale_label = factor(character(), levels = x_levels),
      value = numeric()
    ))
  }
  
  # PLOT
  df_plot <- base %>%
    filter(extent == "plot", plot_loc == plot_loc_sel) %>%
    group_by(domainID, siteID, plot_loc) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    semi_join(sites_tbl, by = c("domainID","siteID","plot_loc")) %>%
    mutate(scale_label = x_levels[1])
  
  # SITE
  if (site_type == "radii") {
    df_site <- base %>%
      filter(extent == "site", extent_type == "radii", elev_res == elev_needed,
             plot_loc == plot_loc_sel) %>%
      group_by(domainID, siteID, plot_loc) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups="drop") %>%
      semi_join(sites_tbl, by = c("domainID","siteID","plot_loc")) %>%
      mutate(scale_label = x_levels[2])
  } else {
    df_site <- base %>%
      filter(extent == "site", extent_type == "footprint", elev_res == elev_needed) %>%
      group_by(siteID) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups="drop") %>%
      inner_join(sites_tbl, by = "siteID") %>%
      transmute(domainID, siteID, plot_loc, scale_label = x_levels[2], value)
  }
  
  # DOMAIN
  if (dom_type == "radii") {
    df_dom <- base %>%
      filter(extent == "domain", extent_type == "radii", elev_res == elev_needed,
             plot_loc == plot_loc_sel) %>%
      group_by(domainID, siteID, plot_loc) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups="drop") %>%
      semi_join(sites_tbl, by = c("domainID","siteID","plot_loc")) %>%
      mutate(scale_label = x_levels[3])
  } else {
    df_dom <- base %>%
      filter(extent == "domain", extent_type == "footprint", elev_res == elev_needed) %>%
      group_by(domainID) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups="drop") %>%
      semi_join(sites_tbl %>% distinct(domainID), by = "domainID") %>%
      inner_join(sites_tbl, by = "domainID") %>%
      transmute(domainID, siteID, plot_loc, scale_label = x_levels[3], value)
  }
  
  bind_rows(df_plot, df_site, df_dom) %>%
    mutate(scale_label = factor(scale_label, levels = x_levels)) %>%
    arrange(domainID, siteID, scale_label)
}

plot_chain <- function(chain_df, ylab, label_by = c("domainID","siteID")) {
  label_by <- match.arg(label_by)
  ggplot(chain_df, aes(x = scale_label, y = value,
                       group = !!sym(label_by), color = !!sym(label_by))) +
    geom_line(alpha = 0.6) +
    geom_point(size = 1.4) +
    labs(x = NULL, y = ylab, color = label_by) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}

make_three_plots <- function(data_long,
                             metric,
                             plot_loc_sel,
                             site_ids = NULL,
                             label_by = c("domainID","siteID")) {
  label_by <- match.arg(label_by)
  list(
    radius30     = plot_chain(build_chain_df(data_long, metric, plot_loc_sel, "radius30", site_ids),     ylab=metric, label_by = label_by),
    radius300    = plot_chain(build_chain_df(data_long, metric, plot_loc_sel, "radius300", site_ids),    ylab=metric, label_by = label_by),
    footprint300 = plot_chain(build_chain_df(data_long, metric, plot_loc_sel, "footprint300", site_ids), ylab=metric, label_by = label_by)
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
  neon_df_long, metric = "srtm_sq", plot_loc_sel = "tower",
  site_ids = c("RMNP","DSNY"), label_by = "siteID"
)

tower_plots$radius30
tower_plots$radius300
tower_plots$footprint300


plot_raster(raster_to_df(terra::rast(paste0("/mnt/research/neon/neonEnvData/L1/elev_tif_30m/", ls[1]))), title = "KONA")

