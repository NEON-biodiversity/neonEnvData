# =============================================================================
# TITLE:            Data Paper Figure 1b Code
# PROJECT:          NEON Geodiversity Analysis
# AUTHORS:          Kelly Kapsar, Pat Bills, Phoebe Zarnetske 
# COLLABORATORS:    Lala Kounta
# DATA INPUT:       .gpkg files from cleaned neon data 
# DATA OUTPUT:      Png files of representative scales of extraction
# DATE:             July 2025 (last updated 10 April 2026)
# OVERVIEW:         Schematic diagram of spatial scale for data extraction
# =============================================================================


req <- c(
  "sf", "ggplot2", "dplyr", "tigris", "ggspatial",
  "units", "cowplot", "ggrepel", "grid"
)
ins <- req[!req %in% installed.packages()[, "Package"]]
if (length(ins)) install.packages(ins, repos = "https://cloud.r-project.org")

library(sf)
library(ggplot2)
library(dplyr)
library(tigris)
library(ggspatial)
library(units)
library(cowplot)
library(ggrepel)
library(grid)

options(tigris_use_cache = TRUE)

source("./R/config.R")

# =========================================================
# Load data
# =========================================================
site <- st_read(paste0(polygon_dir, "/NEON_site_footprint.gpkg"), quiet = TRUE)
plt <- st_read(paste0(polygon_dir, "/NEON_mammal_plot_footprint.gpkg"), quiet = TRUE)
plt_radii <- st_read(paste0(polygon_dir, "/NEON_tower_plot_radii.gpkg"), quiet = TRUE)
site_radii <- st_read(paste0(polygon_dir, "/NEON_tower_site_radii.gpkg"), quiet = TRUE)
dom_radii <- st_read(paste0(polygon_dir, "/NEON_tower_domain_radii.gpkg"), quiet = TRUE)
dom <- st_read(paste0(polygon_dir, "/NEON_domain_footprint.gpkg"), quiet = TRUE)

# =========================================================
# CRS
# =========================================================
crs_conus <- 5070   # NAD83 / Conus Albers
crs_ak    <- 3338   # Alaska Albers
crs_hi    <- 3759   # Hawaii Albers Equal Area Conic
crs_pr    <- 32161  # Puerto Rico StatePlane

# =========================================================
# State boundaries
# =========================================================
states <- tigris::states(cb = TRUE, year = 2023) |>
  st_as_sf()

states_conus <- states |>
  filter(!NAME %in% c("Alaska", "Hawaii", "Puerto Rico"),
         !STUSPS %in% c("VI", "AS", "MP", "GU")) |>
  st_transform(crs_conus)

states_ak <- states |>
  filter(NAME == "Alaska") |>
  st_transform(crs_ak)

states_hi <- states |>
  filter(NAME == "Hawaii") |>
  st_transform(crs_hi)

states_pr <- states |>
  filter(NAME == "Puerto Rico") |>
  st_transform(crs_pr)

# =========================================================
# Helpers
# =========================================================
expand_bbox <- function(x, frac = 0.05) {
  bb <- st_bbox(x)
  dx <- (bb$xmax - bb$xmin) * frac
  dy <- (bb$ymax - bb$ymin) * frac
  c(
    xmin = bb$xmin - dx,
    xmax = bb$xmax + dx,
    ymin = bb$ymin - dy,
    ymax = bb$ymax + dy
  )
}

clip_to_region <- function(layer, region_poly) {
  region_union <- st_union(region_poly) |> st_make_valid()
  layer <- st_make_valid(layer)
  clipped <- suppressWarnings(st_intersection(layer, region_union))
  clipped <- clipped[!st_is_empty(clipped), ]
  clipped
}

select_to_region <- function(layer, region_poly) {
  region_union <- st_union(region_poly) |> st_make_valid()
  layer <- st_make_valid(layer)
  keep <- lengths(st_intersects(layer, region_union)) > 0
  layer[keep, ]
}

crop_states_to_layer_bbox <- function(states_region, layer, frac = 0.10) {
  bb <- st_bbox(layer)
  dx <- (bb$xmax - bb$xmin) * frac
  dy <- (bb$ymax - bb$ymin) * frac
  
  bb_sfc <- st_as_sfc(st_bbox(c(
   bb$xmin - dx,
   bb$xmax + dx,
   bb$ymin - dy,
   bb$ymax + dy
  ), crs = st_crs(layer)))
  
  suppressWarnings(st_crop(states_region, bb_sfc))
}

prep_region <- function(site, dom, dom_radii, states_region, crs_region) {
  site_r <- st_transform(site, crs_region)
  dom_r <- st_transform(dom, crs_region)
  dom_radii_r <- st_transform(dom_radii, crs_region)
  
  list(
    site = select_to_region(site_r, states_region),
    dom = clip_to_region(dom_r, states_region),
    dom_radii = clip_to_region(dom_radii_r, states_region)
  )
}

# =========================================================
# Prepare region-specific layers
# =========================================================
conus <- prep_region(site, dom, dom_radii, states_conus, crs_conus)
ak    <- prep_region(site, dom, dom_radii, states_ak, crs_ak)
hi    <- prep_region(site, dom, dom_radii, states_hi, crs_hi)
pr    <- prep_region(site, dom, dom_radii, states_pr, crs_pr)

# Crop Hawaii state geometry to the NEON Hawaii domain extent
# This removes the far outlying islands from TIGER
if (nrow(hi$dom) > 0) {
  states_hi <- crop_states_to_layer_bbox(states_hi, hi$dom, frac = 0.12)
  # re-trim the data using the cropped Hawaii extent for cleaner inset plotting
  hi$dom <- clip_to_region(hi$dom, states_hi)
  hi$dom_radii <- clip_to_region(hi$dom_radii, states_hi)
  hi$site <- select_to_region(hi$site, states_hi)
}

# Optional: tighten Puerto Rico a bit too
if (nrow(pr$dom) > 0) {
  states_pr <- crop_states_to_layer_bbox(states_pr, pr$dom, frac = 0.10)
  pr$dom <- clip_to_region(pr$dom, states_pr)
  pr$dom_radii <- clip_to_region(pr$dom_radii, states_pr)
  pr$site <- select_to_region(pr$site, states_pr)
}

# =========================================================
# Site label prep for CONUS
# =========================================================
# Use site centroids / point on surface for labeling
site_pts_conus <- st_point_on_surface(conus$site)
coords_conus <- st_coordinates(site_pts_conus)

site_labels_conus <- site_pts_conus |>
  st_drop_geometry() |>
  mutate(
    x = coords_conus[, 1],
    y = coords_conus[, 2]
  )

# =========================================================
# Theme
# =========================================================
map_theme <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_rect(color = "gray60", fill = NA, linewidth = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.justification = "center",
    legend.background = element_rect(fill = alpha("white", 0.92), color = NA)
  )

# =========================================================
# Main CONUS plot
# =========================================================
bb_conus <- expand_bbox(states_conus, frac = 0.03)

p_conus <- ggplot() +
  geom_sf(
    data = states_conus,
    aes(color = "State boundaries"),
    fill = NA,
    linewidth = 0.25,
    show.legend = TRUE
  ) +
  geom_sf(
    data = conus$dom,
    aes(fill = "NEON domain footprint"),
    color = "gray40",
    linewidth = 0.25,
    alpha = 0.42,
    show.legend = TRUE
  ) +
  geom_sf(
    data = conus$dom_radii,
    aes(fill = "Domain radius"),
    color = NA,
    alpha = 0.22,
    show.legend = TRUE
  ) +
  geom_sf(
    data = conus$site,
    aes(fill = "Site footprint"),
    color = "black",
    linewidth = 0.20,
    alpha = 0.95,
    show.legend = TRUE
  ) +
  geom_text_repel(
    data = site_labels_conus,
    aes(x = x, y = y, label = siteID),
    size = 2.7,
    family = "sans",
    min.segment.length = 0,
    segment.color = "gray25",
    segment.size = 0.25,
    box.padding = 0.20,
    point.padding = 0.08,
    force = 2.7,
    force_pull = 0.5,
    max.overlaps = Inf,
    seed = 123
  ) +
  coord_sf(
    xlim = c(bb_conus["xmin"], bb_conus["xmax"]),
    ylim = c(bb_conus["ymin"], bb_conus["ymax"]),
    expand = FALSE
  ) +
  scale_fill_manual(
    breaks = c("NEON domain footprint", "Domain radius", "Site footprint"),
    values = c(
      "NEON domain footprint" = "#B8C9A6",
      "Domain radius" = "#5B8DB8",
      "Site footprint" = "#D95F02"
    )
  ) +
  scale_color_manual(
    breaks = c("State boundaries"),
    values = c("State boundaries" = "gray85")
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.14,
    pad_x = unit(0.20, "in"),
    pad_y = unit(0.20, "in"),
    text_cex = 0.8,
    line_width = 0.6
  ) +
  guides(
    fill = guide_legend(
      nrow = 1,
      byrow = TRUE,
      override.aes = list(
        alpha = c(0.42, 0.22, 0.95),
        color = c("gray40", NA, "black")
      )
    ),
    color = guide_legend(
      nrow = 1,
      byrow = TRUE,
      override.aes = list(fill = NA, linewidth = 0.5)
    )
  ) +
  map_theme

# =========================================================
# Inset function
# =========================================================
make_inset_map <- function(states_region, dom_region, dom_radii_region, site_region, label) {
  bb <- expand_bbox(states_region, frac = 0.03)
  
  ggplot() +
    geom_sf(
      data = states_region,
      color = "gray85",
      fill = NA,
      linewidth = 0.22
    ) +
    geom_sf(
      data = dom_region,
      fill = "#B8C9A6",
      color = "gray40",
      linewidth = 0.18,
      alpha = 0.42
    ) +
    geom_sf(
      data = dom_radii_region,
      fill = "#5B8DB8",
      color = NA,
      alpha = 0.22
    ) +
    geom_sf(
      data = site_region,
      fill = "#D95F02",
      color = "black",
      linewidth = 0.16,
      alpha = 0.95
    ) +
    coord_sf(
      xlim = c(bb["xmin"], bb["xmax"]),
      ylim = c(bb["ymin"], bb["ymax"]),
      expand = FALSE
    ) +
    annotate(
      "text",
      x = bb["xmin"] + 0.04 * (bb["xmax"] - bb["xmin"]),
      y = bb["ymax"] - 0.08 * (bb["ymax"] - bb["ymin"]),
      label = label,
      hjust = 0,
      size = 3.0,
      fontface = "bold"
    ) +
    theme_void() +
    theme(
      panel.border = element_rect(color = "gray60", fill = NA, linewidth = 0.5),
      plot.background = element_rect(fill = alpha("white", 0.96), color = NA)
    )
}

p_ak <- make_inset_map(states_ak, ak$dom, ak$dom_radii, ak$site, "AK")
p_hi <- make_inset_map(states_hi, hi$dom, hi$dom_radii, hi$site, "HI")
p_pr <- make_inset_map(states_pr, pr$dom, pr$dom_radii, pr$site, "PR")

# =========================================================
# Assemble final figure
# Right-side vertical column of inset boxes
# =========================================================
final_map <- ggdraw() +
  draw_plot(p_conus, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(p_ak, x = 0.79, y = 0.08, width = 0.18, height = 0.22) +
  draw_plot(p_hi, x = 0.79, y = 0.32, width = 0.18, height = 0.15) +
  draw_plot(p_pr, x = 0.79, y = 0.50, width = 0.18, height = 0.12)

final_map

# =========================================================
# Save
# =========================================================
ggsave(
  filename = file.path("NEON_map_final.png"),
  plot = final_map,
  width = 15,
  height = 9,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = file.path("NEON_map_final.pdf"),
  plot = final_map,
  width = 15,
  height = 9,
  units = "in",
  bg = "white"
)