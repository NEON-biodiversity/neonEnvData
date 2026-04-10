# =============================================================================
# TITLE:            Data Paper Figure 1a Code
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
  "units", "ggrepel", "grid"
)
ins <- req[!req %in% installed.packages()[, "Package"]]
if (length(ins)) install.packages(ins, repos = "https://cloud.r-project.org")

library(sf)
library(ggplot2)
library(dplyr)
library(tigris)
library(ggspatial)
library(units)
library(ggrepel)
library(grid)

options(tigris_use_cache = TRUE)

source("./R/config.R")

# =========================================================
# Helpers: text sizing
# =========================================================
pt_to_mm <- function(pt) pt * 25.4 / 72.27

# annotation_scale() uses text_cex, which is a multiplier rather than pt.
# Approximate default text size is ~8.8 pt, so convert target pt to cex.
pt_to_cex_for_scalebar <- function(pt, base_pt = 8.8) pt / base_pt

# ---------------------------------------------------------
# Final output-specific text targets
# ---------------------------------------------------------
# CONUS final output target: 6" x 4"
conus_base_pt        <- 10.5
conus_label_pt       <- 9.5
conus_scalebar_pt    <- 9.5
conus_region_pt      <- 10.5

# Insets final output target: 2" x 3"
inset_base_pt        <- 8
inset_label_pt       <- 9
inset_scalebar_pt    <- 8
inset_region_pt      <- 8.5

# Converted units for geoms using mm text size
conus_label_mm       <- pt_to_mm(conus_label_pt)
conus_region_mm      <- pt_to_mm(conus_region_pt)

inset_label_mm       <- pt_to_mm(inset_label_pt)
inset_region_mm      <- pt_to_mm(inset_region_pt)

# Converted units for scale bar
conus_scalebar_cex   <- pt_to_cex_for_scalebar(conus_scalebar_pt)
inset_scalebar_cex   <- pt_to_cex_for_scalebar(inset_scalebar_pt)

# =========================================================
# Load data
# =========================================================
site <- st_read(paste0(polygon_dir, "/NEON_site_footprint.gpkg"), quiet = TRUE)
plt <- st_read(paste0(polygon_dir, "/NEON_mammal_plot_footprint.gpkg"), quiet = TRUE)
plt_radii <- st_read(paste0(polygon_dir, "/NEON_tower_plot_radii.gpkg"), quiet = TRUE)
site_radii <- st_read(paste0(polygon_dir, "/NEON_tower_site_radii.gpkg"), quiet = TRUE)
dom_radii <- st_read(paste0(polygon_dir, "/NEON_tower_domain_radii.gpkg"), quiet = TRUE)
dom <- st_read(paste0(polygon_dir, "/NEON_domain_footprint.gpkg"), quiet = TRUE)

# Make geometries valid up front
site <- st_make_valid(site)
plt <- st_make_valid(plt)
plt_radii <- st_make_valid(plt_radii)
site_radii <- st_make_valid(site_radii)
dom_radii <- st_make_valid(dom_radii)
dom <- st_make_valid(dom)

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
  st_as_sf() |>
  st_make_valid()

states_conus <- states |>
  filter(!NAME %in% c("Alaska", "Hawaii", "Puerto Rico"),
         !STUSPS %in% c("VI", "AS", "MP", "GU")) |>
  st_transform(crs_conus) |>
  st_make_valid()

states_ak <- states |>
  filter(NAME == "Alaska") |>
  st_transform(crs_ak) |>
  st_make_valid()

states_hi <- states |>
  filter(NAME == "Hawaii") |>
  st_transform(crs_hi) |>
  st_make_valid()

states_pr <- states |>
  filter(NAME == "Puerto Rico") |>
  st_transform(crs_pr) |>
  st_make_valid()

# =========================================================
# Geometry helpers
# =========================================================
expand_bbox_asym <- function(x,
                             pad_left = 0.05,
                             pad_right = 0.05,
                             pad_bottom = 0.05,
                             pad_top = 0.05) {
  bb <- st_bbox(x)
  dx <- bb$xmax - bb$xmin
  dy <- bb$ymax - bb$ymin
  
  c(
    bb$xmin - dx * pad_left,
    bb$xmax + dx * pad_right,
    bb$ymin - dy * pad_bottom,
    bb$ymax + dy * pad_top
  )
}

is_valid_sf <- function(x) {
  if (is.null(x) || nrow(x) == 0) return(FALSE)
  
  x <- suppressWarnings(st_make_valid(x))
  x <- x[!st_is_empty(x), ]
  
  if (nrow(x) == 0) return(FALSE)
  
  bb <- tryCatch(st_bbox(x), error = function(e) NULL)
  if (is.null(bb)) return(FALSE)
  
  all(is.finite(unname(bb)))
}

clip_to_region <- function(layer, region_poly) {
  if (is.null(layer) || nrow(layer) == 0) return(layer)
  
  region_union <- st_union(region_poly) |> st_make_valid()
  layer <- st_make_valid(layer)
  
  clipped <- suppressWarnings(st_intersection(layer, region_union))
  clipped <- clipped[!st_is_empty(clipped), ]
  
  clipped
}

select_to_region <- function(layer, region_poly) {
  if (is.null(layer) || nrow(layer) == 0) return(layer)
  
  region_union <- st_union(region_poly) |> st_make_valid()
  layer <- st_make_valid(layer)
  
  keep <- lengths(st_intersects(layer, region_union)) > 0
  layer[keep, ]
}

crop_states_to_layer_bbox <- function(states_region, layer, frac = 0.10) {
  if (is.null(layer) || nrow(layer) == 0) return(states_region)
  
  layer <- st_make_valid(layer)
  layer <- layer[!st_is_empty(layer), ]
  
  if (nrow(layer) == 0) return(states_region)
  
  bb <- tryCatch(st_bbox(layer), error = function(e) NULL)
  if (is.null(bb) || any(!is.finite(unname(bb)))) {
    return(states_region)
  }
  
  dx <- bb["xmax"] - bb["xmin"] * frac
  dy <- bb["ymax"] - bb["ymin"] * frac
  
  bb_sfc <- st_as_sfc(st_bbox(c(
    bb["xmin"] - dx,
    bb["xmax"] + dx,
    bb["ymin"] - dy,
    bb["ymax"] + dy
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

prep_label_points <- function(site_layer, label_col = "siteID") {
  if (is.null(site_layer) || nrow(site_layer) == 0) {
    out <- data.frame(
      label = character(0),
      x = numeric(0),
      y = numeric(0),
      stringsAsFactors = FALSE
    )
    rownames(out) <- NULL
    return(out)
  }
  
  pts <- suppressWarnings(st_point_on_surface(site_layer))
  xy <- st_coordinates(pts)
  
  out <- pts |>
    st_drop_geometry() |>
    mutate(
      label = as.character(.data[[label_col]]),
      x = xy[, 1],
      y = xy[, 2]
    ) |>
    mutate(
      label = trimws(label)
    ) |>
    filter(
      !is.na(label),
      nzchar(label),
      !is.na(x),
      !is.na(y),
      is.finite(x),
      is.finite(y)
    ) |>
    distinct(label, x, y, .keep_all = TRUE)
  
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}

add_repel_labels <- function(p, label_data, size = 4,
                             box.padding = 0.4,
                             point.padding = 0.12,
                             force = 10,
                             force_pull = 0.15,
                             direction = "both",
                             ylim_vals = NULL) {
  if (is.null(label_data) || nrow(label_data) == 0) {
    return(p)
  }
  
  label_data <- as.data.frame(label_data, stringsAsFactors = FALSE)
  rownames(label_data) <- NULL
  
  label_data <- label_data[
    !is.na(label_data$x) &
      !is.na(label_data$y) &
      is.finite(label_data$x) &
      is.finite(label_data$y) &
      !is.na(label_data$label) &
      nzchar(label_data$label),
    , drop = FALSE
  ]
  
  rownames(label_data) <- NULL
  
  if (nrow(label_data) == 0) {
    return(p)
  }
  
  args <- list(
    mapping = aes(x = x, y = y, label = label),
    data = label_data,
    size = size,
    fontface = "bold",
    family = "sans",
    min.segment.length = 0,
    segment.color = "gray25",
    segment.size = 0.50,
    box.padding = box.padding,
    point.padding = point.padding,
    force = force,
    force_pull = force_pull,
    max.overlaps = Inf,
    seed = 123,
    direction = direction,
    na.rm = TRUE
  )
  
  if (!is.null(ylim_vals)) {
    args$ylim <- unname(as.numeric(ylim_vals))
  }
  
  p + do.call(geom_text_repel, args)
}

# =========================================================
# Prepare region-specific layers
# =========================================================
conus <- prep_region(site, dom, dom_radii, states_conus, crs_conus)
ak    <- prep_region(site, dom, dom_radii, states_ak, crs_ak)
hi    <- prep_region(site, dom, dom_radii, states_hi, crs_hi)
pr    <- prep_region(site, dom, dom_radii, states_pr, crs_pr)

# Crop Hawaii state geometry to NEON Hawaii domain extent
if (is_valid_sf(hi$dom)) {
  states_hi <- crop_states_to_layer_bbox(states_hi, hi$dom, frac = 0.15)
  hi$dom <- clip_to_region(hi$dom, states_hi)
  hi$dom_radii <- clip_to_region(hi$dom_radii, states_hi)
  hi$site <- select_to_region(hi$site, states_hi)
}

# Tighten Puerto Rico to NEON domain extent
if (is_valid_sf(pr$dom)) {
  states_pr <- crop_states_to_layer_bbox(states_pr, pr$dom, frac = 0.15)
  pr$dom <- clip_to_region(pr$dom, states_pr)
  pr$dom_radii <- clip_to_region(pr$dom_radii, states_pr)
  pr$site <- select_to_region(pr$site, states_pr)
}

# =========================================================
# Label data
# =========================================================
site_labels_conus <- prep_label_points(conus$site)
site_labels_ak    <- prep_label_points(ak$site)
site_labels_hi    <- prep_label_points(hi$site)
site_labels_pr    <- prep_label_points(pr$site)

# =========================================================
# Themes
# =========================================================
map_theme_conus <- theme_minimal(base_size = conus_base_pt) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = NA, color = NA),
    legend.position = "none"
  )

inset_theme <- theme_void(base_size = inset_base_pt) +
  theme(
    panel.border = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = NA, color = NA),
    legend.position = "none"
  )

# =========================================================
# Common scales
# =========================================================
fill_scale <- scale_fill_manual(
  breaks = c("NEON domain footprint", "Domain radius", "Site footprint"),
  values = c(
    "NEON domain footprint" = "darkgray",
    "Domain radius" = "#05718b",
    "Site footprint" = "white"
  )
)

legend_guides <- guides(
  fill = guide_legend(
    nrow = 1,
    byrow = TRUE,
    override.aes = list(
      alpha = c(0.42, 0.22, 1),
      color = c("gray40", NA, NA)
    )
  )
)

# =========================================================
# Main CONUS map
# =========================================================
bb_conus <- expand_bbox_asym(
  states_conus,
  pad_left = 0.03,
  pad_right = 0.03,
  pad_bottom = 0.03,
  pad_top = 0.03
)

scalebar <- FALSE

p_conus <- ggplot() +
  geom_sf(
    data = states_conus,
    color = "gray85",
    fill = NA,
    linewidth = 0.25,
    show.legend = FALSE
  ) +
  geom_sf(
    data = conus$dom,
    aes(fill = "NEON domain footprint"),
    color = "gray40",
    linewidth = 0.25,
    alpha = 1,
    show.legend = TRUE
  ) +
  geom_sf(
    data = conus$dom_radii,
    aes(fill = "Domain radius"),
    color = NA,
    alpha = 0.7,
    show.legend = TRUE
  ) +
  geom_sf(
    data = conus$site,
    aes(fill = "Site footprint"),
    color = NA,
    alpha = 1,
    show.legend = TRUE
  )

p_conus <- add_repel_labels(
  p = p_conus,
  label_data = site_labels_conus,
  size = conus_label_mm,
  box.padding = 0.18,
  point.padding = 0.08,
  force = 2.6,
  force_pull = 0.5,
  direction = "both"
) +
  coord_sf(
    xlim = unname(c(bb_conus["xmin"], bb_conus["xmax"])),
    ylim = unname(c(bb_conus["ymin"], bb_conus["ymax"])),
    expand = FALSE
  ) +
  fill_scale + 
  legend_guides +
  map_theme_conus

  if (scalebar) {
    p <- p + annotation_scale(
      location = "bl",
      width_hint = scale_width_hint,
      pad_x = unit(0.12, "in"),
      pad_y = unit(0.12, "in"),
      text_cex = inset_scalebar_cex,
      line_width = 0.55
    )
  }

# =========================================================
# Inset map function
# =========================================================
make_inset_map <- function(states_region,
                           dom_region,
                           dom_radii_region,
                           site_region,
                           label_data,
                           region_label = "",
                           pad_left = 0.05,
                           pad_right = 0.05,
                           pad_bottom = 0.05,
                           pad_top = 0.05,
                           scale_width_hint = 0.25,
                           label_bottom_exclude = 0.18,
                           scalebar = TRUE){
  
  bb <- expand_bbox_asym(
    states_region,
    pad_left = pad_left,
    pad_right = pad_right,
    pad_bottom = pad_bottom,
    pad_top = pad_top
  )
  
  y_min_label <- unname(
    as.numeric(bb["ymin"] + label_bottom_exclude * (bb["ymax"] - bb["ymin"]))
  )
  
  p <- ggplot() +
    geom_sf(
      data = states_region,
      color = "gray85",
      fill = NA,
      linewidth = 0.22,
      show.legend = FALSE
    ) +
    geom_sf(
      data = dom_region,
      aes(fill = "NEON domain footprint"),
      color = "gray40",
      linewidth = 0.18,
      alpha = 1,
      show.legend = FALSE
    ) +
    geom_sf(
      data = dom_radii_region,
      aes(fill = "Domain radius"),
      color = NA,
      alpha = 0.7,
      show.legend = FALSE
    ) +
    geom_sf(
      data = site_region,
      aes(fill = "Site footprint"),
      color = NA,
      alpha = 1,
      show.legend = FALSE
    )
  
  # p <- add_repel_labels(
  #   p = p,
  #   label_data = label_data,
  #   size = inset_label_mm,
  #   box.padding = 0.16,
  #   point.padding = 0.08,
  #   force = 3.0,
  #   force_pull = 0.5,
  #   direction = "both",
  #   ylim_vals = unname(c(y_min_label, Inf))
  # )
  
  p <- p + 
    coord_sf(
      xlim = unname(c(bb["xmin"], bb["xmax"])),
      ylim = unname(c(bb["ymin"], bb["ymax"])),
      expand = FALSE
    ) +
    fill_scale 
  
    if (scalebar) {
      p <- p + annotation_scale(
        location = "bl",
        width_hint = scale_width_hint,
        pad_x = unit(0.12, "in"),
        pad_y = unit(0.12, "in"),
        text_cex = inset_scalebar_cex,
        line_width = 0.55
      )
    } 
  
    p <- p + 
      annotate(
        "text",
        x = unname(bb["xmin"]) + 0.04 * unname(bb["xmax"] - bb["xmin"]),
        y = unname(bb["ymax"]) - 0.07 * unname(bb["ymax"] - bb["ymin"]),
        label = region_label,
        hjust = 0,
        size = inset_region_mm,
        fontface = "bold"
      ) +
      inset_theme
}

# =========================================================
# Separate inset maps
# =========================================================
p_ak <- make_inset_map(
  states_region = states_ak,
  dom_region = ak$dom,
  dom_radii_region = ak$dom_radii,
  site_region = ak$site,
  label_data = site_labels_ak,
  region_label = "",
  pad_left = 0.05,
  pad_right = 0.05,
  pad_bottom = 0.08,
  pad_top = 0.05,
  scale_width_hint = 0.28,
  label_bottom_exclude = 0.16, 
  scalebar=F
)

p_hi <- make_inset_map(
  states_region = states_hi,
  dom_region = hi$dom,
  dom_radii_region = hi$dom_radii,
  site_region = hi$site,
  label_data = site_labels_hi,
  region_label = "",
  pad_left = 0.05,
  pad_right = 0.05,
  pad_bottom = 0.10,
  pad_top = 0.05,
  scale_width_hint = 0.30,
  label_bottom_exclude = 0.18, 
  scalebar=F
)

p_pr <- make_inset_map(
  states_region = states_pr,
  dom_region = pr$dom,
  dom_radii_region = pr$dom_radii,
  site_region = pr$site,
  label_data = site_labels_pr,
  region_label = "",
  pad_left = 0.04,
  pad_right = 0.04,
  pad_bottom = 0.22,
  pad_top = 0.06,
  scale_width_hint = 0.22,
  label_bottom_exclude = 0.22, 
  scalebar=F
)

# =========================================================
# Display
# =========================================================
p_conus
p_ak
p_hi
p_pr

# =========================================================
# Save maps at target final sizes
# =========================================================
ggsave(
  filename = file.path(output_dir, "NEON_map_CONUS.png"),
  plot = p_conus,
  width = 6,
  height = 4.5,
  units = "in",
  dpi = 600,
  bg = "transparent"
)

ggsave(
  filename = file.path(output_dir, "NEON_map_AK.png"),
  plot = p_ak,
  width = 3,
  height = 2,
  units = "in",
  dpi = 600,
  bg = "transparent"
)

ggsave(
  filename = file.path(output_dir, "NEON_map_HI.png"),
  plot = p_hi,
  width = 3,
  height = 2,
  units = "in",
  dpi = 600,
  bg = "transparent"
)

ggsave(
  filename = file.path(output_dir, "NEON_map_PR.png"),
  plot = p_pr,
  width = 3,
  height = 2,
  units = "in",
  dpi = 600,
  bg = "transparent"
)

