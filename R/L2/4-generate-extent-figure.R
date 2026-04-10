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
  "ggrepel", "grid"
)
ins <- req[!req %in% installed.packages()[, "Package"]]
if (length(ins)) install.packages(ins, repos = "https://cloud.r-project.org")

library(sf)
library(ggplot2)
library(dplyr)
library(tigris)
library(ggspatial)
library(ggrepel)
library(grid)

options(tigris_use_cache = TRUE)

source("./R/config.R")

# =========================================================
# Helpers: text sizing
# =========================================================
pt_to_mm <- function(pt) pt * 25.4 / 72.27
pt_to_cex_for_scalebar <- function(pt, base_pt = 8.8) pt / base_pt

# ---------------------------------------------------------
# Final output-specific text targets
# ---------------------------------------------------------
conus_base_pt      <- 10.5
conus_label_pt     <- 9.5
conus_scalebar_pt  <- 9.5

inset_base_pt      <- 8
inset_scalebar_pt  <- 8
inset_region_pt    <- 8.5

conus_label_mm     <- pt_to_mm(conus_label_pt)
inset_region_mm    <- pt_to_mm(inset_region_pt)

conus_scalebar_cex <- pt_to_cex_for_scalebar(conus_scalebar_pt)
inset_scalebar_cex <- pt_to_cex_for_scalebar(inset_scalebar_pt)

# =========================================================
# Load data
# =========================================================
site <- st_read(paste0(polygon_dir, "/NEON_site_footprint.gpkg"), quiet = TRUE)
dom_radii <- st_read(paste0(polygon_dir, "/NEON_tower_domain_radii.gpkg"), quiet = TRUE)
dom <- st_read(paste0(polygon_dir, "/NEON_domain_footprint.gpkg"), quiet = TRUE)

site <- st_make_valid(site)
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
is_valid_sf <- function(x) {
  if (is.null(x) || nrow(x) == 0) return(FALSE)
  
  x <- suppressWarnings(st_make_valid(x))
  x <- x[!st_is_empty(x), ]
  
  if (nrow(x) == 0) return(FALSE)
  
  bb <- tryCatch(st_bbox(x), error = function(e) NULL)
  if (is.null(bb)) return(FALSE)
  
  all(is.finite(unname(bb)))
}

select_by_anchor_point <- function(layer, region_poly) {
  if (is.null(layer) || nrow(layer) == 0) return(layer)
  
  layer <- st_make_valid(layer)
  layer <- layer[!st_is_empty(layer), ]
  if (nrow(layer) == 0) return(layer)
  
  region_union <- st_union(region_poly) |> st_make_valid()
  anchors <- suppressWarnings(st_point_on_surface(layer))
  
  keep <- lengths(st_intersects(anchors, region_union)) > 0
  layer[keep, ]
}

select_intersecting <- function(layer, region_poly) {
  if (is.null(layer) || nrow(layer) == 0) return(layer)
  
  layer <- st_make_valid(layer)
  layer <- layer[!st_is_empty(layer), ]
  if (nrow(layer) == 0) return(layer)
  
  region_union <- st_union(region_poly) |> st_make_valid()
  keep <- lengths(st_intersects(layer, region_union)) > 0
  layer[keep, ]
}

expand_bbox_asym_from_bbox <- function(bb,
                                       pad_left = 0.05,
                                       pad_right = 0.05,
                                       pad_bottom = 0.05,
                                       pad_top = 0.05) {
  dx <- bb["xmax"] - bb["xmin"]
  dy <- bb["ymax"] - bb["ymin"]
  
  c(
    bb["xmin"] - dx * pad_left,
    bb["xmax"] + dx * pad_right,
    bb["ymin"] - dy * pad_bottom,
    bb["ymax"] + dy * pad_top
  )
}

combine_bbox_layers <- function(...) {
  layers <- list(...)
  layers <- Filter(function(x) inherits(x, "sf") && !is.null(x) && nrow(x) > 0, layers)
  
  if (length(layers) == 0) {
    stop("combine_bbox_layers(): no non-empty sf layers supplied")
  }
  
  bbs <- lapply(layers, st_bbox)
  
  xmin <- min(vapply(bbs, function(bb) unname(bb["xmin"]), numeric(1)), na.rm = TRUE)
  xmax <- max(vapply(bbs, function(bb) unname(bb["xmax"]), numeric(1)), na.rm = TRUE)
  ymin <- min(vapply(bbs, function(bb) unname(bb["ymin"]), numeric(1)), na.rm = TRUE)
  ymax <- max(vapply(bbs, function(bb) unname(bb["ymax"]), numeric(1)), na.rm = TRUE)
  
  st_bbox(
    c(
      xmin = xmin,
      ymin = ymin,
      xmax = xmax,
      ymax = ymax
    ),
    crs = st_crs(layers[[1]])
  )
}

bbox_to_sfc <- function(bb, crs_obj) {
  bb_obj <- structure(
    c(
      unname(bb["xmin"]),
      unname(bb["ymin"]),
      unname(bb["xmax"]),
      unname(bb["ymax"])
    ),
    names = c("xmin", "ymin", "xmax", "ymax"),
    class = "bbox"
  )
  attr(bb_obj, "crs") <- crs_obj
  
  st_as_sfc(bb_obj)
}

clip_to_bbox <- function(layer, bb) {
  if (is.null(layer) || nrow(layer) == 0) return(layer)
  
  layer <- st_make_valid(layer)
  layer <- layer[!st_is_empty(layer), ]
  if (nrow(layer) == 0) return(layer)
  
  bb_poly <- bbox_to_sfc(bb, st_crs(layer))
  
  out <- suppressWarnings(st_intersection(layer, bb_poly))
  out <- out[!st_is_empty(out), ]
  
  out
}

make_state_subregion <- function(states_region, xmin, xmax, ymin, ymax) {
  region_ll <- st_transform(states_region, 4326) |>
    st_make_valid()
  
  bb <- structure(
    c(xmin, ymin, xmax, ymax),
    names = c("xmin", "ymin", "xmax", "ymax"),
    class = "bbox"
  )
  attr(bb, "crs") <- st_crs(region_ll)
  
  clip_box <- st_as_sfc(bb)
  
  out <- suppressWarnings(st_intersection(region_ll, clip_box))
  out <- out[!st_is_empty(out), ]
  
  out |>
    st_make_valid() |>
    st_transform(st_crs(states_region))
}

prep_region_anchor <- function(site, dom, dom_radii, region_anchor, crs_region) {
  site_r <- st_transform(site, crs_region)
  dom_r <- st_transform(dom, crs_region)
  dom_radii_r <- st_transform(dom_radii, crs_region)
  
  list(
    site = select_by_anchor_point(site_r, region_anchor),
    dom = select_intersecting(dom_r, region_anchor),
    dom_radii = select_intersecting(dom_radii_r, region_anchor)
  )
}

prep_region_windowed <- function(site, dom, dom_radii, region_anchor, search_bb, crs_region) {
  site_r <- st_transform(site, crs_region)
  dom_r <- st_transform(dom, crs_region)
  dom_radii_r <- st_transform(dom_radii, crs_region)
  
  search_poly <- bbox_to_sfc(search_bb, st_crs(region_anchor))
  
  list(
    site = select_by_anchor_point(site_r, region_anchor),
    dom = select_intersecting(dom_r, search_poly),
    dom_radii = select_intersecting(dom_radii_r, search_poly)
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
    mutate(label = trimws(label)) |>
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
# Custom regional anchors / backgrounds
# =========================================================
# Alaska mainland only: exclude Aleutians + southeast panhandle
states_ak_mainland <- make_state_subregion(
  states_ak,
  xmin = -170,
  xmax = -141,
  ymin = 55,
  ymax = 72
)

# Hawaii main inhabited islands only
states_hi_main <- make_state_subregion(
  states_hi,
  xmin = -160.7,
  xmax = -154.5,
  ymin = 18.8,
  ymax = 22.6
)

# Puerto Rico full island only
states_pr_main <- states_pr

# =========================================================
# Prepare region-specific layers
# =========================================================
conus <- prep_region_anchor(site, dom, dom_radii, states_conus, crs_conus)
ak    <- prep_region_anchor(site, dom, dom_radii, states_ak_mainland, crs_ak)
hi    <- prep_region_anchor(site, dom, dom_radii, states_hi_main, crs_hi)

# Puerto Rico uses a PR-focused search window so shared PR/Florida geometry
# is captured near PR, then clipped to the PR map extent later.
pr_search_bb <- expand_bbox_asym_from_bbox(
  st_bbox(states_pr_main),
  pad_left = 1.10,
  pad_right = 1.10,
  pad_bottom = 1.10,
  pad_top = 1.10
)

pr <- prep_region_windowed(
  site, dom, dom_radii,
  region_anchor = states_pr_main,
  search_bb = pr_search_bb,
  crs_region = crs_pr
)

# =========================================================
# Label data
# =========================================================
site_labels_conus <- prep_label_points(conus$site)

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
bb_conus <- expand_bbox_asym_from_bbox(
  st_bbox(states_conus),
  pad_left = 0.03,
  pad_right = 0.03,
  pad_bottom = 0.03,
  pad_top = 0.03
)

scalebar_conus <- FALSE

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

if (scalebar_conus) {
  p_conus <- p_conus + annotation_scale(
    location = "bl",
    width_hint = 0.25,
    pad_x = unit(0.12, "in"),
    pad_y = unit(0.12, "in"),
    text_cex = conus_scalebar_cex,
    line_width = 0.55
  )
}

# =========================================================
# Inset extent calculations
# =========================================================
bb_ak <- expand_bbox_asym_from_bbox(
  combine_bbox_layers(states_ak_mainland, ak$dom_radii),
  pad_left = 0.04,
  pad_right = 0.04,
  pad_bottom = 0.05,
  pad_top = 0.05
)

bb_hi <- expand_bbox_asym_from_bbox(
  combine_bbox_layers(states_hi_main, hi$dom_radii),
  pad_left = 0.06,
  pad_right = 0.06,
  pad_bottom = 0.08,
  pad_top = 0.08
)

bb_pr <- expand_bbox_asym_from_bbox(
  combine_bbox_layers(states_pr_main, pr$dom_radii),
  pad_left = 0.08,
  pad_right = 0.08,
  pad_bottom = 0.10,
  pad_top = 0.10
)

# Clip PR shared domain/radius to PR map window so south Florida is excluded
pr$dom <- clip_to_bbox(pr$dom, bb_pr)
pr$dom_radii <- clip_to_bbox(pr$dom_radii, bb_pr)

# =========================================================
# Inset map function
# =========================================================
make_inset_map <- function(states_bg,
                           bb,
                           dom_region,
                           dom_radii_region,
                           site_region,
                           region_label = "",
                           scale_width_hint = 0.25,
                           scalebar = FALSE) {
  
  p <- ggplot() +
    geom_sf(
      data = states_bg,
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
    ) +
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
  
  p
}

# =========================================================
# Separate inset maps
# =========================================================
p_ak <- make_inset_map(
  states_bg = states_ak_mainland,
  bb = bb_ak,
  dom_region = ak$dom,
  dom_radii_region = ak$dom_radii,
  site_region = ak$site,
  region_label = "",
  scale_width_hint = 0.28,
  scalebar = FALSE
)

p_hi <- make_inset_map(
  states_bg = states_hi_main,
  bb = bb_hi,
  dom_region = hi$dom,
  dom_radii_region = hi$dom_radii,
  site_region = hi$site,
  region_label = "",
  scale_width_hint = 0.30,
  scalebar = FALSE
)

p_pr <- make_inset_map(
  states_bg = states_pr_main,
  bb = bb_pr,
  dom_region = pr$dom,
  dom_radii_region = pr$dom_radii,
  site_region = pr$site,
  region_label = "",
  scale_width_hint = 0.22,
  scalebar = FALSE
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
