
################# VISUALIZATIONS ################# 

out_path <- "/mnt/research/neon/neonEnvData/L2/clean_gpkg_files/"

library(sf)
library(dplyr)
library(purrr)
library(stringr)
library(readr)

#' Read NEON GPKGs and add filename-derived columns
#'
#' Handles two patterns:
#'   1) NEON_<plot_loc>_<extent>_<extent_type>_<elev_res>.gpkg
#'   2) NEON_<extent>_footprint_<elev_res>.gpkg   (no plot_loc)
#'
#' @param files Character vector of file paths, or a single directory.
#' @param pattern Regex used when `files` is a directory.
#' @param bind_rows If TRUE, returns a single sf with all rows; if FALSE, a named list of sf objects.
#' @return sf object (or list) with columns: plot_loc, extent, extent_type, elev_res (numeric), source_file.
read_neon_gpkgs <- function(files,
                            pattern = "^NEON_.*\\.gpkg$",
                            bind_rows = TRUE) {
  
  if (length(files) == 1 && dir.exists(files)) {
    files <- list.files(files, pattern = pattern, full.names = TRUE)
  }
  if (length(files) == 0) {
    stop("No files found. Provide file paths or a directory containing NEON_*.gpkg files.")
  }
  
  parse_components <- function(x) {
    nm <- basename(x)
    
    # Case B: footprint method (no plot_loc)
    m_fp <- str_match(nm, "^NEON_([^_]+)_footprint_([^\\.]+)\\.gpkg$")
    if (!is.na(m_fp[1,1])) {
      extent      <- m_fp[1,2]
      extent_type <- "footprint"
      elev_res    <- as.integer(parse_number(m_fp[1,3]))
      return(tibble(
        plot_loc   = NA_character_,
        extent     = extent,
        extent_type = extent_type,
        elev_res   = elev_res,
        source_file = nm
      ))
    }
    
    # Case A: full pattern with plot_loc
    m <- str_match(nm, "^NEON_([^_]+)_([^_]+)_([^_]+)_([^\\.]+)\\.gpkg$")
    if (is.na(m[1,1])) {
      warning(paste0("Filename didn't match expected pattern and will have NA components: ", nm))
      return(tibble(plot_loc = NA_character_, extent = NA_character_,
                    extent_type = NA_character_, elev_res = NA_integer_,
                    source_file = nm))
    }
    
    raw_plot_loc <- m[1,2]
    extent       <- m[1,3]
    extent_type  <- m[1,4]
    elev_res     <- as.integer(parse_number(m[1,5]))
    
    # Only keep plot_loc if it contains "mammal" or "tower"; otherwise NA
    plot_loc <- ifelse(str_detect(raw_plot_loc, "(?i)\\b(mammal|tower)\\b"),
                       raw_plot_loc, NA_character_)
    
    tibble(
      plot_loc    = plot_loc,
      extent      = extent,
      extent_type = extent_type,
      elev_res    = elev_res,
      source_file = nm
    )
  }
  
  layers <- map(files, function(f) {
    meta <- parse_components(f)
    sf::read_sf(f) |> mutate(!!!meta)
  })
  
  names(layers) <- basename(files)
  
  if (bind_rows) {
    bind_rows(layers)
  } else {
    layers
  }
}


files <- list.files("/mnt/research/neon/neonEnvData/L2/clean_gpkg_files/",
                    pattern = "^NEON_.*\\.gpkg$", full.names = TRUE)

neon_all <- read_neon_gpkgs(files)  # one combined sf object


################################################################################

library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(sf)

# LONG TABLE (as before)
id_cols <- c("plotID","siteID","domainID","plot_loc","extent","extent_type","elev_res","source_file")

neon_df_long <- neon_all %>%
  st_drop_geometry() %>%
  select(any_of(id_cols), everything()) %>%
  pivot_longer(
    cols = -all_of(id_cols),
    names_to = "metric",
    values_to = "value"
  ) %>%
  filter(!is.na(value))

# ---------- LABELS ----------
make_scale_labels <- function(df) {
  df %>%
    mutate(
      extent      = tolower(extent),
      extent_type = tolower(extent_type),
      # BIO labels (no elev)
      label_bio = case_when(
        extent == "plot"                                   ~ "plot",
        extent == "site"   & extent_type == "footprint"    ~ "site footprint",
        extent == "site"   & extent_type == "radii"        ~ "site radius",
        extent == "domain" & extent_type == "footprint"    ~ "domain footprint",
        extent == "domain" & extent_type == "radii"        ~ "domain radius",
        TRUE ~ "other"
      ),
      # SRTM labels (with elev where requested)
      label_srtm = case_when(
        extent == "plot"                                   ~ "plot",
        extent == "site"   & extent_type == "footprint" &
          elev_res == 30                                   ~ "site footprint 30m",
        extent == "site"   & extent_type == "footprint" &
          elev_res == 300                                  ~ "site footprint 300m",
        extent == "site"   & extent_type == "radii" &
          elev_res == 30                                   ~ "site radius 30m",
        extent == "site"   & extent_type == "radii" &
          elev_res == 300                                  ~ "site radius 300m",
        extent == "domain" & extent_type == "footprint"    ~ "domain footprint",
        extent == "domain" & extent_type == "radii" &
          elev_res == 30                                   ~ "domain radius 30m",
        extent == "domain" & extent_type == "radii" &
          elev_res == 300                                  ~ "domain radius 300m",
        TRUE ~ "other"
      )
    )
}

# desired orders
ORDER_BIO  <- c("plot", "site footprint", "site radius",
                "domain footprint", "domain radius")

ORDER_SRTM <- c("plot",
                "site footprint 30m", "site footprint 300m",
                "site radius 30m",    "site radius 300m",
                "domain footprint",
                "domain radius 30m",  "domain radius 300m")

order_levels <- function(dfm, metric_name) {
  if (str_detect(metric_name, "^srtm")) {
    present <- ORDER_SRTM[ORDER_SRTM %in% dfm$label_srtm]
    dfm %>% mutate(scale_label = factor(label_srtm, levels = present))
  } else {
    present <- ORDER_BIO[ORDER_BIO %in% dfm$label_bio]
    dfm %>% mutate(scale_label = factor(label_bio, levels = present))
  }
}

# ---------- PLOTTING ----------
# Separate plots for 'mammal' and 'tower'; each includes NA plot_loc rows
plot_metric_by_plotloc <- function(data_long, metric_name, which_plotloc = c("mammal","tower")) {
  plots <- list()
  for (pl in which_plotloc) {
    dfm <- data_long %>%
      filter(metric == metric_name) %>%
      # include target plot_loc and NA
      filter(plot_loc == pl | is.na(plot_loc)) %>%
      make_scale_labels() %>%
      order_levels(metric_name)
    
    p <- ggplot(dfm, aes(x = scale_label, y = value)) +
      geom_violin(trim = FALSE, linewidth = 0.2) +
      geom_jitter(width = 0.15, height = 0, alpha = 0.35, size = 0.6) +
      coord_flip() +
      labs(
        title = paste0(metric_name, " - ", pl),
        x = NULL, y = "Value"
      ) +
      theme_minimal(base_size = 12)
    plots[[pl]] <- p
  }
  plots
}

# Convenience: build plots for many metrics
plot_all_metrics_by_plotloc <- function(data_long, metrics) {
  out <- list()
  for (m in metrics) out[[m]] <- plot_metric_by_plotloc(data_long, m)
  out
}

# ---------------- EXAMPLES ----------------
# One bio metric:
bio_plot_set  <- plot_metric_by_plotloc(neon_df_long, "bio01_mean")
bio_plot_set$mammal  # print mammal plot
bio_plot_set$tower   # print tower plot

srtm_sq_set  <- plot_metric_by_plotloc(neon_df_long, "srtm_sq")
srtm_sq_set$mammal  # print mammal plot
srtm_sq_set$tower   # print tower plot


# All 'bio' metrics:
bio_metrics  <- neon_df_long %>% distinct(metric) %>% filter(str_detect(metric, "^bio"))  %>% pull()
bio_plots    <- plot_all_metrics_by_plotloc(neon_df_long, bio_metrics)

# All 'srtm' metrics (using the new order with 30m/300m variants):
srtm_metrics <- neon_df_long %>% distinct(metric) %>% filter(str_detect(metric, "^srtm")) %>% pull()
srtm_plots   <- plot_all_metrics_by_plotloc(neon_df_long, srtm_metrics)
