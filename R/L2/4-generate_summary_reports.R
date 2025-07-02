library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(tools)
library(stringr)
library(ggplot2)

# Directories for 30m and 300m resolution shapefiles
dirs <- list(
  "clim_elev_30m" = "/mnt/research/neon/neonEnvData/L2/clim_elev_30m/",
  "clim_elev_300m" = "/mnt/research/neon/neonEnvData/L2/clim_elev_300m/"
)

# Function to summarize a shapefile
summarize_shapefile <- function(file_path, elev_res) {
  message("Processing: ", file_path)
  shp <- st_read(file_path, quiet = TRUE)
  
  # Keep only numeric columns
  numeric_data <- shp %>% st_drop_geometry() %>% select(where(is.numeric))
  
  # Compute summary statistics
  summary_stats <- numeric_data %>% 
    summarise(across(everything(), list(
      min = ~min(.x, na.rm = TRUE),
      median = ~median(.x, na.rm = TRUE),
      max = ~max(.x, na.rm = TRUE),
      na_count = ~sum(is.na(.x))
    ), .names = "{.col}_{.fn}"))
  
  # Convert to tidy format
  summary_tidy <- summary_stats %>%
    pivot_longer(
      cols = everything(),
      names_to = c("var_stat", "measure"),
      names_pattern = "^(.*)_(min|median|max|na_count)$"
    ) %>%
    separate(var_stat, into = c("variable", "stat"), sep = "_(?=[^_]+$)") %>%
    pivot_wider(
      names_from = measure,
      values_from = value
    )
  
  # --- Add metadata columns ---
  file_name <- basename(file_path)
  
  # Determine centroid type
  centroid <- case_when(
    str_detect(file_name, "tower") ~ "tower",
    str_detect(file_name, "mammal") ~ "small mammal",
    str_detect(file_name, "footprint") ~ "footprint",
    TRUE ~ NA_character_
  )
  
  # Determine scale from file name
  scale <- case_when(
    str_detect(file_name, "plot") ~ "plot",
    str_detect(file_name, "site") ~ "site",
    str_detect(file_name, "domain") ~ "domain",
    TRUE ~ NA_character_
  )
  
  # Add metadata columns to the tidy summary
  summary_tidy <- summary_tidy %>%
    mutate(
      centroid = centroid,
      elev_res = elev_res,
      scale = scale,
      source_file = file_name
    ) %>%
    relocate(centroid, scale, elev_res, source_file, .before = variable)  
  
  return(summary_tidy)
}


# Combine all summaries
all_summaries <- list()

for (label in names(dirs)) {
  dir_path <- dirs[[label]]
  elev_res <- ifelse(str_detect(label, "30m"), 30, 300)
  shapefiles <- list.files(dir_path, pattern = "\\.shp$", full.names = TRUE)
  
  for (shp_file in shapefiles) {
    # Temp code to count number of rows in each data set to confirm accuracy
    # t <- st_read(shp_file, quiet = T) 
    # print(paste0(shp_file, ": ", nrow(t), "rows."))
    
    print(paste0("ELEVATION RESOLUTION: ", elev_res))
    summary <- summarize_shapefile(shp_file, elev_res)
    all_summaries[[length(all_summaries) + 1]] <- summary
  }
}

# Bind all data frames and write to master CSV
final_summary <- bind_rows(all_summaries)
write_csv(final_summary, "/mnt/research/neon/neonEnvData/L2/all_shapefile_summaries.csv")


################# VISUALIZATIONS ################# 


# Reshape to long format: min/median/max become rows
df_long <- final_summary %>%
  pivot_longer(cols = c(min, median, max),
               names_to = "stat_type",
               values_to = "value")

# Create a unique metric identifier
df_long <- df_long %>%
  mutate(metric_id = paste(variable, stat, sep = "-"))

# Create output directory
output_dir <- "/mnt/research/neon/neonEnvData/L2/figures/"
dir.create(output_dir, showWarnings = FALSE)

# Loop over each unique variable-stat combination
unique_metrics <- unique(df_long$metric_id)

for (metric in unique_metrics) {
  df_subset <- df_long %>% filter(metric_id == metric)
  
  p <- ggplot(df_subset, aes(x = stat_type, y = value,
                             color = centroid,
                             size = scale,
                             shape = factor(elev_res))) +
    geom_violin(aes(group = stat_type), fill = "gray85", color = NA, alpha = 0.5) +
    geom_jitter(width = 0.1, stroke = 0.2) +
    scale_shape_manual(values = c(16, 17)) +  # 30m = circle, 300m = triangle
    scale_size_manual(values = c(
      "plot" = 1.5,
      "site" = 2.5,
      "domain" = 3.5
    )) +
    labs(title = paste("Summary for", metric),
         x = "Summary Statistic",
         y = "Value",
         shape = "Elevation Res",
         color = "Centroid",
         size = "Scale") +
    theme_minimal()
  
  # Save the plot
  ggsave(filename = paste0(output_dir, metric, "_summary_plot.png"),
         plot = p, width = 8, height = 5, bg = "white")
}

