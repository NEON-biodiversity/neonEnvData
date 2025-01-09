# TITLE:            Geodiversity Metric Calculations
# PROJECT:          NEON Geodiversity Analysis
# AUTHORS:          Kelly Kapsar, Pat Bills, Phoebe Zarnetske 
# COLLABORATORS:    Lala Kounta
# DATA INPUT:       
# DATA OUTPUT:      Elevation plots for NEON sites
# DATE:             November 2024
# OVERVIEW:         Script to generate elevation plots
# REQUIRES:         R libraries: geodiv, terra, sf, dplyr, ggplot2
# NOTES:            Ensure SRTM data and shapefiles are correctly organized


# Import libraries 
library(dplyr)
library(terra)
library(sf)
library(ggplot2)

ls <- list.files("/mnt/scratch/kapsarke/geodiversity/output/polys_EPSG5070_intersected_test/", pattern=".shp", full.names = T)

t <- st_read(ls[[2]]) %>% mutate(scale = "site", method = "footprint", metric_type = "elevation")
u <- st_read(ls[[4]]) %>% mutate(scale = "site", method = "radius", metric_type = "elevation")
v <- rbind(t, u)

# Define the function
plot_numeric_column <- function(sf_object, x_col, numeric_col, output_dir) {
  # Ensure the input columns exist in the data
  if (!(x_col %in% names(sf_object)) | !(numeric_col %in% names(sf_object))) {
    stop("The specified columns are not found in the sf object.")
  }
  
  # Determine metric type
  metric_type <- unique(sf_object$metric_type)
  
  # Filter for non-NA values and arrange by the numeric column
  data_to_plot <- sf_object %>%
    filter(!is.na(.data[[numeric_col]])) %>%
    arrange(desc(.data[[numeric_col]]))
  
  # Reorder the x_col factor levels based on the sorted numeric column
  data_to_plot[[x_col]] <- factor(data_to_plot[[x_col]], levels = unique(data_to_plot[[x_col]]))
  
  # Create the ggplot
  # plt <- ggplot(data_to_plot, aes_string(x = x_col, y = numeric_col)) +
  #   geom_point(stat = "identity", fill = "steelblue") +
  #   labs(
  #     title = paste("Plot of", numeric_col, metric_type),
  #     x = x_col,
  #     y = metric_type
  #   ) +
  #   theme_minimal() +
  #   theme(
  #     axis.text.x = element_text(angle = 90, hjust = 1)
  #   )
  
  # Preprocess the data to calculate the line color
  data_to_plot <- data_to_plot %>%
    group_by(!!sym(x_col)) %>%
    arrange(method) %>%
    mutate(
      next_value = lead(!!sym(numeric_col)),
      line_color = case_when(
        method == "footprint" & next_value > !!sym(numeric_col) ~ "red",
        method == "footprint" & next_value < !!sym(numeric_col) ~ "blue",
        TRUE ~ NA_character_
      )
    ) %>%
    ungroup()
  
  # Filter out rows with NA line_color as they don’t need connecting lines
  line_data <- data_to_plot %>%
    filter(!is.na(line_color))
  
  plt <- ggplot(data_to_plot, aes_string(x = x_col)) +
    # Add the points for each method
    geom_point(aes_string(y = numeric_col, color = "method"), size = 3) +
    # Add the connecting lines with preprocessed color
    geom_line(
      data = line_data,
      aes_string(x = x_col, y = numeric_col, color = "line_color"),
      size = 1
    ) +
    # Add labels and theme
    labs(
      title = paste("Comparison of", numeric_col, "by Method"),
      x = x_col,
      y = numeric_col
    ) +
    scale_color_manual(values = c("footprint" = "black", "radius" = "gray", "blue" = "blue", "red" = "red")) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.title = element_blank()
    )
  
  
  plt
  
  # Construct the filename
  scale <- unique(sf_object$scale)
  method <- unique(sf_object$method)
  
  # filename <- paste0(output_dir, "/", scale, "_", method, "_", metric_type, "_", numeric_col, ".png")
  
  # Save the plot
  # ggsave(filename, plt, width = 10, height = 6)
}


sf_object = v
x_col = "siteID"
numeric_col = "mean"
output_dir = "."

# Example usage: Plot the mean column using siteID as the x-axis
plot_numeric_column(
  sf_object = v, 
  x_col = "siteID", 
  numeric_col = "mean", 
  output_dir = ".")



numeric_cols <- names(t)[sapply(t, is.numeric)]

lapply(numeric_cols, 
       function(x)plot_numeric_column(
         sf_object = t, 
         x_col = "siteID", 
         numeric_col = x, 
         output_dir = "./figures"))




