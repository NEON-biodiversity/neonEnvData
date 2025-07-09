library(sf)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)

# Directory containing the shapefiles
shapefile_dir <- "/mnt/scratch/kapsarke/neonEnvData/L2/domain_footprint"

# Metrics of interest
metrics <- c("sq", "sdq", "sbi", "ssk", "sku", "sfd", "stdi", "sds", "mean", "sd")

# Get full list of shapefiles
shapefiles <- list.files(shapefile_dir, pattern = "_elev\\.shp$", full.names = TRUE)

# Function to extract info and read just the necessary columns
read_metric_file <- function(file_path) {
  # Extract domain and metric from filename
  fname <- basename(file_path)
  match <- str_match(fname, "^(D\\d+)_([a-z]+)_elev\\.shp$")
  domain <- match[2]
  metric <- match[3]
  
  # Skip if metric not in the list
  if (is.na(domain) || !(metric %in% metrics)) return(NULL)
  
  # Read shapefile
  shp <- st_read(file_path, quiet = TRUE)
  
  # Ensure required columns exist
  if (!all(c("domainNumb", "mean", "sd", metric) %in% names(shp))) return(NULL)
  
  # Extract one row with only relevant info (drop geometry)
  tibble(
    domainNumb = shp$domainNumb[1],
    mean       = shp$mean[1],
    sd         = shp$sd[1],
    metric     = metric,
    value      = shp[[metric]][1]
  )
}

# Read and bind all the files
shapefile_data <- map_dfr(shapefiles, read_metric_file)

domain_summary <- shapefile_data %>%
  pivot_wider(
    names_from = metric,
    values_from = value
  )

# Preview
print(domain_summary)
