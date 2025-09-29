library(dplyr)
library(tidyr)

# Function to find the most contrasting site pair for each scale
find_contrasting_sites <- function(data_long, metric) {
  
  # Step 1: subset to the metric of interest and average per siteID/scale
  df <- data_long %>%
    filter(metric == !!metric,
           !is.na(siteID)) %>%
    mutate(scale_label = case_when(
      extent == "plot" ~ "plot",
      extent == "site" & extent_type == "radii" & elev_res == 30  ~ "site radius 30m",
      extent == "site" & extent_type == "radii" & elev_res == 300 ~ "site radius 300m",
      extent == "site" & extent_type == "footprint" & elev_res == 300 ~ "site footprint 300m",
      extent == "domain" & extent_type == "radii" & elev_res == 30  ~ "domain radius 30m",
      extent == "domain" & extent_type == "radii" & elev_res == 300 ~ "domain radius 300m",
      extent == "domain" & extent_type == "footprint" & elev_res == 300 ~ "domain footprint 300m",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(scale_label)) %>%
    group_by(scale_label, siteID) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
  
  # Step 2: compute all pairwise differences per scale
  pairs <- df %>%
    inner_join(df, by = "scale_label", suffix = c("_a", "_b")) %>%
    filter(siteID_a < siteID_b) %>%
    mutate(diff = abs(value_a - value_b))
  
  # Step 3: find the max difference for each scale
  top_pairs <- pairs %>%
    group_by(scale_label) %>%
    slice_max(order_by = diff, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  return(top_pairs)
}

# Example usage
contrasts <- data.frame()
for(i in 34:length(unique(neon_df_long$metric))){
  temp <- find_contrasting_sites(neon_df_long, metric = unique(neon_df_long$metric[i]))
  temp$metric <- unique(neon_df_long$metric[i])
  contrasts <- rbind(contrasts, temp)
  rm(temp)
}

print(contrasts)
