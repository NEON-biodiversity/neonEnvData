

library(sf)
library(dplyr)

dom <- st_read("./data/L0/NEON_Domains/NEON_Domains.shp")   %>%  
  mutate(domainNumb = factor(sprintf("D%02d", DomainID))) 

# Get unique DomainID values
unique_domain_ids <- unique(dom$domainNumb)

# Initialize an empty character vector to store SQL queries
sql_queries <- character(length(unique_domain_ids))

# Loop through each unique DomainID to generate SQL queries
for (i in seq_along(unique_domain_ids)) {
  domain_id <- unique_domain_ids[i]
  
  # Create the SQL script for creating the spatial intersection table
  intersection_sql <- sprintf("CREATE TABLE intersected_%s AS (
    SELECT e.*, d.domainNumb
    FROM elevation_with_na e
    JOIN domain d
    ON ST_Intersects(e.geom, d.geom)
    WHERE d.domainNumb = '%s'
  );", domain_id, domain_id)
  
  # Create the SQL script for saving statistics to a CSV file
  copy1_sql <- sprintf("COPY intersected_domain_%s TO '/mnt/nvme/geodiversity/output/intersected_domain_%s.csv' WITH (HEADER, DELIMITER ',');", domain_id, domain_id)
  
  # Create the SQL script for creating the statistics table
  stats_sql <- sprintf("CREATE TABLE stats_domain_%s AS (
    SELECT 
        domainNumb,
        ROUND(AVG(elevation), 4) AS mean_elevation,
        ROUND(STDDEV(elevation), 4) AS stddev_elevation,
        ROUND(STDDEV(elevation) / AVG(elevation) * 100, 4) AS cv_percentage
    FROM intersected_%s
    GROUP BY domainNumb
  );", domain_id, domain_id)
  
  # Create the SQL script for saving statistics to a CSV file
  copy2_sql <- sprintf("COPY stats_domain_%s TO '/mnt/nvme/geodiversity/output/stats_domain_%s.csv' WITH (HEADER, DELIMITER ',');", domain_id, domain_id)
  
  # Combine the SQL queries into one script for the current DomainID
  sql_query <- paste(intersection_sql, copy1_sql, stats_sql, copy2_sql, sep = "\n\n")
  
  # Store the combined SQL query in the vector
  sql_queries[i] <- sql_query
}

# Combine all SQL queries into a single script
final_sql_script <- paste(sql_queries, collapse = "\n\n")

# Optionally, write the script to a .sql file
writeLines(final_sql_script, "./SQL/L0/domain_spatial_intersection_script.sql")

# Print the final SQL scrip
cat(final_sql_script)
