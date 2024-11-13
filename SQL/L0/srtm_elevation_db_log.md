

INSTALL spatial; 
LOAD spatial; 

# Get a look at the tile_ids for the unnecessary extra longitudes 
SELECT DISTINCT tile_id
FROM elevation
WHERE x > 0;

# Remove rows with longitude values greater than 0 (i.e. Eastern Hemisphere)
DELETE FROM elevation
WHERE x >= -60; 

# Verify there are 1057 tile ids (calculated from length of original file names where lat > 14 and long < -60)
SELECT COUNT(DISTINCT tile_id) FROM elevation; 

# Final number of rows 
# 13,706,331,457
SELECT COUNT(*) FROM elevation; 

# Both have 12967201 rows 
SELECT COUNT(*) as row_count FROM elevation WHERE tile_id = 'N59W165';
SELECT COUNT(*) as row_count FROM elevation WHERE tile_id = 'N17W066';

# Confirm that all tiles have this many pixels 
SELECT tile_id, COUNT(*) AS row_count
FROM elevation
GROUP BY tile_id
HAVING COUNT(*) != 12967201;

# Import spatial data 
CREATE TABLE site AS
SELECT * 
FROM ST_Read('/mnt/nvme/geodiversity/spatial_data/polys_EPSG5070/NEON_sites.shp');

CREATE TABLE site_radii AS
SELECT * 
FROM ST_Read('/mnt/nvme/geodiversity/spatial_data/polys_EPSG5070/site_radii.shp');

CREATE TABLE domain AS
SELECT * 
FROM ST_Read('/mnt/nvme/geodiversity/spatial_data/polys_EPSG5070/NEON_domains.shp');

CREATE TABLE domain_radii AS
SELECT * 
FROM ST_Read('/mnt/nvme/geodiversity/spatial_data/polys_EPSG5070/domain_radii.shp');

CREATE TABLE plot AS
SELECT * 
FROM ST_Read('/mnt/nvme/geodiversity/spatial_data/polys_EPSG5070/plot_radii.shp');

# Test out a spatial intersection 
WITH first_site AS (
    SELECT geom
    FROM site
    LIMIT 1
)
SELECT e.*
FROM elevation e, first_site s
WHERE ST_Intersects(e.geom, s.geom);
+
# Sanity check on db elevation values 
select trunc(y) as lat, count(*) as n_points from elevation group by lat order by lat;
select trunc(x) as lon, max(elevation) as max_elevation from elevation group by lon  order by lon;
select trunc(x) as lon, max(elevation) as max_elevation from elevation group by lon  order by lon;
select trunc(y) as lat, min(elevation) as min_elevation from elevation group by lat  order by lat;
# tallest point = mount whitney at 38N and 118 W 
select trunc(y) as lat, max(elevation) as max_elevation from elevation group by lat order by lat;
select trunc(x) as lon, max(elevation) as max_elevation from elevation WHERE x BETWEEN -120 AND -110 group by lon  order by lon;

# Fix large negative elevation values 
CREATE TABLE elevation_with_na AS
SELECT 
    x,
    y,
    tile_id,
    geom,
    CASE
        WHEN TRY_CAST(elevation AS DOUBLE) < -20 THEN NULL
        ELSE TRY_CAST(elevation AS DOUBLE)
    END AS elevation
FROM elevation;



# SITE OUTPUT 
-- Step 1: Save the intersected elevation rows as a spatial table
CREATE TABLE intersected_site AS (
    SELECT e.*, s.siteID
    FROM elevation_with_na e
    JOIN site s
    ON ST_Intersects(e.geom, s.geom)
);

-- Step 2: Drop the geom column and save the intersected_elevations table as a CSV file
COPY (
    SELECT siteID, elevation, x, y, tile_id
    FROM intersected_site
) TO '/mnt/nvme/geodiversity/output/intersected_site.csv' WITH (HEADER, DELIMITER ',');

-- Step 3: Calculate the mean, standard deviation, and CV of elevation for each site and save it to a table
CREATE TABLE stats_site AS
SELECT 
    siteID,
    ROUND(AVG(elevation), 4) AS mean_elevation,
    ROUND(STDDEV(elevation), 4) AS stddev_elevation,
    ROUND(STDDEV(elevation) / AVG(elevation) * 100, 4) AS cv_percentage
FROM intersected_site
GROUP BY siteID;

-- Step 4: Save the site elevation statistics to a CSV file
COPY stats_site TO '/mnt/nvme/geodiversity/output/stats_site.csv' WITH (HEADER, DELIMITER ',');

# PLOT OUTPUT 
-- Step 1: Save the intersected elevation rows as a spatial table
CREATE TABLE intersected_plot AS (
    SELECT e.*, p.siteID, p.plotID
    FROM elevation_with_na e
    JOIN plot p
    ON ST_Intersects(e.geom, p.geom)
);

-- Step 2: Drop the geom column and save the intersected_elevations table as a CSV file
COPY (
    SELECT siteID, plotID, elevation, x, y, tile_id
    FROM intersected_plot
) TO '/mnt/nvme/geodiversity/output/intersected_plot.csv' WITH (HEADER, DELIMITER ',');

-- Step 3: Calculate the mean, standard deviation, and CV of elevation for each plot and save it to a table
CREATE TABLE stats_plot AS
SELECT 
    siteID,
    plotID,
    ROUND(AVG(elevation), 4) AS mean_elevation,
    ROUND(STDDEV(elevation), 4) AS stddev_elevation,
    ROUND(STDDEV(elevation) / AVG(elevation) * 100, 4) AS cv_percentage
FROM intersected_plot
GROUP BY plotID, siteID;

-- Step 4: Save the site elevation statistics to a CSV file
COPY stats_plot TO '/mnt/nvme/geodiversity/output/stats_plot.csv' WITH (HEADER, DELIMITER ',');

# SITE RADII OUTPUT 
-- Step 1: Save the intersected elevation rows as a spatial table
CREATE TABLE intersected_site_radii AS (
    SELECT e.*, s.siteID
    FROM elevation_with_na e
    JOIN site_radii s
    ON ST_Intersects(e.geom, s.geom)
);

-- Step 2: Drop the geom column and save the intersected_elevations table as a CSV file
COPY (
    SELECT siteID, elevation, x, y, tile_id
    FROM intersected_site_radii
) TO '/mnt/nvme/geodiversity/output/intersected_site_radii.csv' WITH (HEADER, DELIMITER ',');

-- Step 3: Calculate the mean, standard deviation, and CV of elevation for each site and save it to a table
CREATE TABLE stats_site_radii AS
SELECT 
    siteID,
    ROUND(AVG(elevation), 4) AS mean_elevation,
    ROUND(STDDEV(elevation), 4) AS stddev_elevation,
    ROUND(STDDEV(elevation) / AVG(elevation) * 100, 4) AS cv_percentage
FROM intersected_site_radii
GROUP BY siteID;

-- Step 4: Save the site elevation statistics to a CSV file
COPY stats_site_radii TO '/mnt/nvme/geodiversity/output/stats_site_radii.csv' WITH (HEADER, DELIMITER ',');

# DOMAIN RADII OUTPUT 
-- Step 1: Save the intersected elevation rows as a spatial table
CREATE TABLE intersected_domain_radii AS (
    SELECT e.*, d.domainNumb, d.siteID
    FROM elevation_with_na e
    JOIN domain_radii d
    ON ST_Intersects(e.geom, d.geom)
);

-- Step 2: Drop the geom column and save the intersected_elevations table as a CSV file
# Not saved for domains bc it's 1.6 billion points
# COPY (
#     SELECT domainNumb, elevation, x, y, tile_id
#     FROM intersected_domain_radii
# ) TO '/mnt/nvme/geodiversity/output/intersected_domain_radii.csv' WITH (HEADER, DELIMITER ',');

-- Step 3: Calculate the mean, standard deviation, and CV of elevation for each site and save it to a table
CREATE TABLE stats_domain_radii AS
SELECT 
    siteID,
    domainNumb,
    ROUND(AVG(elevation), 4) AS mean_elevation,
    ROUND(STDDEV(elevation), 4) AS stddev_elevation,
    ROUND(STDDEV(elevation) / AVG(elevation) * 100, 4) AS cv_percentage
FROM intersected_domain_radii
GROUP BY domainNumb, siteID;

-- Step 4: Save the site elevation statistics to a CSV file
COPY stats_domain_radii TO '/mnt/nvme/geodiversity/output/stats_domain_radii.csv' WITH (HEADER, DELIMITER ',');
