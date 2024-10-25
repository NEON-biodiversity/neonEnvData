

INSTALL spatial; 
LOAD spatial; 

# Get a look at the tile_ids for the unnecessary extra longitudes 
SELECT DISTINCT tile_id
FROM elevation
WHERE x > 0;

# Remove rows with longitude values greater than 0 (i.e. Eastern Hemisphere)
DELETE FROM elevation
WHERE x >= -60; 

# Verify there are 2268 tile ids (calculated from length of original file names where lat > 14 and long < -60)
SELECT COUNT(DISTINCT tile_id) FROM elevation; 

# Final number of rows 
# 29409572257 
SELECT COUNT(*) FROM elevation; 
