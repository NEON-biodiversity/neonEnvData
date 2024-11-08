load spatial;

CREATE TABLE intersected_D01 AS (
    SELECT e.*, d.domainNumb
    FROM elevation_with_na e
    JOIN domain d
    ON ST_Intersects(e.geom, d.geom)
    WHERE d.domainNumb = 'D01'
  );

COPY intersected_domain_D01 TO '/mnt/nvme/geodiversity/output/intersected_domain_D01.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE stats_domain_D01 AS (
    SELECT 
        domainNumb,
        ROUND(AVG(elevation), 4) AS mean_elevation,
        ROUND(STDDEV(elevation), 4) AS stddev_elevation,
        ROUND(STDDEV(elevation) / AVG(elevation) * 100, 4) AS cv_percentage
    FROM intersected_D01
    GROUP BY domainNumb
  );

COPY stats_domain_D01 TO '/mnt/nvme/geodiversity/output/stats_domain_D01.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE intersected_D02 AS (
    SELECT e.*, d.domainNumb
    FROM elevation_with_na e
    JOIN domain d
    ON ST_Intersects(e.geom, d.geom)
    WHERE d.domainNumb = 'D02'
  );

COPY intersected_domain_D02 TO '/mnt/nvme/geodiversity/output/intersected_domain_D02.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE stats_domain_D02 AS (
    SELECT 
        domainNumb,
        ROUND(AVG(elevation), 4) AS mean_elevation,
        ROUND(STDDEV(elevation), 4) AS stddev_elevation,
        ROUND(STDDEV(elevation) / AVG(elevation) * 100, 4) AS cv_percentage
    FROM intersected_D02
    GROUP BY domainNumb
  );

COPY stats_domain_D02 TO '/mnt/nvme/geodiversity/output/stats_domain_D02.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE intersected_D03 AS (
    SELECT e.*, d.domainNumb
    FROM elevation_with_na e
    JOIN domain d
    ON ST_Intersects(e.geom, d.geom)
    WHERE d.domainNumb = 'D03'
  );

COPY intersected_domain_D03 TO '/mnt/nvme/geodiversity/output/intersected_domain_D03.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE stats_domain_D03 AS (
    SELECT 
        domainNumb,
        ROUND(AVG(elevation), 4) AS mean_elevation,
        ROUND(STDDEV(elevation), 4) AS stddev_elevation,
        ROUND(STDDEV(elevation) / AVG(elevation) * 100, 4) AS cv_percentage
    FROM intersected_D03
    GROUP BY domainNumb
  );

COPY stats_domain_D03 TO '/mnt/nvme/geodiversity/output/stats_domain_D03.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE intersected_D04 AS (
    SELECT e.*, d.domainNumb
    FROM elevation_with_na e
    JOIN domain d
    ON ST_Intersects(e.geom, d.geom)
    WHERE d.domainNumb = 'D04'
  );

COPY intersected_domain_D04 TO '/mnt/nvme/geodiversity/output/intersected_domain_D04.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE stats_domain_D04 AS (
    SELECT 
        domainNumb,
        ROUND(AVG(elevation), 4) AS mean_elevation,
        ROUND(STDDEV(elevation), 4) AS stddev_elevation,
        ROUND(STDDEV(elevation) / AVG(elevation) * 100, 4) AS cv_percentage
    FROM intersected_D04
    GROUP BY domainNumb
  );

COPY stats_domain_D04 TO '/mnt/nvme/geodiversity/output/stats_domain_D04.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE intersected_D05 AS (
    SELECT e.*, d.domainNumb
    FROM elevation_with_na e
    JOIN domain d
    ON ST_Intersects(e.geom, d.geom)
    WHERE d.domainNumb = 'D05'
  );

COPY intersected_domain_D05 TO '/mnt/nvme/geodiversity/output/intersected_domain_D05.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE stats_domain_D05 AS (
    SELECT 
        domainNumb,
        ROUND(AVG(elevation), 4) AS mean_elevation,
        ROUND(STDDEV(elevation), 4) AS stddev_elevation,
        ROUND(STDDEV(elevation) / AVG(elevation) * 100, 4) AS cv_percentage
    FROM intersected_D05
    GROUP BY domainNumb
  );

COPY stats_domain_D05 TO '/mnt/nvme/geodiversity/output/stats_domain_D05.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE intersected_D06 AS (
    SELECT e.*, d.domainNumb
    FROM elevation_with_na e
    JOIN domain d
    ON ST_Intersects(e.geom, d.geom)
    WHERE d.domainNumb = 'D06'
  );

COPY intersected_domain_D06 TO '/mnt/nvme/geodiversity/output/intersected_domain_D06.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE stats_domain_D06 AS (
    SELECT 
        domainNumb,
        ROUND(AVG(elevation), 4) AS mean_elevation,
        ROUND(STDDEV(elevation), 4) AS stddev_elevation,
        ROUND(STDDEV(elevation) / AVG(elevation) * 100, 4) AS cv_percentage
    FROM intersected_D06
    GROUP BY domainNumb
  );

COPY stats_domain_D06 TO '/mnt/nvme/geodiversity/output/stats_domain_D06.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE intersected_D07 AS (
    SELECT e.*, d.domainNumb
    FROM elevation_with_na e
    JOIN domain d
    ON ST_Intersects(e.geom, d.geom)
    WHERE d.domainNumb = 'D07'
  );

COPY intersected_domain_D07 TO '/mnt/nvme/geodiversity/output/intersected_domain_D07.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE stats_domain_D07 AS (
    SELECT 
        domainNumb,
        ROUND(AVG(elevation), 4) AS mean_elevation,
        ROUND(STDDEV(elevation), 4) AS stddev_elevation,
        ROUND(STDDEV(elevation) / AVG(elevation) * 100, 4) AS cv_percentage
    FROM intersected_D07
    GROUP BY domainNumb
  );

COPY stats_domain_D07 TO '/mnt/nvme/geodiversity/output/stats_domain_D07.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE intersected_D08 AS (
    SELECT e.*, d.domainNumb
    FROM elevation_with_na e
    JOIN domain d
    ON ST_Intersects(e.geom, d.geom)
    WHERE d.domainNumb = 'D08'
  );

COPY intersected_domain_D08 TO '/mnt/nvme/geodiversity/output/intersected_domain_D08.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE stats_domain_D08 AS (
    SELECT 
        domainNumb,
        ROUND(AVG(elevation), 4) AS mean_elevation,
        ROUND(STDDEV(elevation), 4) AS stddev_elevation,
        ROUND(STDDEV(elevation) / AVG(elevation) * 100, 4) AS cv_percentage
    FROM intersected_D08
    GROUP BY domainNumb
  );

COPY stats_domain_D08 TO '/mnt/nvme/geodiversity/output/stats_domain_D08.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE intersected_D09 AS (
    SELECT e.*, d.domainNumb
    FROM elevation_with_na e
    JOIN domain d
    ON ST_Intersects(e.geom, d.geom)
    WHERE d.domainNumb = 'D09'
  );

COPY intersected_domain_D09 TO '/mnt/nvme/geodiversity/output/intersected_domain_D09.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE stats_domain_D09 AS (
    SELECT 
        domainNumb,
        ROUND(AVG(elevation), 4) AS mean_elevation,
        ROUND(STDDEV(elevation), 4) AS stddev_elevation,
        ROUND(STDDEV(elevation) / AVG(elevation) * 100, 4) AS cv_percentage
    FROM intersected_D09
    GROUP BY domainNumb
  );

COPY stats_domain_D09 TO '/mnt/nvme/geodiversity/output/stats_domain_D09.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE intersected_D10 AS (
    SELECT e.*, d.domainNumb
    FROM elevation_with_na e
    JOIN domain d
    ON ST_Intersects(e.geom, d.geom)
    WHERE d.domainNumb = 'D10'
  );

COPY intersected_domain_D10 TO '/mnt/nvme/geodiversity/output/intersected_domain_D10.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE stats_domain_D10 AS (
    SELECT 
        domainNumb,
        ROUND(AVG(elevation), 4) AS mean_elevation,
        ROUND(STDDEV(elevation), 4) AS stddev_elevation,
        ROUND(STDDEV(elevation) / AVG(elevation) * 100, 4) AS cv_percentage
    FROM intersected_D10
    GROUP BY domainNumb
  );

COPY stats_domain_D10 TO '/mnt/nvme/geodiversity/output/stats_domain_D10.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE intersected_D11 AS (
    SELECT e.*, d.domainNumb
    FROM elevation_with_na e
    JOIN domain d
    ON ST_Intersects(e.geom, d.geom)
    WHERE d.domainNumb = 'D11'
  );

COPY intersected_domain_D11 TO '/mnt/nvme/geodiversity/output/intersected_domain_D11.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE stats_domain_D11 AS (
    SELECT 
        domainNumb,
        ROUND(AVG(elevation), 4) AS mean_elevation,
        ROUND(STDDEV(elevation), 4) AS stddev_elevation,
        ROUND(STDDEV(elevation) / AVG(elevation) * 100, 4) AS cv_percentage
    FROM intersected_D11
    GROUP BY domainNumb
  );

COPY stats_domain_D11 TO '/mnt/nvme/geodiversity/output/stats_domain_D11.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE intersected_D12 AS (
    SELECT e.*, d.domainNumb
    FROM elevation_with_na e
    JOIN domain d
    ON ST_Intersects(e.geom, d.geom)
    WHERE d.domainNumb = 'D12'
  );

COPY intersected_domain_D12 TO '/mnt/nvme/geodiversity/output/intersected_domain_D12.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE stats_domain_D12 AS (
    SELECT 
        domainNumb,
        ROUND(AVG(elevation), 4) AS mean_elevation,
        ROUND(STDDEV(elevation), 4) AS stddev_elevation,
        ROUND(STDDEV(elevation) / AVG(elevation) * 100, 4) AS cv_percentage
    FROM intersected_D12
    GROUP BY domainNumb
  );

COPY stats_domain_D12 TO '/mnt/nvme/geodiversity/output/stats_domain_D12.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE intersected_D13 AS (
    SELECT e.*, d.domainNumb
    FROM elevation_with_na e
    JOIN domain d
    ON ST_Intersects(e.geom, d.geom)
    WHERE d.domainNumb = 'D13'
  );

COPY intersected_domain_D13 TO '/mnt/nvme/geodiversity/output/intersected_domain_D13.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE stats_domain_D13 AS (
    SELECT 
        domainNumb,
        ROUND(AVG(elevation), 4) AS mean_elevation,
        ROUND(STDDEV(elevation), 4) AS stddev_elevation,
        ROUND(STDDEV(elevation) / AVG(elevation) * 100, 4) AS cv_percentage
    FROM intersected_D13
    GROUP BY domainNumb
  );

COPY stats_domain_D13 TO '/mnt/nvme/geodiversity/output/stats_domain_D13.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE intersected_D14 AS (
    SELECT e.*, d.domainNumb
    FROM elevation_with_na e
    JOIN domain d
    ON ST_Intersects(e.geom, d.geom)
    WHERE d.domainNumb = 'D14'
  );

COPY intersected_domain_D14 TO '/mnt/nvme/geodiversity/output/intersected_domain_D14.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE stats_domain_D14 AS (
    SELECT 
        domainNumb,
        ROUND(AVG(elevation), 4) AS mean_elevation,
        ROUND(STDDEV(elevation), 4) AS stddev_elevation,
        ROUND(STDDEV(elevation) / AVG(elevation) * 100, 4) AS cv_percentage
    FROM intersected_D14
    GROUP BY domainNumb
  );

COPY stats_domain_D14 TO '/mnt/nvme/geodiversity/output/stats_domain_D14.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE intersected_D15 AS (
    SELECT e.*, d.domainNumb
    FROM elevation_with_na e
    JOIN domain d
    ON ST_Intersects(e.geom, d.geom)
    WHERE d.domainNumb = 'D15'
  );

COPY intersected_domain_D15 TO '/mnt/nvme/geodiversity/output/intersected_domain_D15.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE stats_domain_D15 AS (
    SELECT 
        domainNumb,
        ROUND(AVG(elevation), 4) AS mean_elevation,
        ROUND(STDDEV(elevation), 4) AS stddev_elevation,
        ROUND(STDDEV(elevation) / AVG(elevation) * 100, 4) AS cv_percentage
    FROM intersected_D15
    GROUP BY domainNumb
  );

COPY stats_domain_D15 TO '/mnt/nvme/geodiversity/output/stats_domain_D15.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE intersected_D16 AS (
    SELECT e.*, d.domainNumb
    FROM elevation_with_na e
    JOIN domain d
    ON ST_Intersects(e.geom, d.geom)
    WHERE d.domainNumb = 'D16'
  );

COPY intersected_domain_D16 TO '/mnt/nvme/geodiversity/output/intersected_domain_D16.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE stats_domain_D16 AS (
    SELECT 
        domainNumb,
        ROUND(AVG(elevation), 4) AS mean_elevation,
        ROUND(STDDEV(elevation), 4) AS stddev_elevation,
        ROUND(STDDEV(elevation) / AVG(elevation) * 100, 4) AS cv_percentage
    FROM intersected_D16
    GROUP BY domainNumb
  );

COPY stats_domain_D16 TO '/mnt/nvme/geodiversity/output/stats_domain_D16.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE intersected_D17 AS (
    SELECT e.*, d.domainNumb
    FROM elevation_with_na e
    JOIN domain d
    ON ST_Intersects(e.geom, d.geom)
    WHERE d.domainNumb = 'D17'
  );

COPY intersected_domain_D17 TO '/mnt/nvme/geodiversity/output/intersected_domain_D17.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE stats_domain_D17 AS (
    SELECT 
        domainNumb,
        ROUND(AVG(elevation), 4) AS mean_elevation,
        ROUND(STDDEV(elevation), 4) AS stddev_elevation,
        ROUND(STDDEV(elevation) / AVG(elevation) * 100, 4) AS cv_percentage
    FROM intersected_D17
    GROUP BY domainNumb
  );

COPY stats_domain_D17 TO '/mnt/nvme/geodiversity/output/stats_domain_D17.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE intersected_D18 AS (
    SELECT e.*, d.domainNumb
    FROM elevation_with_na e
    JOIN domain d
    ON ST_Intersects(e.geom, d.geom)
    WHERE d.domainNumb = 'D18'
  );

COPY intersected_domain_D18 TO '/mnt/nvme/geodiversity/output/intersected_domain_D18.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE stats_domain_D18 AS (
    SELECT 
        domainNumb,
        ROUND(AVG(elevation), 4) AS mean_elevation,
        ROUND(STDDEV(elevation), 4) AS stddev_elevation,
        ROUND(STDDEV(elevation) / AVG(elevation) * 100, 4) AS cv_percentage
    FROM intersected_D18
    GROUP BY domainNumb
  );

COPY stats_domain_D18 TO '/mnt/nvme/geodiversity/output/stats_domain_D18.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE intersected_D19 AS (
    SELECT e.*, d.domainNumb
    FROM elevation_with_na e
    JOIN domain d
    ON ST_Intersects(e.geom, d.geom)
    WHERE d.domainNumb = 'D19'
  );

COPY intersected_domain_D19 TO '/mnt/nvme/geodiversity/output/intersected_domain_D19.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE stats_domain_D19 AS (
    SELECT 
        domainNumb,
        ROUND(AVG(elevation), 4) AS mean_elevation,
        ROUND(STDDEV(elevation), 4) AS stddev_elevation,
        ROUND(STDDEV(elevation) / AVG(elevation) * 100, 4) AS cv_percentage
    FROM intersected_D19
    GROUP BY domainNumb
  );

COPY stats_domain_D19 TO '/mnt/nvme/geodiversity/output/stats_domain_D19.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE intersected_D20 AS (
    SELECT e.*, d.domainNumb
    FROM elevation_with_na e
    JOIN domain d
    ON ST_Intersects(e.geom, d.geom)
    WHERE d.domainNumb = 'D20'
  );

COPY intersected_domain_D20 TO '/mnt/nvme/geodiversity/output/intersected_domain_D20.csv' WITH (HEADER, DELIMITER ',');

CREATE TABLE stats_domain_D20 AS (
    SELECT 
        domainNumb,
        ROUND(AVG(elevation), 4) AS mean_elevation,
        ROUND(STDDEV(elevation), 4) AS stddev_elevation,
        ROUND(STDDEV(elevation) / AVG(elevation) * 100, 4) AS cv_percentage
    FROM intersected_D20
    GROUP BY domainNumb
  );

COPY stats_domain_D20 TO '/mnt/nvme/geodiversity/output/stats_domain_D20.csv' WITH (HEADER, DELIMITER ',');
