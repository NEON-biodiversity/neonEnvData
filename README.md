# NEON Complementary Abiotic Data Layer Repository

## Table of Contents
- [Introduction](#Introduction)
- [Workflow](#Workflow)
- [Location of Data](#Location-of-data)
- [Spatiotemporal Extent and Resolution](#Spatiotemporal-extent-and-resolution)
- [Usage](#Usage)
- [File Naming Conventions](#File-naming-conventions)
- [Scripts](#Scripts)
- [Contributors](#Contributors)
- [Contact Information](#Contact-information)

## Introduction

This repository contains scripts and resources for analyzing abiotic data sources as complimentary data layers for [National Ecological Observatory (NEON)](https://www.neonscience.org/) research projects. Initial data sources include elevation data from NASA Shuttle Radar Topography Mission (SRTM) and temperature and precipitation data from the Climatologies at high resolution for the Earth's land surface areas (CHELSA). The repository contains code for extracting climate and elevation data across multiple spatial scales relevant to NEON (e.g., plot, site, domain).

## Workflow

The high-level workflow includes the following steps:
1. **Data Preprocessing**: Load and clean raw spatial data (polygons and SRTM rasters).
2. **Intersection and Reprojection**: Align and crop raster datasets to relevant spatial polygons.
3. **Metric Calculations**: Compute geodiversity metrics (e.g., roughness, slope, standard deviation) and mean/sd of climate biovars from processed rasters.
4. **Data Integration**: Combine climate and geodiversity data for each spatial unit.
5. **Visualization and Output**: Generate  plots and save processed datasets for further analysis.

## Location of Data

- **SRTM Data**: SRTM_gl1_v003 available from [NASA EarthData](https://search.earthdata.nasa.gov/)
- UDPATE **CHELSA Data**:
- **NEON Spatial Data**: NEON Domains, Terrestrial Sampling Boundaries, and Sites shapefiles downloaded from [NEON Spatial Data site](https://www.neonscience.org/data-samples/data/spatial-data-maps). 

## Spatiotemporal Extent and Resolution

- **Spatial Extent**: Variable: NEON plots, sites and domains across North America.
- **Spatial Resolution**: SRTM = ~30 m (1 arc second); CHELSA = ~1 km (30 arc seconds)
- **Temporal Extent**: Data include static geophysical variables (e.g., elevation) and climate data spanning historical periods (1981-2009).
- **Temporal Resolution**: Climate data processed as averages across years.

## Usage

The scripts in this repository require the following software:
- **R version**: 4.3.0 or higher
- **Required Libraries**:
  - `terra` (raster data manipulation)
  - `sf` (spatial vector data handling)
  - `dplyr` (data manipulation)
  - `geodiv` (geodiversity metric calculations)
  - `foreach` and `doParallel` (parallel processing)
  - `ggplot2` (visualization)

### File Naming Conventions

- **Scripts**: Scripts follow a clear naming convention of "stepNumber-dataSource_task.R". For example: 
  - `1_srtm_unzip_reproject.R`: Step 1 script takes SRTM data inputs, unzips them, and reprojects them. 
  
Scripts are stored follosing the [Environmental Data Initiative's L0 (raw data), L1 (processed data), and L2 (value added data) folder structure](https://edirepository.org/resources/designing-a-data-package). Scripts with an "x" at the beginning of the name are either not part of the data analysis pipeline or are in progress. 

## Scripts

### `1_srtm_unzip_reproject.R`

- **Purpose**: Script for cleaning and reprojecting geodiversity raster data
- **Inputs**: 
  - SRTM elevation data in zipped files (1 .hgt file per tile)
  - Shapefile of SRTM tiles
  - NEON domain data 
- **Outputs**: SRTM data cropped to extent of NEON domains and reprojected to EPSG:5070. Saved as .tif files. 

### `1-chelsa_crop_reproject.R`

- **Purpose**: This script crops and reprojects climate raster data in parallel
- **Inputs**:
  - CHELSA bioclimatic variables (annual mean values 1981-2009) stored as individual tif files (1 per variable)
- **Outputs**: CHELSA bioclimatic variables projected to EPSG:5070 and cropped to North America stored as individual tif files (1 per variable)

### `2-functions.R`

- **Purpose**: Defines utility functions for use in other step 2 scripts, including raster-polygon intersection and geodiversity metric calculations.
- **Inputs**: NA (functions are sourced in other scripts)
- **Outputs**: NA (functions are sourced in other scripts)

### `2-srtm_extract_polys.R`

- **Purpose**: Extract geodiversity metric values for NEON spatial data
- **Inputs**: 
  - SRTMGl3_v003 data processed in ./R/L0/1-srtm_unzip_reproject.R
  - Shapefile of SRTM tiles
  - NEON spatial data 
- **Outputs**: 
  - NEON spatial data frame with 1 column per geodiversity metric (saved as shapefile)

### `2-chelsa_extract_polys.R`

- **Purpose**: Extract biovar values for NEON spatial data
- **Inputs**: 
  - CHELSA climate rasters (output from 1-chelsa_crop_reproject.R)
  - NEON spatial data 
- **Outputs**: 
  - NEON spatial data frame with 1 column per geodiversity metric (saved as shapefile)
 

### `3-plot_site_rasters.R`

- **Purpose**: Visualizes elevation data for NEON sites by intersecting SRTM rasters with site polygons and generating elevation plots.
- **Inputs**:
  - UPDATE  NEON site polygons: `/mnt/scratch/plz-lab/geodiversity/spatial_data/polys_EPSG5070/NEON_sites.shp`
  - UPDATE  Raster files: `/mnt/scratch/plz-lab/geodiversity/SRTM_gl1_v003/tiles_EPSG5070`
- **Outputs**: PNG plots of site elevations, saved in `/mnt/scratch/plz-lab/geodiversity/output/figures/`.

## Contributors

- **Kelly Kapsar**
- **Pat Bills**
- **Phoebe Zarnetske**
- **Lala Kounta**

## Contact Information

For inquiries related to this repository, please contact:
- **Primary Contact**: Phoebe Zarnetske (plz@msu.edu)
- **Technical Questions**: Kelly Kapsar (kelly.kapsar@gmail.com)
