# NEON Geodiversity Analysis Repository

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

This repository contains scripts and resources for analyzing geodiversity data as part of the NEON Geodiversity Analysis project. The project focuses on processing, integrating, and analyzing geospatial and climatic datasets to understand geodiversity patterns across NEON domains. The scripts facilitate data cleaning, geodiversity metric calculations, and integration of climate and elevation data, supporting broader biodiversity research goals.

## Workflow

The high-level workflow includes the following steps:
1. **Data Preprocessing**: Load and clean raw spatial data (polygons and SRTM rasters).
2. **Intersection and Reprojection**: Align and crop raster datasets to relevant spatial polygons.
3. **Metric Calculations**: Compute geodiversity metrics (e.g., roughness, slope, standard deviation) from processed rasters.
4. **Data Integration**: Combine climate and geodiversity data for each spatial unit.
5. **Visualization and Output**: Generate elevation plots and save processed datasets for further analysis.

## Location of Data

Processed and intermediate datasets are stored on the MSU HPCC cluster in the following directories:
- **Spatial Data**: `/mnt/scratch/plz-lab/geodiversity/spatial_data/`
- **Processed Outputs**: `/mnt/scratch/plz-lab/geodiversity/output/`

## Spatiotemporal Extent and Resolution

- **Spatial Extent**: NEON sites across North America.
- **Spatial Resolution**: Varies depending on the input data, typically 30m for SRTM rasters.
- **Temporal Extent**: Data include static geophysical variables (e.g., elevation) and climate data spanning historical periods (e.g., 1950-2010).
- **Temporal Resolution**: Climate data processed as averages or summaries at relevant temporal scales.

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

- **Data Files**: Files are named to reflect their content and processing step, e.g., `site_radii_clim_elev.shp` for polygons integrated with climate and elevation data.
- **Scripts**: Scripts follow a clear naming convention:
  - `L1-functions.R`: Shared functions for intersection and metric calculations.
  - `process_polygons.R`: Script for polygon processing and geodiversity metric calculations.

## Scripts

### `L1-functions.R`

- **Purpose**: Defines utility functions for spatial data processing, including raster-polygon intersection and geodiversity metric calculations.
- **Inputs**: Spatial polygons (e.g., NEON site boundaries) and raster datasets (e.g., SRTM elevation data).
- **Outputs**: Processed rasters or calculated metrics, depending on the function.

### `process_polygons.R`

- **Purpose**: Processes spatial polygons to calculate geodiversity metrics and integrate with climate data.
- **Inputs**:
  - Polygons: `/mnt/scratch/plz-lab/geodiversity/spatial_data/polys_EPSG5070/`
  - Rasters: `/mnt/scratch/plz-lab/geodiversity/SRTM_gl1_v003/tiles_EPSG5070`
- **Outputs**: Shapefiles with geodiversity and climate metrics, saved to `/mnt/scratch/plz-lab/geodiversity/output/polys_EPSG5070_intersected/`.

### `plot_site_elevations.R`

- **Purpose**: Visualizes elevation data for NEON sites by intersecting SRTM rasters with site polygons and generating elevation plots.
- **Inputs**:
  - NEON site polygons: `/mnt/scratch/plz-lab/geodiversity/spatial_data/polys_EPSG5070/NEON_sites.shp`
  - Raster files: `/mnt/scratch/plz-lab/geodiversity/SRTM_gl1_v003/tiles_EPSG5070`
- **Outputs**: PNG plots of site elevations, saved in `/mnt/scratch/plz-lab/geodiversity/output/figures/`.

## Contributors

- **Kelly Kapsar**
- **Pat Bills**
- **Phoebe Zarnetske**
- **Lala Kounta**

## Contact Information

For inquiries related to this repository, please contact:
- **Primary Contact**: [Insert primary contact name and email]
- **Technical Questions**: [Insert technical contact name and email]
