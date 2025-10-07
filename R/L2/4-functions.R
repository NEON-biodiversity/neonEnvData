#' Summarize Numeric Attributes of a Shapefile
#'
#' This function reads a shapefile and summarizes its numeric attributes
#' (excluding geometry), computing basic statistics including min, median,
#' max, count of NAs, count of zeros, and total non-NA values. It also appends
#' metadata based on the filename.
#'
#' @param file_path Character. Path to the shapefile (.shp).
#'
#' @return A tidy data frame with summary statistics and metadata.
#' 
#' @import sf
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @export
#'
#' @examples
#' \dontrun{
#' summarize_shapefile("data/shapes/NEON_plot_footprint.shp")
#' }
summarize_shapefile <- function(file_path) {
  message("Processing: ", file_path)
  
  # Safe versions of summary functions
  safe_min <- function(x) if (all(is.na(x))) NA else min(x, na.rm = TRUE)
  safe_max <- function(x) if (all(is.na(x))) NA else max(x, na.rm = TRUE)
  safe_median <- function(x) if (all(is.na(x))) NA else median(x, na.rm = TRUE)
  
  elev_res <- ifelse(str_detect(file_path, "30m"), 30, 300)
  
  shp <- st_read(file_path, quiet = TRUE)
  
  numeric_data <- shp %>% st_drop_geometry() %>% select(where(is.numeric))
  
  # Optional: remove columns with all NA values
  # numeric_data <- numeric_data %>% select(where(~ !all(is.na(.))))
  
  summary_stats <- numeric_data %>% 
    summarise(across(everything(), list(
      min = ~safe_min(.x),
      median = ~safe_median(.x),
      max = ~safe_max(.x),
      na_count = ~sum(is.na(.x)), 
      zero_count = ~sum(.x == 0, na.rm = TRUE),
      total_count = ~length(which(!is.na(.x)))
    ), .names = "{.col}_{.fn}"))
  
  summary_tidy <- summary_stats %>%
    pivot_longer(
      cols = everything(),
      names_to = c("var_stat", "measure"),
      names_pattern = "^(.*)_(min|median|max|na_count|zero_count|total_count)$"
    ) %>%
    separate(var_stat, into = c("variable", "stat"), sep = "_(?=[^_]+$)") %>%
    pivot_wider(
      names_from = measure,
      values_from = value
    )
  
  file_name <- basename(file_path)
  
  centroid <- case_when(
    str_detect(file_name, "tower") ~ "tower",
    str_detect(file_name, "mammal") ~ "small mammal",
    str_detect(file_name, "footprint") ~ "footprint",
    TRUE ~ NA_character_
  )
  
  scale <- case_when(
    str_detect(file_name, "plot") ~ "plot",
    str_detect(file_name, "site") ~ "site",
    str_detect(file_name, "domain") ~ "domain",
    TRUE ~ NA_character_
  )
  
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

#' Tag and Copy a GPKG File Based on Source Folder
#'
#' This function identifies whether a `.gpkg` file is located in the 30m or 300m
#' elevation data directory, appends a suffix (`_elev30m` or `_elev300m`) to the filename,
#' and copies the file to a specified output folder.
#'
#' @param gpkg sf object. 
#' @param gpkg_path Character. Full file path to the original `.gpkg` file.
#' @param output_folder Character. Destination directory where the renamed file will be saved.
#'
#' @return None. The renamed `.gpkg` file is copied to the output folder.
#'
#' @details
#' Recognizes the following source directories:
#' - `/mnt/research/neon/neonEnvData/L2/clim_elev_30m/`
#' - `/mnt/research/neon/neonEnvData/L2/clim_elev_300m/`
#'
#' If the file is not in either directory, the function stops with an error.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tag_and_copy_gpkg(gpkg, 
#'   gpkg_path = "/mnt/research/neon/neonEnvData/L2/clim_elev_30m/site_data.gpkg",
#'   output_folder = "processed_data"
#' )
#' }
tag_and_copy_gpkg <- function(gpkg, gpkg_path, output_folder) {

  # Determine folder source
  elev_suffix <- if (grepl("/clim_elev_30m/", gpkg_path)) {
    "_elev30m"
  } else if (grepl("/clim_elev_300m/", gpkg_path)) {
    "_elev300m"
  } else {
    stop("File is not located in a recognized elevation directory.")
  }
  
  # Extract file name without extension
  file_base <- tools::file_path_sans_ext(basename(gpkg_path))
  
  # New file name with suffix
  new_filename <- paste0(file_base, elev_suffix, ".gpkg")
  
  # Ensure output folder exists
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  # Full path for output file
  output_path <- file.path(output_folder, new_filename)
  
  # Save output
  st_write(gpkg, output_path, append=F)
  
  message("File saved to: ", output_path)
}




#' Clean Columns Containing Only 0 and NA Values in a GPKG File
#'
#' This function reads a `.gpkg` file using `sf`, identifies **numeric** columns that contain
#' only `0` and `NA` values, replaces those `0`s with `NA`, and returns the cleaned `sf` object.
#'
#' @param original_file Character. Path to the original GPKG file.
#'
#' @return Cleaned `sf` object with specified values replaced.
#' @export
#'
#' @examples
#' \dontrun{
#' clean_zero_na_columns("data/raw_data.gpkg")
#' }
clean_zero_na_columns <- function(original_file) {
  if (!file.exists(original_file)) {
    stop("Original file does not exist.")
  }
  
  print(paste0("Processing ", original_file))
  
  # Read as sf
  df <- sf::st_read(original_file, stringsAsFactors = FALSE, quiet = TRUE)
  
  if(any(str_detect(colnames(df), "domainNumb"))){
    df <- df %>% rename(domainID = domainNumb)
    if(any(str_detect(colnames(df), "domainName"))){
      df <- df %>% dplyr::select(-domainName)
    }
  }
  
  # Drop geometry to isolate attribute data
  attr_data <- sf::st_drop_geometry(df)
  
  # Function: TRUE if a numeric column has only 0s or NAs
  is_zero_na_only <- function(col) {
    all(is.na(col) | col == 0)
  }
  
  # Identify numeric columns first
  numeric_cols <- sapply(attr_data, is.numeric)
  
  # Names of numeric columns
  numeric_col_names <- names(attr_data)[numeric_cols]
  
  # Apply is_zero_na_only to only numeric columns
  zero_na_flags <- sapply(attr_data[numeric_col_names], is_zero_na_only)
  
  # Subset names using that logical vector
  zero_na_cols <- numeric_col_names[zero_na_flags]
  
  print(paste0(length(zero_na_cols), " numeric columns with only 0/NA values found."))
  
  # Replace 0s with NA in those columns safely
  if (length(zero_na_cols) != 0) {
    for (col_name in zero_na_cols) {
      df[[col_name]][df[[col_name]] == 0] <- NA
    }
  }
      
  return(df)
}


#' Insert XML Metadata into a GeoPackage File
#'
#' This function injects ISO 19115-style XML metadata into the `gpkg_metadata` and
#' `gpkg_metadata_reference` tables of a GeoPackage (`.gpkg`) file. It's designed
#' for bulk or automated metadata population across multiple GeoPackage datasets.
#'
#' @param gpkg_path Character. Full path to the GeoPackage file (.gpkg).
#' @param xml_path Character. Full path to the XML metadata file to be embedded.
#' @param file_id Integer. Unique identifier for the metadata entry in the GeoPackage. Default is `1`.
#' @param table_name Character or `NULL`. Optional name of the table/layer that the metadata references.
#' If `NULL`, metadata will be associated with the entire dataset.
#'
#' @details
#' This function writes the provided XML content to the `gpkg_metadata` table and then
#' creates a reference in `gpkg_metadata_reference`. It uses the standard MIME type
#' `"text/xml"` and assumes conformance to the ISO 19115 metadata model.
#'
#' If the metadata ID already exists, it will be replaced (`INSERT OR REPLACE` logic).
#'
#' @return No return value. The function performs database operations as a side effect.
#'
#' @importFrom DBI dbConnect dbExecute dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom glue glue
#' @importFrom readr read_file
#'
#' @examples
#' \dontrun{
#' insert_metadata(
#'   gpkg_path = "data/climate_regions.gpkg",
#'   xml_path = "metadata/climate_regions.xml",
#'   file_id = 1,
#'   table_name = "regions_layer"
#' )
#' }
#'
#' @export
insert_metadata <- function(gpkg_path, xml_path, file_id = 1, table_name = NULL) {
  # Read XML metadata
  xml_metadata <- read_file(xml_path)
  
  # Connect to GeoPackage
  con <- dbConnect(RSQLite::SQLite(), gpkg_path)
  
  # Check and create gpkg_metadata table if missing
  dbExecute(con, "
  CREATE TABLE IF NOT EXISTS gpkg_metadata (
    id INTEGER NOT NULL PRIMARY KEY,
    md_scope TEXT NOT NULL DEFAULT 'dataset',
    md_standard_uri TEXT NOT NULL,
    mime_type TEXT NOT NULL DEFAULT 'text/xml',
    metadata TEXT NOT NULL
  );
")
  
  # Check and create gpkg_metadata_reference table if missing
  dbExecute(con, "
  CREATE TABLE IF NOT EXISTS gpkg_metadata_reference (
    reference_scope TEXT NOT NULL,
    table_name TEXT,
    column_name TEXT,
    row_id_value INTEGER,
    timestamp DATETIME NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
    md_file_id INTEGER NOT NULL,
    md_parent_id INTEGER,
    CONSTRAINT crmr_mfi_fk FOREIGN KEY (md_file_id) REFERENCES gpkg_metadata(id)
  );
")
  
  
  # Insert metadata into gpkg_metadata
  dbExecute(con, glue("
    INSERT OR REPLACE INTO gpkg_metadata
      (id, md_scope, md_standard_uri, mime_type, metadata)
    VALUES
      ({file_id}, 'dataset', 'http://www.isotc211.org/2005/gmd', 'text/xml', ?)
  "), params = list(xml_metadata))
  
  # Insert reference (table_name is optional)
  ref_scope <- ifelse(is.null(table_name), "dataset", "table")
  
  dbExecute(con, glue("
    INSERT OR REPLACE INTO gpkg_metadata_reference
      (reference_scope, table_name, column_name, row_id_value, timestamp, md_file_id, md_parent_id)
    VALUES
      ('{ref_scope}', {ifelse(is.null(table_name), 'NULL', paste0(\"'\", table_name, \"'\"))}, NULL, NULL, CURRENT_TIMESTAMP, {file_id}, NULL)
  "))
  
  dbDisconnect(con)
}

