library(shiny)
library(sf)
library(leaflet)
library(dplyr)
library(viridis)


# t <- st_read("/mnt/research/neon/neonEnvData/L2/clim_elev_300m/NEON_mammal_site_radii.gpkg") %>% 
#   st_transform(4326) %>% 
#   st_write("/mnt/research/neon/neonEnvData/L2/clim_elev_4326/NEON_mammal_site_radii_srtm300.gpkg")
# 
# t <- st_read("/mnt/research/neon/neonEnvData/L2/clim_elev_300m/NEON_mammal_domain_radii.gpkg")%>% 
#   st_transform(4326) %>% 
#   st_write("/mnt/research/neon/neonEnvData/L2/clim_elev_4326/NEON_mammal_domain_radii_srtm300.gpkg")
# 
# t <- st_read("/mnt/research/neon/neonEnvData/L2/clim_elev_300m/NEON_domain_footprint.gpkg") %>% 
#   st_transform(4326) %>% 
#   st_write("/mnt/research/neon/neonEnvData/L2/clim_elev_4326/NEON_domain_footprint_srtm300.gpkg")
# 
# t <- st_read("/mnt/research/neon/neonEnvData/L2/clim_elev_300m/NEON_site_footprint.gpkg") %>% 
#   st_transform(4326) %>% 
#   st_write("/mnt/research/neon/neonEnvData/L2/clim_elev_4326/NEON_site_footprint_srtm300.gpkg")
# 
# t <- st_read("/mnt/research/neon/neonEnvData/L2/clim_elev_300m/NEON_tower_domain_radii.gpkg") %>% 
#   st_transform(4326) %>% 
#   st_write("/mnt/research/neon/neonEnvData/L2/clim_elev_4326/NEON_tower_domain_radii_srtm300.gpkg")
# 
# 
# t <- st_read("/mnt/research/neon/neonEnvData/L2/clim_elev_300m/NEON_tower_site_radii.gpkg") %>% 
#   st_transform(4326) %>% 
#   st_write("/mnt/research/neon/neonEnvData/L2/clim_elev_4326/NEON_tower_site_srtm300.gpkg")
# 
# 
# t <- st_read("/mnt/research/neon/neonEnvData/L2/clim_elev_30m/NEON_mammal_plot_radii.gpkg") %>% 
#   st_transform(4326) %>% 
#   st_write("/mnt/research/neon/neonEnvData/L2/clim_elev_4326/NEON_mammal_plot_radii_srtm30.gpkg")
# 
# 
# t <- st_read("/mnt/research/neon/neonEnvData/L2/clim_elev_30m/NEON_mammal_site_radii.gpkg") %>% 
#   st_transform(4326) %>% 
#   st_write("/mnt/research/neon/neonEnvData/L2/clim_elev_4326/NEON_mammal_site_radii_srtm30.gpkg")
# 
# t <- st_read("/mnt/research/neon/neonEnvData/L2/clim_elev_30m/NEON_mammal_domain_radii.gpkg") %>% 
#   st_transform(4326) %>% 
#   st_write("/mnt/research/neon/neonEnvData/L2/clim_elev_4326/NEON_mammal_domain_radii_srtm30.gpkg")
# 
# t <- st_read("/mnt/research/neon/neonEnvData/L2/clim_elev_30m/NEON_site_footprint.gpkg") %>% 
#   st_transform(4326) %>% 
#   st_write("/mnt/research/neon/neonEnvData/L2/clim_elev_4326/NEON_site_footrpint_srtm30.gpkg")
# 
# t <- st_read("/mnt/research/neon/neonEnvData/L2/clim_elev_30m/NEON_tower_plot_radii.gpkg") %>% 
#   st_transform(4326) %>% 
#   st_write("/mnt/research/neon/neonEnvData/L2/clim_elev_4326/NEON_tower_plot_radii_srtm30.gpkg")
# 
# t <- st_read("/mnt/research/neon/neonEnvData/L2/clim_elev_30m/NEON_tower_site_radii.gpkg") %>% 
#   st_transform(4326) %>% 
#   st_write("/mnt/research/neon/neonEnvData/L2/clim_elev_4326/NEON_tower_site_radii_srtm30.gpkg")
# 
# t <- st_read("/mnt/research/neon/neonEnvData/L2/clim_elev_30m/NEON_tower_domain_radii.gpkg") %>% 
#   st_transform(4326) %>% 
#   st_write("/mnt/research/neon/neonEnvData/L2/clim_elev_4326/NEON_tower_domain_radii_srtm30.gpkg")


# List of shapefiles
shapefile_paths <- list(
  "Mammal Site Radii" = "/mnt/research/neon/neonEnvData/L2/clim_elev_4326/NEON_mammal_site_radii_srtm300.gpkg",
  "Mammal Domain Radii" = "/mnt/research/neon/neonEnvData/L2/clim_elev_4326/NEON_mammal_domain_radii_srtm300.gpkg",
  "Domain Footprint" = "/mnt/research/neon/neonEnvData/L2/clim_elev_4326/NEON_domain_footprint_srtm300.gpkg",
  "Site Footprint" = "/mnt/research/neon/neonEnvData/L2/clim_elev_4326/NEON_site_footprint_srtm300.gpkg",
  "Tower Site Radii" = "/mnt/research/neon/neonEnvData/L2/clim_elev_4326/NEON_tower_site_radii_srtm300.gpkg",
  "Tower Domain Radii" = "/mnt/research/neon/neonEnvData/L2/clim_elev_4326/NEON_tower_domain_radii_srtm300.gpkg",
  
  
  "Mammal Plot Radii" = "/mnt/research/neon/neonEnvData/L2/clim_elev_4326/NEON_mammal_plot_radii_srtm30.gpkg",
  "Mammal Site Radii (Duplicate)" = "/mnt/research/neon/neonEnvData/L2/clim_elev_4326/NEON_mammal_site_radii_srtm30.gpkg",
  "Mammal Domain Radii (Duplicate)" = "/mnt/research/neon/neonEnvData/L2/clim_elev_4326/NEON_mammal_domain_radii_srtm30.gpkg",
  "Site Footprint (Duplicate)" = "/mnt/research/neon/neonEnvData/L2/clim_elev_4326/NEON_site_footprint_srtm30.gpkg",
  "Tower Plot Radii" = "/mnt/research/neon/neonEnvData/L2/clim_elev_4326/NEON_tower_plot_radii_srtm30.gpkg",
  "Tower Site Radii (Duplicate)" = "/mnt/research/neon/neonEnvData/L2/clim_elev_4326/NEON_tower_site_radii_srtm30.gpkg",
  "Tower Domain Radii (Duplicate)" = "/mnt/research/neon/neonEnvData/L2/clim_elev_4326/NEON_tower_domain_radii_srtm30.gpkg"
)

# UI
ui <- fluidPage(
  titlePanel("NEON Shapefile Visualizer"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("selected_files", "Select Shapefiles to Display",
                         choices = names(shapefile_paths)),
      uiOutput("attribute_selectors")
    ),
    mainPanel(
      leafletOutput("map", height = "800px"),
      hr(),
      uiOutput("histograms")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Load and transform shapefiles to EPSG:4326
  shapefiles <- reactive({
    lapply(shapefile_paths, function(path) {
      tryCatch({
        sf_obj <- st_read(path, quiet = TRUE)
        if (st_crs(sf_obj)$epsg != 4326) {
          sf_obj <- st_transform(sf_obj, 4326)
        }
        sf_obj
      }, error = function(e) NULL)
    })
  })
  
  # Dynamic column selectors
  output$attribute_selectors <- renderUI({
    req(input$selected_files)
    sf_list <- shapefiles()
    selected_ui <- lapply(input$selected_files, function(name) {
      sf_obj <- sf_list[[name]]
      if (is.null(sf_obj)) return(NULL)
      selectInput(paste0("col_", name), 
                  label = paste("Color by column (", name, "):"),
                  choices = names(sf_obj)[sapply(sf_obj, function(x) is.numeric(x) | is.factor(x))])
    })
    do.call(tagList, selected_ui)
  })
  
  # Render base map
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -98.5, lat = 39.5, zoom = 4)
  })
  
  # Add shapefiles to map with coloring and legend
  observe({
    req(input$selected_files)
    leafletProxy("map") %>% clearShapes() %>% clearControls()
    sf_list <- shapefiles()
    
    for (name in input$selected_files) {
      sf_obj <- sf_list[[name]]
      colname <- input[[paste0("col_", name)]]
      if (is.null(sf_obj) || is.null(colname)) next
      
      values <- sf_obj[[colname]]
      pal <- colorNumeric("viridis", domain = values, na.color = "transparent")
      
      if (any(grepl("POLYGON", st_geometry_type(sf_obj)))) {
        leafletProxy("map") %>%
          addPolygons(data = sf_obj,
                      color = ~pal(values),
                      weight = 1,
                      opacity = 0.8,
                      fillOpacity = 0.5,
                      popup = ~paste(colname, "=", values),
                      group = name)
      } else {
        leafletProxy("map") %>%
          addCircleMarkers(data = sf_obj,
                           color = ~pal(values),
                           radius = 4,
                           stroke = TRUE,
                           fillOpacity = 0.7,
                           popup = ~paste(colname, "=", values),
                           group = name)
      }
      
      bbox <- st_bbox(sf_obj)
      leafletProxy("map") %>% fitBounds(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])
      
      # Add legend
      leafletProxy("map") %>%
        addLegend(position = "bottomright",
                  pal = pal,
                  values = values,
                  title = paste0(name, ": ", colname),
                  layerId = paste0("legend_", name),
                  opacity = 1)
    }
  })
  
  # Render histograms per selected layer
  output$histograms <- renderUI({
    req(input$selected_files)
    plots <- lapply(input$selected_files, function(name) {
      output_id <- paste0("hist_", name)
      plotOutput(output_id, height = "200px")
    })
    do.call(tagList, plots)
  })
  
  observe({
    req(input$selected_files)
    sf_list <- shapefiles()
    
    for (name in input$selected_files) {
      local({
        layer_name <- name
        sf_obj <- sf_list[[layer_name]]
        colname <- input[[paste0("col_", layer_name)]]
        output_id <- paste0("hist_", layer_name)
        
        output[[output_id]] <- renderPlot({
          req(sf_obj, colname)
          vals <- sf_obj[[colname]]
          hist(vals,
               main = paste("Histogram of", colname, "\n(", layer_name, ")"),
               xlab = colname,
               col = "steelblue",
               border = "white")
        })
      })
    }
  })
}

# Run app
shinyApp(ui, server)