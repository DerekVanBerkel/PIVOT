

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(tmap)
library(zip)
library(mapview)
library(shinyscreenshot)
library(shinydashboard)
library(tidyverse)
library(shinyWidgets)
library(rgdal)
library(htmltools)
library(DT)
library(readr)
library(formattable)

options(shiny.maxRequestSize=1000000000) 


#ppgis <- function{data = ,  }

# #load shapefile
# VECTOR_FILE <- st_read(system.file("shape/nc.shp", package="sf")) %>%
#   dplyr::mutate(PPGIS_CODE = as.character(row_number()),SELECTED = NA) %>% 
#   dplyr::select(PPGIS_CODE, SELECTED, geometry) %>% ## everything()
#   sf::st_transform(4326)

# 
# base_map_bounds <- VECTOR_FILE %>% 
#   st_bbox() %>% 
#   as.character()



# Creates base map
# Reference coordinates for center of Grand Rapids
createMap <- function() {  
  m <- leaflet() %>%
    # addProviderTiles(providers$nlmaps.water) %>%
    addProviderTiles(providers$CartoDB.VoyagerNoLabels) %>%
    fitBounds(base_map_bounds[1], base_map_bounds[2], base_map_bounds[3], base_map_bounds[4])
  return(m)
}



#Land Use categories and corresponding colors
Land_Use_Categories<- c('Residential', 'Commercial', 'Industrial', 'Institutional', 'Recreational')
landuse_cat <- data.frame(Land_Use_Categories)
color_palette_list = c("#ffff99", "#e31a1c", "#6a3d9a", "#a6cee3", "#b2df8a")
landuse_pallete <- colorBin(palette = color_palette_list, domain=1:length(Land_Use_Categories), na.color = "#FFFFFF00")
###use updateradiobutton, and text input https://shiny.rstudio.com/reference/shiny/0.14/updateRadioButtons.html

# Define UI for application that draws a histogram


ui <- dashboardPage(
  dashboardHeader(title = "Pivot ", titleWidth = 250),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      radioButtons("Land_Use_Cat", label = h3("Radio buttons"),
                   choices = list("Residential" = 1,  "Commercial" = 2,    "Industrial" = 3,    "Institutional" = 4, "Recreational" = 5 ), 
                   selected = 1),
      downloadLink("download_shp", "Download Map"),
      
      hr(),
      
      fileInput("filemap", 
                "Choose shapefile file type",
                multiple = TRUE,
                accept = c(".shp",".dbf",".sbn",".sbx",".shx",".prj", ".gpkg", ".geojson")), 

      actionButton("clear_map", "Reload Map"),
      HTML(paste0(
        "<br>",
        "<a href='https://seas.umich.edu/' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='https://commons.wikimedia.org/wiki/File:UM_SEAS_Logo.png' width = '186'></a>",
        "<br>")),
      
      
      
      HTML(paste0(
        "<br><br><br><br><br><br><br><br><br>",
        "<table style='margin-left:auto; margin-right:auto;'>",
        "<tr>",
        "<td style='padding: 5px;'><a href='https://scholar.google.com/citations?user=ZXsytH8AAAAJ&hl=en' target='_blank'><i class='fab fa-google fa-lg'></i></a></td>",
        "<td style='padding: 5px;'><a href='https://www.youtube.com/nationalparkservice' target='_blank'><i class='fab fa-youtube fa-lg'></i></a></td>",
        "<td style='padding: 5px;'><a href='https://twitter.com/derekvanberkel' target='_blank'><i class='fab fa-twitter fa-lg'></i></a></td>",
        "<td style='padding: 5px;'><a href='https://www.instagram.com/nationalparkservice' target='_blank'><i class='fab fa-instagram fa-lg'></i></a></td>",
        "<td style='padding: 5px;'><a href='https://www.flickr.com/nationalparkservice' target='_blank'><i class='fab fa-flickr fa-lg'></i></a></td>",
        "</tr>",
        "</table>",
        "<br>")),
      HTML(paste0(
        "<script>",
        "var today = new Date();",
        "var yyyy = today.getFullYear();",
        "</script>",
        "<p style = 'text-align: center;'><small>&copy; - <a href='https://seas.umich.edu/research/faculty/derek-van-berkel' target='_blank'>DerekVanBerkel.com</a> - <script>document.write(yyyy);</script></small></p>",
        "<p style = 'text-align: center;'><small>&copy; - <a href='https://www.linkedin.com/in/rahul-agrawal-bejarano-5b395774/' target='_blank'>RahulAgrawalBejarano.com</a> - <script>document.write(yyyy);</script></small></p>"))
    )
  ),
  dashboardBody(
    fluidRow(
      column(12,leafletOutput('PPGISmap', width='100%', height='850')),actionButton("go", "Take a screenshot"))
  )
)


server <- function(input, output) {
  
  observeEvent(input$clear_map, {  # reload all map contents with whatever file was uploaded
    
    if (is.null(input$filemap)) {  # if no upload, use default NC
      VECTOR_FILE <<- st_read(system.file("shape/nc.shp", package="sf")) %>% 
        dplyr::mutate(PPGIS_CODE = as.character(row_number()),SELECTED = NA) %>% 
        dplyr::select(PPGIS_CODE, SELECTED, geometry) %>% ## everything()
        sf::st_transform(4326)
      is_selected <<- NA
      print(summary(VECTOR_FILE))
      print(colnames(VECTOR_FILE))
      
      base_map_bounds <<- VECTOR_FILE %>% 
        st_bbox() %>% 
        as.character()
    } 
    else {
      if(length(input$filemap$datapath ) > 1){  # if shapefile
        # the upload gives the files unique names, so read_sf won't recognize them as belonging to the same shapefile
        for (path in input$filemap$datapath){
          newpath <- sub('.\\.', 'shapefile.', path)
          file.copy(path, newpath)  # create new set of properly named files
        }
        
        # locate which file is the .shp file to feed
        shppth_idx <- which(str_detect(input$filemap$datapath, '\\.shp'))
        shppth <- sub('.\\.', 'shapefile.', input$filemap$datapath[shppth_idx])
        
        VECTOR_FILE <<- st_read(shppth) %>%
          dplyr::mutate(PPGIS_CODE = as.character(row_number()), SELECTED = NA) %>% 
          dplyr::select(PPGIS_CODE, SELECTED, geometry) %>% ## everything()
          sf::st_transform(4326)
      }
      else{ # if not a shapefile, proceed as normal
        VECTOR_FILE <<- st_read(input$filemap$datapath) %>%
          dplyr::mutate(PPGIS_CODE = as.character(row_number()), SELECTED = NA) %>% 
          dplyr::select(PPGIS_CODE, SELECTED, geometry) %>% ## everything()
          sf::st_transform(4326)
      }
      
      
      is_selected <<- NA
      print(summary(VECTOR_FILE))
      print(colnames(VECTOR_FILE))
      print(VECTOR_FILE$PPGIS_CODE)
      
      base_map_bounds <<- VECTOR_FILE %>% 
        st_bbox() %>% 
        as.character()
    }
    
    output$PPGISmap <- renderLeaflet({
      createMap() %>%
        addPolygons(
          data=VECTOR_FILE,
          layerId=~PPGIS_CODE,
          #group='base_polygons',
          weight=1,
          fillOpacity=0
        ) %>%
        addTiles(group = "OSM (default)") %>%
        addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
        addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
        # Overlay groups
        # addCircles(~long, ~lat, ~10^mag/5, stroke = F, group = "Quakes") %>%
        # addPolygons(data = outline, lng = ~long, lat = ~lat,
        #             fill = F, weight = 2, color = "#FFFFCC", group = "Outline") %>%
        # Layers control
        addLayersControl(
          baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
          # overlayGroups = c("Quakes", "Outline"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        addLegend(
          # pal=landuse_pallete,
          values=landuse_cat$Land_Use_Categories,
          position='bottomleft',
          title="Legend of Landuse Categories",
          opacity=0.6,
          colors = color_palette_list,
          labels = Land_Use_Categories
        )
    })
    
  })
  
  observeEvent(input$go, {
    screenshot(id="PPGISmap")
  })
  
  
  
  # just testing here
  observeEvent(input$PPGISmap_shape_click, {
    polygon_clicked <- input$PPGISmap_shape_click
    
    if (is.null(polygon_clicked)) { return() }
    
    row_idx <- which(VECTOR_FILE$PPGIS_CODE == polygon_clicked$id)
    
    print('polygon_clicked:')
    print(polygon_clicked$id)
    print('row_idx:')
    print(row_idx)
    
    is_selected <- VECTOR_FILE[row_idx, ]$SELECTED  
    
    print(is_selected)
    
    if (!is.na(is_selected)) { # if polygon is already selected
      
      VECTOR_FILE[row_idx, ]$SELECTED <<- NA # zeros out polygon selected value
      
      # isolates polygon that needs to be redrawn
      VECTOR_FILE_selected <- VECTOR_FILE[row_idx, ]
      
      
      # redraws polygon without any color (base settings)
      leafletProxy(mapId='PPGISmap') %>%
        removeShape(VECTOR_FILE[row_idx, ]$PPGIS_CODE) %>%
        addPolygons(
          data=VECTOR_FILE_selected,
          layerId=~PPGIS_CODE,
          weight=1,
          fillOpacity=0#,
          #fillColor = #FFFFFF00
        ) 
      
      #print(VECTOR_FILE_selected)
    }
    else { # if polygon is not selected
      landuse_palette_code_selected <- as.numeric(input$Land_Use_Cat)
      #print(landuse_palette_code_selected)
      
      # Get current table selected
      #row_clicked <- input$groups_table_cell_clicked
      
      # substitutes selected value for polygon with group number
      VECTOR_FILE[row_idx, ]$SELECTED <<- landuse_palette_code_selected
      
      #isolates polygon that needs to be redrawn
      VECTOR_FILE_selected <- VECTOR_FILE[row_idx, ]
      
      
      # redraws polygon with correct color (defined by global palette)
      leafletProxy(mapId='PPGISmap') %>%
        removeShape(VECTOR_FILE[row_idx, ]$PPGIS_CODE) %>%
        addPolygons(
          data=VECTOR_FILE,
          layerId=~PPGIS_CODE,
          weight=1,
          fillOpacity=0.5,
          fillColor = ~landuse_pallete(SELECTED)
        )
      print(VECTOR_FILE$SELECTED)
    }
    
    
    
    
    
    output$download_shp <- downloadHandler(
      filename <- function() {
        "Data_shpExport.zip"
        
      },
      content = function(file) {
        withProgress(message = "Exporting Data", {
          
          
          FILE <- VECTOR_FILE
          FILE$SELECTED[is.na(FILE$SELECTED)] <- "NONE"
          
          incProgress(0.5)
          tmp.path <- dirname(file)
          
          name.base <- file.path(tmp.path, "PivotOutput")
          name.glob <- paste0(name.base, ".*")
          name.shp  <- paste0(name.base, ".shp")
          name.zip  <- paste0(name.base, ".zip")
          
          if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
          sf::st_write(FILE, dsn = name.shp, ## layer = "shpExport",
                       driver = "ESRI Shapefile", quiet = TRUE)
          
          zip::zipr(zipfile = name.zip, files = Sys.glob(name.glob))
          req(file.copy(name.zip, file))
          
          if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
          
          incProgress(0.5)
        })
      }  
    )
    
    
  })
  
  
  
  
  
  
}

shinyApp(ui, server)




shinyApp(ui, server)


