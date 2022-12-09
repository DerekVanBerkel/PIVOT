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
library(raster)
library(dashboardthemes)
library(fontawesome)
library(shinyBS)
library(shinyjs)


options(shiny.maxRequestSize=1000000000) 
values <- c("No Category" = NA)
bmap_fields <- NULL
user_basemap <- NULL
basemap_type <- 'None'
basemap_groups <<-c("OSM (default)", "Toner", "Toner Lite", "Open Topo Map", "ESRI World Imagery")


theme_blue_gradient <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Helvetica"
  ,appFontColor = "rgb(0,0,0)"
  ,primaryFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(248,248,248)"
  
  ### header
  ,logoBackColor = "rgb(30,47,68)"
  
  ,headerButtonBackColor = "rgb(30,47,68)"
  ,headerButtonIconColor = "rgb(255,255,255)"
  ,headerButtonBackColorHover = "rgb(210,210,210)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = "rgb(30,47,68)"
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "2px 2px 2px"
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(255,255,255)"
    ,colorMiddle = "rgb(235,237,240)"
    ,colorEnd = "rgb(64,64,65)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(55,72,80)"
  
  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(35,106,135)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(44,222,235,1)"
    ,colorMiddle = "rgba(44,222,235,1)"
    ,colorEnd = "rgba(0,255,213,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(44,222,235,1)"
    ,colorMiddle = "rgba(44,222,235,1)"
    ,colorEnd = "rgba(0,255,213,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "rgb(50,50,50)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)

js <- '
$(document).ready(function(){
  $("[id^=sw-content-]").on("shown", function(){
    $(".sidebar").css({"overflow-y": "visible"});
  }).on("hidden", function(){
    $(".sidebar").css({"overflow-y": "auto"});
  });
});
'

#ppgis <- function{data = ,  }

# #load shapefile
VECTOR_FILE <<- st_read(system.file("shape/nc.shp", package="sf")) %>%
  dplyr::mutate(PPGIS_CODE = as.character(row_number()),SELECTED = NA) %>%
  dplyr::select(PPGIS_CODE, SELECTED, geometry) %>% ## everything()
  sf::st_transform(4326)

Default_file <- VECTOR_FILE
base_map_bounds <<- Default_file %>% 
  st_bbox() %>% 
  as.character()

# Creates base map
createMap <- function() {  
  m <- leaflet() %>%
    # addProviderTiles(providers$nlmaps.water) %>%
    addProviderTiles(providers$CartoDB.VoyagerNoLabels) %>%
    fitBounds(base_map_bounds[1], base_map_bounds[2], base_map_bounds[3], base_map_bounds[4])
  return(m)
}

# This function is designed to allow attempts at loading vector files without 
# crashing the app. If an improper shapefile is loaded, it will raise an error 
# warning and return the default NC shapefile instead
catch_vec <- function(file_path){ 
  tryCatch({
    spatial_data <- st_read(file_path) %>%
      sf::st_transform(4326)
  }, warning = function(w) {
    showNotification('There was an error - please make sure you included all shapefile components. Loading default file instead.', '', duration = NULL, type = 'error')
    spatial_data <- Default_file
  }, error = function(e) {
    showNotification('There was an error - please make sure you included all shapefile components. Loading default file instead.', '', duration = NULL, type='error')
    spatial_data <- Default_file
  })
  return(spatial_data)
}

# This function prepares uploaded data for being read by the catch_vec function.
# It renames shapefiles in a way that is readable to st_read and it unzips .zip
# files. It relies on catch_vec to handle errors if the resulting files are 
# problematic.
load_spatial <- function(file_in){
    if(any(str_detect(file_in$datapath, '.shp'))){  # if shapefile
      # the upload gives the files unique names, so st_read() won't recognize 
      # them as belonging to the same shapefile. Thus we rename them.
      for (path in file_in$datapath){
        newpath <- sub('.\\.', 'shapefile.', path)
        file.copy(path, newpath)  # create new set of properly named files
      }
      # locate which file is the .shp file to feed into st_read()
      shppth_idx <- which(str_detect(file_in$datapath, '\\.shp'))
      shppth <- sub('.\\.', 'shapefile.', file_in$datapath[shppth_idx])
      spatial_data <- catch_vec(shppth)
    }
    else if (any(str_detect(file_in$datapath, '.zip'))){  # zipped shapefile
      shppth <- sub('.\\....', '', file_in$datapath)  # get temporary file location
      zip::unzip(file_in$datapath, exdir = shppth)  # unzip to temp location
      shpname <- list.files(path = shppth, pattern = '\\.shp')[1]  # get name of shapefile in temp location
      req(shpname)  # if the zip has no shapefile, do not try to load it
      spatial_data <- catch_vec(paste0(shppth, shpname))
    }
    else{ # if not a shapefile, proceed as normal
      spatial_data <<- catch_vec(file_in$datapath)
    }
  return(spatial_data)
}

#Land Use categories and corresponding colors
Land_Use_Categories<- c('Residential', 'Commercial', 'Industrial', 'Institutional', 'Recreational')
landuse_cat <- data.frame(Land_Use_Categories)
color_palette_list2 = c(RColorBrewer::brewer.pal(n = 9, name = "Set1")) # for color assignments for polygons
color_palette_list = c("#ffffff", color_palette_list2) # for legend
map_pallette <- colorFactor(palette = color_palette_list2, domain=1:9, na.color = "#FFFFFF00") # for fill
map_pallete2 <- colorFactor(palette = color_palette_list2, domain=1:9, na.color = "black") # for borders
###use updateradiobutton, and text input https://shiny.rstudio.com/reference/shiny/0.14/updateRadioButtons.html

# Define UI for application that draws a histogram


ui <- dashboardPage(
  dashboardHeader(title = "PIVOT ", titleWidth = 250),
  dashboardSidebar(
    width = 300,
    
    sidebarMenu(
      class = "sidebar",
      style = "height: 90vh; overflow-y: auto;",
      
      #tags$style(
        # "#sidebarItemExpanded {
        #     overflow: auto;
        #     height: calc(100vh - 50px) !important;
        # }"),
      
      hr(),
      # "Add a map as your PPGIS base, and press the Reload Map button when uploaded. If you simple want to test the application", tags$br(),
      # "press the Reload Map button for test data"
      tags$p(style = "font-size: 16px;color: black;font-weight: bold;padding-left: 15px;padding-bottom: 0px",
             span("1. Add base map layer for",br(), "planning and press Reload Map"),br(),
             span(icon("info-circle"), id = "icon1", style = "color: blue; 15px;")
      ),bsPopover("icon1", "Choose spatial data", "This include .shp, .gpkg, and .geojson for the base layer that you will use for planning. *Note shapefile must be accomponied by necessary additional files (.) You can also press Reload Map to test the application using data from North Carolina", trigger = "hover", placement = "bottom"),
      fileInput("filemap",
                label = NULL, 
                multiple = TRUE,
                buttonLabel = "Browse to upload spatial data",
                accept = c(".shp",".dbf",".sbn",".sbx",".shx",".prj", ".gpkg", ".geojson", ".zip")),
                
      fluidRow(column(11, actionBttn(
        inputId = "clear_map",
        label = "Reload Map",
        color = "success",
        size = "md",
        style = "unite",
        icon = icon("map"),
        block = TRUE
      ))),          
      
                
      wellPanel(
        tags$p(style = "font-size: 16px;color: black;font-weight: bold;padding-left: 15px;padding-bottom: 0px",
                       span("2. Optional step. Add a map",br(), "for veiwing and press Reload",br(), "Map"),
                       span(icon("info-circle"), id = "icon2", style = "color: blue")
      ),
      fileInput("basemap_file",
                          label = NULL, 
                          multiple = TRUE,
                          buttonLabel = "Browse to upload spatial data",
                          accept = c(".shp",".dbf",".sbn",".sbx",".shx",".prj", ".gpkg", ".geojson", ".zip")),
                bsPopover("icon2", "Add an additional map", "Explore your region and aid in decision-making. Press the Reload Map button when uploaded", trigger = "hover", placement = "bottom"),

      
      selectInput("field", tags$p(style = "font-size: 16px;","Choose a measure from the optional map to display:"), 'N/A'),
      
      fluidRow(column(11, actionBttn(
        inputId = "reload_basemap",
        label = "Reload Basemap",
        color = "success",
        size = "md",
        style = "unite",
        icon = icon("map"),
        block = TRUE
      )))),
      
      hr(),
      fluidRow(column(3, verbatimTextOutput("value"))),
      textInput("textinp",
                tags$p(style = "font-size: 16px;",
                                span("3. Type in categories for mapping and click Add Map Category"),
                                span(icon("info-circle"), id = "icon3", style = "color: blue"))
                , placeholder = "Type here"),
      bsPopover("icon3", "Create categories for mapping", "For example, the category green space for prioritizing the location of these projects in your city. Press Add Map Category when you have finished typing", trigger = "hover", placement = "bottom"),
                
                
                
      fluidRow(column(11,actionBttn(
        inputId = "labbutton",
        label = "Add Map Category",
        color = "success",
        size = "md",
        style = "unite",
        icon = icon("pencil"),
        block = TRUE
      ))),

      
      #"Choose the groups that you want to add to the map, and click the map to indicate these preferences"
    
      awesomeRadio("radioInt",  
                   label = tags$p(style = "font-size: 16px;",
                   span("4. Choose categories and click on the map to prioritize"),
                   span(icon("info-circle"), id = "icon4", style = "color: blue")),
                   status= "success", 
                   choices=values,
                   selected = 'No Category'),
      bsPopover("icon4", "Choose a mapping category", "Click the circle to left to choose mapping categories you want to add to the map. click the map to indicate these preferences", trigger = "hover", placement = "bottom"),
      
                     
                     hr(),
      
      
      
      HTML(paste0(
        "<br>",
        "<a href='https://seas.umich.edu/' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='UM_SEAS_Logo.png' width = '186'></a>",
        "<br>"
      )),
      
      HTML(paste0(
        "<br>",
        "<a href='https://glisa.umich.edu/' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='GLISA_logo_standard_0.png' width = '186'></a>",
        "<br>"
      )),   
      
      HTML(paste0(
        "<br><br><br><br><br><br><br><br><br>",
        "<table style='margin-left:auto; margin-right:auto;'>",
        "<tr>",
        "<td style='padding: 5px;'><a href='https://scholar.google.com/citations?user=ZXsytH8AAAAJ&hl=en' target='_blank'><i class='fab fa-google fa-lg'></i></a></td>",
        "<td style='padding: 5px;'><a href='https://twitter.com/derekvanberkel' target='_blank'><i class='fab fa-twitter fa-lg'></i></a></td>",
        "<td style='padding: 5px;'><a href='https://www.instagram.com/nationalparkservice' target='_blank'><i class='fab fa-university fa-lg'></i></a></td>",
        "</tr>",
        "</table>",
        "<br>")),
      HTML(paste0(
        "<script>",
        "var today = new Date();",
        "var yyyy = today.getFullYear();",
        "</script>",
        "<p style = 'text-align: center;'><small>&copy; - <a href='https://seas.umich.edu/research/faculty/derek-van-berkel' target='_blank'>DerekVanBerkel.com</a> - <script>document.write(yyyy);</script></small></p>",
        "<p style = 'text-align: center;'><small>&copy; - <a href='https://www.linkedin.com/in/tgestabrook/' target='_blank'>ThomasEstabrook.com</a> - <script>document.write(yyyy);</script></small></p>",
        "<p style = 'text-align: center;'><small>&copy; - <a href='https://www.linkedin.com/in/rahul-agrawal-bejarano-5b395774/' target='_blank'>RahulAgrawalBejarano.com</a> - <script>document.write(yyyy);</script></small></p>",
        "<p style = 'text-align: center;'><small>&copy; - <a href='https://www.researchgate.net/profile/Nathan-Fox-8' target='_blank'>NathanFox.com</a> - <script>document.write(yyyy);</script></small></p>"))
    ), downloadButton(outputId = "download_shp", label = "Download Map")
  ),
  dashboardBody(theme_blue_gradient,
                leafletOutput('PPGISmap', width='100%', height='650'), 
                div(style= "left:1500px; right:40px; bottom:60px; position:fixed; cursor:inherit; z-index: 10000;", 
                    wellPanel(
                      style = "padding: 8px; border-bottom: 1px solid #CCC; background: #EBEDF0;",
                      HTML("Download a print of your map?"), actionButton("go", "Take a screenshot"))),
                div(style= "left:1500px; right:40px; bottom:0px; position:fixed; cursor:inherit; z-index: 10000;", 
                    wellPanel(
                      style = "padding: 8px; border-bottom: 1px solid #CCC; background: #EBEDF0;",
                      HTML("Download your map data?")))
  )
)


server <- function(input, output, session) {
  
  ###observe user categories input
  value <- c("No Category" = NA)
  rv <- reactiveValues(values=value)
  print(rv)
  
  # Event to add new categories to list
  observeEvent(input$labbutton,{
    req(input$textinp)
    newVal <- length(rv$values)  
    
    if(input$textinp %in% names(rv$values)){
      showNotification('Cannot add duplicate category.', '', duration = 3, type = 'warning')
      return()
    }
    if(length(rv$values) > 12){
      showNotification('Cannot add more than 12 categories. If you need to reset the list, click Reload Map.', '', duration = 3, type = 'warning')
      return()
    }
    
    names(newVal) <- input$textinp
    rv$values <- c(rv$values, newVal)
    updateRadioButtons(session,inputId ="radioInt",choices=rv$values)
    print(rv$values)
  })
  
  # Renders the map output
  output$PPGISmap <- renderLeaflet({
    createMap() %>%
      addMapPane('base_layers', 410) %>%  # This ensures the base layers will render below the clickable polygon layer
      addMapPane('poly_layer', 450) %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      addProviderTiles(providers$OpenTopoMap, group = "Open Topo Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "ESRI World Imagery") %>%
      addPolygons(
        data=VECTOR_FILE,
        layerId=~PPGIS_CODE,
        group='base_polygons',
        weight=1.5,
        fillOpacity=0,
        color = 'black',
        options = pathOptions(pane = "poly_layer")
      )  %>%
      addLayersControl(
        baseGroups = basemap_groups,
        options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend(
        # pal=landuse_pallete,
        values=landuse_cat$Land_Use_Categories,
        position='bottomleft',
        title="Legend of Categories",
        opacity=0.6,
        colors = color_palette_list[1:length(values)],
        labels = names(values)
      )
  })
  
  # clear_map has been reworked and now just checks if a new vector map is 
  # uploaded and then uses a proxy to add it to the leaflet map
  observeEvent(input$clear_map, {  # reload all map contents with whatever file was uploaded
    
    if (is.null(input$filemap)) {  # if no upload, use default NC
      VECTOR_FILE <<- Default_file
      is_selected <<- NA
      print(summary(VECTOR_FILE))
      print(colnames(VECTOR_FILE))
      
      base_map_bounds <<- VECTOR_FILE %>% 
        st_bbox() %>% 
        as.character()
    } 
    else {
      VECTOR_FILE <<- load_spatial(input$filemap) %>%
        dplyr::mutate(PPGIS_CODE = as.character(row_number()),SELECTED = NA) %>%
        dplyr::select(PPGIS_CODE, SELECTED, geometry) ## everything()
      
      # clear selections from previous file
      is_selected <<- NA
      base_map_bounds <<- VECTOR_FILE %>% 
        st_bbox() %>% 
        as.character()
    }
    leafletProxy(mapId = 'PPGISmap') %>%
      clearGroup('base_polygons') %>%
      addPolygons(
        data=VECTOR_FILE,
        layerId=~PPGIS_CODE,
        group='base_polygons',
        weight=1.5,
        fillOpacity=0,
        color = 'black',
        options = pathOptions(pane = "poly_layer")) %>%
      fitBounds(base_map_bounds[1], base_map_bounds[2], base_map_bounds[3], base_map_bounds[4])
    
    # reset categories
    rv$values <- c("No Category" = NA)
    landuse_palette_code_selected <- NA
    updateRadioButtons(session,inputId ="radioInt",choices=rv$values, selected = 'No Category')
  })
  
  # Event to take a screenshot
  observeEvent(input$go, {
    screenshot(id="PPGISmap")
  })
  
  # Event for clicking basemap button. Checks if basemap exists and loads it in
  observeEvent(input$reload_basemap, {
    basemap_groups <<-c("OSM (default)", "Toner", "Toner Lite", "Open Topo Map", "ESRI World Imagery")
    if(basemap_type == 'raster'){
      leafletProxy(mapId='PPGISmap') %>%
        clearImages() %>% 
        addLayersControl(
          baseGroups = basemap_groups,
          # overlayGroups = c("Quakes", "Outline"),
          options = layersControlOptions(collapsed = FALSE))
    }
    else if(basemap_type == 'vector'){
      leafletProxy(mapId='PPGISmap') %>%
        clearGroup(basemap_name) %>%
        addLayersControl(
          baseGroups = basemap_groups,
          # overlayGroups = c("Quakes", "Outline"),
          options = layersControlOptions(collapsed = FALSE))
    }

    # Here, we figure out if we have uploaded a basemap, if it is raster or vector, and load it
    if(is.null(input$basemap_file)){  # if no file
      user_basemap <<- NULL
      basemap_type <<- 'None'
      basemap_groups <<-c("OSM (default)", "Toner", "Toner Lite", "Open Topo Map", "ESRI World Imagery")
      updateSelectInput(inputId = 'field', choices = 'N/A')
      return()
    }
    else if (tools::file_ext(input$basemap_file$name[1]) == 'tif'){  # if it is a .tif raster
      basemap_type <<- 'raster'
      user_basemap <<- raster(input$basemap_file$datapath)
      basemap_name <<- toString(input$basemap_file$name[1])
      basemap_groups <<- c("OSM (default)", "Toner", "Toner Lite", "Open Topo Map", "ESRI World Imagery", basemap_name)
      bmap_fields <<- NULL
      updateSelectInput(inputId = 'field', choices = 'N/A')
    }
    else {  # if a vector
      user_basemap <<- load_spatial(input$basemap_file)
      basemap_type <<- 'vector'
      basemap_name <<- toString(input$basemap_file$name[1])
      basemap_groups <<- c("OSM (default)", "Toner", "Toner Lite", "Open Topo Map", "ESRI World Imagery", basemap_name)
      bmap_fields <<- colnames(user_basemap %>% dplyr::select(where(is.numeric)))
      # Update the field selector with the basemap's fields
      updateSelectInput(inputId = 'field', choices = bmap_fields[!bmap_fields == 'geometry']) # trying to load geometry column causes crash
      }
    leafletProxy(mapId='PPGISmap') %>%
      #  If there is a basemap, either display the raster or vector file.
      {if(is.null(user_basemap)) . 
        else if (basemap_type == 'raster') addRasterImage(map = ., x = user_basemap, group = basemap_name) 
        else addPolygons(map = ., 
                         data = user_basemap,
                         group = basemap_name, 
                         weight = 0.5,
                         fillOpacity = 0,
                         color = 'blue',
                         options = pathOptions(pane = "base_layers"))} %>%
      addLayersControl(
        baseGroups = basemap_groups,
        # overlayGroups = c("Quakes", "Outline"),
        options = layersControlOptions(collapsed = FALSE))
  })
  
  #  When users select a basemap field to display:
  #  First, check if a valid basemap exists and has fields to display. Then, 
  #  Create a simple palette for a quintile classification of the selected 
  #  field, then update the layer to use the selected symbology
  observeEvent(input$field, {
    if (is.null(user_basemap) || is.null(bmap_fields)){
      return()
    }
    else {
      basemap_var <- input$field
      bins <- quantile(user_basemap[[basemap_var]], na.rm=TRUE)
      bmap_pal <- colorBin("Greys", domain = user_basemap[[basemap_var]], bins = bins)  # may want to change color ramp
      
      leafletProxy(mapId='PPGISmap') %>%
        removeShape(user_basemap) %>%
        addPolygons(data = user_basemap, 
                    group = basemap_name, 
                    weight = 0.5,
                    fillOpacity = 0.25,
                    color = 'white',
                    fillColor = ~bmap_pal(user_basemap[[basemap_var]]),
                    options = pathOptions(pane = "base_layers")) 
      
    }
  }, ignoreInit = TRUE)
  
  # refresh legend?
  observe({
    proxy <- leafletProxy("PPGISmap", data = VECTOR_FILE)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$labbutton) {
      newMapLgd <- rv$values
      colorHex <- color_palette_list[1:length(newMapLgd)]
      print(colorHex)
      proxy %>% addLegend(
        # pal=landuse_pallete,
        values=newMapLgd,
        position='bottomleft',
        title="Legend of Categories",
        opacity=0.6,
        colors = colorHex,
        labels = names(newMapLgd)
      )
    }
  })
  
  # Event to handle clicking polygons to assign categories
  observeEvent(input$PPGISmap_shape_click, {
    polygon_clicked <- input$PPGISmap_shape_click
    
    if (is.null(polygon_clicked)) { return() }
    if (is.null(polygon_clicked$id)) {return()}  # User-inputted vector basemaps will have no unique id value, so this line prevents them from being clicked
    
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
          group='base_polygons',
          weight=1,
          fillOpacity=0,
          color = 'black',
          options = pathOptions(pane = "poly_layer")
        ) 
      
      #print(VECTOR_FILE_selected)
    }
    else { # if polygon is not selected
      if(is.null(input$radioInt)){
        showNotification('Please select a category to assign.', '', duration = 5, type = 'warning')
        return()}
      landuse_palette_code_selected <- as.numeric(input$radioInt)
      print(landuse_palette_code_selected)
      
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
          group='base_polygons',
          weight=1.5,
          fillOpacity=0.5,
          color = ~map_pallete2(as.factor(SELECTED)),
          fillColor = ~map_pallette(as.factor(SELECTED)),
          options = pathOptions(pane = "poly_layer")
        )
      print(VECTOR_FILE$SELECTED)
    }
  })
  
  # Download shapefile
  output$download_shp <- downloadHandler(
    
    filename <- function() {"Data_shpExport.zip"},
    content = function(file) {
      withProgress(message = "Exporting Data", {
        
        incProgress(0.5)
        tmp.path <- dirname(file)
        
        name.base <- file.path(tmp.path, "PivotOutput")
        name.glob <- paste0(name.base, ".*")
        name.shp  <- paste0(name.base, ".shp")
        name.zip  <- paste0(name.base, ".zip")
        
        print(tmp.path)
        print(name.glob)
        
        if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
        VECTOR_FILE %>%
          left_join(data.frame(values, CAT = names(values)), by=c('SELECTED' = 'values')) %>%
          sf::st_write(dsn = name.shp, ## layer = "shpExport",
                       driver = "ESRI Shapefile", quiet = TRUE)
        
        zip::zipr(zipfile = name.zip, files = Sys.glob(name.glob))
        req(file.copy(name.zip, file))
        
        if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
        
        incProgress(0.5)
      })
    }  
  )
  
}

shinyApp(ui, server)





