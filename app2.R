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


PPGISr <- function(base_map = NULL, information_layers = NULL, mapping_categories = c("High Density Development", "Street Trees", "Infrastructure Need"), mapping_colors = c('red', 'blue', 'green')){ 
  if (is.null(base_map)) {
    VECTOR_FILE <<- st_read(system.file("shape/nc.shp", package="sf")) %>%
      dplyr::mutate(PPGIS_CODE = as.character(row_number()),SELECTED = NA) %>%
      dplyr::select(PPGIS_CODE, SELECTED, geometry) %>% ## everything()
      sf::st_transform(4326)
  } else {VECTOR_FILE <- st_read(base_map) %>%
    dplyr::mutate(PPGIS_CODE = as.character(row_number()),SELECTED = NA) %>%
    dplyr::select(PPGIS_CODE, SELECTED, geometry) %>% ## everything()
    sf::st_transform(4326)
    
  }
  # if (!exists("information_layers")) {
  #   user_basemap <- NULL
  # } else {
  #   user_basemap <- information_layers
  # }
  
  if(is.null(information_layers)){  # if no file
    basemap_type <<- 'None'
    basemap_groups <<-c("OSM (default)", "Toner", "Toner Lite", "Open Topo Map", "ESRI World Imagery")
  }
  else if (tools::file_ext(information_layers) == 'tif'){  # if it is a .tif raster
    basemap_type <<- 'raster'
    basemap_name <<- toString(information_layers)
    user_basemap <<- raster(information_layers)
    basemap_groups <<- c("OSM (default)", "Toner", "Toner Lite", "Open Topo Map", "ESRI World Imagery", basemap_name)
    bmap_fields <<- NULL
  }
  else {  # if a vector
    basemap_name <<- toString(information_layers)
    user_basemap <<- st_read(information_layers)
    basemap_type <<- 'vector'
    basemap_groups <<- c("OSM (default)", "Toner", "Toner Lite", "Open Topo Map", "ESRI World Imagery", basemap_name)
    bmap_fields <<- colnames(user_basemap %>% dplyr::select(where(is.numeric)))
    # Update the field selector with the basemap's fields
  }
    
  cat_len <- length(mapping_categories)
  if (cat_len != length(mapping_colors)){
    stop('Number of mapping categories must match number of colors.')
  }
  CAT_LIST <- setNames(c(NA, 1:cat_len), c("No Category", mapping_categories))
  COLOR_PAL <- c(mapping_colors)

  
#####################################################################################################
############################### definition of global varibles #######################################
#####################################################################################################
options(shiny.maxRequestSize=1000000000) 
#values <- c("No Category" = NA)
## These are the viewable map fields
# bmap_fields <- NULL
# ##This is the viewable map
# user_basemap <- NULL
# ## this is the viewable map type
# basemap_type <- 'None'
# basemap_groups <<-c("OSM (default)", "Toner", "Toner Lite", "Open Topo Map", "ESRI World Imagery")
##Color palettes
#COLOR_PAL = c(RColorBrewer::brewer.pal(n = 9, name = "Set1")) # for color assignments for polygons
COLOR_PAL2 = c("#ffffff", COLOR_PAL) # for legend
map_palette <- colorFactor(palette = COLOR_PAL, domain=1:length(COLOR_PAL), na.color = "#FFFFFF00") # for fill
map_palette2 <- colorFactor(palette = COLOR_PAL, domain=1:length(COLOR_PAL), na.color = "black") # for borders
###use updateradiobutton, and text input https://shiny.rstudio.com/reference/shiny/0.14/updateRadioButtons.html

## load shapefile and assign it as the default file
# VECTOR_FILE <<- st_read(system.file("shape/nc.shp", package="sf")) %>%
#   dplyr::mutate(PPGIS_CODE = as.character(row_number()),SELECTED = NA) %>%
#   dplyr::select(PPGIS_CODE, SELECTED, geometry) %>% ## everything()
#   sf::st_transform(4326)

Default_file <- VECTOR_FILE
base_map_bounds <<- Default_file %>% 
  st_bbox() %>% 
  as.character()

#####################################################################################################
############################### definition of model functions #######################################
#####################################################################################################


##this function defines the colors for the base-colors for the dashboard based on 
##the "dashboardthemes" library
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
# catch_vec <- function(file_path){ 
#   tryCatch({
#     spatial_data <- st_read(file_path) %>%
#       sf::st_transform(4326)
#   }, warning = function(w) {
#     showNotification('There was an error - please make sure you included all shapefile components. Loading default file instead.', '', duration = NULL, type = 'error')
#     spatial_data <- Default_file
#   }, error = function(e) {
#     showNotification('There was an error - please make sure you included all shapefile components. Loading default file instead.', '', duration = NULL, type='error')
#     spatial_data <- Default_file
#   })
#   return(spatial_data)
# }

# This function prepares uploaded data for being read by the catch_vec function.
# It renames shapefiles in a way that is readable to st_read and it unzips .zip
# files. It relies on catch_vec to handle errors if the resulting files are 
# problematic.
# load_spatial <- function(file_in){
#   if(any(str_detect(file_in$datapath, '.shp'))){  # if shapefile
#     # the upload gives the files unique names, so st_read() won't recognize 
#     # them as belonging to the same shapefile. Thus we rename them.
#     for (path in file_in$datapath){
#       newpath <- sub('.\\.', 'shapefile.', path)
#       file.copy(path, newpath)  # create new set of properly named files
#     }
#     # locate which file is the .shp file to feed into st_read()
#     shppth_idx <- which(str_detect(file_in$datapath, '\\.shp'))
#     shppth <- sub('.\\.', 'shapefile.', file_in$datapath[shppth_idx])
#     spatial_data <- catch_vec(shppth)
#   }
#   else if (any(str_detect(file_in$datapath, '.zip'))){  # zipped shapefile
#     shppth <- sub('.\\....', '', file_in$datapath)  # get temporary file location
#     zip::unzip(file_in$datapath, exdir = shppth)  # unzip to temp location
#     shpname <- list.files(path = shppth, pattern = '\\.shp')[1]  # get name of shapefile in temp location
#     req(shpname)  # if the zip has no shapefile, do not try to load it
#     spatial_data <- catch_vec(paste0(shppth, shpname))
#   }
#   else{ # if not a shapefile, proceed as normal
#     spatial_data <<- catch_vec(file_in$datapath)
#   }
#   return(spatial_data)
# }


#####################################################################################################
############################### definition of user interface ########################################
#####################################################################################################


# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "PIVOT ", titleWidth = 250),
  dashboardSidebar(
    width = 300,
    
    
    sidebarMenu(
      ##give a slider to the sidebar this resolves needing to 
      ##scroll the map
      class = "sidebar",
      style = "height: 90vh; overflow-y: auto;",
      
      hr(),
      ## We define the header outide of the widget as it also prompted the download, 
      ## which interfered with the information button
      tags$p(style = "font-size: 16px;color: black;font-weight: bold;padding-left: 15px;padding-bottom: 0px",
             span("1. Add base map layer for",br(), "planning and press Reload Map"),br(),
             ##this defines the in-line info icon 
             span(icon("info-circle"), id = "icon1", style = "color: blue; 15px;")
      ),
      ## this function defines the info-circle text using the shinyBS library
      bsPopover("icon1", "Choose spatial data", "This include .shp, .gpkg, and .geojson for the base layer that you will use for planning. *Note shapefile must be accomponied by necessary additional files (.) You can also press Reload Map to test the application using data from North Carolina", trigger = "hover", placement = "bottom"),
      ## the widget prompts for uploading your data
      # fileInput("filemap",
      #           label = NULL, 
      #           multiple = TRUE,
      #           buttonLabel = "Browse to upload spatial data",
      #           accept = c(".shp",".dbf",".sbn",".sbx",".shx",".prj", ".gpkg", ".geojson", ".zip")),
      # ##this function loads the mapping layer
      # fluidRow(column(11, actionBttn(
      #   inputId = "clear_map",
      #   label = "Reload Map",
      #   color = "success",
      #   size = "md",
      #   style = "unite",
      #   icon = icon("map"),
      #   block = TRUE
      # ))),          
      
      ## We use a well panel for this code to indicate it is a optional step after 
      ## intial upload of the mapping layer
      #wellPanel(
        ## style formating that approximates the widget headers below
        tags$p(style = "font-size: 16px;color: black;font-weight: bold;padding-left: 15px;padding-bottom: 0px",
               span("2. Optional step. Add a map",br(), "for veiwing and press Reload",br(), "Map"),
               span(icon("info-circle"), id = "icon2", style = "color: blue")
        ),
        ## this widget allows you to upload additional layers for visualizing while
        ## doing your ppgis exercise
        # fileInput("basemap_file",
        #           label = NULL, 
        #           multiple = TRUE,
        #           buttonLabel = "Browse to upload spatial data",
        #           accept = c(".shp",".dbf",".sbn",".sbx",".shx",".prj", ".gpkg", ".geojson", ".zip")),
        # bsPopover("icon2", "Add an additional map", "Explore your region and aid in decision-making. Press the Reload Map button when uploaded", trigger = "hover", placement = "bottom"),
        # 
        ## this function allows you to choose which field to visualize for the 
        ## additional map layer
        selectInput("field", tags$p(style = "font-size: 16px;","Choose a measure from the optional map to display:"), choices = bmap_fields[!bmap_fields == 'geometry']),
        
        ##button to add the additional layer to the ui
        # fluidRow(column(11, actionBttn(
        #   inputId = "reload_basemap",
        #   label = "Reload Basemap",
        #   color = "success",
        #   size = "md",
        #   style = "unite",
        #   icon = icon("map"),
        #   block = TRUE
        #)
        #))
  #),
      
      hr(),
      # fluidRow(column(3, verbatimTextOutput("value"))),
      # ## text input to add new radio options (mapping categories)
      # textInput("textinp",
      #           tags$p(style = "font-size: 16px;",
      #                  span("3. Type in categories for mapping and click Add Map Category"),
      #                  span(icon("info-circle"), id = "icon3", style = "color: blue"))
      #           , placeholder = "Type here"),
      # bsPopover("icon3", "Create categories for mapping", "For example, the category green space for prioritizing the location of these projects in your city. Press Add Map Category when you have finished typing", trigger = "hover", placement = "bottom"),
      # 
      # 
      # ## action button to add new mapping categories
      # fluidRow(column(11,actionBttn(
      #   inputId = "labbutton",
      #   label = "Add Map Category",
      #   color = "success",
      #   size = "md",
      #   style = "unite",
      #   icon = icon("pencil"),
      #   block = TRUE
      # ))),
      
      
      ## this is radio button where you choose which category to add to the map
      awesomeRadio("radioInt",  
                   label = tags$p(style = "font-size: 16px;",
                                  span("4. Choose categories and click on the map to prioritize"),
                                  span(icon("info-circle"), id = "icon4", style = "color: blue")),
                   status= "success", 
                   choices=CAT_LIST,
                   selected = 'No Category'),
      bsPopover("icon4", "Choose a mapping category", "Click the circle to left to choose mapping categories you want to add to the map. click the map to indicate these preferences", trigger = "hover", placement = "bottom"),
      
      
      hr(),
      
      ## This section of code adds the authors, images and link to the sidepanel
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
  ## this code add the new color theme defined at shinyDashboardthemesDIY()
  dashboardBody(theme_blue_gradient,
                ## here is the map
                leafletOutput('PPGISmap', width='100%', height='650'), 
                ## this fixes the location of the download screenshot and data button
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


#####################################################################################################
############################### definition of server ################################################
#####################################################################################################


server <- function(input, output, session) {
  
  ###observe user categories input
  #value <- c("No Category" = NA)
  #rv <- reactiveValues(values=value)
  #print(rv)
  
  # Event to add new categories to list
  # observeEvent(input$labbutton,{
  #   req(input$textinp)
  #   newVal <- length(rv$values)  
  #   
  #   if(input$textinp %in% names(rv$values)){
  #     showNotification('Cannot add duplicate category.', '', duration = 3, type = 'warning')
  #     return()
  #   }
  #   if(length(rv$values) > 9){
  #     showNotification('Cannot add more than 9 categories. If you need to reset the list, click Reload Map.', '', duration = 3, type = 'warning')
  #     return()
  #   }
  #   
  #   names(newVal) <- input$textinp
  #   rv$values <<- c(rv$values, newVal)
  #   updateAwesomeRadio(session,inputId ="radioInt",choices=rv$values)
  #   print('New values:')
  #   print(rv$values)
  #   print(length(rv$values))
  # })
  
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
        position='bottomleft',
        title="Legend of Categories",
        opacity=0.6,
        colors = COLOR_PAL2,
        labels = names(CAT_LIST)
      ) %>%
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
        options = layersControlOptions(collapsed = FALSE))
  })
  
  # clear_map has been reworked and now just checks if a new vector map is 
  # uploaded and then uses a proxy to add it to the leaflet map
  # observeEvent(input$clear_map, {  # reload all map contents with whatever file was uploaded
    
    # if (is.null(input$filemap)) {  # if no upload, use default
    #   VECTOR_FILE <<- Default_file
    #   is_selected <<- NA
    #   print(summary(VECTOR_FILE))
    #   
    #   base_map_bounds <<- VECTOR_FILE %>% 
    #     st_bbox() %>% 
    #     as.character()
    # } 
    # else {
    #   VECTOR_FILE <<- load_spatial(input$filemap) %>%
    #     dplyr::mutate(PPGIS_CODE = as.character(row_number()),SELECTED = NA) %>%
    #     dplyr::select(PPGIS_CODE, SELECTED, geometry) ## everything()
    #   
    #   # clear selections from previous file
    #   is_selected <<- NA
    #   base_map_bounds <<- VECTOR_FILE %>% 
    #     st_bbox() %>% 
    #     as.character()
    # }
    # leafletProxy(mapId = 'PPGISmap') %>%
    #   clearGroup('base_polygons') %>%
    #   addPolygons(
    #     data=VECTOR_FILE,
    #     layerId=~PPGIS_CODE,
    #     group='base_polygons',
    #     weight=1.5,
    #     fillOpacity=0,
    #     color = 'black',
    #     options = pathOptions(pane = "poly_layer")) %>%
    #   fitBounds(base_map_bounds[1], base_map_bounds[2], base_map_bounds[3], base_map_bounds[4])
    # 
    # reset categories
  #   rv$values <- c("No Category" = NA)
  #   landuse_palette_code_selected <<- NA
  #   updateAwesomeRadio(session,inputId ="radioInt",choices=rv$values)
  #   print(length(rv$values))
  #   print('radio status:')
  #   print(input$radioInt)
  # })
  
  # Event to take a screenshot
  observeEvent(input$go, {
    screenshot(id="PPGISmap")
  })
  
  # Event for clicking basemap button. Checks if basemap exists and loads it in
  # observeEvent({
  #   # basemap_groups <<-c("OSM (default)", "Toner", "Toner Lite", "Open Topo Map", "ESRI World Imagery")
  #   # if(basemap_type == 'raster'){
  #   #   leafletProxy(mapId='PPGISmap') %>%
  #   #     clearImages() %>% 
  #   #     addLayersControl(
  #   #       baseGroups = basemap_groups,
  #   #       options = layersControlOptions(collapsed = FALSE))
  #   # }
  #   # else if(basemap_type == 'vector'){
  #   #   leafletProxy(mapId='PPGISmap') %>%
  #   #     clearGroup(basemap_name) %>%
  #   #     addLayersControl(
  #   #       baseGroups = basemap_groups,
  #   #       options = layersControlOptions(collapsed = FALSE))
  #   # }
  #   
  #   # Here, we figure out if we have uploaded a basemap, if it is raster or vector, and load it
  #   leafletProxy(mapId='PPGISmap') 
  # })
  
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
  # observe({
  #   proxy <- leafletProxy("PPGISmap", data = VECTOR_FILE)
  #   
  #   # Remove any existing legend, and only if the legend is
  #   # enabled, create a new one.
  #   proxy %>% clearControls()
  #   if (input$labbutton) {
  #     #newMapLgd <- rv$values
  #     #colorHex <- color_palette_list[1:length(newMapLgd)]
  #     #print(colorHex)
  #     proxy %>% addLegend(
  #       values=newMapLgd,
  #       position='bottomleft',
  #       title="Legend of Categories",
  #       opacity=0.6,
  #       colors = COLOR_PAL,
  #       labels = CAT_LIST
  #     )
  #   }
  # })
  
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
      # if(length(rv$values) < as.numeric(input$radioInt)){
      #   showNotification('Please select a category to assign.', '', duration = 5, type = 'warning')
      #   return()
      # }
      
      palette_code_selected <- as.numeric(input$radioInt)
      #print(palette_code_selected)
      
      # Get current table selected
      #row_clicked <- input$groups_table_cell_clicked
      
      # substitutes selected value for polygon with group number
      VECTOR_FILE[row_idx, ]$SELECTED <<- palette_code_selected
      
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
          color = ~map_palette2(as.factor(SELECTED)),
          fillColor = ~map_palette(as.factor(SELECTED)),
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

shinyApp(ui, server) }
