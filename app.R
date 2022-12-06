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


options(shiny.maxRequestSize=1000000000) 
values <- c("None" = NA)
bmap_fields <- NULL

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

#ppgis <- function{data = ,  }

# #load shapefile
VECTOR_FILE <- st_read(system.file("shape/nc.shp", package="sf")) %>%
  dplyr::mutate(PPGIS_CODE = as.character(row_number()),SELECTED = NA) %>%
  dplyr::select(PPGIS_CODE, SELECTED, geometry) %>% ## everything()
  sf::st_transform(4326)

#  polygon basemap code
Basemap_test <- st_read(system.file("shape/nc.shp", package="sf")) %>%
  sf::st_transform(4326)
# basemap_var <- 'BIR79'
# bins <- quantile(Basemap_test[[basemap_var]])
# bmap_pal <- colorBin("YlOrRd", domain = Basemap_test[[basemap_var]], bins = bins)

# Creates base map
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
color_palette_list = c("#ffffff","#ffff99", "#e31a1c", "#6a3d9a", "#a6cee3", "#b2df8a")
landuse_pallete <- colorBin(palette = color_palette_list, domain=1:length(Land_Use_Categories), na.color = "#FFFFFF00")
map_pallette <- colorBin(palette =  c("#ffff99", "#e31a1c", "#6a3d9a", "#a6cee3", "#b2df8a"), domain=1:length(Land_Use_Categories), na.color = "#FFFFFF00")
landuse_pallete2 <- colorBin(palette = color_palette_list, domain=1:length(Land_Use_Categories), na.color = "black") # for borders
###use updateradiobutton, and text input https://shiny.rstudio.com/reference/shiny/0.14/updateRadioButtons.html

# Define UI for application that draws a histogram


ui <- dashboardPage(
  dashboardHeader(title = "PIVOT ", titleWidth = 250),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      hr(),
      # "Add a map as your PPGIS base, and press the Reload Map button when uploaded. If you simple want to test the application", tags$br(),
      # "press the Reload Map button for test data"
      fileInput("filemap",
                tags$p(style = "font-size: 16px;",span("1. Add base map layer for planning and press Reload Map", div(style = "display:inline-block;",
                    title = "Choose spatial vector data (i.e., .shp, .gpkg, .geojson) for the base layer that you will use for planning. *Note shapefile must be accomponied by necessary additional files (.) You can also press Reload Map to test the application using data from North Carolina",
                    icon("info-circle")))),
                multiple = TRUE,
                buttonLabel = "Browse to upload spatial data",
                accept = c(".shp",".dbf",".sbn",".sbx",".shx",".prj", ".gpkg", ".geojson", ".zip")),
      
      wellPanel(fileInput("basemap_file",
                tags$p(style = "font-size: 16px;",
                span("2. Optional step. Add a map for veiwing and press Reload Map", div(style = "display:inline-block;",
                                                                                    title = "Add a map to help explore your region and aid in decision-making. Press the Reload Map button when uploaded",
                                                                                    icon("info-circle")))),
                multiple = TRUE,
                accept = c(".tif")),
      
      selectInput("field", tags$p(style = "font-size: 16px;","Choose a measure from the optional map to display:"), c("None", bmap_fields))),
      
      #actionButton("clear_map", "Reload Map"),
      
      actionBttn(
        inputId = "clear_map",
        label = "Reload Map",
        color = "success",
        size = "md",
        style = "unite",
        icon = icon("map"),
        block = TRUE
      ),
      
      
      hr(),
      fluidRow(column(3, verbatimTextOutput("value"))),
      textInput("textinp",tags$div( style = "font-size: 16px;",
                                    span("3. Create categories for mapping and press Add Map Category", div(style = "display:inline-block;",
                                                                                                                          title = "Type the name of the group options you wish to add to the map and press the Add Group button",
                                                                                                                          icon("info-circle"))),), placeholder = "Type here"),
      #actionButton("labbutton","Add Group"),
      
      actionBttn(
        inputId = "labbutton",
        label = "Add Map Category",
        color = "success",
        size = "md",
        style = "unite",
        icon = icon("pencil"),
        block = TRUE
      ),
      #"Choose the groups that you want to add to the map, and click the map to indicate these preferences"
    
      awesomeRadio("radioInt", label = tags$div(style = "font-size: 16px;",
                                                span("4. Choose categories and click on the map to prioritize", 
                                                     div(style = "display:inline-block;",
                                                         title = "Choose the groups that you want to add to the map, and click the map to indicate these preferences",
                                                         icon("info-circle")))),status= "success", choices=values),
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
    )
  ),
  dashboardBody(theme_blue_gradient,
                leafletOutput('PPGISmap', width='100%', height='850'), 
                div(style= "left:1260px; right:40px; bottom:90px; position:fixed; cursor:inherit; z-index: 10000;", 
                    wellPanel(
                      style = "padding: 8px; border-bottom: 1px solid #CCC; background: #EBEDF0;",
                      HTML("Download a print of your map?"), actionButton("go", "Take a screenshot"))),
                div(style= "left:1260px; right:40px; bottom:0px; position:fixed; cursor:inherit; z-index: 10000;", 
                    wellPanel(
                      style = "padding: 8px; border-bottom: 1px solid #CCC; background: #EBEDF0;",
                      HTML("Download your map data?"), downloadButton("download_shp", "Download Map")))
  )
)


server <- function(input, output, session) {
  
  ###observe user categories input
  value <- c("None" = NA)
  rv <- reactiveValues(values=value)
  print(rv)
  
  observeEvent(input$labbutton,{
    req(input$textinp)
    newVal <- length(rv$values)  
    names(newVal) <- input$textinp
    rv$values <- c(rv$values, newVal)
    updateRadioButtons(session,inputId ="radioInt",choices=rv$values)
    print(rv$values)
  })
  
  # The clear_map event executes the following actions:
  # First, it checks if a polygon file has been uploaded and then processes that 
  # file into the form necessary for use in PIVOT, or defaults to the NC vector 
  # Then, it clears any prior memory of selected polygons for an old vector file
  # Then, it checks if there is an input to the user-supplied basemap fileInput.
  # If there is, it sets up some variables needed to added it as a display
  # option in the next step. 
  # Next, the renderLeafet() function is called, incorporating the polygon file
  # and optional basemap initialized in the previous sections.
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
        # the upload gives the files unique names, so read_sf won't recognize 
        # them as belonging to the same shapefile
        for (path in input$filemap$datapath){
          newpath <- sub('.\\.', 'shapefile.', path)
          file.copy(path, newpath)  # create new set of properly named files
        }
        
        # locate which file is the .shp file to feed
        shppth_idx <- which(str_detect(input$filemap$datapath, '\\.shp'))
        shppth <- sub('.\\.', 'shapefile.', input$filemap$datapath[shppth_idx])
        
        tryCatch({
          VECTOR_FILE <<- st_read(shppth) %>%
            dplyr::mutate(PPGIS_CODE = as.character(row_number()), SELECTED = NA) %>% 
            dplyr::select(PPGIS_CODE, SELECTED, geometry) %>% ## everything()
            sf::st_transform(4326)
        }, warning = function(w) {
          showNotification('There was an error - please make sure you included all shapefile components. Loading default file instead.', '', duration = NULL, type = 'error')
          VECTOR_FILE <<- st_read(system.file("shape/nc.shp", package="sf")) %>%  # use the default file instead
            dplyr::mutate(PPGIS_CODE = as.character(row_number()),SELECTED = NA) %>% 
            dplyr::select(PPGIS_CODE, SELECTED, geometry) %>% ## everything()
            sf::st_transform(4326)
        }, error = function(e) {
          showNotification('There was an error - please make sure you included all shapefile components. Loading default file instead.', '', duration = NULL, type='error')
          VECTOR_FILE <<- st_read(system.file("shape/nc.shp", package="sf")) %>%  # use the default file instead
            dplyr::mutate(PPGIS_CODE = as.character(row_number()),SELECTED = NA) %>% 
            dplyr::select(PPGIS_CODE, SELECTED, geometry) %>% ## everything()
            sf::st_transform(4326)
        })
      }
      else if (str_detect(input$filemap$datapath, '.zip')){  # zipped shapefile
        #print(input$filemap$datapath)
        shppth <- sub('.\\....', '', input$filemap$datapath)  # get temporary file location and remove zip file name
        zip::unzip(input$filemap$datapath, exdir = shppth)  # unzip to temp location
        shpname <- list.files(path = shppth, pattern = '\\.shp')[1]  # get name of shapefile in temp location
        #print(shpname)
        req(shpname)  # if the zip has not shapefile, do not try to load it
        VECTOR_FILE <<- st_read(paste0(shppth, shpname)) %>%
          dplyr::mutate(PPGIS_CODE = as.character(row_number()), SELECTED = NA) %>% 
          dplyr::select(PPGIS_CODE, SELECTED, geometry) %>% ## everything()
          sf::st_transform(4326)
      }
      else if (str_detect(input$filemap$datapath, '.shp')){  # only added .shp, no other parts
        showNotification('There was an error - please make sure you included all shapefile components. Loading default file instead.', '', duration = NULL, type='error')
        
        VECTOR_FILE <<- st_read(system.file("shape/nc.shp", package="sf")) %>%  # use the default file instead
          dplyr::mutate(PPGIS_CODE = as.character(row_number()),SELECTED = NA) %>% 
          dplyr::select(PPGIS_CODE, SELECTED, geometry) %>% ## everything()
          sf::st_transform(4326)
      }
      else{ # if not a shapefile, proceed as normal
        VECTOR_FILE <<- st_read(input$filemap$datapath) %>%
          dplyr::mutate(PPGIS_CODE = as.character(row_number()), SELECTED = NA) %>% 
          dplyr::select(PPGIS_CODE, SELECTED, geometry) %>% ## everything()
          sf::st_transform(4326)
      }
      
      # clear selections from previous file
      is_selected <<- NA
      #print(summary(VECTOR_FILE))
      #print(colnames(VECTOR_FILE))
      #print(VECTOR_FILE$PPGIS_CODE)
      
      base_map_bounds <<- VECTOR_FILE %>% 
        st_bbox() %>% 
        as.character()
    }
    
    # Here, we figure out if we have uploaded a basemap, if it is raster or vector, and load it
    if(is.null(input$basemap_file)){  # if no file
      user_basemap <<- NULL
      basemap_groups <<-c("OSM (default)", "Toner", "Toner Lite", "Open Topo Map", "ESRI World Imagery")
    }
    else if (tools::file_ext(input$basemap_file$name[1]) == 'tif'){  # if it is a .tif raster
      basemap_type <<- 'raster'
      user_basemap <<- raster(input$basemap_file$datapath)
      basemap_name <<- toString(input$basemap_file$name[1])
      basemap_groups <<- c("OSM (default)", "Toner", "Toner Lite", "Open Topo Map", "ESRI World Imagery", basemap_name)
      bmap_fields <<- NULL
    }
    else if (tools::file_ext(input$basemap_file$name[1]) %in% c('geojson', 'gpkg')){  # if it is a single-file vector
      basemap_type <<- 'vector'
      user_basemap <<- st_read(input$basemap_file$datapath) %>%
        sf::st_transform(4326)
      basemap_name <<- toString(input$basemap_file$name[1])
      basemap_groups <<- c("OSM (default)", "Toner", "Toner Lite", "Open Topo Map", "ESRI World Imagery", basemap_name)
      bmap_fields <<- colnames(user_basemap %>% dplyr::select(where(is.numeric)))
    }
    else {  # if a shapefile
      for (path in input$filemap$datapath){
        newpath <- sub('.\\.', 'shapefile.', path)
        file.copy(path, newpath)  # create new set of properly named files
      }
      # locate which file is the .shp file to feed
      shppth_idx <- which(str_detect(input$filemap$datapath, '\\.shp'))
      shppth <- sub('.\\.', 'shapefile.', input$filemap$datapath[shppth_idx])
      
      tryCatch({
        user_basemap <<- st_read(shppth) %>%
          sf::st_transform(4326)
        basemap_type <<- 'vector'
        basemap_name <<- toString(input$basemap_file$name[1])
        basemap_groups <<- c("OSM (default)", "Toner", "Toner Lite", "Open Topo Map", "ESRI World Imagery", basemap_name)
        bmap_fields <<- colnames(user_basemap %>% dplyr::select(where(is.numeric)))
      }, warning = function(w) {
        showNotification('Invalid basemap upload - please make sure you included all shapefile components.', '', duration = NULL, type = 'error')
        user_basemap <<- NULL
        basemap_groups <<-c("OSM (default)", "Toner", "Toner Lite", "Open Topo Map", "ESRI World Imagery")
        bmap_fields <<- NULL
      }, error = function(e) {
        showNotification('Invalis basemap upload - please make sure you included all shapefile components.', '', duration = NULL, type='error')
        user_basemap <<- NULL
        basemap_groups <<-c("OSM (default)", "Toner", "Toner Lite", "Open Topo Map", "ESRI World Imagery")
        bmap_fields <<- NULL
      })
      # Update the field selector with the basemap's fields
      updateSelectInput(inputId = 'field', choices = bmap_fields[!bmap_fields == 'geometry']) # trying to load geometry column causes crash
    }
    
    output$PPGISmap <- renderLeaflet({
      createMap() %>%
        addMapPane('base_layers', 410) %>%  # This ensures the base layers will render below the clickable polygon layer
        addMapPane('poly_layer', 450) %>%
        addTiles(group = "OSM (default)") %>%
        addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
        addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
        addProviderTiles(providers$OpenTopoMap, group = "Open Topo Map") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "ESRI World Imagery") %>%
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
        addPolygons(
          data=VECTOR_FILE,
          layerId=~PPGIS_CODE,
          #group='base_polygons',
          weight=1.5,
          fillOpacity=0,
          color = 'black',
          options = pathOptions(pane = "poly_layer")
        ) %>%
        # Overlay groups
        # addCircles(~long, ~lat, ~10^mag/5, stroke = F, group = "Quakes") %>%
        # addPolygons(data = outline, lng = ~long, lat = ~lat,
        #             fill = F, weight = 2, color = "#FFFFCC", group = "Outline") %>%
        # Layers control
        addLayersControl(
          baseGroups = basemap_groups,
          # overlayGroups = c("Quakes", "Outline"),
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
    
  })
  observeEvent(input$go, {
    screenshot(id="PPGISmap")
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
      bins <- quantile(user_basemap[[basemap_var]])
      bmap_pal <- colorBin("Greys", domain = user_basemap[[basemap_var]], bins = bins)  # may want to change color ramp
      
      leafletProxy(mapId='PPGISmap') %>%
        removeShape(user_basemap) %>%
        addPolygons(data = user_basemap, 
                    group = basemap_name, 
                    weight = 0.5,
                    fillOpacity = 0.25,
                    color = 'white',
                    fillColor = ~bmap_pal(user_basemap[[basemap_var]]),
                    options = pathOptions(pane = "base_layers")
        ) 
      
    }
  }, ignoreInit = TRUE)
  
  
  observe({
    proxy <- leafletProxy("PPGISmap", data = VECTOR_FILE)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$labbutton) {
      newMapLgd <- rv$values
      print(newMapLgd)
      proxy %>% addLegend(
        # pal=landuse_pallete,
        values=newMapLgd,
        position='bottomleft',
        title="Legend of Landuse Categories",
        opacity=0.6,
        colors = color_palette_list[0:length(newMapLgd)],
        labels = names(newMapLgd)
      )
    }
  })
  
  # just testing here
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
          weight=1,
          fillOpacity=0,
          color = 'black',
          options = pathOptions(pane = "poly_layer")
        ) 
      
      #print(VECTOR_FILE_selected)
    }
    else { # if polygon is not selected
      landuse_palette_code_selected <- as.numeric(input$radioInt)
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
          weight=1.5,
          fillOpacity=0.5,
          color = ~landuse_pallete2(SELECTED),
          fillColor = ~map_pallette(SELECTED),
          options = pathOptions(pane = "poly_layer")
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

