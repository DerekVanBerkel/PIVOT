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


PPGISr <- function(base_map = NULL, information_layers = NULL, mapping_categories = c("High Density Development", "Street Trees", "Infrastructure Need"), mapping_colors = c("#880015", "#22b14c", "#00a2e8")){ 
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
  
  if(is.null(information_layers)){  # if no file
    basemap_type <<- 'None'
    basemap_groups <<-c("OSM (default)", "Toner", "Toner Lite", "Open Topo Map", "ESRI World Imagery")
    bmap_fields <<- NULL
    user_basemap <<- NULL
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
  
  ##Color palettes
  COLOR_PAL2 = c("#ffffff", COLOR_PAL) # for legend
  map_palette <- colorFactor(palette = COLOR_PAL, domain=1:length(COLOR_PAL), na.color = "#FFFFFF00") # for fill
  map_palette2 <- colorFactor(palette = COLOR_PAL, domain=1:length(COLOR_PAL), na.color = "black") # for borders
  ###use updateradiobutton, and text input https://shiny.rstudio.com/reference/shiny/0.14/updateRadioButtons.html
  
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
  
  
  #####################################################################################################
  ############################### definition of user interface ########################################
  #####################################################################################################
  
  
  # Define UI for application that draws a histogram
  ui <- dashboardPage(
    dashboardHeader(title = tags$a(href='https://github.com/DerekVanBerkel/PIVOT',
                                   tags$img(src='PPGIS_logo3.png',height="85%", width="105%")), titleWidth = '300'),
    
    dashboardSidebar(
      width = 300,
      
      
      sidebarMenu(
        ##give a slider to the sidebar this resolves needing to 
        ##scroll the map
        class = "sidebar",
        style = "height: 90vh; overflow-y: auto;",
        
        tags$p(style = "font-size: 12px;color: black;font-weight: bold;padding-left: 15px;padding-bottom: 0px",
               span("This PPGIS tool is designed to support" ,br(), "community planning efforts, helping users" ,br(), "to explore their communities through" ,br(), "spatial data. By following the steps below," ,br(), "users can upload their own spatial data and" ,br(), "create categories that outline regions of the" ,br(), "map for planning purposes.")),
        
        hr(),
        ## We define the header outide of the widget as it also prompted the download, 
        ## which interfered with the information button
        tags$p(style = "font-size: 16px;color: black;font-weight: bold;padding-left: 15px;padding-bottom: 0px",
               span("Select the variable you",br(), "want to view"),br(),
               ##this defines the in-line info icon 
               span(icon("info-circle"), id = "icon1", style = "color: blue; 15px;")
        ),
        ## this function defines the info-circle text using the shinyBS library
        bsPopover("icon1", "Map Variable", "This allows you to view data for making your ", trigger = "hover", placement = "bottom"),
        
        selectInput("field",label = NULL ,'N/A'),
        
        hr(),
        
        ## this is radio button where you choose which category to add to the map
        awesomeRadio("radioInt",  
                     label = tags$p(style = "font-size: 16px;",
                                    span("Choose categories and click on the map to prioritize"),
                                    span(icon("info-circle"), id = "icon4", style = "color: blue")),
                     status= "success", 
                     choices=CAT_LIST,
                     selected = 'No Category'),
        bsPopover("icon4", "Choose a mapping category", "Click the circle to left to choose mapping categories you want to add to the map. click the map to indicate these preferences", trigger = "hover", placement = "bottom"),
        
        
        hr(),
        fluidRow(column(11, actionBttn(
          inputId = "go",
          label = "Download Map",
          color = "success",
          size = "md",
          style = "unite",
          icon = icon("camera"),
          block = TRUE
        ))),
        
        fluidRow(column(11,downloadBttn(
          outputId = "download_shp",
          label = "Download Map Data",
          style = "unite",
          color = "success",
          icon = icon("download")))),
        

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
      )
    ),
    ## this code add the new color theme defined at shinyDashboardthemesDIY()
    dashboardBody(theme_blue_gradient,
                  ## here is the map
                  leafletOutput('PPGISmap', width='100%', height='650') 
                  ## this fixes the location of the download screenshot and data button
                
    )
  )
  
  
  #####################################################################################################
  ############################### definition of server ################################################
  #####################################################################################################
  
  server <- function(input, output, session) {
    
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
    
    
    # Event to take a screenshot
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
        
        
        palette_code_selected <- as.numeric(input$radioInt)
        
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
