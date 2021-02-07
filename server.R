server <- function(input, output, session){
  if(exists("df")) {load("df_ships.RData")}
  
  ##Subset dataset for input controls
  ship_type <- sort(unique(df$ship_type))
  output$ship_type <- renderUI({
    selectInput("ship_type", "Select Ship Type", choices = ship_type, selected = "Cargo", width = 350)
  })
  
  ship_name <- subset(df, ship_type == "Cargo")$SHIPNAME %>% unique() %>% sort()
  output$ship_name <- renderUI({
    selectInput("ship_name", "Select Ship Name", choices = ship_name, width = 350)
  })

  observeEvent(input$ship_type, 
               updateSelectInput(session, "ship_name", "Ship Name",
                                 choices = unique(
                                   sort(subset(df, ship_type == input$ship_type)$SHIPNAME)) ))
  
  ## Creating dataset for plotting ship positions
  
  #Filter dataset for plot
  distance <- reactive({
    if(is.null(input$ship_name) | is.null(input$ship_type)) { print("NULL INPUTS") } else {
      distance <- df%>%
        filter(ship_type == input$ship_type & SHIPNAME==input$ship_name)%>%
        arrange(desc(DATETIME))%>%
        unique() %>%
        select(DATETIME, LAT, LON, DESTINATION, FLAG)
      
      #Calculate distance
      if(nrow(distance)>0){
      distance <- transform(distance, DATETIME_2 = c(DATETIME[-1], NA), LAT_2 = c(LAT[-1], NA), LON_2 = c(LON[-1], NA), DESTINATION=DESTINATION, FLAG=FLAG)
      distance <- distance %>% dplyr::rowwise() %>% mutate(distance = geosphere::distm(c(LON, LAT), c(LON_2, LAT_2), fun = geosphere::distHaversine)) %>%
        ungroup() %>% arrange(desc(distance, DATETIME))
      
      distance <- distance[1,]
      return(distance)
      }
    else {return(data.frame())}
    } 
  })
  
  ## Create base map
  map <- reactiveValues(dat = 0)
  output$map <- renderLeaflet ({
    
    map$dat <- leaflet() %>% 
      addTiles() %>% 
      addProviderTiles("Esri.WorldImagery", group = "Satellite View") %>%
      addLayersControl(
        baseGroups = c("Default","Satellite View"),
        position = "topleft",
        options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))
    })
  
  leafletProxy("map", data = df) %>%
    fitBounds(lng1=min(df$LON)-1, lng2=max(df$LON)+1, lat1=min(df$LAT)-1, lat2=max(df$LAT)+1 )
  
  ## Plotting calculated distance on base map
  observe({
    if( length(distance()) > 0 & !is.null(input$ship_name) & !is.null(input$ship_type)) {
      
      icons <- list(
        ship_icon = awesomeIcons( icon = 'ship', library = "fa", markerColor = 'white', iconColor = "green"))
      
      legend_title <- "Selected ship information"
      legend <- paste0("Selected ", input$ship_type, " ship ", input$ship_name, " travelled ", round(distance()$distance, 1), " metres from ", distance()$DATETIME, " to ", distance()$DATETIME_2 )
      
  leafletProxy("map", data = distance())%>%
    fitBounds(lng1=distance()$LON-0.01, lng2=distance()$LON_2+0.01, lat1=distance()$LAT-0.01, lat2=distance()$LAT_2+0.01 ) %>%
    clearMarkers() %>%
    clearAntpath() %>%
    addCircleMarkers(lng=~LON, lat=~LAT, radius = 5, stroke = T, weight = 1, fillColor = "gray", fillOpacity=0.8, label = ~as.character(DATETIME)) %>%
    addAwesomeMarkers(lng=~LON_2, lat=~LAT_2, icon = icons$ship_icon, label = ~as.character(DATETIME_2)) %>%
    addAntpath(lng = ~c(LON,LON_2), lat = ~c(LAT,LAT_2), weight = 3, opacity = 0.2, smoothFactor = 5) %>%
    clearControls() %>%
    addLegend("topright", values = legend, title = legend_title, opacity = .8, layerId = "colorLegend", labels=legend, colors = "white")
    }
  })
  #Ship pop-up information
  
  PopupInfo <- function(id, lat, lng) {
    
    content <- as.character(tagList(
       tags$h4(input$ship_type,"Ship:",input$ship_name),
       tags$strong("Flag: ", unique(distance()$FLAG)), tags$br(),
       tags$strong("Destination: ", unique(distance()$DESTINATION)), 
       ))
    
  leafletProxy("map") %>% addPopups(lng = lng, lat = lat, popup = content)
  
  }
  
  # Ship pop-up information
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_marker_click
    if (is.null(event))
      return()
    
    isolate({
      PopupInfo(event$id, event$lat, event$lng)
    })
  })

}