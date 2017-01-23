library(shiny)
library(shinyjs)
library(leaflet)
library(ggmap)
library(httr)

geocode("Koenigsstrasse 10, Stuttgart, Germany")

# Load rentalZones for lat and lon data
rentalZones <- read.csv2("/home/ubuntu/RENTAL_ZONE.csv", sep=";")
stations <- rentalZones[!is.na(rentalZones$RENTAL_ZONE_X_COORDINATE),]
stations <- stations[stations$RENTAL_ZONE_X_COORDINATE!=0,]

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$output1 <- renderText({ 
    paste("For DEV - Anfangsort:", input$anfangsort)
  })
  
  output$output2 <- renderText({ 
    paste("For DEV - Date and Time selected:", input$datePicker)
  })
  
  # Listen for 'Button Click Event' and trigger the creation of the leaflet map
  # Also make sure to catch warnings and errors

  observeEvent(input$showApp, {
    output$mymap <- renderLeaflet({
      result = tryCatch({
        lonLat <- geocode(input$anfangsort)
        TRUE
      }, warning = function(w) {
        print("Warning catched: Please make sure the user inserts a valid location.")
        FALSE
      }, error = function(e) {
        print("Error catched: See ggmap for possible error sources.")
        FALSE
      }, finally = {
        
      })
      # Rshiny function to use user-friendly error messages
      validate(
        need(result, "Die Eingabe 'Anfangsort' ist nicht gültig. Bitte benutzen Sie folgendes Format: Strassenname Nr., Stadt")
      )
      
      # Run only if input from 'Anfgansort' is valid, i.e. if geocode(...) was successful
      if (result){
        removeClass(selector = "#mymap", class = "hidden")
        addClass(selector = "input", class = "disable")
        show(selector = "#hiddenDiv") 
        
        lon <- lonLat[1]
        lat <- lonLat[2]
        print(lon)
        print(lat)

        # Use httr-package to ask for Call-a-bike realtime data (CAB-API)
        
        # Key used in application is stored locally for security purposes
        apikey <- Sys.getenv("CALLABIKE_API")
        customRadius <- 500
        customLimit <- 50
        providerNetwork <- 2 # tell API to use Call-a-bike Network
        
        getResults <- GET("https://api.deutschebahn.com/flinkster-api-ng/v1/bookingproposals", 
                       query = list(lat = lat, lon = lon, radius= customRadius, 
                                 limit = customLimit, providernetwork = providerNetwork),
                       , add_headers(Accept = "application/json", Authorization = apikey))
        
        #Use this for development only
        #getResults <- GET("https://api.deutschebahn.com/flinkster-api-ng/v1/bookingproposals?lat=48.7821547&lon=9.1821333&radius=500&limit=40&providernetwork=2", add_headers(Accept = "application/json", Authorization = "Bearer 05fb7a3fb8c904a3945176880ae31dfa"))
        getContent <- content(getResults) # content function will try to parse to R object
        
        # Get latitude-frequency from each bike-item (effectively how many bikes in each latitude)
        # The second line does the same for the latitude
        listLonLat <- lapply(getContent$items, function(listElement) unlist(listElement$position$coordinates))
        listLonLat <- lapply(listLonLat, as.character)
        bikesProStation <- as.data.frame(table(unlist(lapply(listLonLat, function(vec) paste(vec[2], vec[1],sep="---")))))
        tableLonLat <- t(as.data.frame(apply(bikesProStation[1], 1, strsplit, "---")))
        finalTable <- cbind(tableLonLat, bikesProStation[2])
        print(finalTable)
        finalLat <- as.numeric(as.matrix(finalTable[1]))
        finalLon <- as.numeric(as.matrix(finalTable[2]))
        finalFreq <- as.numeric(as.matrix(finalTable[3]))
        
        # We use 'leafletProxy' to change the Layers on the map without rerendering
        if(input$showCurrent) {
          leafletProxy("mymap") %>%
            clearShapes() %>%
            addCircleMarkers(finalLon, finalLat, radius = finalFreq*1.7,
                             popup = paste(as.character(finalFreq), " Fahrräder(aktuell)",
                                           sep=""), color = c('red'))
        }
        else {
          print("Unchecked")
        }
        
        #
        # leaflet() has to be the last operation!!!
        #
        
        logoPath = 'location-pointer.png'
        leaflet(data = stations[stations$CITY=="Stuttgart",]) %>% addTiles() %>%
          addMarkers(as.numeric(lon) , as.numeric(lat), icon = list(
            iconUrl = logoPath, iconSize = c(30, 50)
          )) %>%
          addMarkers(~RENTAL_ZONE_X_COORDINATE, ~RENTAL_ZONE_Y_COORDINATE,popup = ~as.character(RENTAL_ZONE_HAL_ID)) %>%
          setView(lat, lon, zoom = 17)
        
      }
      
    })
    
  })
  
  
})