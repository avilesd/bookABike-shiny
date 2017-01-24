library(shiny)
library(shinyjs)
library(leaflet)
library(ggmap)
library(httr)

# Import functions from other files if needed
if(!exists("locationToBikes", mode="function")) source("locationToNumberOfBikes.R")

# Load rentalZones for lat and lon data
rentalZones <- read.csv2("/home/ubuntu/RENTAL_ZONE.csv", sep=";")
stations <- rentalZones[!is.na(rentalZones$RENTAL_ZONE_X_COORDINATE),]
stations <- stations[stations$RENTAL_ZONE_X_COORDINATE!=0,]

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  #anfangsort <- paste(input$strasse, input$stadt, "Germany", sep = ",")
  #print(anfangsort)
  
  pasteHelp <- function(street, city) {
    anfangsort <- paste(input$strasse, input$stadt, "Germany", sep = ",")
    anfangsort
  }
  
  output$output1 <- renderText({ 
    paste("For DEV - Anfangsort:", pasteHelp(input$strasse, input$stadt))
  })
  
  output$output2 <- renderText({ 
    paste("For DEV - Date and Time selected:", input$datePicker)
  })
  
  # Listen for 'Button Click Event' and trigger the creation of the leaflet map
  # Also make sure to catch warnings and errors

  observeEvent(input$showApp, {
    output$mymap <- renderLeaflet({
      result = tryCatch({
        lonLat <- geocode(pasteHelp(input$strasse, input$stadt))
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
        #show(selector = "#hiddenDiv") 
        
        lon <- lonLat[1]
        lat <- lonLat[2]
        print(lon)
        print(lat)
        
        finalLat <-  resultList$finalLat
        finalLon <-  resultList$finalLon
        finalFreq <- resultList$finalFreq
        
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
        leaflet(data = stations[stations$CITY==input$stadt,]) %>% addTiles() %>%
          addMarkers(as.numeric(lon) , as.numeric(lat), icon = list(
            iconUrl = logoPath, iconSize = c(30, 50)
          )) %>%
          addMarkers(~RENTAL_ZONE_X_COORDINATE, ~RENTAL_ZONE_Y_COORDINATE,popup = ~as.character(RENTAL_ZONE_HAL_ID)) %>%
          setView(lat, lon, zoom = 17)
        
      }
      
    })
    
  })
  
  
})