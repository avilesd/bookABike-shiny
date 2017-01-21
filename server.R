library(shiny)
library(shinyjs)
library(leaflet)
library(ggmap)

geocode("Koenigsstrasse 10, Stuttgart, Germany")

#load rentalZones for lat and lon data
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
  
  observeEvent(input$showApp, {
    output$mymap <- renderLeaflet({
      # Show first 20 rows from the `quakes` dataset
      #lonLat <- tryCatch(geocode(as.character(input$anfangsort)),error=function(e) "error", warning=function(w) "there was an error")
      
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
      validate(
        need(result, "Die Eingabe 'Anfangsort' ist nicht gültig. Bitte benutzen Sie folgendes Format: Strassenname Nr., Stadt")
      )
      if (result){
        #addClass(selector = "#mymap", class = "show")
        removeClass(selector = "#mymap", class = "hidden")
        addClass(selector = "input", class = "disable")
        show(selector = "#hiddenDiv") 
        
        lon <- lonLat[1]
        lat <- lonLat[2]
        
        leaflet(data = stations[stations$CITY=="Stuttgart",]) %>% addTiles() %>%
          addMarkers(~RENTAL_ZONE_X_COORDINATE, ~RENTAL_ZONE_Y_COORDINATE, popup = ~as.character(RENTAL_ZONE_HAL_ID)) %>%
          setView(lat, lon, zoom = 16)
      }
    })
  })
  
  
  
})