library(shiny)
library(shinyjs)
library(leaflet)
library(ggmap)
library(httr)

# Import functions from other files if needed
#source("locationToNumberOfBikes.R"

# Load rentalZones for lat and lon data
rentalZones <- read.csv2("/home/ubuntu/RENTAL_ZONE.csv", sep=";")
stations <- rentalZones[!is.na(rentalZones$RENTAL_ZONE_X_COORDINATE),]
stations <- stations[stations$RENTAL_ZONE_X_COORDINATE!=0,]

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  pasteHelp <- function(street, city) {
    anfangsort <- paste(input$strasse, input$stadt, "Germany", sep = ",")
    anfangsort
  }
  print("Input DatePicker")
  
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
      
      # Run only if input from 'Strasse' & 'Stadt' is valid, i.e. if result exists
      if (result){

        lon <- lonLat[1]
        lat <- lonLat[2]
        print(lon)
        print(lat)
        
        tripleTable <- triangulateLocation(lat, lon, input$datePicker, input$radius)
        
        #resultList <- locationToBikes(lat, lon)
        finalLat  <- as.numeric(as.vector(tripleTable[, 1])) #resultList$finalLat
        finalLon  <- as.numeric(as.vector(tripleTable[, 2])) #resultList$finalLon
        finalFreq <- as.numeric(as.vector(tripleTable[, 3])) #resultList$finalFreq
        
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



#' Algortihm for calculating the number of bikes on several stations around
#' a user-given location
#'
#' triangulateLocation : Since the Call-a-bike API limits the number of asked items (read bikes) to 50 pro
#' query, we created a function that makes at least 3 queries to ask around a location
#' to try to get (altogether) more than just 50 available bikes around a location.
#' 
#' locationtoBikes : only cares about the results of the API, so this functios is called
#' 3 times within triangulateLocation, i.e. the API is called three times. 
#' 
#' 
#' It uses a geocoded location (latitude, longitude) to query real time data from the
#' CAB-API.
#'
#' @param aMatrix a matrix to be 'normalized'
#' @param attr attributes IDs, vector of integer numbers corresponding to the
#'   attributes you desire to use; attr are assumed to be 1-indexed. Even if not
#'   all attributes are given the function normalizes all columns, by default as
#'   benefit type unless shown otherwise with \code{cost_ids}.
#' @param cost_ids argument used to convert selected cost attributes into
#'   benefit attributes. Integer vector.
#' @return a normalized matrix
#' @export

locationToBikes <- function(latitude, longitude, inputTime, radius) {
  # Key used in application is stored locally for security purposes
  apikey <- Sys.getenv("CALLABIKE_API")
  customRadius <- radius
  customLimit <- 50
  begin <- as.character(inputTime)
  
  providerNetwork <- 2 # tell API to use Call-a-bike Network
  
  getResults <- GET("https://api.deutschebahn.com/flinkster-api-ng/v1/bookingproposals", 
                    query = list(lat = latitude, lon = longitude, radius= customRadius, begin=begin,
                                 limit = customLimit, providernetwork = providerNetwork),
                    , add_headers(Accept = "application/json", Authorization = apikey))
  # Get parsed JSON content
  getContent <- content(getResults) # content function will try to parse to R object
  
  # Get latitude-frequency from each bike-item (effectively how many bikes in each latitude)
  # The second line does the same for the latitude
  listLonLat <- lapply(getContent$items, function(listElement) unlist(listElement$position$coordinates))
  listLonLat <- lapply(listLonLat, as.character)
  bikesProStation <- as.data.frame(table(unlist(lapply(listLonLat, function(vec) paste(vec[2], vec[1],sep="---")))))
  tableLonLat <- t(as.data.frame(apply(bikesProStation[1], 1, strsplit, "---")))
  finalTable <- cbind(tableLonLat, bikesProStation[2])
  print(finalTable)
  print(getContent$size)
  result <- finalTable
}

triangulateLocation <- function (latitude, longitude, dateTime, radius, merge = TRUE) {
  dateTime <- gsub(" ", "T", dateTime)
  dateTime <- paste(dateTime, "-01:00:00", sep="")
  print(dateTime)
  # Call locationBikes with original coordinates
  mainLocation <- locationToBikes(latitude, longitude, dateTime, radius)
  
  # Call locationBikes with coordinates slightly positively moved
  positiveMain <- locationToBikes(latitude + 0.002, longitude + 0.002, dateTime, radius)
  
  # Call locationBikes with coordinates slightly positively moved
  negativeMain <- locationToBikes(latitude - 0.002, longitude - 0.002, dateTime, radius)
  
  if (merge) {
    df3 <- merge(mainLocation, positiveMain, by.x=c("1", "2"), by.y=c("1", "2"), all.x = T)
    df4 <- merge(df3, negativeMain, by.x=c("1", "2"), by.y=c("1", "2"))
    # Remove redundant and NA-containing columns
    df4 <- df4[, -c(4,5)] 
  }
  else {
    join3 <- merge(mainLocation, positiveMain, by.x=c("1", "2"), by.y=c("1", "2"), all.x = T, all.y = T)
    df4 <- merge(join3, negativeMain, by.x=c("1", "2"), by.y=c("1", "2"), all.x = T, all.y = T)
    df4[is.na(df4)] <- 0
    newCol <- pmax(df4$"Freq.x", df4$"Freq.y", na.rm = TRUE)
    newCol <- pmax(newCol, df4$"Freq", na.rm = TRUE)
    df4 <- df4[, -c(3,4,5)] 
    df4$"newFreq" <- newCol
  }
  df4
}