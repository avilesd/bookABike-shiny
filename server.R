#Lade benötigte Pakete
library(shiny)
library(shinyjs)
library(leaflet)
library(ggmap)
library(httr)

# Import functions from other files if needed
#source("locationToNumberOfBikes.R"

# Load rentalZones for lat and lon data
rentalZones <- read.csv2("/srv/shiny-server/RENTAL_ZONE.csv", sep=";")
stations <- rentalZones[!is.na(rentalZones$RENTAL_ZONE_X_COORDINATE),]
stations <- stations[stations$RENTAL_ZONE_X_COORDINATE!=0,]

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  pasteHelp <- function(street, city) {
    anfangsort <- paste(street, city, "Germany", sep = ",")
    return(anfangsort)
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
      
      shiny::validate(
        need(result, "Die Eingabe 'Strasse & Strassennr.' ist ungültig. Bitte benutzen Sie folgendes Format: Strassenname Nr. ODER maximale API Abrufe pro Minute erreicht, bitte warten Sie eine Minute.")
      )
      
      # Run only if input from 'Anfgansort' is valid, i.e. if geocode(...) was successful
      if (result){
        
        lon <- lonLat[1]
        lat <- lonLat[2]
        print(lon)
        print(lat)
        
        tripleTable <- triangulateLocation(lat, lon, Sys.time(), input$radius)
        
        #resultList <- locationToBikes(lat, lon)
        finalLat  <- as.numeric(as.vector(tripleTable[, 1])) #resultList$finalLat
        finalLon  <- as.numeric(as.vector(tripleTable[, 2])) #resultList$finalLon
        finalFreq <- as.numeric(as.vector(tripleTable[, 3])) #resultList$finalFreq
        futureFreq <- as.numeric(as.vector(tripleTable[, 3])) #Vector fututre Freq
        reserveTagLat <-vector() #Vector Reserve Tag per Station Lat
        reserveTagLon <-vector() #Vector Reserve Tag per Station Lon
        tag <- FALSE
        # We use 'leafletProxy' to change the Layers on the map without rerendering
        if(input$showCurrent) {
          leafletProxy("mymap") %>%
            addCircleMarkers(finalLon, finalLat, radius = finalFreq*3,
                             popup = paste(as.character(finalFreq), " Fahrräder(aktuell)",
                                           sep=""), color = c('red'))
        }
        else {
          #Berechne die Vorhersage für die verschiednenen Stationen
          for (i in 1:length(finalLat)){
            deltaPerStationHourly <- getDelta(finalLat[i],finalLon[i], input$datePicker)
            futureFreqActual <-  futureFreq[i]+sum(deltaPerStationHourly)
            if(futureFreqActual<0){futureFreqActual <- 0}
            futureFreq[i] <- futureFreqActual
            costNotToReserveStation <- costNotToReserve(futureFreqActual)
            costReserveStation <- costReserve(finalFreq[i],deltaPerStationHourly)
            #Entscheide welche Kostenart höher ist
            if(input$costRatio*costNotToReserveStation>costReserveStation){
              tag <- TRUE
              reserveTagLon <- c(reserveTagLon,finalLon[i])
              reserveTagLat <- c(reserveTagLat,finalLat[i])
            }
          }
          print("Unchecked")
        }
        
        #
        # leaflet() has to be the last operation!!!
        #Füge entsprechende Marker für Leaflet hinzu
        logoPath = 'location-pointer.png'
        
        if(tag){
          leaflet(data = stations[stations$CITY==input$stadt,]) %>% addTiles() %>%
            addMarkers(as.numeric(lon) , as.numeric(lat), icon = list(
              iconUrl = logoPath, iconSize = c(30, 50)
            )) %>%
            addMarkers(~RENTAL_ZONE_X_COORDINATE, ~RENTAL_ZONE_Y_COORDINATE,popup = ~as.character(RENTAL_ZONE_GROUP)) %>%
            addCircleMarkers(finalLon, finalLat, radius = futureFreq*3,
                             popup = paste(as.character(round(futureFreq)), " Fahrräder (Prognose)",
                                           sep=""), color = c('blue')) %>%
            addPopups(reserveTagLon, reserveTagLat, "Hier Reservieren (Empfehlung)") %>%
            setView(lat, lon, zoom = 17)
          
        }else{
          leaflet(data = stations[stations$CITY==input$stadt,]) %>% addTiles() %>%
            addMarkers(as.numeric(lon) , as.numeric(lat), icon = list(
              iconUrl = logoPath, iconSize = c(30, 50)
            )) %>%
            addMarkers(~RENTAL_ZONE_X_COORDINATE, ~RENTAL_ZONE_Y_COORDINATE,popup = ~as.character(RENTAL_ZONE_GROUP)) %>%
            addCircleMarkers(finalLon, finalLat, radius = futureFreq*3,
                             popup = paste(as.character(round(futureFreq)), " Fahrräder (Prognose)",
                                           sep=""), color = c('blue')) %>%
            setView(lat, lon, zoom = 17)
        }
      }
      
    })
    
  })
  
  observeEvent(input$showHelp, {
    output$helpRadius <- renderText({ 
      "<div class='help-panel'><span class=''><b>Radius</b>: Der Nutzer kann mit Hilfe eines Schiebereglers den Radius für die Suche anpassen. Eine Änderung des Radius führt dazu, dass nur die Stationen innerhalb dieses Radius angezeigt werden. Einheit: Meter.</span></div>"
    })
    output$helpRisiko <- renderText({ 
      "<div class='help-panel'><span class=''><b>Kostenverhältnis</b>: Dieses Parameter hat lediglich einen Einfluss auf die Entscheidungsfunktion (Simulation), nämlich ob wir eine Empfehlung reservieren oder nicht.
      Die Kosten ein Fahrrad zu reservieren werden gegen die Kosten ein Fahrrad nicht zu reservieren abgewägt. Ist das Kostenverhältnis größer als 1, sind die Kosten einer Reservierung für diesen hypothetischen Nutzer höher als nicht
      zu reservieren. D.h. er tendiert nicht zu reservieren und 'erhöht' somit das Risiko kein Fahrrad zu haben. In Kürze: das Parameter wird als die Risikoaversion der Nutzer interpretiert.</span></div>"
    })
    output$helpGeneral <- renderText({ 
      "Kurze Anleitung der App:

Die interaktive Karte zeigt die Anzahl der Call-a-Bike (CAB) Fahrräder in Abhängigkeit des eingegebenen Standortes und des eingegebenen Datums. Die Karte weist folgende Features und Parameter auf:
<ul><li>Fahrrad-Stationen: Alle offiziellen Fahrrad-Stationen werden für die eingegebene Stadt auf der Karte mit dem Symbol <img style='width:25px' src='station.png' /> dargestellt. Wenn man auf das Symbol klickt, wird 
die Name der Station angezeigt.</li>
<li>Aktuelle Verfügbarkeit der Fahrräder pro Station: Diese erfährt man indem man auf die Option 'Aktuell' klickt und wird durch die roten Kreise dargestellt.</br>Daten werden aus der CAB-API entnommen, allerdings ist diese eine Beta-Version. Daher erlaubt die API nur 30 Anfragen pro Minute. Dies muss bei ständiger Änderung der Eingabe-Parametern berücksichtigt werden. Einheit: Fahrräder pro Station.</li>
<li>Prognostizierte Verfügbarkeit:Wird durch die Größe der blauen Kreisen pro Station dargestellt. Die prognostizierte Anzahl erfährt man indem man auf den blauen Kreis klickt. Prognose wird zum eingegebenen Zeitpunkt und in der Nähe des Eingabeortes berechnet. Einheit: Fahrräder pro Station.
<ul>
  <li>Parameter anpassen: Diese Änderung dieser verursacht eine neue Kalkulation der aktuellen (CAB-API) und der prognostizierten Verfügbarkeit.
  <ul>
    <li>Radius : Der Nutzer kann mit Hilfe eines Schiebereglers den Radius für die Suche anpassen. Eine Änderung des Radius führt dazu, dass nur die Stationen innerhalb dieses Radius angezeigt werden. Einheit: Meter.</li>
    <li>Standort : Der Nutzer kann eine beliebige Adresse eingeben.</li>
    <li>Datum : Fahrräder für die Reservierungen können maximal 4 Tage im Voraus angefragt bzw. gebucht werden.</li>
    <li>Reservierung : Empfehlung bzw. Keine Empfehlung einer Reservierung in Abhängigkeit der eingegebenen Parameter.</li>
  </ul>
</li>
</ul>
</li>
<li>Kostenverhältnis : Misst die Risikoaversion der Nutzer.</li>
      </ul>"

    })
  })
  
})






#' Algortihm for calculating the number of bikes on several stations around
#' a user-given location
#'
#' Since the Call-a-bike API limits the number of asked items (read bikes) to 50 pro
#' query, we created a function that makes at least 3 queries to ask around a location
#' to try to get (altogether) more than just 50 available bikes around a location.
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
  attr(dateTime, "tzone") <- "UTC"
  dateTime <- gsub(" ", "T", dateTime)
  dateTime <- paste(dateTime, "-00:00:00", sep="")
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

#Lade benötigte Pakete für SVM und JSON umwandlung der Wetterdaten
require(e1071)
library(jsonlite)

#Funktion um die Veränderung an einer Station zu bekommen
getDelta <- function (lat,lon,futureTime, resultType = "delta"){
  #Finde Stationennamen 
  stationName <-  as.character(stations[which.min(abs((stations$RENTAL_ZONE_X_COORDINATE-lat)+abs(stations$RENTAL_ZONE_Y_COORDINATE-lon))),]$RENTAL_ZONE_GROUP)
  stationName <-gsub("-", " ", stationName)
  #Wandle Zeit in entsprechendes Format
  futureTime <- as.POSIXct(futureTime)
  #Lese SVMSpace ein 
  SVMspace <- read.csv("/home/r/R/StuttgartSVM.csv", sep=",", stringsAsFactors = FALSE, fileEncoding="UTF-8")
  rownames(SVMspace) <- SVMspace$X
  SVMspace <-  SVMspace[colnames(SVMspace)!="X"]
  SVMspace <- SVMspace[colnames(SVMspace)!="hour"]
  #Lege einen Data Frame an für die Klassen
  categories  <-  as.data.frame(SVMspace[,"category"])
  colnames(categories) <- "category"
  #Lösche im SVMRaum das Klassenattribut
  SVMspace <- SVMspace[colnames(SVMspace)!="category"]
  #Lösche Schnee, da er kaum auswirkungen hat
  SVMspace <- SVMspace[colnames(SVMspace)!="SNWD"]
  print(stationName)
  print(futureTime)
  #Lese die Veränderungen pro klasse aus
  deltaClass <- read.csv("/home/r/R/StuttgartVeränderungKlassen.csv", sep=",", stringsAsFactors = FALSE, fileEncoding="UTF-8")
  deltaClass <- deltaClass[names(deltaClass)!="X"]
  colnames(deltaClass)[1:(ncol(deltaClass)-1)] <- substring(colnames(deltaClass)[1:(ncol(deltaClass)-1)],2)
  colnames(deltaClass) <-gsub("\\.", " ", colnames(deltaClass))
  deltaClass
  #Erzeuge url zum Abruf von Wetterdaten
  url <- "http://api.openweathermap.org/data/2.5/forecast?units=metric&appid=a68fd0d9af55f756ea35eb3f51a4c3b6&q="
  url <- paste(url,"Stuttgart",sep="") 
  
  # Formatiere die Wetterdaten entsprechend
  # rufe hierbei Wetterdaten als JSON Ojekt ab
  getContent <- fromJSON(url) # content function will try to parse to R object
  tmax <- getContent$list$main$temp_max
  tmin <- getContent$list$main$temp_min
  tavg <- (tmax-tmin)/2+tmin
  weatherData <- data.frame(matrix(0, ncol = 7,nrow=nrow(getContent$list)))
  weatherData$X1 <- as.POSIXct(getContent$list$dt_txt)
  weatherData$X2 <- as.numeric(strftime(getContent$list$dt_txt, "%H"))
  weatherData$X3 <- tmax
  weatherData$X4 <- tmin
  weatherData$X5 <- tavg
  weatherData$X6 <- getContent$list$rain$`3h`*8
  weatherData$X7 <- as.numeric(strftime(weatherData$X1,"%w"))
  colnames(weatherData)[1] <- "Date"
  colnames(weatherData)[2] <- "hour"
  colnames(weatherData)[3:ncol(weatherData)] <- colnames(SVMspace)
  weatherData$PRCP[is.na(weatherData$PRCP)] <- 0
  
  #Erstelle SVM-Model auf Basis der historischen Daten
  svm.model <- svm(SVMspace, categories[,], kernel="linear")
  #Klassifiziere aktuelle Wetterdaten
  svm.pred <-  predict(svm.model, weatherData[,3:ncol(weatherData)])
  #Speicher relevante Wetterdaten (kleiner als die Zeit in der Zukunft)
  forecast <- weatherData[weatherData$Date<=futureTime,]
  #Wie viele Stunden sollen Vorhergesagt werden
  currentTime <- Sys.time()
  attr(currentTime, "tzone") <- "Europe/Paris"
  hours <- round(as.numeric(difftime(futureTime,currentTime,units="hours")))
  
  #Speichere die gesamte Veränderung der Station
  stationChange <- 0
  for(i in 1:hours)
  {
    currentHour <- currentTime+i*60*60
    if(length(rownames(forecast[forecast$Date<=currentHour,]))==0){
      forecastIndex <- 1
    }else{
      forecastIndex <- max(as.numeric(rownames(forecast[forecast$Date<=currentHour,])))
    }
    classHour <- as.matrix(c(svm.pred))[forecastIndex]
    hourOfDay <- as.numeric(strftime(currentHour, "%H"))+1
    
    stationChange[i] <- deltaClass[deltaClass$class==classHour,colnames(deltaClass)==stationName][hourOfDay]
  }
  #Gebe Veränderung an Station zurück
  return(stationChange)
}

costNotToReserve <- function (finalProjection) {
  # Assure that no negative final projection is allowed
  costs <- 1/(1+finalProjection)
  return(costs)
}

costReserve <- function(currentBikes, projectedChange, reserved=1) {
  # You have to account for the bikes that are reserved
  totalNumberBikes <- currentBikes + reserved
  totalCosts <- 0
  # Begin algorithm
  for (j in 1:length(projectedChange)) {
    if(projectedChange[j] >= 0) {
      cost <- 0
    }
    else {
      cost <- (reserved/totalNumberBikes)*abs(projectedChange[j]) 
    }
    totalCosts <- totalCosts + cost
    currentBikes <- turnXIfLessThanX(currentBikes + projectedChange[j], 0)
    totalNumberBikes <- turnXIfLessThanX(totalNumberBikes + projectedChange[j], 1)
  }
  totalCosts
}
# Auxiliary function for turning values less than 0 automatically into 0
turnXIfLessThanX <- function(actualValue, x) {
  if (actualValue <= x) {
    result <- x
  }
  else {
    result <- actualValue
  }
  result
}