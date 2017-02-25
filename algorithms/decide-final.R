#Lade benötigte Pakete für SVM und JSON umwandlung der Wetterdaten
require(e1071)
library(jsonlite)

#' This functions returns two costs using two separate functions and returns them together
#' as lists. It uses Max Blanck's 'getDelta' function to get the hourly predicted change
#' of bike number depending on the station and the inputed time.
#' 
#' @param currentBikes represents the actual number of bikes in the given station. Ideally
#' when the user made the request.
#' @stationName is the station name as given by the CAB-API
#' @futureTime is a time in the future, which represents when the user want to check 
#' bike number projections.
#' 
getCosts <- function(currentBikes, projectedChange, stationName, futureTime, ratioNR2R=1) {
  #test NR
  #projectedChange <- c(-0.0476190476190476,-0.0740740740740741, -0.814814814814815, -1.2962962962963,-0.333333333333333,0.333333333333,0.962962962962963,2.18518518518519)  #getDelta(stationName,as.POSIXlt(futureTime))
  #test R
  #projectedChange <- c(-4/3,-2/3,-2/3,-2/3,0.13,-0.65)
  

  costNR <- costNR * ratioNR2R
  costR  <- costReserve(currentBikes, projectedChange, reserved = 1)
  list(costNR = costNR, costR = costR)
}

#Teste function
getCosts(3, "7010 Königstraße","2017-02-01 19:47:22")


#' This functions projects the (general) costs produced to a person who
#' comes to a given station to book a bike where there is at least one reserved bike.
#' We use the probability of that person encountering a booked bike and calculate the costs
#'that this creates. That is, what are the repercusions of recommending booking a bike?
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


#' This functions projects the cost produced to a person to whom we explicitly
#' did not recommend to book a bike (through our app), but he/she came to a station and found
#' the station empty.
costNotToReserve <- function (finalProjection) {
  # Assure that no negative final projection is allowed
  costs <- 1/(1+finalProjection)
  costs
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

#Funktion um die Veränderung an einer Station zu bekommen
getDelta <- function (stationName,futureTime, resultType = "delta"){
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
  
  #Lese die Veränderungen pro klasse aus
  deltaClass <- read.csv("/home/r/R/StuttgartVeränderungKlassen.csv", sep=",", stringsAsFactors = FALSE, fileEncoding="UTF-8")
  deltaClass <- deltaClass[names(deltaClass)!="X"]
  colnames(deltaClass)[1:(ncol(deltaClass)-1)] <- substring(colnames(deltaClass)[1:(ncol(deltaClass)-1)],2)
  colnames(deltaClass) <-gsub("\\.", " ", colnames(deltaClass))
  
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
  hourlyChange <- 0
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
    hourlyChange[i] <-as.numeric(paste(deltaClass[deltaClass$class==classHour,colnames(deltaClass)==stationName][hourOfDay]))
    stationChange <- stationChange+deltaClass[deltaClass$class==classHour,colnames(deltaClass)==stationName][hourOfDay]
  }
  #Gebe Veränderung an Station zurück
  if (resultType == "delta") {
    return(stationChange) 
  }
  else {
    return(hourlyChange)
  }
}

#test
getDelta("7850 Hedelfingen",as.POSIXlt("2017-02-01 19:47:22"))
getDelta("7010 Königstraße",as.POSIXlt("2017-02-01 19:47:22"), resultType = "hourly")
