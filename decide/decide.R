stuttgartChange <- read.csv("/home/r/R/StuttgartVeränderung.csv", sep=",", stringsAsFactors = FALSE, fileEncoding="utf-8")
#View(stuttgartChange)

# Convert timestamp to day of week, hourStart, length & class -> cut from table

wday <- function(x) as.POSIXlt(x)$wday
geocodeToNearestStations <- function (lat, lon, resultType = "stations", radius=1000, limit=5, providerNetwork = 2) {
  apikey <- Sys.getenv("CALLABIKE_API")
  
  getResults <- GET("https://api.deutschebahn.com/flinkster-api-ng/v1/areas", 
                    query = list(lat = lat, lon = lon, radius= radius,
                                 limit = limit, providernetwork = providerNetwork),
                    , add_headers(Accept = "application/json", Authorization = apikey))
  # Get parsed JSON content
  getContent <- content(getResults) # content function will try to parse to R object
  
  if (resultType!="stations") {
    lonLat <- lapply(getContent$items, function(listElement) unlist(listElement$geometry$position$coordinates))
    lonLat <- lapply(lonLat, as.character)
    colNames <- paste("station" , 1:getContent$limit, sep="")
    result <- as.data.frame(test, col.names = colNames, row.names = c("lon", "lat")) 
  }
  else {
    stationNames <- lapply(getContent$items, function(listElement) unlist(listElement$name))
    colNames2 <- paste("station" , 1:getContent$limit, sep="")
    result <- as.data.frame(stationNames, col.names = colNames2, row.names = "STATIONS")
  }
  result
}
timeToDayAndHour <- function(timestamp) {
  # Get weekday from inputed timestamp & hour
  dateTime <- strsplit(as.character(timestamp), " ")
  date <- dateTime[[1]][1]
  time <- dateTime[[1]][2]
  timeHour <- strsplit(time, ":")
  
  weekday <- wday(as.Date(date))
  time <- timeHour[[1]][1]
  data.frame(weekday, time)
}




# Cut table according to Class, currenTime, chosenTime & location (St, Ha)
##### TODO - REMOVEE DEFAULT FROM OURCLASS & LOCATION ######
##### TODO - CHOSE THE RIGHT TABLE #####
relevantTable <- function(currentTime, chosenTime, city="Stuttgart", ourclass="Monday light rain cold") {
  if (city=="Stuttgart") {
    rawTable <- stuttgartChange 
  }
  else {
    rawTable <- hamburgChange
  }
  # Cut Table
  
  cityTable <- rawTable[, rawTable=="Monday light rain cold"]
  stuttgartChange[stuttgartChange$category=="Monday light rain cold", ]
  
  
  
  names(stuttgartChange)
}



