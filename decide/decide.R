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
relevantTable <- function(currentNumberOfBikes, currentTime, chosenTime, reserved = 1, projectedBikes) {
  disponibility <- 0
  #changeVector <- 0
  
  for (i in 1:length(projectedBikes)) {
    if (i==1) {
      help <- currentNumberOfBikes + projectedBikes[i]
      disponibility[i] <- allowZero(help) 
      #changeVector[i] <- help 
    }
    
    if (is.na(projectedBikes[i+1])) {
      break;
      print("In IFFFFFFF")
    }
    
    disponibility[i+1] <- allowZero(disponibility[i] + projectedBikes[i+1])
    #changeVector [i+1] <- changeVector[i]    + projectedBikes[i+1]
  }
  finalCostNR <- sum(sapply(disponibility, costNotToReserve))
  finalCostR <- sum(sapply(disponibility, costReserve, 1))
  list(CostNotR = finalCostNR , CostR = finalCostR)
  
}

allowZero <- function(actualValue) {
  if (actualValue <= 0) {
    result <- 0
  }
  else {
    result <- actualValue
  }
  result
}

# This functions projects the cost produced to a person (per hour) who
# comes to a given station to book a bike and there are at least one reserved bike. We use the probability
# of that person encountering a booked bike and calculate the costs that this creates.
# That is, a way to measure: what are the repercusions of recommending booking a bike.
# The cost of not founding a bike are normalized to 1 (K=1).
#
# 'currentNumberOfBikes' should be one number not a vector
costNotToReserve <- function (availableBikes, k=1) {
  print("available")
  print(availableBikes)
  print("available---")
  if (availableBikes==0) {
    # since we failed to recommend a booking and there are 0 bikes the costs are present
    costs <- 1*k
  }
  else {
    # means there are bikes to rent, so no costs. That is, the fact that we didn't recommend did not have
    # repercusions  
    costs <- 0
  }
  costs
}

# This functions projects the cost produced to a person (per hour) to whom we explicitly
# did not recommend to book a bike, but he/she came to a station and found
# the station empty. The cost of not founding a bike are normalized to 1 (K=1).
#
# 'currentNumberOfBikes' should be one number not a vector
costReserve <- function (availableBikes, reserved=1, k=1) {
  actualNumberOfBikes <- availableBikes + reserved
  if (availableBikes==0) {
    # since we failed to recommend a booking and there are 0 bikes the costs are present
    costs <- 1*k
  }
  else {
    # means there are bikes to rent, so no costs. That is, the fact that we didn't recommend did not have
    # repercusions  
    costs <- (reserved/actualNumberOfBikes) * k
  }
  print(costs)
  costs
}

#
#
# Input is average change in one station for one forecast-class
#
#

change.costNotToReserve <- function (availableBikes, k=1) {
  
  if (availableBikes==0) {
    
  }
  else {
    
  }
  costs
}

costReserve <- function (availableBikes, reserved=1, k=1) {
  actualNumberOfBikes <- availableBikes + reserved
  if (availableBikes==0) {
    # costs <- 1*k
  }
  else {
      # costs <- (reserved/actualNumberOfBikes) * k
  }
  costs
}

