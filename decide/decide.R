stuttgartChange <- read.csv("/home/r/R/StuttgartVeränderung.csv", sep=",", stringsAsFactors = FALSE, fileEncoding="utf-8")
#View(stuttgartChange)

# Convert timestamp to day of week, hourStart, length & class -> cut from table

wday <- function(x) as.POSIXlt(x)$wday

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

dayToNumber <- function(x) {
  factor(x, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), ordered = TRUE)
}

