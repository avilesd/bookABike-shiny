locationToStations <- function(latitude, longitude, radius) {
  # Key used in application is stored locally for security purposes
  apikey <- Sys.getenv("CALLABIKE_API")
  customRadius <- radius
  customLimit <- 50
  #begin <- as.character(inputTime)
  
  providerNetwork <- 2 # tell API to use Call-a-bike Network
  
  getResults <- GET("https://api.deutschebahn.com/flinkster-api-ng/v1/bookingproposals", 
                    query = list(lat = latitude, lon = longitude, radius= customRadius, #, begin=begin,
                                 limit = customLimit, providernetwork = providerNetwork, expand = "area"),
                    , add_headers(Accept = "application/json", Authorization = apikey))
  # Get parsed JSON content
  getContent <- content(getResults) # content function will try to parse to R object
  
  # Get latitude-frequency from each bike-item (effectively how many bikes in each latitude)
  # The second line does the same for the latitude
  listLonLat <- lapply(getContent$items, function(listElement) unlist(listElement$area$name))
  listLonLat <- lapply(listLonLat, as.character)
  bikesProStation <- as.data.frame(table(unlist(lapply(listLonLat, function(vec) paste(vec[2], vec[1],sep="---")))))
  tableLonLat <- t(as.data.frame(apply(bikesProStation[1], 1, strsplit, "---")))
  finalTable <- cbind(tableLonLat, bikesProStation[2])
  print(finalTable)
  print(getContent$size)
  result <- finalTable
}
