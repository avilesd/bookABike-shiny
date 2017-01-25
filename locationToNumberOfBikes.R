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

locationToBikes <- function(latitude, longitude) {
  # Key used in application is stored locally for security purposes
  apikey <- Sys.getenv("CALLABIKE_API")
  customRadius <- 600
  customLimit <- 50
  providerNetwork <- 2 # tell API to use Call-a-bike Network
  
  getResults <- GET("https://api.deutschebahn.com/flinkster-api-ng/v1/bookingproposals", 
                    query = list(lat = latitude, lon = longitude, radius= customRadius, 
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
  #finalLat <- as.numeric(as.matrix(finalTable[1]))
  #finalLon <- as.numeric(as.matrix(finalTable[2]))
  #finalFreq <- as.numeric(as.matrix(finalTable[3]))
  #result <- list(finalLat = finalLat, finalLon = finalLon, finalFreq = finalFreq)
  result <- finalTable
}

triangulateLocation <-function (latitude, longitude) {
  # Call locationBikes with original coordinates
  mainLocation <- locationToBikes(latitude, longitude)
  
  # Call locationBikes with coordinates slightly positively moved
  positiveMain <- locationToBikes(latitude + 0.002, longitude + 0.002)
  
  # Call locationBikes with coordinates slightly positively moved
  negativeMain <- locationToBikes(latitude - 0.002, longitude - 0.002)
  
  print(class(mainLocation))
  print(class(positiveMain))
  print(class(negativeMain))
}

