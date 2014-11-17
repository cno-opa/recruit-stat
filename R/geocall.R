#geocall.R
#helper functions to get more descriptive geographies from ZIP codes

#TODO: PROFIT

require(jsonlite)
require(RCurl)
require(stringr)


init_geocall <- function() {
  #
  #

  geocall <- function(zip) {
    api_key <- "AIzaSyDb2JQ9Cllc4476oInOXZU4CLUUzOS3-bc"
    zip <- as.character(zip)
    url <- paste0("https://maps.googleapis.com/maps/api/distancematrix/json?origins=", zip, "&destinations=New+Orleans,+LA&units=imperial&key=", api_key)

    response <- getURL(url)
    response <- fromJSON(response)
    response$rows$elements <- data.frame(response$rows$elements)

    if(response$status != "OK") {
      response$status
    } else if(substr(response$origin_addresses, 1, 11) == "New Orleans") {
      substr(response$origin_addresses, 1, 11)
    } else if (response$rows$elements$status != "OK") {
      #if loop to check if "USA" is in response$origin_address. for states where distance matrix returns a flight
      if(grepl("USA", response$origin_address) == T) {
        substr(str_extract(response$origin_addresses, ", (.*)"), 3, 4)
      } else {
        response$rows$elements$status
      }
    } else if (response$rows$elements$duration$value < 3600) {
      "GNO"
    } else {
      substr(str_extract(response$origin_addresses, ", (.*)"), 3, 4)
    }
  }

  #for some reason, cannot vectorize the geocall function. throws error when it gets to last element in vector because of trailing garbage
  geoloop <- function(zips) {
    geo <- list()

    for(zip in zips) {
      geo <- append(geo, geocall(zip))
      print(paste("Geocoding", zip))
    }

    return(geo)
  }

  #
  #end init_geocall()
}
