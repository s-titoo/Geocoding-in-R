# ///////////////////////////////////////////////
# SUPLEMENTARY FUNCTIONS
# ///////////////////////////////////////////////

# ///////////////////////////////////////////////
# 1. GENERATE API CALLS
# ///////////////////////////////////////////////

url_google_rev_geocoding <- function(coordinates_url, key_url) {
    
    # load libraries
    library(RCurl)
    
    # convert everything into data frame
    
    if (is.matrix(coordinates_url) || is.data.frame(coordinates_url)) {
        coordinates <- data.frame(matrix(NA, nrow(coordinates_url), ncol(coordinates_url)))
        names(coordinates) <- c("lat", "lng")
        coordinates[, 1] <- coordinates_url[, 1]
        coordinates[, 2] <- coordinates_url[, 2]
    } else if (is.list(coordinates_url)) {
        coordinates <- data.frame(matrix(NA, nrow = length(coordinates_url), ncol = 2))
        names(coordinates) <- c("lat", "lng")
        for (i in 1:length(coordinates_url)) {
            coordinates[i, 1] <- coordinates_url[[i]][1]
            coordinates[i, 2] <- coordinates_url[[i]][2]
        }
    } else if (is.vector(coordinates_url)) {
        coordinates <- data.frame(lat = NA, lng = NA)
        coordinates[1,1] <- coordinates_url[1]
        coordinates[1,2] <- coordinates_url[2]
    }
    
    coordinates$lat_lng <- paste0(coordinates$lat, ",", coordinates$lng)
    
    # google gecoding api url
    url_geocoding_api <- "https://maps.googleapis.com/maps/api/geocode/"
    
    # construct search request for reverse geocoding
    url_rev_geocoding_call <- paste0(url_geocoding_api, "json",
                                     "?latlng=", coordinates$lat_lng, "&key=", key_url)
    
    # return data frame with coordinates and API call
    
    coordinates$api_call <- url_rev_geocoding_call
    
    return(coordinates)
    
}

# ///////////////////////////////////////////////
# 2. EXTRACT DATA FROM JSON
# ///////////////////////////////////////////////

get_rev_geodata_from_json_google <- function(geodata_json) {
    
    # load library
    library(jsonlite)
    
    # convert json output into r object
    geodata <- lapply(geodata_json, fromJSON,simplifyVector = FALSE)
    
    # extract address, city and country from the json output
    
    address_df <- data.frame(address = NA, city = NA, country = NA)
    
    for (i in 1:length(geodata)) {
        
        if (geodata[[i]]$status=="OK") {
            
            # extract address
            address <- geodata[[i]]$results[[1]]$formatted_address
            
            # find out how many elements there are in "address_components"
            n <- length(geodata[[i]]$results[[1]]$address_components)
            
            # extract city and country
            
            for (j in 1:n) {
                
                # extract type of "address_components"
                type <- geodata[[i]]$results[[1]]$address_components[[j]]$types[[1]]
                
                # extract city and country
                
                if (type == "postal_town") {
                    city <- geodata[[i]]$results[[1]]$address_components[[j]]$long_name 
                } else if (type == "country") {
                    country <- geodata[[i]]$results[[1]]$address_components[[j]]$long_name
                }
                
            }
            
            # prepare output
            address_df[i, ] <- c(address, city, country)
            
        } else {
            address_df[i, ] <- NA
        }
        
    }
    
    return(address_df)
    
}

# ///////////////////////////////////////////////
# MAIN FUNCTION
# ///////////////////////////////////////////////

rev_geocode_google <- function(coordinates, key) {
    
    # load libraries
    library(RCurl)
    
    # construct url for reverse geocoding
    rev_geocoding_info <- url_google_rev_geocoding(coordinates, key)
    
    # get data from google
    geodata_json <- getURL(rev_geocoding_info$api_call)
    
    # get data from json output
    geodata_df <- rev_geocoding_info[, c("lat", "lng")]
    geodata_df[, 3:5] <- get_rev_geodata_from_json_google(geodata_json)
    
    # return dataframe with the geodata
    return(geodata_df)
    
}