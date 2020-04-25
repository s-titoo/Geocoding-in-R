# ///////////////////////////////////////////////
# SUPLEMENTARY FUNCTIONS
# ///////////////////////////////////////////////

# ///////////////////////////////////////////////
# 1. GENERATE API CALLS
# ///////////////////////////////////////////////

url_nominatim_rev_geocoding <- function(coordinates_url, language_url, email_url) {
    
    # load libraries
    library(RCurl)
    
    # convert everything into a data frame
    
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
    
    # nominatim reverse api url
    url_nominatim_reverse_api <- "https://nominatim.openstreetmap.org/reverse"
    
    # parameters
    
    lat <- coordinates$lat
    lon <- coordinates$lng
    
    parameters_url <- paste0("?format=json", "&lat=", lat, "&lon=", lon,
                             "&addressdetails=1&extratags=1","&accept-language=",
                             language_url, "&zoom=18", "&email=", email_url)
    
    # construct search request for geocode
    url_nominatim_reverse_call <- paste0(url_nominatim_reverse_api, parameters_url)
    
    # return data frame with coordinates and API call
    coordinates$api_call <- url_nominatim_reverse_call
    
    return(coordinates)
    
}

# ///////////////////////////////////////////////
# 2. EXTRACT DATA FROM JSON
# ///////////////////////////////////////////////

get_rev_geodata_from_json_nominatim <- function(geodata_json) {
    
    # load library
    library(jsonlite)
    
    # convert json output into r object
    geodata <- lapply(geodata_json, fromJSON,simplifyVector = FALSE)
    
    # extract address, city and country
    
    address_df <- data.frame(address = NA, pub_name = NA, street_name = NA,
                             house_number = NA, suburb = NA, postcode = NA,
                             state_district = NA, country = NA)
    
    for(i in 1:length(geodata)) {
        
        if(length(geodata[[i]]) != 0) {
            
            # get data
            
            address <- geodata[[i]]$display_name
            pub_name <- geodata[[i]]$address$pub
            street_name <- geodata[[i]]$address$road
            house_number <- geodata[[i]]$address$house_number
            suburb <- geodata[[i]]$address$suburb
            postcode <- geodata[[i]]$address$postcode
            state_district <- geodata[[i]]$address$state_district
            country <- geodata[[i]]$address$country
            
            # get rid of NULLs
            
            info <- list(address, pub_name, street_name, house_number,
                         suburb, postcode, state_district, country)
            
            for (j in 1:length(info)) {
                if (is.null(info[[j]])) info[[j]] <- NA
            }
            
            # create output data frame
            
            address_df[i, ] <- info
            
        } else {
            address_df[i, ] <- NA
        }
    }
    
    return(address_df)
    
}

# ///////////////////////////////////////////////
# MAIN FUNCTION
# ///////////////////////////////////////////////

rev_geocode_nominatim <- function(coordinates, language = "en", email) {
    
    # load libraries
    library(RCurl)
    
    # construct url for reverse geocoding
    rev_geocoding_info <- url_nominatim_rev_geocoding(coordinates, language, email)
    
    # get data from nominatim
    # wait 3 seconds between each call
    
    geodata_json <- list()
    
    for (i in 1:dim(rev_geocoding_info)[1]) {
        geodata_json[i] <- getURL(rev_geocoding_info$api_call[i])
        Sys.sleep(3)
    }
    
    # get data from json output
    geodata_df <- rev_geocoding_info[, c("lat", "lng")]
    geodata_df[, 3:10] <- get_rev_geodata_from_json_nominatim(geodata_json)
    
    # return dataframe with the geodata
    return(geodata_df)
    
}