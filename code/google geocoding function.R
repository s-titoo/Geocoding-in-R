# ///////////////////////////////////////////////
# SUPLEMENTARY FUNCTIONS
# ///////////////////////////////////////////////

# ///////////////////////////////////////////////
# 1. CHECK ARGUMENTS
# ///////////////////////////////////////////////

check_arguments <- function(search_query_arg, fields_arg, key_arg) {
    
    # create empty error messages
    msg_1 <- msg_2 <- msg_3 <- ""
    
    # check if "search_query" argument is a string or
    # a list/vector/data frame with string data
    
    if(is.data.frame(search_query_arg) || is.matrix(search_query_arg)) {
        is_df <- TRUE
    } else {
        is_df <- FALSE
    }
    
    if(is_df) {
        if(dim(search_query_arg)[2] > 1) {
            error_1 <- TRUE
        } else if(!is.character(search_query_arg[, 1])) {
            error_1 <- TRUE
        } else {
            error_1 <- FALSE
        }
    }
    
    if(!is_df && is.list(search_query_arg)) {
        match_1 <- c()
        for(i in 1:length(search_query_arg)) {
            match_1[i] <- !is.character(search_query_arg[[i]])
        }
        if(any(match_1)) {
            error_1 <- TRUE
        } else {
            error_1 <- FALSE
        }
    }
    
    if(!is_df && !is.list(search_query_arg)) {
        if(!is.character(search_query_arg)) {
            error_1 <- TRUE
        } else {
            error_1 <- FALSE
        }
    }
    
    if(error_1) {
        msg_1 <- "Error: search_query argument (or any of its elements) is not of a string type"
    }
    
    # check if "fields" argument:
    # consists of a single word only - "all"
    # OR
    # * is a string vector of length <=3
    # * and consists of words 'coordinates', 'address', 'contacts' only
    
    match_2 <- fields_arg %in% c("coordinates", "address", "contacts")
    
    if (!(fields_arg %in% "all" && length(unique(fields_arg)) == 1)) {
        if (!all(match_2) || length(unique(fields_arg)) > 3) {
            msg_2 <- paste0("Error: fields argument must be ",
                            "either a combination of 'coordinates', ",
                            "'address', and 'contacts' or a single ",
                            "word 'all'")
        }
    }
    
    # check if "key" argument is a string
    
    if(is.list(key_arg)) {
        if(length(key_arg) > 1) {
            error_3 <- TRUE
        } else if(!is.character(key_arg[[1]])) {
            error_3 <- TRUE
        } else {
            error_3 <- FALSE
        }
    } else if(!is.character(key_arg) || length(key_arg) > 1) {
        error_3 <- TRUE
    } else {
        error_3 <- FALSE
    }
    
    if(error_3) {
        msg_3 <- "Error: key argument is not of a string type"
    }
    
    # return error messages (if any)
    
    errors <- c(msg_1, msg_2, msg_3)
    
    if(any(errors!="")) {
        errors <- paste(errors[errors!=""], collapse = "\n")
        return(list(TRUE, errors))
    } else {
        return(list(FALSE))
    }
    
}

# ///////////////////////////////////////////////
# 2. GENERATE API CALLS
# ///////////////////////////////////////////////

url_google_geocoding <- function(search_query_url, key_url) {
    
    # load libraries
    library(RCurl)
    
    # convert input into a list
    search_query_url <- sapply(search_query_url, as.list)
    
    # google gecoding api url
    url_geocoding_api <- "https://maps.googleapis.com/maps/api/geocode/"
    
    # percent-encode search request
    search_query_url <- sapply(search_query_url, URLencode)
    
    # construct search request for geocode
    url_geocoding_call <- paste0(url_geocoding_api, "json",
                                 "?address=", search_query_url, "&key=", key_url)
    
    return(url_geocoding_call)
    
}

# ///////////////////////////////////////////////

url_google_place_search <- function(search_query_url, key_url) {
    
    # load libraries
    library(RCurl)
    
    # convert input into a list
    search_query_url <- sapply(search_query_url, as.list)
    
    # google places api url
    url_places_api <- "https://maps.googleapis.com/maps/api/place/"
    
    # percent-encode search request
    search_query_url <- sapply(search_query_url, URLencode)
    
    # construct search request for place id
    url_place_search_call <- paste0(url_places_api, "findplacefromtext/",
                                    "json", "?input=", search_query_url,
                                    "&inputtype=textquery","&fields=place_id",
                                    "&key=", key_url)
    
    return(url_place_search_call)
    
}

# ///////////////////////////////////////////////

url_google_place_details <- function(place_id_url, key_url) {
    
    # load libraries
    library(RCurl)
    
    # google places api url
    url_places_api <- "https://maps.googleapis.com/maps/api/place/"
    
    # in case you would want to add "fields" as an argument
    # fields_url <- paste(fields_url, collapse = ",")
    
    # construct search request for place details
    url_place_details_call <- paste0(url_places_api, "details/",
                                     "json", "?place_id=", place_id_url,
                                     "&fields=formatted_phone_number,website",
                                     "&key=", key_url)
    
    return(url_place_details_call)
    
}

# ///////////////////////////////////////////////
# 3. EXTRACT DATA FROM JSON
# ///////////////////////////////////////////////

get_geodata_from_json_google <- function(geodata_json) {
    
    # load library
    library(jsonlite)
    
    # convert json output into r object
    geodata <- lapply(geodata_json, fromJSON,simplifyVector = FALSE)
    
    # extract coordinates, address and city name
    
    lat_lng_a <- data.frame(lat = NA, lng = NA, address = NA, city = NA)
    
    for (i in 1:length(geodata)) {
        
        if (geodata[[i]]$status=="OK") {
            
            # extract coordinates and address
            
            lat <- geodata[[i]]$results[[1]]$geometry$location$lat
            lng <- geodata[[i]]$results[[1]]$geometry$location$lng
            address <- geodata[[i]]$results[[1]]$formatted_address
            
            # find out how many elements there are in "address_components"
            n <- length(geodata[[i]]$results[[1]]$address_components)           
            
            # extract city and country
            
            for (j in 1:n) {
                
                # extract the type of the "address_components"
                type <- geodata[[i]]$results[[1]]$address_components[[j]]$types[[1]]
                
                # extract the city name
                
                if (type == "postal_town") {
                    city <- geodata[[i]]$results[[1]]$address_components[[j]]$long_name 
                }
                
            }                
            
            lat_lng_a[i, ] <- c(lat, lng, address, city)
            
        } else {
            lat_lng_a[i, ] <- NA
        }
        
    }
    
    return(lat_lng_a)
    
}

# ///////////////////////////////////////////////

get_place_id_from_json_google <- function(place_json) {
    
    # load library
    library(jsonlite)
    
    # convert json output into r object
    place_search <- lapply(place_json, fromJSON,simplifyVector = FALSE)
    
    # extract place id
    
    place_id <- list()
    
    for (i in 1:length(place_search)) {
        
        if (place_search[[i]]$status=="OK") {
            place_id[[i]] <- place_search[[i]]$candidates[[1]]$place_id
        } else {
            place_id[[i]] <- NA
        }
    }
    
    return(place_id)
    
}

# ///////////////////////////////////////////////

get_contacts_from_json_google <- function(place_details_json) {
    
    # load library
    library(jsonlite)
    
    # convert json output into r object
    place_details <- lapply(place_details_json, fromJSON, simplifyVector = FALSE)
    
    # extract phone number and website
    
    contacts <- data.frame("phone number" = NA, "website" = NA)
    
    for (i in 1:length(place_details)) {
        
        if (place_details[[i]]$status=="OK") {
            
            # get data
            
            phone_number <- place_details[[i]]$result$formatted_phone_number
            website <- place_details[[i]]$result$website
            
            # get rid of NULLs
            
            info <- list(phone_number, website)
            
            for (j in 1:length(info)) {
                if (is.null(info[[j]])) info[[j]] <- NA
            }
            
            # create output data frame
            contacts[i, ] <- info
            
        } else {
            contacts[i, ] <- NA
        }
    }
    
    return(contacts)
    
}

# ///////////////////////////////////////////////
# MAIN FUNCTION
# ///////////////////////////////////////////////

geocode_google <- function(search_query, fields = "coordinates", key) {
    
    # STOP RUNNING THE FUNCTION IF ARGUMENTS ARE INCORRECT
    
    errors <- check_arguments(search_query, fields, key)
    
    if (errors[[1]]) {
        stop(errors[[2]])
    }
    
    # LOAD LIBRARIES
    
    library(RCurl)
    
    # EXTRACT COORDINATES
    
    if (any(c("coordinates", "address") %in% fields) || "all" %in% fields) {
        
        # construct url for geocoding
        url_geocode <- url_google_geocoding(search_query, key)
        
        # get data from google
        geodata_json <- getURL(url_geocode)
        
        # get data from json output
        
        geodata_df <- as.data.frame(sapply(search_query, as.character),
                                    stringsAsFactors = FALSE)
        names(geodata_df) <- "search query"
        rownames(geodata_df) <- NULL
        geodata_df[, 2:5] <- get_geodata_from_json_google(geodata_json)
        
        # return dataframe with the geodata
        
        if (all(c("coordinates", "address") %in% fields) || "all" %in% fields) {
            geodata_df
        } else if ("coordinates" %in% fields) {
            geodata_df <- geodata_df[, 1:3]
        } else {
            geodata_df <- geodata_df[, c(1, 4:5)]
        }
        
    }
    
    # EXTRACT CONTACTS
    
    if ("contacts" %in% fields || "all" %in% fields) {
        
        # /// get place_id from Place Search API ///
        
        # construct url for place search
        url_place_search <- url_google_place_search(search_query, key)
        
        # get data from google
        place_json <- getURL(url_place_search)
        
        # get place_id from json output
        place_id <- get_place_id_from_json_google(place_json)
        
        # /// get contacts from Place Details API ///
        
        # construct url for place details
        url_place_details <- url_google_place_details(place_id, key)
        
        # get data from google
        place_details_json <- getURL(url_place_details)
        
        # get place_id from json output
        contacts <- get_contacts_from_json_google(place_details_json)
        
        # /// add contacts to our output data frame ///
        
        if (!exists("geodata_df")) {
            geodata_df <- as.data.frame(sapply(search_query, as.character),
                                        stringsAsFactors = FALSE)
            names(geodata_df) <- "search query"
            rownames(geodata_df) <- NULL
        }
        
        geodata_df[, c("phone", "web page")] <- contacts
        
    }
    
    return(geodata_df)
    
}