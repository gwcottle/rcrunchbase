mGET <- memoise::memoise(httr::GET)

#' Building order parameters for collections queries.
#' 
#' This is a pretty useless helper function.
#' @param mod Ordering function (eg created_at, modified_at, etc)
#' @param type Order (asc or desc)
#' @export
crunchbase_order <- function(mod="created_at", type="asc") {
    paste(mod, type, sep=" ")
}

#' Query CrunchBase.
#' 
#' Query CrunchBase via a GET request. All of the CrunchBase API's 
#' functionality comes through GET requests. This function collects any 
#' query parameters and GETs the resulting URL
#' 
#' @param path A query endpoint, presented either as a single string 
#'  (e.g. "people" or "person/johndoe") or as a vector that drills down a 
#'  hierarchy (e.g. c("person", "johndoe"))
#'  
#' @param ... any other query parameters, each entered as parameter = "value"
#' @export
#' @examples
#' x <- crunchbase_GET(c("person", "bill-gates"))
#' x <- crunchbase_GET("person/bill-gates")
#' crunchbase_GET("organizations", location="San Francisco")
crunchbase_GET <- function(path, ...) {
    
    query <- list(...)
    
    if (!("user_key" %in% names(query)) || is.null(query$user_key)) 
        query$user_key <- crunchbase_key()
    
    path <- paste(path, collapse="/")
    
    request <- list(scheme = "http",
                    hostname = "api.crunchbase.com",
                    path = paste("v", "2", path, sep="/"),
                    query = query)
    class(request) <- "url"                
    request <- gsub("%5F", "_", httr::build_url(request))
    p <- mGET(request)
    
    if (crunchbase_GET_audit(p)) {
        return(NULL)
    } else {return(p)}
}

crunchbase_GET_audit <- function(p) {
    if (p$status_code < 400) return(FALSE)    
    warning("HTTP failure: ", p$status_code, " ", 
            p$headers$statusmessage, 
            call. = FALSE)
    return(TRUE)
}

crunchbase_check <- function(p) {
    if (is.null(p$data$error)) 
        return(FALSE)
    
    message <- p$data$error$message
    warning(message, " (error ", p$data$error$code, ")", call. = FALSE)
    return(TRUE)
}

#' Parse raw CrunchBase API responses.
#' 
#' You only need to use this if you're using the basic /code{crunchbase_GET} function. 
#' Other functions automatically parse the responses. 
#' @export
#' @examples
#' x <- crunchbase_GET(c("person", "bill-gates"))
#' crunchbase_parse(x)
crunchbase_parse <- function(req) {
    if (is.null(req)) {
        warning("No output to parse", call. = FALSE)
        return(NULL)
    }
    text <- httr::content(req, as = "text")
    if (identical(text, "")) {
        warning("No output to parse", call. = FALSE)
        return(NULL)
    }
    p <- jsonlite::fromJSON(text, simplifyDataFrame = TRUE)
    if (crunchbase_check(p)) return(NULL)
    p$data
}