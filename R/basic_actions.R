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
    
    if (!("user_key" %in% names(query)) || is.null(query$user_key)) query$user_key <- crunchbase_key()
    if (!("page" %in% names(query)) || is.null(query$page)) query$page <- 1    
    if (!("order" %in% names(query)) || is.null(query$order)) query$order <- crunchbase_order()
    
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
    warning("HTTP failure: ", p$status_code, "\n", p$headers$statusmessage, call. = FALSE)
    return(TRUE)
}

crunchbase_check <- function(p) {
    if (is.null(p$data$error)) 
        return(FALSE)
    
    message <- p$data$error$message
    warning("HTTP failure: ", p$data$error$code, "\n", message, call. = FALSE)
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

#' Set or get a CrunchBase user key.
#' 
#' Reads (or writes) the user key to an environment variable.
#' 
#' Request a user key from CrunchBase \url{https://developer.crunchbase.com/} 
#' in order to use the API. This key gets appended to GET queries via the 
#' \code{user_key} parameter. By saving the key to an environment variable, a 
#' user only needs to enter it once per session. 
#' 
#' @param force Overwrite existing key? If TRUE, the user will be prompted to 
#'  enter a user key even if one has already been entered before.
#' @export
crunchbase_key <- function(force = FALSE) {
    env <- Sys.getenv("CRUNCHBASE_KEY")
    if (!identical(env, "") && !force) 
        return(env)
    
    if (!interactive()) {
        stop("Please set env var CRUNCHBASE_KEY to your crunchbase user key", 
             call. = FALSE)
    }
    
    message("Couldn't find env var CRUNCHBASE_KEY. See ?crunchbase_key for more details.")
    message("Please enter your user_key and press enter:")
    key <- readline(": ")
    
    if (identical(key, "")) {
        stop("Crunchbase user_key entry failed", call. = FALSE)
    }
    
    message("Updating CRUNCHBASE_KEY environment variable")
    Sys.setenv(CRUNCHBASE_KEY = key)
    
    key
}