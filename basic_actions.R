cb_query <- function(user_key = crunchbase_key(), ...) {
    list(user_key = user_key, ...)
}

crunchbase_GET <- function(endpoint, permalink=NULL, rels=NULL, query=cb_query()) {
    if (!require(httr)) {
        stop("the httr package is required, please install")
    }
    
    if (!is.null(rels) || endpoint %in% c("people", "organizations", "products")) {
        if (!("page" %in% names(query)) || is.null(query$page)) {
            query$page <- 1
            warning(paste("collections queries require a page parameter, page has defaulted to ",
                          query$page, sep=""),
                    call. = FALSE)
        }      
        
        if (!("order" %in% names(query)) || is.null(query$order)) {
            query$order = crunchbase_order()
            warning(paste("collections queries require an order parameter, order has defaulted to '",
                          query$order, "'", sep=""),
                    call. = FALSE)
        }
    }
    
    if (endpoint %in% c("person", "organization", "product", "funding-round", "fund-raise", "acquisition", "ipo") && is.null(permalink)) {
        stop("entity endpoints require a permalink")
    }
    
    request <- list(scheme = "http",
                    hostname = "api.crunchbase.com",
                    path = paste("v", "2", endpoint, permalink, rels, sep="/"),
                    query = query)
    class(request) <- "url"                
    request <- gsub("%5F", "_", build_url(request))  
    GET(request)
}

crunchbase_order <- function(mod="created_at", type="asc") {
    paste(mod, type, sep=" ")
}

crunchbase_check <- function(p) {
    if (is.null(p$data$error)) 
        return(invisible())
    
    message <- p$data$error$message
    warning("HTTP failure: ", p$data$error$code, "\n", message, call. = FALSE)
}

crunchbase_parse <- function(req) {
    if (!require(httr)) {
        stop("the httr package is required, please install")
    }    
    text <- content(req, as = "text")
    if (identical(text, "")) 
        stop("No output to parse", call. = FALSE)
    p <- jsonlite::fromJSON(text, simplifyDataFrame = TRUE)
    crunchbase_check(p)
    p
}

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