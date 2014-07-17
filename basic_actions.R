mGET <- memoise(httr::GET)

crunchbase_order <- function(mod="created_at", type="asc") {
    paste(mod, type, sep=" ")
}

crunchbase_GET <- function(path, ...) {
    
    query <- list(...)
    if (sum(names(query) == "") > 0) stop("all query parameters must be named")
    
    if (!("user_key" %in% names(query)) || is.null(query$user_key)) query$user_key <- crunchbase_key()
    if (!("page" %in% names(query)) || is.null(query$page)) query$page <- 1    
    if (!("order" %in% names(query)) || is.null(query$order)) query$order <- crunchbase_order()
    
    if (length(path) > 1) path <- paste(path, collapse="/")
    
    request <- list(scheme = "http",
                    hostname = "api.crunchbase.com",
                    path = paste("v", "2", path, sep="/"),
                    query = query)
    class(request) <- "url"                
    request <- gsub("%5F", "_", build_url(request))
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

crunchbase_parse <- function(req) {
    if (is.null(req)) {
        warning("No output to parse", call. = FALSE)
        return(NULL)
    }
    text <- content(req, as = "text")
    if (identical(text, "")) {
        warning("No output to parse", call. = FALSE)
        return(NULL)
    }
    p <- jsonlite::fromJSON(text, simplifyDataFrame = TRUE)
    if (crunchbase_check(p)) return(NULL)
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