managed_call <- function(f, events = 44L, every = 60L) {
    force(f)
    minute_ <- rep(NA, events)
    function(...) {       
        m_dif <- as.numeric(Sys.time() - minute_, units = "secs")
        minute_[!is.na(m_dif) & m_dif > every] <<- NA
        calls_remaining <- sum(is.na(minute_))
        if (!calls_remaining) {
            message("Close to API limit, pausing for ", 
                    round(every - max(m_dif), 3), 
                    " seconds")
            Sys.sleep(every - max(m_dif))
            minute_[which.max(m_dif)] <- NA
            minute_[Position(is.na, minute_)] <<- Sys.time()
            f(...)
        } else {
            minute_[Position(is.na, minute_)] <<- Sys.time()
            f(...)
        }
    }
}

#' @importFrom httr GET
mGET <- managed_call(httr::GET, events=44L, every=60L)

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
#' @param ... any other query parameters, see 
#' \url{https://developer.crunchbase.com/docs} for details
#' 
#' @note \code{crunchbase_GET} keeps track of API calls and will automatically 
#' pause a query for the appropriate amount of time in order to keep from making 
#' more than 44 calls per minute. Furthermore, results are cached for the length 
#' of your R session (using \code{memoise}). To make it easier to use the API 
#' collections filters (currently only applies to organization queries), you can 
#' pass \code{data.frame}s directly to \code{crunchbase_GET}, as long as it 
#' contains columns named "type" and "uuid," as the \code{locations} and 
#' \code{categories} collections do.
#' @export
#' @examples
#' x <- crunchbase_GET(c("people", "bill-gates"))
#' x <- crunchbase_GET("people/bill-gates")
crunchbase_GET <- function(path, ...) {
    
    request <- crunchbase_build_url(path, ...)

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
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
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
    p <- jsonlite::fromJSON(text, simplifyDataFrame = TRUE, flatten=TRUE)
    if (crunchbase_check(p)) return(NULL)
    if (any(names(p$data) == "paging")) {
        class_p <- "cb_collection"
    } else {
        class_p <- "cb_node"
    }
    structure(p$data, class=class_p)
}