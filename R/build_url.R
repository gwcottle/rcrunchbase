#' @importFrom httr build_url
crunchbase_build_url <- function(path, ...) {
    
    query <- prep_params(...)
    
    if (!("user_key" %in% names(query)) || is.null(query$user_key)) 
        query$user_key <- crunchbase_key()
    
    path <- paste(path, collapse="/")
    
    request <- list(scheme = "https",
                    hostname = "api.crunchbase.com",
                    path = paste("v", "3", path, sep="/"),
                    query = query)
    class(request) <- "url"                
    request <- gsub("%5F", "_", httr::build_url(request))
    request
}
