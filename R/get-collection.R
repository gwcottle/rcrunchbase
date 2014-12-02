#' Get a collections endpoint
#' 
#' Simplifies GETting multi-page collections endpoints. Instead of making 
#' repeated calls to \code{crunchbase_GET} with \code{page=n}, just use this.
#' 
#' @param path the path for the collections endpoint.
#' @param ... other arguments passed to crunchbase_GET
#' @export
#' @examples
#' crunchbase_get_collection(c("organization","facebook","current_team"))
#' crunchbase_get_collection("people") ## returns all people in CrunchBase
#' 
crunchbase_get_collection <- function(path, ...) {    
    # get first page of results and check the paging data to see how many more
    page_one <- crunchbase_GET(path, ...)
    page_one <- crunchbase_parse(page_one)
    if (is.null(page_one)) {
        warning("No results returned", call.=FALSE)
        return(NULL)
    }

    pages <- page_one[[c("paging","number_of_pages")]]
    
    # if there's only one page, we're done!
    if (pages < 2L) return(crunchbase_flatten_collection(list(page_one)))

    # otherwise, keep querying. store the results in a list
    message(pages, " pages in all, starting to download at ", 
        format(Sys.time(), "%X"))    
    
    getpage <- function(p) {
        cat(".")
        pg <- crunchbase_GET(path, page=p, ...)
        crunchbase_parse(pg)
    }
    
    rest <- lapply(2:pages, getpage)    
    output <- c(list(page_one), rest)    
    crunchbase_flatten_collection(output)
}


crunchbase_flatten_collection <- function(collection) {    
    if (length(collection) == 1)
        return(data.frame(collection[[1]]$items))
    
    alldf <- vector("list", length(collection))
    for (i in 1:length(collection)) {
        alldf[[i]] <- data.frame(collection[[i]]$items)
    }
    do.call("rbind", alldf)
}
