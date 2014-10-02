#' Get a collections endpoint
#' 
#' Simplifies GETting multi-page collections endpoints. Instead of making 
#' repeated calls to \code{crunchbase_GET} with \code{page=n}, just use this.
#' 
#' @param path the path for the collections endpoint
#' @param page.limit use if you only want a subset of the pages
#' @param delay length of time (in seconds) to pause between each GET request, 
#'  useful to avoid going over usage limits.
#' @param ... other arguments passed to crunchbase_GET
#' @importFrom magrittr %>%
#' @export
#' @examples
#' crunchbase_get_collection(c("organization","facebook","current_team"))
#' crunchbase_get_collection("people") ## returns all people in CrunchBase
#' crunchbase_get_collection("organizations", location="Oakland")
#' 
crunchbase_get_collection <- function(path, flatten = TRUE, page.limit=FALSE, delay=1.3, ...) {    
    # pull ALL items in a collection 
    # (easy if one page, need to access first page "paging" to get future pages otherwise)
    
    # get first page of results and check the paging data to see how many more (if any)
    pageOne <- crunchbase_GET(path, ...) %>% crunchbase_parse
    if (is.null(pageOne)) stop(paste(paste(path, collapse="/"), "is not a valid collection", sep=" "), call.=FALSE)
    if (page.limit) {
        pages <- pmin(pageOne[[c("paging","number_of_pages")]], page.limit)
    } else {
        pages <- pageOne[[c("paging","number_of_pages")]]}
    
    # if there's only one page, we're done!
    if (pages < 2) {
        if (flatten) {
            return(crunchbase_flatten_collection(list(pageOne)))
                   } else {
                       return(list(pageOne))}
    }

    # otherwise, let's keep querying. store the results in a list, one element per page
    # we already have the first page
    output <- vector("list", pages)
    output[[1]] <- pageOne
    cat(pages, " pages in all, starting to download at ", format(Sys.time(), "%X"), "\n",sep="")
    
    # outputting the time at each iteration is helpful for me, but should
    # eventually be made optional (verbose=FALSE)
    for (i in 2:pages) {
        cat("Getting page ", i," at ", format(Sys.time(), "%X"), "\n",sep="")
        output[[i]] <- crunchbase_GET(path, page=i, ...) %>%
            crunchbase_parse
        Sys.sleep(delay)
    }
    if (flatten) return (crunchbase_flatten_collection(output)) else return(output)
}


#' Flatten a collections object
#' 
#' Each page of a collections object is returned as an element of a list. This
#' function flattens it out into one long data frame. Note that if you use 
#' \code{flatten=TRUE} in \code{crunchbase_get_collection}, then you don't need 
#' to use this function
#' 
#' @param collection a downloaded collections object, e.g. as returned 
#' by crunchbase_get_collection
#' @export
#' @examples
#' ## a list of all organizations listed in the CrunchBase database
#' crunchbase_get_collection("organizations") %>% crunchbase_flatten_collection
#' 
crunchbase_flatten_collection <- function(collection) {
    # this should take an object output by crunchbase_get_collection (ie a list of lists)
    # and return just a dataframe of the collection data. just in case it ends up helping,
    # i append to each row the page of "collection" that the record came from.
    
    if (length(collection) == 1)
        return(data.frame(collection[[1]]$items, page=collection[[1]]$paging$current_page))
    
    alldf <- vector("list", length(collection))
    for (i in 1:length(collection)) {
        alldf[[i]] <- data.frame(collection[[i]]$items, page=collection[[i]]$paging$current_page)
    }
    do.call("rbind", alldf)
}

#' Get one or more entity endpoints
#' 
#' Retrieve data about one or more CrunchBase entities. This is a convenient 
#' wrapper around the \code{crunchbase_GET} function, with the ability to 
#' wait between requests (to avoid usage limits) and exclude entities from 
#' your results.
#' 
#' The return value will be a list where each element is an entity detail 
#' node. \code{crunchbase_get_entities} is useful in conjunction with the 
#' results of \code{crunchbase_get_collection} (see examples)
#' 
#' @param paths a vector or list of paths, for instance 
#' c("person/some-name", "person/another-name", "person/somebody") or 
#' list(c("person", "some-name"), c("person", "another-name"), 
#' c("person", "somebody")). This can also be a data.frame with a column named 
#' "path," (for instance: the result of crunchbase_get_collection)
#' @param exclude any function that takes as its input a crunchbase entity 
#' and returns as its output either \code{TRUE} or \code{FALSE}
#' @param delay length of time in seconds to wait between GET requests, useful 
#' to avoid hitting usage limits
#' @param ... other arguments passed to \code{crunchbase_GET}
#' @importFrom magrittr %>%
#' @export
#' @examples
#' ## see details on all companies in Oakland
#' crunchbase_get_collection("organizations", location="Oakland") %>% 
#'     crunchbase_get_entities
#' 
crunchbase_get_entities <- function(paths, exclude=no_exclude, delay=1.3, ...) {
    # as input, a vector of paths
    # eg: c("person/some-name", "person/another-name", "person/somebody")
    # as output, a list of lists, each list is the output of crunchbase_GET(path)
    # optionally, use exclude(node) to determine whether the node should be left out of the 
    # returned object -- saves you from retrieving stuff you don't want, saves time
    if (is.data.frame(paths)) paths <- paths$path
    entities <- vector("list", length(paths))
    for (i in 1:length(paths)) {
        cat("Getting data for ", paths[[i]], "\n")
        Sys.sleep(delay)
        temp <- crunchbase_GET(paths[[i]], ...) %>% crunchbase_parse
        if (exclude(temp)) next
        
        cat("Adding ", paths[[i]], "to list", "\n")
        entities[[i]] <- temp
    }
    if (length(entities) == 0) return(entities)
    entities <- entities[sapply(entities, function(x) !is.null(x))]
    if (length(entities) == 1) return(entities[[1]]) else return(entities)
}

#' Expand a section of an entity detail
#' 
#' Node details include numerous sections. For instance, an organization record 
#' may include a "current team" section. These sections will only include the 
#' first 8 records in the section, along with a path to the full collection. 
#' This function calls \code{crunchbase_get_collection} to pull the entire 
#' collection. 
#' 
#' @param node a downloaded entity node
#' @param relationship character: the name of the section to expand
#' @param ... other arguments passed to crunchbase_get_collection
#' @export
#' @examples
#' crunchbase_get_entities(c("organization", "facebook")) %>%
#'     crunchbase_expand_section("current_team")
crunchbase_expand_section <- function(node, relationship, ...) {
    # takes as input a parsed node detail response and a relationship name 
    # (eg current_team, web_presences, etc) and returns the entire collection 
    # (all pages,if >1). 
    
    # the section we'll be working with
    section <- node[[c("relationships",relationship)]]
    if(is.null(section)) return(NULL)
    
    # if it's all here, don't need to do anything
    if (section$paging$total_items <= 8) return(data.frame(section$items, page=1))
    
    # otherwise, get and flatten the necessary collection.
    # the section paging has the entire url for the first page,
    # we need to pass just the path part to get_collection
    crunchbase_get_collection(path=stringr::str_match(section$paging$first_page_url, 
                                             "/v/2/(.+$)")[,2],
                              ...)
}

#' Don't exclude anything
#' 
#' This is the default exclusion function for \code{crunchbase_get_entities}, 
#' there isn't any reason to call it otherwise. It always returns \code{FALSE}, 
#' meaning don't exclude anything.
#' @export
no_exclude <- function(node) {
    FALSE
}

#' Construct exclusion functions based on a word or pattern.
#' 
#' @param section which section of the node is used for the exclusion?
#' @param pattern a string or regular expression to look for within the section
#' @param case.insensitive TRUE/FALSE, defaults to TRUE, meaning the search is 
#' case-insensitive.
#' @export
word_exclude <- function(section, pattern, ignore.case=TRUE) {
    if (ignore.case) pattern <- stringr::ignore.case(pattern)
    excluder <- function(node) {
        if (class(try(node[[section]], silent=TRUE)) == "try-error") return(TRUE)
        is.null(node[[section]]) || !stringr::str_detect(node[[section]], pattern)
    }
    return(excluder)
}

#' Just an example
#' @export
exclude_noncal <- function(node) {
    word_exclude(section=c("properties", "bio"), pattern="berkeley")(node) &&
        word_exclude(section = c("relationships", "degrees", "items", "organization_name"),
                     pattern="berkeley")(node)
}