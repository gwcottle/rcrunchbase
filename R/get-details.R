#' Get one or more entity endpoints
#' 
#' Retrieve data about one or more CrunchBase entities. This is a convenient 
#' wrapper around the \code{crunchbase_GET} function, with the ability to 
#' wait between requests (to avoid usage limits) and exclude entities from 
#' your results.
#' 
#' The return value will be a list where each element is an entity detail 
#' node. \code{crunchbase_get_details} is useful in conjunction with the 
#' results of \code{crunchbase_get_collection} (see examples)
#' 
#' @param paths a vector or list of paths, for instance 
#' c("person/some-name", "person/another-name", "person/somebody") or 
#' list(c("person", "some-name"), c("person", "another-name"), 
#' c("person", "somebody")). This can also be a data.frame with a column named 
#' "path," (for instance: the result of crunchbase_get_collection)
#' @param filter any function that takes as its input a crunchbase node detail 
#' and returns as its output either \code{TRUE} (include) or \code{FALSE}
#' @param speed maximum number of requests per minute
#' @param ... other arguments passed to \code{crunchbase_GET}
#' @export
#' 
crunchbase_get_details <- function(paths, ..., df_path = "path", filter=no_filter, speed=44L) {
    if (is.data.frame(paths)) paths <- paths[[df_path]]
    
    get_ents <- function(path, ...) {
        temp <- crunchbase_parse(crunchbase_GET(path, ...))
        if (!filter(temp)) return(NULL)
        temp
    }
    
    entities <- lapply(paths, delay(speed, 60L, get_ents))
    
    if (length(entities) == 0) return(entities)
    entities <- entities[sapply(entities, function(x) !is.null(x))]
    structure(entities, class = "cb_nodes")
}

#' Don't filter anything
#' 
#' This is the default exclusion function for \code{crunchbase_get_entities}, 
#' there isn't any reason to call it otherwise. It always returns \code{FALSE}, 
#' meaning don't exclude anything.
#' @export
no_filter <- function(node) {
    TRUE
}

#' Construct filter functions based on a word or pattern.
#' 
#' @param section which section of the node is used for the exclusion?
#' @param pattern a string or regular expression to look for within the section
#' @param case.insensitive TRUE/FALSE, defaults to TRUE, meaning the search is 
#' case-insensitive.
#' @export
word_filter <- function(section, pattern, ignore.case=TRUE) {
    if (ignore.case) pattern <- stringr::ignore.case(pattern)
    filter <- function(node) {
        if (class(try(node[[section]], silent=TRUE)) == "try-error") return(FALSE)
        !is.null(node[[section]]) && stringr::str_detect(node[[section]], pattern)
    }
    return(filter)
}

#' Just an example
#' @export
filter_cal <- function(node) {
    word_filter(section=c("properties", "bio"), pattern="berkeley")(node) ||
        word_filter(section = c("relationships", "degrees", "items", "organization_name"),
                     pattern="berkeley")(node)
}