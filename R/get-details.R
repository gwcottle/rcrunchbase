#' @import stringr
get_pathnames <- function(path_df) {
    nm <- names(path_df)
    paths <- stringr::str_detect(nm, "api_path")
    nm[paths]
}

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
#' c("person", "somebody")). This can also be a data.frame (for instance: the 
#' result of crunchbase_get_collection)
#' @param df_path character if \code{paths} is a data.frame, this should be the 
#' name or integer index of the column containing the query paths. Defaults to 
#' "path"
#' @param filter any function that takes as its input a crunchbase node detail 
#' and returns as its output either \code{TRUE} (include) or \code{FALSE}
#' @param ... other arguments passed to \code{crunchbase_GET}
#' @export
#' 
crunchbase_get_details <- function(paths, df_path = NULL, filter=no_filter, ...) {
    if (is.data.frame(paths)) {
      if (!is.null(df_path)) paths <- paths[[df_path[1]]]
      else {
          pathnames <- get_pathnames(paths)
          if (length(pathnames) == 0)
              stop("If you use a data frame as input to crunchbase_get_details, you need to specify which column contains the path to the API endpoint")
          if (length(pathnames) > 1) {
              namelist <- paste(pathnames, collapse="\n")
              stop(paste("Which column in your data frame contains paths to the API endpoints? Is it one of these?", namelist, collapse="\n", sep="\n"))
          }
          paths <- paths[[pathnames]]
      }
    } 
    
    get_ents <- function(path, ...) {
        cat("_")
        temp <- crunchbase_parse(crunchbase_GET(path, ...))
        if (!filter(temp)) return(NULL)
        cat(".")
        temp
    }
    
    entities <- lapply(paths, get_ents)
    
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
    pattern <- stringr::fixed(pattern, ignore_case=ignore.case)
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