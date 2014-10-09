#' @import plyr
#' @export
crunchbase_expand_section.cb_node <- function(node, relationship, ...) {
    expander <- function(r) {
        section <- node[[c("relationships", r)]]
        if(is.null(section)) return(NULL)
        
        # if it's all here, don't need to do anything
        if (section$paging$total_items <= 8) 
            return(data.frame(section$items))
        
        # otherwise, get and flatten the necessary collection.
        crunchbase_get_collection(path=stringr::str_match(section$paging$first_page_url, 
                                                          "/v/2/(.+$)")[,2], ...)
    }
    expanded <- lapply(relationship, expander)
    do.call(plyr::rbind.fill, expanded)
}

#' @import plyr
#' @export
crunchbase_expand_section.cb_nodes <- function(node, relationship, ...) {    
    expanded <- lapply(node, crunchbase_expand_section, relationship, ...)
    do.call(plyr::rbind.fill, expanded)
}

#' @import plyr
#' @export
crunchbase_expand_section.default <- function(node, relationship, ...) {
    expanded <- lapply(node, crunchbase_expand_section, relationship, ...)
    do.call(plyr::rbind.fill, expanded)    
}

#' Expand a section of a node detail
#' 
#' Node details include numerous sections. For instance, an organization record 
#' may include a "current team" section. These sections will only include the 
#' first 8 records in the section, along with a path to the full collection. 
#' This function calls \code{crunchbase_get_collection} to pull the entire 
#' collection. 
#' 
#' @param node a downloaded node, or multiple nodes as returned by 
#' \code{crunchbase_get_details}
#' @param relationship character: the name of the section to expand
#' @param ... other arguments passed to crunchbase_get_collection
#' @import stringr
#' @export
#' @examples
#' crunchbase_get_details("organization/facebook") %>%
#'     crunchbase_expand_section("current_team")
crunchbase_expand_section <- function(node, relationship, ...) {
    UseMethod("crunchbase_expand_section")
}
