#' Get a collections endpoint
#' 
#' Simplifies GETting multi-page collections endpoints. Instead of making 
#' repeated calls to /code{crunchbase_GET} with /code{page=n}, just use this.
#' 
#' @param path the path for the collections endpoint
#' @param page.limit use if you only want a subset of the pages
#' @param delay length of time (in seconds) to pause between each GET request, 
#'  useful to avoid going over usage limits.
#' @param ... other arguments passed to /code{crunchbase_GET}
#' @export
#' 
crunchbase_get_collection <- function(path, page.limit=FALSE, delay=1.3, ...) {    
    # pull ALL items in a collection 
    # (easy if one page, need to access first page "paging" to get future pages otherwise)
    
    # get first page of results and check the paging data to see how many more (if any)
    pageOne <- crunchbase_GET(path, ...) %>% crunchbase_parse
    if (is.null(pageOne)) stop(paste(paste(path, collapse="/"), "is not a valid collection", sep=" "), call.=FALSE)
    if (page.limit) {
        pages <- pmin(pageOne[[c("data","paging","number_of_pages")]], page.limit)
    } else {
        pages <- pageOne[[c("data","paging","number_of_pages")]]}
    
    # if there's only one page, we're done!
    if (pages < 2) 
        return(list(pageOne))
    
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
    return(output)
}

crunchbase_flatten_collection <- function(collection) {
    # this should take an object output by crunchbase_get_collection (ie a list of lists)
    # and return just a dataframe of the collection data. just in case it ends up helping,
    # i append to each row the page of "collection" that the record came from.
    
    if (length(collection) == 1)
        return(data.frame(collection[[1]]$data$items, page=collection[[1]]$data$paging$current_page))
    
    alldf <- vector("list", length(collection))
    for (i in 1:length(collection)) {
        alldf[[i]] <- data.frame(collection[[i]]$data$items, page=collection[[i]]$data$paging$current_page)
    }
    do.call("rbind", alldf)
}

crunchbase_get_entities <- function(paths, exclude=no_exclude, delay=1.3, ...) {
    # as input, a vector of paths
    # eg: c("person/some-name", "person/another-name", "person/somebody")
    # as output, a list of lists, each list is the output of crunchbase_GET(path)
    # optionally, use exclude(node) to determine whether the node should be left out of the 
    # returned object -- saves you from retrieving stuff you don't want, saves time
    
    entities <- vector("list", length(paths))
    for (i in 1:length(paths)) {
        cat("Getting data for ", paths[[i]], "\n")
        Sys.sleep(delay)
        temp <- crunchbase_GET(paths[[i]], ...) %>% crunchbase_parse
        if (exclude(temp)) next
        
        cat("Adding ", paths[[i]], "to list", "\n")
        entities[[i]] <- temp
    }
    return(entities[sapply(entities, function(x) !is.null(x))])
}

crunchbase_expand_section <- function(node, relationship, delay=1.3, ...) {
    # takes as input a parsed node detail response and a relationship name 
    # (eg current_team, web_presences, etc) and returns the entire collection 
    # (all pages,if >1). 
    
    # the section we'll be working with
    section <- node[[c("data","relationships",relationship)]]
    
    # if it's all here, don't need to do anything
    if (section$paging$total_items <= 8) return(data.frame(section$items, page=1))
    
    # otherwise, get and flatten the necessary collection.
    # the section paging has the entire url for the first page,
    # we need to pass just the path part to get_collection
    crunchbase_get_collection(path=str_match(section$paging$first_page_url, "/v/2/(.+$)")[,2],
                              delay=delay,
                              ...) %>%
        crunchbase_flatten_collection()
}

crunchbase_onehop <- function() {
    # as input: parsed node detail, a specific relationships item that has a path (MUST!), and a 
    # relationships type on the other end:
    # returns the collections object on that end. for example: list of all people who work at this
    # company (or whatever)
}

# exclusion examples:
no_exclude <- function(node) {
    FALSE
}

# exclusion constructor function
word_exclude <- function(section, pattern) {
    excluder <- function(node) {
        if (class(try(node[[section]], silent=TRUE)) == "try-error") return(TRUE)
        is.null(node[[section]]) || !str_detect(node[[section]], ignore.case(pattern))
    }
    return(excluder)
}


exclude_noncal <- word_exclude(section=c("data", "properties", "bio"), pattern="berkeley")