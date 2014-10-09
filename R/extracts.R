## some processing functions to move data to researchers
# the goal: strip some of the most relevant info from crunchbase_parse results
# and convert to single rows in a dataframe (for export to csv/excel)

# first: df flattener function (not exported)
df_collapse <- function(df, column_names, col_sep, row_sep) {
  paste(do.call(paste, c(df[,column_names], sep=col_sep)), collapse=row_sep)
}


#' Collapse a crunchbase node into a spreadsheet
#' 
#' Crunchbase nodes are represented as hierarchical lists. Sometimes, you just 
#' want to see a summarized table version of the data. This function collapses 
#' a single node into a data.frame
#' 
#' @param node the node object to be collapsed
#' @export
#' @examples
#' crunchbase_GET("person/dan-rubinstein") %>% crunchbase_parse %>% crunchbase_strip
# take a list of df names from a parsed object, return 
# a named vector of df_collapsed strings
crunchbase_strip <- function(node) {
  permalink <- paste(tolower(node$type), "/", node$properties$permalink, sep="")
  bio <- node$properties$bio
    if (is.null(bio)) bio <- ""
  
  # degree info
  if (is.null(node$relationships$degrees$items)) {
    degrees <- ""} else {
      degrees <- df_collapse(node$relationships$degrees$items, 
                             column_names=c("degree_type_name", "degree_subject"), 
                             col_sep=" in ", 
                             row_sep="\n")}
  
  # experience info
  if (is.null(node$relationships$experience$items)) {
    experience <- ""} else {
      experience <- df_collapse(node$relationships$experience$items, 
                                column_names=c("organization_name", "title"), 
                                col_sep=": ", 
                                row_sep="\n")}
  
  # web presences
  if (is.null(node$relationships$websites$items)) {
    web <- ""} else {
      web <- df_collapse(node$relationships$websites$items, 
                         column_names=c("title", "url"), 
                         col_sep=": ", 
                         row_sep="\n")}

  
  c(crunchbase=permalink, bio=bio, degrees=degrees, experience=experience, web=web)
}

#' Collapse a list of nodes into one data.frame
#' 
#' Uses \code{crunchbase_strip} on every element of a list of nodes, and then 
#' returns the result as one \code{data.frame}, which can be exported or whatever.
#' @export
crunchbase_strip_list <- function(nodes) {
  stripped <- lapply(nodes, crunchbase_strip) 
  stripped <- do.call(rbind, stripped)
  data.frame(stripped, stringsAsFactors=FALSE)
}