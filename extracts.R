## some processing functions to move data to researchers
# the goal: strip some of the most relevant info from crunchbase_parse results
# and convert to single rows in a dataframe (for export to csv/excel)

# first: df flattener function
df_collapse <- function(df, column_names, col_sep, row_sep) {
  paste(do.call(paste, c(df[,column_names], sep=col_sep)), collapse=row_sep)
}

# take a list of df names from a parsed object, return 
# a named vector of df_collapsed strings

crunchbase_strip <- function(node) {
  permalink <- paste(node$metadata$www_path_prefix, tolower(node$data$type), "/", node$data$properties$permalink, sep="")
  bio <- node$data$properties$bio
    if (is.null(bio)) bio <- ""
  
  # degree info
  if (is.null(node$data$relationships$degrees$items)) {
    degrees <- ""} else {
      degrees <- df_collapse(node$data$relationships$degrees$items, 
                             column_names=c("degree_type_name", "degree_subject"), 
                             col_sep=" in ", 
                             row_sep="\n")}
  
  # experience info
  if (is.null(node$data$relationships$experience$items)) {
    experience <- ""} else {
      experience <- df_collapse(node$data$relationships$experience$items, 
                                column_names=c("organization_name", "title"), 
                                col_sep=": ", 
                                row_sep="\n")}
  
  # web presences
  if (is.null(node$data$relationships$websites$items)) {
    web <- ""} else {
      web <- df_collapse(node$data$relationships$websites$items, 
                         column_names=c("title", "url"), 
                         col_sep=": ", 
                         row_sep="\n")}

  
  c(crunchbase=permalink, bio=bio, degrees=degrees, experience=experience, web=web)
}

crunchbase_strip_list <- function(nodes) {
  lapply(nodes, crunchbase_strip) %>% do.call(rbind, .) %>% data.frame(stringAsFactors=FALSE)
}