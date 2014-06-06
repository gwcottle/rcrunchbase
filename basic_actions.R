require("httr")

crunchbase_GET <- function(path, ..., page=1, user_key = crunchbase_key(), order = crunchbase_order()) {
  auth <- user_key
  query <- list(user_key=auth, page=page, order=order)
  req <- GET("http://api.crunchbase.com/", path = path, query=query, ...)
  
  req
}

crunchbase_order <- function(mod="created_at", type="asc") {
  paste(mod, type, sep=" ")
}

crunchbase_check <- function(p) {
  if (is.null(p$data$error)) 
    return(invisible())
  
  message <- p$data$error$message
  warning("HTTP failure: ", p$data$error$code, "\n", message, call. = FALSE)
}

crunchbase_parse <- function(req) {
  text <- content(req, as = "text")
  if (identical(text, "")) 
    stop("No output to parse", call. = FALSE)
  p <- jsonlite::fromJSON(text, simplifyDataFrame = TRUE)
  crunchbase_check(p)
  p
}

crunchbase_key <- function(force = FALSE) {
  env <- Sys.getenv("CRUNCHBASE_KEY")
  if (!identical(env, "") && !force) 
    return(env)
  
  if (!interactive()) {
    stop("Please set env var CRUNCHBASE_KEY to your crunchbase user key", 
         call. = FALSE)
  }
  
  message("Couldn't find env var CRUNCHBASE_KEY. See ?crunchbase_key for more details.")
  message("Please enter your user_key and press enter:")
  key <- readline(": ")
  
  if (identical(key, "")) {
    stop("Crunchbase user_key entry failed", call. = FALSE)
  }
  
  message("Updating CRUNCHBASE_KEY environment variable")
  Sys.setenv(CRUNCHBASE_KEY = key)
  
  key
}