#' Set or get a CrunchBase user key.
#' 
#' Reads (or writes) the user key to an environment variable.
#' 
#' Request a user key from CrunchBase \url{https://developer.crunchbase.com/} 
#' in order to use the API. This key gets appended to GET queries via the 
#' \code{user_key} parameter. The key will be saved so you don't have to enter 
#' it repeatedly.
#' 
#' @param force Overwrite existing key? If TRUE, the user will be prompted to 
#'  enter a user key even if one has already been entered before.
#' @export
crunchbase_key <- function(force = FALSE) {
    key_name <- "CRUNCHBASE_KEY"
    env <- Sys.getenv(key_name)
    if (!identical(env, "") && !force) 
        return(env)
    
    # this is where the file does/will live
    file_name <- paste(system.file("extdata", package="rcrunchbase"), 
                       "/", key_name, sep="")
    
    # if the file exists, just read the key from there and quietly set the 
    # environment variable
    if (file.exists(file_name) && !force) {
        key <- readRDS(file_name)
        args = list(key)
        names(args) = key_name
        do.call(Sys.setenv, args)
        return(key)
    }    
    
    if (!interactive()) {
        stop("Please set env var CRUNCHBASE_KEY to your crunchbase user key", 
             call. = FALSE)
    }
    
    
    key <- readline(paste("Please enter your Crunchbase API key: "))
    
    if (identical(key, "")) {
        stop("Crunchbase user_key entry failed", call. = FALSE)
    }
    
    message("Updating CRUNCHBASE_KEY environment variable")
    Sys.setenv(CRUNCHBASE_KEY = key)
    
    args = list(key)
    names(args) = key_name
    do.call(Sys.setenv, args)
    saveRDS(key, file_name)
    key
}