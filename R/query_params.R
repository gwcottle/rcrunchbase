#' @import assertthat
make_params <- function(df) {
    assertthat::assert_that(is.data.frame(df),
                            "uuid" %in% names(df),
                            "type" %in% names(df))
    by_type <- split(df, df$type)
    params <- lapply(by_type, function(x) paste(x[["uuid"]], collapse=","))
    names(params) <- tolower(paste(names(params), "_uuids", sep=""))
    params
}

prep_params <- function(...) {
    params = list(...)
    if (length(params) == 0) return(params)
    df_params <- sapply(params, is.data.frame)
    if (sum(df_params) > 0) {
        raw_params <- params[!df_params]
        df_params <- params[df_params]
        df_params <- lapply(df_params, function(df) {
            if (!any(names(df) == "type") || !any(names(df) == "uuid")) {
                stop("Data frames are only accepted as query parameters if they 
                     have 'type' and 'uuid' columns", call. = FALSE)
            } else {return(df[c("type", "uuid")])}
        })
        df_params <- do.call("rbind", df_params)
        query_params <- make_params(df_params)
        params <- c(raw_params, query_params)
    }
    params
}

