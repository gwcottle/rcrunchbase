delay <- function(events, every, f) {
    i <- 1
    t <- Sys.time()
    function(...) {        
        cat(".")        
        tnew <- as.numeric(Sys.time() - t, units = "secs")
        if(i >= events && tnew < every) {
            cat("\n")
            i <<- 1            
            Sys.sleep(every - tnew)
            t <<- Sys.time()
            f(...)
        }
        i <<- i + 1
        f(...)
    }
}
