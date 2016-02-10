# managed_call <- function(f, events = 44L, every = 60L) {
#     force(f)
#     minute_ <- rep(NA, events)
#     function(...) {       
#         m_dif <- as.numeric(Sys.time() - minute_, units = "secs")
#         minute_[!is.na(m_dif) & m_dif > every] <<- NA
#         calls_remaining <- sum(is.na(minute_))
#         if (!calls_remaining) {
#             message("Close to API limit, pausing for ", 
#                     round(every - max(m_dif), 3), 
#                     " seconds")
#             Sys.sleep(every - max(m_dif))
#             minute_[which.max(m_dif)] <- NA
#             minute_[Position(is.na, minute_)] <<- Sys.time()
#             f(...)
#         } else {
#             minute_[Position(is.na, minute_)] <<- Sys.time()
#             f(...)
#         }
#     }
# }