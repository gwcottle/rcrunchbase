context("Appropriate delays to control calls per minute")

test_that("managed_call does not make more than 'events' calls per 'every' seconds", {
    f <- function() {
        tm <- Sys.time()
        Sys.sleep(runif(1, max=.03))
        tm
    }
    
    managed1 <- managed_call(f, events=10, every=.3)
    y1 <- replicate(30, managed1())
    y1_test <- vapply(1:30, function(i) length(y1[y1 < y1[i] + .3 & y1 >= y1[i]]), FUN.VALUE = numeric(1))
    expect_true(max(y1_test) <= 10)
})
