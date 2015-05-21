context("Simple GET requests")

test_that("GET returns predictable objects", {
    expect_that(crunchbase_build_url("people/bill-gates"), 
                is_identical_to(crunchbase_build_url(c("people", "bill-gates"))))
    expect_null(crunchbase_GET("bloop"))    
    expect_null(crunchbase_parse(crunchbase_GET("people/tarak-shah")))    
})

test_that("Can pass data frame as query parameter to GET", {
    loc <- data.frame(type="Location", uuid="4e2b4deedf949da5ce5e2e81ec1d3ebd")
    expect_identical(crunchbase_build_url("organizations", loc),
                     crunchbase_build_url("organizations", 
                                          location_uuids = "4e2b4deedf949da5ce5e2e81ec1d3ebd"))
})