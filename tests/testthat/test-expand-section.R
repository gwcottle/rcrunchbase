context("expand-section")

test_that("sections can be expanded regardless of where they came from", {
    fb_node <- crunchbase_parse(crunchbase_GET("organization/facebook"))
    fb_nodes <- crunchbase_get_details("organization/facebook")
    expect_that(crunchbase_expand_section(fb_node, "current_team"),
                is_identical_to(crunchbase_expand_section(fb_nodes, "current_team")))
})

test_that("can expand sections from multiple nodes", {
    nodes <- crunchbase_get_details(c("organization/facebook", "organization/twitter"))
    expect_is(crunchbase_expand_section(nodes, c("current_team", "past_team")), 
                                        "data.frame")
})