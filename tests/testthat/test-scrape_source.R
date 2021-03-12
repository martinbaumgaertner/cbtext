test_that("returns tibble", {
  is.data.frame(scrape_source(list(name="ecb"),c("minutes")))
})
