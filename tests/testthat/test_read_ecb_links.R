test_that("read_ecb_links returns the correct links", {
  # Create a mock HTML page
  html <- 'https://www.ecb.europa.eu/press/key/date/2020/html/index_include.en.html'

  # Test read_ecb_links
  links <- read_ecb_links(html, ".en")
  
  # Expected link
  expected_link <- "https://www.ecb.europa.eu/press/key/date/2020/html/ecb.sp200108~323f3e7dac.en.html"
  
  expect_true(expected_link %in% links)
})

is_url <- function(x) {
  grepl("^(http|https)://", x)
}

test_that("read_ecb_blog_links returns website URLs", {
  # Test read_ecb_blog_links
  links <- read_ecb_blog_links(2022)
  
  # Check if the output is a vector
  expect_type(links, "character")
  
  # Check if all elements are valid URLs
  expect_true(all(sapply(links, is_url)))
})

test_that("read_ecb_blog_links returns website URLs", {
  # Test read_ecb_blog_links
  links <- read_ecb_economicoutlook_links(2022)
  
  # Check if the output is a vector
  expect_type(links, "character")
  
  # Check if all elements are valid URLs
  expect_true(all(sapply(links, is_url)))
})

test_that("read_ecb_blog_links returns website URLs", {
  # Test read_ecb_blog_links
  links <- read_ecb_interviews_links(2022)
  
  # Check if the output is a vector
  expect_type(links, "character")
  
  # Check if all elements are valid URLs
  expect_true(all(sapply(links, is_url)))
})

test_that("read_ecb_blog_links returns website URLs", {
  # Test read_ecb_blog_links
  links <- read_ecb_minute_links(2022)
  
  # Check if the output is a vector
  expect_type(links, "character")
  
  # Check if all elements are valid URLs
  expect_true(all(sapply(links, is_url)))
})

test_that("read_ecb_blog_links returns website URLs", {
  # Test read_ecb_blog_links
  links <- read_ecb_pc_links(2022)
  
  # Check if the output is a vector
  expect_type(links, "character")
  
  # Check if all elements are valid URLs
  expect_true(all(sapply(links, is_url)))
})

test_that("read_ecb_blog_links returns website URLs", {
  # Test read_ecb_blog_links
  links <- read_ecb_interview_links(2022)
  
  # Check if the output is a vector
  expect_type(links, "character")
  
  # Check if all elements are valid URLs
  expect_true(all(sapply(links, is_url)))
})
