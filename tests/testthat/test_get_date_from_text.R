library(purrr)

test_that("get_date_from_text returns the correct output", {
  # Test data
  texts <- tibble::tibble(
    text = c(
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit.",
      "12th May 2023",
      "Lorem ipsum 14th June 2023 dolor sit amet.",
      "Lorem ipsum 20-22 July 2023 dolor sit amet."
    )
  )
  
  cb = "bis"
  type = "speech"
  links = "https://www.example.com/minutes_2022.html"
  
  # Expected output
  expected_output <- list(
    start_date = as.POSIXlt(as.Date(c(NA, "2023-05-12", "2023-06-14", "2023-07-20"))),
    end_date = as.POSIXlt(as.Date(c(NA, "2023-05-12", "2023-06-14", "2023-07-22"))),
    release_date = as.POSIXlt(as.Date(c(NA, NA, NA, NA)))
  )
  
  # Test get_date_from_text
  output <- get_date_from_text(texts, cb, type, links)
  
  # Check if the output matches the expected output
  expect_equal(output$start_date, expected_output$start_date)
  expect_equal(output$end_date, expected_output$end_date)
  expect_equal(output$release_date, expected_output$release_date)
})