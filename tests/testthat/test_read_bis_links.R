# Sicherstellen, dass die testthat-Bibliothek geladen ist
library(testthat)

# Tests für die Funktion read_xml_link
test_that("read_xml_link correctly reads an XML file", {
  test_url <- "https://www.bis.org/sitemap.xml?documents=2005,2006,2007,2008,2009"
  expect_s3_class(read_xml_link(test_url), "tbl_df")
})

# Tests für die Funktion read_bis_links
test_that("read_bis_links correctly reads multiple XML files and filters links", {
  test_urls <- c("https://www.bis.org/sitemap.xml?documents=2005,2006,2007,2008,2009",
                 "https://www.bis.org/sitemap.xml?documents=2010,2011,2012,2013,2014")
  result <- read_bis_links(test_urls)
  
  # Test, ob das Ergebnis ein tibble ist
  expect_s3_class(result, "tbl_df")
  
  # Test, ob alle Links "/review/" enthalten
  expect_true(all(stringr::str_detect(result$link, "/review/")))
})
