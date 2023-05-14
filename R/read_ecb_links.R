read_ecb_links <- function(url, pattern) {
  part <- xml2::read_html(url) %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    unique() %>%
    grep(pattern, ., value = TRUE, fixed = TRUE)
  paste0("https://www.ecb.europa.eu", part)
}

read_ecb_blog_links <- function(year) {
  url <- paste0("https://www.ecb.europa.eu/press/blog/date/", year, "/html/index_include.en.html")
  read_ecb_links(url, ".en.")
}

read_ecb_economicoutlook_links <- function(year) {
  url <- "https://www.ecb.europa.eu/pub/economic-bulletin/html/all_releases.en.html"
  read_ecb_links(url, "en.pdf")
}

read_ecb_interviews_links <- function(year) {
  url <- paste0("https://www.ecb.europa.eu/press/inter/date/", year, "/html/index_include.en.html")
  read_ecb_links(url, ".en.")
}

read_ecb_minute_links <- function(year) {
  url <- paste0("https://www.ecb.europa.eu/press/accounts/", year, "/html/index_include.en.html")
  read_ecb_links(url, ".en.")
}

read_ecb_pc_links <- function(year) {
  url <- paste0("https://www.ecb.europa.eu/press/pressconf/", year, "/html/index_include.en.html")
  read_ecb_links(url, ".en.")
}

read_ecb_interview_links <- function(year) {
  url <- paste0("https://www.ecb.europa.eu/press/key/date/", year, "/html/index_include.en.html")
  read_ecb_links(url, ".en.")
}
