read_links <- function(base_url, suffix = NULL, filter_pattern = NULL, xpath = NULL, index = TRUE, full_url = TRUE, year_range = NULL) {
  if (!is.null(year_range)) {
    url <- lapply(year_range, function(x) {
      if (index) {
        paste0(base_url, x, suffix, "/index.htm/")
      } else {
        paste0(base_url, x, suffix)
      }
    })
  } else {
    if (index) {
      url <- paste0(base_url, suffix, "/index.htm/")
    } else {
      url <- paste0(base_url, suffix)
    }
    url <- list(url) # Convert to list for consistent processing
  }
  
  links <- lapply(url, function(u) {
    link <- xml2::read_html(u)
    
    if (!is.null(xpath)) {
      link <- link %>%
        rvest::html_nodes(xpath=xpath) %>%
        rvest::html_nodes("a")
    } else {
      link <- link %>%
        rvest::html_nodes("a")
    }
    
    link %>%
      rvest::html_attr("href") %>%
      unique()
  }) %>% unlist()
  
  if (!is.null(filter_pattern)) {
    links <- links[grepl(filter_pattern, links, fixed = FALSE)]
  }
  
  if (full_url) {
    return(paste0(base_url, links))
  } else {
    return(links)
  }
}

read_australia_minutes_links <- function() {
  read_links(
    "https://www.rba.gov.au/monetary-policy/rba-board-minutes/",
    year_range = as.list(2006:lubridate::year(Sys.time())),
    filter_pattern = "/rba-board-minutes/.*.html$",
    index = FALSE
  )
}

read_iceland_minute_links <- function() {
  read_links(
    "https://www.cb.is/monetary-policy/monetary-policy-committee/?year=",
    year_range = as.list(2006:lubridate::year(Sys.time())),
    filter_pattern = "Minutes-"
  )
}

read_boj_economicoutlook_links <- function() {
  read_links("https://www.boj.or.jp/en/mopo/outlook", filter_pattern = ".pdf", xpath='/html/body/div[2]/div[2]/main/div/table')
}

read_boj_minute_links <- function(x) {
  read_links(paste0("https://www.boj.or.jp/en/mopo/mpmsche_minu/minu_"),
             year_range = as.list(1998:(lubridate::year(Sys.time() - months(3)))),
             xpath='/html/body/div[2]/div[2]/main/div/table')
}

read_boj_monthlyreport_links <- function(x) {
  read_links("https://www.boj.or.jp/en/mopo/gp_",
             year_range = as.list(1998:2015),
             filter_pattern = ".pdf",
             xpath='/html/body/div[2]/div[2]/main/div/table')
}

read_boj_release_links <- function(x) {
  read_links("https://www.boj.or.jp/en/mopo/mpmdeci/mpr_",
             year_range = as.list(1998:(lubridate::year(Sys.time() - months(3)))),
             xpath='/html/body/div[2]/div[2]/main/div/table')
}

read_ecb_blog_links <- function(year) {
  read_links("https://www.ecb.europa.eu/press/blog/date/",
             year_range = as.list(2021:2022),
             suffix = "/html/index_include.en.html", filter_pattern = ".en.")
}

read_ecb_economicoutlook_links <- function() {
  read_links("https://www.ecb.europa.eu/pub/economic-bulletin/html/all_releases.en.html", filter_pattern = "en.pdf", index = FALSE)
}

read_ecb_interviews_links <- function(year) {
  read_links("https://www.ecb.europa.eu/press/inter/date/",
             year_range = as.list(2004:(lubridate::year(Sys.time()))),
             suffix = "/html/index_include.en.html", filter_pattern = ".en.")
}

read_ecb_minute_links <- function(year) {
  read_links("https://www.ecb.europa.eu/press/accounts/",
                    year_range = as.list(2015:(lubridate::year(Sys.time() - months(3)))),
                    suffix = "/html/index_include.en.html", filter_pattern = ".en.")
}

read_ecb_pc_links <- function(year) {
  read_links("https://www.ecb.europa.eu/press/pressconf/",
             year_range = as.list(1999:(lubridate::year(Sys.time()))), 
             suffix = "/html/index_include.en.html", filter_pattern = ".en.")
}

read_ecb_speeches_links <- function(year) {
  read_links("https://www.ecb.europa.eu/press/key/date/",
             year_range = as.list(1997:(lubridate::year(Sys.time()))),
             suffix = "/html/index_include.en.html", filter_pattern = ".en.")
}

read_poland_minute_links <- function() {
  read_links("https://nbp.pl/en/monetary-policy/mpc-documents/mpc-minutes", filter_pattern = "mi_|voting", index = FALSE, full_url = FALSE)
}

read_poland_ir_links <- function() {
  read_links("https://nbp.pl/en/monetary-policy/mpc-documents/inflation-report", filter_pattern = ".pdf", index = FALSE, full_url = FALSE)
}

read_poland_release_links <- function() {
  read_links("https://nbp.pl/en/monetary-policy/mpc-documents/monetary-policy-council-press-releases", filter_pattern = ".pdf", index = FALSE, full_url = FALSE)
}