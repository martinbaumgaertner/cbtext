read_ecb_economicoutlook_links<-function(x){
  part<-xml2::read_html("https://www.ecb.europa.eu/pub/economic-bulletin/html/all_releases.en.html")%>%
    rvest::html_nodes("a")%>%
    rvest::html_attr("href")
  part<-part[grepl("en.pdf",part)]
  paste0("https://www.ecb.europa.eu",part)
}

