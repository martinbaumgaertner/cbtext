read_ecb_blog_links<-function(x){
  part<-xml2::read_html(paste0("https://www.ecb.europa.eu/press/blog/date/",x,"/html/index_include.en.html"))%>%
    rvest::html_nodes("a")%>%
    rvest::html_attr("href")
  part<-unique(part[grepl(".en.",part)])
  paste0("https://www.ecb.europa.eu",part)
}

read_ecb_economicoutlook_links<-function(x){
  part<-xml2::read_html("https://www.ecb.europa.eu/pub/economic-bulletin/html/all_releases.en.html")%>%
    rvest::html_nodes("a")%>%
    rvest::html_attr("href")
  part<-part[grepl("en.pdf",part)]
  paste0("https://www.ecb.europa.eu",part)
}

read_ecb_interviews_links<-function(x){
  part<-xml2::read_html(paste0("https://www.ecb.europa.eu/press/inter/date/",x,"/html/index_include.en.html"))%>%
    rvest::html_nodes("a")%>%
    rvest::html_attr("href")
  part<-unique(part[grepl(".en.",part)])
  paste0("https://www.ecb.europa.eu",part)
}

read_ecb_minute_links<-function(x){
  part<-xml2::read_html(paste0("https://www.ecb.europa.eu/press/accounts/",x,"/html/index_include.en.html"))%>%
    rvest::html_nodes("a")%>%
    rvest::html_attr("href")
  paste0("https://www.ecb.europa.eu",part)
}

read_ecb_pc_links<-function(x){
  part<-xml2::read_html(paste0("https://www.ecb.europa.eu/press/pressconf/",x,"/html/index_include.en.html"))%>%
    rvest::html_nodes("a")%>%
    rvest::html_attr("href")
  paste0("https://www.ecb.europa.eu",part)
}
