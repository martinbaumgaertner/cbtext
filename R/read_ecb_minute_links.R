read_ecb_minute_links<-function(x){
  part<-xml2::read_html(paste0("https://www.ecb.europa.eu/press/accounts/",x,"/html/index_include.en.html"))%>%
    rvest::html_nodes("a")%>%
    rvest::html_attr("href")
  paste0("https://www.ecb.europa.eu",part)
}
