read_ecb_blog_links<-function(x){
  part<-xml2::read_html(paste0("https://www.ecb.europa.eu/press/blog/date/",x,"/html/index_include.en.html"))%>%
    rvest::html_nodes("a")%>%
    rvest::html_attr("href")
  part<-unique(part[grepl(".en.",part)])
  paste0("https://www.ecb.europa.eu",part)
}
