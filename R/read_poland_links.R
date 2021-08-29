read_poland_minute_links<-function(){
  part<-xml2::read_html("https://www.nbp.pl/homen.aspx?f=/en/onbp/organizacja/minutes.html")%>%
    rvest::html_nodes("a")%>%
    rvest::html_attr("href")
  
  part<-part[grepl("minutes/mi",part)]
  
  paste0("https://www.nbp.pl",part)
}
