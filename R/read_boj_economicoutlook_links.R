read_boj_economicoutlook_links<-function(){
  part<-xml2::read_html(paste0("https://www.boj.or.jp/en/mopo/outlook/index.htm/"))%>%
    rvest::html_nodes(xpath='/html/body/section/div/section/div/table')%>% 
    rvest::html_nodes("a")%>%
    rvest::html_attr("href")
  part<-part[grepl(".pdf",part)]
  paste0("https://www.boj.or.jp",part)
}
