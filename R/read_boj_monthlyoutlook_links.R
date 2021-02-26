read_boj_monthlyoutlook_links<-function(x){
  part<-xml2::read_html(paste0("https://www.boj.or.jp/en/mopo/gp_",x,"/index.htm/"))%>%
    rvest::html_nodes(xpath='/html/body/section/div/section/div/table')%>% 
    rvest::html_nodes("a")%>%
    rvest::html_attr("href")
  part<-part[grepl(".pdf",part)]
  paste0("https://www.boj.or.jp",part)
}
