read_boj_economicoutlook_links<-function(){
  part<-xml2::read_html(paste0("https://www.boj.or.jp/en/mopo/outlook/index.htm/"))%>%
    rvest::html_nodes(xpath='/html/body/section/div/section/div/table')%>% 
    rvest::html_nodes("a")%>%
    rvest::html_attr("href")
  part<-part[grepl(".pdf",part)]
  paste0("https://www.boj.or.jp",part)
}

read_boj_minute_links<-function(x){
  part<-xml2::read_html(paste0("https://www.boj.or.jp/en/mopo/mpmsche_minu/minu_",x,"/index.htm/"))%>%
    rvest::html_nodes(xpath='/html/body/section/div/section/div/table')%>% 
    rvest::html_nodes("a")%>%
    rvest::html_attr("href")
  paste0("https://www.boj.or.jp",part)
}

read_boj_monthlyreport_links<-function(x){
  part<-xml2::read_html(paste0("https://www.boj.or.jp/en/mopo/gp_",x,"/index.htm/"))%>%
    rvest::html_nodes(xpath='/html/body/section/div/section/div/table')%>% 
    rvest::html_nodes("a")%>%
    rvest::html_attr("href")
  part<-part[grepl(".pdf",part)]
  paste0("https://www.boj.or.jp",part)
}

read_boj_release_links<-function(x){
  part<-xml2::read_html(paste0("https://www.boj.or.jp/en/announcements/release_",x,"/index.htm/"))%>%
    rvest::html_nodes(xpath='/html/body/section/div/section/div/table')%>% 
    rvest::html_nodes("a")%>%
    rvest::html_attr("href")
  paste0("https://www.boj.or.jp",part)
}