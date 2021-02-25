read_boe_minute_links<-function(x){
  minutes_base_link<-"https://www.bankofengland.co.uk/sitemap/minutes"
  old_minutes<-xml2::read_html(minutes_base_link)%>%
    rvest::html_nodes(xpath='/html/body/div[1]/main/section[2]/div/div[1]/ul[3]')%>% 
    rvest::html_nodes("a")%>%
    rvest::html_attr("href")
  new_minutes<-xml2::read_html(minutes_base_link)%>%
    rvest::html_nodes(xpath='/html/body/div[1]/main/section[2]/div/div[1]/ul[4]')%>% 
    rvest::html_nodes("a")%>%
    rvest::html_attr("href")
  minutes_links<-c(old_minutes,new_minutes)[!grepl("(1[0-8][0-9]{2}|19[0-8][0-9]|199[0-7])",c(old_minutes,new_minutes))] #exclude all links before 1998
  minutes_links
}

