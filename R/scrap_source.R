parts="minutes"
library(dplyr)
library(rvest)
library(data.table)
library(pdftools)
library("future.apply")
library(lubridate)
data.table()
source<-list(name="test")
so
scrap_source<-function(source,parts){
output<-list()
  plan(multisession)
  
  if("minutes" %in% parts){
    if(source$name=="boe"){
      minutes_base_link<-"https://www.bankofengland.co.uk/sitemap/minutes"
      old_minutes<-rvest::xml2(minutes_base_link)%>%
        rvest::html_nodes(xpath='/html/body/div[1]/main/section[2]/div/div[1]/ul[3]')%>% 
        rvest::html_nodes("a")%>%
        rvest::html_attr("href")
      new_minutes<-rvest::xml2(minutes_base_link)%>%
        rvest::html_nodes(xpath='/html/body/div[1]/main/section[2]/div/div[1]/ul[4]')%>% 
        rvest::html_nodes("a")%>%
        rvest::html_attr("href")
      minutes_links<-c(old_minutes,new_minutes)[!grepl("(1[0-8][0-9]{2}|19[0-8][0-9]|199[0-7])",c(old_minutes,new_minutes))] #exclude all links before 1998
    }else if(source$name=="boj"){
      minutes_links<-unlist(future_lapply(as.list(1998:(year(Sys.time() %m-% months(3)))), read_boj_minute_links)) #3 month delay for boj
    }
    minutes<-create_tibble(doc_features()$doc_features,length(minutes_links))
    
    minutes$text<-future_lapply(as.list(minutes_links), 
                                read_text,future.seed=TRUE)
    minutes$link<-minutes_links
    minutes$type<-"minutes"
    minutes$cb<-source$name
    
    output$minutes<-minutes
  }
}





x<-

tibble(colnames=doc_features()$doc_features_steady)

x<-doc_features()$doc_features_steady %>% purrr::map_dfc(setNames, object = list(logical()))

vec <- setNames(rep("", 4), letters[1:3])
bind_rows(vec)[4, ]

x<-tibble(links=rep(NA,length(minutes_links)))
x$links<-minutes_links
tibble()
doc_features()
?html_nodes
