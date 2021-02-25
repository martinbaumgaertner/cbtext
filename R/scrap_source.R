parts="minutes"
library(dplyr)
library(rvest)
library(data.table)
library(pdftools)
library("future.apply")
library(lubridate)

source<-list(name="ecb")

scrap_source<-function(source,parts){
  output<-list()
  
  if("minutes" %in% parts){
    minutes_links<-get_links(source,part="minutes")
    output$minutes<-load_data_from_links(minutes_links,"minutes",source$name)
  }
  if("pc" %in% parts){
    pc_links<-get_links(source,part="pc")
    output$pc<-load_data_from_links(links=pc_links,type="pc",cb=source$name)
  }
  
  return(output)
}

x<-scrap_source(list(name="ecb"),c("minutes","pc"))

x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE, FALSE, FALSE, TRUE))
y0 <- lapply(x, FUN = quantile, probs = 1:3/4)
y1 <- future_lapply(x, FUN = quantile, probs = 1:3/4)

microbenchmark::microbenchmark(lapply(x, FUN = quantile, probs = 1:3/4),future_lapply(x, FUN = quantile, probs = 1:3/4, future.globals = FALSE),times=100)

minutes_links[stringr::str_detect(minutes_links,"www.federalreserve.govhttps"),times=1]


?microbenchmark::microbenchmark


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
