find_process_bis<-function(max_page){
  countries_cb<-get_bis_countries()
  
  d<-future::future(lapply(as.list(1:max_page),read_bis_page,countries_cb))
   return(d%>%
     dplyr::bind_rows())
}

for(i in 1:10){print(i)
  read_bis_page(i,countries_cb)
}

find_process_bis(10)
?future

future::future(lapply(as.list(1:max_page),read_bis_page,countries_cb),globals = countries_cb)

future::plan("multisession")
future.apply::future_lapply(as.list(1:10),read_bis_page,countries_cb,
                            future.globals=c("countries_cb","extract_speaker","extract_title",
                                             "extract_position","extract_event","extract_date",
                                             "get_last_date","search_cb","extract_text","process_text"),
                            future.seed=TRUE,future.packages="magrittr")
x<-lapply(as.list(1:2), read_bis_page,countries_cb)

library(dplyr)
magrittr::
?future.apply::future_lapply

a <- 42

future::future({ b <- 2; a * b }, globals = "a")
usethis::use_pipe()
