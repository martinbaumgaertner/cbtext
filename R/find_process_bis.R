find_process_bis<-function(max_page){
  countries_cb<-get_bis_countries()
  
  d<-future::future(lapply(as.list(1:max_page),read_bis_page,countries_cb))
   return(d%>%
     dplyr::bind_rows())
}
