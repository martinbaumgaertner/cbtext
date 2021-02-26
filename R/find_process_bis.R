find_process_bis<-function(max_page){
  countries_cb<-get_bis_countries()
  
  lapply(as.list(1:max_page),read_bis_page,countries_cb) %>% 
           dplyr::bind_rows()
}

