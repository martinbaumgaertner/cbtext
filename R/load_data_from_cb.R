load_data_from_cb<-function(links,type,cb){
  tib<-create_tibble(doc_features()$doc_features,length(links))
  future::plan(future::multisession)
  tib$text<-future.apply::future_lapply(as.list(links), 
                                        read_text,sour=cb,future.seed=TRUE)# %>% 
    #unlist()
  tib$link<-links
  tib$type<-type
  if(cb=="boe"){
    tib$cb<-"Bank of England"
    tib$country<-"United Kingdom"
  }else if(cb=="ecb"){
    tib$cb<-"European Central Bank"
    tib$country<-"Euro area"
  }else if(cb=="fed"){
    tib$cb<-"US Federal Reserve"
    tib$country<-"United States"
  }else if(cb=="boj"){
    tib$cb<-"Bank of Japan"
    tib$country<-"Japan"
  }else if(cb=="poland"){
    tib$cb<-"Narodowy Bank Polski"
    tib$country<-"Poland"
  }else if(cb=="iceland"){
    tib$cb<-"Central Bank of Iceland"
    tib$country<-"Iceland"
  }else if(cb=="australia"){
    tib$cb<-"Reserve Bank of Australia"
    tib$country<-"Australia"
  }else if(cb=="riksbank"){
    tib$cb<-"Sveriges Riksbank"
    tib$country<-"Sweden"
  }else{
    tib$cb<-cb
  }
  
  dates<-get_date_from_text(tib,cb,type,links)
  tib$start_date<-dates$start_date
  tib$end_date<-dates$end_date
  tib$release_date<-dates$release_date
  tib$language<-cld3::detect_language(tib %>% 
                                        dplyr::rowwise() %>% 
                                        dplyr::mutate(text=paste(text,collapse = " ")) %>% 
                                        dplyr::pull(text))
  tib$access_time<-Sys.time()
  return(tib)
}
