load_data_from_links<-function(links,type,cb){
  tib<-create_tibble(doc_features()$doc_features,length(links))
  future::plan(future::multisession)
  tib$text<-future.apply::future_lapply(as.list(links), 
                                        read_text,sour=cb,future.seed=TRUE) %>% 
    unlist()
  tib$link<-links
  tib$type<-type
  tib$cb<-cb
  dates<-get_date_from_text(tib$text,type)
  tib$start_date<-dates$start_date
  tib$end_date<-dates$end_date
  tib$language<-cld3::detect_language(tib$text)
  return(tib)
}
