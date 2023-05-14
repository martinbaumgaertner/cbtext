load_data_from_cb<-function(links,type,cb){
  tib<-create_tibble(doc_features()$doc_features,length(links))
  future::plan(future::multisession)
  tib$text<-future.apply::future_lapply(as.list(links), 
                                        read_text,sour=cb,future.seed=TRUE)# %>% 
    #unlist()
  tib$link<-links
  tib$type<-type
  
  country_map <- list(
    boe = c("Bank of England", "United Kingdom"),
    ecb = c("European Central Bank", "Euro area"),
    fed = c("US Federal Reserve", "United States"),
    boj = c("Bank of Japan", "Japan"),
    poland = c("Narodowy Bank Polski", "Poland"),
    iceland = c("Central Bank of Iceland", "Iceland"),
    australia = c("Reserve Bank of Australia", "Australia"),
    riksbank = c("Sveriges Riksbank", "Sweden")
  )
  
  if (cb %in% names(country_map)) {
    tib$cb <- country_map[[cb]][1]
    tib$country <- country_map[[cb]][2]
  } else {
    tib$cb <- cb
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
