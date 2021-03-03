extract_speaker<-function(x){
  out<-stringr::str_split(x$item_title,":")
  out<-sapply( out, head, 1 )
  
}
extract_title<-function(x){
  out<-stringr::str_split(x$item_title,":")
  out<-trimws(sapply( out, tail, 1 ))
  return(out)
}
extract_position<-function(x){
  out<-stringr::str_split(x$item_description,",",simplify = T)
  out<-dplyr::as_tibble(out,.name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE))
  out<-dplyr::mutate_at(out,"...2", trimws)
  out<-dplyr::select(out,2)
  out<-dplyr::pull(out)
  return(out)
}
extract_event<-function(x){
  x1<-dplyr::tibble(spacyr::entity_extract(spacyr::spacy_parse(x$item_description)))
  x1<-dplyr::filter(x1,entity_type=="GPE")
  x1<-dplyr::select(x1,doc_id,entity)
    
  out<-dplyr::full_join(dplyr::tibble(doc_id=paste0("text",1:nrow(x))),x1,by="doc_id") 
    out<-dplyr::group_by(out,doc_id)
    out<-dplyr::filter(out,dplyr::row_number()==1 )
    out<-dplyr::pull(out,entity)
  return(out)
}
extract_date<-function(x){
  Sys.setlocale("LC_ALL","English")
  x$item_description %>% 
    stringr::str_replace_all("March,","March") %>% 
    stringr::str_replace_all("Sept\\.","September") %>% 
    stringr::str_replace_all("Hel...","Helsinki, 9 October 2003") %>% 
    stringr::str_replace_all("at the Swiss-American Chamber ...","at the Swiss-American Chamber of Commerce, Zurich, 8 October 2003") %>% 
    stringr::str_replace_all("on the occasion of the signing of...","Moscow, 13 October 2003") %>% 
    stringr::str_replace_all("Committee on Finance, Stockholm, 16 October...","Committee on Finance, Stockholm, 16 October 2003") %>% 
    stringr::str_replace_all("at the 2003 IMF","at the 2003 IMF, 24 September 2003") %>% 
    stringr::str_replace_all("/"," ") %>% 
    stringr::str_remove_all("Claude Trichet President of the European Central Bank Frankfurt am Main") %>% 
    stringr::str_remove_all("Communication") %>%
    stringr::str_remove_all("on the occasion of a special ECOFIN di...") %>%
    stringr::str_remove_all("workshop on ¿Managing Non Performing Asset¿ hosted") %>% 
    stringr::str_remove_all("The references for the speech can be found on the Board of Governors of the Federal Reserve System¿s website.") %>%
    stringr::str_remove_all("Basel Committee on Banking") %>% 
    stringr::str_remove_all("on the occasion") %>% 
    stringr::str_remove_all("Seminar on his") %>% 
    stringr::str_replace_all("on the European Commission's proposal of 29 January 2014 for a regulation on the separation of certain trading activities from credit institutions.","29 January 2014") %>% 
    stringr::str_split(.,",") %>% 
    sapply( ., tail, 1 )%>% 
    dplyr::tibble(date=.) %>% 
    dplyr::mutate(date=ifelse(stringr::str_detect(date," on "),stringr::str_split(date," on (?=\\d{1,2})",simplify=T)[,2],date)) %>% 
    dplyr::mutate(date=ifelse(stringr::str_detect(date,"-"),get_last_date(date)%>% 
                                stringr::str_remove(.,"\\.$") %>% 
                                stringr::str_trim(),date%>% 
                                stringr::str_remove(.,"\\.$") %>% 
                                stringr::str_trim())) %>% 
    dplyr::mutate(date=ifelse(stringr::str_detect(date,"^\\d{1,2}",negate=T),paste0("1 ",date),date))%>% 
    dplyr::mutate(date=as.POSIXlt(as.Date(ifelse(grepl("/", date ),
                                                 as.Date(date , format = c("%d/%m/%y")),
                                                 as.Date(date , format = c("%d %B %Y"))), origin = "1970-01-01"))) %>% 
    dplyr::pull(date)
}
get_last_date<-function(x){
  x%>%
    stringr::str_split(.,"-",simplify = T) %>% 
    dplyr::as_tibble(.,.name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)) %>% 
    dplyr::select(2) %>% pull() %>% stringr::str_remove(.,"^_") 
  
}
search_cb<-function(strings,aux_string,countrys){
  dat<-data.frame(country=rep(NA,length(strings)),cb=rep(NA,length(strings)))
  for (i in 1:length(strings)){
    if(nrow(countrys %>% 
            dplyr::filter(stringr::str_detect(strings[i],countrys %>% 
                                              dplyr::pull(cb))))==0){
      dat_temp<-countrys %>% 
        dplyr::filter(stringr::str_detect(strings[i],countrys %>% 
                                            dplyr::pull(country)))
    }else{
      dat_temp<-countrys %>% 
        dplyr::filter(stringr::str_detect(strings[i],countrys %>% 
                                            dplyr::pull(cb)))
    }
    
    if(nrow(dat_temp)==0){
      dat_temp<-countrys %>% 
        dplyr::filter(stringr::str_detect(aux_string[i],countrys %>% 
                                            dplyr::pull(cb)))
    }
    if(nrow(dat_temp)==0){
      print(aux_string[i])
      dat_temp<-NA
    }
    
    dat[i,]<-dat_temp
  }
  
  return(dat)
}
extract_text<-function(x){
  links<-x$item_link%>%
    dplyr::tibble("links"=.) %>% 
    dplyr::mutate(links=gsub(".htm",".pdf",links)) %>% 
    dplyr::mutate(links=dplyr::if_else(RCurl::url.exists(links),links,gsub(".pdf",".htm",links))) %>% 
    dplyr::pull(links)
  
  #future::plan(future::multisession)
  texts<-lapply(as.list(links),process_text#,future.seed=TRUE
                                     )
  
  return(texts)
}
process_text<-function(x){
  if(grepl(pattern = ".htm",x)){#check if pdf exists, if not proceed
    text<-xml2::read_html(x)%>% 
      rvest::html_nodes(xpath = "//div[@id='cmsContent']") %>% 
      rvest::html_text()%>%
      readr::read_lines()
  }else{
    text<-readtext::readtext(x) %>% 
      readr::read_lines(.)
  }
  return(text)
}
