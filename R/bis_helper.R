extract_speaker<-function(x){
  x$item_title %>% 
    str_split(.,":") %>% 
    sapply( ., head, 1 )
}
extract_title<-function(x){
  x$item_title %>% 
    str_split(.,":") %>% 
    sapply( ., tail, 1 ) %>% 
    trimws(.)
}
extract_position<-function(x){
  x$item_description %>% 
    str_split(.,",",simplify = T) %>% 
    as_tibble(.,.name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)) %>%
    dplyr::mutate_at("...2", trimws) %>% 
    dplyr::select(2) %>% 
    dplyr::pull()
}
extract_event<-function(x){
  x1<-x$item_description %>%
    spacyr::spacy_parse(.) %>% 
    spacyr::entity_extract(.) %>% 
    dplyr::tibble() %>% 
    dplyr::filter(entity_type=="GPE") %>% 
    dplyr::select(doc_id,entity)
  
  out<-full_join(tibble(doc_id=paste0("text",1:nrow(x))),x1,by="doc_id") %>% 
    dplyr::group_by(doc_id)%>%
    dplyr::filter(row_number()==1 )%>% 
    dplyr::pull(entity)
  return(out)
}
extract_date<-function(x){
  x$item_description %>% 
    str_replace_all("March,","March") %>% 
    str_replace_all("Sept\\.","September") %>% 
    str_replace_all("Hel...","Helsinki, 9 October 2003") %>% 
    str_replace_all("at the Swiss-American Chamber ...","at the Swiss-American Chamber of Commerce, Zurich, 8 October 2003") %>% 
    str_replace_all("on the occasion of the signing of...","Moscow, 13 October 2003") %>% 
    str_replace_all("Committee on Finance, Stockholm, 16 October...","Committee on Finance, Stockholm, 16 October 2003") %>% 
    str_replace_all("at the 2003 IMF","at the 2003 IMF, 24 September 2003") %>% 
    str_replace_all("/"," ") %>% 
    str_remove_all("Claude Trichet President of the European Central Bank Frankfurt am Main") %>% 
    str_remove_all("Communication") %>%
    str_remove_all("on the occasion of a special ECOFIN di...") %>%
    str_remove_all("workshop on ¿Managing Non Performing Asset¿ hosted") %>% 
    str_remove_all("The references for the speech can be found on the Board of Governors of the Federal Reserve System¿s website.") %>%
    str_remove_all("Basel Committee on Banking") %>% 
    str_remove_all("on the occasion") %>% 
    str_remove_all("Seminar on his") %>% 
    str_replace_all("on the European Commission's proposal of 29 January 2014 for a regulation on the separation of certain trading activities from credit institutions.","29 January 2014") %>% 
    str_split(.,",") %>% 
    sapply( ., tail, 1 )%>% 
    tibble(date=.) %>% 
    dplyr::mutate(date=ifelse(str_detect(date," on "),str_split(date," on (?=\\d{1,2})",simplify=T)[,2],date)) %>% 
    dplyr::mutate(date=ifelse(str_detect(date,"-"),get_last_date(date)%>% 
                                str_remove(.,"\\.$") %>% 
                                str_trim(),date%>% 
                                str_remove(.,"\\.$") %>% 
                                str_trim())) %>% 
    dplyr::mutate(date=ifelse(str_detect(date,"^\\d{1,2}",negate=T),paste0("1 ",date),date))%>% 
    dplyr::mutate(date=as.POSIXlt(as.Date(ifelse(grepl("/", date ),
                                                 as.Date(date , format = c("%d/%m/%y")),
                                                 as.Date(date , format = c("%d %B %Y"))), origin = "1970-01-01"))) %>% 
    pull(date)
}
get_last_date<-function(x){
  x%>%
    str_split(.,"-",simplify = T) %>% 
    as_tibble(.,.name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)) %>% 
    dplyr::select(2) %>% pull() %>% str_remove(.,"^_") 
  
}
search_cb<-function(strings,aux_string,countrys){
  dat<-data.frame(country=rep(NA,length(strings)),cb=rep(NA,length(strings)))
  for (i in 1:length(strings)){
    if(nrow(countrys %>% 
            dplyr::filter(str_detect(strings[i],countrys %>% 
                                     pull(cb))))==0){
      dat_temp<-countrys %>% 
        dplyr::filter(str_detect(strings[i],countrys %>% 
                                   pull(country)))
    }else{
      dat_temp<-countrys %>% 
        dplyr::filter(str_detect(strings[i],countrys %>% 
                                   pull(cb)))
    }
    
    if(nrow(dat_temp)==0){
      dat_temp<-countrys %>% 
        dplyr::filter(str_detect(aux_string[i],countrys %>% 
                                   pull(cb)))
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
    pull(links)
  
  future::plan(future::multisession)
  texts<-unlist(future.apply::future_lapply(as.list(links),process_text))
  
  return(texts)
}
process_text<-function(x){
  if(grepl(pattern = ".htm",x)){#check if pdf exists, if not proceed
    text<-xml2::read_html(x)%>% 
      rvest::html_nodes(xpath = "//div[@id='cmsContent']") %>% 
      rvest::html_text()%>%
      readr::read_lines()
  }else{
    text<-pdftools::pdf_text(x) %>% 
      readr::read_lines(.)
  }
  return(text)
}
