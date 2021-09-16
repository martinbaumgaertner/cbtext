get_bis_features<-function(column,countries_cb){
  
  html_site<-xml2::read_html(paste0(column[1],".htm"))
  if(!is.na(column[2])){
    pdf_link<-paste0(column[1],".pdf")
  }
  
  title<-html_site %>% 
    rvest::html_element("h1")%>% 
    rvest::html_text()
  
  if(stringr::str_detect(title,":")){
    title<-title %>% 
      stringr::str_split(pattern=":",simplify = T) %>% 
      .[,2]
  }
  
  speaker<-html_site%>%
    rvest::html_element(".authorlnk") %>% 
    rvest::html_text()
  
  description<-html_site %>% 
    rvest::html_element("#extratitle-div") %>% 
    rvest::html_text()%>% 
    stringr::str_remove_all("\n|\t")
  
  dates<-get_date_from_text(description,"bis",type="speeches")
  start_date<-dates$start_date
  end_date<-dates$end_date
  
  release_date<-html_site %>% 
    rvest::html_element(".date") %>% 
    rvest::html_text() %>% 
    as.Date(c("%d %B %Y"))
  
  position<-extract_position(description)
  event<-extract_event(description)
  cb<-search_cb(position,description,countries_cb)$cb
  country<-search_cb(position,description,countries_cb)$country
  
  if(!is.na(column[2])){
    link<-pdf_link
    text<-suppressMessages(readtext::readtext(pdf_link)) %>% 
      dplyr::pull(text) %>% 
      readr::read_lines(.)
    text<-text[stringr::str_detect(text,"BIS Review",negate = T)] #%>% 
      #paste(collapse = " ")
    
    text<-list(dplyr::tibble(text))
  }else{
    link<-paste0(column[1],".htm")
    text<-html_site %>% rvest::html_nodes(xpath = "//div[@id='cmsContent']") %>% 
      rvest::html_text()%>%
      readr::read_lines()#%>% 
      #paste(collapse = " ")
    text<-list(dplyr::tibble(text))
  }
  
  access_time<-Sys.time()
  language<-names(which.max(table(cld3::detect_language(text[[1]]))))
  type="speech"
  
  #fill gaps
  
  # if(is.na(start_date)){
      #if no date can be found in description use information from bis site
  #   Sys.setlocale("LC_ALL","English")
  #   date_NA <- function(x) tryCatch(as.Date(x, tryFormats = c("%d/%m/%y","%m/%d/%y", "%Y/%m/%d",
  #                                                             "%d %B %Y","%d %B, %Y","%d %B. %Y","%B %d, %Y","%dth %B %Y","%d.%m.%y")), error = function(e) NA)
  #   dates<-html_site%>% 
  #     rvest::html_element(".date")%>% 
  #     rvest::html_text() %>%
  #     date_NA()
  #   start_date<-dates
  #   end_date<-dates
  # }
  
  if(is.na(start_date)){
    dates<-get_date_from_text(text[[1]],"bis","speech")
    start_date<-dates$start_date
    end_date<-dates$end_date
  }
  if(is.na(speaker)){
    speaker<-html_site %>% 
      rvest::html_element("h1")%>% 
      rvest::html_text() %>% 
      stringr::str_split(":")
    speaker<-sapply( speaker, utils::head, 1 )
  }
  
  return(dplyr::tibble(title,speaker,start_date,end_date,release_date,position,event,cb,country,type,text,link,access_time,language))
}

search_cb<-function(strings,aux_string,countrys){
  dat<-data.frame(country=rep(NA,length(strings)),cb=rep(NA,length(strings)))
  for (i in 1:length(strings)){
    
    if(nrow(countrys %>% 
            dplyr::filter(stringr::str_detect(strings[i],countrys %>% 
                                              dplyr::pull(cb))))==0){
      #if central bank cannot found use country names
      dat_temp<-countrys %>% 
        dplyr::filter(stringr::str_detect(strings[i],countrys %>% 
                                            dplyr::pull(country)))
    }else{
      #else use cb names
      dat_temp<-countrys %>% 
        dplyr::filter(stringr::str_detect(strings[i],countrys %>% 
                                            dplyr::pull(cb)))
      if(nrow(dat_temp)>1){
        dat_temp=dat_temp[1,]
      }
    }
    
    #if speaker position gives no central bank or country use total description
    if(nrow(dat_temp)==0){
      dat_temp<-countrys %>% 
        dplyr::filter(stringr::str_detect(aux_string[i],countrys %>% 
                                            dplyr::pull(cb))) %>% 
        .[1,]
    }
    if(nrow(dat_temp)==0){
      print(aux_string[i])
      dat_temp<-NA
    }
    
    dat[i,]<-dat_temp
  }
  
  return(dat)
}
extract_date<-function(x){
  Sys.setlocale("LC_ALL","English")
  x %>% 
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
    sapply( ., utils::tail, 1 )%>% 
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
    dplyr::select(2) %>% dplyr::pull() %>% stringr::str_remove(.,"^_") 
}
extract_position<-function(x){
  out<-stringr::str_split(x,",",simplify = T)
  out<-dplyr::as_tibble(out,.name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE))
  
  if(ncol(out)>1){
    out<-dplyr::mutate_at(out,"...2", trimws)
    out<-dplyr::select(out,2)
    out<-dplyr::pull(out)
    return(out)
  }else{
    return(NA)
  }
  
}
extract_event<-function(x){
  x1<-dplyr::tibble(spacyr::entity_extract(spacyr::spacy_parse(x)))
  x1<-dplyr::filter(x1,entity_type=="GPE")
  x1<-paste(dplyr::pull(x1,entity),collapse = ", ")
  
  return(x1)
}


load_data_from_bis<-function(links,countries_cb){
  #loop over all links and collect results in tibble
  bis_data<-list()
  pb<-utils::txtProgressBar(0,nrow(links),style=3)
  for(i in 1:nrow(links)){
    bis_data[[i]]<-get_bis_features(links[i,],countries_cb)
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)
  bis_data<-bis_data %>% 
    dplyr::bind_rows()
  
  return(bis_data)
}
