read_australia_minutes_links<-function(){
  links<-lapply(as.list(2006:lubridate::year(Sys.time())),read_htmlcb,part1="https://www.rba.gov.au/monetary-policy/rba-board-minutes/",part2="/") %>% 
    unlist()%>% 
    dplyr::as_tibble() %>% 
    dplyr::group_by(value) %>% 
    dplyr::filter(dplyr::n()==1) %>% 
    dplyr::pull(value)
  
  links<-links[!grepl(".pdf",links)]
  
  links<-paste0("https://www.rba.gov.au",links)
  return(unique(links))
}
