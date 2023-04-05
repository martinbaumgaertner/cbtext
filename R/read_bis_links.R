read_bis_links<-function(urls=c("https://www.bis.org/sitemap.xml?documents=2005,2006,2007,2008,2009",
                               "https://www.bis.org/sitemap.xml?documents=2010,2011,2012,2013,2014",
                               "https://www.bis.org/sitemap.xml?documents=2015,2016,2017,2018,2019",
                               "https://www.bis.org/sitemap.xml?documents=2020,2021,2022,2023")){
  links<-list()
  for(i in 1:length(urls)){
    url<-urls[i]
    links[[i]]<-xml2::read_xml(url) %>% 
      xml2::as_list() %>% 
      tibble::as_tibble() %>%
      tidyr::unnest_wider(urlset) %>%
      # 1st time unnest to release the 2-dimension list?
      tidyr::unnest(cols = names(.)) %>%
      # 2nd time to nest the single list in each cell?
      tidyr::unnest(cols = names(.)) %>%
      # convert data type
      suppressMessages(readr::type_convert())
  }
  
  links<-links %>% 
    dplyr::bind_rows() %>% 
    dplyr::rename(link=loc) %>%
    dplyr::select(link) %>% 
    #filter speeches
    dplyr::filter(stringr::str_detect(link,"/review/")) %>% 
    #check possible data formats
    dplyr::mutate(type=ifelse(stringr::str_detect(link,".pdf"),"pdf","html")) %>% 
    dplyr::mutate(link=stringr::str_remove(link,".pdf|.htm")) %>% 
    tidyr::pivot_wider(names_from = type,values_from=type)
  
  return(links)
}
