read_fed_minute_links<-function(part="minutes",just_html=T){
  new_part<-xml2::read_html(paste0("https://www.federalreserve.gov/monetarypolicy/fomccalendars.htm"))%>%
    rvest::html_nodes("a")%>%
    rvest::html_attr("href")
  plan(multisession)
  old_part<-unlist(future_lapply(as.list(1993:(year(Sys.time())-6)),read_htmlcb,
                   part1="https://www.federalreserve.gov/monetarypolicy/fomchistorical",
                   part2=".htm",future.seed=TRUE))
  
  full<-c(new_part,old_part)
  
  if (part=="minutes"){
    filter="fomcminutes|MINUTES|minutes"
  }
  full<-full[grepl(filter,full)]
  
  if (isTRUE(just_html)){
    full<-full[!grepl(".pdf",full)]
  }

  full<-gsub("https://www.federalreserve.gov|http://www.federalreserve.gov","",full)
  

  paste0("https://www.federalreserve.gov",full)
}





