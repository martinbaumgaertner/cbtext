read_fed_links<-function(part="minutes",restrict_to="nothing"){
  new_part<-xml2::read_html(paste0("https://www.federalreserve.gov/monetarypolicy/fomccalendars.htm"))%>%
    rvest::html_nodes("a")%>%
    rvest::html_attr("href")
  
  future::plan(future::multisession)
  old_part<-future.apply::future_lapply(
    as.list(1977:(lubridate::year(Sys.time())-6)),
    read_htmlcb,
    part1="https://www.federalreserve.gov/monetarypolicy/fomchistorical",
    part2=".htm",future.seed=TRUE) %>% 
    unlist()
  
  full<-c(new_part,old_part)
  
  filter <- switch(
    part,
    minutes = "fomcminutes|MINUTES|minutes|fomcmoa",
    beige = "BeigeBook|beige",
    green1 = "gbpt1|greenbook",
    green2 = "gbpt2|greenbook",
    teala = "tealbooka",
    tealb = "tealbookb",
    green_sub = "gbsub",
    blue = "bluebook",
    agenda = "Agenda",
    transkript = "meeting|confcall",
    red = "redbook",
    pc = "fomcpresconf",
    statement = "general|boarddocs|/newsevents/press/monetary/|/newsevents/pressreleases/monetary"
  )
  
  full<-full[grepl(filter,full)]
  
  if (restrict_to=="html"){
    full<-full[grepl(".htm|.html",full)]
  }else if(restrict_to=="pdf"){
    full<-full[grepl(".pdf",full)]
  }

  full<-gsub("https://www.federalreserve.gov|http://www.federalreserve.gov","",full)
  full<-paste0("https://www.federalreserve.gov",full)
  
  if(part=="pc"){
    future::plan(future::multisession)
    full<-unlist(future.apply::future_lapply(as.list(full),fed_pc,future.seed=TRUE))
  }
  
  return(full)
}

fed_pc<-function(x){
  #fetch pdfs from pressconference site
  site<-xml2::read_html(x) %>% 
    rvest::html_nodes("a")%>%
    rvest::html_attr("href")
  site<-site[grepl("FOMCpresconf|fomcpresconf",site)]
  paste0("https://www.federalreserve.gov",site)
}
