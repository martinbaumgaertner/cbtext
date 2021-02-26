read_fed_minute_links<-function(part="minutes",restrict_to="nothing"){
  new_part<-xml2::read_html(paste0("https://www.federalreserve.gov/monetarypolicy/fomccalendars.htm"))%>%
    rvest::html_nodes("a")%>%
    rvest::html_attr("href")
  future::plan("multisession")
  old_part<-unlist(future.apply::future_lapply(as.list(1977:(lubridate::year(Sys.time())-6)),read_htmlcb,
                   part1="https://www.federalreserve.gov/monetarypolicy/fomchistorical",
                   part2=".htm",future.seed=TRUE))
  
  full<-c(new_part,old_part)
  
  if (part=="minutes"){
    filter="fomcminutes|MINUTES|minutes|fomcmoa"
  }else if(part=="beige"){
    filter="BeigeBook|beige"
  }else if(part=="green1"){
    filter="gbpt1|greenbook"
  }else if(part=="green2"){
    filter="gbpt2|greenbook"
  }else if(part=="teala"){
    filter="tealbooka"
  }else if(part=="tealb"){
    filter="tealbookb"
  }else if(part=="green_sub"){
    filter="gbsub"
  }else if(part=="blue"){
    filter="bluebook"
  }else if(part=="agenda"){
    filter="Agenda"
  }else if(part=="transkript"){
    filter="meeting|confcall"
  }else if(part=="red"){
    filter="redbook"
  }else if(part=="pc"){
    filter="fomcpresconf"
  }

  full<-full[grepl(filter,full)]
  
  if (restrict_to=="html"){
    full<-full[grepl(".htm|.html",full)]
  }else if(restrict_to=="pdf"){
    full<-full[grepl(".pdf",full)]
  }

  full<-gsub("https://www.federalreserve.gov|http://www.federalreserve.gov","",full)
  
  full<-paste0("https://www.federalreserve.gov",full)
  
  if(part=="pc"){
    future::plan("multisession")
    full<-unlist(future.apply::future_lapply(as.list(full),fed_pc))
  }
  return(full)
}


