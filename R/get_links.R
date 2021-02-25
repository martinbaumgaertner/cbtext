get_links<-function(source,part){
  if(part=="minutes"){
    if(source$name%in%"boe"){
      links<-read_boe_minute_links()
    }
    if(source$name%in%"boj"){
      future::plan(multisession)
      links<-unlist(future.apply::future_lapply(as.list(1998:(year(Sys.time() %m-% months(3)))), read_boj_minute_links)) #3 month delay for boj
    }
    if(source$name%in%"ecb"){
      future::plan(multisession)
      links<-unlist(future.apply::future_lapply(as.list(2015:(year(Sys.time() %m-% months(3)))), read_ecb_minute_links))
    }
    if(source$name%in%"fed"){
      links<-read_fed_minute_links(part=part,just_html = T)
    }
    if(source$name%in%"poland"){
      links<-read_poland_minute_links()
    }
    if(source$name%in%"iceland"){
      links<-read_iceland_minute_links(part=part)
    }
  }
  if(part=="pc"){
    if(source$name%in%"boe"){
      stop(paste0(part," is currently not supported for ",source$name))
    }
    if(source$name%in%"boj"){
      stop(paste0(part," is currently not supported for ",source$name))
    }
    if(source$name%in%"ecb"){
      future::plan(multisession)
      links<-unlist(future.apply::future_lapply(as.list(1999:(year(Sys.time()))), read_ecb_pc_links))
    }
    if(source$name%in%"fed"){
      stop(paste0(part," is currently not supported for ",source$name))
    }
    if(source$name%in%"poland"){
      stop(paste0(part," is currently not supported for ",source$name))
    }
    if(source$name%in%"iceland"){
      stop(paste0(part," is currently not supported for ",source$name))
    }
  }
  
  return(links)
}

