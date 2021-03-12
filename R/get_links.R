get_links<-function(source,part){
if(source$name=="boe"){
  if(part=="minutes"){
    links<-read_boe_minute_links()
    }
  }else if(source$name=="boj"){
    if(part=="minutes"){
    future::plan(future::multisession)
    links<-unlist(future.apply::future_lapply(as.list(1998:(lubridate::year(Sys.time() - months(3)))), read_boj_minute_links,future.seed=TRUE)) #3 month delay for boj
    }
    if(part=="economic_report"){
      future::plan(future::multisession)
      links<-unlist(future.apply::future_lapply(as.list(1998:2015), read_boj_monthlyreport_links,future.seed=TRUE))
    }
    if(part=="release"){
      future::plan(future::multisession)
      links<-unlist(future.apply::future_lapply(as.list(1998:(lubridate::year(Sys.time() - months(3)))), read_boj_release_links,future.seed=TRUE))
    }
    if(part=="outlook_report"){
      links<-read_boj_economicoutlook_links()
    }
  }else if(source$name=="ecb"){
    if(part=="minutes"){
      future::plan(future::multisession)
      links<-unlist(future.apply::future_lapply(as.list(2015:(lubridate::year(Sys.time() - months(3)))), read_ecb_minute_links,future.seed=TRUE))
    }
    if(part=="pc"){
      future::plan(future::multisession)
      links<-unlist(future.apply::future_lapply(as.list(1999:(lubridate::year(Sys.time()))), read_ecb_pc_links,future.seed=TRUE))
    }
    if(part=="economic_outlook"){
      future::plan(future::multisession)
      links<-unlist(future.apply::future_lapply(as.list(2004:(lubridate::year(Sys.time()))), read_ecb_economicoutlook_links,future.seed=TRUE))
    }
    if(part=="interview"){
      future::plan(future::multisession)
      links<-unlist(future.apply::future_lapply(as.list(2004:(lubridate::year(Sys.time()))), read_ecb_interviews_links,future.seed=TRUE))
    }
    if(part=="blog"){
      future::plan(future::multisession)
      links<-unlist(future.apply::future_lapply(as.list(2021:(lubridate::year(Sys.time()))), read_ecb_blog_links,future.seed=TRUE))
    }
    }else if(source$name=="fed"){
      if(part=="minutes"){
        links<-read_fed_minute_links(part=part,restrict_to = "html")
      }
      if(part=="pc"){
        links<-read_fed_minute_links(part="pc")
      }
      if(part%in%c("beige","green1","green2","teala","tealb","green_sub","blue","agenda","transkript","red")){
        links<-read_fed_minute_links(part=part,restrict_to = "pdf")
      }
    }else if(source$name=="poland"){
      if(part=="minutes"){
        links<-read_poland_minute_links()
      }
    }else if(source$name=="iceland"){
      if(part=="minutes"){
        links<-read_iceland_minute_links(part=part)
      }
    }else if(source$name=="riksbank"){
      if(part=="mpreports"){
        links<-read_sweden_mpreports_links()
      }
      if(part=="economicreview"){
        links<-read_sweden_economicreview_links()
      }
      if(part=="minutes"){
        links<-read_sweden_minutes_links()
      }
    }else if(source$name=="australia"){
      if(part=="minutes"){
        links<-read_australia_minutes_links()
      }
    }
  
  
  return(unique(links))
}
