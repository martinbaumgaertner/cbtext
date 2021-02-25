read_iceland_minute_links<-function(part){
  full<-unlist(future_lapply(as.list(2009:year(Sys.time())),read_htmlcb,
                       part1="https://www.cb.is/monetary-policy/monetary-policy-committee/?year=",
                       part2="",future.seed=TRUE))

  if (part=="minutes"){
    filter="Minutes-"
  }
  full<-full[grepl(filter,full)]
  
  paste0("https://www.cb.is",full)
}
