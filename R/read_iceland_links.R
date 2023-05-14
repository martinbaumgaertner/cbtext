read_iceland_minute_links<-function(part){
  full<-unlist(future.apply::future_lapply(
    #2020 is excluded because the links are broken
    as.list(seq(2009, year(Sys.Date()), by = 1)[-which(seq(2009, year(Sys.Date()), by = 1) == 2020)]),
    read_htmlcb,
    part1="https://www.cb.is/monetary-policy/monetary-policy-committee/?year=",
    part2="",
    future.seed=TRUE))

  if (part=="minutes"){
    filter="Minutes-"
  }
  full<-full[grepl(filter,full)]
  full<-full[!grepl("statement|Statement",full)]
  
  paste0("https://www.cb.is",full)
}
