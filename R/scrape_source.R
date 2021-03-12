#' @title scrape_source
#' @description Scrape data from central bank source
#' @param source character vector describing central banks
#' @param parts character vector describing parts to scrape
#' @param page Maximal numbers to scrape bis page, Default: 668
#' @return tibble
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'   x<-scrape_source(list(name="ecb"),c("minutes"))
#'   x<-scrape_source(list(name="ecb"),c("minutes","pc","economic_outlook","blog"))
#'   x<-scrape_source(list(name="fed"),c("minutes","pc","red","transkript","agenda",
#'   "blue","green_sub","teala","tealb","green1","green2","beige"))
#'   x<-scrape_source(list(name="boj"),c("minutes","economic_outlook","release","outlook_report"))
#'   x<-scrape_source(list(name="poland"),c("minutes"))
#'   x<-scrape_source(list(name="iceland"),c("minutes"))
#'   x<-scrape_source(list(name="australia"),c("minutes"))
#'   x<-scrape_source(list(name="riksbank"),c("minutes","economicreview","mpreports"))
#'  }
#' }
#' @rdname scrape_source
#' @export 
scrape_source<-function(source,parts,page=668){
  output<-list()
  Sys.setlocale("LC_ALL","English")
  for(part in parts){
    message(paste("Find",part))
    if(part=="speeces"){
      output[[part]]<-find_process_bis(page)
    }else{
      links<-get_links(source,part=part)
      output[[part]]<-load_data_from_links(links,part,source$name)
    }
  }
  return(output)
}


