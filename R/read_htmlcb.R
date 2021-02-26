read_htmlcb<-function(part1,x,part2){
  xml2::read_html(paste0(part1,x,part2))%>%
    rvest::html_nodes("a")%>%
    rvest::html_attr("href")
}
