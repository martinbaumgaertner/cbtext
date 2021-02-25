read_htmlcb<-function(part1,x,part2){
  read_html(paste0(part1,x,part2))%>%
    html_nodes("a")%>%
    html_attr("href")
}