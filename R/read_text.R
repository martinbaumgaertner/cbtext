read_text<-function(x,sour){
  if(grepl(".pdf",x)){
    pdftools::pdf_text(x) %>% 
      readr::read_lines(.)
  }else if(sour=="boj"){
    xml2::read_html(x)%>% 
      rvest::html_nodes(xpath = '//*[@id="contents-skip"]') %>% 
      rvest::html_text()%>%
      readr::read_lines()
  }else {
    xml2::read_html(x)%>% 
      rvest::html_text()%>%
      readr::read_lines()
  }
}

