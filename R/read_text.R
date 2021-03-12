x<-"http://archive.riksbank.se/Documents/Rapporter/PPR/2016/160907/rap_ppr_160907_eng_r%C3%A4ttelse.pdf"
read_text<-function(x,sour){
  if(grepl(".pdf",x)){
    suppressMessages(pdftools::pdf_text(x)) %>% 
      readr::read_lines(.)%>% 
      paste(collapse = " ")
  }else if(sour=="boj"){
    suppressMessages(xml2::read_html(x,encoding =  "utf8"))%>% 
      rvest::html_nodes(xpath = '//*[@id="contents-skip"]') %>% 
      rvest::html_text()%>%
      readr::read_lines()%>% 
      paste(collapse = " ")
  }else {
    suppressMessages(xml2::read_html(x))%>% 
      rvest::html_nodes('p,h1,h2,h3') %>%
      rvest::html_text()%>%
      stringr::str_replace_all(.,"\u0092","'") %>% 
      stringr::str_remove_all(.,'\"') %>% 
      stringr::str_remove_all(.,"\t") %>% 
      readr::read_lines()%>% 
      paste(collapse = " ")
  }
}
