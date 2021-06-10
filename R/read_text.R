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
    clean_html(x)%>% 
      rvest::html_nodes('i,p,h1,h2,h3') %>%
      rvest::html_text()%>%
      stringr::str_replace_all(.,"\u0092","'") %>% 
      stringr::str_remove_all(.,'\"') %>% 
      stringr::str_remove_all(.,"\t") %>% 
      readr::read_lines()%>% 
      paste(collapse = " ")
  }
  }

clean_html<-function(x){
  if(stringr::str_detect(x,"#")){
    x1<-suppressMessages(xml2::read_html(x))
  }else{
    x1<-suppressMessages(xml2::read_html(url(x)))
  }
  #fedheading
  heading<-x1%>%
    rvest::html_nodes(".jumbotron.hidden-xs")
  xml2::xml_remove(heading)
  #fed sidebar
  navbar<-x1 %>% 
    rvest::html_nodes(".nav__header")
  xml2::xml_remove(navbar)
  #ecb footer
  footer_ecb<-x1 %>% 
    rvest::html_nodes(".ecb-footerBottom")
  xml2::xml_remove(footer_ecb)
  adress_ecb<-x1 %>% 
    rvest::html_nodes(".address-box.-top-arrow")
  xml2::xml_remove(adress_ecb)
  cookies_ecb<-x1 %>% 
    rvest::html_nodes(".ecb-cookieConsent.hidden")
  xml2::xml_remove(cookies_ecb)
  
  
  
  out<-x1
  return(out)
}


