read_text<-function(x){
  if(grepl(".pdf",x)){
    pdftools::pdf_text(x) %>% 
      readr::read_lines(.)
  }else{
    read_html(x)%>% 
      html_nodes(xpath = '//*[@id="contents-skip"]') %>% 
      html_text()%>%
      readr::read_lines()
  }
}
x="https://www.boj.or.jp/en/mopo/mpmsche_minu/minu_2020/g200316.htm"
