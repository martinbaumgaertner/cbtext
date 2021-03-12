read_sweden_mpreports_links<-function(){
  new<-xml2::read_html(paste0("https://www.riksbank.se/en-gb/press-and-published/publications/monetary-policy-report/articles-in-the-monetary-policy-reports/"))%>%
    rvest::html_nodes("a")%>%
    rvest::html_attr("href") 
  
  archiv_adress<-paste0(stringr::str_remove(new[grepl("Web-archive",new)],"Boxes-in-the-"),"@all=1.html")
  
  archiv<-xml2::read_html(paste0(archiv_adress))%>%
    rvest::html_nodes("a")%>%
    rvest::html_attr("href") 
  
  new<-paste0("https://www.riksbank.se",new)
  archiv[!grepl("http://archive.riksbank.se",archiv)]<-paste0("https://www.riksbank.se",archiv[!grepl("http://archive.riksbank.se",archiv)])
  
  full<-c(new,archiv)
  full<-stringr::str_remove_all(full,"../../../../../..")
  full<-full[grepl(".pdf",full)]
  full<-full[!grepl("diagram",full)]
  full<-full[grepl("rapporter|Rapporter",full)]
  
  full<-full[full!="https://www.riksbank.se/Documents/Rapporter/PPR/2016/160907/rap_ppr_160907_eng_rättelse.pdf"]#ä in link
  full<-full[full!="http://archive.riksbank.se/Documents/Rapporter/PPR/2016/160907/rap_ppr_160907_eng_rättelse.pdf"]#ä in link
  full<-full[full!="http://archive.riksbank.se/Upload/Dokument_riksbank/Kat_publicerat/Rapporter/1993/penningpolindikatorer_93_juni_eng.pdf"]#no ocr
  full<-full[full!="http://archive.riksbank.se/Upload/Dokument_riksbank/Kat_publicerat/Rapporter/1993/inflforv93_okt_eng.pdf"]#no ocr
  
  return(full)
}


