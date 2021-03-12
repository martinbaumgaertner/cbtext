read_sweden_economicreview_links<-function(){
  new<-xml2::read_html(paste0("https://www.riksbank.se/en-gb/press-and-published/publications/economic-review/"))%>%
    rvest::html_nodes("a")%>%
    rvest::html_attr("href") 
  
  archiv_adress<-paste0(stringr::str_remove(new[grepl("Web-archive",new)],"Boxes-in-the-"),"index.html@all=1.html")
  
  archiv<-xml2::read_html(paste0(archiv_adress))%>%
    rvest::html_nodes("a")%>%
    rvest::html_attr("href") 
  
  new[!grepl("https://www.riksbank.se",new)]<-paste0("https://www.riksbank.se",new[!grepl("https://www.riksbank.se",new)])
  archiv[!grepl("http://archive.riksbank.se",archiv)]<-paste0("http://archive.riksbank.se",archiv[!grepl("http://archive.riksbank.se",archiv)])
  
  full<-c(new,archiv)
  
  full<-stringr::str_remove_all(full,"../../../../../..")
  full<-full[grepl(".pdf",full)]
  full<-full[grepl("rapporter|Rapporter|Upload",full)]
  
  full<-full[full!="http://archive.riksbank.se/Upload/Dokument_riksbank/Kat_publicerat/PoV_sve/eng/2002/economic_review_2001_2.pdf"]
  full<-full[full!="http://archive.riksbank.se/Upload/Dokument_riksbank/Kat_publicerat/PoV_sve/eng/2002/eco_review_2001_1.pdf"]
  
  return(full)
}
