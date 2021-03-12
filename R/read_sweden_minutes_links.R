read_sweden_minutes_links<-function(){
  new<-xml2::read_html(paste0("https://www.riksbank.se/en-gb/press-and-published/minutes-of-the-executive-boards-monetary-policy-meetings/"))%>%
    rvest::html_nodes("a")%>%
    rvest::html_attr("href") 
  
  archiv_adress<-paste0(stringr::str_remove(new[grepl("Web-archive",new)],"Boxes-in-the-"),"index.html@all=1.html")
  
  archiv<-xml2::read_html(archiv_adress) %>%
    rvest::html_nodes(xpath = "//td/a") %>% 
    rvest::html_attr("href")
  
   archiv<-paste0("http://archive.riksbank.se/en/Web-archive/Published/Minutes-of-the-Executive-Boards-monetary-policy-meetings/",archiv)
  
   archiv<-archiv[archiv!="http://archive.riksbank.se/en/Web-archive/Published/Minutes-of-the-Executive-Boards-monetary-policy-meetings/2008/Unanimous-Board-lowered-the-repo-rate-to-425-per-cent/index.html"]
  get_pdf_links<-function(link){
    links<-xml2::read_html(paste0(link))%>%
      rvest::html_nodes("a")%>%
      rvest::html_attr("href") 
    
    links<-links[grepl(".pdf",links)]
    return(links)
  }
  pdf_archiv<-lapply(as.list(archiv), get_pdf_links)%>% unlist() %>% 
    stringr::str_remove_all(.,"../../../../../..")
  
  pdf_archiv[!grepl("http://archive.riksbank.se",pdf_archiv)]<-paste0("http://archive.riksbank.se",pdf_archiv[!grepl("http://archive.riksbank.se",pdf_archiv)])
  
  new<-new[grepl(".pdf",new)]
  new<-paste0("https://www.riksbank.se",new)
  
  full<-c(new,pdf_archiv)
  
  full[full=="http://archive.riksbank.se../Pagefolders/37125/ppp081008e.pdf"]<-"http://archive.riksbank.se/Pagefolders/37125/ppp081008e.pdf"
  
  return(full)
}

