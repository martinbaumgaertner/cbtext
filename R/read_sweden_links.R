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

read_sweden_mpreports_links<-function(){
  
  new<-xml2::read_html(paste0("https://www.riksbank.se/en-gb/press-and-published/publications/monetary-policy-report/?year=Show+all"))%>%
    rvest::html_nodes("a")%>%
    rvest::html_attr("href") 
  
  archiv_adress<-unique(paste0(stringr::str_remove(new[grepl("Web-archive",new)],"Boxes-in-the-"),"index.html@all=1.html"))
  new<-new[grepl("monetary-policy-report-",new)]
  
  
  
    
  archiv<-xml2::read_html(paste0(archiv_adress))%>%
    rvest::html_nodes("a")%>%
    rvest::html_attr("href") 
  
  new<-paste0("https://www.riksbank.se",new)
  get_single_mpreport_link<-function(link){
    t<-xml2::read_html(link)%>%
      rvest::html_nodes("a")%>%
      rvest::html_attr("href")
    t<-t[grepl(".pdf",t)]
    t<-t[grepl("/monetary-policy-report-|rap_ppr",t)]
    t<-paste0("https://www.riksbank.se",t)
  }
  future::plan(future::multisession)
  new<-future.apply::future_lapply(as.list(new),get_single_mpreport_link,future.seed = T) %>% 
    unlist()
  
  archiv[!grepl("http://archive.riksbank.se",archiv)]<-paste0("https://www.riksbank.se",archiv[!grepl("http://archive.riksbank.se",archiv)])
  
  archiv<-archiv[grepl(".pdf",archiv)]
  archiv<-archiv[!grepl("diagram",archiv)]
  archiv<-archiv[!grepl("diagram",archiv)]
  
  full<-c(new,archiv)
   full<-stringr::str_remove_all(full,"../../../../../..")
  # full<-full[grepl(".pdf",full)]
  # full<-full[!grepl("diagram",full)]
  # full<-full[grepl("rapporter|Rapporter",full)]
  
  full<-full[full!="https://www.riksbank.se/Documents/Rapporter/PPR/2016/160907/rap_ppr_160907_eng_rättelse.pdf"]#ä in link
  full<-full[full!="http://archive.riksbank.se/Documents/Rapporter/PPR/2016/160907/rap_ppr_160907_eng_rättelse.pdf"]#ä in link
  full<-full[full!="http://archive.riksbank.se/Upload/Dokument_riksbank/Kat_publicerat/Rapporter/1993/penningpolindikatorer_93_juni_eng.pdf"]#no ocr
  full<-full[full!="http://archive.riksbank.se/Upload/Dokument_riksbank/Kat_publicerat/Rapporter/1993/inflforv93_okt_eng.pdf"]#no ocr
  
  return(full)
}

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
