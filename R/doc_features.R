doc_features<-function(){
  doc_features<-c("title","text","speaker","cb","country","speaker_position",
                  "location","date","release_date","type","chapter")
  doc_features_steady<-doc_features[-length(doc_features)]                
    
  return(list("doc_features"=doc_features,"doc_features_steady"=doc_features_steady))
}
