#' @title doc_features
#' @description returns document features

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname doc_features
#' @export 
doc_features<-function(){
  doc_features<-c("title","text","speaker","cb","country","speaker_position",
                  "location","date","release_date","type","chapter","language")
  doc_features_steady<-doc_features[-length(doc_features)]                
    
  return(list("doc_features"=doc_features,"doc_features_steady"=doc_features_steady))
}
