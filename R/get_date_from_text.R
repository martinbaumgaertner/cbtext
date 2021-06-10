get_date_from_text<-function(texts,type){
  date_NA <- function(x) tryCatch(as.Date(x, tryFormats = c("%d/%m/%y","%m/%d/%y", "%Y/%m/%d",
                                                            "%d %B %Y","%d %B, %Y","%B %d, %Y","%dth %B %Y","%d.%m.%y")), error = function(e) NA)
  pattern<-list("\\d{1,2}(th)?(-|–)?(\\d{1,2})?\\s+(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|Jun(e)?|Jul(y)?|Aug(ust)?|Sep(tember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?)(,)?\\s+\\d{4}.?",
                "(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|Jun(e)?|Jul(y)?|Aug(ust)?|Sep(tember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?)\\s+\\d{1,2}(-|–)?(\\d{1,2})?(,)?\\s+\\d{4}",
                "\\d{1,2}\\/\\d{1,2}\\/\\d{1,4}",
                "\\d{1,2}\\.\\d{1,2}\\.\\d{1,4}",
                "\\d{1,2}(th)?(-|–)?(\\d{1,2})?\\s+(JAN(UARY)?|FEB(RUARY)?|MAR(CH)?|APR(IL)?|MAY|JUN(E)?|JUL(Y)?|AUG(UST)?|SEP(TEMBER)?|OCT(OBER)?|NOV(EMBER)?|DEC(EMBER)?)(,)?\\s+\\d{4}.?")
  out<-list(start_date=as.POSIXlt(as.Date(rep(NA,length(texts)))),
              end_date=as.POSIXlt(as.Date(rep(NA,length(texts)))))
  
  for(i in 1:length(texts)){
    
    if(type %in% c("blue","teala","tealb","green1","green2")){
      texts[i]=stringr::str_split(texts[i],"CONFIDENTIAL")[[1]][2]
    }
    
    first_pattern<-which.min(rowSums(stringr::str_locate(texts[i],unlist(pattern))))

    if(length(first_pattern)!=0){
      found_pattern<-c(stringr::str_extract_all(texts[i],pattern[[first_pattern]],simplify = T))
      
      if(found_pattern[1]==c("29 February 2009")){
        found_pattern[1]=c("28 February 2009")
      }
      
      if(stringr::str_detect(found_pattern[1],"(-|–)")){
        year=stringr::str_extract(found_pattern[1],"\\d{4}")
        month=stringr::str_extract(found_pattern[1],"(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|Jun(e)?|Jul(y)?|Aug(ust)?|Sep(tember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?)")
        days=c(stringr::str_split(stringr::str_extract(found_pattern[1],"\\d{1,2}(-|–)\\d{1,2}"),"(-|–)",simplify = T))
        
        date_start<-date_NA(paste0(month," ",days[[1]],", ",year))
        date_end<-date_NA(paste0(month," ",days[[2]],", ",year))
      }else if (stringr::str_detect(texts[i],"and continued on|and continuing on")){
        
        #sort dates because of pdf sorting mistakes
        dates<-sort(c(date_NA(found_pattern[1]),date_NA(found_pattern[2])))
        
        date_start<-dates[1]
        date_end<-dates[2]
        
      }else{
        date_start<-date_NA(found_pattern[1])
        date_end<-date_NA(found_pattern[1])
        }
      out$start_date[i]<-date_start
      out$end_date[i]<-date_end
    }else{
      out$start_date[i]<-NA
      out$end_date[i]<-NA
    }
  }
  return(out)
}
