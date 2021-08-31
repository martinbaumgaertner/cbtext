get_date_from_text<-function(texts,cb,type,links){
  Sys.setlocale("LC_ALL","English")
  date_NA <- function(x) tryCatch(as.Date(x, tryFormats = c("%d/%m/%y","%m/%d/%y", "%Y/%m/%d",
                                                            "%d %B %Y","%d %B, %Y","%B %d, %Y","%dth %B %Y","%d.%m.%y")), error = function(e) NA)
  pattern<-list("\\d{1,2}(th)?(-|–| and )?(\\d{1,2})?\\s+(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|Jun(e)?|Jul(y)?|Aug(ust)?|Sep(tember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?)(,)?\\s+\\d{4}.?",
                "(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|Jun(e)?|Jul(y)?|Aug(ust)?|Sep(tember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?)\\s+\\d{1,2}(-|–| and )?(\\d{1,2})?(,)?\\s+\\d{4}",
                "\\d{1,2}\\/\\d{1,2}\\/\\d{1,4}",
                "\\d{1,2}\\.\\d{1,2}\\.\\d{1,4}",
                "\\d{1,2}(th)?(-|–| and )?(\\d{1,2})?\\s+(JAN(UARY)?|FEB(RUARY)?|MAR(CH)?|APR(IL)?|MAY|JUN(E)?|JUL(Y)?|AUG(UST)?|SEP(TEMBER)?|OCT(OBER)?|NOV(EMBER)?|DEC(EMBER)?)(,)?\\s+\\d{4}.?")
  #output is a list including the start and end date
  out<-list(start_date=as.POSIXlt(as.Date(rep(NA,length(texts)))),
              end_date=as.POSIXlt(as.Date(rep(NA,length(texts)))),
            release_date=as.POSIXlt(as.Date(rep(NA,length(texts)))))
  
  for(i in 1:length(texts)){
    
    release_date=NA
    
    if(type %in% c("blue","teala","tealb","green1","green2")){
      #if type=T split text on "CONFIDENTIAL"
      texts[i]=stringr::str_split(texts[i],"CONFIDENTIAL")[[1]][2]
    }
      #which is the first pattern found in text
      first_pattern<-which.min(rowSums(stringr::str_locate(texts[i],unlist(pattern))))
      
      if(length(first_pattern)!=0){
        found_pattern<-c(stringr::str_extract_all(texts[i],pattern[[first_pattern]],simplify = T))
        
        if(type=="minutes"){
          if(cb=="boj"&stringr::str_detect(links[i],".htm",negate=T)){
            # use first date in boj non-html minutes as release date
            release_date<-date_NA(unique(found_pattern)[1])
            found_pattern<-unique(found_pattern)[-1]
          }else if(cb=="boe"&stringr::str_detect(links[i],"\\/(199[0-9]|200[0-9]|201[0-4]\\/)",negate=T)){
            release_date<-date_NA(unique(found_pattern)[1])
            found_pattern<-unique(found_pattern)[-1]
          }else if(cb=="fed"){
            release_date=NA
          }else if(cb=="poland"){
            release_date=stringr::str_extract_all(texts[i],paste0("Publication date: |Date of publication: ",pattern[[first_pattern]]),simplify = T)
            release_date=stringr::str_remove_all(release_date,"Publication date: |Date of publication: ")
            release_date=date_NA(release_date)
          }else if(cb=="ecb"){
            release_date=stringr::str_extract(links[i],"mg\\d{6}")
            release_date=stringr::str_remove(release_date,"mg")
            release_date=as.Date(release_date,"%y%m%d")
          }else if(cb=="iceland"){
            release_date=stringr::str_extract_all(texts[i],paste0("Published |Published: ",pattern[[first_pattern]]),simplify = T)
            release_date=stringr::str_remove_all(release_date,"Published |Published: ")
            release_date=date_NA(release_date)
            found_pattern<-unique(found_pattern)[-1]
          }else if(cb=="riksbank"){
            release_date=NA
          }else if(cb=="australia"){
            release_date=NA
          }else{
            release_date<-date_NA(unique(found_pattern)[2])
          }
        }
        
        #correct encoding mistake
        if(found_pattern[1]==c("29 February 2009")){
          found_pattern[1]=c("28 February 2009")
        }
        
        if(stringr::str_detect(found_pattern[1],"(-|–|and)")){
          year=stringr::str_extract(found_pattern[1],"\\d{4}")
          month=stringr::str_extract(found_pattern[1],"(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|Jun(e)?|Jul(y)?|Aug(ust)?|Sep(tember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?)")
          days=c(stringr::str_split(stringr::str_extract(found_pattern[1],"\\d{1,2}(-|–| and )\\d{1,2}"),"(-|–| and )",simplify = T))
          
          date_start<-date_NA(paste0(month," ",days[[1]],", ",year))
          date_end<-date_NA(paste0(month," ",days[[2]],", ",year))
        }else if (stringr::str_detect(texts[i],"\\band continued on|\\band continuing on")){
          #sort dates because of pdf sorting mistakes
          dates<-sort(c(date_NA(found_pattern[1]),date_NA(found_pattern[2])))
          
          date_start<-dates[1]
          date_end<-dates[2]
          
        }else{
          date_start<-date_NA(found_pattern[1])
          date_end<-date_NA(found_pattern[1])
        }
      
      }
      if(cb=="iceland"&type=="minutes"){
        #iceland uses 2 days discussing on economic situation and one day on interest. Last day is not found by pattern (Just: %d %B)
        date_end=date_end+1
      }else if(cb=="australia"&type=="minutes"){
        #minutes are released 2 weeks after decision
        release_date=date_end+14
      }
      
    out$start_date[i]<-date_start
    out$end_date[i]<-date_end
    if(is.null(release_date)){
      release_date=NA
    }
    out$release_date[i]<-release_date
    
  }
  return(out)
}
