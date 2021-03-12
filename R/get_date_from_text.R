get_date_from_text<-function(texts){
  date_NA <- function(x) tryCatch(as.Date(x, tryFormats = c("%d/%m/%y","%m/%d/%y", "%Y/%m/%d",
                                                            "%d %B %Y","%d %B, %Y","%B %d, %Y","%dth %B %Y","%d.%m.%y")), error = function(e) NA)
  pattern<-list("\\d{1,2}\\s+(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|Jun(e)?|Jul(y)?|Aug(ust)?|Sep(tember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?)\\s+\\d{4}",
                "\\d{1,2}\\s+(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|Jun(e)?|Jul(y)?|Aug(ust)?|Sep(tember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?),\\s+\\d{4}",
                "\\d{1,2}\\s+(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|Jun(e)?|Jul(y)?|Aug(ust)?|Sep(tember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?)\\s+\\d{4}.",
                "\\d{1,2}\\/\\d{1,2}\\/\\d{1,2}",
                "\\d{1,2}\\.\\d{1,2}\\.\\d{1,2}",
                "(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|Jun(e)?|Jul(y)?|Aug(ust)?|Sep(tember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?)\\s+\\d{1,2},\\s+\\d{4}",
                "\\d{1,2}th\\s+(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|Jun(e)?|Jul(y)?|Aug(ust)?|Sep(tember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?)\\s+\\d{4}",
                "\\d{1,2}\\s+(JAN(UARY)?|FEB(RUARY)?|MAR(CH)?|APR(IL)?|MAY|JUN(E)?|JUL(Y)?|AUG(UST)?|SEP(TEMBER)?|OCT(OBER)?|NOV(EMBER)?|DEC(EMBER)?)\\s+\\d{4}")
  out<-as.POSIXlt(as.Date(rep(NA,length(texts))))
  for(i in 1:length(texts)){
    first_pattern<-which.min(rowSums(stringr::str_locate(texts[i],c(pattern[[1]],pattern[[2]],pattern[[3]],
                                                           pattern[[4]],pattern[[5]],pattern[[6]],
                                                           pattern[[7]],pattern[[8]]))))
    
    if(length(first_pattern)!=0){
      found_pattern<-stringr::str_extract(texts[i],pattern[[first_pattern]])
      if(found_pattern==c("29 February 2009")){
        found_pattern=c("28 February 2009")
      }
      out[i]<-date_NA(found_pattern)
    }else{
      out[i]<-NA
    }
  }
  return(out)
}
