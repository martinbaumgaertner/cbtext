#' Extract Dates from Text
#'
#' This function extracts dates from text using specified patterns and formats.
#'
#' @param texts A tibble containing the text from which dates will be extracted. The tibble must have a column named "text" containing the text data.
#' @param cb A character string indicating the source or context of the text (e.g., "boj", "fed").
#' @param type A character string indicating the type of text (e.g., "minutes", "teala").
#' @param links A character vector containing the links associated with the text.
#'
#' @return A list with three elements:
#'   \item{start_date}{A vector of start dates extracted from the text.}
#'   \item{end_date}{A vector of end dates extracted from the text.}
#'   \item{release_date}{A vector of release dates extracted from the text.}
#'
#' @details The function searches for specific date patterns in the provided text using regular expressions.
#'   It handles different formats and variations of dates commonly found in texts, such as "1st January 2022",
#'   "Jan 1, 2022", "01/01/22", etc. The extracted dates are then returned as start dates, end dates,
#'   and release dates, depending on the context and type of text.
#'
#' @examples
#' \dontrun{
#' texts <- tibble::tibble(text = c("The minutes of the meeting held on January 15, 2022 are available now.",
#'            "Please refer to the report published on 01/20/22 for more information."))
#' cb <- "boj"
#' type <- "minutes"
#' links <- c("https://www.example.com/minutes_2022.html", "https://www.example.com/report_2022.html")
#' get_date_from_text(texts, cb, type, links)
#' }
#'
#' @importFrom stringr str_replace str_detect str_extract_all str_remove_all
#' @importFrom lubridate year
#' @importFrom dplyr tibble pull mutate
#'
#' @export
get_date_from_text<-function(texts,cb,type,links){
  Sys.setlocale("LC_ALL","en_US.UTF-8")
  date_NA <- function(x) tryCatch(as.Date(x, tryFormats = c("%d/%m/%y","%m/%d/%y", "%Y/%m/%d",
                                                            "%d %B %Y","%d %B, %Y","%d %B. %Y","%B %d, %Y","%B %d %Y","%dth %B %Y","%d.%m.%y",
                                                            "%B %dth, %Y")), error = function(e) NA)
  pattern<-list("\\d{1,2}(th)?(-|–| - | and | AND )?(\\d{1,2})?( of)?\\s+(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|Jun(e)?|Jul(y)?|Aug(ust)?|Sep(tember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?|JAN(UARY)?|FEB(RUARY)?|MAR(CH)?|APR(IL)?|MAY|JUN(E)?|JUL(Y)?|AUG(UST)?|SEP(TEMBER)?|OCT(OBER)?|NOV(EMBER)?|DEC(EMBER)?).?(,)?\\s+(?:\\d{4}|\\d{2}).?",
                "(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|Jun(e)?|Jul(y)?|Aug(ust)?|Sep(tember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?)\\s+\\d{1,2}(th)?(-|–| - | and )?(\\d{1,2})?(,)?\\s+\\d{4}",
                "\\d{1,2}\\/\\d{1,2}\\/\\d{1,4}",
                "\\d{1,2}\\.\\d{1,2}\\.\\d{1,4}")
  
  #output is a list including the start and end date
  init <- rep(NA,nrow(texts))
  out<-list(start_date=as.POSIXlt(as.Date(init)),
              end_date=as.POSIXlt(as.Date(init)),
            release_date=as.POSIXlt(as.Date(init)))
  texts<-texts%>%
    #dplyr::tibble() %>% 
    dplyr::mutate(text = purrr::map_chr(text, ~ paste(unlist(.), collapse = " "))) %>% 
    dplyr::pull(text)
  
  for(i in 1:length(texts)){
    
    release_date=NA
    
    text=texts[i]
    
    #clean non english month abre
    text=stringr::str_replace(text,"Sept\\.","Sep")
    
    if(type %in% c("blue","teala","tealb","green1","green2")){
      #if type=T split text on "CONFIDENTIAL"
      text=stringr::str_split(text,"CONFIDENTIAL")[[1]][2]
    }
      #which is the first pattern found in text
      first_pattern<-which.min(rowSums(stringr::str_locate(text,unlist(pattern))))
      
      if(length(first_pattern)!=0){
        found_pattern<-c(stringr::str_extract_all(text,pattern[[first_pattern]],simplify = T))
        if(type=="minutes"){
          if(cb=="boj"&stringr::str_detect(links[i],".htm",negate=T)){
            # use first date in boj non-html minutes as release date
            release_date<-date_NA(unique(found_pattern)[1])
            found_pattern<-unique(found_pattern)[-1]
          }else if(cb=="boe"&stringr::str_detect(links[i],"\\/(199[0-9]|200[0-9]|201[0-4]\\/)",negate=F)|stringr::str_detect(links[i],"minutes-january-2015|minutes-february-2015|minutes-march-2015",negate=F)){
            release_date<-date_NA(unique(found_pattern)[1])
            found_pattern<-unique(found_pattern)[-1]
          }else if(cb=="fed"){
            release_date=NA
          }else if(cb=="poland"){
            release_date=stringr::str_extract_all(text,paste0("(Publication date: |Date of publication: )",pattern[[first_pattern]]),simplify = T)
            release_date=stringr::str_remove_all(release_date,"Publication date: |Date of publication: ")
            release_date=date_NA(release_date)
          }else if(cb=="ecb"){
            release_date=stringr::str_extract(links[i],"mg\\d{6}")
            release_date=stringr::str_remove(release_date,"mg")
            release_date=as.Date(release_date,"%y%m%d")
          }else if(cb=="iceland"){
            release_date=stringr::str_extract_all(text,paste0("(Published |Published: )",pattern[[first_pattern]]),simplify = T)
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
        if(found_pattern[1]%in%c("29 February 2009","29 February 2009.")){
          found_pattern[1]=c("28 February 2009")
        }
        
        if(stringr::str_detect(found_pattern[1],"(-|–| - |and|AND)")){
          year=stringr::str_extract(found_pattern[1],"\\d{4}")
          month=stringr::str_extract(found_pattern[1],"(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|Jun(e)?|Jul(y)?|Aug(ust)?|Sep(t)?(ember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?|JAN(UARY)?|FEB(RUARY)?|MAR(CH)?|APR(IL)?|MAY|JUN(E)?|JUL(Y)?|AUG(UST)?|SEP(TEMBER)?|OCT(OBER)?|NOV(EMBER)?|DEC(EMBER)?)")
          days=c(stringr::str_split(stringr::str_extract(found_pattern[1],"\\d{1,2}(-|–| - | and | AND )\\d{1,2}"),"(-|–| - | and | AND )",simplify = T))
          
          date_start<-date_NA(paste0(month," ",days[[1]],", ",year))
          date_end<-date_NA(paste0(month," ",days[[2]],", ",year))
        }else if (stringr::str_detect(text,"\\band continued on|\\band continuing on")){
          #sort dates because of pdf sorting mistakes
          dates<-sort(c(date_NA(found_pattern[1]),date_NA(found_pattern[2])))
          
          date_start<-dates[1]
          date_end<-dates[2]
          
        }else{
          date_start<-date_NA(found_pattern[1])
          date_end<-date_NA(found_pattern[1])
        }
        if(cb=="iceland"&type=="minutes"){
          #iceland uses 2 days discussing on economic situation and one day on interest. Last day is not found by pattern (Just: %d %B)
          date_end=date_end+1
        }else if(cb=="australia"&type=="minutes"){
          #minutes are released 2 weeks after decision
          release_date=date_end+14
        }
      }else{
        date_start=NA
        date_end=NA
      }
      
    if(cb=="bis"){
      if(length(date_start)>1){
        date_start<-c(na.omit(date_start))
      }
      if(length(date_end)>1){
        date_end<-c(na.omit(date_end))
      }
      if(length(release_date)>1){
        release_date<-c(na.omit(release_date))
      }
    }
      
    date_start<-fix_twodigit_year(date_start)
    date_end<-fix_twodigit_year(date_end)
    out$start_date[i]<-date_start
    out$end_date[i]<-date_end
    
    release_date<-fix_twodigit_year(release_date)
    out$release_date[i]<-release_date
  }
  
  return(out)
}

fix_twodigit_year<-function(date_in){
  if(length(date_in)==0){
    date_out<-NA
    return(date_out)
  }
  if(is.na(date_in)){
    date_out<-NA
    return(date_out)
  }
  if(lubridate::year(date_in)<=1000){
    if(lubridate::year(date_in)<=lubridate::year(Sys.time())-2000){
      #if year is NOT higher than current year add 2000
      date_out<-as.Date(date_in) + lubridate::years(2000)
    }else if(lubridate::year(date_in)>lubridate::year(Sys.time())-2000){
      #if year is higher than current year add 1900
      date_out<-as.Date(date_in) + lubridate::years(1900)
    }
  }else if(lubridate::year(date_in)>lubridate::year(Sys.time())){
      date_out=as.Date(date_in) - lubridate::years(100)
  }else{
    date_out=date_in
  }
  
  return(date_out)
}
