read_bis_page<-function(x,countries_cb){
  page_summary<-suppressMessages(tidyRSS::tidyfeed(paste0("https://www.bis.org/doclist/cbspeeches.rss?from=&till=&page=",x,
                                         "&paging_length=25&sort_list=date_desc&theme=cbspeeches&ml=false&mlurl=&emptylisttext=")))
  title<-extract_title(page_summary)
  speaker<-extract_speaker(page_summary)
  speaker_position<-extract_position(page_summary)
  cb<-search_cb(speaker_position,page_summary$item_description,countries_cb %>%
                  dplyr::filter(complete.cases(.)))$cb
  country<-search_cb(speaker_position,page_summary$item_description,countries_cb %>%
                       dplyr::filter(complete.cases(.)))$country
  event<-extract_event(page_summary)
  date<-extract_date(page_summary)
  link<-page_summary$item_link
  text<-extract_text(page_summary)
  
  dplyr::tibble(title,speaker,cb,country,speaker_position,event,date,link,text)
}

c<-"test 01 February 2012"
pattern<-"\\d{1,2}\\s+(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|Jun(e)?|Jul(y)?|Aug(ust)?|Sep(tember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?)\\s+\\d{4}"

str_extract(c, pattern)


str_detect(c,pattern)

x=100
for(i in 1:5){print(i)
  read_bis_page(i,countries_cb)
}
