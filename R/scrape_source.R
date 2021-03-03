parts="minutes"
library(dplyr)
library(rvest)
library(data.table)
library(pdftools)
library(future.apply)
library(lubridate)

source<-list(name="boj")
parts="pc"
scrape_source<-function(source,parts,page=668){
  output<-list()
  
  if("minutes" %in% parts){
    minutes_links<-get_links(source,part="minutes")
    output$minutes<-load_data_from_links(minutes_links,"minutes",source$name)
  }
  if("pc" %in% parts){
    pc_links<-get_links(source,part="pc")
    output$pc<-load_data_from_links(links=pc_links,type="pc",cb=source$name)
  }
  if("speeches"%in% parts){
    if(source$name=="bis"){
      output$speech<-find_process_bis(page)
    }
  }
  if("beige" %in% parts){
    minutes_links<-get_links(source,part="beige")
    output$beige<-load_data_from_links(minutes_links,"beige",source$name)
  }
  if("green1" %in% parts){
    green1_links<-get_links(source,part="green1")
    output$green1<-load_data_from_links(green1_links,"green1",source$name)
  }
  if("green2" %in% parts){
    green2_links<-get_links(source,part="green2")
    output$green2<-load_data_from_links(green2_links,"green2",source$name)
  }
  if("teala" %in% parts){
    teala_links<-get_links(source,part="teala")
    output$teala<-load_data_from_links(teala_links,"teala",source$name)
  }
  if("tealb" %in% parts){
    tealb_links<-get_links(source,part="tealb")
    output$tealb<-load_data_from_links(tealb_links,"tealb",source$name)
  }
  if("green_sub" %in% parts){
    green_sub_links<-get_links(source,part="green_sub")
    output$green_sub<-load_data_from_links(green_sub_links,"green_sub",source$name)
  }
  if("blue" %in% parts){
    blue_links<-get_links(source,part="blue")
    output$blue<-load_data_from_links(blue_links,"blue",source$name)
  }
  if("agenda" %in% parts){
    agenda_links<-get_links(source,part="agenda")
    output$agenda<-load_data_from_links(agenda_links,"agenda",source$name)
  }
  if("transkript" %in% parts){
    transkript_links<-get_links(source,part="transkript")
    output$transkript<-load_data_from_links(transkript_links,"transkript",source$name)
  }
  if("red" %in% parts){
    red_links<-get_links(source,part="red")
    output$red<-load_data_from_links(red_links,"red",source$name)
  }
  if("economic_outlook" %in% parts){
    links<-get_links(source,part="economic_outlook")
    output$economic_outlook<-load_data_from_links(links,"economic_outlook",source$name)
  }
  if("interview" %in% parts){
    links<-get_links(source,part="interview")
    output$interview<-load_data_from_links(links,"interview",source$name)
  }
  if("blog" %in% parts){
    links<-get_links(source,part="blog")
    output$blog<-load_data_from_links(links,"blog",source$name)
  }
  if("release" %in% parts){
    links<-get_links(source,part="release")
    output$release<-load_data_from_links(links,"release",source$name)
  }
  if("outlook_report" %in% parts){
    links<-get_links(source,part="outlook_report")
    output$outlook_report<-load_data_from_links(links,"outlook_report",source$name)
  }
  
  
  return(output)
}
library(tidyverse)
x<-scrape_source(list(name="bis"),c("speeches"),page=10)

xml2::read_html("https://www.bis.org/cbspeeches/index.htm?cbspeeches") %>% 
  #rvest::html_nodes("html.js.localstorage.sessionstorage.rgba.opacity.boxshadow.flexbox.vcawhfdmws.idc0_328 body div#body.dt.tagwidth div#bispage div#pagecontent div#_cbspeeches_index_htm div.fullwidth-outer div.fullwidth-inner div#container.document_container div#center.defaultstyles.overridedefault.under900.under950 div#cmsContent div#cbspeeches.bisobj_document_list div.list div#cbspeeches_list.list div div.listtop.noprint") %>% 
  read_lines()
.listitems > b:nth-child(1)

x$interview[1,]$text
?str_detect
spacyr::find_spacy()
?find_spacy
library(spacyr)
spacy_download_langmodel(
  model = "en",
  envname = "spacy_condaenv",
  conda = "auto"
)
spacy_initialize(model = "en_core_web_sm")
?spacy_download_langmodel
x$minutes %>% 
  filter(type=="minutes")

