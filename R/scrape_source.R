parts="minutes"
library(dplyr)
library(rvest)
library(data.table)
library(pdftools)
library("future.apply")
library(lubridate)

source<-list(name="fed")
parts="pc"
scrape_source<-function(source,parts){
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
      output$speech<-find_process_bis(1)
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
  
  return(output)
}

x<-scrape_source(list(name="fed"),c("transkript","blue","green_sub","tealb"))



x$minutes %>% 
  filter(type=="minutes")
