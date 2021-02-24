#' @title FUNCTION_TITLE
#' @description Creates folder to source object
#' @param data_path base path to store collected data
#' @param source_name name of data source
#' @param folder PARAM_DESCRIPTION, Default: c("minutes", "pc")
#' @return 
#' @details creates a tree of folder under data_path to store the collected data in html, pdf and text
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname source_folder
#' @export 
source_folder<-function(data_path,source_name,folder=c("minutes","pc")){
  list<-list()
  
  list$base_path<-paste0(data_path,"cb/",source_name,"/")
  dir.create(file.path(list$base_path), showWarnings = FALSE)
  
  subpath<-matrix(NA,1,length(folder),dimnames=list(NULL,paste0(folder,"_path")))
  for(i in 1:length(folder)){
    subpath[i]<-paste0(list$base_path,folder[i],"/")
  }
  lapply(subpath, function(x){dir.create(file.path(x), showWarnings = FALSE)})
  
  w<-as.data.frame(matrix(NA,length(folder),4,dimnames=list(colnames(subpath),
                                                           c("base","html","pdf","txt"))))
  
  for(j in 1:length(subpath)){
    for(i in 1:4){
      d<-c("","html/","pdf/","txt/")[i]
      w[j,i]<-paste0(subpath[j],d)
    }
  }
  
  xy.list <- split(w, seq(nrow(w)))
  xy.list <- setNames(split(w, seq(nrow(w))), rownames(w))
  
  list<-c(list,xy.list)
  for(i in names(xy.list)){
    dat<-xy.list[i] %>% unlist()
    for(j in names(dat)){
      dir.create(dat[j], showWarnings = FALSE)
      writeLines(c("*.pdf","*.html","*.txt","*.htm",".xlsx",".Rdata"),paste0(dat[j],".gitignore"))
    }
  }
  return(list)
}
