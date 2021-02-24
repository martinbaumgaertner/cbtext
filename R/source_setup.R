#' @title FUNCTION_TITLE
#' @description creates a source object including folder structure
#' @param data_path base path to store collected data
#' @param name name of data source
#' @param folder vector of strings with all folder to estabilsh
#' @param urls list of urls for sources, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  ecb<-source_setup(data_path,"ecb",list(voting_url = NA,speeches_url="https://www.ecb.europa.eu/press/key/shared/data/all_ECB_speeches.csv"))
#'  }
#' }
#' @rdname source_setup
#' @export 
source_setup<-function(data_path,name,folder=c("minutes", "pc"),urls=list(voting_url = NA,speeches_url=NA)){
  folder<-source_folder(data_path,name)
  out<-list(name=name,url=urls,folder=folder)
  return(out)
}
