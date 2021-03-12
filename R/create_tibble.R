#' @title create_tibble
#' @description prepares and empty tibble
#' @param names columnnames
#' @param rlength rows
#' @return tibble
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{reexports}}
#' @rdname create_tibble
#' @export 
#' @importFrom dplyr as_tibble
create_tibble<-function(names,rlength){
  dplyr::as_tibble(matrix(NA,rlength,length(names),dimnames = list(NULL,names)))
}

