#' @title Kill Cache
#' @description Remove the contents of subdirectory 'cache', 'figure' and 'table'.
#' @details Sometimes it is easier clean the cache than to set \code{CACHE = FALSE} globally, since you may have forgotten to specify all dependencies... 
#' @author Henrik Renlund
#' @param cache remove files from sub directory 'cache'? (default: TRUE)
#' @param figure remove files from sub directory 'figure'? (default: TRUE)
#' @param table remove files from sub directory 'table'? (default: TRUE)
#' @export

killCache <- function(cache=TRUE, figure=TRUE, table=TRUE){
   if(cache)  file.remove(list.files('cache',  full.names=TRUE))
   if(figure) file.remove(list.files('figure', full.names=TRUE))
   if(table)  file.remove(list.files('table',  full.names=TRUE))
   invisible(NULL)
}