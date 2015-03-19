#' @title Kill Cache
#' @description Remove the contents of subdirectory 'cache', 'figure' and 'table'.
#' @details Sometimes it is easier clean the cache than to set \code{CACHE = FALSE} globally, since you may have forgotten to specify all dependencies...
#' @author Henrik Renlund
#' @param pattern if not \code{NULL} (default) this can search for cache items to remove
#' @param cache remove files from sub directory 'cache'? (default: TRUE)
#' @param figure remove files from sub directory 'figure'? (default: TRUE)
#' @param table remove files from sub directory 'table'? (default: TRUE)
#' @export

killCache <- function(pattern=NULL, cache=TRUE, figure=TRUE, table=TRUE){
   if(!is.null(pattern)){
      da_flies <- list.files(path = 'cache',  pattern = pattern, full.names=TRUE)
      if(length(da_flies)==0){
         cat("No matching files in directory 'cache'.\n")
      } else {
         cat(paste0("The following files will be deleted in directory 'cache':\n       ", paste0(da_flies, collapse="\n       ")))
         if(readline(prompt = "\n Press 'y' to accept, anything else to abort.\n         \n   ") == "y"){
            file.remove(da_flies)
         }
      }
      da_flies <- list.files(path = 'figure',  pattern = pattern, full.names=TRUE)
      if(length(da_flies)==0){
         cat("No matching files in directory 'figure'.\n")
      } else {
         cat(paste0("The following files will be deleted in directory 'cache':\n       ", paste0(da_flies, collapse="\n       ")))
         if(readline(prompt = "\n Press 'y' to accept, anything else to abort.\n         \n   ") == "y"){
            file.remove(da_flies)
         }
      }
      da_flies <- list.files(path = 'table',  pattern = pattern, full.names=TRUE)
      if(length(da_flies)==0){
         cat("No matching files in directory 'table'.\n")
      } else {
         cat(paste0("The following files will be deleted in directory 'cache':\n       ", paste0(da_flies, collapse="\n       ")))
         if(readline(prompt = "\n Press 'y' to accept, anything else to abort.\n         \n   ") == "y"){
            file.remove(da_flies)
         }
      }
   } else {
      if(cache)  file.remove(list.files('cache',  full.names=TRUE))
      if(figure) file.remove(list.files('figure', full.names=TRUE))
      if(table)  file.remove(list.files('table',  full.names=TRUE))
   }
   invisible(NULL)
}
