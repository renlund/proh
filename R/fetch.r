#' @title Fetch
#' @description Load an object from 'calc' directory
#' @author Henrik Renlund
#' @param name character; the name of a variable
#' @param overwrite if variable already exists in global workspace, should it be overwritten?
#' @param message do you want a message?
#' @export

fetch <- function(name, overwrite=TRUE, message=FALSE){
   if(!is.character(name)) 
      stop("[proh::Load] 'name' should be the names (as a character vector) of variables saved in subdirectory 'calc'.")
   L <- gsub(".rdat", "", list.files('calc'))
   if(length(L)==0) stop("[proh::Load] there are no saves whatsoever")
   for(K in name){
      if(K %in% L){
         dummy <- if(exists(K, envir=.GlobalEnv)) 1 else 0
         if(dummy==1){
            if(overwrite) {
               load(file=file.path("calc",paste0(K, ".rdat")), envir=.GlobalEnv)
               if(message) message(paste0("[proh::Load] '",K,"' was overwritten."))
            } else {
               if(message) message(paste0("[proh::Load] '",K,"' exists and was not overwritten."))
            }
         } else {
            load(file=file.path("calc",paste0(K, ".rdat")), envir=.GlobalEnv)
            if(message) message(paste0("[proh::Load] '",K,"' dit not exist and was loaded."))
         }
      } else {
         warning(paste0("[proh::Load] '", K, "' does not exists in sub directory 'calc'."))
      }
   }
}