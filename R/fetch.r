#' @title Fetch
#' @description Load an object from 'calc' directory
#' @author Henrik Renlund
#' @param name character; the name of a variable
#' @param overwrite if variable already exists in global workspace, should it be overwritten?
#' @param message do you want an explanatory message?
#' @param autoload is the variable in subdirectory 'autoload'? Default \code{FALSE}
#' @param formats formats to look for. Default \code{c('.rdata', '.rdat')}.
#' @export

fetch <- function(name, overwrite=TRUE, message=FALSE, autoload=FALSE, formats=c("rdat", "rdata")){
   if(!is.character(name)) 
      stop("[proh::fetch] 'name' should be the names (as a character vector) of variables saved")
   types <- paste0("\\.(", paste0(formats,collapse=")|("),")" )
   location <- if(autoload) file.path("calc", "autoload") else "calc"
   Lext <- list.files(location, pattern=types, all.files=TRUE)
   L <- gsub(types, "", Lext)
   if(length(Lext)==0) stop("[proh::fetch] there are no saves whatsoever")
   for(K in name){
      if(K %in% L){
         dummy <- if(exists(K, envir=.GlobalEnv)) 1 else 0
         place <- which(L==K)
         if(dummy==1){
            if(overwrite) {
               load(file=file.path(location, Lext[place]), envir=.GlobalEnv)
               if(message) message(paste0("[proh::fetch] '", K, "' was overwritten."))
            } else {
               if(message) message(paste0("[proh::fetch] '", K, "' exists and was not overwritten."))
            }
         } else {
            load(file=file.path(location, Lext[place]), envir=.GlobalEnv)
            if(message) message(paste0("[proh::fetch] '",K,"' dit not exist and was loaded."))
         }
      } else {
         warning(paste0("[proh::fetch] '", K, "' does not exists in directory '",location,"'."))
      }
   }
}