#' @title Save 
#' @description Save an object to 'calc' folder
#' @author Henrik Renlund
#' @param name character; the name of one or more variable
#' @export

keep <- function(name){
   if(!is.character(name)) 
      stop("[proh::keep] 'name' should be the names (as a character vector) of variables.")
   for(K in name){
      if(exists(K, envir=.GlobalEnv)){
         save(list=name, envir=.GlobalEnv, file=file.path("calc", paste0(K, ".rdat")))
      } else {
         warning(paste0("[proh::keep] '", K, "' does not exists."))
      }
   }
}
