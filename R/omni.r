#' @title Keep an object omnipresent
#' @description Keep an object available within PROH structure
#' @details If .rnw files are cached, sometimes the objects used in 'Sexpr' are
#' unavailable. 'Keep' stores variables in 'calc/autoload' which are 
#' automatically loaded in the first chunk of a PROH rapport.
#' @author Henrik Renlund
#' @param name character; the name of a variable
#' @export
 
omni <- function(name){
   if(!all(is.character(name))) 
      stop("[proh::omni] 'name' should be the names (as a character vector) of variables.")
   for(K in name){
      if(exists(K, envir=.GlobalEnv)){
         save(list=name, envir=.GlobalEnv, file=file.path("calc", "autoload", paste0(K, ".rdat")))
      } else {
         warning(paste0("[proh::omni] '", K, "' does not exists."))
      }
   }
}