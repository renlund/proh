#' @title Save
#' @description Save an object to 'calc' folder
#' @author Henrik Renlund
#' @param name character; the name of one or more variable
#' @param autoload should the object be saved in subdirectory 'calc/autoload'?
#' @export

keep <- function(name, autoload=FALSE){
  location <- if(autoload) file.path('calc', 'autoload') else 'calc'
  if(!is.character(name))
      stop("[proh::keep] 'name' should be the names (as a character vector) of variables.")
   for(K in name){
      if(exists(K, envir=.GlobalEnv)){
         L <- list.files(path = "calc")
         if(any(grepl(K, L, ignore.case=TRUE)) & !any(grepl(K, L))){
            if(Sys.info()['sysname'] == "Windows") stop("[proh::keep] Windows does not distinguish between upper- and lower case in filenames and there is a similar file kept in 'calc/'.")
         }
         save(list=K, envir=.GlobalEnv, file=file.path(location, paste0(K, ".rdat")))
      } else {
         warning(paste0("[proh::keep] '", K, "' does not exists."))
      }
   }
}
