#' @title Load all 'kept' objects
#' @description Load the contents of 'calc' and its subdirectory 'autoLoad'
#' @author Henrik Renlund
#' @param calc should contents of directory 'calc' be loaded?
#' @param autoload should contents of directory 'calc/autoload' be loaded?
#' @param overwrite if variable already exists in global workspace, should it be overwritten?
#' @param message do you want an explanatory message?
#' @param formats formats to look for. Default \code{c('.rdata', '.rdat')}.
#' @export

fetchAll <- function(calc=TRUE, autoload=TRUE, overwrite=TRUE, message=FALSE, formats=c("rdat", "rdata")){
  types <- paste0("\\.(", paste0(formats,collapse=")|("),")" ) 
  if(calc){ 
    tmp1 <- list.files(path='calc', recursive=FALSE, pattern=types, all.files=TRUE)
    tmp2 <- gsub(types, "", tmp1)
    for(name in tmp2) fetch(name, overwrite=overwrite, message=message, autoload=FALSE, formats=formats)
  }
  if(autoload) {
    tmp1 <- list.files(path=file.path('calc','autoload'), pattern=types, recursive=FALSE, all.files=TRUE)
    tmp2 <- gsub(types, "", tmp1)
    for(name in tmp2) fetch(name, overwrite=overwrite, message=message, autoload=TRUE, formats=formats)
  }
  invisible(NULL)
}
