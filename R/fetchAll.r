#' @title Load all
#' @description Load the contents of 'calc' and its subdirectories (in particular 'autoLoad')
#' @author Henrik Renlund
#' @param calc should contents of directory 'calc' be loaded?
#' @param autoload should contents of directory 'calc/autoload' be loaded?
#' @export

fetchAll <- function(calc=TRUE, autoload=TRUE){
   LIST <- c()
   if(calc){ 
      x <- list.files(path='calc', recursive=FALSE, full.names=TRUE, include.dirs=FALSE, pattern="(.rdat)|(.rdata)")
      x <- setdiff(x, file.path('calc', 'autoload'))
      LIST <- c(LIST, x)
   }
   if(autoload) 
      LIST <- c(LIST, list.files(path=file.path('calc','autoload'), pattern="(.rdat)|(.rdata)", recursive=FALSE, full.names=TRUE, all.files=TRUE))
   for(L in LIST) load(L, envir=.GlobalEnv)
   invisible(NULL)
}
