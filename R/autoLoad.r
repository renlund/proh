#' @title autoFetch
#' @description Load all files in 'calc/autoload'
#' @author Henrik Renlund
#' @export

autoLoad <- function(){
   alist <- list.files(path=file.path('calc','autoload'), pattern="(.rdat)|(.rdata)", full.names=TRUE)
   for(L in alist) load(L, envir=.GlobalEnv)
   invisible(NULL)
}