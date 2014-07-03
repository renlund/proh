#' @title (do not use)
#' @description Load all files in 'calc/autoload' - but this is now done with \code{fetchAll}. This function is kept for now to not break old code.
#' @author Henrik Renlund
#' @export

autoLoad <- function(){
   alist <- list.files(path=file.path('calc','autoload'), pattern="(.rdat)|(.rdata)", full.names=TRUE)
   for(L in alist) load(L, envir=.GlobalEnv)
   invisible(NULL)
}