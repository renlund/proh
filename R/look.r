#' @title View the pdf
#' @description Open the current pdf version of the rapport
#' @author Henrik Renlund
#' @param sumatra not implemented yet, will be FALSE
#' @export

look <- function(sumatra=FALSE){
   if(!identical(sumatra, FALSE)){
      warning("[proh::Pdf] sumatra!=FALSE currently unsupported")
      sumatra <- FALSE
   }
   if(sumatra){
      sumatra_path <- opts_proh$get("sumatra_path")[[1]]
      sumatra <- file.path(sumatra_path,"SumatraPDF.exe")
      if(!file.exists(sumatra)){
         warning("[proh::View] SumatraPDF.exe does not exists in default windows location. You can change the path with 'opts_proh$set'.")
      } else {
         wd <- getwd()
         rapport <- file.path(wd, "rapport.pdf", fsep="\\")
         setwd(sumatra_path)
         shell.exec(paste("SumatraPDF.exe", rapport))
         setwd(wd)
         return(invisible(NULL))
      }
   }
   shell.exec("rapport.pdf")
   invisible(NULL)
}