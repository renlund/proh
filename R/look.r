#' @title View the pdf
#' @description Open the current pdf version of the rapport (or indeed try to
#' open any file, as this is just a wrapper for \code{shell.exec})
#' @author Henrik Renlund
#' @param file filename (default 'rapport.pdf')
#' @export

look <- function(file = NULL){
   if(is.null(file)){
      opts_proh$check()
      file <- opts_proh$get("out_file")
   }
   shell.exec(file)
   invisible(NULL)
}
