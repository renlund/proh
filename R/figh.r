#' @title Caption generator
#' @description Use this with chunk option \code{fig.cap} to attach a copy
#' of the produced graphic in the resulting pdf.
#' @param s Caption
#' @param attach_graph attach graph? if \code{NULL} then this parameter will
#' be taken from \code{opts_proh$get('attach_graph')}
#' @param ext what is the file extension? if \code{NULL} then this parameter will
#' be taken from \code{opts_proh$get('graph_dev')}
#' @param warn warn if extension does not seem available?
#' @export

figh <- function(s, attach_graph = NULL, ext = NULL, warn = TRUE){
   if(is.null(attach_graph)) attach_graph <- opts_proh$get("attach_graph")
   if(attach_graph){
      lab <- knitr::opts_current$get('label')
      path <- opts_current$get('fig.path')
      if(is.null(ext)){
         ext <- opts_proh$get("graph_dev")
      }
      if(!ext %in% knitr::opts_current$get('dev')){
         if(warn) warning("[proh::fig] the file extension may not exist")
      }
      paste0(s, " \\attachfile{", path, lab,"-1.",ext,"}")
   } else {
      s
   }
}
