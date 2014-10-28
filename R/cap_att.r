#' @title Caption for attaching
#' @description Caption that possible makes a caption that attaches a figure
#' @param caption the caption
#' @param chunk the chunk that contains the plot
#' @export

cap_att  <- function(caption, chunk){
   if(opts_proh$get("attach_graph")[[1]]){
      paste0(caption, " \\attachfile{figure/", chunk, "-1.", .dev(), "}")
   } else {
      caption
   }
}

#' @export

.dev <- function() opts_proh$get("graph_dev")[[1]]

#' @export

.keep <- function() {
   if(!opts_proh$get("attach_graph")[[1]]){
      'none'
   } else {
      opts_chunk$get('fig.keep')
   }
}
