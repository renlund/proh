#' @title Compile the rapport
#' @description Compile the rapport
#' @author Henrik Renlund
#' @param input should be 'rapport.rnw' (but can be changed)
#' @param settings should the settings be read off '_META_.r'?
#' @param clean should the LaTeX files be cleaned?
#' @param look should the pdf be opened after compilation?
#' @param ... arguments to be passed to \code{knit}
#' @import knitr
#' @export

comp <- function(input="rapport.rnw", settings=FALSE, clean=TRUE, look=FALSE, ...){
   if(settings){
      X <- tryCatch(
         expr = readLines(con="_META_.r"),
         error=function(e) {
            message("[proh:Comp] an error occured while reading '_META_.r'. (Does the file exist?) Skipping!")
            NULL
            }
         )
      if(!is.null(X)){
         a <- which(grepl("( )*#( )*KNITR OPTIONS.*", X))
         b <- which(grepl("( )*#( )*PACKAGE OPTIONS.*", X))
         c <- which(grepl("( )*#( )*PROH OPTIONS.*", X))
         d <- which(grepl("( )*#( )*CREATE PDF.*", X))
         if(length(a)==1 & length(b)==1 & b > a + 1){
            eval(parse(text=paste0(X[(a+1):(b-1)])))
         } else {
            message("[proh::comp] There seems to be no 'KNITR OPTIONS' section to _META_")
         }
         if(length(b)==1 & length(c)==1 & c > b + 1){
            eval(parse(text=paste0(X[(b+1):(c-1)])))
         } else {
            message("[proh::comp] There seems to be no 'PACKAGE OPTIONS' section to _META_")
         }
         if(length(c)==1 & length(d)==1 & d > c+ 1){
            eval(parse(text=paste0(X[(c+1):(d-1)])))
         } else {
            message("[proh::comp] There seems to be no 'PROH OPTIONS' section to _META_")
         }
      }
   } else {
      hmm <- paste(rep("*", options("width")$width-5), collapse = "")
      cat(hmm, "\n")
      message("[proh::comp] default value of 'settings', since proh version 0.2.0, if FALSE")
      cat(hmm, "\n")
   }
   knit2pdf(input, ..., clean=clean, envir=.GlobalEnv)
   if(look) look()
   invisible(NULL)
}
