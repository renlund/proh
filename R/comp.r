#' @title Compile the rapport
#' @description Compile the rapport in a PROH structured project
#' @author Henrik Renlund
#' @param input should be 'rapport.rnw' (but can be changed)
#' @param settings should the settings be read off '_META_.r'?
#' @param clean should the LaTeX files be cleaned?
#' @param look should the pdf be opened after compilation? 
#' @param ... arguments to be passed to \code{knit}
#' @importFrom knitr knit2pdf
#' @export

comp <- function(input="rapport.rnw", settings=TRUE, clean=TRUE, look=FALSE,...){
   if(settings){
      tryCatch(X <- readLines(con="_META_.r"), error=function(e) stop("[proh:Comp] an error occured while reading '_META_.r'. (Does the file exist?)"))
      a <- which(grepl("( )*#( )*KNITR OPTIONS.*", X))
      b <- which(grepl("( )*#( )*PACKAGE OPTIONS.*", X))
      c <- which(grepl("( )*#( )*CREATE PDF.*", X))
      if(length(a)==1 & length(b)==1){
         eval(parse(text=paste0(X[(a+1):(b-1)])))
      } else {
         warning("[proh::Comp] There seems to be no 'KNITR OPTIONS' section to _META_")
      }
      if(length(b)==1 & length(c)==1){
         eval(parse(text=paste0(X[(b+1):(c-1)])))
      } else {
         warning("[proh::Comp] There seems to be no 'PACKAGE OPTIONS' section to _META_")
      }
   }
   knit2pdf(input, ..., clean=TRUE, envir=.GlobalEnv)
   if(look) look()
   invisible(NULL)
}