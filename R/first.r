#' @title Run first chunk
#' @description This function executes the chunk called autoLoad 
#' (as created by \code{newProject})
#' @author Henrik Renlund
#' @export

first <- function(){
   X <- readLines(con="rapport.rnw")
   n <- which(grepl("^<<autoLoad.*", X))
   if(length(n)!=1) stop("[First] does two autoLoad chunk exist?")
   ms <- which(grepl("^@", X))
   m <- (ms[ms>n])[1]
   if(length(m)==0) stop("[First] does the autoLoad chunk never end?")
   X <- X[(n+1):(m-1)]
   eval(parse(text=paste(X, collapse="\n ")), envir=.GlobalEnv)
   invisible(NULL)
}
