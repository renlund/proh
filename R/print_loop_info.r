#' @title Print loop info
#' @description Prints progress
#' @param i loopin index (\code{i in 1:n})
#' @param n length of looping index
#' @param len number of times to get progress information
#' @export

print_loop_info <- function(i,n,len=10){
   if(i %in% floor(seq(1, n, length.out=len+1)[-1])){
      cat(round(100*i/n), "percent done\n")
   }
}
