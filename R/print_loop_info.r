#' @title Print loop info
#' @description Prints progress
#' @param i loopin index (\code{i in 1:n})
#' @param n length of looping index
#' @param len number of times to get progress information
#' @examples
#' n <- 1E3
#' for(i in 1:n){
#'    print_loop_info(i, n)
#'    print_loop_info(i, n, len = 20, "       ", " <yeah>")
#' }; rm(i, n)
#' @export

print_loop_info <- function(i, n, len = 10, prefix = "", suffix = ""){
   if(i %in% floor(seq(1, n, length.out=len+1)[-1])){
      cat(prefix, round(100*i/n), "percent done.", suffix, "\n")
   }
}
