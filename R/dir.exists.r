#' @title Directory exists?
#' @description Check if a directory exists
#' @param name name of directory to check for
#' @export

dir.exists <- function(name){
   is_it <- file.info(name)$isdir
   if(is.na(is_it)) FALSE else is_it
}
