#' @title Shorthand for \code{fetchAll(calc=F, autoload=T)}
#' @description Load all files in 'calc/autoload'. 
#' @author Henrik Renlund
#' @export

autoLoad <- function() fetchAll(calc=FALSE, autoload=TRUE)