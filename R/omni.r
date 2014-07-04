#' @title Keep an object omnipresent
#' @description Shorthand for \code{keep(..., autoload=TRUE)}
#' @details If .rnw files are cached, sometimes the objects used in 'Sexpr' are
#' unavailable. 'omni' stores variables in 'calc/autoload' which are 
#' automatically loaded in the first chunk of the rapport.
#' @author Henrik Renlund
#' @param name character; the name of a variable
#' @export
 
omni <- function(name) keep(name=name, autoload=TRUE)