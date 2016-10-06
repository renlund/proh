#' @title Compile the rapport
#' @description Compile the rapport
#' @author Henrik Renlund
#' @param input file to be compiled
#' @param cess run proh::cess to get (possible) defaults in first chunk?
#' @param ... arguments to be passed on
#' @import knitr
#' @import rmarkdown
#' @export
cmp <- function(input=NULL, cess = TRUE, ...){
    if(grepl("\\.(R|r)nw$", input)){
        if(is.null(input)){
            opts_proh$check()
            input <- opts_proh$get("source_file")
        } else {
            if(cess){
                tryCatch(cess(profile = TRUE), error = function(e){
                    stop("FAILED to evaluate first chunk\nPerhaps you need to point to a file...")
                })
            }
        }
        cmp_rnw(input = input, ...)
    } else if(grepl("\\.(R|r)md$", input)){
        stop("methods for non-Rnw files not implemented")
        ## cmp_rmd(input = input, ...)
    }
}

#' @describeIn cmp Compile rnw files
#' @param clean should the LaTeX files be cleaned?
#' @param look should the pdf be opened after compilation?
#' @export
cmp_rnw <- function(input, clean = TRUE, look = FALSE, ...){
   opts_proh$check()
   ut <- sub("\\.pdf$", "\\.tex", opts_proh$get("output_file"))
   knitr::knit2pdf(
      input  = input,
      output = ut,
      clean  = clean,
      envir  = .GlobalEnv,
      ...
   )
   if(look) look(file = opts_proh$get("output_file"))
   invisible(NULL)
}

# - # @describeIn cmp Compile rmd files
# - # @param twice need two code executions? (e.g. for counters)
# - # @export
## cmp_rmd <- function(input, twice = FALSE, clean = FALSE, look = FALSE, ...){
##    opts_proh$check()
##    if(twice){
##       md <- sub("\\.rmd$", "\\.md$", )
##       if(md == input) stop("failed to make md file")
##       knitr::knit(input = input, output = md, envir = .GlobalEnv)
##    }
##    rmarkdown::render(input = input,
##                      output_format = opts_proh$get("output_format"),
##                      output_file   = opts_proh$get("output_file"),
##                      clean = clean,
##                      envir = .GlobalEnv)
##    if(look) look(file = opts_proh$get("output_file"))
##    invisible(NULL)
## }
