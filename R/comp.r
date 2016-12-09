##' Compile rnw files
##'
##' Wrapper for \code{knitr::knit2pdf}
##' @param input file to compile
##' @param output output tex file
##' @param clean should the LaTeX files be cleaned?
##' @param ... arguments passed to \code{\link[knitr]{knit2pdf}}
##' @export
##' @importFrom knitr knit2pdf
cmp_rnw <- function(input, output, clean = TRUE, ...){
    if(!grepl("\\.(R|r)nw$", input)){
        stop("methods for non-Rnw files not implemented")
    }
    ut <- sub("\\.pdf$", "\\.tex", output)
    knitr::knit2pdf(
        input  = input,
        output = ut,
        clean  = clean,
        envir  = .GlobalEnv,
        ...
    )
    invisible(NULL)
}

##' @describeIn cmp_rnw apply to 'source_file'
##' @param cess run \code{proh::cess} to get (possible) defaults in first chunk?
##' @export
cmp <- function(cess = TRUE, ...){
    if(cess){
        tryCatch(cess(), error = function(e){
            stop("FAILED to evaluate first chunk\n")
        })
    }
    opts_proh$check()
    input <- opts_proh$get("source_file")
    output <- opts_proh$get("output_file")
    cmp_rnw(input = input, output = output, ...)
}

##' @describeIn cmp_rnw apply to 'dm_source_file'
##' @export
cmp_dm <- function(cess = TRUE, ...){
    if(cess){
        tryCatch(cess_dm(), error = function(e){
            stop("FAILED to evaluate first chunk\n")
        })
    }
    opts_proh$check()
    input <- opts_proh$get("dm_source_file")
    output <- opts_proh$get("dm_output_file")
    cmp_rnw(input = input, output = output, ...)
}

