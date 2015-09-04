#' @title Compile the rapport
#' @description Compile the rapport
#' @author Henrik Renlund
#' @param input should be 'rapport.rnw' (but can be changed)
#' @param clean should the LaTeX files be cleaned?
#' @param look should the pdf be opened after compilation?
#' @param ... arguments to be passed to \code{knit}
#' @import knitr
#' @export

comp <- function(input=NULL, clean=TRUE, look=FALSE, ...){
   if(is.null(input)){
      opts_proh$check()
      input <- opts_proh$get("main_document")
   }
   if(grepl("\\.rnw$", input)){
      knitr::knit2pdf(input = input, output = opts_proh$get("output_file"),
                      clean=clean, envir=.GlobalEnv, ...)
   } else if(grepl("\\.rmd$", input)){
      opts_proh$check()
      md_file <- gsub("\\.rmd$", "\\.md", input)
      out_form <- opts_proh$get("output_format")
      knitr::knit(input = input, output = md_file)
      rmarkdown::render(input = md_file,
                        output_format = out_form,
                        output_file = opts_proh$get("output_file"))
   }
   if(look) look()
   invisible(NULL)
}
