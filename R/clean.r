#' @title Clean LaTeX files
#' @description Remove unnecessary LaTeX files
#' @details Sometimes LaTeX leaves a mess...
#' @author Henrik Renlund
#' @export

clean <- function(){
   look <- c(
      ".tex",
      ".toc",
      ".concordance",
      ".log",
      ".brf",
      ".bbl",
      ".blg",
      ".lof",
      ".out",
      ".aux",
      ".lot"
   )
   monster <- paste0("(", paste(paste0("(", look, ")"), collapse="|"), ")$")
   file.remove(list.files(pattern=monster))
   invisible(NULL)
}