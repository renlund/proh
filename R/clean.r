#' @title Clean LaTeX files
#' @description Remove unnecessary LaTeX files
#' @details Sometimes LaTeX leaves a mess...
#' @author Henrik Renlund
#' @export

clean <- function(){
   removables <- c(
      ".tex",
      ".toc",
      ".concordance",
      "concordance.tex",
      ".log",
      ".brf",
      ".bbl",
      ".blg",
      ".lof",
      ".out",
      ".aux",
      ".lot",
      ".synctex.gz",
      ".nav",
      ".snm",
      "vrb"
   )
   monster <- paste0("(", paste(paste0("(\\", removables, ")"), collapse="|"), ")$")
   file.remove(list.files(pattern=monster))
   invisible(NULL)
}
