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
   cat("The following files will be removed\n")
   cat(NULL, paste0(paste0("   ", monster), sep = "\n"))
   if(readline(prompt = "'y' to proceed? ") == "y"){
      file.remove(list.files(pattern=monster))
      cat("\nFiles erased\n")
   } else {
      cat("\nNo files erased\n")
   }
   invisible(NULL)
}
