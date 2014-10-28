#' @title Create table with attached info
#' @description \code{Hmisc::latex} and object, write to file and attach
#' @param object usually a data frame or matrix
#' @param label a label 'tab:<label>' will be set in \code{latex}
#' @param fun what function to use to write \code{object} to file 'table/<label>.txt'.
#' @param caption caption for \code{latex}
#' @param data if attach data is something other than object
#' @param ... arguments passed to \code{latex}
#' @importFrom Hmisc latex
#' @export


tableh <- function(object, label, fun=write.csv, caption, data, ...){
   if(opts_proh$get("attach_table")[[1]]){
      if(missing(data)) data <- object
      if(!dir.exists("table")) dir.create("table")
      file_path <- file.path("table", paste0(label, ".txt"))
      fun(data, file=file_path, row.names=FALSE, quote=FALSE)
      caption <- paste0(caption, "\\attachfile{",file_path,"}")
   }
   latex(
      object = object,
      file = "",
      label = paste0("tab:", label),
      caption = caption,
      where = "htb",
      ...
   )
}
