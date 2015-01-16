#' @title Create table with attached info
#' @description \code{Hmisc::latex} and object, write to file and attach
#' @param object usually a data frame or matrix
#' @param attach_table logical; should the table be attached? if missing this
#'   will be determined by \code{opts_proh$get("attach_table")}
#' @param label a label 'tab:<label>' will be set in \code{latex}
#' @param fun what function to use to write \code{object} to file
#'   'table/<label>.txt'.
#' @param caption caption for \code{latex}
#' @param data if attach data is something other than object
#' @param fe file extension given if data is written to file
#' @param ... arguments passed to \code{latex}
#' @importFrom Hmisc latex
#' @export

tableh <- function(object, attach_table, label, fun=write.csv, caption, data, fe = "txt", ...){
   given_caption <- caption
   if(!grepl("^\\.", fe)) fe <- paste0(".", fe)
   if(missing(attach_table)) attach_table <- opts_proh$get("attach_table")[[1]]
   if(attach_table){
      if(missing(data)) data <- object
      if(!dir.exists("table")) dir.create("table")
      file_path <- file.path("table", paste0(label, fe))
      fun(data, file=file_path)
      caption <- paste0(caption, "\\attachfile{",file_path,"}")
   }
   latex(
      object = object,
      file = "",
      label = paste0("tab:", label),
      caption = caption,
      where = "htb",
      caption.lot = given_caption,
      ...
   )
}
