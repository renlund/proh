#' @title Create table with attached info
#' @description \code{Hmisc::latex} and object, write to file and attach
#' @param object passed to \code{Hmisc::latex}
#' @param attach_table logical; should the table be attached? if missing this
#'   will be determined by \code{opts_proh$get("attach_table")}
#' @param label passed to \code{Hmisc::latex}
#' @param file passed to \code{Hmisc::latex}
#' @param where passed to \code{Hmisc::latex}
#' @param fun what function to use to write \code{object} to file
#' @param caption passed to \code{Hmisc::latex}
#' @param caption.lot passed to \code{Hmisc::latex}
#' @param data if attach data is something other than object
#' @param fe file extension given if data is written to file
#' @param data.file name of file 'object' or 'data' is written to
#' @param table.dir directory name in which to store 'data.file'
#' @param ... arguments passed to \code{Hmisc::latex}
#' @export

tableh <- function(object,
                   attach_table = NULL,
                   label = NULL,
                   file = "",
                   where = "htb",
                   fun = write.csv,
                   caption = NULL,
                   caption.lot = NULL,
                   data = object,
                   fe = "txt",
                   data.file = NULL,
                   table.dir = "tableh",
                   ...){
   object_name <- as.character(substitute(object))
   tidy_object_name <- gsub("_", "\\_", object_name, fixed = TRUE)
   if(is.null(caption) & is.null(caption.lot)){
      caption <- paste0("Table for \\texttt{",tidy_object_name,"}.")
      caption.lot <- caption
   }
   if(is.null(caption)) caption <- caption.lot
   if(is.null(caption.lot)) caption.lot <- caption
   if(is.null(label)) label <- paste0("tab:", object_name)
   if(!grepl("^\\.", fe)) fe <- paste0(".", fe)
   if(is.null(attach_table)) attach_table <- opts_proh$get("attach_table")[[1]]
   if(attach_table){
      if(!dir.exists(table.dir)) dir.create(table.dir)
      if(is.null(data.file)) data.file <- paste0(object_name, fe)
      file_path <- file.path(table.dir, data.file)
      fun(data, file=file_path)
      caption <- paste0(caption, " \\attachfile{",file_path,"}")
   }
   Hmisc::latex(
      object = object,
      file = file,
      label = label,
      caption = caption,
      where = where,
      caption.lot = caption.lot,
      ...
   )
}
