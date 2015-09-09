#' @title Capions in rmarkdown
#' @description This function allows enumerated tables and figures in rmarkdown
#'   with html or word output (and pdf I suppose).
#' @details The \code{md_cap} function adds a list element to the \code{loft}
#'   list in \code{opts_proh}. Use \code{md_tcap} for tables and
#'   \code{md_fcap}. (If \code{at.ref} is \code{TRUE} these are converted by
#'   \code{md_enumerate_at_ref} to two referrable, numbered list (List of Tables
#'   and List of Figures, respectively), which, when created, will have the
#'   effect of replacing the @@-label with a number.
#' @param label the label you can refer to in the .rmd document. If \code{NULL}
#'   it will inherit the 'label' from the current chunk
#' @param caption the caption
#' @param type "table" or "figure". User do not have to care as you use
#'   \code{md_tcap} or \code{md_fcap}, which calls \code{md_cap} with this
#'   argument fixed
#' @param at.ref if \code{TRUE} use pandoc markdown's enumerated list to create
#'   the references (default \code{FALSE}). This option will most likely be
#'   removed in the future!
#' @note If the chunk with \code{md_enumerate_labels} is at the beginning of the
#'   document you probably have to \code{knit} the document twice before the
#'   List of Tables/Figures appears, since the \code{md_cap} function has not
#'   actually been called yet.
#' @examples
#' # <<>>=
#' # md_enumerate_labels() # knit twice first time
#' # @@
#' #
#' # Below is Table @@myLabel.
#' #
#' # <<>>=
#' # object <- data.frame("Foo" = 1:2, "Bar" = letters[1:2])
#' # knitr::kable(object, caption = md_tcap("myLabel", "My Caption))
#' # @@

md_cap <- function(caption, label, type = NULL, at.ref = FALSE){
   if(is.null(type) | !type %in% c("table", "figure")){
      warning("[proh::md_cap] 'type' should be 'table' or 'figure'.
              Will be set to 'table'.")
      type <- "table"
   }
   if(is.null(label)) label <- knitr::opts_current$get("label")
   md_loft_index(label, caption, type)
   ref <- if(at.ref){
      paste0("@", label)
   } else {
      md_get_ref(label, type)
   }
   paste0(chartr("tf", "TF", type), " ", ref, ": ", caption)
}

# ' @title
# ' @description

md_loft_index <- function(label, caption, type){
   LOFT <- opts_proh$get("loft")
   N <- length(LOFT[[type]])
   if(is.null(LOFT[[type]][[label]])){
       LOFT[[type]][[label]] <- c(label, caption, N+1)
   }
   opts_proh$set(loft = LOFT)
   invisible(NULL)
}

# ' @title
# ' @description

md_reset_index <- function(){
   opts_proh$set(loft = list(figure = list(), table=list()))
}

#' @title Get reference number
#' @description Get the refenrence number associated with a label
#' @param label a label
#' @param type look in "table" or "figure" (if \code{NULL} look everywhere)
#' @export

md_get_ref <- function(label, type = NULL){
   LOFT <- opts_proh$get("loft")
   if(is.null(type)){
      dummy <- NULL
      for(K in names(LOFT)) dummy <- c(dummy, LOFT[[K]][[label]][3])
   } else {
      dummy <- LOFT[[type]][[label]][3]
   }
   if(is.null(dummy)) "(_reference_ _unset_)" else dummy
}

#' @describeIn md_cap Captions for tables
#' @export
md_tcap <- function(caption = "", label = NULL, at.ref = FALSE) md_cap(label = label, caption = caption, type = "table", at.ref = FALSE)

#' @describeIn md_cap Captions for figures
#' @export
md_fcap <- function(caption = "", label = NULL, at.ref = FALSE) md_cap(label = label, caption = caption,  type = "figure", at.ref = FALSE)

#' @title Create enumeration of labels
#' @description Create List of Tables/Figures and thus convert all labels
#'   created with \code{md_tcap} and \code{md_fcap} to numbers. Will most
#'   likely be removed in the near future.
#' @export

md_enumerate_at_ref <- function(){
   results <- knitr::opts_current$get("results")
   warn <- "[proh::md_enumerate_labels] I want chunk option 'results' to be 'asis'."
   if(is.null(results)) warning(warn) else if(results != 'asis') warning(warn)
   LOFT <- opts_proh$get("loft")
   tables  <- LOFT$table
   figures <- LOFT$figure
   display <- function(x) paste0("(@", x[1], ") ", x[2], "\n")
   if(length(tables) > 0) {
      cat("**List of Tables**\n\n")
      cat("", unlist(lapply(tables, display)))
   }
   if(length(figures) > 0) {
      cat("\n**List of Figures**\n\n")
      cat("", unlist(lapply(figures, display)))
   }
}



