#' @title Capions in rmarkdown
#' @description This function allows enumerated tables and figures in rmarkdown
#'   with html or word output
#' @details The \code{md_cap} function adds a list element to the \code{loft}
#'   list in \code{opts_proh}. These are converted by \code{md_enumerate_labels}
#'   to two referrable, numbered list (List of Tables and List of Figures,
#'   respectively), which, when created, will have the effect of replacing the
#'   @@-label with a number. Use \code{md_tcap} for tables and \code{md_fcap}.
#' @param label the label you can refer to by @@label in the .rmd document. If
#'   \code{NULL} it will inherit the 'label' from the current chunk
#' @param caption the caption
#' @param type "table" or "figure". User do not have to care as you use
#'   \code{md_tcap} or \code{md_fcap}, which calls \code{md_cap} with
#'   this argument fixed
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

md_cap <- function(label, caption, type = NULL){
   if(is.null(type)){
      warning("[proh::md_cap] 'type' should be 'table' or 'figure'.
              Will be set to 'table'.")
      type <- "table"
   } else if(!type %in% c("table", "figure"))
   if(is.null(label)) label <- knitr::opts_current$get("label")
   LOFT <- opts_proh$get("loft")[[1]]
   LOFT[[type]][[label]] <- c(label, caption)
   opts_proh$set(loft = LOFT)
   paste0(chartr("tf", "TF", type), " @", label, ": ", caption)
}

#' @describeIn md_cap Captions for tables
#' @export
md_tcap <- function(label = NULL, caption = "") md_cap(label = label, caption = caption,  type = "table")

#' @describeIn md_cap Captions for figures
#' @export
md_fcap <- function(label = NULL, caption = "") md_cap(label = label, caption = caption,  type = "figure")

#' @title Create enumeration of labels
#' @description Create List of Tables/Figures and thus convert all labels
#'   created with \code{md_tcap} and \code{md_fcap} to numbers.
#' @export

md_enumerate_labels <- function(){
   results <- knitr::opts_current$get("results")
   warn <- "[proh::md_enumerate_labels] I want chunk option 'results' to be 'asis'."
   if(is.null(results)) warning(warn) else if(results != 'asis') warning(warn)
   LOFT <- opts_proh$get("loft")[[1]]
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

