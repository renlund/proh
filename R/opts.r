# These function creates and handles overall project options
# and will hopefully be elaborated. The options are stored in
# an environment 'milieu'

# @title milieu
# @description an environment

milieu <- new.env(parent = getNamespace("proh"))

# @title proh_get
# @description this function retrieves the proh settings
# @param name name of proh setting variable
proh_get <- function(name){
   if(length(ls(envir=milieu))==0) proh_restore(check = FALSE)
   defaults <- get("defaults", envir=milieu)
   if (missing(name))
      defaults
   else {
      L <- as.list(NULL)
      for(k in name){
         L[[k]] <- defaults[[k]]
      }
      if(length(L) == 1) L[[1]] else if(length(L) == 0) NULL else L
   }
}

# @title proh_set
# @description this function sets the proh settings
# @param ... the names and values you want set, e.g. \code{"add_graph"=TRUE}
proh_set <- function(..., check = TRUE){
   if(length(ls(envir=milieu))==0) proh_restore()
   dots <- list(...)
   value <- get("value", milieu)
   for(k in names(dots)) if(!(k %in% value)) dots[[k]] <- NULL
   current <- proh_get()
   for(k in names(dots)) current[[k]] <- dots[[k]]
   assign(x="defaults", value=current, envir=milieu)
   if(check) proh_check()
   invisible(NULL)
}

# @title proh_restore
# @description this function restores the default proh settings
proh_restore <- function(check = FALSE){
   assign(x="defaults", value=list(
      main_document = "rapport.rnw",
      ## output_format = "pdf_document",
      output_file = NULL,
      version = NULL,
      version_latex = NULL
   ), envir=milieu)
   assign(x="value", value = names(get(x="defaults", envir=milieu)), envir=milieu)
   if(check) proh_check()
   invisible(NULL)
}

# @title proh_check
# @description some checks of the proh options
proh_check <- function(){
    main_doc <- proh_get("main_document")
    if(!file.exists(main_doc)){
        foo <- paste0(rep("~", options("width")$width), collapse = "")
        warning(paste0("\n", foo,
                       "\nSource document set to ", main_doc,
                       " which I can't find in the current directory.\n",
                       "Perhaps use an .Rprofile with:\n",
                       "opts_proh$set('main_document' = <correct-file>)\n",
                       foo))
    }
    out_file <- proh_get("output_file")
    name <- file_name(main_doc)$name
    ext <- file_name(main_doc)$extension
    if(!ext %in% c(".rnw", ".Rnw")){
        stop("source document should be an rnw file")
    }
    if(is.null(out_file)) {
        proh_set("output_file" = paste0(name, ".pdf"), check = FALSE)
    }
    version <- proh_get("version")
    version_latex <- proh_get("latex_version")
    if(!is.null(version_latex)) if(version_latex == "") version_latex <- NULL
    if(is.null(version_latex)){
        vl <- if(is.null(version)) "" else  paste0("\\\\ ", version)
        proh_set("version_latex" = vl, check = FALSE)
    }
}

#' @title proh options
#' @description This list tries to mimic the behaviour of opts_chunk from knitr.
#' Currently these values are maintained with the functions in (the list)
#' \code{opts_proh}:
#' \itemize{
#' \item main_document - default: rapport.rnw
#' \item output_format - default: pdf_document
#' \item output_file - will be like main_document but appropriate file extension (
#' unless set manually)
#' \item version a version number as character, e.g. "Version 1". This will
#'     appear on the LaTeX version of the rapport
#' \item version_latex this is the string that will determine how the version
#'     number appears in the title (in LaTeX produced pdf:s) and will be set
#'     automatically
#' }
#' @export
opts_proh <- list(
   "get" = proh_get,
   "set" = proh_set,
   "restore" = proh_restore,
   "check" = proh_check
)
