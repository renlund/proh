# These function creates and handles overall project options
# and will hopefully be elaborated. The options are stored in
# an environment created by 'make_milieu' (this is probably
# the wrong way to do this...)

# @title milieu
# @description an environment

milieu <- new.env(hash=FALSE, parent=.GlobalEnv)

# @title proh_get
# @description this function retrieves the proh settings
# @param name name of proh setting variable

proh_get <- function(name){
   if(length(ls(envir=milieu))==0) restore()
   defaults <- get("defaults", envir=milieu)
   if (missing(name))
      defaults
   else {
      L <- as.list(NULL)
      for(k in name){
         L[[k]] <- defaults[[k]]
      }
      L
   }
}

# @title proh_set
# @description this function sets the proh settings
# @param ... the names and values you want set, e.g. \code{"add_graph"=TRUE}

proh_set <- function(...){
   if(length(ls(envir=milieu))==0) restore()
   dots <- list(...)
   value <- get("value", milieu)
   for(k in names(dots)) if(!(k %in% value)) dots[[k]] <- NULL
   current <- proh_get()
   for(k in names(dots)) current[[k]] <- dots[[k]]
   assign(x="defaults", value=current, envir=milieu)
   invisible(NULL)
}

# @title restore
# @description this function restores the default proh settings

restore <- function(){
   assign(x="defaults", value=list(
      attach_graph = FALSE,
      attach_table = FALSE,
      graph_dev = "pdf"
      #sumatra_path = file.path("C:", "Program Files", "RStudio", "bin", "sumatra")
   ), envir=milieu)
   assign(x="value", value = names(get(x="defaults", envir=milieu)), envir=milieu)
   invisible(NULL)
}

#' @title opts (not useful yet)
#' @description This list tries to mimic the behaviour of opts_chunk from knitr.
#' Currently two values are maintained with the functions in (the list) opts_proh: \itemize{
#' \item attach_graph
#' \item attach_table
#' }
#' Currently, these values has no effect on anything - but hopefully they will
#' in the future...
#' @export

opts_proh <- list(
   "get" = proh_get,
   "set" = proh_set,
   "restore" = restore
)
