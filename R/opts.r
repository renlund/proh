# These function creates and handles overall project options
# and will hopefully be elaborated. The options are stored in
# an environment 'milieu'

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
      graph_dev = "pdf",
      attach_table = FALSE,
      table_fnc = utils::write.csv
   ), envir=milieu)
   assign(x="value", value = names(get(x="defaults", envir=milieu)), envir=milieu)
   invisible(NULL)
}

#' @title proh options
#' @description This list tries to mimic the behaviour of opts_chunk from knitr.
#' Currently these values are maintained with the functions in (the list)
#' \code{opts_proh}:
#' \itemize{
#' \item attach_graph - use \code{figh} in \code{fig.caption}
#' (in a \code{knitr} chunk), i.e. \code{fig.caption = figh("My Caption")},
#' this forces \code{figh} to include an attach-statement
#' pointing to the relevant figure in 'figure/' .
#' \item graph_dev - graphical extension
#' \item attach_table - this forces \code{tableh} to also attach its tables
#' \item table_fnc - this is the function to write the \code{tableh}-table
#' to file
#' }

#' @export

opts_proh <- list(
   "get" = proh_get,
   "set" = proh_set,
   "restore" = restore
)
