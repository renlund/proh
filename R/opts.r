# - #' @title milieu
# - #' @description a function to create a function and an environment

make_milieu <- function() function() invisible(NULL)

# - #' @title milieu
# - #' @description a function that can serve as a reference to a 
# - #' environment

milieu <- make_milieu()

# - #' @title proh_get
# - #' @description this function retrieves the proh settings
# - #' @param name name of proh setting variable

proh_get <- function(name){
   defaults <- get("defaults", envir=environment(milieu))
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

# - #' @title proh_set
# - #' @description this function sets the proh settings
# - #' @param ... the names and values you want set, e.g. \code{"add_graph"=TRUE}

proh_set <- function(...){
   dots <- list(...)
   value <- get("value", environment(milieu))
   for(k in names(dots)) if(!(k %in% value)) dots[[k]] <- NULL
   current <- proh_get()
   for(k in names(dots)) current[[k]] <- dots[[k]]
   assign(x="defaults", value=current, envir=environment(milieu))
   invisible(NULL)
}

# - #' @title restore
# - #' @description this function restores the default proh settings

restore <- function(){
   assign(x="defaults", value=list(
      attach_graph = FALSE,
      attach_table = FALSE,
      sumatra_path = file.path("C:", "Program Files", "RStudio", "bin", "sumatra")
   ), envir=environment(milieu)) 
   assign(x="value", value = names(get(x="defaults", envir=environment(milieu))), envir=environment(milieu)) 
   invisible(NULL)
}

#' @title opts
#' @description This list tries to mimic the behaviour of opts_chunk from knitr. 
#' Currently two values are maintained with opts_proh: \itemize{
#' \item attach_graph
#' \item attach_table
#' }
#' @export

opts_proh <- list(
   "get" = proh_get,
   "set" = proh_set,
   "restore" = restore
)
