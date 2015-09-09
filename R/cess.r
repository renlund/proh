#' @title Run chunks
#' @description Run a chunk in a given document
#' @param chunk name or number of chunk, if \code{NULL} (default) then the
#' first chunk will be executed
#' @param file the file which has the chunks, default: 'rapport.rnw'
#' @param envir is \code{.GlobalEnv} by default, the environemnt in which to
#' evaluate chunks
#' @note This will replace \code{proh::first}
#' @export

cess <- function(chunk = NULL, file = NULL, envir = .GlobalEnv){
   if(is.null(file)) file = opts_proh$get("main_document")
   cinfo <- chunks_info(file, all = TRUE)
   if(is.null(chunk)) chunk <- 1
   if(is.numeric(chunk)){
      N  <- nrow(cinfo)
      if((mer <- max(chunk)) > N) stop(paste0("[cess] there are ", N, " chunks and you've supplied number up to ", mer, "."))
   }
   if(is.character(chunk)){
      if(!all(chunk %in% cinfo$name)){
         cat("Available chunks in ", file, " are:\n", paste0(paste0(" * ", cinfo$name), collapse="\n"), sep = "")
         no <- which(!chunk %in% cinfo$name)
         cat("\n\nThe following chunks do not exists:\n", paste(paste(" * ", chunk[no]), collapse="\n"), sep = "")
         stop("[cess] specified chunks does not completely match")
      }
      copy <- chunk
      for(k in seq_along(chunk)){
         copy[k] <- which(cinfo$name %in% chunk[k])
      }
      chunk  <- as.numeric(copy)
   }
   for(indx in chunk){ # indx = chunk[1]
      cat("Evaluating chunk ", indx, " ('", cinfo$name[indx], "'):\n", sep = "")
      eval(expr = parse(text = cinfo$code[indx]), envir = envir)
      cat("\n")
   }
   invisible(NULL)
}

if(FALSE){
   file <- "C:/R/P_package/proh/ignorera_detta/chunk_tester.rnw"
   cess(file=file)
   chunk = c("auto", "B", "2")
   cess(chunk, file)
   chunk = c("auto", "B", "2", "ERROR", "do not compute")
   cess(chunk, file)
}
