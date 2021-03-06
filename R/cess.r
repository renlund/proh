#' @title Run chunks
#' @description Run a chunk in a given document
#' @param chunk name or number of chunk, if \code{NULL} (default) then the
#' first chunk will be executed
#' @param file the file which has the chunks, default 'source_file'
#' @param envir is \code{.GlobalEnv} by default, the environemnt in which to
#' evaluate chunks
#' @param profile source .Rprofile if it exists?
#' @param verbose if TRUE, may print some slightly unneccessary messages
#' @note This will replace \code{proh::first}
#' @export
cess <- function(chunk = NULL, file = NULL, envir = .GlobalEnv, profile = FALSE,
                 verbose = TRUE){
    if(profile){
        if(file.exists(".Rprofile")){
            source(".Rprofile")
        }
    }
   if(is.null(file)) file = opts_proh$get("source_file")
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
      if(verbose) cat("Evaluating chunk ", indx, " ('", cinfo$name[indx], "'):\n", sep = "")
      eval(expr = parse(text = cinfo$code[indx]), envir = envir)
      if(verbose) cat("\n")
   }
   invisible(NULL)
}

#' @describeIn cess this function has 'dm_source_file' as it's default file
#' @export
cess_dm <- function(chunk = NULL, file = NULL, envir = .GlobalEnv,
                    profile = FALSE, verbose = TRUE){
    if(profile){
        if(file.exists(".Rprofile")){
            source(".Rprofile")
        }
    }
    if(is.null(file)) file = opts_proh$get("dm_source_file")
    cess(chunk = chunk, file = file, envir = envir,
         profile = profile, verbose = verbose)
}

if(FALSE){
   file <- "C:/R/P_package/proh/ignorera_detta/chunk_tester.rnw"
   cess(file=file)
   chunk = c("auto", "B", "2")
   cess(chunk, file)
   chunk = c("auto", "B", "2", "ERROR", "do not compute")
   cess(chunk, file)
}
