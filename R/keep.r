#' @title Save
#' @description Save an object to 'calc' folder
#' @author Henrik Renlund
#' @param name character; the name of one or more variable
#' @param autoload should the object be saved in subdirectory 'calc/autoload'?
#' @export

keep <- function(name, autoload=FALSE){
  location <- if(autoload) file.path('calc', 'autoload') else 'calc'
  if(!is.character(name))
      stop("[proh::keep] 'name' should be the names (as a character vector) of variables.")
  .proh <- file.path("calc", ".proh")
  P <- file.exists(.proh)
  if(P){
     if(length(readLines(con = .proh, n = 1)) != 0) {
        info <- read.csv(.proh)
     } else {
        info <- NULL
     }
  }
   for(K in name){
      if(exists(K, envir=.GlobalEnv)){
          L <- list.files(path = "calc")
          L_ <- gsub("\\.rdata*$", "", L, ignore.case = TRUE)
          K_ <- paste0("^", K, "$")
         if(any(grepl(K_, L_, ignore.case=TRUE)) & !any(grepl(K_, L_))){
            if(Sys.info()['sysname'] == "Windows") stop("[proh::keep] Windows does not distinguish between upper- and lower case in filenames and there is a similar file kept in 'calc/'.")
         }
         save(list=K, envir=.GlobalEnv, file=file.path(location, paste0(K, ".rdat")))
         #save(tmp_var, file=file.path(location, paste0(K, ".rdat")))
         tmp_var <- get(K, envir=.GlobalEnv)
         if(P){
            classy = class(tmp_var)
            if(K %in% info$object){
               info <- subset(info, object != K)
            }
            df_names = if("data.frame" %in% classy){
               paste0(names(tmp_var), collapse = ",")
            } else {
               " "
            }
            df_variables <- if(nchar(df_names) > 35) {
               paste0(substring(df_names, 1, 32), "...")
            } else {
               df_names
            }
            tmp_info <- data.frame(object = K,
                                   saved = as.character(Sys.time()),
                                   class = classy[1],
                                   variables = df_variables,
                                   stringsAsFactors = FALSE)
         }
      } else {
         warning(paste0("[proh::keep] '", K, "' does not exists."))
      }
      if(P) info <- rbind(info, tmp_info)
   }
  if(P) write.csv(info, file = .proh, row.names = FALSE)
}

#' @describeIn keep Non-standard evaluation version
#' @param ... (possibly unquoted) names of objects
#' @export

keep_ <- function(..., autoload=FALSE){
   name <- as.character(eval(substitute(alist(...))))
   keep(name, autoload = autoload)
}
