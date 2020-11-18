#' @title Save
#' @description Save an object to 'calc' folder
#' @author Henrik Renlund
#' @param name character; the name of one or more variable
#' @param autoload should the object be saved in subdirectory 'calc/autoload'?
#' @export

keep <- function(name, autoload=FALSE){
    location <- if(autoload) file.path('calc', 'autoload') else 'calc'
    if(!is.character(name)){
        stop("[proh::keep] 'name' should be the names (as a character vector) of variables.")
    }
    for(K in name){
        if(exists(K, envir=.GlobalEnv)){
            L <- list.files(path = "calc")
            L_ <- gsub("\\.rdata*$", "", L, ignore.case = TRUE)
            K_ <- paste0("^", K, "$")
            if(any(grepl(K_, L_, ignore.case=TRUE)) & !any(grepl(K_, L_))){
                if(Sys.info()['sysname'] == "Windows"){
                    stop(paste0("[proh::keep] Windows does not distinguish between upper-",
                                " and lower case in filenames and there is a similar file",
                                " kept in 'calc/'."))
                }
            }
            save(list=K, envir=.GlobalEnv, file=file.path(location, paste0(K, ".rdat")))
            add_meta_info(get(K, envir=.GlobalEnv), K)
        } else {
            warning(paste0("[proh::keep] '", K, "' does not exists."))
        }
    }
    invisible(NULL)
}

#' @describeIn keep Non-standard evaluation version
#' @param ... (possibly unquoted) names of objects
#' @export
keep_ <- function(..., autoload=FALSE){
    name <- as.character(eval(substitute(alist(...))))
    keep(name, autoload = autoload)
}


## - ##########################################################################
##               HELPER FUNCTIONS
## - ##########################################################################

ifnotnull <- function(x) if(!is.null(x)) x else NULL

meta_info <- function(x, name = NULL){
    content <- if("data.frame" %in% class(x)){
                   unlist(lapply(x, class))
               } else {
                   NULL
               }
    list(
        name = if(is.null(name)) as.character(substitute(x)) else name,
        class = class(x),
        names = ifnotnull(names(x)),
        dim = ifnotnull(dim(x)),
        content = ifnotnull(content),
        length = ifnotnull(length(x)),
        size = utils::object.size(x),
        when = Sys.time()
    )
}

meta_path <- file.path("calc", ".meta")

get_meta <- function(){
    if(file.exists(meta_path)){
        load(meta_path) ## loads meta
        if(!is.list(meta)){
            stop(paste0("calc/.meta is reserved for information on saves\n",
                 "should be a list of 'meta' information"))
        }
    } else {
        message("meta did not exist but will be initialized")
        if(!dir.exists("calc")) stop("...but there is no directory 'calc'")
        meta <- as.list(NULL)
        save(meta, file = meta_path)
    }
    meta
}

add_meta_info <- function(x, name = NULL){
    meta <- get_meta()
    xname <- if(!is.null(name)) name else as.character(substitute(x))
    ## if(xname %in% names(meta)){
    ##     message(paste0(xname, " is updated."))
    ## } else {
    ##     message(paste0(xname, " is added."))
    ## }
    meta[[xname]] <- meta_info(x, name = xname)
    save(meta, file = meta_path)
}

cut_string <- function(x, N, n = 3, s = "."){
    if(n>N) stop("computer says no.")
    xn <- nchar(x)
    if(xn>N){
        paste0(substr(x, 1, N-n), paste0(rep(s, n), collapse = ""))
    } else {
        x
    }
}

extract_meta_info <- function(x){
    w0 <- options("width")$width
    w <- if(w0<41) Inf else w0
    data.frame(
        object = cut_string(x$name, 15),
        saved = gsub("-", "", substr(x$when, 3, 16), fixed = TRUE),
        class = cut_string(paste0(x$class, collapse = ","), 14),
        names = cut_string(paste0(x$names, collapse = ","), w - (15 + 3 + 13 + 3 + 14 + 3))
    )
}

whatsaved <- function(){
    L <- list.files(path = "calc", pattern = "\\.rdata*$")
    x <- gsub("\\.rdata*$", "", L, ignore.case = TRUE)
    return(x)
}

# - #' @param saved_only logic
# - #' @import knitr
get_meta_info <- function(saved_only = TRUE){
    meta <- get_meta()
    R <- as.data.frame(NULL)
    namn <- names(meta)
    if(saved_only){
        saves <- whatsaved()
        namn <- namn[namn %in% saves]
    }
    for(N in namn){
        R <- rbind(R, extract_meta_info(meta[[N]]))
    }
    if(!is.null(R)){
        cat(paste0("\n## This project has documented information on:",
                  " #################################"))
        print(knitr::kable(R, format = "pandoc"))
    } else {
        cat("")
    }
    invisible(NULL)
}

saved_info <- function(exclude_meta = TRUE){
    L <- list.files(path = "calc", pattern = "\\.rdata*$", full.names = TRUE)
    saves <- whatsaved()
    if(exclude_meta){
        meta <- get_meta()
        filter <- !saves %in% names(meta)
        L     <- L[filter]
        saves <- saves[filter]
    }
    R <- NULL
    for(i in seq_along(L)){
        fi <- file.info(L[i])
        df <- data.frame(
            object = cut_string(saves[i], 80 - (14+3)),
            saved = gsub("-", "", substr(fi$mtime, 3, 16), fixed = TRUE)
            ## size = fi$size
        )
        R <- rbind(R, df)
    }
    if(!is.null(R)){
        cat(paste0("\n## This project has",
                   if(exclude_meta) " (undocumented)" else "",
                   " saves: ######################################"))
        print(knitr::kable(R, format = "pandoc"))
    } else {
        cat("")
    }
    invisible(NULL)
}

if(FALSE){
    cut_string("foo bar baz gerigegrein", 15)
    x <- 1:3
    (y <- meta_info(x))
    extract_meta_info(y)
    aLongVariableName <- data.frame(x = 1:1e4, y = sample(letters, 1e4, T),
                    z = runif(1e6), fantasticallyLongVariablenameThatReallyNeverSeemsToEnd = rnorm(1e6))
    class(aLongVariableName) <- c("data.frame", "foo")
    (y <- meta_info(aLongVariableName))
    extract_meta_info(y)


    ## keep(c("aLongVariableName", "x"))
    keep_(x)
    keep_(aLongVariableName)
    save(y, file = "calc/y.rdat")

    get_meta_info()
    whatsaved()
    saved_info()

    fetch()

    AL <- "g"
    foo <- list(a=7, b=1:1e4)
    keep_(AL, foo)
    fetch()

    qwerty <- TRUE
    bar_ <- list(c=7, ARH=1:1e4)
    keep(c("qwerty", "bar_"))

    fetch()
}
