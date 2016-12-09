##' mark file for delivery
##'
##' date and (possibly) version stamp a rapport and place in 'sent' folder
##' @param output_file the rapport pdf
##' @param version version number
##' @param source_file the rapport rnw
##' @param git commit? (default \code{TRUE})
##' @export
send_function <- function(output_file, version, source_file, git){
    fn <- file_name(output_file)
    ext <- fn$extension
    name <- fn$name
    today <- gsub('-', '', Sys.Date())
    nameD <- paste0(name, version, '--', today)
    ## this part makes sure we do not overwrite files
    FnameDE <- function(nameD) paste0(nameD, ext)
    nameDE <- FnameDE(nameD)
    while(file.exists(file.path('sent', nameDE))){
        if(readline(paste0("A pdf with name '", nameDE,"'",
                          " already exists in 'sent'.\n",
                          " Overwrite it? (If so 'y') ")) == 'y') break
        suffix <- readline(paste0("\nOk - I will not overwrite it.",
                                  " Please provide a suffix. "))
        nameD <- paste0(nameD, suffix)
        nameDE <- FnameDE(nameD)
    }
    ## now commit files with default or given commit message
    if(git){
        if(is.null(source_file)) stop("proh options does not specify a source file")
        default_message <- paste("Source for:", nameDE)
        invite_response <- paste0("The current commit message is '",
                                  default_message, "'.\n",
                                  "Want to change it? ('y' for yes, anything for 'no')")
        response <- readline(invite_response)
        if(response == 'y'){
            invite_response <- "Provide new commit message"
            commit_message <- readline(invite_response)
            if(commit_message == ""){
                warning(paste("We do not like empty commit messages.",
                              "Default message will be used"))
                commit_message <- default_message
            }
        } else {
            commit_message <- default_message
        }
        system(paste("git add", source_file))
        system(paste0("git commit -m \"", commit_message, "\""))
    }
    file.copy(from=output_file, to=file.path('sent', nameDE))
    if(!git) cat(paste0(" *-----------------------------------------*",
                        "  This is a good time to commit your files!",
                        " *-----------------------------------------*\n"))
    invisible(NULL)
}

##' @describeIn send_function use proh options 'output_file', 'version', and
##'     'source_file' as defaults
##' @export
send <- function(git = TRUE){
    opts_proh$check()
    output_file <- as.character(opts_proh$get("output_file"))
    if(is.null(output_file)) stop("proh options does not specify an output file")
    version <- if(is.null(v <- proh_get("version"))){
                  ""
              } else {
                  paste0("--", gsub(" ", "-", v, fixed = TRUE))
              }
    source_file <- opts_proh$get("source_file")
    send_function(output_file = output_file, version = version,
                source_file = source_file, git = git)
}

##' @describeIn send_function use proh options 'dm_output_file', 'dm_version', and
##'     'dm_source_file' as defaults
##' @export
send_dm <- function(git = TRUE){
    opts_proh$check()
    output_file <- as.character(opts_proh$get("dm_output_file"))
    if(is.null(output_file)) stop("proh options does not specify an dm output file")
    version <- if(is.null(v <- proh_get("dm_version"))){
                  ""
              } else {
                  paste0("--", gsub(" ", "-", v, fixed = TRUE))
              }
    source_file <- opts_proh$get("dm_source_file")
    send_function(output_file = output_file, version = version,
                source_file = source_file, git = git)
}


# - #' @title Mark a rapport with current date
# - #' @description Mark a rapport with todays date and put it in the 'sent' folder
# - #' @param name name of output rapport (will include todays date), defaults
# - #' to 'report', but if TRUE the name of the project will be used
# - #' @param minimal should ONLY the rapport.pdf be sent? (default: TRUE). If not
# - #' the rapport will be 'zipped' into a 'delivery' along with graphs and tables
# - #' @param graph.filter filter for which graphs to include in 'delivery'
# - #' @param table.filter filter for which tables to include in 'delivery'
# - #' @param git will not ask if a little git:ing is to be done, but this can also be
# - #' set to 'ask'
## send <- function(name=NULL, minimal = TRUE,
##                  graph.filter=NULL, table.filter=NULL, git=FALSE){
##    if(is.null(name)){
##       opts_proh$check()
##       name <- as.character(opts_proh$get("output_file"))
##    }
##    version <- if(is.null(v <- proh_get("version"))){
##                   ""
##               } else {
##                   paste0("--", gsub(" ", "-", v, fixed = TRUE))
##               }
##    name_org <- name
##    ext <- NULL
##    if(grepl("\\.", name)){
##       tmp <- strsplit(name, "\\.")[[1]]
##       m <- length(tmp)
##       ext <- paste0(".", tmp[m])
##       name <- paste(tmp[-m], collapse = ".")
##    }
##    today <- gsub('-', '', Sys.Date())
##    nameD <- paste0(name, version, '--', today)
##    FnameDE <- function(nameD) paste0(nameD, ext)
##    nameDE <- FnameDE(nameD)
##    #dummy <- 0
##    while(file.exists(file.path('sent', nameDE))){
##       #dummy <- 1
##       if(readline(paste0("A pdf with name '", nameDE, "' already exists in 'sent'.\n Overwrite it? (If so 'y') ")) == 'y') break
##       suffix <- readline("\nOk - I will not overwrite it. Please provide a suffix. ")
##       nameD <- paste0(nameD, suffix)
##       nameDE <- FnameDE(nameD)
##    }
##    if(git=='ask'){
##       git_branch <- readline("\n Do you want to commit 'rapport.nrw' and create a git branch for this Send?\n (If so 'y'.) ")
##       if(git_branch=='y')
##          git <- TRUE
##       else
##          git <- FALSE
##    }
##    if(git){
##       commit_message <- readline("Please provide commit message\n (Leave blank for 'commit_[todays-date-and-time]'.) ")
##       if(commit_message=="") {
##          if(!exists("suffix", inherits=FALSE)) suffix <- ""
##          #commit_message <- paste0("commit_", today,  suffix)
##          commit_message <- paste0("commit ", Sys.time())
##       }
##       system(paste("git add rapport.rnw"))
##       system(paste0("git commit -m \"", commit_message, "\""))
##       branch_name <- readline("Please provide (valid - no spaces) branch name\n (Leave blank for 'branch_[todays-date_(and time)]'.)  ")
##       if(branch_name=="") {
##          if(!exists("suffix", inherits=FALSE)) suffix <- ""
##          x <- Sys.time()
##          todaytime <- paste0(gsub(':', '', gsub(' ', '_(', x)),')')
##          branch_name <- paste0("branch_", todaytime)
##       }
##       gitBranch(branch_name)
##    }

##    if(minimal){
##       file.copy(from=name_org, to=file.path('sent', nameDE) )
##    } else {
##       graphs <-
##          setdiff(
##             list.files(path='figure', pattern=graph.filter, recursive=FALSE),
##             sub('graf//', '', list.dirs('graf', recursive=FALSE))
##             )
##       outs <-
##          setdiff(
##             list.files('table', pattern=table.filter, recursive=FALSE),
##             sub('..', '', list.dirs('tabell', recursive=FALSE))
##             )
##       sentZip <- file.path('sent', nameD,'.zip')
##       grafZip <- file.path('figure',graphs)
##       outZip  <- file.path('table', outs)
##       rappZip <- file.path(nameDE)
##       file.copy(from='rapport.pdf', to=rappZip)
##       utils::zip(
##          zipfile=sentZip,
##          files=c(grafZip, outZip, rappZip)
##       )
##       file.remove(rappZip)
##     }
##     if(!git) cat(' *------------------------------------*
##  This is a good time to Git your files!
##  *------------------------------------*\n')
##    invisible(NULL)
## }
