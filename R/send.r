#' @title Mark a rapport with current date
#' @description Mark a rapport with todays date and put it in the 'sent' folder
#' @author Henrik Renlund
#' @param name name of output rapport (will include todays date), defaults 
#' to 'report', but if TRUE the name of the project will be used
#' @param minimal should ONLY the rapport.pdf be sent? (default: TRUE). If not
#' the rapport will be 'zipped' into a 'delivery' along with graphs and tables
#' @param graph.filter filter for which graphs to include in 'delivery'
#' @param table.filter filter for which graphs to include in 'delivery'
#' @param git will ask if a little git:ing is to be done, but this can also be
#' to TRUE/FALSE
#' @export

send <- function(name='rapport', minimal = TRUE, 
               graph.filter=NULL, table.filter=NULL, git='ask'){
   if(!is.character(name)){
      if(is.logical){
         if(TRUE) name <- rev(strsplit(getwd(),.Platform$file.sep)[[1]])[1]
      } else {
         name <- 'rapport'
      }
   }
   today <- gsub('-','',Sys.Date())
   nameD <- paste0(name, '_', today)
   FnameDE <- function(nameD) paste0(nameD, '.pdf')
   nameDE <- FnameDE(nameD)
   #dummy <- 0
   while(file.exists(file.path('sent', nameDE))){
      #dummy <- 1
      if(readline(paste0("A pdf with name '", nameDE, "' already exists in 'sent'.\n Overwrite it? (If so 'y') ")) == 'y') break
      suffix <- readline("\nOk - I will not overwrite it. Please provide a suffix. ")
      nameD <- paste0(nameD, suffix)
      nameDE <- FnameDE(nameD)
   }
   if(git=='ask'){
      git_branch <- readline("\n Do you want to commit 'rapport.nrw' and create a git branch for this Send?\n (If so 'y'.) ")
      if(git_branch=='y') 
         git <- TRUE
      else
         git <- FALSE
   }
   if(git){
      commit_message <- readline("Please provide commit message\n (Leave blank for 'commit_[todays-date-and-time]'.) ")
      if(commit_message=="") {
         if(!exists("suffix", inherits=FALSE)) suffix <- ""
         #commit_message <- paste0("commit_", today,  suffix)
         commit_message <- paste0("commit ", Sys.time())
      }
      system(paste("git add rapport.rnw"))
      system(paste0("git commit -m \"", commit_message, "\""))
      branch_name <- readline("Please provide (valid - no spaces) branch name\n (Leave blank for 'branch_[todays-date_(and time)]'.)  ")
      if(branch_name=="") {
         if(!exists("suffix", inherits=FALSE)) suffix <- ""
         x <- Sys.time()
         todaytime <- paste0(gsub(':', '', gsub(' ', '_(', x)),')')
         branch_name <- paste0("branch_", todaytime)
      }
      gitBranch(branch_name)
   }
   
   if(minimal){
      file.copy( from='rapport.pdf', 
         to=file.path('sent', nameDE) )
   } else {   
      graphs <- 
         setdiff(
            list.files(path='figure', pattern=graph.filter, recursive=FALSE),
            sub('graf//', '', list.dirs('graf', recursive=FALSE))
            )
      outs <- 
         setdiff(
            list.files('table', pattern=table.filter, recursive=FALSE),
            sub('..', '', list.dirs('tabell', recursive=FALSE))
            )
      sentZip <- file.path('sent', nameD,'.zip')
      grafZip <- file.path('figure',graphs)
      outZip  <- file.path('table', outs)
      rappZip <- file.path(nameDE)
      file.copy(from='rapport.pdf', to=rappZip)
      zip(
         zipfile=sentZip,
         files=c(grafZip, outZip, rappZip)
      )
      file.remove(rappZip)
    }
    if(!git) cat(' *------------------------------------*
 This is a good time to Git your files! 
 *------------------------------------*')
   invisible(NULL)
}
