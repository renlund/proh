#' @title Create git branch
#' @description Create git branch. This function is mainly a helper for \code{send}.
#' @author Henrik Renlund
#' @param name Name of branch
#' @export

gitBranch <- function(name){
   branch_name <- gsub(" ", "_", name)
   system(paste("git branch", branch_name))
   system(paste("git checkout", branch_name))
   fil <- paste0("BRANCH_", branch_name, ".txt")
   branchTextFile(branch_name, fil)
   system(paste0("git add \"",fil,"\""))
   system(paste0("git commit -m \"initializing branch ",gsub("-","",Sys.Date()),"\""))
   system("git checkout master")
   system("git checkout master")
}

# @title Create a information text file for a git branch
# @param name the name the project
# @param file_name the file name to be used

branchTextFile <- function(name, file_name){
   text <- paste0("If you can see this file you are in branch '", name,"'\n of project '",rev(strsplit(getwd(),.Platform$file.sep)[[1]])[1],"'.\n",paste(rep("-",60),collapse=""),"\nThis file was created with ",R.version.string," and \npackage proh ",packageVersion('proh')," on ",Sys.Date() )
   cat(text, file=file_name)
}