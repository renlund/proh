#' @title Create new PROH structured project
#' @description This function sets up a project directory structure along with some files
#' @author Henrik Renlund
#' @details This function sets up a folder with subfolders
#' \itemize{
#' \item{cache: this is only used by knitr}
#' \item{calc: this is for storage of .rdat files}
#' \item{calc/autoload: this is for storage of inline values. Typically this folder will automatically be loaded in a first (uncached) chunk of the rapport file by \code{fetchAll}.}
#' \item{figure: for plots (also used by knitr)}
#' \item{recieved: typically this is were I put files given by clients}
#' \item{sent: this is were I store things sent to client. The function \code{Send} will attach the current date to the pdf version of the rapport and put it in this directory. Optionally, \code{send} can zip the rapport along with the graphs and tables from their respective directory}
#' \item{table: for (human readable) tabulated data}
#' }
#' and creates the files
#' \itemize{
#' \item{'rapport.rnw': the rapport file, can be changed to suit your needs, but is designed to have a first uncached chunk (by default called 'autoLoad') that executes \code{fetchAll()}}
#' \item{'_META_.r': this is a file for the compilation of the rapport, which is useful if you want to change e.g. global chunk options - if, not, usually \code{comp()} will suffice}
#' \item{'references.bib': a template for bibTeX references}
#' \item{a .rsproj file with the project name: this is an RStudio project file, by starting this file RStudio will set the working directory and remember what documents you were looking at. There are settings to be made that can be project specific}
#' \item{.gitignore: a file that git used to tell which files to ignore}
#' }
#'  @param name Name of the project
#'  @param path Path to project directory (else current)
#'  @param class Class of document in 'rapport.rnw' (default: 'ucr')
#'  @param go_there Set working directory to project directory? (default: TRUE)
#'  @param RSproj Start a RStudio project? (deault: TRUE)
#'  @param git should git be initialized? (also a .gitignore file will be created)
#'  @export

newProject <- function(name="new_project", path=NULL, class="ucr", go_there=TRUE, RSproj=TRUE, git=TRUE){
   wd <- getwd()
   if(is.null(path)) {
      cat(paste0("The new '",name,"' project directory structure will be created\n in the current working directory:\n   ", wd, "\n Press 'x' to abort.\n Press anything else to proceed.")) 
      if( readline()=="x" ) {
         setwd(wd)
         return(NULL)
      }
      path <- wd
   }
   full.path <- file.path(path,name)
   dummy <- 0
   ENV <- environment()
   tryCatch(
      setwd(full.path), 
      error = function(e) assign("dummy", value=1, envir=ENV)
   )
   if( dummy == 0 ) {
      setwd(wd)
      stop( paste("Directory '", full.path,"' already exists.", sep="") )
   }
   rm(dummy)
   dir.create(full.path)
   setwd(full.path)
   SET <- c("table", "received", "sent", "calc", "figure", "cache")
   for(S in SET) dir.create(S)
   setwd(file.path(full.path, "calc"))
   dir.create("autoload")
   setwd(full.path)
   
   cat(create_meta(full.path), file="_META_.r")
   cat(create_rapport(name, class), file="rapport.rnw")
   cat(create_bib(), file="references.bib")
   if(RSproj) cat(create_proj(), file=paste0(name,".rproj"))
   
   end.text <- paste0( 
      paste(rep("-", 65),collapse=""),
      "\nCreated new PROH project directory:\n ", full.path, "\n",
      paste(rep("-", 65),collapse=""), "\n"
   )
   cat(end.text)
   if(git) {
      cat(create_git_ignore(), file=".gitignore")
      system("git init")
      cat(paste(rep("-", 65),collapse=""), "\n")
      yr_name <- readline("Provide name for git\n (e.g. Anaximandros Janson)     ")
      system(paste0("git config user.name \"",yr_name,"\""))
      yr_mail <- readline("Provide email for git\n (e.g. Anaximandros.Janson@foo.bar)     ")
      system(paste0("git config user.email ",yr_mail))
      system("git add rapport.rnw references.bib")
      system(paste0("git commit -m \"proh initialized project ",gsub("-","",Sys.Date()),"\""))
      cat(paste(rep("-", 65),collapse=""), " Done! \n")
   }
   if(go_there) setwd(full.path) else setwd(wd)
}

# META TEXT ---------------------

create_meta <- function(full.path){
   paste0(
"# setwd('",full.path ,"') # should not need this

# KNITR OPTIONS: ---------------------
require(knitr)
opts_chunk$set(
   cache=TRUE,   
   include=FALSE,
   echo=FALSE,
   fig.pos='hbt'
#   message=TRUE, 
#   error=TRUE, 
#   warning=TRUE,
)
# PACKAGE OPTIONS: --------------------
opts_knit$set(
   #    aliases=c( # rename often used chunk options
   #       h='fig.heigth', 
   #       w='fig.width'
   #       ),
   eval.after=c( # list of chunk options to be evaluated after the chunk
      'fig.cap'
      )#,
#    header = '\\newcommand{\\rlang}{\\textbf{R}}' # things that go into the preamble
#      width=75 #63
   )
# CREATE PDF : ------------------------
comp() # knit2pdf(input='rapport.rnw', clean=TRUE)

# SEND RAPPORT : ----------------------
send()
")
}

# RAPPORT TEXT ------------------
create_rapport <- function(name, class){
   
   paste0(
"%%%%%%  This file was created with ",R.version.string," and
%%%%%%  package proh ",packageVersion('proh')," on ",Sys.Date(),"
\\documentclass{",class,"}
%\\usepackage{lscape}
%\\usepackage[swedish, english]{babel} % swedish % ?
%\\usepackage[latin1]{inputenc}
%\\usepackage{subfig}
%\\newcommand{\\path}{\\texttt}
%\\newcommand{\\code}{\\texttt}
\\title{",gsub("_","\\_", name, fixed=TRUE),"}   % <-------------------- MAYBE CHANGE THIS?
\\author{",Sys.info()['login'],"}  % <--------- MAYBE CHANGE THIS?
% \\addtolength{\\hoffset}{-1.5cm} 
% \\addtolength{\\textwidth}{3cm}
% \\addtolength{\\voffset}{-1.5cm} 
% \\addtolength{\\textheight}{3cm}
% \\usepackage{attachfile}

\\begin{document}

<<autoLoad, cache=FALSE, include=FALSE>>=   
library(ucR)
library(proh)
fetchAll(calc=FALSE, autoload=TRUE)
@

\\section{Meta Information}
<<purl, cache=FALSE, include=FALSE>>=
purl('rapport.rnw', documentation=0)
@
This rapport was generated by knitr \\cite{knitr} and the R code
can be found here: \\attachfile{rapport.R} 

Information about the statistical software R: 
<<meta_information, cache=FALSE, echo=FALSE, results='asis'>>=
toLatex(sessionInfo())
@

\\bibliography{references}
\\bibliographystyle{plain}

\\end{document}
")
}


# BIB TEXT ----------------------
create_bib <- function(){
   paste0(
"@book{knitr,
 author               = {Xie, Y.},
 journal              = {},
 publisher            = {CRC Press},
 title                = {Dynamic Documents with R and Knitr},
 year                 = {2013}
 }

@comment{ ******** BELOW ARE TEMPLATES FOR ARTICLES, BOOKS AND TECHNICAL REPORTS ********

@article{RR83,
 author               = {Rosenbaum, P. R. and Rubin, D. B.},
 journal              = {Biometrika},
 pages                = {41--55},
 title                = {The central role of the propensity score in observational studies},
 volume               = {70},
 year                 = {1983},
 }

@book{,
 author               = {},
 journal              = {},
 publisher            = {},
 title                = {},
 year                 = {},
 }

@techreport{,
 author               = {},
 type                 = {},
 institution          = {},
 pages                = {},
 title                = {},
 number               = {},
 year                 = {},
 }
}
")
}


# IGNORE TEXT -------------------
create_git_ignore <- function(){
   paste0(
"_META_.r
.Rproj.user
*.Rhistory
*.rproj
*.RData
*.tex
*.toc
*.concordance
*.log
*.brf
*.bbl
*.blg
*.lof
*.lot
*.out
*.aux
.gitignore
*~
cache/*
figure/*
table/*
sent/*
")
}

# PROJ TEXT ---------------------
create_proj <- function(){
paste0(
"Version: 1.0

RestoreWorkspace: No
SaveWorkspace: No
AlwaysSaveHistory: Yes

EnableCodeIndexing: Yes
UseSpacesForTab: Yes
NumSpacesForTab: 3
Encoding: UTF-8

RnwWeave: knitr
LaTeX: pdfLaTeX
")
}
