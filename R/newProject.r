#' @title Create new project
#' @description This function sets up a project directory structure along with
#'   some files
#' @author Henrik Renlund
#' @details This function sets up a folder with subfolders \itemize{
#'   \item{cache: this is only used by knitr} \item{calc: this is for storage of
#'   .rdat files} \item{calc/autoload: this is for storage of inline values.
#'   Typically this folder will automatically be loaded in a first (uncached)
#'   chunk of the rapport file by \code{fetchAll}.} \item{figure: for plots
#'   (also used by knitr)} \item{recieved: typically this is were I put files
#'   given by clients} \item{sent: this is were I store things sent to client.
#'   The function \code{send} will attach the current date to the pdf version of
#'   the rapport and put it in this directory. Optionally, \code{send} can zip
#'   the rapport along with the graphs and tables from their respective
#'   directory} \item{table: for (human readable) tabulated data} } and creates,
#'   optionally, the files \itemize{ \item{'rapport.rnw': the rapport file, can
#'   be changed to suit your needs, but is designed to have a first uncached
#'   chunk (by default called 'autoLoad') that executes \code{fetchAll()}}
#'   \item{'_META_.r': this WAS a file for the global options which can be read
#'   by comp... nowadays I think it is better to include these options in a
#'   first uncached chunk} \item{'references.bib': a template for bibTeX
#'   references} \item{a .rsproj file with the project name: this is an RStudio
#'   project file, by starting this file RStudio will set the working directory
#'   and remember what documents you were looking at. There are settings to be
#'   made that can be project specific} \item{.gitignore: a file that git uses
#'   to tell which files to ignore} }
#' @param name Name of the project
#' @param path Path to project directory (else current)
#' @param meta should a '_META_' file be created? (default \code{FALSE})
#' @param class Class of document in 'rapport.rnw' (default: 'ucr')
#' @param go_there Set working directory to project directory? (default: TRUE)
#' @param RSproj Start a RStudio project? (deault: TRUE)
#' @param git should git be initialized? (also a .gitignore file will be
#'   created)
#' @param org should an org file be created?
#' @export

newProject <- function(name="new_project", path=NULL, meta = FALSE, class="ucr", go_there=TRUE, RSproj=TRUE, git=TRUE, org = TRUE){
   wd <- getwd()
   if(is.null(path)) {
      cat(paste0("The new '",name,"' project directory structure will be created\n in the current working directory:\n   ", wd, "\n Press 'x' to abort.\n Press anything else to proceed."))
      if( readline()=="x" ) {
         setwd(wd)
         return(NULL)
     }
      yr_name <- readline("Provide name for project/git\n (e.g. Anaximandros Janson)     ")
      yr_mail <-
          readline("Provide email for project/git\n (e.g. Anaximandros.Janson@foo.bar)     ")
      if(yr_name == "") yr_name <-  Sys.info()['login']
      if(yr_mail == "") yr_mail <-  paste0(Sys.info()['login'], "@mail.com")
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
   file.create("calc/.proh")
   setwd(file.path(full.path, "calc"))
   dir.create("autoload")
   setwd(full.path)
   if(meta){
      message("[newProject] since proh version 0.2.0, the use of '_META_' file is adviced against.\n")
      if(readline(prompt = "Are you sure? ('y' for yes, anything else to skip)") == "y"){
         cat(create_meta(full.path), file="_META_.r")
     }
   }
   cat(create_rapport(name, yr_name, yr_mail, class), file="rapport.rnw")
   cat(create_bib(), file="references.bib")
   if(RSproj) cat(create_proj(), file=paste0(name,".rproj"))

   end.text <- paste0(
      paste(rep("-", 65),collapse=""),
      "\nCreated new PROH project directory:\n ", full.path, "\n",
      paste(rep("-", 65),collapse=""), "\n"
   )
   cat(end.text)
   if(org) cat(create_org(name, yr_name, yr_mail),
               file = paste0(namn, "-org.org"))
   if(git) create_git(yr_name, yr_mail)
   if(go_there) setwd(full.path) else setwd(wd)
   invisible(NULL)
}

# create_git --------------------

create_git <- function(yr_name = NULL, yr_mail = NULL){
  cat(create_git_ignore(), file=".gitignore")
  system("git init")
  cat(paste(rep("-", 65),collapse=""), "\n")
  if(is.null(yr_name)) yr_name <- readline("Provide name for git\n (e.g. Anaximandros Janson)     ")
  system(paste0("git config user.name \"",yr_name,"\""))
  if(is.null(yr_mail)) yr_mail <- readline("Provide email for git\n (e.g. Anaximandros.Janson@foo.bar)     ")
  system(paste0("git config user.email ",yr_mail))
  system("git add rapport.rnw references.bib")
  system(paste0("git commit -m \"proh initialized project ",gsub("-","",Sys.Date()),"\""))
  cat(paste(rep("-", 65),collapse=""), " Done! \n")
}

# create_org --------------------

create_org <- function(name = NULL, yr_name = NULL, yr_mail = NULL){
    paste0(
"#+TITLE: ", name,"
#+AUTHOR: ", yr_name, "
#+EMAIL: ", yr_mail, "
#+STARTUP: overview
#+STARTUP: hidestars

This is an org mode file, to be used with emacs. See: [[http://orgmode.org/][org mode link]].

* DONE initialize project '", name,"'
  CLOSED: [", Sys.Date(),"]
* TODO start working on project '", name, "'
  SCHEDULED: <", Sys.Date()+1,">

* header 1
  some text
** header 2a
   more text
*** header 3a
    yet more
*** header 3b
    again, more
** header 2b
   the last text
"
)
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
   fig.pos='hbt',
   fig.width=7,
   fig.height=5,
   message=FALSE,
   error=FALSE,
   warning=FALSE
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
# PROH OPTIONS: -------------------
#opts_proh$set(
#   ?
#   )
# CREATE PDF : ------------------------
cmp() # knit2pdf(input='rapport.rnw', clean=TRUE)

# SEND RAPPORT : ----------------------
send()
")
}

# RAPPORT TEXT ------------------
create_rapport <- function(name, yr_name = NULL, yr_mail = NULL,
                           class, meta = FALSE){
    if(is.null(yr_name)) yr_name <- Sys.info()['login']
    if(is.null(yr_mail)) yr_mail <- paste0(Sys.info()['login'], "@mail.com")
   paste0(
"%%%%%%  This file was created with ",R.version.string," and
%%%%%%  package proh ",packageVersion('proh')," on ",Sys.Date(),"
\\documentclass{",class,"}
%\\usepackage[swedish, english]{babel} % swedish % ?
%\\usepackage[latin1]{inputenc}
%\\newcommand{\\path}{\\texttt}
%\\newcommand{\\code}{\\texttt}
\\title{",gsub("_","\\_", name, fixed=TRUE),"\\\\ Version 0.0}
\\author{",yr_name,"\\\\ \\vspace{0.2cm}\\texttt{",yr_mail,"} }
% \\addtolength{\\hoffset}{-1.5cm}
% \\addtolength{\\textwidth}{3cm}
% \\addtolength{\\voffset}{-1.5cm}
% \\addtolength{\\textheight}{3cm}
% \\usepackage{attachfile}
% \\usepackage{subfig}
% \\usepackage{lscape}
% \\usepackage{longtable}
\\DeclareGraphicsExtensions{.pdf, .eps, .png, .jpg, .jpeg}

\\begin{document}

<<autoLoad, cache=FALSE, include=FALSE>>=
library(proh)
library(ucR)",
if(meta) "file.exists('_META_.r'){
   message('a _META_.r file exists - adjust accordingly!')
}",
"
### CHUNK OPTIONS: ---------------------
opts_chunk$set(
   cache=TRUE,
   include=FALSE,
   echo=FALSE,
   fig.pos='hbt',
   fig.width=7,
   fig.height=5,
   message=FALSE,
   error=FALSE,
   warning=FALSE
)
### KNIT OPTIONS: --------------------
## opts_knit$set(eval.after=c('fig.cap'))

fetchAll(calc=FALSE, autoload=TRUE)
@



\\section{Meta Information}
This rapport was generated by R \\cite{R} and knitr \\cite{knitr}.

Information about the R session:
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
  author = {Xie, Y.},
  journal = {},
  publisher = {CRC Press},
  title = {Dynamic Documents with R and Knitr. 2nd edition.},
  year = {2015}
}

@Manual{R,
  title = {R: A Language and Environment for Statistical Computing},
  author = {{R Core Team}},
  organization = {R Foundation for Statistical Computing},
  address = {Vienna, Austria},
  year = {2015},
  url = {http://www.R-project.org/},
}

@comment{ ******** BELOW ARE TEMPLATES FOR ARTICLES, BOOKS AND TECHNICAL REPORTS ********

@article{RR83,
  author = {Rosenbaum, P. R. and Rubin, D. B.},
  journal = {Biometrika},
  pages = {41--55},
  title = {The central role of the propensity score in observational studies},
  volume = {70},
  year = {1983}
}

@book{,
  author = {},
  journal = {},
  publisher = {},
  title = {},
  year = {}
 }

@techreport{,
  author = {},
  type = {},
  institution = {},
  pages = {},
  title = {},
  number = {},
  year = {}
}
}
")
}


# IGNORE TEXT -------------------
create_git_ignore <- function(){
   paste0(
".Rproj.user
*.Rhistory
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
NumSpacesForTab: 4
Encoding: UTF-8

RnwWeave: knitr
LaTeX: pdfLaTeX

AutoAppendNewline: Yes
StripTrailingWhitespace: Yes
")
}

#' @title Impose project structure
#' @description Impose project structure in existing directory, files and
#'   directories created by \code{newProject} will be
#' @author Henrik Renlund
#' @param path Path to project directory (else current)
#' @param class Class of document in 'rapport.rnw' (default: 'ucr')
#' @param go_there Set working directory to project directory? (default: TRUE)
#' @param RSproj Start a RStudio project? (deault: TRUE)
#' @param git should git be initialized? (also a .gitignore file will be
#'   created)
#' @seealso \code{proh::newProject}
#' @export

imposeProject <- function(path=NULL, class="ucr", go_there=TRUE, RSproj=TRUE, git=TRUE){
  wd <- getwd()
  if(is.null(path)) path <- wd
  tryCatch(
    expr=setwd(path),
    error = function(e) stop("[proh::imposeProject] there seems to be no such directory")
  )
  name <- rev(strsplit(getwd(),.Platform$file.sep)[[1]])[1]
  cat(paste0("A project directory structure will be imposed among\n   ",paste0(list.files(), collapse="\n   "),"\nin the directory:\n   ", wd, "\n Press 'x' to abort.\n Press anything else to proceed."))
  if( readline()=="x" ) {
    setwd(wd)
    return(NULL)
  }

  SET <- c("table", "received", "sent", "calc", "figure", "cache")
  for(S in SET) {
    if(!file.exists(S)) {
      dir.create(S)
      cat(paste0("created directory '", S, "'\n"))
    }
  }
  setwd(file.path(path, "calc"))
  if(!file.exists("autoload")) {
    dir.create("autoload")
    cat("created directory 'calc/autoload'\n")
  }
  setwd(path)

  if(!file.exists("_META_.r")) {
    cat(create_meta(path), file="_META_.r")
    cat("created file '_META_.r'\n")
  }
  if(!file.exists("rapport.rnw")) {
    cat(create_rapport(name, class), file="rapport.rnw")
    cat("created file 'rapport.rnw'\n")
  }
  if(!file.exists("references.bib")) {
    cat(create_bib(), file="references.bib")
    cat("created file 'references.bib'\n")
  }
  if(!file.exists(paste0(name,".rproj"))) {
    if(RSproj) {
      cat(create_proj(), file=paste0(name,".rproj"))
      cat("created Rstudio project file\n")
    }
  }

  cat( paste(rep("-", 65),collapse="") )

  if(!file.exists(".git")) {
    if(git) create_git()
  }
  if(go_there) setwd(path) else setwd(wd)
  invisible(NULL)
}
