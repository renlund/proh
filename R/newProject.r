#' @title Create new project
#' @description This function sets up a project directory structure along with
#'   some files
#' @author Henrik Renlund
#' @details This function sets up a folder with subfolders \itemize{
#' \item{cache: this is only used by knitr}
#' \item{calc: this is for storage of
#'   .rdat files}
#' \item{calc/autoload: this is for storage of inline values.
#'   Typically this folder will automatically be loaded in a first (uncached)
#'   chunk of the rapport file by \code{fetch_all}.}
#' \item{figure: for plots
#'   (also used by knitr)}
#' \item{recieved: typically this is were I put files
#'   given by clients}
#' \item{sent: this is were I store things sent to client.
#'   The function \code{send} will attach the current date to the pdf version of
#'   the rapport and put it in this directory. Optionally, \code{send} can zip
#'   the rapport along with the graphs and tables from their respective
#'   directory}
#' \item{table: for (human readable) tabulated data}
#' }
#' and creates, optionally, the files
#' \itemize{
#' \item{'rapport.rnw': the rapport file, can
#'   be changed to suit your needs, but is designed to have a first uncached
#'   chunk (by default called 'autoLoad') that executes \code{fetch_all()}}
#' \item{.Rprofile which will load and point proh to the source file}
#' \item{'references.bib': a template for bibTeX
#'   references}
#' \item{a .rsproj file with the project name: this is an RStudio
#'   project file, by starting this file RStudio will set the working directory
#'   and remember what documents you were looking at. There are settings to be
#'   made that can be project specific}
#' \item{.gitignore: a file that git uses
#'   to tell which files to ignore}
#' }
#' @param name Name of the project
#' @param path Path to project directory (else current)
#' @param class Class of document in 'rapport.rnw' (default: 'ucr')
#' @param go_there Set working directory to project directory? (default: TRUE)
#' @param RSproj Start a RStudio project? (deault: TRUE)
#' @param git should git be initialized? (also a .gitignore file will be
#'   created)
#' @param org should an org file be created?
#' @export
new_project <- function(name="new_project", path=NULL, class="ucr",
                       go_there=TRUE, RSproj=TRUE, git=TRUE, org = FALSE){
    install_directory <- if(is.null(path)) getwd() else path
    cat(paste0("The new '", name, "' project directory structure will be created\n under directory:\n   ", install_directory, "\n Press 'x' to abort.\n Press anything else to proceed."))
    if( readline()=="x" ) {
        setwd(install_directory)
        return(NULL)
    }
    yr_name <- readline("Provide name for project/git\n (e.g. Anaximandros Janson)     ")
    yr_mail <-
        readline("Provide email for project/git\n (e.g. Anaximandros.Janson@foo.bar)     ")
    if(yr_name == "") yr_name <-  Sys.info()['login']
    if(yr_mail == "") yr_mail <-  paste0(Sys.info()['login'], "@mail.com")
    full.path <- file.path(install_directory, name)
    dummy <- 0
    ENV <- environment()
    tryCatch(
        setwd(full.path),
        error = function(e) assign("dummy", value=1, envir=ENV)
    )
    if( dummy == 0 ) {
        setwd(full.path)
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
    rapport_name <- gsub(" ", "-", name, fixed = TRUE)
    source_file <- paste0(rapport_name, ".rnw")
    output_file <- paste0(rapport_name, ".pdf")
    cat(create_rnw_rapport(name = name, yr_name = yr_name,
                           yr_mail = yr_mail, class = class,
                           source_file = source_file,
                           output_file = output_file),
        file=source_file)
    cat(create_bib(), file="references.bib")
    if(RSproj) cat(create_proj(), file=paste0(rapport_name,".rproj"))
    end.text <- paste0(
        paste(rep("-", 65),collapse=""),
        "\nCreated new PROH project directory:\n ", full.path, "\n",
        paste(rep("-", 65),collapse=""), "\n"
    )
    cat(end.text)
    if(org) cat(create_org(name, yr_name, yr_mail),
                file = paste0(name, "-org.org"))
    if(git) create_git(yr_name, yr_mail, source_file)
    if(go_there) setwd(full.path) else setwd(full.path)
    cat(create_rprofile(source_file, output_file),
        file = ".Rprofile")
    invisible(NULL)
}

## create_git --------------------

create_git <- function(yr_name = NULL, yr_mail = NULL, source_file){
  cat(create_git_ignore(), file=".gitignore")
  system("git init")
  cat(paste(rep("-", 65),collapse=""), "\n")
  if(is.null(yr_name)) yr_name <- readline("Provide name for git\n (e.g. Anaximandros Janson)     ")
  system(paste0("git config user.name \"",yr_name,"\""))
  if(is.null(yr_mail)) yr_mail <- readline("Provide email for git\n (e.g. Anaximandros.Janson@foo.bar)     ")
  system(paste0("git config user.email ",yr_mail))
  system(paste0("git add ", source_file, " references.bib"))
  system(paste0("git commit -m \"proh initialized project ",gsub("-","",Sys.Date()),"\""))
  cat(paste(rep("-", 65),collapse=""), " Done! \n")
}

## create_org --------------------

create_org <- function(name = NULL, yr_name = NULL, yr_mail = NULL){
    paste0(
"#+TITLE: ", name,"
#+AUTHOR: ", yr_name, "
#+EMAIL: ", yr_mail, "
#+STARTUP: contents

This is an org mode file, to be used with emacs. See: [[http://orgmode.org/][org mode link]].
You might want to edit .emacs to include this file in the org-agenda-files variable.

* ", name," action list
** DONE initialize project '", name,"'
  CLOSED: [", Sys.Date(),"]
** TODO start working on project '", name, "'
  SCHEDULED: <", Sys.Date()+1,">
"
)
}

## RAPPORT TEXT ------------------
create_rnw_rapport <- function(name, yr_name = NULL, yr_mail = NULL, class,
                               source_file, output_file){
    if(is.null(yr_name)) yr_name <- Sys.info()['login']
    if(is.null(yr_mail)) yr_mail <- paste0(Sys.info()['login'], "@mail.com")
   paste0(
"%%%%%%  This file was created with ", R.version.string," and
%%%%%%  package proh ", utils::packageVersion('proh')," on ",Sys.Date(),"
\\documentclass{",class,"}
%\\usepackage[swedish, english]{babel} % swedish % ?
%\\usepackage[latin1]{inputenc}
%\\newcommand{\\path}{\\texttt}
%\\newcommand{\\code}{\\texttt}
% \\addtolength{\\hoffset}{-1.5cm}
% \\addtolength{\\textwidth}{3cm}
% \\addtolength{\\voffset}{-1.5cm}
% \\addtolength{\\textheight}{3cm}
% \\usepackage{attachfile}
% \\usepackage{subfig}
% \\usepackage{lscape}
% \\usepackage{longtable}
\\DeclareGraphicsExtensions{.pdf, .eps, .png, .jpg, .jpeg}

<<'SETUP', cache=FALSE, include=FALSE>>=
### PACKAGES: ----------------------------------------------
library(proh)
## library(ucR)
## library(dataman)
## library(descripteur)

## library(dplyr)
## library(tidyr)
## library(ggplot2)

## library(Hmisc)
## library(rms)
## library(data.table)
## library(survival)
## library(coxme)
## library(optmatch)

### CHUNK OPTIONS: -----------------------------------------
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

### KNIT OPTIONS: ------------------------------------------
opts_knit$set(eval.after=c('fig.cap'))

### PROH OPTIONS: ------------------------------------------
opts_proh$set(
    source_file = '", source_file,"',
    output_file = '", output_file,"',
    version = 'Version 0.01'
)

### LOAD/SET PARAMETERS: -----------------------------------
fetch_all(calc=FALSE, autoload=TRUE) ## loads 'calc/autoload'
@

\\title{",gsub("_","\\_", name, fixed=TRUE),"\\Sexpr{proh::proh_get('version_latex')}}
\\author{",yr_name,"\\\\ \\vspace{0.2cm}\\texttt{",yr_mail,"} }

\\begin{document}

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


## BIB TEXT ----------------------
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
  year = {2016},
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


## IGNORE TEXT -------------------
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
table/*
calc/*
")
}

## PROJ TEXT ---------------------
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

## Rprofile -------------------
create_rprofile <- function(source_file, output_file){
paste0(
"tryCatch(
    exp = {
        require(knitr)
        require(proh)
        opts_proh$set(
            source_file = '", source_file,"'
        )
    },
    error = function(e) warning('package proh not installed')
)
"
)
}


#' @describeIn new_project An alias
#' @export
newProject <- function(name="new_project", path=NULL, class="ucr",
                       go_there=TRUE, RSproj=TRUE, git=TRUE, org = TRUE){
    message("from proh 0.3 we recommend using 'new_project' instead")
    new_project(name=name, path=path, class=class,
                       go_there=go_there, RSproj=RSproj, git=git, org = org)

}

# - # @title Impose project structure
# - # @description Impose project structure in existing directory, files and
# - #   directories created by \code{newProject} will be
# - # @author Henrik Renlund
# - # @param path Path to project directory (else current)
# - # @param class Class of document in 'rapport.rnw' (default: 'ucr')
# - # @param go_there Set working directory to project directory? (default: TRUE)
# - # @param RSproj Start a RStudio project? (deault: TRUE)
# - # @param git should git be initialized? (also a .gitignore file will be
# - #   created)
# - # @seealso \code{proh::newProject}
# - # @export
## imposeProject <- function(path=NULL, class="ucr", go_there=TRUE, RSproj=TRUE, git=TRUE){
##   wd <- getwd()
##   if(is.null(path)) path <- wd
##   tryCatch(
##     expr=setwd(path),
##     error = function(e) stop("[proh::imposeProject] there seems to be no such directory")
##   )
##   name <- rev(strsplit(getwd(),.Platform$file.sep)[[1]])[1]
##   cat(paste0("A project directory structure will be imposed among\n   ",paste0(list.files(), collapse="\n   "),"\nin the directory:\n   ", wd, "\n Press 'x' to abort.\n Press anything else to proceed."))
##   if( readline()=="x" ) {
##     setwd(wd)
##     return(NULL)
##   }

##   SET <- c("table", "received", "sent", "calc", "figure", "cache")
##   for(S in SET) {
##     if(!file.exists(S)) {
##       dir.create(S)
##       cat(paste0("created directory '", S, "'\n"))
##     }
##   }
##   setwd(file.path(path, "calc"))
##   if(!file.exists("autoload")) {
##     dir.create("autoload")
##     cat("created directory 'calc/autoload'\n")
##   }
##   setwd(path)
##   if(!file.exists("rapport.rnw")) {
##     cat(create_rnw_rapport(name, class), file="rapport.rnw")
##     cat("created file 'rapport.rnw'\n")
##   }
##   if(!file.exists("references.bib")) {
##     cat(create_bib(), file="references.bib")
##     cat("created file 'references.bib'\n")
##   }
##   if(!file.exists(paste0(name,".rproj"))) {
##     if(RSproj) {
##       cat(create_proj(), file=paste0(name,".rproj"))
##       cat("created Rstudio project file\n")
##     }
##   }

##   cat( paste(rep("-", 65),collapse="") )

##   if(!file.exists(".git")) {
##     if(git) create_git()
##   }
##   if(go_there) setwd(path) else setwd(wd)
##   invisible(NULL)
## }

