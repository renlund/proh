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
#'   chunk of the report file by \code{fetch_all}.}
#' \item{figure: for plots
#'   (also used by knitr)}
#' \item{recieved: typically this is were I put files
#'   given by clients}
#' \item{sent: this is were I store things sent to client.
#'   The function \code{send} will attach the current date to the pdf version of
#'   the report and put it in this directory. Optionally, \code{send} can zip
#'   the report along with the graphs and tables from their respective
#'   directory}
#' \item{table: for (human readable) tabulated data}
#' }
#' and creates, optionally, the files
#' \itemize{
#' \item{'rapport.rnw': the report file, can
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
#' @param dm should a data management file be created? (default: TRUE)
#' @param RSproj Start a RStudio project? (deault: TRUE)
#' @param git should git be initialized? (also a .gitignore file will be
#'   created)
#' @param checkpoint should checkpoint be used?
#' @param checkpoint.date date for checkpoint
#' @param go_there Set working directory to project directory? (default: FALSE)
#' @param org should an org file be created?
#' @export
new_project <- function(name="new_project", path=NULL, class="ucr",
                        dm = TRUE, RSproj=TRUE, git=TRUE,
                        checkpoint = TRUE, checkpoint.date = NULL,
                        go_there=FALSE, org = FALSE){
    wd <- getwd()
    if(is.null(checkpoint.date)){
        checkpoint.date <- as.character(Sys.Date())
    } else {
        cp_date <- as.Date(checkpoint.date)
        if(cp_date > Sys.Date()) stop("bad date")
    }
    if(checkpoint){
        cat("You want to use checkpoint, right? If you are not currently\n",
            "using the R version that you want for this project, stop and\n",
            "run this function with the version you want (it will be much\n",
            "easier)\n Press 'x' to abort.\n Press anything else to proceed.")
        if(readline() == "x") return(invisible(NULL))
    }
    install_directory <- if(is.null(path)) wd else path
    cat(paste0("The new '", name, "' project directory structure will be created\n under directory:\n   ", install_directory, "\n Press 'x' to abort.\n Press anything else to proceed."))
    if( readline()=="x" ) {
        ## setwd(install_directory) ## why was this here?
        return(invisible(NULL))
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
    ## file.create("calc/.proh")
    setwd(file.path(full.path, "calc"))
    dir.create("autoload")
    setwd(full.path)
    rapport_name <- gsub(" ", "-", name, fixed = TRUE)
    source_file <- paste0(rapport_name, ".rnw")
    dm_source_file <- if(dm) paste0("DM--", rapport_name, ".rnw") else NULL
    output_file <- paste0(rapport_name, ".pdf")
    dm_output_file <- if(dm) paste0("DM--", rapport_name, ".pdf") else NULL
    cat(create_rnw(name = name, yr_name = yr_name,
                   yr_mail = yr_mail, class = class,
                   source_file = source_file,
                   output_file = output_file, DM = FALSE,
                   checkpoint = checkpoint),
        file=source_file)
    if(dm) cat(create_rnw(name = name, yr_name = yr_name,
                          yr_mail = yr_mail, class = class,
                          source_file = dm_source_file,
                          output_file = dm_output_file, DM = TRUE,
                          checkpoint = checkpoint),
               file=dm_source_file)
    cat(create_bib(), file="references.bib")
    if(RSproj) cat(create_proj(), file=paste0(rapport_name,".rproj"))
    end.text <- paste0(
        paste(rep("-", 65),collapse=""),
        "\nCreated new PROH project directory:\n ", full.path, "\n",
        paste(rep("-", 65),collapse=""), "\n"
    )
    cat(end.text)
    cat(create_rprofile(source_file, dm_source_file, checkpoint = checkpoint,
                        cp.date = checkpoint.date),
        file = ".Rprofile")
    if(checkpoint){
        cp_path <- file.path("~", ".checkpoint", checkpoint.date,
                  "lib", R.version$platform,
                  base::getRversion())
        dir.create(cp_path, recursive = TRUE, showWarnings = FALSE)
        ## old_repos <- getOption("repos")
        ## checkpoint::setSnapshot(checkpoint.date)
        old_lib <- .libPaths()
        .libPaths(cp_path)
        laddad <- function(s){
            tryCatch(
                expr = isNamespaceLoaded(s),
                error = function(e) FALSE
            )
        }
        proh_loaded <- laddad("proh")
        knitr_loaded <- laddad("knitr")
        devt_loaded <- laddad("devtools")
        rmark_loaded <- laddad("rmarkdown")
        if(proh_loaded){
            ## warning("proh is loaded, will be unloaded")
            unloadNamespace("proh")
        }
        if(knitr_loaded){
            ## warning("knitr is loaded, will be unloaded")
            unloadNamespace("knitr")
        }
        if(devt_loaded){
            ## warning("devtools is loaded, will be unloaded")
            unloadNamespace("devtools")
        }
        if(rmark_loaded){
            ## warning("rmarkdown is loaded, will be unloaded")
            unloadNamespace("rmarkdown")
        }
        paket <- c("devtools", "knitr", "rmarkdown", "Hmisc", "coin", "broom", "dplyr")
        utils::install.packages(pkgs = paket, ## lib = cp_path,
                                repos = paste0("https://mran.microsoft.com/snapshot/",
                                               checkpoint.date),
                                dependencies = c("Depends", "Imports"),
                                verbose = TRUE, type = "binary")
        devtools::install_github("renlund/proh", reload = FALSE, force = TRUE) ## , lib = cp_path)
        .libPaths(old_lib)
        ##options(repos = old_repos)
        ## install basic packages into checkpoint

    }
    if(org) cat(create_org(name, yr_name, yr_mail),
                file = paste0(name, "-org.org"))
    if(git) create_git(yr_name, yr_mail, source_file)
    if(go_there){
        setwd(full.path)
        proh::opts_proh$set("source_file" = source_file)
    } else {
        setwd(wd)
    }

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

## REPORT TEXT ------------------
create_rnw <- function(name, yr_name = NULL, yr_mail = NULL, class,
                       source_file, output_file, DM = FALSE,
                       checkpoint = FALSE){
    if(is.null(yr_name)) yr_name <- Sys.info()['login']
    if(is.null(yr_mail)) yr_mail <- paste0(Sys.info()['login'], "@mail.com")
    pre_text <- if(DM) "Data management for " else ""
   paste0(
"%%%%%%  This file was created with ", R.version.string," and
%%%%%%  package proh ", utils::packageVersion('proh')," on ",Sys.Date(),"
\\documentclass{",class,"}
%\\usepackage[swedish, english]{babel}
%\\usepackage[latin1]{inputenc}
%\\newcommand{\\path}{\\texttt}
%\\newcommand{\\code}{\\texttt}
% \\addtolength{\\hoffset}{-1.5cm}
% \\addtolength{\\textwidth}{3cm}
% \\addtolength{\\voffset}{-1.5cm}
% \\addtolength{\\textheight}{3cm}
% \\usepackage[table]{xcolor}
\\usepackage{attachfile}
\\usepackage{subfig}
\\usepackage{lscape}
\\usepackage{longtable}
\\DeclareGraphicsExtensions{.pdf, .eps, .png, .jpg, .jpeg}

<<'", if(DM) "DM-", "SETUP', cache=FALSE, include=FALSE>>=
### PACKAGES: ----------------------------------------------
", if(checkpoint) "if(FALSE){
    library(knitr)
    library(devtools)
    devtools::install_github('renlund/proh') ## , ref = ?)
    ## get latest ref-number from:
    ##      https://github.com/renlund/proh/commit/master
}\n",
"library(proh)       # https://github.com/renlund/proh
## library(datma)       # https://github.com/renlund/datma
## library(descripteur) # https://github.com/renlund/descripteur

## library(dplyr)
## library(ggplot2); theme_set(theme_bw())
## library(tidyr)
## library(haven)

## library(Hmisc)
## library(rms)
## library(data.table)
## library(survival)
## library(optmatch)
## library(lme4)

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

## KNIT HOOKs: ---------------------------------------------
## ## this hook can resolve a common color conflict
## knit_hooks$set(document = function(x) {
##     sub(pattern = '\\\\usepackage[]{color}',
##         replacement = '\\\\usepackage[table]{xcolor}',
##         x, fixed = TRUE)
## })

### KNIT OPTIONS: ------------------------------------------
opts_knit$set(eval.after=c('fig.cap'))

### PROH OPTIONS: ------------------------------------------
opts_proh$set(
    ", if(DM) "dm_", "source_file = '", source_file,"', ## also in .rprofile
    ", if(DM) "dm_", "output_file = '", output_file,"',
    ", if(DM) "dm_", "version = 'Version 0.1'
)

### LOAD/SET PARAMETERS: -----------------------------------
fetch_all(calc=FALSE, autoload=TRUE) ## loads 'calc/autoload'
@

\\title{",pre_text, gsub("_","\\_", name, fixed=TRUE),
"\\Sexpr{proh:::proh_get('",
if(DM) "dm_", "version_latex')}}
\\author{",yr_name,"\\\\ \\vspace{0.2cm}\\texttt{",yr_mail,"} }

\\begin{document}

%\\tableofcontents
%\\listoftables
%\\listoffigures
%\\newpage

\\section{First section}

\\clearpage
\\section{Meta Information}
This report was generated by R \\cite{R} and knitr \\cite{knitr} via
GNU Emacs \\cite{emacs} and ESS \\cite{ESS}.

Information about the R session:
<<'", if(DM) "DM-", "META', cache=FALSE, echo=FALSE, results='asis', include = TRUE>>=
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
"@Manual{R,
  title = {R: A Language and Environment for Statistical Computing},
  author = {{R Core Team}},
  organization = {R Foundation for Statistical Computing},
  address = {Vienna, Austria},
  year = {2016},
  note = {\\url{http://www.R-project.org/}}
}

@book{knitr,
  author = {Xie, Y.},
  journal = {},
  publisher = {CRC Press},
  title = {Dynamic Documents with R and Knitr. 2nd edition.},
  year = {2015}
}

@Misc{emacs,
author =   {Richard Stallman},
title =    {{GNU} {E}macs},
howpublished = {\\url{https://www.gnu.org/software/emacs/}}
}

@Misc{ESS,
author = {{A.J. Rossini} and {R.M. Heiberger} and {K. Hornik} and {M. Maechler} and {R.A Sparapani} and {S.J. Eglen} and {S.P. Luque} and {H. Redestig} and {V. Spinu} and {L. Henry}},
title = {{ESS}: {E}macs {S}peaks {S}tatistics},
howpublished = {\\url{http://ess.r-project.org}},
version = {Version 16.10-1}
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
"
*
!.Rprofile
!*.rproj
!*.bib
!*.org
!*.txt
!*.r
!*.rnw
")
}

## ## old list below worked by exclusion, new list above works more with inclusion
## ".Rproj.user
## *.Rhistory
## *.RData
## *.tex
## *.toc
## *.concordance
## *.log
## *.brf
## *.bbl
## *.blg
## *.lof
## *.lot
## *.out
## *.aux
## .gitignore
## *~
## *.pdf
## cache/*
## figure/*
## sent/*
## table/*
## calc/*
## "

## PROJ TEXT ---------------------
create_proj <- function(){
paste0(
"Version: 1.0

RestoreWorkspace: No
SaveWorkspace: No
AlwaysSaveHistory: No

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
create_rprofile <- function(source_file, dm_source_file, checkpoint, cp.date){
    DM <- !is.null(dm_source_file)
    s_file_text <- if(DM) paste0(",\n            dm_source_file = '", dm_source_file,"'") else ""
paste0(
"if(file.exists(file.path('~', '.rprofile'))){
    source(file.path('~', '.rprofile'))
} else {
    cat('\\n ## There is no .rprofile found in the home directory\\n')
}
tmp <- paste0(rep('+', options('width')$width-3), collapse = '')
cat(paste0('\\n ', tmp, '\\n   R started in a proh-directory with an .rprofile file.\\n',
           '   This will set a source_file in the proh options and also try\\n',
           '   to load the .rprofile (if it exists) in the home directory.\\n'))
",
if(checkpoint){
paste0(
"
cat(paste0('   It will also load knitr and activate checkpoint with snapshot\\n',
           '   date ", cp.date, "\\n'))

require(checkpoint)
.checkpoint_startup <- checkpoint::checkpoint(
    snapshotDate = '", cp.date,"',
    R.version = '", as.character(getRversion()), "',
    use.knitr = TRUE,
    scan.rnw.with.knitr = TRUE
)
checkpoint::setSnapshot('", cp.date,"')
")
} else "",
"tryCatch(
    exp = {
        require(proh)
        opts_proh$set(
            source_file = '", source_file,"'",s_file_text,"
        )
    },
    error = function(e) warning('package proh not installed')
)
cat('\\n', tmp, '\\n')
rm(tmp)
"
)
}
