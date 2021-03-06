% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetchAll.r
\name{fetch_all}
\alias{fetch_all}
\alias{fetchAll}
\title{Load all saves}
\usage{
fetch_all(
  calc = TRUE,
  autoload = TRUE,
  overwrite = TRUE,
  message = FALSE,
  formats = c("rdat", "rdata")
)

fetchAll(
  calc = TRUE,
  autoload = TRUE,
  overwrite = TRUE,
  message = FALSE,
  formats = c("rdat", "rdata")
)
}
\arguments{
\item{calc}{should contents of directory 'calc' be loaded?}

\item{autoload}{should contents of directory 'calc/autoload' be loaded?}

\item{overwrite}{if variable already exists in global workspace, should it be overwritten?}

\item{message}{do you want an explanatory message?}

\item{formats}{formats to look for. Default \code{c('.rdata', '.rdat')}.}
}
\description{
Load the contents of 'calc' and its subdirectory 'autoLoad'
}
\section{Functions}{
\itemize{
\item \code{fetchAll}: An alias
}}

\author{
Henrik Renlund
}
