% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/killCache.r
\name{kill_cache}
\alias{kill_cache}
\alias{killCache}
\title{Kill Cache}
\usage{
kill_cache(
  pattern = NULL,
  cache = TRUE,
  figure = TRUE,
  table = TRUE,
  env = NULL
)

killCache(
  pattern = NULL,
  cache = TRUE,
  figure = TRUE,
  table = TRUE,
  env = TRUE
)
}
\arguments{
\item{pattern}{if not \code{NULL} (default) this can search for cache items to remove}

\item{cache}{remove files from sub directory 'cache'? (default: TRUE)}

\item{figure}{remove files from sub directory 'figure'? (default: TRUE)}

\item{table}{remove files from sub directory 'table'? (default: TRUE)}

\item{env}{remove ALL objects in the global environment? (default: TRUE if no
pattern given)}
}
\description{
Remove the contents of subdirectory 'cache', 'figure' and 'table'.
}
\details{
Sometimes it is easier clean the cache than to set \code{CACHE = FALSE} globally, since you may have forgotten to specify all dependencies...
}
\section{Functions}{
\itemize{
\item \code{killCache}: An alias
}}

\author{
Henrik Renlund
}
