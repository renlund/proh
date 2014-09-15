\docType{data}
\name{opts_proh}
\alias{opts_proh}
\title{opts (not useful yet)}
\format{\preformatted{List of 3
 $ get    :function (name)  
 $ set    :function (...)  
 $ restore:function ()  
}}
\usage{
opts_proh
}
\description{
This list tries to mimic the behaviour of opts_chunk from knitr.
Currently two values are maintained with the functions in (the list) opts_proh: \itemize{
\item attach_graph
\item attach_table
}
Currently, these values has no effect on anything - but hopefully they will
in the future...
}
\keyword{datasets}

