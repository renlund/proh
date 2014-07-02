\docType{data}
\name{opts_proh}
\alias{opts_proh}
\title{opts}
\format{\preformatted{List of 3
 $ get    :function (name)  
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 16 13 27 1 13 1 16 27
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x00000000088bc540> 
 $ set    :function (...)  
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 33 13 41 1 13 1 33 41
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x00000000088bc540> 
 $ restore:function ()  
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 46 12 54 1 12 1 46 54
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x00000000088bc540> 
}}
\usage{
opts_proh
}
\description{
This list tries to mimic the behaviour of opts_chunk from
knitr. Currently two values are maintained with opts_proh:
\itemize{ \item attach_graph \item attach_table }
}
\keyword{datasets}

