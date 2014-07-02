\name{omni}
\alias{omni}
\title{Keep an object omnipresent}
\usage{
omni(name)
}
\arguments{
  \item{name}{character; the name of a variable}
}
\description{
Keep an object available within PROH structure
}
\details{
If .rnw files are cached, sometimes the objects used in
'Sexpr' are unavailable. 'Keep' stores variables in
'calc/autoload' which are automatically loaded in the first
chunk of a PROH rapport.
}
\author{
Henrik Renlund
}

