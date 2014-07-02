\name{send}
\alias{send}
\title{Mark a rapport with current date}
\usage{
send(name = "rapport", minimal = TRUE, graph.filter = NULL,
  table.filter = NULL, git = "ask")
}
\arguments{
  \item{name}{name of output rapport (will include todays
  date), defaults to 'report', but if TRUE the name of the
  project will be used}

  \item{minimal}{should ONLY the rapport.pdf be sent?
  (default: TRUE). If not the rapport will be 'zipped' into
  a 'delivery' along with graphs and tables}

  \item{graph.filter}{filter for which graphs to include in
  'delivery'}

  \item{table.filter}{filter for which graphs to include in
  'delivery'}

  \item{git}{will ask if a little git:ing is to be done,
  but this can also be to TRUE/FALSE}
}
\description{
Mark a rapport with todays date and put it in the 'sent'
folder
}
\author{
Henrik Renlund
}

