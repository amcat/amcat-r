\name{amcat.term.statistics}
\alias{amcat.term.statistics}
\title{Compute some useful corpus statistics for a dtm}
\usage{
amcat.term.statistics(dtm)
}
\arguments{
  \item{dtm}{a document term matrix (e.g. the output of
  \code{\link{amcat.createDTM}})}
}
\value{
A data frame with rows corresponding to the terms in dtm
and the statistics in the columns
}
\description{
Compute a number of useful statistics for filtering words:
term frequency, idf, etc.
}

