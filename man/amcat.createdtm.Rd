\name{amcat.createdtm}
\alias{amcat.createdtm}
\title{Create a document term matrix from a list of tokens}
\usage{
amcat.createdtm(ids, terms, freqs)
}
\arguments{
  \item{ids}{a vector of document ids}

  \item{terms}{a vector of words of the same length as ids}

  \item{freqs}{a vector of the frequency a a term in a
  document}
}
\value{
a document-term matrix \code{\link{DocumentTermMatrix}}
}
\description{
Create a document-term matrix (dtm) from a list of ids,
terms, and frequencies
}

