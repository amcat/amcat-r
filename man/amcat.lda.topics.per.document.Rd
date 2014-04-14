\name{amcat.lda.topics.per.document}
\alias{amcat.lda.topics.per.document}
\title{Get the topics per document, optionally merged with}
\usage{
amcat.lda.topics.per.document(topics)
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
Return a data frame containing article metadata and topic
occurence per document
}

