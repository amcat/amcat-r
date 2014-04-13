\name{amcat.getpages}
\alias{amcat.getpages}
\title{Get and rbind pages from the AmCAT API}
\usage{
amcat.getpages(conn, path, format = "csv", page_size = 1000,
  filters = NULL, post = FALSE)
}
\arguments{
  \item{conn}{the connection object from
  \code{\link{amcat.connect}}}

  \item{path}{the path of the url to retrieve (using the
  host from conn)}

  \item{filters:}{a named vector of filters, e.g.
  c(project=2, articleset=3)}

  \item{post}{use HTTP POST instead of GET}
}
\value{
dataframe
}
\description{
Get and rbind pages from the AmCAT API
}

