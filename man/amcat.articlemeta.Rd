\name{amcat.articlemeta}
\alias{amcat.articlemeta}
\title{Get article metadata from AmCAT}
\usage{
amcat.articlemeta(conn, sets, filters = list(), columns = c("id", "date",
  "medium", "length"), time = F)
}
\arguments{
  \item{conn}{the connection object from
  \code{\link{amcat.connect}}}

  \item{sets}{the article set id(s) to retrieve}

  \item{filters}{additional filters, e.g. list(medium=1)}

  \item{columns}{the names of columns to retrieve}

  \item{time}{if true, parse the date as POSIXct datetime
  instead of Date}
}
\value{
A dataframe containing the articles and the selected
columns
}
\description{
Uses the \code{\link{amcat.getobjects}} function to
retrieve article metadata, and applies some additional
postprocessing, e.g. to convert the data to Date objects.
}

