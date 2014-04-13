\name{amcat.aggregate}
\alias{amcat.aggregate}
\title{Conduct an aggregate query on amcat}
\usage{
amcat.aggregate(conn, queries, labels = queries, sets, ...)
}
\arguments{
  \item{conn}{the connection object from
  \code{\link{amcat.connect}}}

  \item{queries}{a vector of queries to run}

  \item{labels}{if given, labels corresponding to the
  queries}

  \item{sets}{one or more article set ids to query on}

  \item{...}{additional arguments to pass to the AmCAT API.
  Use axis1 and axis2 to specify the grouping, possible
  values are medium, year/month/week/day. Additional
  filters are also possible.}
}
\value{
A data frame with hits per group
}
\description{
This function is similar to using the 'show table' function
in AmCAT. It allows you to specify a number of queries and
get the number of hits per search term, per period, etc.
}

