\name{amcat.articlemeta}
\alias{amcat.articlemeta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve article metadata from the API
}
\description{
Retrieve a data frame with the id, headline etc for the given articles
}
\usage{
amcat.articlemeta(conn, articlesets, media=c(), from_date=NA, to_date=NA, filter_columns=c('id','date','medium','length'), time=F)
}
\arguments{

\item{conn}{The connection object returned from amcat.connect}
\item{articlesets}{A vector of article set ids}
\item{media}{An optional list of medium ids to select}
\item{from_date}{An optional from_date filter}
\item{to_date}{An optional to_date filter}
\item{filter_columns}{Which columns to retrieve}
\item{time}{No clue!}


}
\details{
}
\value{
}
\references{
}
\author{
}
\note{
}


\seealso{
}
\examples{
}
