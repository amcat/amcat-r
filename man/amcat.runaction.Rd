\name{amcat.runaction}
\alias{amcat.runaction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Run an action on the API.
}
\description{
Run an action from the api (in api/4v/action).
}
\usage{
amcat.runaction(conn, action, format = "csv", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conn}{
%%     ~~Describe \code{conn} here~~
}
  \item{action}{
%%     ~~Describe \code{action} here~~
}
  \item{format}{
%%     ~~Describe \code{format} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (conn, action, format = "csv", ...) 
{
    resource = "api/action"
    url = paste(conn$host, resource, action, sep = "/")
    url = paste(url, "?format=", format, sep = "")
    print(paste("Running action at", url))
    opts = list(userpwd = paste(conn$username, conn$passwd, sep = ":"), 
        ssl.verifypeer = FALSE, httpauth = 1L)
    result = postForm(url, ..., .opts = opts)
    if (result == "401 Unauthorized") 
        stop("401 Unauthorized")
    if (format == "json") {
        result = fromJSON(result)
    }
    else if (format == "csv") {
        con <- textConnection(result)
        result = read.csv2(con)
    }
    result
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
