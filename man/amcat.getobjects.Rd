\name{amcat.getobjects}
\alias{amcat.getobjects}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve objects from the API
}
\description{
Retrieve a list of objects from the API, returning a data frame.
}
\usage{
amcat.getobjects(conn, resource, format = "csv", stepsize = 50000, filters = list(), use__in = c(), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conn}{
%%     ~~Describe \code{conn} here~~
}
  \item{resource}{
%%     ~~Describe \code{resource} here~~
}
  \item{format}{
%%     ~~Describe \code{format} here~~
}
  \item{stepsize}{
%%     ~~Describe \code{stepsize} here~~
}
  \item{filters}{
%%     ~~Describe \code{filters} here~~
}
  \item{use__in}{
%%     ~~Describe \code{use__in} here~~
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
function (conn, resource, format = "csv", stepsize = 50000, filters = list(), 
    use__in = c(), ...) 
{
    limit = stepsize
    url = paste(conn$host, "/api/v4/", resource, "?format=", 
        format, "&page_size=", limit, sep = "")
    filters = c(filters, list(...))
    if (length(filters) > 0) {
        for (i in 1:length(filters)) if (names(filters)[i] \%in\% 
            use__in) {
            url = paste(url, "&", paste(paste(names(filters)[i], 
                filters[[i]], sep = "="), collapse = "&"), sep = "")
        }
        else url = paste(url, "&", names(filters)[i], "=", filters[[i]], 
            sep = "")
    }
    opts = list(userpwd = paste(conn$username, conn$passwd, sep = ":"), 
        ssl.verifypeer = FALSE, httpauth = 1L)
    print(paste("Getting objects from", url))
    page = 1
    result = data.frame()
    while (TRUE) {
        subresult = getURL(paste(url, "&page=", as.integer(page), 
            sep = ""), .opts = opts)
        subresult = amcat.readoutput(subresult, format = format)
        result = rbind(result, subresult)
        if (nrow(subresult) < stepsize) 
            break
        page = page + 1
    }
    result
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
