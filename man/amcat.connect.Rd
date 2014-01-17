\name{amcat.connect}
\alias{amcat.connect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Connect to AmCAT}

\description{
Connect to AmCAT with the given username and password. Stores the result plaintext in a list, so be warned.
}
\usage{
amcat.connect(host, username = NULL, passwd = NULL, test = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{host}{
%%     ~~Describe \code{host} here~~
}
  \item{username}{
%%     ~~Describe \code{username} here~~
}
  \item{passwd}{
%%     ~~Describe \code{passwd} here~~
}
  \item{test}{
%%     ~~Describe \code{test} here~~
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
function (host, username = NULL, passwd = NULL, test = T) 
{
    if (is.null(username)) {
        cat(paste("Please enter the username for", host, "\n"))
        username = readline()
    }
    if (is.null(passwd)) {
        cat(paste("Please enter the password for ", username, 
            "@", host, "\n", sep = ""))
        passwd = readline()
    }
    conn = list(passwd = passwd, username = username, host = host)
    if (test) {
        version = amcat.getobjects(conn, "amcat")$db_version
        cat(paste("Connected to ", username, "@", host, " (db_version=", 
            version, ")\n", sep = ""))
    }
    conn
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
