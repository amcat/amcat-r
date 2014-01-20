library(rjson)
library(RCurl)

#' Connect to the AmCAT API
#'
#' Connect to the AmCAT API and requests a temporary (24h) authentication token that will be stored in the output
#' If username and password are not given, a file ~/.amcatauth will be read, which should be a csv file with
#' columns host (may be *), username, password. If the file cannot be read, username will be taken from $USER
#' and the user will be prompted for the password.
#' 
#' @param host the hostname, e.g. http://amcat.vu.nl or http://localhost:8000
#' @param username the username to login with, e.g. 'amcat'. 
#' @param passwd the password to login with, e.g. 'amcat'
#' @param token an existing token to authenticate with. If given, username and password are not used and the token is not tested
#' @return A list with authentication information that is used by the other functions in this package
#' @export
amcat.connect <- function(host, username=NULL, passwd=NULL, token=NULL) {
  if (is.null(token)) {
    if (is.null(passwd)) { # try amcatauth file
      a = tryCatch(.readauth(host), error=function(e) print("Could not read ~/.amcatauth"))
      if (!is.null(a)) {
        username = a$username
        passwd = a$password
      }
    }
    if (is.null(username)) { # try USER variable
      username = Sys.getenv("USER")
      if (username == '') { # ask
        cat(paste("Please enter the username for", host, "\n"))
        username=readline()
      }
    }
    if (is.null(passwd)) { #ask
      cat(paste("Please enter the password for ",username,"@", host, " (or create a ~/.amcatauth file) \n", sep=""))
      passwd=readline()
    }
    # get auth token
    url = paste(host, '/api/v4/get_token', sep='')
    
    res = tryCatch(postForm(url, username=username, password=passwd, .checkParams=F), 
                   error=function(e) stop(paste("Could not get token from ",
                                                 username,":",passwd,"@", host,
                                                 " please check host, username and password. Error: ", e, sep="")))
    token = fromJSON(res)$token
  }
  list(host=host, token=token)
}

#' Get authentication info from ~/.amcatauth file
.readauth <- function(host) {
  if (!file.exists("~/.amcatauth")) return()
  rows = read.csv("~/.amcatauth", header=F, stringsAsFactors=F)
  colnames(rows) <- c("host", "username", "password")
  r = rows[rows$host == '*' | rows$host == host,]
  if (nrow(r) > 0) list(username=r$username[1], password=r$password[1]) 
}

#' Get objects from the AmCAT API
#'
#' Get a table of objects from the AmCAT API, e.g. projects, sets etc.
#' 
#' @param conn the connection object from \code{\link{amcat.connect}}
#' @param resource the name of the resource, e.g. 'projects'
#' @param format the format to use, e.g. csv or json. 
#' @param stepsize the number of objects to retrieve per query. Set to a lower number of larger objects
#' @param filters a list of filters to pass to the API, e.g. list(project=1)
#' @param use__in a subset of names(filters) that should be passed with an in argument
#' @return A dataframe of objects (rows) by properties (columns)
#' @export
amcat.getobjects <- function(conn, resource, format='csv', stepsize=50000, filters=list(), use__in=c()) {
  limit = stepsize
  url = paste(conn$host, '/api/v4/', resource, '?format=', format, '&page_size=', limit, sep='')
  if (length(filters) > 0) {
    for (i in 1:length(filters))
      if(names(filters)[i] %in% use__in) {
        url = paste(url, '&', paste(paste(names(filters)[i],filters[[i]],sep='='),collapse='&'), sep='')
      } else url = paste(url, '&', names(filters)[i], '=', filters[[i]], sep='')
  }
  
  httpheader = c(Authorization=paste("Token", conn$token))
  print(paste("Getting objects from", url))
  page = 1
  result = data.frame()
  while(TRUE){
    subresult = getURL(paste(url, '&page=', as.integer(page), sep=''), httpheader=httpheader)
    subresult = .amcat.readoutput(subresult, format=format)
    result = rbind(result, subresult)
    #print(paste("Got",nrow(subresult),"rows, expected", stepsize))
    if(nrow(subresult) < stepsize) break
    page = page + 1
  }
  result
}

#' Internal call to check GET results and parse as csv or json
.amcat.readoutput <- function(result, format){
  if (result == '401 Unauthorized')
    stop("401 Unauthorized")
  if (format == 'json') {
    result = fromJSON(result)
    
  } else  if (format == 'csv') {
    con <- textConnection(result)
    result = tryCatch(read.csv(con), 
                      error=function(e) {warning(e); data.frame()})
  }
  result
}

#' Run an action from the AmCAT API
#'
#' @param conn the connection object from \code{\link{amcat.connect}}
#' @param action the name of the action
#' @param format the format to request, e.g. csv or json
#' @param ... any additional (e.g. action-specific) arguments to pass to the action API
#' @return A dataframe containing the result of the action [WvA: shouldn't that depend on the action??]
#' @export
amcat.runaction <- function(conn, action, format='csv', ...) {
  resource = 'api/action'
  url = paste(conn$host, resource, action, sep="/")
  url = paste(url, '?format=', format, sep="")
  print(paste("Running action at", url))
  httpheader = c(Authorization=paste("Token", conn$token))
  result = postForm(url, ..., .opts=list(httpheader=httpheader))
  
  if (result == '401 Unauthorized')
    stop("401 Unauthorized")
  if (format == 'json') {
    result = fromJSON(result)
  } else  if (format == 'csv') {
    con <- textConnection(result)
    result = read.csv2(con)
  }
  result
}

#' Get article metadata from AmCAT
#'
#' Uses the \code{\link{amcat.getobjects}} function to retrieve article metadata, and applies some
#' additional postprocessing, e.g. to convert the data to Date objects.
#'
#' @param conn the connection object from \code{\link{amcat.connect}}
#' @param sets the article set id(s) to retrieve
#' @param filters additional filters, e.g. list(medium=1)
#' @param columns the names of columns to retrieve
#' @param time if true, parse the date as POSIXct datetime instead of Date
#' @return A dataframe containing the articles and the selected columns
#' @export
amcat.articlemeta <- function(conn, sets, filters=list(), columns=c('id','date','medium','length'), time=F){
  filters[['articleset']] = sets 
  result = amcat.getobjects(conn, "articlemeta", filters=filters, use__in=c('articleset'))
  if(length(columns > 0)) result = result[,columns]
  if ("date" %in% names(result)) {
    result$date = (if(time == T) as.POSIXct(result$date, format='%Y-%m-%d %H:%M:%S') 
                   else as.Date(result$date, format='%Y-%m-%d'))
  }
  return(result)
}