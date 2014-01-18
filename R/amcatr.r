library(rjson)
library(RCurl)

#' Connect to the AmCAT API
#'
#' Connect to the AmCAT API. Currently simply stores the authentication information
#' in a list and performs a trivial call to check that the use can log in.
#' 
#' @param host the hostname, e.g. http://amcat.vu.nl or http://localhost:8000
#' @param username the username to login with, e.g. 'amcat'
#' @param passwd the password to login with, e.g. 'amcat'
#' @param test if True, make a test call to check the password
#' @return A list with authentication information that is used by the other functions in this package
#' @export
amcat.connect <- function(host, username=NULL, passwd=NULL, test=T) {
  if (is.null(username)) {
    cat(paste("Please enter the username for", host, "\n"))
    username=readline()
  }
  if (is.null(passwd)) {
    cat(paste("Please enter the password for ",username,"@", host, "\n", sep=""))
    passwd=readline()
  }
  conn = list(passwd=passwd, username=username, host=host)
  if (test) {
    version = amcat.getobjects(conn, "amcat")$db_version
    cat(paste("Connected to ", username, "@", host, " (db_version=",version,")\n", sep=""))
  }
  conn
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
#' @param test if True, make a test call to check the password
#' @param use__in a subset of names(filters) that should be passed with an in argument
#' @return A dataframe of objects (rows) by properties (columns)
#' @export
amcat.getobjects <- function(conn, resource, format='csv', stepsize=50000, filters=list(), use__in=c(), ...) {
  limit = stepsize
  url = paste(conn$host, '/api/v4/', resource, '?format=', format, '&page_size=', limit, sep='')
  filters = c(filters, list(...))
  if (length(filters) > 0) {
    for (i in 1:length(filters))
      if(names(filters)[i] %in% use__in) {
        url = paste(url, '&', paste(paste(names(filters)[i],filters[[i]],sep='='),collapse='&'), sep='')
      } else url = paste(url, '&', names(filters)[i], '=', filters[[i]], sep='')
  }
  
  opts = list(userpwd=paste(conn$username,conn$passwd,sep=':'), ssl.verifypeer = FALSE, httpauth=1L)
  print(paste("Getting objects from", url))
  page = 1
  result = data.frame()
  while(TRUE){
    subresult = getURL(paste(url, '&page=', as.integer(page), sep=''), .opts=opts)
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
#' @return A dataframe containing the result of the action [WvA: shouldn't that depend on the action??]
#' @export
amcat.runaction <- function(conn, action, format='csv', ...) {
  resource = 'api/action'
  url = paste(conn$host, resource, action, sep="/")
  url = paste(url, '?format=', format, sep="")
  print(paste("Running action at", url))
  opts = list(userpwd=paste(conn$username,conn$passwd,sep=':'), ssl.verifypeer = FALSE, httpauth=1L)
  result = postForm(url, ..., .opts=opts)
  
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
#' @param format the format to request, e.g. csv or json
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