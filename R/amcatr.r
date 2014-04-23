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
#' @param disable_ipv6. If True, only use ipv4 resolving (faster if ipv6 causes timeout). Defaults to true, but this may change in the future.
#' @return A list with authentication information that is used by the other functions in this package
#' @export
amcat.connect <- function(host, username=NULL, passwd=NULL, token=NULL, disable_ipv6=TRUE) {
  opts = if (disable_ipv6) list(ipresolve=1) else list()
  
  if (is.null(token)) {
    if (is.null(passwd)) { # try amcatauth file
      a = tryCatch(.readauth(host), error=function(e) warning("Could not read ~/.amcatauth"))
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
    
    res = tryCatch(postForm(url, username=username, password=passwd, .checkParams=F, .opts=opts), 
                   error=function(e) stop(paste("Could not get token from ",
                                                 username,":",passwd,"@", host,
                                                 " please check host, username and password. Error: ", e, sep="")))
    token = fromJSON(res)$token
  }
  list(host=host, token=token, opts=opts)
}

#' Get authentication info from ~/.amcatauth file
.readauth <- function(host) {
  if (!file.exists("~/.amcatauth")) return()
  rows = read.csv("~/.amcatauth", header=F, stringsAsFactors=F)
  colnames(rows) <- c("host", "username", "password")
  r = rows[rows$host == '*' | rows$host == host,]
  if (nrow(r) > 0) list(username=r$username[1], password=r$password[1]) 
}


#' Retrieve a single URL from AmCAT with authentication and specified filters (GET or POST) 
#' 
#' @param conn the connection object from \code{\link{amcat.connect}}
#' @param path the path of the url to retrieve (using the host from conn)
#' @param filters a named vector of filters, e.g. c(project=2, articleset=3)
#' @param post use HTTP POST instead of GET
#' @return the raw result
amcat.getURL <- function(conn, path, filters=NULL, post=FALSE, post_options=list()) {
  httpheader = c(Authorization=paste("Token", conn$token))
  url = paste(conn$host, path, sep="/")
  # strip NULL filters
  for (n in names(filters)) if (is.null(filters[[n]])) filters[[n]] <- NULL
  if (!post) {
    
    # build GET url query
    filters = sapply(1:length(filters), function(i) paste(names(filters)[i], curlEscape(filters[i]), sep="="))
    url = paste(url, paste(filters, collapse="&"), sep="?")
    message("GET ", url)
    getURL(url, httpheader=httpheader, .opts=conn$opts)
  } else {  
    post_opts = modifyList(conn$opts, list(httpheader=httpheader))
    post_opts = modifyList(post_opts, post_options)
    postForm(url, .params=filters, .opts=post_opts)
  }
}

#' Get and rbind pages from the AmCAT API
#' 
#' @param conn the connection object from \code{\link{amcat.connect}}
#' @param path the path of the url to retrieve (using the host from conn)
#' @param filters: a named vector of filters, e.g. c(project=2, articleset=3)
#' @param post use HTTP POST instead of GET
#' @return dataframe 
amcat.getpages <- function(conn, path, format='csv', page=1, page_size=1000, filters=NULL, post=FALSE) {
  filters = c(filters, page_size=page_size, format=format)
  result = data.frame()
  while(TRUE){
    page_filters = c(filters, page=page)
    subresult = amcat.getURL(conn, path, page_filters, post=post)
    if (subresult == "") break
    subresult = .amcat.readoutput(subresult, format=format)
    result = rbind(result, subresult)
    if(nrow(subresult) < page_size) break
    page = page + 1
  }
  result
}

#' Get objects from the AmCAT API
#'
#' Get a table of objects from the AmCAT API, e.g. projects, sets etc.
#' 
#' @param conn the connection object from \code{\link{amcat.connect}}
#' @param resource the name of the resource, e.g. 'projects'. If it is of length>1, a path a/b/c/ will be created (e.g. c("projects",1,"articlesets"))
#' @param ... Other options to pass to \code{\link{amcat.getpages}}, e.g. page_size, format, and filters
#' @return A dataframe of objects (rows) by properties (columns)
#' @export
amcat.getobjects <- function(conn, resource, ...) {
  if (length(resource) > 1) resource = paste(c(resource, ""), collapse="/")
  path = paste('api', 'v4', resource, sep='/')
  amcat.getpages(conn, path, ...)
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
  message("Running action at ", url)
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
#' @param set the article set id to retrieve
#' @param filters additional filters, e.g. list(medium=1)
#' @param columns the names of columns to retrieve
#' @param time if true, parse the date as POSIXct datetime instead of Date
#' @param dateparts if true, add date parts (year, month, week)
#' @param medium_names if true, retrieve medium names and turn medium column into a factor
#' @return A dataframe containing the articles and the selected columns
#' @export
amcat.getarticlemeta <- function(conn, set, filters=list(), columns=c('id','date','medium','length'), time=F, dateparts=F, medium_names=T){
  filters[['articleset']] = set 
  result = amcat.getobjects(conn, "articlemeta", filters=filters)
  if(length(columns > 0)) result = result[,columns]
  if ("date" %in% names(result)) {
    result$date = (if(time == T) as.POSIXct(result$date, format='%Y-%m-%d %H:%M:%S') 
                   else as.Date(result$date, format='%Y-%m-%d'))
  }
  if (dateparts) {
    result$year = as.Date(format(result$date, '%Y-1-1'))
    result$month = as.Date(format(result$date, '%Y-%m-1'))
    result$week = as.Date(paste(format(result$date, '%Y-%W'),1), '%Y-%W %u')
  }
  if (medium_names) {
    media = unique(result$medium)
    # make filter from names by adding names (pk=)
    names(media) = rep("pk", length(media))
    media = amcat.getobjects(conn, "medium", filters=media)
    result$medium = factor(result$medium, levels=media$id, labels=media$name)
  }
  return(result)
}


#' Add articles to an article set
#' 
#' Add the given article ids to a new or existing article set
#' 
#' @param conn the connection object from \code{\link{amcat.connect}}
#' @param project the project to add the articles to
#' @param articles a vector of article ids
#' @param articleset the article set id of an existing set
#' @param articleset.name the name for a new article set
#' @param articleset.provenance a provenance text for a new article set
#' @return The articleset id of the new or existing article set
amcat.add.articles.to.set <- function(conn, project, articles, articleset=NULL,
                                      articleset.name=NULL, articleset.provenance=NULL) {
  if (is.null(articleset)) {
    if (is.null(articleset.name)) 
      stop("Provide articleset or articleset.name")
    path = paste("api", "v4", "projects",project, "articlesets", "?format=json", sep="/")
    if (is.null(articleset.provenance)) 
      articleset.provenance=paste("Uploaded", length(articles), "articles from R on", format(Sys.time(), "%FT%T"))
    r = amcat.getURL(conn, path, filters=list(name=articleset.name, provenance=articleset.provenance), post=TRUE) 
    
    articleset = fromJSON(r)$id
    message("Created articleset ", articleset, ": ", articleset.name," in project ", project)
  }
  idlist = lapply(articles, function(x) list(id=x))
  path = paste("", "api", "v4", "projects",project, "articlesets", articleset, "articles", "", sep="/")
  .amcat.post(conn, path, toJSON(idlist))
  articleset
}


#' Unfortunately, rcurl postform does not allow us to specify the json content-type
#' so we need to use a raw call using make.socket
#' Inspired by httprequest, but its functions don't allow us to specify authentication header :-(
.amcat.post <- function (conn, path, data, expect.status=201) {
  if (grepl(":\\d+$", conn$host)) {
    port = as.numeric(sub(".*:(\\d+)$", "\\1", conn$host))
    host = sub(":\\d+", "", conn$host)
  } else {
    port = 80
    host = conn$host
  }
  # strip http:
  host = sub("^http://", "", host)
  lengthdata <- length(strsplit(data, "")[[1]])
  
  header <- paste("POST ", path, " HTTP/1.1\n", sep = "")
  header <- c(header, paste("Host: ", host, "\n", sep = ""))
  header <- c(header, "Accept: application/json\n")
  header <- c(header, "Content-Type: application/json\n")
  header <- c(header, paste("Authorization: Token ", conn$token, "\n", sep=""))
  header <- c(header, paste("Content-Length: ", lengthdata, "\n"))
  header <- c(header, "Connection: Keep-Alive")
  tosend  <- paste(c(header, "\n\n", data, "\n"), collapse = "")
  fp <- make.socket(host = host, port = port, server = FALSE)
  write.socket(fp, tosend)
  output <- character(0)
  repeat {
    ss <- read.socket(fp, loop = FALSE)
    output <- paste(output, ss, sep = "")
    if (regexpr("\r\n0\r\n\r\n", ss) > -1) 
      (break)()
    if (ss == "") 
      (break)()
  }
  close.socket(fp)
  if (!is.null(expect.status)) {
    # try to interpret output, no idea how robus this really is
    tokens = strsplit(output, "\\s+")[[1]]
    if (expect.status != as.numeric(tokens[2])) {
      status = paste(tokens[1:3], collapse=" ")
      stop(paste("Server returned '", status, "', expected status ", expect.status, sep=""))
    }
  }
  return(output)
}
