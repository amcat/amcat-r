library(rjson)
library(RCurl)

#' Connect to the AmCAT API
#'
#' Connect to the AmCAT API and requests a temporary (24h) authentication token that will be stored in the output
#' The host should be known in the ~/.amcatauth file, you can use amcat.save.password to add a password to this file
#' 
#' @param host the hostname, e.g. http://amcat.vu.nl or http://localhost:8000
#' @param token an existing token to authenticate with. If given, username and password are not used and the token is not tested
#' @param disable_ipv6. If True, only use ipv4 resolving (faster if ipv6 causes timeout). Defaults to true, but this may change in the future.
#' @return A list with authentication information that is used by the other functions in this package
#' @param passwordfile optionally, specify a different password file
#' @export
amcat.connect <- function(host,token=NULL, disable_ipv6=TRUE, ssl.verifypeer=FALSE,  passwordfile="~/.amcatauth") {
  opts = list(ssl.verifypeer = ssl.verifypeer)
  if (disable_ipv6) opts = c(opts, list(ipresolve=1))
  
  
  if (is.null(token)) {
      a = tryCatch(.readauth(host, passwordfile=passwordfile), error=function(e) warning("Could not read ", passwordfile))
      if (is.null(a)) stop("Cannot find password in ", passwordfile, ", please add an entry to this file by using amcat.save.password!")
      username = a$username
      passwd = a$password
      
    # get auth token
    url = paste(host, '/api/v4/get_token', sep='')
    
    res = tryCatch(postForm(url, username=username, password=passwd, .checkParams=F, .opts=opts), 
                   error=function(e) stop(paste("Could not get token from ",
                                                 username,"@", host,
                                                 " please check host, username and password. Error: ", e, sep="")))
    token = fromJSON(res)$token
    version = fromJSON(res)$version
    if (is.null(version)) version = "0"
  }
  list(host=host, token=token, version=version, opts=opts)
}

#' Get authentication info for a host from password file
.readauth <- function(host, passwordfile="~/.amcatauth") {
  if (!file.exists(passwordfile)) return()
  rows = read.csv(passwordfile, header=F, stringsAsFactors=F)
  colnames(rows) <- c("host", "username", "password")
  r = rows[rows$host == '*' | rows$host == host,]
  if (nrow(r) > 0) list(username=r$username[1], password=r$password[1]) 
}

#' Add or change an entry in the cached authentication file
#' 
#' amcat-r uses the file '~/.amcatauth' to read credentials for connecting to servers
#' to prevent passwords from appearing in script files. This function will update
#' the .amcatauth file (if present) to add or change the password for the given host
#' 
#' @param host the host, e.g. "https://amcat.nl"
#' @param username the AmCAT username to use
#' @param password the password for the AmCAT user
#' @param passwordfile optionally, specify a different password file
#' @export
amcat.save.password <- function(host, username, password, passwordfile="~/.amcatauth") {
  existing = if (file.exists(passwordfile)) read.csv(passwordfile, header=F, stringsAsFactors=F) else data.frame(V1=character(0),V2=character(0),V3=character(0))
  if (host %in% existing$V1) {
    existing$V2[existing$V1 == host] = username
    existing$V3[existing$V1 == host] = password
  } else {
    existing = rbind(existing, data.frame(V1=host, V2=username, V3=password, stringsAsFactors = F))
  }
  write.table(existing, file=passwordfile, sep=",",  col.names=FALSE, row.names=F)
}
#' Retrieve a single URL from AmCAT with authentication and specified filters (GET or POST) 
#' 
#' @param conn the connection object from \code{\link{amcat.connect}}
#' @param path the path of the url to retrieve (using the host from conn)
#' @param filters a named vector of filters, e.g. c(project=2, articleset=3)
#' @param post use HTTP POST instead of GET
#' @return the raw result
#' @export
amcat.getURL <- function(conn, path, filters=NULL, post=FALSE, post_options=list(), error_unless_200=TRUE, null_on_404=FALSE, binary=FALSE) {
  httpheader = c(Authorization=paste("Token", conn$token))
  url = parse_url(conn$host)
  url$path = paste(path, sep="/")
  h = getCurlHandle()
  # strip NULL filters
  for (n in names(filters)) if (is.null(filters[[n]])) filters[[n]] <- NULL
  if (!post) {
    # convert list(a=c(1,2)) to list(a=1, a=2). From: http://stackoverflow.com/a/22346656
    url$query = structure(do.call(c, lapply(filters, function(z) as.list(z))), names=rep(names(filters), sapply(filters, length)))
    
    # build GET url query
    url = build_url(url)
    message("GET ", url)
    urlfunc = if (binary) getBinaryURL else getURL
    result = urlfunc(url, httpheader=httpheader, .opts=conn$opts, curl=h)
    if (getCurlInfo(h)$response.code != 200){
      if (error_unless_200 && !(null_on_404 && getCurlInfo(h)$response.code == 404)) {
        
        if (!is.null(getCurlInfo(h)$content.type) && grepl("application/x-r-rda", getCurlInfo(h)$content.type)) {
          result = .load.rda(result)
          result = paste(result$exception_type, result$detail, sep = ": ")
        } else {
          if (binary) result = rawToChar(result) 
          fn = tempfile()
          write(result, file=fn)
          result = paste("Response written to", fn)
        }
        error_type = floor(getCurlInfo(h)$response.code / 100)
        if (error_type == 5) msg = "This seems to be an AmCAT server error. Please see the server logs or create an issue at http://github.com/amcat/amcat/issues"
        if (error_type == 4) msg = "It looks like you did something wrong, or there is something wrong in the amcat-r library. Please check your command and the error message below, or create an issue at http://github.com/amcat/amcat-r/issues"
        
        stop("Unexpected Response Code ", getCurlInfo(h)$response.code, "\n", msg, "\n", result)
      }
      return(NULL)
    }
    result
  } else {  
    
    post_opts = modifyList(conn$opts, list(httpheader=httpheader))    
    post_opts = modifyList(post_opts, post_options)
    postForm(build_url(url), .params=filters, .opts=post_opts)
  }
}


#' Get and rbind pages from the AmCAT API
#' 
#' @param conn the connection object from \code{\link{amcat.connect}}
#' @param path the path of the url to retrieve (using the host from conn)
#' @param filters a named vector of filters, e.g. c(project=2, articleset=3)
#' @param post use HTTP POST instead of GET
#' @param page the page number to start retrieving
#' @param page_size the number of rows per page
#' @param max_page the page number to stop retrieving at, if given
#' @return dataframe 
#' @export
amcat.getpages <- function(conn, path, format=NULL, page=1, page_size=1000, filters=NULL, 
                           post=FALSE, post_options=list(), max_page=NULL, rbind_results=T) {

  if (is.null(format)) format = if (.has.version(conn$version, "3.4.2")) "rda" else "csv" 
  filters = c(filters, page_size=page_size, format=format)
  result = list()
  npages = "?"
  while (TRUE) {
    if (!is.null(max_page)) if (page > max_page) break
    page_filters = c(filters, page=page)
    subresult = amcat.getURL(conn, path, page_filters, post=post, post_options, binary = format=="rda", null_on_404 = format != "rda")
    if (format == "rda") {
      res = .load.rda(subresult)
      npages = res$pages
      subresult = res$result
      result = c(result, list(subresult))
      if (page >= npages) break
    } else {
      if (is.null(subresult) || subresult == "") break
      subresult = .amcat.readoutput(subresult, format=format)
      result = c(result, list(subresult))
      if(nrow(subresult) < page_size) break
    }
    message("Retrieved page ",page,"/",npages, "; last page had ", nrow(subresult), " result rows")
    page = page + 1
  }
  if (rbind_results) result = rbind.fill(result)
  result
}

.load.rda <- function(bytes) {
  e = new.env()
  c = rawConnection(bytes)
  load(c, envir = e)
  close(c)
  as.list(e)
}
  
.read.version <- function(vstr) {
  if (!is.null(vstr)) {
    m = str_match(vstr, "(\\d+)\\.(\\d+)\\.(\\d+)\\s*")
    if (!is.na(m[[1]]))  {
      v = as.numeric(m[-1])
      names(v) = c("major", "minor", "patch")
      return(v)
    }
  }
  c(major=0, minor=0, patch=0)
}

.has.version <- function(actual, required) {
  actual = .read.version(actual)
  required = .read.version(required)
  
  if (actual["major"] > required["major"]) return(T)
  if (actual["major"] < required["major"]) return(F)
  if (actual["minor"] > required["minor"]) return(T)
  if (actual["minor"] < required["minor"]) return(F)
  return(actual["patch"] >= required["patch"])
}



#' Get objects from the AmCAT API
#'
#' Get a table of objects from the AmCAT API, e.g. projects, sets etc.
#' 
#' @param conn the connection object from \code{\link{amcat.connect}}
#' @param resource the name of the resource, e.g. 'projects'. If it is of length>1, a path a/b/c/ will be created (e.g. c("projects",1,"articlesets"))
#' @param ... Other options to pass to \code{\link{amcat.getpages}}, e.g. page_size, format, and filters
#'
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
                      error=function(e) data.frame())
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
#' @param article_ids the article ids to retrieve
#' @param filters additional filters, e.g. list(medium=1)
#' @param columns the names of columns to retrieve
#' @param time if true, parse the date as POSIXct datetime instead of Date
#' @param dateparts if true, add date parts (year, month, week)
#' @param medium_names if true, retrieve medium names and turn medium column into a factor
#' @param ... additional arguments are passed to \code{\link{amcat.getpages}}. Useful arguments include page_size, page (starting page) and maxpage (end page)
#' @return A dataframe containing the articles and the selected columns
#' @export
amcat.getarticlemeta <- function(conn, set=NULL, article_ids=NULL, filters=list(), columns=c('id','date','medium','length'), time=F, dateparts=F, medium_names=T, page_size=10000, ...){
  if(is.null(set) & is.null(article_ids)) stop('"set" or "article_ids" not specified')
  if(!is.null(set) & !is.null(article_ids)) stop('Either "set" OR "article_ids" has to be specified')
  if(!is.null(set)) {
    filters[['articleset']] = set 
    result = amcat.getobjects(conn, "articlemeta", filters=filters, page_size=page_size, ...)
  }
  if(!is.null(article_ids)) {
    filters[['pk']] = article_ids 
    result = amcat.getobjects(conn, "article", filters=filters, post=T, 
                              post_options = list(httpheader = 'X-HTTP-Method-Override: GET'), ...)
  }

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
  if (medium_names & "medium" %in% columns) {
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
#' @export
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
  if (!is.null(articles)) {
    #idlist = lapply(articles, function(x) list(id=x))
    idlist = articles
    url = paste(conn$host, "api", "v4", "projects",project, "articlesets", articleset, "articles", "", sep="/")
    
    resp = POST(url, body=toJSON(idlist), content_type_json(), accept_json(), add_headers(Authorization=paste("Token", conn$token)))
    if (resp$status_code != 201) stop("Unexpected status: ", resp$status_code, "\n", content(resp, type="text/plain"))
    content(resp)
  }
  articleset
}

#' Upload new articles to AmCAT
#' 
#' Upload articles into a given project and article set, or into a new article set if the articleset argument is character
#' All arguments headline, medium etc. should be either of the same length as text, or of length 1
#' All factor arguments will be converted to character using as.character
#' For date, please provide either a string in ISO notatoin (i.e. "2010-12-31" or "2010-12-31T23:59:00")
#' or a variable that can be converted to string using format(), e.g. Date, POSIXct or POSIXlt. 
#' The articles will be uploaded in batches of 100. 
#' 
#' @param conn the connection object from \code{\link{amcat.connect}}
#' @param project the project to add the articles to
#' @param articleset the article set id of an existing set, or the name of a new set to create
#' @param text the text of the articles to upload
#' @param headline the headlines of the articles to upload
#' @param medium the medium of the articles to upload. 
#' @param provenance if articleset is character, an optional provenance string to store with the new set
#' @param ... and additional fields to upload, e.g. author, byline etc. 
#' @export
amcat.upload.articles <- function(conn, project, articleset, text, headline, date, medium, provenance=NULL, ...) {
  
  n = length(text)
  if (is.character(articleset)) {
    if (is.null(provenance)) provenance=paste("Uploaded", n, "articles using R function amcat.upload.articles")
    articleset = amcat.add.articles.to.set(conn, project, articles=NULL, articleset.name=articleset, articleset.provenance=provenance) 
  }
  
  if (is.factor(date)) date=as.character(date)
  if (!is.character(date)) date = format(date, "%Y-%m-%dT%H:%M:%S")
  fields = data.frame(headline=headline, text=text, date=date, medium=medium, ...)
  # make sure all fields have correct length
  for (f in names(fields)) {
    if (is.factor(fields[[f]])) fields[[f]] = as.character(fields[[f]])
    if (length(fields[[f]]) == 1) fields[[f]] = rep(fields[[f]], n)
    if (length(fields[[f]]) != n) stop(paste("Field", f, "has incorrect length:", length(fields[[f]]), "should be 1 or ", n))
  }
  
  # not very efficient, but probably not the bottleneck
  chunks = split(fields, ceiling((1:n)/100))
  for(chunk in chunks) {
    json_data = vector("list", nrow(chunk))
    for (i in seq_along(json_data)) {
      json_data[[i]] = unlist(lapply(chunk, function(x) x[i]))
    }
    json_data = toJSON(json_data)
    message("Uploading ", nrow(chunk), " articles to set ", articleset)
    
    url = paste(conn$host, "api", "v4", "projects",project, "articlesets", articleset, "articles", "", sep="/")
    
    resp = POST(url, body=json_data, content_type_json(), accept_json(), add_headers(Authorization=paste("Token", conn$token)))
    if (resp$status_code != 201) stop("Unexpected status: ", resp$status_code, "\n", content(resp, type="text/plain"))
  }
  invisible(articleset)
}
