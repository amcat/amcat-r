#' Connect to the AmCAT API
#'
#' Connect to the AmCAT API and requests a temporary (24h) authentication token that will be stored in the output
#' The host should be known in the ~/.amcatauth file, you can use save_amcat_password to add a password to this file
#' 
#' @param host the hostname, e.g. http://amcat.vu.nl or http://localhost:8000
#' @param token an existing token to authenticate with. If given, username and password are not used and the token is not tested
#' @param disable_ipv6 If True, only use ipv4 resolving (faster if ipv6 causes timeout). Defaults to true, but this may change in the future.
#' @param ssl.verifypeer If True, verifies the authenticity of the peer's certificate
#' @param passwordfile optionally, specify a different password file
#' 
#' @return A list with authentication information that is used by the other functions in this package
#' @examples
#' \dontrun{
#' host = 'https://amcat.nl'  ## existing and available host
#' username = 'XXX'           ## registered username  
#' password = 'XXX'           ## registered password
#' 
#' ## first, store username and password for a given host. 
#' save_amcat_password(host = host, username = username, password = password)
#' 
#' ## connect by just giving the host
#' conn = amcat_connect(host)
#' }
#' @export
amcat_connect <- function(host,token=NULL, disable_ipv6=TRUE, ssl.verifypeer=FALSE,  passwordfile="~/.amcatauth") {
  opts = list(ssl.verifypeer = ssl.verifypeer)
  if (disable_ipv6) opts = c(opts, list(ipresolve=1))
  
  if (is.null(token)) {
      a = tryCatch(readauth(host, passwordfile=passwordfile), error=function(e) warning("Could not read ", passwordfile))
      if (is.null(a)) stop("Cannot find password in ", passwordfile, ", please add an entry to this file by using save_amcat_password!")
      username = a$username
      passwd = a$password
    
    # get auth token
    url = paste(host, '/api/v4/get_token', sep='')
    
    res = tryCatch(RCurl::postForm(url, username=username, password=passwd, .checkParams=F, .opts=opts), 
                   error=function(e) stop(paste("Could not get token from ",
                                                 username,"@", host,
                                                 " please check host, username and password. Error: ", e, sep="")))
    token = rjson::fromJSON(res)$token
    version = rjson::fromJSON(res)$version
    if (is.null(version)) version = "0"
  }
  list(host=host, token=token, version=version, opts=opts)
}

# Get authentication info for a host from password file
readauth <- function(host, passwordfile="~/.amcatauth") {
  if (!file.exists(passwordfile)) return()
  rows = utils::read.csv(passwordfile, header=F, stringsAsFactors=F)
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
#' 
#' @examples
#' \dontrun{
#' host = 'https://amcat.nl'  ## existing and available host
#' username = 'XXX'           ## registered username  
#' password = 'XXX'           ## registered password
#' 
#' save_amcat_password(host = host, username = username, password = password)
#' }
#' @export
save_amcat_password <- function(host, username, password, passwordfile="~/.amcatauth") {
  existing = if (file.exists(passwordfile)) utils::read.csv(passwordfile, header=F, stringsAsFactors=F) else data.frame(V1=character(0),V2=character(0),V3=character(0))
  if (host %in% existing$V1) {
    existing$V2[existing$V1 == host] = username
    existing$V3[existing$V1 == host] = password
  } else {
    existing = rbind(existing, data.frame(V1=host, V2=username, V3=password, stringsAsFactors = F))
  }
  utils::write.table(existing, file=passwordfile, sep=",",  col.names=FALSE, row.names=F)
}

#' Retrieve a single URL from AmCAT with authentication and specified filters (GET or POST) 
#' 
#' @param conn the connection object from \code{\link{amcat_connect}}
#' @param path the path of the url to retrieve (using the host from conn)
#' @param filters a named vector of filters, e.g. c(project=2, articleset=3)
#' @param post use HTTP POST instead of GET
#' @param post_options a list with options for HTTP POST (if post is TRUE) 
#' @param error_unless_200 if TRUE, yield an error if HTTP request is not succesfull (status code 200: OK)
#' @param null_on_404 if TRUE, return NULL if url not found (status code 404: NOT FOUND)
#' @param binary if TRUE, convert with \link{rawToChar}
#'
#' @return the raw result
get_url <- function(conn, path, filters=NULL, post=FALSE, post_options=list(), error_unless_200=TRUE, null_on_404=FALSE, binary=FALSE) {
  httpheader = c(Authorization=paste("Token", conn$token))
  url = httr::parse_url(conn$host)
  url$path = paste(path, sep="/")
  h = RCurl::getCurlHandle()
  # strip NULL filters
  for (n in names(filters)) if (is.null(filters[[n]])) filters[[n]] <- NULL
  if (!post) {
    # convert list(a=c(1,2)) to list(a=1, a=2). From: http://stackoverflow.com/a/22346656
    url$query = structure(do.call(c, lapply(filters, function(z) as.list(z))), names=rep(names(filters), sapply(filters, length)))
    
    # build GET url query
    url = httr::build_url(url)
    message("GET ", url)
    urlfunc = if (binary) RCurl::getBinaryURL else get_url
    result = urlfunc(url, httpheader=httpheader, .opts=conn$opts, curl=h)
    if (RCurl::getCurlInfo(h)$response.code != 200){
      if (error_unless_200 && !(null_on_404 && RCurl::getCurlInfo(h)$response.code == 404)) {
        
        if (!is.null(RCurl::getCurlInfo(h)$content.type) && grepl("application/x-r-rda", RCurl::getCurlInfo(h)$content.type)) {
          result = load_rda(result)
          result = paste(result$exception_type, result$detail, sep = ": ")
        } else {
          if (binary) result = rawToChar(result) 
          fn = tempfile()
          write(result, file=fn)
          result = paste("Response written to", fn)
        }
        error_type = floor(RCurl::getCurlInfo(h)$response.code / 100)
        if (error_type == 5) msg = "This seems to be an AmCAT server error. Please see the server logs or create an issue at http://github.com/amcat/amcat/issues"
        if (error_type == 4) msg = "It looks like you did something wrong, or there is something wrong in the amcat-r library. Please check your command and the error message below, or create an issue at http://github.com/amcat/amcat-r/issues"
        
        stop("Unexpected Response Code ", RCurl::getCurlInfo(h)$response.code, "\n", msg, "\n", result)
      }
      return(NULL)
    }
    result
  } else {  
    
    post_opts = utils::modifyList(conn$opts, list(httpheader=httpheader))    
    post_opts = utils::modifyList(post_opts, post_options)
    RCurl::postForm(httr::build_url(url), .params=filters, .opts=post_opts)
  }
}


#' Get and rbind pages from the AmCAT API
#' 
#' @param conn the connection object from \code{\link{amcat_connect}}
#' @param path the path of the url to retrieve (using the host from conn)
#' @param format a character string giving the output format. Can be 'rda', 'csv' or 'json'. 'rda' is recommended. If NULL (default), 'rda' is used if the amcat version supports it.
#' @param page the page number to start retrieving
#' @param page_size the number of rows per page
#' @param filters a named vector of filters, e.g. c(project=2, articleset=3)
#' @param post use HTTP POST instead of GET
#' @param post_options a list with options for the HTTP POST (if post is TRUE)
#' @param max_page the page number to stop retrieving at, if given
#' @param rbind_results if TRUE, return results of multiple pages as a single data.frame. if FALSE, a list with data.frames is returned
#' 
#' @return dataframe
get_pages <- function(conn, path, format=NULL, page=1, page_size=1000, filters=NULL, 
                           post=FALSE, post_options=list(), max_page=NULL, rbind_results=T) {

  if (is.null(format)) format = if (has_version(conn$version, "3.4.2")) "rda" else "csv" 
  filters = c(filters, page_size=page_size, format=format)
  result = list()
  npages = "?"
  while (TRUE) {
    if (!is.null(max_page)) if (page > max_page) break
    page_filters = c(filters, page=page)
    subresult = get_url(conn, path, page_filters, post=post, post_options, binary = format=="rda", null_on_404 = format != "rda")
    if (format == "rda") {
      res = load_rda(subresult)
      npages = res$pages
      subresult = res$result
      result = c(result, list(subresult))
      if (page >= npages) break
    } else {
      if (is.null(subresult) || subresult == "") break
      subresult = readoutput(subresult, format=format)
      result = c(result, list(subresult))
      if(nrow(subresult) < page_size) break
    }
    message("Retrieved page ",page,"/",npages, "; last page had ", nrow(subresult), " result rows")
    page = page + 1
  }
  #if (rbind_results) result = plyr::rbind.fill(result)
  if (rbind_results) result = as.data.frame(data.table::rbindlist(result))
  result
}

load_rda <- function(bytes) {
  e = new.env()
  c = rawConnection(bytes)
  load(c, envir = e)
  close(c)
  as.list(e)
}
  
read_version <- function(vstr) {
  if (!is.null(vstr)) {
    m = stringr::str_match(vstr, "(\\d+)\\.(\\d+)\\.(\\d+)\\s*")
    if (!is.na(m[[1]]))  {
      v = as.numeric(m[-1])
      names(v) = c("major", "minor", "patch")
      return(v)
    }
  }
  c(major=0, minor=0, patch=0)
}

has_version <- function(actual, required) {
  actual = read_version(actual)
  required = read_version(required)
  
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
#' @param conn the connection object from \code{\link{amcat_connect}}
#' @param resource the name of the resource, e.g. 'projects'. If it is of length>1, a path a/b/c/ will be created (e.g. c("projects",1,"articlesets"))
#' @param ... Other options to pass to \code{\link{get_pages}}, e.g. page_size, format, and filters
#'
#' @return A dataframe of objects (rows) by properties (columns)
get_objects <- function(conn, resource, ...) {
  if (length(resource) > 1) resource = paste(c(resource, ""), collapse="/")
  path = paste('api', 'v4', resource, sep='/')
  get_pages(conn, path, ...)
}

# Internal call to check GET results and parse as csv or json
readoutput <- function(result, format){
  if (result == '401 Unauthorized')
    stop("401 Unauthorized")
  if (format == 'json') {
    result = rjson::fromJSON(result)
    
  } else  if (format == 'csv') {
    con <- textConnection(result)
    result = tryCatch(utils::read.csv(con), 
                      error=function(e) data.frame())
  }
  result
}

#' Get article metadata from AmCAT
#'
#' Uses the \code{\link{get_objects}} function to retrieve article metadata, and applies some
#' additional postprocessing, e.g. to convert the data to Date objects.
#'
#' @param conn the connection object from \code{\link{amcat_connect}}
#' @param project the project of a set to retrieve metadata from
#' @param articleset the article set id to retrieve - provide either this or articleset
#' @param articles the article ids to retrieve - provide either this or articleset
#' @param uuid like the articles argument, but using the universally unique identifier (uuid)
#' @param columns the names of columns to retrieve, e.g. date, medium, text, headline
#' @param time if true, parse the date as POSIXct datetime instead of Date
#' @param dateparts if true, add date parts (year, month, week)
#' @param page_size the number of articles per (downloaded) page
#'
#' @return A dataframe containing the articles and the selected columns
#' 
#' @examples
#' \dontrun{
#' conn = amcat_connect('https://amcat.nl')
#' meta = get_articles(conn, project = 1, articleset = 1)
#' head(meta) 
#' 
#' texts = get_articles(conn, project = 1, articleset = 1, columns = c('text'))
#' texts$text[1]
#' }
#' @export
get_articles <- function(conn, project, articleset=NULL, articles=NULL, uuid=NULL, columns=c('date','medium'), time=F, dateparts=F, page_size=10000){
  if (is.null(articleset) & is.null(articles) & is.null(uuid)) stop("Provide either articleset or articles (ids)/uuids")
  
  if (!is.null(articleset)) {
    path = paste("api", "v4", "projects", project, "articlesets", articleset,  "meta", sep="/")
    result = scroll(conn, path, page_size=page_size, columns=paste(columns, collapse=","))
  } else {
    path = paste("api", "v4", "meta", sep="/")
    if (!is.null(articles)) {
      articles = paste(articles, collapse=",")
      result = scroll(conn, path, id=articles, page_size=page_size, columns=paste(columns, collapse=","))
    } else {
      uuid = paste(uuid, collapse=",")
      result = scroll(conn, path, uuid=uuid, page_size=page_size, columns=paste(columns, collapse=","))
    }
  }
  
  if ("date" %in% colnames(result)) {
    result$date = (if(time == T) as.POSIXct(result$date, format='%Y-%m-%dT%H:%M:%S') 
                   else as.Date(result$date, format='%Y-%m-%d'))
  
    if (dateparts) {
      result$year = as.Date(cut(result$date, "year"))
      result$month = as.Date(cut(result$date, "month"))
      result$week = as.Date(cut(result$date, "week"))
      columns = c(columns, "year", "month", "week")
    }
  }
  columns = c('id', columns)
  if (nrow(result) > 0) {
    for(missing in setdiff(columns, colnames(result))) result[[missing]] <- NA
    result = result[columns]
  }
  
  result
}

#' Add articles to an article set
#' 
#' Add the given article ids to a new or existing article set
#' 
#' @param conn the connection object from \code{\link{amcat_connect}}
#' @param project the project to add the articles to
#' @param articles a vector of article ids
#' @param articleset the article set id of an existing set
#' @param articleset.name the name for a new article set
#' @param articleset.provenance a provenance text for a new article set
#' @return The articleset id of the new or existing article set
#' 
#' @examples
#' \dontrun{
#' conn = amcat_connect('https://amcat.nl')
#' 
#' h = get_hits(conn, queries=c("tyrant OR brute", "saddam"), labels=c("tyrant", "saddam"), sets=10271)
#' articles = h$id[h$query == "tyrant"]
#' 
#' ## make new setwith the (two) documents that mention "tyrant"
#' setid = add_articles_to_set(conn, project=429, articles=articles, articleset.name="New set from howto")
#' 
#' ## set contains only these two documents
#' get_articles(conn, project = 429, articleset=setid)
#' }
#' @export
add_articles_to_set <- function(conn, project, articles, articleset=NULL,
                                      articleset.name=NULL, articleset.provenance=NULL) {
  if (is.null(articleset)) {
    if (is.null(articleset.name)) 
      stop("Provide articleset or articleset.name")
    path = paste("api", "v4", "projects",project, "articlesets", "?format=json", sep="/")
    if (is.null(articleset.provenance)) 
      articleset.provenance=paste("Uploaded", length(articles), "articles from R on", format(Sys.time(), "%FT%T"))
    r = get_url(conn, path, filters=list(name=articleset.name, provenance=articleset.provenance), post=TRUE) 
    
    articleset = rjson::fromJSON(r)$id
    message("Created articleset ", articleset, ": ", articleset.name," in project ", project)
  }
  if (!is.null(articles)) {
    #idlist = lapply(articles, function(x) list(id=x))
    idlist = articles
    url = paste(conn$host, "api", "v4", "projects",project, "articlesets", articleset, "articles", "", sep="/")
    
    resp = httr::POST(url, body=rjson::toJSON(idlist), httr::content_type_json(), httr::accept_json(), httr::add_headers(Authorization=paste("Token", conn$token)))
    if (resp$status_code != 201) stop("Unexpected status: ", resp$status_code, "\n", httr::content(resp, type="text/plain"))
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
#' @param conn the connection object from \code{\link{amcat_connect}}
#' @param project the project to add the articles to
#' @param articleset the article set id of an existing set, or the name of a new set to create
#' @param text the text of the articles to upload
#' @param headline the headlines of the articles to upload
#' @param date the date of the articles to upload
#' @param medium the medium of the articles to upload. 
#' @param provenance if articleset is character, an optional provenance string to store with the new set
#' @param ... and additional fields to upload, e.g. author, byline etc. 
#' 
#' @examples
#' \dontrun{
#' conn = amcat_connect('https://amcat.nl')
#' 
#' set_id = upload_articles(conn, project = 1, articleset = 'example set', 
#'                          headline = 'example', text = 'example',
#'                          date = as.POSIXct('2010-01-01'), medium = 'example')
#'                          
#' get_articles(conn, project = 1, articleset = set_id, 
#'              columns = c('headline','text','date','medium'))
#' }
#' @export
upload_articles <- function(conn, project, articleset, text, headline, date, medium, provenance=NULL, ...) {
  
  n = length(text)
  if (is.character(articleset)) {
    if (is.null(provenance)) provenance=paste("Uploaded", n, "articles using R function upload.articles")
    articleset = add_articles_to_set(conn, project, articles=NULL, articleset.name=articleset, articleset.provenance=provenance) 
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
    json_data = rjson::toJSON(json_data)
    message("Uploading ", nrow(chunk), " articles to set ", articleset)
    
    url = paste(conn$host, "api", "v4", "projects",project, "articlesets", articleset, "articles", "", sep="/")
    
    resp = httr::POST(url, body=json_data, httr::content_type_json(), httr::accept_json(), httr::add_headers(Authorization=paste("Token", conn$token)))
    if (resp$status_code != 201) stop("Unexpected status: ", resp$status_code, "\n", httr::content(resp, type="text/plain"))
  }
  invisible(articleset)
}

#' Scroll an amcat API page with 'next' link
#'
#' @param conn the connection object from \code{\link{amcat_connect}}
#' @param path the path to scroll
#' @param page_size the amount of pages per request
#' @param ... additional query arguments
#'
#' @return a data frame of all returned rows
scroll <- function(conn, path, page_size=100, ...) {
  if (!has_version(conn$version, "3.4.2")) stop("Scrolling only possible on AmCAT >= 3.4.2")
  result = list()
  httpheader = c(Authorization=paste("Token", conn$token))
  url = httr::parse_url(conn$host)
  url$path = path
  url$query = list(page_size=page_size, format="rda", ...)
  url = httr::build_url(url)
  n = 0
  while(!is.na(url)) {
    h = RCurl::getCurlHandle()
    message(url)
    res = RCurl::getBinaryURL(url, httpheader=httpheader, .opts=conn$opts, curl=h)
    if (RCurl::getCurlInfo(h)$response.code != 200) stop("ERROR")
    res = load_rda(res)  
    subresult = res$results
    n = n + nrow(subresult)
    result = c(result, list(subresult))
    message("Got ", nrow(subresult), " rows (total: ",n," / ", res$total,")")
    url = res$`next`
    if (nrow(subresult) < page_size) break
  }
  #plyr::rbind.fill(result)
  as.data.frame(data.table::rbindlist(result))
}


#' Ask AmCAT to flush the elasticsearch
#'
#' @param conn the connection object from \code{\link{amcat_connect}}
flush_elasticsearch <- function(conn) {
  invisible(get_url(conn, "api/v4/flush/", filters=list(format="json")))
}
