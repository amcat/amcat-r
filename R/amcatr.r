


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
#' @param format a character string giving the output format. Can be 'rda', 'csv' or 'json'. 'rda' is recommended. If NULL (default), 'rda' is used.
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

  if (is.null(format)) format = "rda" 
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
    m = stringr::str_match(vstr, "[a-zA-Z]*(\\d+)\\.[a-zA-Z]*(\\d+)\\.[a-zA-Z]*(\\d+)\\s*") ## the alpha match is for development notes
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
#' @param return_as determine whether article data is returned as a "data.frame", "quanteda_corpus" (requires quanteda package) or "tcorpus" (requires corpustools package). 
#' @param text_columns if return_as is a corpus (either quanteda_corpus or tcorpus), the columns that contain the article text need to be specified. If multiple columns are given (e.g., headline, text), they are pasted together (separated by a double linebreak)
#' @param ... if return_as is a corpus (either quanteda_corpus or tcorpus), additional arguments are passed to quanteda::corpus or corpustools::create_tcorpus. 
#'
#' @return The article data in the format specified with the return_as parameter
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
get_articles <- function(conn, project, articleset=NULL, articles=NULL, uuid=NULL, columns=c('date','medium'), time=F, dateparts=F, page_size=10000, return_as = c('data.frame','quanteda_corpus','tcorpus'), text_columns = c('headline','text'), ...){
  if (is.null(articleset) & is.null(articles) & is.null(uuid)) stop("Provide either articleset or articles (ids)/uuids")
  return_as = match.arg(return_as)
    
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
  
  if (return_as == 'quanteda_corpus') result = as_quanteda_corpus(result, text_columns, ...)  
  if (return_as == 'tcorpus') result = as_tcorpus(result, text_columns, ...)
  result
}

as_quanteda_corpus <- function(articles, text_columns = c('headline','text'), ...){
  if(!requireNamespace('quanteda', quietly = T)) stop('To use this function, first install the quanteda package.')
  text_columns = intersect(colnames(articles), text_columns)
  metavars = setdiff(colnames(articles), text_columns)
  
  if (length(text_columns) > 1){
    text = do.call(paste, c(as.list(articles[,text_columns]), sep='\n\n'))
  } else {
    text = articles[[text_columns]]
  }
  quanteda::corpus(texts, docnames=articles$id, docvars=articles[metavars], ...)
}

as_tcorpus <- function(articles, text_columns, ...) {
  if(!requireNamespace('corpustools', quietly = T)) stop('To use this function, first install the quanteda package.')
  text_columns = intersect(colnames(articles), text_columns)
  corpustools::create_tcorpus(articles, doc_column = 'id', text_columns=text_columns, ...)
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
    print(RCurl::getCurlInfo(h)$response.code)
    if (RCurl::getCurlInfo(h)$response.code != 200) stop("ERROR")
    res = load_rda(res)  
    subresult = res$results
    n = n + nrow(subresult)
    result = c(result, list(subresult))
    message("Got ", nrow(subresult), " rows (total: ",n," / ", res$total,")")
    url = res$`next`
    if (nrow(subresult) < page_size) break
  }
  as.data.frame(data.table::rbindlist(result))
}


#' Ask AmCAT to flush the elasticsearch
#'
#' @param conn the connection object from \code{\link{amcat_connect}}
flush_elasticsearch <- function(conn) {
  invisible(get_url(conn, "api/v4/flush/", filters=list(format="json")))
}
