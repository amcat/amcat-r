#' Get article data from AmCAT
#'
#' Retrieve article data. Be default only returns basic meta data ("id","date","title"),
#' but any available article data can be retrieved.
#' 
#' If "date" is included, it will be returned as either a Date (default) or as a POSIXct (if time is TRUE)
#'
#' @param project the project of a set to retrieve metadata from
#' @param articleset the article set id to retrieve - provide either this or articleset
#' @param articles the article ids to retrieve - provide either this or articleset
#' @param conn the connection object from \code{\link{amcat_connect}}
#' @param uuid like the articles argument, but using the universally unique identifier (uuid)
#' @param columns the names of columns to retrieve, e.g. date, medium, text, headline. If set to NULL, all available columns are retrieved.
#' @param include_all shorthand for including all columns. (this is identical to passing NULL to columns)
#' @param include_text shorthand for including the "headline", "byline" and "text" columns. (this is identical to passing these names to columns)
#' @param time if true, parse the date as POSIXct datetime instead of Date
#' @param dateparts if true, add date parts (year, month, week)
#' @param page_size the number of articles per (downloaded) page
#'
#' @return A data.frame with article columns
#' 
#' @examples
#' \dontrun{
#' conn = amcat_connect('https://amcat.nl', 'username')
#' meta = get_articles(project = 1, articleset = 1)
#' head(meta) 
#' 
#' texts = get_articles(project = 1, articleset = 1, columns = c('text'))
#' texts$text[1]
#' }
#' @export
get_articles <- function(project, articleset=NULL, articles=NULL, conn=conn_from_env(), uuid=NULL, columns=c('id','date','title'), include_all=F, include_text=F, time=F, dateparts=F, page_size=10000, ...){
  if (is.null(conn)) stop('conn not specified. Either provide conn as argument or run amcat_connect in the current session')
  if (is.null(articleset) & is.null(articles) & is.null(uuid)) stop("Provide either articleset or articles (ids)/uuids")
  if (include_all) columns = NULL
  if (include_text) columns = union(columns, c('headline','byline','text'))
  include_text = ifelse('text' %in% columns, 1, 0)
    
  if (!is.null(articleset)) {
    path = paste("api", "v4", "projects", project, "articlesets", articleset,  "articles", sep="/")
    result = get_pages(path, conn=conn, page_size=page_size, text=include_text)
  } else {
    path = paste("api", "v4", "articles", sep="/")
    if (!is.null(articles)) {
      articles = paste(articles, collapse=",")
      result = get_pages(path, conn=conn, id=articles, page_size=page_size, text=include_text)
    } else {
      uuid = paste(uuid, collapse=",")
      result = get_pages(path, conn=conn, uuid=uuid, page_size=page_size, text=include_text)
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
  
  colnames(result) = gsub('properties.', '', colnames(result), fixed=T)
  
  if (!is.null(columns)) {
    columns = union('id', columns)
    if (nrow(result) > 0) {
      for(missing in setdiff(columns, colnames(result))) result[[missing]] <- NA
      result = result[columns]
    }
  }
  
  result
}


#' Add articles to an article set
#' 
#' Add the given article ids to a new or existing article set
#' 
#' @param project the project to add the articles to
#' @param articles a vector of article ids
#' @param conn the connection object from \code{\link{amcat_connect}}
#' @param articleset the article set id of an existing set
#' @param articleset.name the name for a new article set
#' @param articleset.provenance a provenance text for a new article set
#' @return The articleset id of the new or existing article set
#' 
#' @examples
#' \dontrun{
#' conn = amcat_connect('https://amcat.nl', 'username')
#' 
#' h = get_hits(queries=c("tyrant OR brute", "saddam"), labels=c("tyrant", "saddam"), sets=10271)
#' articles = h$id[h$query == "tyrant"]
#' 
#' ## make new setwith the (two) documents that mention "tyrant"
#' setid = add_articles_to_set(project=429, articles=articles, articleset.name="New set from howto")
#' 
#' ## set contains only these two documents
#' get_articles(project = 429, articleset=setid)
#' }
#' @export
add_articles_to_set <- function(project, articles, conn=conn_from_env(), articleset=NULL,
                                articleset.name=NULL, articleset.provenance=NULL) {
  if (is.null(conn)) stop('conn not specified. Either provide conn as argument or run amcat_connect in the current session')
  if (is.null(articleset)) {
    if (is.null(articleset.name)) 
      stop("Provide articleset or articleset.name")
    path = paste("api", "v4", "projects",project, "articlesets", "?format=json", sep="/")
    if (is.null(articleset.provenance)) 
      articleset.provenance=paste("Uploaded", length(articles), "articles from R on", format(Sys.time(), "%FT%T"))
    r = get_url(path, conn, filters=list(name=articleset.name, provenance=articleset.provenance), post=TRUE) 
    
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
  invisible(articleset)
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
#' @param project the project to add the articles to
#' @param articleset the article set id of an existing set, or the name of a new set to create
#' @param text the text of the articles to upload
#' @param headline the headlines of the articles to upload
#' @param date the date of the articles to upload
#' @param medium the medium of the articles to upload. 
#' @param conn the connection object from \code{\link{amcat_connect}}. 
#' @param provenance if articleset is character, an optional provenance string to store with the new set
#' @param ... and additional fields to upload, e.g. author, byline etc. 
#' 
#' @examples
#' \dontrun{
#' conn = amcat_connect('https://amcat.nl', 'username')
#' 
#' set_id = upload_articles(project = 1, articleset = 'example set', 
#'                          headline = 'example', text = 'example',
#'                          date = as.POSIXct('2010-01-01'), medium = 'example')
#'                          
#' get_articles(project = 1, articleset = set_id, 
#'              columns = c('headline','text','date','medium'))
#' }
#' @export
upload_articles <- function(project, articleset, text, headline, date, medium, conn=conn_from_env(), provenance=NULL, ...) {
  if (is.null(conn)) stop('conn not specified. Either provide conn as argument or run amcat_connect in the current session')
  n = length(text)
  if (is.character(articleset)) {
    if (is.null(provenance)) provenance=paste("Uploaded", n, "articles using R function upload.articles")
    articleset = add_articles_to_set(project, conn=conn, articles=NULL, articleset.name=articleset, articleset.provenance=provenance) 
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