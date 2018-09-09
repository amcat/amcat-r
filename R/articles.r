#' Get article meta data from AmCAT
#'
#' Retrieve article data. Be default only returns basic meta data ("id","date","title"),
#' but any available article data can be retrieved.
#' 
#' For working with full texts, it is recommended to use the \link{amcat_corpus} function instead
#' 
#' If "date" is included, it will be returned as either a Date (default) or as a POSIXct (if time is TRUE)
#'
#' @param project the project of a set to retrieve metadata from
#' @param articleset the article set id to retrieve - provide either this or articleset
#' @param articles the article ids to retrieve - provide either this or articleset (only works on latest AmCAT versions)
#' @param uuid like the articles argument, but using the universally unique identifier (uuid)
#' @param columns the names of columns to retrieve, e.g. date, medium, text, headline. If set to NULL, all available columns are retrieved.
#' @param text if TRUE, include the text field (text)
#' @param time if true, parse the date as POSIXct datetime instead of Date
#' @param dateparts if true, add date parts (year, month, week)
#' @param page_size the number of articles per (downloaded) page
#' @param ignore_missing If FALSE, requested columns (in the columns argument) that are not in the data will be added with NA values.
#' @param conn the connection object from \code{\link{amcat_connect}}
#' @param verbose If true, report progress
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
get_articles <- function(project, articleset=NULL, articles=NULL, uuid=NULL, columns=c('date','medium','title'), 
                         text=F, time=F, dateparts=F, page_size=10000, ignore_missing=T, conn=conn_from_env(), verbose=T, ...){
  if (is.null(conn)) stop('conn not specified. Either provide conn as argument or run amcat_connect in the current session')
  if (is.null(articleset) & is.null(articles) & is.null(uuid)) stop("Provide either articleset or articles (ids)/uuids")
  if ('text' %in% columns) text = T
  if (text && !is.null(columns)) columns = union(columns, 'text')
  text = ifelse(text, 1, 0)
  
  if (!is.null(articleset)) {
    result = get_pages(c('projects',project,'articlesets',articleset,'articles'), conn=conn, page_size=page_size, text=text, verbose=verbose, ...)
  } else {
    stop('Getting articles by ID not yet supported')
    if (!is.null(articles)) {
      articles = paste(articles, collapse=",")
      result = get_pages('articles', conn=conn, id=articles, page_size=page_size, text=text, ...)
    } else {
      uuid = paste(uuid, collapse=",")
      result = get_pages('articles', conn=conn, uuid=uuid, page_size=page_size, text=text, ...)
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
      if (ignore_missing) {
        columns = intersect(columns, colnames(result))
      } else {
        for (missing in setdiff(columns, colnames(result))) result[[missing]] <- NA
      }
      result = result[columns]
    }
  }
  
  result
}






