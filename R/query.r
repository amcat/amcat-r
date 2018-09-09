#' Perform a query on AmCAT
#'
#' This function is similar to using the 'show article list' function in AmCAT. It allows you to specify a
#' number of queries and get document metadata and number of hits per document.
#' 
#' The output will contain a column for each query. The column names can be specified with the labels argument, 
#' or by adding the label to the query itself, separated with a hashtag: "label# query". 
#' If no labels are given, the queries themselves are used as column names (up to 25 characters)
#' 
#' @param project    The project id
#' @param articleset One or more article set ids to query on. If NULL, all sets in the project will be used.
#' @param queries    A character vector with queries. Default is the special case "*" which finds anything and is automatically given the label "total".
#' @param labels     If given, labels corresponding to the queries. Alternatively, if a query starts with a label and a hashtag (e.g., "label# term1 AND term2"), this label is used.
#' @param start_date  Search only from given date. Must be either a date type (Date, POSIXct or POSIXlt) or a 
#' @param hits       If TRUE (default) return number of hits per query per documents. If FALSE, only return documents.
#' @param columns    Optionally, request additional fields (e.g., date, title, ) to be included as columns. 
#' @param conn t     The connection object from \code{\link{amcat_connect}}
#' @param verbose    If TRUE, report progress
#' @param ...        additional arguments to pass to the AmCAT API
#' 
#' @return A data frame with hits per article
#' 
#' @examples 
#' \dontrun{
#' conn = amcat_connect('https://amcat.nl')
#' h = get_hits(queries=c("tyrant OR brute", "saddam"), labels=c("tyrant", "saddam"), sets=10271)
#' head(h)
#' table(h$query)
#' }
#' @export
get_hits <- function(project, articleset, queries='*', labels=substr(queries,0,25), start_date=NULL, end_date=NULL, hits=T, columns=NULL, conn=conn_from_env(), verbose=T, ...) {
  if (is.null(conn)) stop('conn not specified. Either provide conn as argument or run amcat_connect in the current session')
  if (is.null(articleset)) stop('articleset cannot be NULL')
  
  columns = union('id', columns)
  if (hits) columns = c('hits', columns)
  
  start_date = prepare_query_date(start_date, 'start_date')  
  end_date = prepare_query_date(end_date, 'end_date')  
  
  ## set labels
  label = ifelse(grepl('#', queries), gsub('#.*', '', queries), labels) 
  if (label == '*') label = 'total'
  label[label == 'hits'] = "$$HITS_LABEL$$" ## placeholder
  if (any(label %in% columns)) stop('Labels cannot have the same name as field columns')
  dups = duplicated(label)
  if (any(dups)) stop(sprintf('Labels must be unique. Current duplicates: %s', paste(label[dups], collapse=',')))
  queries = gsub('.*#', '', queries)
  dummy_label = paste0('QUERY', 1:length(queries))
  queries = paste0(dummy_label, '# ', queries)
  
  ## get results
  result = get_pages('search', conn=conn, q=queries, col=columns, project=project, sets=articleset, start_date=start_date, end_date=end_date, as_dt=T, verbose = T, ...)
  ## arrange results
  if ('hits' %in% colnames(result)) result$hits = NULL
  data.table::setnames(result, old = dummy_label, new = label)
  data.table::setcolorder(result, c(intersect(columns, colnames(result)), label))
  if ('$$HITS_LABEL$$' %in% colnames(result)) data.table::setnames(result, old = '$$HITS_LABEL$$', new = 'hits')
  
  as.data.frame(result)
}


prepare_query_date <- function(date, columnname) {
  if (is.null(date)) return(NULL)
  date_classes = c('character','factor','Date','POSIXlt','POSIXct')
  if (!any(sapply(date_classes, function(x) is(date, x)))) stop(paste0('date type column (', columnname, ') must either a date type vector (Date, POSIXct or POSIXlt) or character type in format "%Y-%m-%d" or "%Y-%m-%d %H:%S:%M"'))
  if (is(date, 'factor')) date = as.character(date)
  if (is(date, 'character')) {
    date = as.POSIXct(date)
    if (any(is.na(date))) stop(paste0(columnname, ' must either a date type vector (Date, POSIXct or POSIXlt) or character type in format "%Y-%m-%d" or "%Y-%m-%d %H:%S:%M"'))
  }
  format(date, '%d-%m-%Y')
}
