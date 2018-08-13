#' Get article metadata from AmCAT
#'
#' Uses the \code{\link{get_objects}} function to retrieve article metadata, and applies some
#' additional postprocessing, e.g. to convert the data to Date objects.
#'
#' @param project the project of a set to retrieve metadata from
#' @param articleset the article set id to retrieve - provide either this or articleset
#' @param articles the article ids to retrieve - provide either this or articleset
#' @param conn the connection object from \code{\link{amcat_connect}}
#' @param uuid like the articles argument, but using the universally unique identifier (uuid)
#' @param text_columns The columns that contain the article text. If multiple columns are given (e.g., headline, text), they are pasted together (separated by a double linebreak)
#' @param meta_columns the names of columns to retrieve, e.g. date, medium, text, headline
#' @param time if true, parse the date as POSIXct datetime instead of Date
#' @param dateparts if true, add date parts (year, month, week)
#' @param page_size the number of articles per (downloaded) page
#' @param format determine whether article data is returned as a "data.frame", "quanteda_corpus" (requires quanteda package) or "tcorpus" (requires corpustools package). 
#' @param ... additional arguments are passed to quanteda::corpus or corpustools::create_tcorpus. 
#
#'
#' @return
#' @export
#'
#' @examples
get_corpus <- function(project, articleset=NULL, articles=NULL, conn=conn_from_env(), uuid=NULL, text_columns = c('headline','byline','text'), meta_columns=NULL, time=F, dateparts=F, page_size=1000, format = c('quanteda_corpus','tcorpus','data.frame'), ...){
  format = match.arg(format)
  if (!is.null(meta_columns)) meta_columns = union(meta_columns, text_columns)
  result = get_articles(project=project, articleset=articleset, articles=articles, conn=conn, uuid=uuid, columns=meta_columns, time=time, dateparts=dateparts, page_size=page_size, text_columns=text_columns)
  if (format == 'quanteda_corpus') result = as_quanteda_corpus(result, text_columns, ...)  
  if (format == 'tcorpus') result = as_tcorpus(result, text_columns, ...)
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
  quanteda::corpus(as.character(text), docnames=articles$id, docvars=articles[metavars], ...)
}

as_tcorpus <- function(articles, text_columns, ...) {
  if(!requireNamespace('corpustools', quietly = T)) stop('To use this function, first install the quanteda package.')
  text_columns = intersect(colnames(articles), text_columns)
  corpustools::create_tcorpus(articles, doc_column = 'id', text_columns=text_columns, ...)
}