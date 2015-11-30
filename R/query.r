#' Conduct an aggregate query on amcat
#'
#' This function is similar to using the 'show table' function in AmCAT. It allows you to specify a
#' number of queries and get the number of hits per search term, per period, etc.
#'
#' @param conn the connection object from \code{\link{amcat.connect}}
#' @param queries a vector of queries to run
#' @param labels if given, labels corresponding to the queries
#' @param sets one or more article set ids to query on
#' @param axis1 The first grouping (break/group by) variable, e.g. year, month, week, day, or medium
#' @param axis2 The second grouping (break/group by) variable, e.g. medium. Do not use a date interval here.
#' @param ... additional arguments to pass to the AmCAT API. 
#' @return A data frame with hits per group
#' @export
amcat.aggregate <- function(conn, queries, labels=queries, sets, axis1=NULL, axis2=NULL, ...) {
  result = NULL
  queries = as.character(queries)
  for (i in 1:length(queries)) {
    if (!is.na(queries[i])) {
      
      r = tryCatch(amcat.getobjects(conn,"aggregate", filters=list(q=queries[i], sets=sets, axis1=axis1, axis2=axis2, ...)),
                   error=function(e) {warning("Error on querying '", labels[i], "': ", e$message); NULL})
      if (is.null(r)) next
      if (nrow(r) > 0) {
        if (names(r)[1] == "count") {
          r$query = labels[i]
          result = rbind(result, r)
        } else {
          warning(paste("Error on querying",labels[i]))
        }
      }
    }
  }  
  # convert axis1 to Date object if needed
  if (!is.null(axis1))
    if (axis1 %in% c("year", "quarter", "month", "week", "day")) result[, axis1] = as.Date(result[, axis1])
  return(result)
}

#' Conduct a query on amcat
#'
#' This function is similar to using the 'show article list' function in AmCAT. It allows you to specify a
#' number of queries and get document metadata and number of hits per document
#'
#' @param conn the connection object from \code{\link{amcat.connect}}
#' @param queries a vector of queries to run
#' @param labels if given, labels corresponding to the queries. Alternatively, if a query starts with a label and a hashtag (e.g., label# "term1 term2"), this label is used.
#' @param sets one or more article set ids to query on
#' @param ... additional arguments to pass to the AmCAT API, e.g. extra filters
#' @return A data frame with hits per article
#' @export
amcat.hits <- function(conn, queries, labels=queries, sets,  minimal=T, warn.no.result=T, ...) {
  result = NULL
  
  for (i in 1:length(queries)) {
    q = paste("count", queries[i], sep="#")
    r = tryCatch(amcat.getobjects(conn, "search",filters=list(q=q, col="hits", sets=sets, minimal=minimal, ...)),
                 error=function(e) {warning("Error on querying '", labels[i], "': ", e$message); NULL})
    if (is.null(r)) next
    
    label = if(grepl('#', queries[i])) gsub('#.*', '', queries[i]) else labels[i] 
    if (nrow(r) > 0) {
      r$query = label
      result = rbind(result, r)
    } else {
      if (warn.no.result) warning(paste("Query",label," produced no results"))
    }
  }
  return(result)
}
