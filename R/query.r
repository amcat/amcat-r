#' Conduct an aggregate query on amcat
#'
#' This function is similar to using the 'show table' function in AmCAT. It allows you to specify a 
#' number of queries and get the number of hits per search term, per period, etc. 
#' 
#' @param conn the connection object from \code{\link{amcat.connect}}
#' @param queries a vector of queries to run
#' @param labels if given, labels corresponding to the queries
#' @param sets one or more article set ids to query on
#' @param ... additional arguments to pass to the AmCAT API. Use axis1 and axis2 to specify 
#'            the grouping, possible values are medium, year/month/week/day. Additional filters
#'            are also possible.
#' @return A data frame with hits per group
#' @export
amcat.aggregate <- function(conn, queries, labels=queries, sets, ...) {
  result = NULL
  queries = as.character(queries)
  for (i in 1:length(queries)) {
    if (!is.na(queries[i])) {
      r = amcat.getobjects(conn,"aggregate", filters=list(q=URLencode(queries[i]), sets=sets, ...))
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
  return(result)  
}

#' Conduct a query on amcat
#'
#' This function is similar to using the 'show article list' function in AmCAT. It allows you to specify a 
#' number of queries and get document metadata and number of hits per document
#' 
#' @param conn the connection object from \code{\link{amcat.connect}}
#' @param queries a vector of queries to run
#' @param labels if given, labels corresponding to the queries
#' @param sets one or more article set ids to query on
#' @param ... additional arguments to pass to the AmCAT API, e.g. extra filters
#' @return A data frame with hits per article
#' @export
amcat.hits <- function(conn, queries, labels=queries, sets, ...) {
  result = NULL
  for (i in 1:length(queries)) {
    q = URLencode(paste("count", queries[i], sep="#"))
    r = amcat.getobjects(conn, "search",filters=list(q=q, col="hits", sets=sets, ...))
    if (nrow(r) > 0) {
      r$query = labels[i]
      result = rbind(result, r)
    } else {
      warning(paste("Query",labels[i]," produced no results"))
    }
  }
  return(result)
}