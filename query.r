amcat.aggregate <- function(conn, queries, labels=queries, sets, ...) {
  result = NULL
  for (i in 1:length(queries)) {
    r = amcat.getobjects(conn,"aggregate", filters=list(q=URLencode(queries[i]), ...))
    if (nrow(r) > 0) {
      r$query = labels[i]
      result = rbind(result, r)
    }
  }
  return(result)  
}

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