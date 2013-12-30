getcounts <- function(labels, queries, ...) {
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