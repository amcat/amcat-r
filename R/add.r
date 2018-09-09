#' Copy articles from one articleset to another
#' 
#' Add the given article ids to an existing article set
#' 
#' @param articleset   The id of the articleset to which the articles will be added
#' @param articles     A numeric vector with article ids
#' @param conn         The connection object from \code{\link{amcat_connect}}. 
#' 
#' @return the articleset id
#' 
#' @export
add_articles <- function(articleset, articles, conn=conn_from_env()) {
  if (is.null(conn)) stop('conn not specified. Either provide conn as argument or run amcat_connect in the current session')
  
  if (is.null(articleset) || !is.numeric(articleset))  stop('Not a valid articleset')
  aset_data = request('articleset', id=articleset)
  project = aset_data$results$project
  r = request(c('projects',project,'articlesets',articleset,'articles'), param=list(articles), conn=conn, post=TRUE) 
  
  invisible(articleset)
}

#amcat_error()

#' Link articlesets to the project
#' 
#' @param project   The id of the project to which the articlesets will be linked
#' @param articles  A numeric vector with article ids
#' @param conn         The connection object from \code{\link{amcat_connect}}. 
#' 
#' @return the articleset id
#' 
#' @export
add_articlesets <- function(articleset, articles, conn=conn_from_env()) {
  if (is.null(conn)) stop('conn not specified. Either provide conn as argument or run amcat_connect in the current session')
  stop('No idea how to fix this in the API... yet')
}

#' Link articlesets to the project
#' 
#' @param project   The id of the project to which the articlesets will be linked
#' @param articles  A numeric vector with article ids
#' @param conn         The connection object from \code{\link{amcat_connect}}. 
#' 
#' @return the articleset id
#' 
#' @export
add_users <- function(project, articles, conn=conn_from_env()) {
  if (is.null(conn)) stop('conn not specified. Either provide conn as argument or run amcat_connect in the current session')
  
  if (is.null(articleset) || !is.numeric(articleset))  stop('Not a valid articleset')
  aset_data = request('articleset', id=articleset)
  project = aset_data$results$project
  r = request(c('projects',project,'articlesets',articleset,'articles'), param=list(articles), conn=conn, post=TRUE) 
  
  invisible(articleset)
}

