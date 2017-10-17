' Get and rbind pages from the AmCAT API
#' 
#' @param path the path of the url to retrieve (using the host from conn)
#' @param conn the connection object from \code{\link{amcat_connect}}
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
get_pages_old <- function(path, conn=conn_from_env(), format=NULL, page=1, page_size=1000, filters=NULL, 
                          post=FALSE, post_options=list(), max_page=NULL, rbind_results=T) {
  if (is.null(conn)) stop('conn not specified. Either provide conn as argument or run amcat_connect in the current session')
  if (is.null(format)) format = "rda" 
  filters = c(filters, page_size=page_size, format=format)
  result = list()
  npages = "?"
  while (TRUE) {
    if (!is.null(max_page)) if (page > max_page) break
    page_filters = c(filters, page=page)
    subresult = get_url(path, conn, page_filters, post=post, post_options, binary = format=="rda", null_on_404 = format != "rda")
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