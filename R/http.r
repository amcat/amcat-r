#' A wrapper for get_url to get data from multiple pages
#' 
#' @param path the path of the url to retrieve (using the host from conn)
#' @param conn the connection object from \code{\link{amcat_connect}}
#' @param param a named vector of parameters, e.g. c(project=2, articleset=3)
#' @param page_size an integer, specifying the number of items per page 
#' @param verbose if TRUE, report size of data to be retrieved and report progress
#' @param ... additional parameters (param) in name-value pairs
#'
#' @return the raw result
get_pages <- function(path, conn, param=list(), page_size=500, start_page=1, end_page=NA, verbose=T, ...) {
  param[['page_size']] = page_size
  param[['page']] = start_page
  param = c(param, list(...))
  
  ## get start_page, and prepare loop based on page information
  res = get_url(path, conn, param)
  
  if (is.na(end_page) | res$pages < end_page) end_page = res$pages
  if (end_page < start_page) stop('end_page cannot be lower than start_page')
  if (end_page == start_page) {
    if (verbose) message(res$total, ' items / 1 page')
    return(res$results)
  }
  
  result = vector('list', res$pages)
  result[[res$page]] = res$results
  
  if (verbose) {
    if (verbose) message(res$total, ' items / ', res$pages, ' pages. Retrieving page ', start_page, ' to ', end_page)
    pb = utils::txtProgressBar(min = start_page, max = end_page, style = 3)
    pb$up(res$page)
  }
  
  ## loop till no next page, or till end_page is reached
  while(!is.na(res$`next`)) {
    url = httr::parse_url(res['next'])
    res = get_url(url$path, conn=conn, param=url$query)
    result[[res$page]] = res$results
    if (verbose) pb$up(res$page)
    if (res$page == end_page) break
  }
  
  as.data.frame(data.table::rbindlist(result, fill=T))
}

#' Retrieve a single URL from AmCAT with authentication and specified filters (GET or POST) 
#' 
#' @param path the path of the url to retrieve (using the host from conn)
#' @param conn the connection object from \code{\link{amcat_connect}}
#' @param param a named vector of parameters, e.g. c(project=2, articleset=3)
#' @param post use HTTP POST instead of GET
#' @param post_options a list with options for HTTP POST (if post is TRUE) 
#' @param read If TRUE, read the content from the response. Otherwise, return the response object
#' @param ... additional parameters (param) in name-value pairs
#'
#' @return the raw result
get_url <- function(path, conn, param=list(), post=FALSE, post_options=list(), read=T, ...) {
  url = httr::parse_url(conn$host)
  url$path = path
  param = c(param, list(...))
  
  if (!post) {
    url$query = arrange_url_arguments(param, format='rda')
    res = httr::GET(url, get_headers(conn), get_config(conn))  
  } else {  
    json_data = rjson::toJSON(arrange_url_arguments(param))
    res = httr::POST(url, body = json_data, httr::content_type_json(), httr::accept_json(), get_headers(conn), get_config(conn, post_options))  
  }
  if (read) read_response(res) else res
}

arrange_url_arguments <- function(l, ...) {
  l = c(list(...), l)                            ## add optional arguments
  l = l[!sapply(l, is.null)]                     ## drop NULL arguments
  if (is.null(l) | length(l) == 0) return(NULL)  ## return NULL if no arguments
  out = unlist(l, use.names = F)                 ## expand arguments: list(x = c(1,2)) --> list(x=1, x=2)
  names(out) = rep(names(l), sapply(l, length))
  out = as.list(out)
  out[!duplicated(cbind(names(out), out))]       ## remove duplicated name-value pairs
}

load_rda <- function(bytes) {
  e = new.env()
  rconn = rawConnection(bytes)
  load(rconn, envir = e)
  close(rconn)
  as.list(e)
}

read_response <- function(res, only_2xx=T) {
  error_handling(res, only_2xx)
  parse_response(res)
}

parse_response <- function(res) {
  ct = res$headers$`content-type`
  if (is.null(ct)) return(NULL)
  if (grepl('application/x-r-rda', ct)) return(load_rda(res$content))
  if (grepl('application/json', ct)) return(rjson::fromJSON(rawToChar(res$content)))
  return(rawToChar(res$content))
}

error_handling <- function(res, only_2xx) {
  code_class = floor(res$status_code/100)
  if (code_class != 2 && only_2xx){
    res_msg = parse_response(res)
    if (!methods::is(res_msg, 'character')) res_msg = rjson::toJSON(res_msg)
    fn = tempfile()
    write(res_msg, file=fn)
    response = paste("Response written to", fn)
    
    if (code_class == 4) msg = "It looks like you did something wrong, or there is something wrong in the amcat-r library. Please check your command and the error message below, or create an issue at http://github.com/amcat/amcat-r/issues"
    if (code_class == 5) msg = "This seems to be an AmCAT server error. Please see the server logs or create an issue at http://github.com/amcat/amcat/issues"
    
    stop("Unexpected Response Code ", res$status_code, "\n", msg, "\n\n", response)
  }
}

