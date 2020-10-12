#' A wrapper for request to get data from multiple pages
#' 
#' @param branch a character vector with the names and values of the API resources. For example, use c('projects', 1, 'articlesets', 10) for the url host/api/v4/projects/1/articlesets/10
#' @param param a named vector of parameters, e.g. c(project=2, articleset=3)
#' @param page_size an integer, specifying the number of items per page 
#' @param as_dt if TRUE, return as data.table (for internal use: more efficient, but confusing for user)
#' @param conn the connection object from \code{\link{amcat_connect}}
#' @param verbose if TRUE, report size of data to be retrieved and report progress
#' @param ... additional parameters (param) in name-value pairs
#'
#' @return The response
#' 
#' @export
get_pages <- function(branch, param=list(), page_size=500, start_page=1, end_page=NA, as_dt=F, conn=conn_from_env(), verbose=T, ...) {
  param[['page_size']] = page_size
  param[['page']] = start_page
  param = c(param, list(...))

  ## get start_page, and prepare loop based on page information
  res = request(branch=branch, param=param, conn=conn)
  if (is.na(end_page) | res$pages < end_page) end_page = res$pages
  if (end_page < start_page) stop('end_page cannot be lower than start_page')
  if (end_page == start_page) {
    #if (verbose) message(res$total, ' items')
    if (as_dt) return(data.table::as.data.table(res$results)) else return(res$results)
  }
  
  result = vector('list', res$pages)
  result[[res$page]] = res$results
  
  if (verbose) {
    #message(res$total, ' items / ', res$pages, ' pages. Retrieving page ', start_page, ' to ', end_page)
    pb = utils::txtProgressBar(min = start_page, max = end_page, style = 3)
    pb$up(res$page)
  }
  
  ## loop till no next page, or till end_page is reached
  while(!is.na(res$`next`)) {
    url = res[['next']]
    res = request(url=url, conn=conn)
    result[[res$page]] = res$results
    if (verbose) pb$up(res$page)
    if (res$page == end_page) break
  }
  pb = utils::txtProgressBar(min = 1, max = 2, style = 3)
  
  result = data.table::rbindlist(result, fill=T)
  if (as_dt) result else as.data.frame(result)
}

#' Retrieve a single URL from AmCAT with authentication and specified filters (GET or POST) 
#' 
#' Either provide the branch and param, or provide a full url with param included (ignoring param). If post is used, it is possible to provide the json_data directly (ignoring param)
#' 
#' @param branch a character vector with the names of the API resources. For example, use c('projects','articlesets') for the url host/api/v4/projects/articlesets
#' @param param a named vector or list of parameters, e.g. c(project=2, articleset=3)
#' @param url   directly provide the url. If used, branch and param argument are ignored
#' @param json_data For sending (post = TRUE) data, directly provide the json body. If used, the param argument is ignored.
#' @param post use HTTP POST instead of GET
#' @param post_options a list with options for HTTP POST (if post is TRUE) 
#' @param conn the connection object from \code{\link{amcat_connect}}
#' @param api the api version
#' @param read If TRUE, read the content from the response. Otherwise, return the response object
#' @param ... additional parameters (param) in name-value pairs
#'
#' @return the response
#' 
#' @export
request <- function(branch=NULL, param=list(), url=NULL, json_data=NULL, post=FALSE, post_options=list(), conn=conn_from_env(), api='api/v4', read=T, ...) {
  param = c(param, list(...))

  if (!post) {
    if (is.null(url)) {
      url = httr::parse_url(conn$host)
      if (!is.null(api)) url$path = paste(api, paste(branch, collapse='/'), sep='/') else paste(branch, collapse='/')
      url$path = paste(api, paste(branch, collapse='/'), sep='/')
      url$query = arrange_url_arguments(param, format='rda')
      url = httr::build_url(url)
    }
    res = httr::GET(url, get_headers(conn), get_config(conn))  
  } else {
    if (is.null(url)) {
      path = if (!is.null(api)) paste(api, paste(branch, collapse='/'), sep='/') else paste(branch, collapse='/')
      url = paste0(conn$host, '/', path, '/?', "format=json")
    }
    if (is.null(json_data)) json_data = jsonlite::toJSON(arrange_url_arguments(param), auto_unbox=T)
    res = httr::POST(url, body = json_data, httr::content_type_json(), httr::accept_json(), get_headers(conn), get_config(conn, post_options))  
  }

  if (read) read_response(res) else res
}

arrange_url_arguments <- function(l=NULL, ...) {
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
  if (grepl('application/json', ct)) return(jsonlite::fromJSON(rawToChar(res$content)))
  return(rawToChar(res$content))
}



error_handling <- function(res, only_2xx) {
  code_class = floor(res$status_code/100)
  cutoff_string <- function(x, n=70) if (nchar(x) > n) paste0(substr(x, 0, n), '...') else x
  if (code_class != 2 && only_2xx){
    res_msg = parse_response(res)
    
    print(res_msg)
    #if (!methods::is(res_msg, 'character')) {
    res_msg_json = jsonlite::toJSON(res_msg)
    fn = tempfile()
    write(res_msg_json, file=fn)

    response = paste0("Full response written to ", fn, '\n', 'Use amcat_error() to view')
    
    res_msg = res_msg[!names(res_msg) %in% c('error','status')]
    response_strings = paste(names(res_msg), 
                             sapply(res_msg, function(x) cutoff_string(paste(x, collapse=' '))),
                             sep=':\t')
    
    msg = paste(response_strings, collapse='\n')
    
    Sys.setenv(AMCAT_ERROR = fn)
    stop("Unexpected Response Code ", res$status_code, "\n", msg, "\n\n", response, call. = F)
  }
}

#' View most recent AmCAT error
#'
#' @export
amcat_error <- function() {
  fn = Sys.getenv('AMCAT_ERROR')  
  jsonlite::fromJSON(fn)
}
