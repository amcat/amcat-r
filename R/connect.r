#' Connect to the AmCAT API
#'
#' Connect to the AmCAT API and requests a temporary (24h) authentication token that will be stored in the output
#' The host should be known in the ~/.amcatauth file, you can use save_amcat_password to add a password to this file
#' 
#' @param host the hostname, e.g. http://amcat.vu.nl or http://localhost:8000
#' @param username an existing username on the host server
#' @param token an existing token to authenticate with. If given, username is not used and the token is not tested
#' @param disable_ipv6 If True, only use ipv4 resolving (faster if ipv6 causes timeout). Defaults to true, but this may change in the future.
#' @param ssl.verifypeer If True, verifies the authenticity of the peer's certificate
#' @param conn an existing AmCAT connection object. If provided, the connection will be opened (if not yet open) and refreshed (i.e. new token). The other arguments are ignored.
#' 
#' @return A list with authentication information that is used by the other functions in this package
#' @examples
#' \dontrun{
#' host = 'https://amcat.nl'  ## existing and available host
#' username = 'XXX'           ## registered username  
#' password = 'XXX'           ## registered password
#' 
#' ## first, store username and password for a given host. 
#' save_amcat_password(host = host, username = username, password = password)
#' 
#' ## connect by just giving the host
#' conn = amcat_connect(host)
#' }
#' @export
amcat_connect <- function(host=NULL, username=NULL, token=NULL, disable_ipv6=TRUE, ssl.verifypeer=FALSE, conn=NULL) {
  if (is.null(conn)) {
    conn = structure(list(host = host, 
                          username = username,
                          token = token, 
                          ssl.verifypeer = ssl.verifypeer,
                          disable_ipv6 = disable_ipv6),
                     class = "amcat_connection")
  }
  conn$token = get_token(conn)
  
  conn_to_env(conn)
  invisible(conn)
}

get_opts <- function(conn) {
  opts = list(ssl.verifypeer = conn$ssl.verifypeer)
  if (!is.null(conn$token)) opts = c(opts, list(httpheader = c(Authorization=paste("Token", conn$token))))
  if (conn$disable_ipv6) opts = c(opts, list(ipresolve=1))
  opts
}

#' Save AmCAT connection as environment variable
#'
#' @param conn An AmCAT connection, as created with \link{amcat_connect}
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' conn = amcat_connect('localhost:8000', 'username')
#' conn_to_env(conn)
#' }
conn_to_env <- function(conn) {
  if(!methods::is(conn, 'amcat_connection')) stop("conn is not an amcat_connection object")
  Sys.setenv(AMCAT_CONN = rjson::toJSON(conn))
}


#' Obtain AmCAT connection from environment
#'
#' ! This only works if \link{amcat_connect} has been used in the current session.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' conn = conn_from_env()
#' }
conn_from_env <- function(){
  amcat_conn = Sys.getenv('AMCAT_CONN')
  if (amcat_conn == '') return(NULL)
  structure(rjson::fromJSON(amcat_conn),
            class = 'amcat_connection')
}

get_token <- function(conn) {
  url = paste(conn$host, '/api/v4/get_token', sep='')

  if (!is.null(conn$token)) {
    res = tryCatch(RCurl::postForm(url, username=conn$username, password='', .checkParams=F, .opts=get_opts(conn)), 
                   error= function(e) NULL)
    if (is.null(res)) {
      message('Token is not valid or expired. Please re-enter password')
      conn$token = NULL
    }
  }
  if (is.null(conn$token)) {
    passwd = getPass::getPass(paste('Enter AmCAT password for user', conn$username))
    res = tryCatch(RCurl::postForm(url, username=conn$username, password=passwd, .checkParams=F, .opts=get_opts(conn)), 
                 error= function(e) stop(paste("Could not get token for ", conn$username,"@", conn$host, " please check host, username and password")))
  }
  
  rjson::fromJSON(res)$token
}



#' S3 print method for amcat_connection (API connection) objects
#'
#' @param x an amcat_connection connection object, created with \link{amcat_connect}
#' @param ... not used
#'
#' @method print amcat_connection
#' @examples
#' \dontrun{
#' conn = amcat_connect('http://localhost:8000')
#' conn
#' }
#' @export
print.amcat_connection <- function(x, ...){
  cat(sprintf('connection to AmCAT server\nhost:\t%s\nuser:\t%s\n', x$host, x$username))
}
