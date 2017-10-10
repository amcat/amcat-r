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
#' @param remember_token if True, the token will be saved in the package folder for use in future sessions. 
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
amcat_connect <- function(host, username=NULL, token=NULL, disable_ipv6=TRUE, ssl.verifypeer=FALSE, remember_token=FALSE) {
  if (is.null(username) && is.null(token)) stop('Either username or token has to be specified')
  opts = list(ssl.verifypeer = ssl.verifypeer)
  if (disable_ipv6) opts = c(opts, list(ipresolve=1))

  if (is.null(token)) token = get_token_cache(host, username)
  #if (!is.null(token)) {  ## test token
  #                        ## if token does not work, set to NULL to get a new one
  #}
  
  if (is.null(token)) token = get_token(host, username, opts)
  if (remember_token) set_token_cache(host, username, token)

  structure(list(host = host, 
                 token = token, 
                 username = username, 
                 opts = opts),
            class = "amcat_api")
  conn
}

get_token <- function(host, username, opts) {
  passwd = getPass::getPass(paste('Enter AmCAT password for user', username))

  # get auth token
  url = paste(host, '/api/v4/get_token', sep='')
  res = tryCatch(RCurl::postForm(url, username=username, password=passwd, .checkParams=F, .opts=opts), 
                 error=function(e) stop(paste("Could not get token from ",
                                              username,"@", host,
                                              " please check host, username and password. Error: ", e, sep="")))
  rjson::fromJSON(res)$token
}


#' S3 print method for amcat_api (API connection) objects
#'
#' @param x an amcat_api connection object, created with \link{amcat_connect}
#' @param ... not used
#'
#' @method print amcat_api
#' @examples
#' \dontrun{
#' conn = amcat_connect('http://localhost:8000')
#' conn
#' }
#' @export
print.amcat_api <- function(x, ...){
  cat(sprintf('connection to AmCAT server\nhost:\t%s\nuser:\t%s\n', conn$host, conn$username))
}
