
token_cache_path <- function(){
  ## create path
  path = system.file(package='amcatr')
  if (file.access(path,"6") == -1) stop('Cannot use remember_token, because you do not have write permission for the location of this package')
  path = paste(path, 'ext_resources', sep='/')
  if (!dir.exists(path)) dir.create(path, recursive = T)
  
  ## create file (using ideas from httr package)
  path = paste(path, '.token_cache.rds', sep='/')
  if (!file.exists(path)) {
    file.create(path, showWarnings = FALSE)
    if (!file.exists(path)) stop("Failed to create token cache")
    Sys.chmod(path, "0600")
  }
  
  path
}


get_token_cache <- function(host, username) {
  h = digest::digest(c(host, username))
  path = token_cache_path()
  
  has_cache = !file.info(path, extra_cols = FALSE)$size == 0
  if (has_cache) readRDS(path)[[h]] else NULL
}

set_token_cache <- function(host, username, token) {
  h = digest::digest(c(host, username))
  path = token_cache_path()
  
  has_cache = !file.info(path, extra_cols = FALSE)$size == 0
  tk = if (has_cache) readRDS(path) else list()
  tk[[h]] = token
  saveRDS(tk, file=path)
}

#' Delete saved AmCAT tokens for given host and username
#'
#' @param host the hostname, e.g. http://amcat.vu.nl or http://localhost:8000
#' @param username an existing username on the host server
#'
#' @export
flush_token <- function(host, username) {
  set_token_cache(host, username, NULL)
}

#' Delete all saved AmCAT tokens
#'
#' @export
flush_all_tokens <- function() {
  path = token_cache_path()
  has_cache = !file.info(path, extra_cols = FALSE)$size == 0
  tk = if (has_cache) readRDS(path) else list()
  if (length(tk) > 0) {
    saveRDS(list(), path)
  }
}

