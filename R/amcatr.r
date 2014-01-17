library(rjson)
library(RCurl)

amcat.connect <- function(host, username=NULL, passwd=NULL, test=T) {
  if (is.null(username)) {
    cat(paste("Please enter the username for", host, "\n"))
    username=readline()
  }
  if (is.null(passwd)) {
    cat(paste("Please enter the password for ",username,"@", host, "\n", sep=""))
    passwd=readline()
  }
  conn = list(passwd=passwd, username=username, host=host)
  if (test) {
    version = amcat.getobjects(conn, "amcat")$db_version
    cat(paste("Connected to ", username, "@", host, " (db_version=",version,")\n", sep=""))
  }
  conn
}

amcat.getobjects <- function(conn, resource, format='csv', stepsize=50000, filters=list(), use__in=c(), ...) {
  limit = stepsize
  url = paste(conn$host, '/api/v4/', resource, '?format=', format, '&page_size=', limit, sep='')
  filters = c(filters, list(...))
  if (length(filters) > 0) {
    for (i in 1:length(filters))
      if(names(filters)[i] %in% use__in) {
        url = paste(url, '&', paste(paste(names(filters)[i],filters[[i]],sep='='),collapse='&'), sep='')
      } else url = paste(url, '&', names(filters)[i], '=', filters[[i]], sep='')
  }
  
  opts = list(userpwd=paste(conn$username,conn$passwd,sep=':'), ssl.verifypeer = FALSE, httpauth=1L)
  print(paste("Getting objects from", url))
  page = 1
  result = data.frame()
  while(TRUE){
    subresult = getURL(paste(url, '&page=', as.integer(page), sep=''), .opts=opts)
    subresult = .amcat.readoutput(subresult, format=format)
    result = rbind(result, subresult)
    #print(paste("Got",nrow(subresult),"rows, expected", stepsize))
    if(nrow(subresult) < stepsize) break
    page = page + 1
  }
  result
}

.amcat.readoutput <- function(result, format){
  if (result == '401 Unauthorized')
    stop("401 Unauthorized")
  if (format == 'json') {
    result = fromJSON(result)
    
  } else  if (format == 'csv') {
    con <- textConnection(result)
    result = tryCatch(read.csv(con), 
                      error=function(e) {warning(e); data.frame()})
  }
  result
}


amcat.runaction <- function(conn, action, format='csv', ...) {
  resource = 'api/action'
  url = paste(conn$host, resource, action, sep="/")
  url = paste(url, '?format=', format, sep="")
  print(paste("Running action at", url))
  opts = list(userpwd=paste(conn$username,conn$passwd,sep=':'), ssl.verifypeer = FALSE, httpauth=1L)
  result = postForm(url, ..., .opts=opts)
  
  if (result == '401 Unauthorized')
    stop("401 Unauthorized")
  if (format == 'json') {
    result = fromJSON(result)
  } else  if (format == 'csv') {
    con <- textConnection(result)
    result = read.csv2(con)
  }
  result
}

amcat.articlemeta <- function(conn, articlesets, media=c(), from_date=NA, to_date=NA, filter_columns=c('id','date','medium','length'), time=F){
  meta = NULL
  if(length(media) == 0) media = c(NA)
  
  for(medium in media) {
    filters = list(articleset=articlesets)
    if(!is.na(medium)) filters = c(filters, medium=medium)
    if(!is.na(from_date)) filters = c(filters, date_from=from_date)
    if(!is.na(to_date)) filters = c(filters, date_to=to_date)
    
    ameta = amcat.getobjects(conn, "articlemeta", filters=filters, use__in=c('articleset'))
    if(nrow(ameta) == 0) next
    if(length(filter_columns > 0)) ameta = ameta[,filter_columns]
    meta = rbind(meta,ameta)
  }
  if(time == T) meta$date = as.POSIXct(meta$date, format='%Y-%m-%d %H:%M:%S') else meta$date = as.Date(meta$date, format='%Y-%m-%d')
  meta
}