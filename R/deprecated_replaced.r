###########################################################
####################### amcat.r ###########################

#' Connect to the AmCAT API
#'
#' Connect to the AmCAT API and requests a temporary (24h) authentication token that will be stored in the output
#' The host should be known in the ~/.amcatauth file, you can use save_amcat_password to add a password to this file
#' 
#' @param host the hostname, e.g. http://amcat.vu.nl or http://localhost:8000
#' @param token an existing token to authenticate with. If given, username and password are not used and the token is not tested
#' @param disable_ipv6 If True, only use ipv4 resolving (faster if ipv6 causes timeout). Defaults to true, but this may change in the future.
#' @param ssl.verifypeer If True, verifies the authenticity of the peer's certificate
#' @param passwordfile optionally, specify a different password file
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
amcat.connect <- function(host, token=NULL, disable_ipv6=TRUE, ssl.verifypeer=FALSE,  passwordfile="~/.amcatauth") {
  .Deprecated('amcat_connect')
  
  opts = list(ssl.verifypeer = ssl.verifypeer)
  if (disable_ipv6) opts = c(opts, list(ipresolve=1))
  
  if (is.null(token)) {
    a = tryCatch(readauth(host, passwordfile=passwordfile), error=function(e) warning("Could not read ", passwordfile))
    if (is.null(a)) stop("Cannot find password in ", passwordfile, ", please add an entry to this file by using save_amcat_password!")
    username = a$username
    passwd = a$password
    
    # get auth token
    url = paste(host, '/api/v4/get_token', sep='')
    
    res = tryCatch(RCurl::postForm(url, username=username, password=passwd, .checkParams=F, .opts=opts), 
                   error=function(e) stop(paste("Could not get token from ",
                                                username,"@", host,
                                                " please check host, username and password. Error: ", e, sep="")))
    token = rjson::fromJSON(res)$token
    version = rjson::fromJSON(res)$version
    if (is.null(version)) version = "0"
  }
  list(host=host, token=token, version=version, opts=opts)
}

#' Add or change an entry in the cached authentication file
#' 
#' amcat-r uses the file '~/.amcatauth' to read credentials for connecting to servers
#' to prevent passwords from appearing in script files. This function will update
#' the .amcatauth file (if present) to add or change the password for the given host
#' 
#' @param host the host, e.g. "https://amcat.nl"
#' @param username the AmCAT username to use
#' @param password the password for the AmCAT user
#' @param passwordfile optionally, specify a different password file
#' 
#' @examples
#' \dontrun{
#' host = 'https://amcat.nl'  ## existing and available host
#' username = 'XXX'           ## registered username  
#' password = 'XXX'           ## registered password
#' 
#' save_amcat_password(host = host, username = username, password = password)
#' }
#' @export
amcat.save.password <- function(host, username, password, passwordfile="~/.amcatauth") {
  .Deprecated('save_amcat_password')
  
  existing = if (file.exists(passwordfile)) utils::read.csv(passwordfile, header=F, stringsAsFactors=F) else data.frame(V1=character(0),V2=character(0),V3=character(0))
  if (host %in% existing$V1) {
    existing$V2[existing$V1 == host] = username
    existing$V3[existing$V1 == host] = password
  } else {
    existing = rbind(existing, data.frame(V1=host, V2=username, V3=password, stringsAsFactors = F))
  }
  utils::write.table(existing, file=passwordfile, sep=",",  col.names=FALSE, row.names=F)
}

#' Get article metadata from AmCAT
#'
#' Uses the \code{\link{get_objects}} function to retrieve article metadata, and applies some
#' additional postprocessing, e.g. to convert the data to Date objects.
#'
#' @param conn the connection object from \code{\link{amcat_connect}}
#' @param project the project of a set to retrieve metadata from
#' @param articleset the article set id to retrieve - provide either this or articleset
#' @param articles the article ids to retrieve - provide either this or articleset
#' @param uuid like the articles argument, but using the universally unique identifier (uuid)
#' @param columns the names of columns to retrieve, e.g. date, medium, text, headline
#' @param time if true, parse the date as POSIXct datetime instead of Date
#' @param dateparts if true, add date parts (year, month, week)
#' @param page_size the number of articles per (downloaded) page
#' @return A dataframe containing the articles and the selected columns
#' 
#' @examples
#' \dontrun{
#' conn = amcat_connect('https://amcat.nl')
#' meta = amcat.articles(conn, project = 1, articleset = 1)
#' head(meta) 
#' 
#' texts = amcat.articles(conn, project = 1, articleset = 1, columns = c('text'))
#' texts$text[1]
#' }
#' @export
amcat.articles <- function(conn, project, articleset=NULL, articles=NULL, uuid=NULL, columns=c('date','medium'), time=F, dateparts=F, page_size=10000){
  .Deprecated('get_articles')
  
  if (is.null(articleset) & is.null(articles) & is.null(uuid)) stop("Provide either articleset or articles (ids)/uuids")
  
  if (!is.null(articleset)) {
    path = paste("api", "v4", "projects", project, "articlesets", articleset,  "meta", sep="/")
    result = scroll(conn, path, page_size=page_size, columns=paste(columns, collapse=","))
  } else {
    path = paste("api", "v4", "meta", sep="/")
    if (!is.null(articles)) {
      articles = paste(articles, collapse=",")
      result = scroll(conn, path, id=articles, page_size=page_size, columns=paste(columns, collapse=","))
    } else {
      uuid = paste(uuid, collapse=",")
      result = scroll(conn, path, uuid=uuid, page_size=page_size, columns=paste(columns, collapse=","))
    }
  }
  
  if ("date" %in% colnames(result)) {
    result$date = (if(time == T) as.POSIXct(result$date, format='%Y-%m-%dT%H:%M:%S') 
                   else as.Date(result$date, format='%Y-%m-%d'))
    
    if (dateparts) {
      result$year = as.Date(cut(result$date, "year"))
      result$month = as.Date(cut(result$date, "month"))
      result$week = as.Date(cut(result$date, "week"))
      columns = c(columns, "year", "month", "week")
    }
  }
  columns = c('id', columns)
  if (nrow(result) > 0) {
    for(missing in setdiff(columns, colnames(result))) result[[missing]] <- NA
    result = result[columns]
  }
  
  result
}

#' Get article metadata from AmCAT
#'
#' Uses the \code{\link{get_objects}} function to retrieve article metadata, and applies some
#' additional postprocessing, e.g. to convert the data to Date objects.
#'
#' @param conn the connection object from \code{\link{amcat_connect}}
#' @param project the project of a set to retrieve metadata from
#' @param articleset the article set id to retrieve - provide either this or articleset
#' @param articles the article ids to retrieve - provide either this or articleset
#' @param uuid like the articles argument, but using the universally unique identifier (uuid)
#' @param columns the names of columns to retrieve, e.g. date, medium, text, headline
#' @param time if true, parse the date as POSIXct datetime instead of Date
#' @param dateparts if true, add date parts (year, month, week)
#' @param page_size the number of articles per (downloaded) page
#' @return A dataframe containing the articles and the selected columns
#' 
#' 
#' @examples
#' \dontrun{
#' conn = amcat_connect('https://amcat.nl')
#' meta = amcat.getarticlemeta(conn, project = 1, articleset = 1)
#' head(meta) 
#' 
#' texts = amcat.getarticlemeta(conn, project = 1, articleset = 1, columns = c('text'))
#' texts$text[1]
#' }
#' @export
amcat.getarticlemeta <- function(conn, project, articleset=NULL, articles=NULL, uuid=NULL, columns=c('date','medium'), time=F, dateparts=F, page_size=10000){
  .Deprecated('get_articles')
  amcat.articles(conn=conn, project=project, articleset=articleset, articles=articles, uuid=uuid, columns=columns, time=time, dateparts=dateparts, page_size=page_size)
}

#' Add articles to an article set
#' 
#' Add the given article ids to a new or existing article set
#' 
#' @param conn the connection object from \code{\link{amcat_connect}}
#' @param project the project to add the articles to
#' @param articles a vector of article ids
#' @param articleset the article set id of an existing set
#' @param articleset.name the name for a new article set
#' @param articleset.provenance a provenance text for a new article set
#' 
#' @examples
#' \dontrun{
#' conn = amcat_connect('https://amcat.nl')
#' 
#' h = get_hits(conn, queries=c("tyrant OR brute", "saddam"), labels=c("tyrant", "saddam"), sets=10271)
#' articles = h$id[h$query == "tyrant"]
#' 
#' ## make new setwith the (two) documents that mention "tyrant"
#' setid = amcat.add.articles.to.set(conn, project=429, articles=articles, articleset.name="New set from howto")
#' 
#' ## set contains only these two documents
#' get_articles(conn, project = 429, articleset=setid)
#' }
#' 
#' @return The articleset id of the new or existing article set
#' @export
amcat.add.articles.to.set <- function(conn, project, articles, articleset=NULL,
                                articleset.name=NULL, articleset.provenance=NULL) {
  .Deprecated('add_articles_to_set')

  if (is.null(articleset)) {
    if (is.null(articleset.name)) 
      stop("Provide articleset or articleset.name")
    path = paste("api", "v4", "projects",project, "articlesets", "?format=json", sep="/")
    if (is.null(articleset.provenance)) 
      articleset.provenance=paste("Uploaded", length(articles), "articles from R on", format(Sys.time(), "%FT%T"))
    r = get_url(conn, path, filters=list(name=articleset.name, provenance=articleset.provenance), post=TRUE) 
    
    articleset = rjson::fromJSON(r)$id
    message("Created articleset ", articleset, ": ", articleset.name," in project ", project)
  }
  if (!is.null(articles)) {
    #idlist = lapply(articles, function(x) list(id=x))
    idlist = articles
    url = paste(conn$host, "api", "v4", "projects",project, "articlesets", articleset, "articles", "", sep="/")
    
    resp = httr::POST(url, body=rjson::toJSON(idlist), httr::content_type_json(), httr::accept_json(), httr::add_headers(Authorization=paste("Token", conn$token)))
    if (resp$status_code != 201) stop("Unexpected status: ", resp$status_code, "\n", httr::content(resp, type="text/plain"))
  }
  articleset
}

#' Upload new articles to AmCAT
#' 
#' Upload articles into a given project and article set, or into a new article set if the articleset argument is character
#' All arguments headline, medium etc. should be either of the same length as text, or of length 1
#' All factor arguments will be converted to character using as.character
#' For date, please provide either a string in ISO notatoin (i.e. "2010-12-31" or "2010-12-31T23:59:00")
#' or a variable that can be converted to string using format(), e.g. Date, POSIXct or POSIXlt. 
#' The articles will be uploaded in batches of 100. 
#' 
#' @param conn the connection object from \code{\link{amcat_connect}}
#' @param project the project to add the articles to
#' @param articleset the article set id of an existing set, or the name of a new set to create
#' @param text the text of the articles to upload
#' @param headline the headlines of the articles to upload
#' @param date the date of the articles to upload
#' @param medium the medium of the articles to upload. 
#' @param provenance if articleset is character, an optional provenance string to store with the new set
#' @param ... and additional fields to upload, e.g. author, byline etc. 
#' 
#' @examples
#' \dontrun{
#' conn = amcat_connect('https://amcat.nl')
#' 
#' set_id = amcat.upload.articles(conn, project = 1, articleset = 'example set', 
#'                          headline = 'example', text = 'example',
#'                          date = as.POSIXct('2010-01-01'), medium = 'example')
#'                          
#' get_articles(conn, project = 1, articleset = set_id, 
#'              columns = c('headline','text','date','medium'))
#' }
#' @export
amcat.upload.articles <- function(conn, project, articleset, text, headline, date, medium, provenance=NULL, ...) {
  .Deprecated('upload_articles')
  
  n = length(text)
  if (is.character(articleset)) {
    if (is.null(provenance)) provenance=paste("Uploaded", n, "articles using R function upload.articles")
    articleset = add_articles_to_set(conn, project, articles=NULL, articleset.name=articleset, articleset.provenance=provenance) 
  }
  
  if (is.factor(date)) date=as.character(date)
  if (!is.character(date)) date = format(date, "%Y-%m-%dT%H:%M:%S")
  fields = data.frame(headline=headline, text=text, date=date, medium=medium, ...)
  # make sure all fields have correct length
  for (f in names(fields)) {
    if (is.factor(fields[[f]])) fields[[f]] = as.character(fields[[f]])
    if (length(fields[[f]]) == 1) fields[[f]] = rep(fields[[f]], n)
    if (length(fields[[f]]) != n) stop(paste("Field", f, "has incorrect length:", length(fields[[f]]), "should be 1 or ", n))
  }
  
  # not very efficient, but probably not the bottleneck
  chunks = split(fields, ceiling((1:n)/100))
  for(chunk in chunks) {
    json_data = vector("list", nrow(chunk))
    for (i in seq_along(json_data)) {
      json_data[[i]] = unlist(lapply(chunk, function(x) x[i]))
    }
    json_data = rjson::toJSON(json_data)
    message("Uploading ", nrow(chunk), " articles to set ", articleset)
    
    url = paste(conn$host, "api", "v4", "projects",project, "articlesets", articleset, "articles", "", sep="/")
    
    resp = httr::POST(url, body=json_data, httr::content_type_json(), httr::accept_json(), httr::add_headers(Authorization=paste("Token", conn$token)))
    if (resp$status_code != 201) stop("Unexpected status: ", resp$status_code, "\n", httr::content(resp, type="text/plain"))
  }
  invisible(articleset)
}



###########################################################
####################### codebook.r ########################

#' Get the hierarchy from an AmCAT codebook
#' 
#' Get the labels and parents for all codes in a codebook
#' 
#' @param conn the connection object from \code{\link{amcat.connect}}
#' @param codebook_id the id of the codebook
#' @param languages which languages to retrieve
#' @return A data frame with code, parent, and one column per language
#'  
#' @examples 
#' \dontrun{
#' conn = amcat_connect("https://amcat.nl")
#' amcat.gethierarchy(conn, codebook_id=374, languages=c("en", "query"))
#' }
#' 
#' @export
amcat.gethierarchy <- function(conn, codebook_id, languages=NULL) {
  .Deprecated('get_hierarchy')
  
  hierarchy = get_objects(conn, "codebookcode", filters=list(codebook__id=codebook_id))
  hierarchy = hierarchy[,c("code", "parent")]
  
  for (lang in languages) {
    r = get_objects(conn, "language", filters=list(label=lang))
    if (nrow(r) != 1) stop(paste("Searching for language", lang, "gave", nrow(r), "results"))
    lang_id = r$id
    labels = get_objects(conn, "label", filters=list(language__id=lang_id, 
                                                           code__codebook_codes__codebook__id=codebook_id))
    labels = labels[, c("code", "label")]
    colnames(labels) = c("code", paste("label", lang, sep="."))  
    hierarchy = merge(hierarchy, labels, all.x=T)
  }
  
  if (!is.null(languages)) {
    labels = rep(NA, nrow(hierarchy))
    for (lang in languages) {
      labelcol = paste("label", lang, sep=".")
      labels[is.na(labels)] = as.character(hierarchy[is.na(labels), labelcol])
    }
    labels = factor(labels)
    hierarchy$parent = labels[match(hierarchy$parent, hierarchy$code)]
    hierarchy$code = labels
    
  }
  
  return(hierarchy)
}

#' Add categories (ancestors) to a codebook 'hierarchy'
#' 
#' Adds one or more categories to codebook codes. Suppose that you have a hierarchy like
#' 
#' code, parent
#' issue, NA
#' economy, issue
#' unemployment, economy
#' environment, issue
#' 
#' The first category or 'root' for all objects will be 'issue'. The second category would be
#' 'economy' for economy and unemployment, and 'environment' for environment. For 'issue', the second
#' category would simply be issue:
#' 
#' code, parent, cat1, cat2
#' issue, NA, issue, issue
#' economy, issue, issue, economy
#' unemployment, economy, issue, economy
#' environment, issue, issue, environment
#' 
#' @param hierarchy the hierarchy data frame from \code{\link{amcat.gethierarchy}}
#' @param maxdepth the maxium number of ancestors per code
#' @return The hierarchy data frame with a column added for each code
#' 
#' @examples 
#' \dontrun{
#' conn = amcat_connect("https://amcat.nl")
#' h = get_hierarchy(conn, codebook_id=374, languages=c("en", "query"))
#' h
#' 
#' h = amcat.hierarchy.cats(h, maxdepth=1)
#' h
#' }
#' @export
amcat.hierarchy.cats <- function(hierarchy, maxdepth=2) {
  .Deprecated('hierarchy_cats')
  
  for(depth in 0:maxdepth) {
    target = paste("cat", (depth+1), sep=".")
    hierarchy[, target] = codebookcat(hierarchy, depth)  
    if (depth > 0) {
      fallback = paste("cat", (depth), sep=".")
      hierarchy[is.na(hierarchy[,target]), target] = hierarchy[is.na(hierarchy[, target]), fallback]
    }
  }
  # Thanks, http://stackoverflow.com/questions/16441952/sort-a-data-frame-by-multiple-columns-whose-names-are-contained-in-a-single-obje
  sortnames = paste("cat", (0:maxdepth) + 1, sep=".")
  hierarchy[do.call("order", hierarchy[, sortnames]),]
}


###########################################################
####################### corpus.r ##########################

#' Get Tokens from AmCAT
#' 
#' Get Tokens (pos, lemma etc) from AmCAT
#' 
#' @param conn the connection object from \code{\link{amcat.connect}}
#' @param project id of the project containing the tokens
#' @param articleset id of the articleset to get features from. If not specified, specify sentence for 'ad hoc' parsing
#' @param module the NLP preprocessing module to get the tokens from
#' @param filters Additional filters, ie c(pos1="V", pos1="A") to select only verbs and adjectives 
#' @param page_size the number of features (articles?) to include per call
#' @param sentence a sentence (string) to be parsed if articleset id is not given
#' @param only_cached if true, only get tokens that have already been preprocessed (recommended for large corpora!)
#' @param ... additional arguments to get_pages
#' @return A data frame of tokens
#' @export
amcat.gettokens <- function(conn, project=NULL, articleset=NULL, module="elastic", 
                            filters=NULL,page_size=1, sentence=NULL, only_cached=F, ...) {
  .Deprecated('get_tokens')
  
  # TODO: now do adhoc / articleset as completely different paths, converge?
  if (!is.null(articleset) & !is.null(project)) {
    if (only_cached) filters = c(filters, list(only_cached=as.numeric(only_cached)))
    filters = c(module=module, filters)
    path = paste("api", "v4", "projects", project, "articlesets", articleset, "tokens", "", sep="/")
    result = get_pages(conn, path, page_size=page_size, rbind_results=F, filters=filters, ...)
    result = make_pages_unique(result)
    result = plyr::rbind.fill(result)
    result
  } else if (!is.null(sentence)) {
    filters = c(module=module, page_size=page_size, format='csv', sentence=sentence, filters)
    path = paste("api", "v4", "tokens", "", sep="/")
    t = get_url(conn, path, filters)
    readoutput(t, format='csv')
  } else stop("Please provide project+articleset or sentence")
} 


###########################################################
####################### django.r ##########################

###########################################################
####################### nlpipe.r ##########################

#' Process an articleset with nlpipe
#' 
#' This requires vanatteveldt/nlpiper to be install as well
#' 
#' @param conn the amcatr connection object
#' @param nlpipe_server the nlpipe server name or folder
#' @param project the amcat project id
#' @param articleset the amcat articleset id
#' @param module the nlpipe module name
#' @export
amcat.nlpipe <- function(conn, project, articleset, module, nlpipe_server=getOption("nlpiper.server", default="http://localhost:5001")) {
  .Deprecated('nlpipe')
  
  if(!requireNamespace('nlpiper', quietly = T)) stop("Please install_github('/vanatteveldt/nlpiper')")
  ids = get_articles(conn, project, articleset, columns=NULL)$id
  status = nlpiper::status(module, ids, nlpipe_server)
  todo = ids[status == "UNKNOWN"]
  if (length(todo) == 0) {
    message("All ", length(ids), " articles from ", conn$host, " set ", articleset, " are already assigned with ", module, " at ", nlpipe_server)
  } else {
    message("Assigning ", length(todo), " articles from ", conn$host, " set ", articleset, " for processing with ", module, " at ", nlpipe_server)
    chunks = split(todo, ceiling(seq_along(todo)/1000))
    for (chunk in chunks) {
      articles = get_articles(conn, project, articles=chunk, columns=c("headline", "text"))
      texts = paste(articles$headline, articles$text, sep="\n\n")
      nlpiper::process_async(module, texts, ids=articles$id, server = nlpipe_server)
    }
  }
  return(ids) # also return ids that were already on queue
}

###########################################################
####################### query.r ###########################

#' Conduct an aggregate query on amcat
#'
#' This function is similar to using the 'show table' function in AmCAT. It allows you to specify a
#' number of queries and get the number of hits per search term, per period, etc.
#'
#' @param conn the connection object from \code{\link{amcat.connect}}
#' @param queries a vector of queries to run
#' @param labels if given, labels corresponding to the queries
#' @param sets one or more article set ids to query on
#' @param axis1 The first grouping (break/group by) variable, e.g. year, month, week, day, or medium
#' @param axis2 The second grouping (break/group by) variable, e.g. medium. Do not use a date interval here.
#' @param ... additional arguments to pass to the AmCAT API. 
#' @return A data frame with hits per group
#' 
#' @examples 
#' \dontrun{
#' conn = amcat_connect('https://amcat.nl')
#' a = get_aggregate(conn, sets=10271, axis1="year",
#'                   queries=c("*", "obama", "bush"), 
#'                   labels=c("Total", "Obama", "Bush"))
#' head(a)
#' }
#' @export
amcat.aggregate <- function(conn, queries, labels=queries, sets, axis1=NULL, axis2=NULL, ...) {
  .Deprecated('get_aggregate')
  
  result = NULL
  queries = as.character(queries)
  for (i in 1:length(queries)) {
    if (!is.na(queries[i])) {
      
      r = tryCatch(get_objects(conn,"aggregate", filters=list(q=queries[i], sets=sets, axis1=axis1, axis2=axis2, ...)),
                   error=function(e) {warning("Error on querying '", labels[i], "': ", e$message); NULL})
      if (is.null(r)) next
      if (nrow(r) > 0) {
        if (names(r)[1] == "count") {
          r$query = labels[i]
          result = rbind(result, r)
        } else {
          warning(paste("Error on querying",labels[i]))
        }
      }
    }
  }  
  # convert axis1 to Date object if needed
  if (!is.null(axis1))
    if (axis1 %in% c("year", "quarter", "month", "week", "day")) result[, axis1] = as.Date(result[, axis1])
  return(result)
}

#' Conduct a query on amcat
#'
#' This function is similar to using the 'show article list' function in AmCAT. It allows you to specify a
#' number of queries and get document metadata and number of hits per document
#'
#' @param conn the connection object from \code{\link{amcat.connect}}
#' @param queries a vector of queries to run
#' @param labels if given, labels corresponding to the queries. Alternatively, if a query starts with a label and a hashtag (e.g., label# "term1 term2"), this label is used.
#' @param sets one or more article set ids to query on
#' @param minimal if TRUE, returns only hits per document. if set to FALSE, includes more meta data
#' @param warn.no.result if TRUE, yield a warning in case no results were found
#' @param ... additional arguments to pass to the AmCAT API, e.g. extra filters
#' @return A data frame with hits per article
#' 
#' @examples 
#' \dontrun{
#' conn = amcat_connect('https://amcat.nl')
#' h = get_hits(conn, queries=c("tyrant OR brute", "saddam"), labels=c("tyrant", "saddam"), sets=10271)
#' head(h)
#' table(h$query)
#' }
#' @export
amcat.hits <- function(conn, queries, labels=queries, sets,  minimal=T, warn.no.result=T, ...) {
  .Deprecated('get_hits')
  
  result = NULL
  
  for (i in 1:length(queries)) {
    q = paste("count", queries[i], sep="#")
    r = tryCatch(get_objects(conn, "search",filters=list(q=q, col="hits", sets=sets, minimal=minimal, ...)),
                 error=function(e) {warning("Error on querying '", labels[i], "': ", e$message); NULL})
    if (is.null(r)) next
    
    label = if(grepl('#', queries[i])) gsub('#.*', '', queries[i]) else labels[i] 
    if (nrow(r) > 0) {
      r$query = label
      result = rbind(result, r)
    } else {
      if (warn.no.result) warning(paste("Query",label," produced no results"))
    }
  }
  return(result)
}