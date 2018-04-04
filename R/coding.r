#' Create a new codingjob
#' @param conn the connection object from \code{\link{amcat.connect}}
#' @param project the project to add the articles to
#' @param articleset the article set id of an existing set, or the name of a new set to create
#' @param articlesarticle IDs to be added to this coding job. Ignored if articleset is an ID
#' @param coder the ID of the coder to assign the job to
#' @param articleschema the ID of the articleschema (may be ommitted)
#' @param unitschema the ID of the unit (sentence) schema (may be ommitted)
#' @export
amcat.create.codingjob <- function(conn, project, codingjobname, coder, articleschema=NULL, unitschema=NULL, articleset=NULL, articles=NULL) {
  if (!is.numeric(articleset)) {
    if (is.null(articles)) stop("Please specify either an article set ID or a vector of article IDs")
    setname = if (is.null(articleset)) codingjobname else articleset
    message("Creating new set ", setname, " with ", length(articles), " articles")
    articleset = amcat.add.articles.to.set(conn, project, articles, articleset.name=setname)
  }
  
  data = list(name=codingjobname, project=project, coder=coder, articleschema=articleschema, unitschema=unitschema, articleset=articleset)
  json_data = toJSON(data)
  url = paste(conn$host, "api", "v4", "projects", project, "codingjobs", "", sep = "/")
  message("Creating codingjob for coder ",coder)
  resp = httr::POST(url, body = json_data, content_type_json(), accept_json(), add_headers(Authorization = paste("Token", conn$token)))
  if (resp$status_code != 201) stop("Error on POST to ",url,": ", resp$status_code, " ", http_status(resp)$reason,"\n", content(resp))
  invisible(content(resp)$id)
}


#' Convert a list of rows (named list) into a data frame
list.to.df = function(x, stringsAsFactors=F) {
  dplyr::bind_rows(lapply(x, function(f) {
    as.data.frame(Filter(Negate(is.null), f), stringsAsFactors=stringsAsFactors)
  }))
}

#' Get the codebook codes for a job
amcat.get.job.codes <- function(conn, project, job) {
  base = c("projects", project, "codingjobs", job)
  codebooks = amcat.getobjects(conn, c(base, "codebooks"), page_size=99999, format="json")
  codes = list()
  for(cb in codebooks) {
    codes[[as.character(cb$id)]] = data.frame(label = sapply(cb$codes, function(x) x$label), code=sapply(cb$codes, function(x) x$code), stringsAsFactors = F)
  }
  unique(dplyr::bind_rows(codes))
}

# ' Get the 'raw' codings for a job
amcat.get.job.codings <- function(conn, project, job, coded_article_ids) {
  base = c("projects", project, "codingjobs", job)
  bar = utils::txtProgressBar(max=length(coded_article_ids), style=3)
  result = list()
  for (id in coded_article_ids) {
    bar$up(bar$getVal()+1)
    codings = amcat.getobjects(conn, c(base, "coded_articles", id, "codings"), format="json", verbose=F)
    for (coding in codings) {
      v = list.to.df(coding$values)
      if (nrow(v) == 0) next
      v$coding_id = coding$id 
      v$id = NULL # codingvalue_id is not relevant
      v$coded_article = coding$coded_article
      v$sentence = coding$sentence
      result[[as.character(coding$id)]] = v
    }
  }
  bar$kill()
  dplyr::bind_rows(result)
}

#' Process a 'long' list of raw codings into a 'wide' data frame with a column per coded variable
process.codings <- function(codings, fields, codes) {
  if (length(unique(fields$label)) != nrow(fields)) stop("Duplicate label!")
  result = unique(codings[c("coded_article", "sentence", "coding_id")])
  for (field in unique(codings$field)) {
    f = fields[fields$id == field,]
    # field types:  TEXT = 1     INT = 2     CODEBOOK = 5     BOOLEAN = 7     QUALITY = 9
    if (f$fieldtype == 1) {
      col = subset(codings, field == f$id, select=c("coding_id", "strval"))
    } else {
      col = subset(codings, field == f$id, select=c("coding_id", "intval"))
      if (f$fieldtype == 5) col$intval = codes$label[match(col$intval, codes$code)]
      if (f$fieldtype == 7) col$intval = col$intval == 1
      if (f$fieldtype == 9) col$intval = col$intval / 10
    }
    colnames(col)[2] = as.character(f$label)
    result = merge(result, col, all.x=T)
  }
  result
}

.STATUSCODES=c(NOT_STARTED=0, IN_PROGRESS=1, COMPLETE=2, IRRELEVANT=9)


#' Get the results of a codingjob
#' 
#' (note: current implementation is fairly slow because )
#' 
#' @param conn the connection object from \code{\link{amcat.connect}}
#' @param project project ID
#' @param job coding job ID
#' 
#' @result a list with article.codings and (if available) sentence.codings
#' @export
amcat.get.codingjob.results <- function(conn, project, job) {
  base = c("projects", project, "codingjobs", job)
  fields = amcat.getobjects(conn, c(base, "codingschemafields"))
  codes = amcat.get.job.codes(conn, project, job)
  articles = amcat.getobjects(conn, c(base, "coded_articles"))
  articles = dplyr::transmute(articles, codingjob=codingjob, article_id=article_id, coded_article=id, 
                              status=names(.STATUSCODES)[match(articles$status, .STATUSCODES)], comments=as.character(comments))
  
  codings = amcat.get.job.codings(conn, project, job, articles$coded_article)
  # if no article codings, only return article meta
  if (nrow(codings) == 0) return(list(article.codings=articles))
  
  has_scodings = "sentence" %in% names(codings)
  if (!has_scodings) codings$sentence = NA
  acodings = dplyr::select(process.codings(subset(codings, is.na(sentence)), fields, codes), -sentence)
  articles
  result = list(article.codings = merge(articles, acodings, all.x = T))
  if (has_scodings) {
    scodings = process.codings(subset(codings, !is.na(sentence)), fields, codes)
    result[["sentence.codings"]] = merge(dplyr::select(articles, -status, -comments), scodings)
  }
  result
}
