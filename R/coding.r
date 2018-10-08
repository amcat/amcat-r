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
      if (f$fieldtype == 5) { # codebook
        col$intval = factor(paste(col$intval, codes$label[match(col$intval, codes$code)], sep = " - "))
      }
      if (f$fieldtype == 7) col$intval = col$intval == 1
      if (f$fieldtype == 9) col$intval = col$intval / 10
    }
    colnames(col)[2] = as.character(f$label)
    attributes(col[[2]])$amcat.codingschemafield = field
    result = merge(result, col, all.x=T)
  }
  result
}

.STATUSCODES=c(NOT_STARTED=0, IN_PROGRESS=1, COMPLETE=2, IRRELEVANT=9)


#' Get the results of a codingjob
#' 
#' (note: current implementation is fairly slow because it makes HTTP calls per coded article)
#' (it is presumably still quicker than the human coders, though :) )
#' 
#' @param conn the connection object from \code{\link{amcat.connect}}
#' @param project project ID
#' @param job coding job ID
#' 
#' @result a list with article.codings and (if available) sentence.codings
#' @export
amcat.codingjob.results <- function(conn, project, job) {
  base = c("projects", project, "codingjobs", job)
  fields = amcat.getobjects(conn, c(base, "codingschemafields"))
  codes = amcat.get.job.codes(conn, project, job)
  articles = amcat.getobjects(conn, c(base, "coded_articles"))
  if (nrow(articles) == 0) return(NULL)
  articles = dplyr::transmute(articles, rticle_id=article_id, coded_article=id, 
                              status=names(.STATUSCODES)[match(articles$status, .STATUSCODES)], comments=as.character(comments))
  
  codings = amcat.get.job.codings(conn, project, job, articles$coded_article)
  # if no article codings, only return article meta
  if (nrow(codings) == 0) return(list(article.codings=articles, fields=fields))
  
  has_scodings = "sentence" %in% names(codings)
  if (!has_scodings) codings$sentence = NA
  acodings = subset(codings, is.na(sentence))
  fields$artcoding = fields$id %in% acodings$field
  acodings = dplyr::select(process.codings(acodings, fields, codes), -sentence)
  articles
  result = list(article.codings = merge(articles, acodings, all.x = T),
                fields = fields)
  if (has_scodings) {
    scodings = process.codings(subset(codings, !is.na(sentence)), fields, codes)
    result[["sentence.codings"]] = merge(dplyr::select(articles, -status, -comments), scodings)
  }
  result
}

#' Strip the codeid from columns
#' 
#' Removes the ID from columns containing "<ID> - <Label>" values
#' @param codes A vector containing codes, or a data frame with codes, e.g. from  \code{\link{amcat.codingjob.results}}
#' @param columns If codes is a data frame: the columns to strip. If not given, strips all factor columns in the data frame. 
#' @export
amcat.strip.codeid <- function(codes, columns=NULL) {
  if (is.data.frame(codes)) {
    # resursively apply to all (selected) columns
    if (is.null(columns)) columns=colnames(codes)
    for (col in columns) 
      if (is.factor(codes[[col]]))
        codes[[col]] = amcat.strip.codeid(codes[[col]])
    return(codes)
  } else {
    factor(gsub("^\\d+ - ", "", codes))
  }
}

#' Memoised function to get the hierarchy as code / parent columns, both character "<ID> - <Label>"
.gethierarchy = function(conn, codebook_id) {
  h = amcat.getobjects(conn, "codebookcode", filters=list(codebook__id=codebook_id))
  parentlabels = h$label[match(h$parent, h$code)]
  data.frame(code=paste(h$code, h$label, sep=" - "), parent=ifelse(is.na(h$parent), NA, paste(h$parent, parentlabels, sep=" - ")), stringsAsFactors = F)
}
if (require("memoise")) .gethierarchy =  memoise::memoise(.gethierarchy)

#' Add parents to a data frame of codes
#' 
#' New columns are generated for each code, containing their roots / parents
#' If columns is not specified, adds parents to each codebook column
#' 
#' @param conn the connection object from \code{\link{amcat.connect}}
#' @param codes a data frame of codes, e.g. from \code{\link{amcat.codingjob.results}}
#' @param fields the coding field specifications, e.g. from \code{\link{amcat.codingjob.results}}
#' @param columns a character vector containing the column names to add parents to
#' @param maxdepth how many parent columns to add for each column, e.g. maxdepth=1 gives roots only
#' @export
amcat.add.parents <- function(conn, codes, fields, columns=NULL, maxdepth=1) {
  if (length(unique(fields$label)) < nrow(fields)) stop("Duplicate labels!")
  if (is.null(columns)) columns = intersect(colnames(codes), as.character(fields$label[!is.na(fields$codebook)]))
  for (column in columns) {
    cb = fields$codebook[fields$label == column]
    hierarchy = .gethierarchy(conn, cb)
    #message(column, " (codebook ", cb, " with ", nrow(hierarchy), " codes)")
    roots = .create.cats(hierarchy, label=column, maxdepth=maxdepth)
    # roots = amcat.code.roots(conn, cb, depths = depths, label=column, add_id=add_id)
    # merge roots to result, but use some hackery to add new columns next to existing column
    oldnames = colnames(codes)
    codes = merge(codes, roots, all.x=T)
    pos = which(oldnames == column)
    newnames = c(oldnames[1:(pos-1)], colnames(roots) , if (pos >= length(oldnames)) NULL else oldnames[(pos+1):length(oldnames)])
    codes = codes[newnames]
  }
  codes
}

#' Create categories from a hierarchy (parent-code) list
#' The result is a data frame with the same number of rows as the hierarchy,
#' where the first column consists only of the roots, the second column of the roots and their children
#' and so un until the last column consists of all nodes, so each row is a path to the root
#' @param label column names to generate: label, label.p1, label.p2 etc
#' @param maxdepth number of columns to keep, not counting the code column.
.create.cats = function(hierarchy, label="code", maxdepth=NULL) {
  # start from roots
  result = data.frame(code0=hierarchy$code[is.na(hierarchy$parent)], stringsAsFactors = F)
  # iteratively merge with children, including a code -> NA entry for each code so non-leaf nodes also propagate
  # (needs to be NA to prevent merging children on again - NA's will be replaced by values after the for loop)
  children = rbind(na.omit(hierarchy[c("parent", "code")]), data.frame(parent=hierarchy$code, code=NA))
  colnames(result) = paste0(label, ".p1")
  for (i in 1:nrow(children)) {
    colnames(children) = c(colnames(result)[i], paste0(label,".p", (i+1)))
    result = merge(result, children, all.x=T)
    if (nrow(result) == nrow(hierarchy)) break
  }
  # reorder columns from most detailed to the root
  result = result[paste0(label, ".p",  (i+1):1)]
  colnames(result)[[1]] = label # drop p.N
  # fill out NA's to the left - these are the earlier code -> NA entries, 
  for (i in (ncol(result)-1):1) result[[i]][is.na(result[[i]])] = result[[i+1]][is.na(result[[i]])]
  # drop columns if needed
  if (!is.null(maxdepth) && ncol(result) > maxdepth+1) {
    result = result[c(1, (ncol(result) - maxdepth + 1):ncol(result))]
  }
  # convert to factor
  for (c in colnames(result)) result[[c]] = factor(result[[c]])
  result
}


