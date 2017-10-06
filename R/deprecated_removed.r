#' Download articles from AmCAT into a Quanteda corpus object
#'
#' @param conn the connection object from \code{\link{amcat_connect}}
#' @param project The project ID
#' @param articleset  The Articleset ID
#' @param textcolumns Which columns contain text (default: headline, text)
#' @param metacolumns Which columns to include as docvars (deafult: date, medium)
#' @param headline.as.docvar Should headline be a docvar (as well as a text column)
#' @param ... Other arguments to pass to get_articles, i.e. dateparts=T
#'
#' @return a quanteda corpus object
#' @export
quanteda.corpus <- function(conn, project, articleset, textcolumns=c("headline", "text"), 
                            metacolumns=c("date", "medium"), headline.as.docvar=T, ...) {
  .Deprecated()
  
  if(!requireNamespace('quanteda', quietly = T)) stop('To use this function, first install the quanteda package.')
  articles = get_articles(conn, project, articleset, columns=c(textcolumns, metacolumns), ... )
  
  if (headline.as.docvar) textcolumns = setdiff(textcolumns, "headline")
  metavars = setdiff(colnames(articles), textcolumns)
  x = apply(articles[textcolumns], 1, paste, collapse='\n')
  texts = apply(articles[textcolumns], 1, paste, collapse='\n')
  quanteda::corpus(texts, docnames=articles$id, docvars=articles[metavars])
}


#' Change a (simple) lucene query to a regular expression
#' 
#' It is assumed that this will be used to filter single words/lemmata, so the query should only consist of a list of synonyms with optional wildcards.
#' E.g. a query like (many* words*) is fine, but many AND words or "many words" will give an error.
#' 
#' @param queries a character vector containing the queries
#' @param names an optional vector of the same length as queries containing names for the concepts
#' @return a character vector of regular expressions, with names added if given
#' @export
lucene_to_re <- function(queries, names=NULL) {
  .Deprecated()
  
  # remove trailing/leading parens
  queries = gsub("^\\s*\\(|\\)\\s*$", "", queries)
  # check no weird syntax
  if (any(grepl(' (OR|AND|NOT) |\\(|\\)|"', queries)))
    stop("Queries should only contain disjunctions and wildcards")
  # replace space by | and * by .*
  queries = gsub("[, ]+", "|", queries)
  queries = gsub("\\*", ".*", queries)
  # add parens and initial/final bind and return
  queries = paste("^(", queries, ")$", sep="")
  if (!is.null(names))
    names(queries) = names
  queries
}



#' Matches a regular expression pattern on multiple strings
#' 
#' This is intended as a 'drop-in' replacement for grepl and should give identical results
#' This function first matches on the unique strings and then matches back to the original,
#' so it will give a speedup iff length(unique(words)) << length(words)
#' 
#' @param pattern a regular expression pattern to use
#' @param words a vector of words, this should probably be a factor to achieve speedup
#' @param ignore.case passed to grepl, defaults to TRUE for this function
#' @return a vector of booleans indicating which words matched the pattern
#' @export
match_words <- function(pattern, words, ignore.case=T) {
  .Deprecated()
  
  u = unique(words)
  hits = u[grepl(pattern, u, ignore.case=ignore.case)]
  words %in% hits
}