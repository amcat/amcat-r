#' Get Tokens from AmCAT
#' 
#' Get Tokens (pos, lemma etc) from AmCAT
#' 
#' @param conn the connection object from \code{\link{amcat.connect}}
#' @param project id of the project containing the tokens
#' @param articleset id of the articleset to get features from
#' @param module the NLP preprocessing module to get the tokens from
#' @param keep an optional list of attributes to keep (and aggregate on), e.g. c("lemma", "pos1")
#' @param drop an optional list of attributes to drop, default (token)id and sentence. If keep is given, drop is ignored. 
#' @param filters Additional filters, ie c(pos1="V", pos1="A") to select only verbs and adjectives 
#' @param page_size the number of features (articles?) to include per call
#' @return A data frame of tokens
#' @export
amcat.gettokens <- function(conn, project, articleset, module="corenlp_lemmatize", keep=NULL, drop=c("id", "sentence", "offset"), filters=NULL, page_size=1, page=1, npages=NULL) {
  filters = c(module=module, page_size=page_size, format='csv', filters)
  path = paste("api", "v4", "projects", project, "articlesets", articleset, "tokens", "", sep="/")
  result = NULL
  while (TRUE) {
    page_filters = c(page=page, filters)
    t = amcat.getURL(conn, path, page_filters)
    if (t == "") break
    
    t = .amcat.readoutput(t, format='csv')
    if (is.null(keep)) keep = colnames(t)[!colnames(t) %in% drop]
    freqs = count(t[, keep])
    result = rbind(result, freqs)
    
    if (!is.null(npages)) if (npages <= page) break
    page = page + 1
  }
  result
} 

#' Create a document term matrix from a list of tokens
#' 
#' Create a document-term matrix (dtm) from a list of ids, terms, and frequencies
#' 
#' @param ids a vector of document ids
#' @param terms a vector of words of the same length as ids
#' @param freqs a vector of the frequency a a term in a document
#' @return a document-term matrix  \code{\link{DocumentTermMatrix}}
#' @export

amcat.dtm.create <- function(ids, terms, freqs) {
    documents = unique(ids)
    vocabulary = unique(terms)
    m = simple_triplet_matrix(match(ids, documents), 
                              match(terms, vocabulary), 
                              freqs, 
                              dimnames=list(documents=as.character(documents), words=as.character(vocabulary)))
    as.DocumentTermMatrix(m, weighting=weightTf)
}

#' Estimate a topic model using the lda package
#' 
#' Estimate an LDA topic model using the \code{\link{lda.collapsed.gibbs.sampler}} function
#' The parameters other than dtm are simply passed to the sampler but provide a workable default.
#' See the description of that function for more information
#' 
#' @param dtm a document term matrix (e.g. the output of \code{\link{amcat.createDTM}})
#' @param K the number of clusters
#' @param num.iterations the number of iterations
#' @param alpha the alpha parameter
#' @param eta the eta parameter
#' @return A fitted LDA model (see \code{\link{lda.collapsed.gibbs.sampler}})
#' @export
amcat.lda.fit <- function(dtm, K=50, num.iterations=100, alpha=50/K, eta=.01) {
  x = dtm2ldaformat(dtm)
  m = lda.collapsed.gibbs.sampler(x$documents, vocab=x$vocab, K=K, num.iterations=num.iterations, alpha=alpha, eta=eta)
  m$dtm = dtm
  m
}


#' Compute some useful corpus statistics for a dtm
#' 
#' Compute a number of useful statistics for filtering words: term frequency, idf, etc.
#' 
#' @param dtm a document term matrix (e.g. the output of \code{\link{amcat.createDTM}})
#' @return A data frame with rows corresponding to the terms in dtm and the statistics in the columns
#' @export
amcat.term.statistics <- function(dtm) {
    vocabulary = colnames(dtm)
    data.frame(term = vocabulary,
               characters = nchar(vocabulary),
               number = grepl("[0-9]", vocabulary),
               termfreq = col_sums(dtm),
               docfreq = col_sums(dtm > 0),
               tfidf = tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0)))
}
 
#' Get the topics per document, optionally merged with 
#' 
#' Return a data frame containing article metadata and topic occurence per document
#' 
#' @param dtm a document term matrix (e.g. the output of \code{\link{amcat.createDTM}})
#' @return A data frame with rows corresponding to the terms in dtm and the statistics in the columns
#' @export
amcat.lda.topics.per.document <- function(topics) {
  ids = as.numeric(rownames(topics$dtm))
  cbind(id=ids, data.frame(t(topics$document_sums)))
}



