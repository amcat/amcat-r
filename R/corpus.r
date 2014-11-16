#' Get Tokens from AmCAT
#' 
#' Get Tokens (pos, lemma etc) from AmCAT
#' 
#' @param conn the connection object from \code{\link{amcat.connect}}
#' @param project id of the project containing the tokens
#' @param articleset id of the articleset to get features from. If not specified, specify sentence for 'ad hoc' parsing
#' @param module the NLP preprocessing module to get the tokens from
#' @param keep an optional list of attributes to keep (and aggregate on), e.g. c("lemma", "pos1")
#' @param drop an optional list of attributes to drop, default (token)id and sentence. If keep is given, drop is ignored. 
#' @param count if true, tokens will be made unique and a 'freq' column will be added
#' @param filters Additional filters, ie c(pos1="V", pos1="A") to select only verbs and adjectives 
#' @param page_size the number of features (articles?) to include per call
#' @param sentence a sentence (string) to be parsed if articleset id is not given
#' @return A data frame of tokens
#' @export
amcat.gettokens <- function(conn, project=NULL, articleset=NULL, module="corenlp_lemmatize", 
                            keep=NULL, drop=c("id", "sentence", "offset"), count=TRUE, filters=NULL, 
                            page_size=1, page=1, npages=NULL, 
                            sentence=NULL) {
  # TODO: now do adhoc / articleset as completely different paths, converge?
  if (!is.null(articleset) & !is.null(project)) {
    filters = c(module=module, page_size=page_size, format='csv', filters)
    path = paste("api", "v4", "projects", project, "articlesets", articleset, "tokens", "", sep="/")
    result = list()
    while (TRUE) {
      page_filters = c(page=page, filters)
      t = amcat.getURL(conn, path, page_filters)
      if (t == "") break
      
      t = .amcat.readoutput(t, format='csv')
      # slightly memory inefficient 
      # we could filter keep/drop before appending but we might not know all column names yet
      result = c(result, list(t))
      if (!is.null(npages)) if (npages <= page) break
      page = page + 1
    }
    # which columns to use?
    if (is.null(keep)) {
      keep = unique(unlist(lapply(result, colnames)))
      if (!is.null(drop)) keep=setdiff(keep, drop)
    }
    result = lapply(result, function(x) {x = .select.columns(x, columns=keep); if (count) count(x) else x})
    do.call(rbind, result)
  } else if (!is.null(sentence)) {
    filters = c(module=module, page_size=page_size, format='csv', sentence=sentence, filters)
    path = paste("api", "v4", "tokens", "", sep="/")
    t = amcat.getURL(conn, path, filters)
    .amcat.readoutput(t, format='csv')
  } else stop("Please provide project+articleset or sentence")
} 

#' Select given columns on the dataframe, adding NA columns if needed
.select.columns <- function(df, columns, count=TRUE) {
  for (col in columns) if (!(col %in% colnames(df))) df[col] = rep(NA, nrow(df))
  df[columns]
}

#' Cast data.frame to sparse matrix
#' 
#' Create a sparse matrix from matching vectors of row indices, column indices and values
#' 
#' @param rows a vector of row indices: [i,]
#' @param columns a vector of column indices: [,j]
#' @param values a vector of the values for each (non-zero) cell: [i,j] = value
#' @return a sparse matrix of the dgTMatrix class (\code{\link{Matrix}} package) 
#' @export
amcat.cast.sparse.matrix <- function(rows, columns, values=NULL) {
  if(is.null(values)) values = rep(1, length(rows))
  d = data.frame(rows=rows, columns=columns, values=values)
  if(nrow(d) > nrow(unique(d[,c('rows','columns')]))){
    message('(Duplicate row-column matches occured. Values of duplicates are added up)')
    d = aggregate(values ~ rows + columns, d, FUN='sum')
  }
  unit_index = unique(d$rows)
  char_index = unique(d$columns)
  sm = spMatrix(nrow=length(unit_index), ncol=length(char_index),
                match(d$rows, unit_index), match(d$columns, char_index), d$values)
  rownames(sm) = unit_index
  colnames(sm) = char_index
  sm
}
  

#' Create a document term matrix from a list of tokens
#' 
#' Create a \code{\link{DocumentTermMatrix}} from a list of ids, terms, and frequencies. 
#' 
#' @param ids a vector of document ids
#' @param terms a vector of words of the same length as ids
#' @param freqs a vector of the frequency a a term in a document
#' @return a document-term matrix  \code{\link{DocumentTermMatrix}}
#' @export
amcat.dtm.create <- function(ids, terms, freqs) {
  # remove NA terms
  d = data.frame(ids=ids, terms=terms, freqs=freqs)
  if (sum(is.na(d$terms)) > 0) {
    warning("Removing ", sum(is.na(d$terms)), "rows with missing term names")
    d = d[!is.na(d$terms), ]
  }
  sparsemat = amcat.cast.sparse.matrix(d$ids, d$terms, d$freqs)
  as.DocumentTermMatrix(sparsemat, weighting=weightTf)
}

#' Estimate a topic model using the lda package
#' 
#' Estimate an LDA topic model using the \code{\link{lda.collapsed.gibbs.sampler}} function
#' The parameters other than dtm are simply passed to the sampler but provide a workable default.
#' See the description of that function for more information
#' 
#' @param dtm a document term matrix (e.g. the output of \code{\link{amcat.dtm.create}})
#' @param K the number of clusters
#' @param num.iterations the number of iterations
#' @param alpha the alpha parameter
#' @param eta the eta parameter
#' @return A fitted LDA model (see \code{\link{lda.collapsed.gibbs.sampler}})
#' @export
amcat.lda.fit <- function(dtm, K=50, num.iterations=100, alpha=50/K, eta=.01, burnin=100, compute.log.likelihood=F) {
  dtm = dtm[row_sums(dtm) > 0,col_sums(dtm) > 0]
  x = dtm2ldaformat(dtm)
  m = lda.collapsed.gibbs.sampler(x$documents, vocab=x$vocab, K=K, num.iterations=num.iterations, 
                                  alpha=alpha, eta=eta, burnin=burnin, compute.log.likelihood=compute.log.likelihood)
  m$dtm = dtm
  m
}

#' Compute some useful corpus statistics for a dtm
#' 
#' Compute a number of useful statistics for filtering words: term frequency, idf, etc.
#' 
#' @param dtm a document term matrix (e.g. the output of \code{\link{amcat.dtm.create}})
#' @return A data frame with rows corresponding to the terms in dtm and the statistics in the columns
#' @export
amcat.term.statistics <- function(dtm) {
    dtm = dtm[row_sums(dtm) > 0,col_sums(dtm) > 0]    # get rid of empty rows/columns
    vocabulary = colnames(dtm)
    data.frame(term = vocabulary,
               characters = nchar(vocabulary),
               number = grepl("[0-9]", vocabulary),
               nonalpha = grepl("\\W", vocabulary),
               termfreq = col_sums(dtm),
               docfreq = col_sums(dtm > 0),
               reldocfreq = col_sums(dtm > 0) / nDocs(dtm),
               tfidf = tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0)))
}
 
#' Get the topics per document, optionally merged with 
#' 
#' Return a data frame containing article metadata and topic occurence per document
#' 
#' @param dtm a document term matrix (e.g. the output of \code{\link{amcat.dtm.create}})
#' @return A data frame with rows corresponding to the terms in dtm and the statistics in the columns
#' @export
amcat.lda.topics.per.document <- function(topics) {
  ids = as.numeric(rownames(topics$dtm))
  cbind(id=ids, data.frame(t(topics$document_sums)))
}

#' Compute the chi^2 statistic for a 2x2 crosstab containing the values
#' [[a, b], [c, d]]
chi2 <- function(a,b,c,d) {
  ooe <- function(o, e) {(o-e)*(o-e) / e}
  tot = 0.0 + a+b+c+d
  a = as.numeric(a)
  b = as.numeric(b)
  c = as.numeric(c)
  d = as.numeric(d)
  (ooe(a, (a+c)*(a+b)/tot)
   +  ooe(b, (b+d)*(a+b)/tot)
   +  ooe(c, (a+c)*(c+d)/tot)
   +  ooe(d, (d+b)*(c+d)/tot))
}

#' Compare two corpora
#' 
#' Compare the term use in corpus dtm with a refernece corpus dtm.ref, returning relative frequencies
#' and overrepresentation using various measures
#' 
#' @param dtm.x the main document-term matrix
#' @param dtm.y the 'reference' document-term matrix
#' @param smooth the smoothing parameter for computing overrepresentation
#' @return A data frame with rows corresponding to the terms in dtm and the statistics in the columns
#' @export
amcat.compare.corpora <- function(dtm.x, dtm.y, smooth=.001) {
  freqs = amcat.term.statistics(dtm.x)[, c("term", "termfreq")]
  freqs.rel = amcat.term.statistics(dtm.y)[, c("term", "termfreq")]
  f = merge(freqs, freqs.rel, all=T, by="term")    
  f[is.na(f)] = 0
  f$relfreq.x = f$termfreq.x / sum(freqs$termfreq)
  f$relfreq.y = f$termfreq.y / sum(freqs.rel$termfreq)
  f$over = (f$relfreq.x + smooth) / (f$relfreq.y + smooth)
  f$chi = chi2(f$termfreq.x, f$termfreq.y, sum(f$termfreq.x) - f$termfreq.x, sum(f$termfreq.y) - f$termfreq.y)
  f
}

