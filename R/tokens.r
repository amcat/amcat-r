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
amcat.getTokens <- function(conn, project, articleset, module="corenlp_lemmatize", keep=NULL, drop=c("id", "sentence", "offset"), filters=NULL, page_size=1, npages=NULL) {
  filters = c(module="corenlp_lemmatize", filters)
  path = paste("api", "v4", "projects", project, "articlesets", articleset, "tokens", "", sep="/")
  page = 1
  result = NULL
  while (TRUE) {
    filters = c(page=page, page_size=page_size, module=module, format='csv')
    t = amcat.getURL(conn, path, filters)
    if (t == "") break
    
    t = .amcat.readoutput(t, format='csv')
    if (is.null(keep)) keep = colnames(t)[!colnames(t) %in% drop]
    freqs = count(t[, keep])
    result = rbind(result, freqs)
    
    page += 1
    if (!is.null(npages) & npages <= page) break
  }
  result
} 

