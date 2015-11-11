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
#' @return A data frame of tokens
#' @export
amcat.gettokens <- function(conn, project=NULL, articleset=NULL, module="corenlp_lemmatize", 
                            filters=NULL,page_size=1, page=1, npages=NULL, 
                            sentence=NULL, only_cached=F) {
  # TODO: now do adhoc / articleset as completely different paths, converge?
  if (!is.null(articleset) & !is.null(project)) {
    if (only_cached) filters = c(filters, list(only_cached=as.numeric(only_cached)))
    filters = c(module=module, page_size=page_size, format='csv',  filters)
    path = paste("api", "v4", "projects", project, "articlesets", articleset, "tokens", "", sep="/")
    result = list()
    while (TRUE) {
      page_filters = c(page=page, filters)
      t = amcat.getURL(conn, path, page_filters, error_unless_200=FALSE)
      if (is.null(t)) break
      t = .amcat.readoutput(t, format='csv')
      result = c(result, list(t))
      if (!is.null(npages)) if (npages <= page) break
      page = page + 1
    }
    result = .make.pages.unique(result)
    result = rbind.fill(result)
    result
  } else if (!is.null(sentence)) {
    filters = c(module=module, page_size=page_size, format='csv', sentence=sentence, filters)
    path = paste("api", "v4", "tokens", "", sep="/")
    t = amcat.getURL(conn, path, filters)
    .amcat.readoutput(t, format='csv')
  } else stop("Please provide project+articleset or sentence")
} 

#' Make the given columns unique within a context for a list of data frames
#' 
#' It will iterate over the list of data frames and make the given columns globally unique
#' assuming they are unique within a context.
#' E.g. if you have a list of pages containing tokens within articles, and sentence is unique
#' within an article but not globally unique, this will make it globally unique.
#' 
#' @param result list of token data frames
#' @param context name of the context column
#' @param columns names of the columns to make unique
#' @return the original list with the columns made unique
.make.pages.unique <- function(result, context="aid", columns =  c("clause_id", "source_id", "coref", "sentence")) {
  all_colnames = Reduce(function(a,b) unique(c(a,colnames(b))), init=NULL, result)
  for (col in intersect(all_colnames, columns)) {
    inc = 0
    for(i in 1:length(result)) {
      if (i%%100 == 0) message("Creating unique index for column ",col," page ", i, "/",length(result))
      if (!(col %in% colnames(result[[i]]))) {
        warning("Column ",col," not present in page ",i)
        next
      }
      if (sum(!is.na(result[[i]][[col]])) == 0) {
        warning("Column ",col," had no valid values for page ", i)
        next
      }
      result[[i]][[col]] = .make.unique(result[[i]][[context]], result[[i]][[col]]) + inc
      inc = max(result[[i]][[col]], na.rm = T) + 1
    }
  }
  result
}

.make.unique <- function(context, col)  {
  values = interaction(context, col)
  match(values, na.omit(unique(values)))
}