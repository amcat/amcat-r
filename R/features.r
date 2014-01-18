#' Get NLP features from AmCAT
#' 
#' Get NLP features (pos, lemma etc) from AmCAT for a selection of sets
#' 
#' @param conn the connection object from \code{\link{amcat.connect}}
#' @param articleset_id the id of the articleset to get features from
#' @param unit_level the level of aggregation, e.g. 'article' or 'sentence'
#' @param posfilter which POS to include
#' @param use_stemming stem words
#' @param batchsize the number of features (articles?) to include per call
#' @param min_freq the minimal total frequency of words
#' @param min_freq_perbatch the minimal frequency of a word per batch
#' @param sample_pct take a sample of the documents in the set
#' @return A data frame of counts per feature per unit
#' @export
amcat.features <- function(conn, articleset_id, unit_level='article', posfilter=c('noun','verb','NN'), 
                           use_stemming=T, batchsize=500, min_freq=0, min_freq_perbatch=0, sample_pct=100){
  data = NULL
  offset = 0
  while(TRUE){
    output = amcat.runaction(conn, 'Features', articleset=as.integer(articleset_id), unitlevel=unit_level, 
                             posfilter=posfilter, use_stemming=use_stemming, 
                             offset=as.integer(offset),batchsize=batchsize,mindocfreq=0)
    if(nrow(output) == 0) break
    if(min_freq_perbatch > 0) output = filterWords(output, min_freq_perbatch)
    output = output[sample(1:nrow(output), nrow(output)*(sample_pct/100)),]
    print(paste('Finished batch: articles',offset,'to',offset+batchsize))
    data = rbind(data, output)
    offset = offset + batchsize
  }
  if(min_freq > 1) data = filterWords(data, min_freq)
  data
}
