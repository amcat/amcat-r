
amcat.features <- function(conn, articleset_id, unit_level='article', posfilter=c('noun','verb','NN'), use_stemming=T, batchsize=500, min_freq=0, min_freq_perbatch=0, sample_pct=100){
  data = NULL
  offset = 0
  while(TRUE){
    output = amcat.runaction(conn, 'Features', articleset=as.integer(articleset_id), unitlevel=unit_level, posfilter=posfilter, use_stemming=use_stemming, offset=as.integer(offset),batchsize=batchsize,mindocfreq=0)
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