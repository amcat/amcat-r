library(lda)
library(reshape)

lda.create.matrix <- function(wordnrs, freqs, documents) {
  # wordnrs = vector of word indexes into voca
  # freqs = vector of frequencies of the same length as words
  # documents = vector of documents indices that has the samen length as words
  # voca = character vector of words
  
  docs = unique(documents)
  
  n = length(docs)
  corpus = vector("list", n)
  i = 2
  for (i in 1:n) {
    if (i%%100 == 0) print(paste("Document",i," / ", n))
    
    select = documents == docs[i]
    
    corpus[[i]] = matrix(as.integer(c(wordnrs[select] - 1, freqs[select])), nrow=2, byrow=T)
  }
  corpus
}

lda.cluster <- function(corpus, voca, nclusters = 25, niterations=25) {
  lda.collapsed.gibbs.sampler(corpus,
                              nclusters,
                              voca,
                              niterations,
                              0.1,
                              0.1,
                              compute.log.likelihood=TRUE)
}

chi2 <- function(a,b,c,d) {
  # Compute the chi^2 statistic for a 2x2 crosstab containing the values
  # [[a, b], [c, d]]
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

fixUnitId <- function(data){
  ## if unit_level is 'paragraph' or 'sentence', merge parnr/sentnr into article id 
  if('paragraph' %in% colnames(data) | 'sentence' %in% colnames(data)){ 
    data$id = as.character(data$id)
    if('paragraph' %in% colnames(data)) data$id = paste(data$id,data$paragraph, sep='-')
    if('sentence' %in% colnames(data)) data$id = paste(data$id,data$sentence, sep='-')  
    data$id = as.factor(data$id)
  }
  data$id
}

lda.prepareFeatures <- function(features, features.reference=data.frame(), docfreq.thres=5, docfreq_pct.max=50, over.thres=1.5, chi.thres=5, use.pos=c()) {
  ## prepares data for LDA. 
  ## A reference set can be given in order to filter words based on frequency difference
  ## docfreq.thres and docfreq_pct.max apply to features set only
  ## use docfreq.thres to filter words on a minimum nr of documents in which they occur
  ## use docfreq_pct.max to filter words on the maximum percentage of documents in the corpus
  features$id = fixUnitId(features)
  
  if (docfreq_pct.max < 100 | docfreq.thres > 0) {
    print('Calculating document frequency')
    docfreq = aggregate(hits ~ word, features, FUN='length') # in how many documents does a word occur?
    too_rare = docfreq$word[docfreq$hits < docfreq.thres]
    docfreq$docfreq_pct = (docfreq$hits / length(unique(features$id))) * 100
    too_common = docfreq$word[docfreq$docfreq_pct > docfreq_pct.max]
    print(paste('  ', length(too_rare), 'words are too rare (< docfreq.thres)'))
    print(paste('  ',length(too_common), 'words are too common (> docfreq_pct.max)'))
  }
  
  print('Selecting vocabulary')
  if (length(use.pos) > 0) {
    print(paste('  ', 'Only using words with POS tag:', paste(use.pos,collapse=', ')))
    features = features[features$pos %in% use.pos,]
    if(nrow(features.reference) > 0) features.reference = features.reference[features.reference$pos %in% use.pos,]
  }
  
  if(nrow(features.reference) > 0){
    features.all = rbind(features, features.reference)
    features.all$source = c(rep('target', nrow(features)), rep('reference', nrow(features.reference)))
  
    words = cast(features.all, word + pos ~ source, value="hits", fun.aggregate=sum)
    if (docfreq.thres > 0) words = words[!words$word %in% too_rare,]
    if (docfreq_pct.max < 100) words = words[!words$word %in% too_common,]
    
    words$chi = chi2(words$target, words$reference, sum(words$target) - words$target, sum(words$reference) - words$reference)
    words$over = (words$target / words$reference) / (sum(words$reference) / sum(words$target))
  
    voca = words[words$over > over.thres & words$chi > chi.thres,]
    voca = voca[order(voca$over),]
  } else {
    words = aggregate(hits ~ word + pos, features, FUN='sum')
    if (docfreq.thres > 0) words = words[!words$word %in% too_rare,]
    if (docfreq_pct.max < 100) words = words[!words$word %in% too_common,]
    voca = words
  }
  
  print(paste('  ','Vocabulary size =', nrow(voca)))
  print('Building matrix')
  features = features[features$word %in% voca$word,]
  ldamatrix = lda.create.matrix(match(features$word, voca$word), features$hits, features$id)
  list(matrix=ldamatrix, voca=voca, article_ids=unique(features$id))
}
