library(reshape)
library(lda)

amcat.gettokens <- function(conn, setid) {
  articles = amcat.getobjects(conn, "analysedarticlelist", article__articlesets_set__id=setid)
  
  tokens = NULL
  for (i in 1:nrow(articles)) {
    print(paste("Article",i,"/",nrow(articles)))
    aaid = articles$id[i]
    new.tokens = amcat.getobjects(conn, "token", sentence__analysed_article__id=aaid)
    if (nrow(new.tokens)) {
      new.tokens = cast(new.tokens, word ~ ., value="id", fun.aggregate="length")
      colnames(new.tokens) = c("wordid", "n")
      new.tokens$article = aaid
      tokens= rbind(tokens, new.tokens)
    }
  }     
  tokens
}

amcat.getwords <- function(conn, wordids) {
  batch = 100
  pages = ceiling(length(wordids) / batch)
  words = NULL
  for (page in 1:pages) {
    print(paste("Words page", page, "/", pages))
    
    # get words
    ids = wordids[((page-1)*batch + 1):(page*batch)]  
    ids = ids[!is.na(ids)]
    
    filters = list(pk=paste(ids, collapse="&pk="))
    new.words = amcat.getobjects(conn, "word", filters=filters)
    colnames(new.words) = c("wordid", "lemmaid", "word")
    new.words$word = as.character(new.words$word)
    
    # get lemmata
    filters = list(pk=paste(new.words$lemmaid, collapse="&pk="))
    lemmata =   amcat.getobjects(conn, "lemma", filters=filters) 
    colnames(lemmata)[1] = "lemmaid"    
    lemmata$lemma = as.character(lemmata$lemma)
    
    # add
    words = rbind(words, merge(new.words, lemmata))
  }
  words
}

amcat.getcorpus <- function(conn, setid, f.threshold=5) {
  tokens = amcat.gettokens(conn, setid)
  freqs = aggregate(tokens$n, list(wordid=tokens$wordid), sum)
  wordids = freqs$wordid[freqs$x >= f.threshold]
  tokens = tokens[tokens$wordid %in% wordids,]
  words = amcat.getwords(conn, wordids)
  colnames(freqs)[2] = "freq"
  words = merge(words, freqs, all.x=T)
  list(tokens=tokens, words=words)
}


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
