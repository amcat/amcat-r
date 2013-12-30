# article meta
amcat.getMeta <- function(conn, articlesets, media=c(), from_date=NA, to_date=NA, filter_columns=c('id','date','medium','length'), time=F){
  meta = NULL
  if(length(media) == 0) media = c(NA)
  
  for(medium in media) {
    filters = list(articleset=articlesets)
    if(!is.na(medium)) filters = c(filters, medium=medium)
    if(!is.na(from_date)) filters = c(filters, date_from=from_date)
    if(!is.na(to_date)) filters = c(filters, date_to=to_date)
    
    ameta = amcat.getobjects(conn, "articlemeta", filters=filters, use__in=c('articleset'))
    if(nrow(ameta) == 0) next
    if(length(filter_columns > 0)) ameta = ameta[,filter_columns]
    meta = rbind(meta,ameta)
  }
  if(time == T) meta$date = as.POSIXct(meta$date, format='%Y-%m-%d %H:%M:%S') else meta$date = as.Date(meta$date, format='%Y-%m-%d')
  meta
}

filterWords <- function(data, min_freq){
  agg = aggregate(hits ~ word, data, FUN='sum')
  agg = agg[agg$hits >= min_freq,]
  data[data$word %in% agg$word,]
}

amcat.getFeatures <- function(conn, articleset_id, unit_level='article', posfilter=c('noun','verb','NN'), use_stemming=T, batchsize=500, min_freq=0, min_freq_perbatch=0, sample_pct=100){
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

amcat.getMediumString <- function(mediumids, formulaproof=T){
  mediumindex = amcat.getobjects(conn, "medium", filters=list(pk=unique(mediumids)),use__in=c('pk'))
  medium_strings = mediumindex$name[match(mediumids,mediumindex$id)]
  if(formulaproof == T) {
    problemcharacters = c(' ',':','.','-')
    for(pc in problemcharacters) medium_strings = gsub(pc,'_',medium_strings, fixed=T)
  }
  medium_strings
}

amcat.getMetaForArticlelist <- function(conn, articleids, output_fields=c('id','date','medium')){
  meta = NULL
  batch_count = 1
  setsize = length(articleids)
  while(TRUE){
    batch = articleids[batch_count:(batch_count+99)]
    print(paste(batch_count, '/', setsize))
    batch_count = batch_count + 100
    batch = batch[!is.na(batch)]  
    if(length(batch) == 0) break
    
    output = amcat.getobjects(conn=conn, resource='articlemeta', format='csv', filters = list(pk=batch), use__in=c('pk')) 
    output = output[,output_fields]
    meta = rbind(meta, output)
    
    if(length(batch) < 100) break
  }
  meta
}

amcat.getHits <- function(conn, queries, articlesets, mediumids=NULL){
  hits = NULL
  if(!class(queries) == 'data.frame') {
    getQueryLabel <- function(x) strsplit(as.character(x), '#')[[1]][1]
    queries=data.frame(query=queries,code=sapply(queries, getQueryLabel))
  }
  
  for (i in 1:nrow(queries)) {
    query = as.character(queries$query[i])
    code = as.character(queries$code[i])
    print(code)
    if(query == '') next
    
    if(length(grep('#',query)) == 1) query = strsplit(query, '#')[[1]][2]
    query = paste("code",'# ',query,sep='') # to prevent codes too long for query labels
    
    # h = amcat.runaction(conn, 'Query', articlesets=articlesets, query=query) # outdated
    filters = list(sets=articlesets, q=query, col='hits')
    if(!is.null(mediumids)) filters = c(filters, mediumids=mediumids)
    h = amcat.getobjects(conn, "search", filters=filters, use__in=c('sets', 'mediumids'))
    
    if(nrow(h) == 0){
      print('   Zero hits')
      next
    }
    colnames(h) = c('id','hits')
    h$code = code
    hits = rbind(hits, h)
  }
  hits$code = as.factor(hits$code)
  hits
}

# words and tokens
amcat.getTokens <- function(conn, setid, articleids=F) {
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
      if(articleids==T) new.tokens$article_id = articles$article[i]
      tokens= rbind(tokens, new.tokens)
    }
  }     
  tokens
}

amcat.getWords <- function(conn, wordids) {
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

amcat.getCorpus <- function(conn, setid, f.threshold=5) {
  tokens = amcat.getTokens(conn, setid)
  freqs = aggregate(tokens$n, list(wordid=tokens$wordid), sum)
  wordids = freqs$wordid[freqs$x >= f.threshold]
  tokens = tokens[tokens$wordid %in% wordids,]
  words = amcat.getWords(conn, wordids)
  colnames(freqs)[2] = "freq"
  words = merge(words, freqs, all.x=T)
  list(tokens=tokens, words=words)
}

# codebook

amcat.getCodeLabel <- function(code, language=NA, query_language=13){
  label = amcat.getobjects(conn, "label", filters=list(code__id=code), use__in=c('code__id'))
  if(!is.na(language)) label=as.character(label$label[label$language==language]) else label=as.character(label$label[!label$language==query_language][1])
}

amcat.getCodeQuery <- function(code, query_language=13) {
  query = amcat.getobjects(conn, "label", filters=list(language__id=query_language, code__id=code), use__in=c('code__id'))$label
  if(is.null(query)) query = ''
  as.character(query)
}

amcat.getCodebook <- function(conn, codebook_id, language=NA, query_language=13){
  codebook = amcat.getobjects(conn, "codebookcode", filters=list(language__id=language, codebook__id=codebook_id))
  codebook = codebook[,c("code", "parent")]
  codebook$label = sapply(codebook$code, amcat.getCodeLabel)
  colnames(codebook) = c('code_id','parent_id','code')
  
  hierarchy = merge(codebook, data.frame(code_id=codebook$code_id, parent=codebook$code), by.x='parent_id', by.y='code_id')
  queries = codebook[,c('code_id','code')]
  queries$query = sapply(queries$code_id, amcat.getCodeQuery)
  
  list(hierarchy=hierarchy, queries=queries)
}

amcat.getHierarchy <- function(codebook_id, password, username, host) {
  labels = amcat.getobjects("label",  password, username, host=host, filters=list(language=2))
  
  hierarchy = amcat.getobjects("codebookcode", password, username, host=host, filters=list(codebook_id=codebook_id))
  hierarchy = hierarchy[,c("X_code", "X_parent")]
  
  hierarchy = merge(hierarchy, labels, by.x="X_code", by.y="code", all.x=T)[,c("X_code", "label", "X_parent")]
  colnames(hierarchy) = c("code_id", "code", "parent_id")
  hierarchy = merge(hierarchy, labels, by.x="parent_id", by.y="code", all.x=T)[,c("code_id", "code", "label")]
  colnames(hierarchy) = c("code_id", "code", "parent_0")
  
  for (i in 1:4) {
    h = hierarchy[, c("code", "parent_0")]
    colnames(h) = c(paste("parent", i-1, sep="_"), paste("parent", i, sep="_"))
    hierarchy = merge(hierarchy, h, all.x=T)
  }
  
  i = 1
  columns = c("code", paste("parent", 0:4, sep="_"))
  hierarchy$cat = NA
  for (i in 1:length(columns)) {
    col = hierarchy[, columns[i]]
    hierarchy$root[!is.na(col)] = as.character(col[!is.na(col)])
    if (i < length(columns)) {
      parent = hierarchy[, columns[i+1]]
      hierarchy$cat[!is.na(parent)] = as.character(col[!is.na(parent)])
    }
  }
  
  hierarchy[, c("code_id", "code", "cat", "root")]
}