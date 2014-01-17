amcat.getChildren <- function(hierarchy, code){
  children = as.character(hierarchy[hierarchy$parent==code,]$code)
  allchildren = children
  while(length(children) > 0){
    children = as.character(hierarchy[hierarchy$parent %in% children,]$code)
    allchildren = c(allchildren, children)
  }
  allchildren
}

amcat.aggCode <- function(hits, hierarchy, code){
  columns = c(code, as.character(amcat.getChildren(hierarchy, code)))
  occ = hits[hits$code %in% columns,]
  print(code)
  print(paste("    nr of children =", length(columns)))
  if(nrow(occ) > 0) {
    occ = tapply(occ$hits, occ$id, FUN='sum')
    occ = data.frame(id=as.numeric(names(occ)),agg_hits=occ, code=code, row.names=NULL)
  } else occ = data.frame(id=c(), agg_hits=c(), code=c())
  occ
}

amcat.aggAllCodes <- function(hits, hierarchy, codes=c()){
  if(length(codes)==0) codes = unique(c(as.character(hierarchy$parent),as.character(hierarchy$code)))
  codes = as.character(codes)
  aggscores = data.frame()
  print('Aggregating hits')
  for(code in codes){
    aggscore = amcat.aggCode(hits, hierarchy, code)
    if(nrow(aggscore)==0) next
    aggscores = rbind(aggscores, aggscore)
  }
  print('Done!')
  aggscores
}

amcat.appendAggHits <- function(hits, hierarchy){
  hits = hits[,colnames(hits)[!colnames(hits) == 'agg_hits']]
  agghits = amcat.aggAllCodes(hits, hierarchy)
  agghits = merge(agghits, hits, by=c('id','code'), all.x=T)
  agghits$hits[is.na(agghits$hits)] = 0
  agghits
}

amcat.gethierarchy <- function(conn, codebook_id, languages=NULL) {
  
  hierarchy = amcat.getobjects(conn, "codebookcode", filters=list(codebook__id=codebook_id))
  hierarchy = hierarchy[,c("code", "parent")]
  
  for (lang in languages) {
    r = amcat.getobjects(conn, "language", filters=list(label=lang))
    if (nrow(r) != 1) stop(paste("Searching for language", lang, "gave", nrow(r), "results"))
    lang_id = r$id
    labels = amcat.getobjects(conn, "label", filters=list(language__id=lang_id, 
                                                          code__codebook_codes__codebook__id=codebook_id))
    labels = labels[, c("code", "label")]
    colnames(labels) = c("code", paste("label", lang, sep="."))  
    hierarchy = merge(hierarchy, labels, all.x=T)
  }
  
  if (!is.null(languages)) {
    labels = rep(NA, nrow(hierarchy))
    for (lang in languages) {
      labelcol = paste("label", lang, sep=".")
      labels[is.na(labels)] = as.character(hierarchy[is.na(labels), labelcol])
    }
    labels = factor(labels)
    hierarchy$parent = labels[match(hierarchy$parent, hierarchy$code)]
    hierarchy$code = labels
    
  }
  
  
  
  return(hierarchy)
}

amcat.codebookcat <- function(hierarchy, depth=0) {
  # depth 0 is root, depth 1 is cat, etc

  get.ancestors <- function (hierarchy){
    p = data.frame(c=hierarchy$code, p1 = hierarchy$parent)
    for (i in 1:nrow(p)) { #nrow(p) is max number of required merges, but we'll break sooner
      m = p[, c("c", "p1")]
      colnames(m) = paste("p", c(i, i+1), sep="")
      p = merge(p, m, all.x=T)
      if (all(is.na(p[, ncol(p)]))) {
	# last column is all NA, so we are done. Drop the column and break to return
	p = p[, -ncol(p)]
	break
      }
    }
    p = p[, sort(colnames(p))]
    return(p)
  }

  anc = get.ancestors(hierarchy)
  x = rep(NA, nrow(anc))
  for (i in 1:(ncol(anc) - depth)) {
      print(paste("i",i,"ncol",ncol(anc), "depth", depth))
      col = anc[, i]
      parent = anc[, i+depth]
      x[!is.na(parent)] = as.character(col[!is.na(parent)])
  }
  
  return(x[match(hierarchy$code, anc$c)])
}


amcat.codebook.cats <- function(hierarchy, maxdepth=2) {
  for(depth in 0:maxdepth) {
    target = paste("cat", (depth+1), sep=".")
    hierarchy[, target] = amcat.codebookcat(hierarchy, depth)  
    if (depth > 0) {
      fallback = paste("cat", (depth), sep=".")
      hierarchy[is.na(hierarchy[,target]), target] = hierarchy[is.na(hierarchy[, target]), fallback]
    }
  }
  # Thanks, http://stackoverflow.com/questions/16441952/sort-a-data-frame-by-multiple-columns-whose-names-are-contained-in-a-single-obje
  sortnames = paste("cat", (0:maxdepth) + 1, sep=".")
  hierarchy[do.call("order", hierarchy[, sortnames]),]
}

amcat.getqueries <- function(queries, index) {
  result = list()
  for (i in 1:length(queries)) {
    c = as.character(index[i])                
    new = as.character(queries[i])
    if (!is.na(new)) 
      result[[c]] = c(result[[c]], new)
  }

  # combine queries to single string
  parenthesize <- function(s) {
    spaces = grepl(" ", s)
    s[spaces] =  paste("(", s[spaces], ")", sep="")
    return(s)  
  }
  combined = NULL
  for(q in names(result))
    combined = rbind(combined, c(label=q, query= paste(parenthesize(result[[q]]), collapse=" ")))
  return(as.data.frame(combined, stringsAsFactors=F))
}


