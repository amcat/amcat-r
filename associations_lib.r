associations.calculate.prob <- function(hits, weight=T, alpha = 2){
  hits[is.na(hits)] = 0
  if (weight) (1 - ((1/alpha) ^ hits)) else as.numeric(hits>0)
}

associations.association <- function(hits.a, hits.b, ...){
  # returns the association (a AND b)/a, hits.[ab] should be integer vectors of equal length
  prob.a = associations.calculate.prob(hits.a, ...)
  prob.b = associations.calculate.prob(hits.b, ...)
  sum(prob.a * prob.b) / sum(prob.a)
}

associations.per.period <- function(hits.a, hits.b, index = rep('total', length(hits.a)), ...)  {
  # index should be a 'break' variable similar to tapply that identify to which group each hit belongs
  # returns a data frame with columns groups=unique(index) and associations
  groups = unique(index)
  data.frame(group=groups, association=sapply(groups, function(i) associations.association(hits.a[index==i],hits.b[index==i], ...), USE.NAMES=F))
}

associations.from.hits <- function(hits, ids, objects, objects.from=unique(objects), objects.to = unique(objects), index=NULL, ...) {
  # calculate associations from each object in objects to each object in objects.to 
  # hits, ids, objects and index (if given) should all be vectors of equal length
  # ids identify the documents (articles), and hits are the keyword frequencies per object per id.  
  # if objects.from and objects.to are given, only calculate assocaitions from/to these objects
  # index, if given, should be a 'break' variable (see associations.per.period)
  result = NULL
  for (from in objects.from) {
    from.hits = hits[objects == from]
    for (to in objects.to[objects.to != from]) {
      # select hits for to object where id matches the ids of the from objects
      to.hits = (hits[objects == to])[match(ids[objects == from], ids[objects == to])]
      to.hits[is.na(to.hits)] = 0
      if (is.null(index))
        r = data.frame(association=associations.association(from.hits, to.hits, ...))
      else {
        r = associations.per.period(from.hits, to.hits, index=index[objects == from], ...)
      }
      r$from = from
      r$to = to
      result = rbind(result, r)
    }
  }
  return(result)
}



