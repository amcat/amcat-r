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

corpus.compare <- function(words.x, freq.x=rep(1, length(words.x)), words.y, freq.y=rep(1, length(words.y))) {
  # Compare two corpora, listen relative frequency and chi-squared
  # words should be a vector of character with the words for corpus x and y
  # freq, if given, should be a vector of the same length as words with the counts per word
  # words may be duplicated
  n.x = aggregate(freq.x, list(words.x), FUN=sum)
  colnames(n.x) = c("word", "freq.x")
  n.y = aggregate(freq.y, list(words.y), FUN=sum)
  colnames(n.y) = c("word", "freq.y")
  result = merge(n.x, n.y, all=T)
  result[is.na(result)] = 0
  result$chi = chi2(result$freq.x, result$freq.y, sum(result$freq.x) - result$freq.x, sum(result$freq.y) - result$freq.y)
  result$over = (result$freq.x / result$freq.y) / (sum(result$freq.x) / sum(result$freq.y))
  result
}

corpus.split <- function(aid, words, freq=NULL, pattern, ...) {
  selected.aid = aid[grepl(pattern, words, ...)]
  print(paste("Selected", length(selected.aid),"/", length(unique(aid)),"using pattern", pattern))
  words.x = words[aid %in% selected.aid]
  freq.x = if (is.null(freq)) NULL else freq[aid %in% selected.aid]
  words.y = words[!(aid %in% selected.aid)]
  freq.y = if (is.null(freq)) NULL else freq[!(aid %in% selected.aid)]
  list(words.x=words.x, freq.x=freq.x, words.y=words.y, freq.y=freq.y)  
}
