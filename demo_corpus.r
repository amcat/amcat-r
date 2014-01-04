library(reshape)

# get the features file. This should become an amcat call at some point!
# expects columns aid, lemma, pos, and n
f = read.csv("~/features_perart.csv")
table(f$pos)

# What are the most frequent lemmata?
words = aggregate(f$n, list(f$lemma), FUN=sum)
colnames(words) = c("word", "n")
words = words[order(-words$n), ]

# What are the most frequent nouns?
words = with(f[f$pos == 'N',], aggregate(n, list(lemma), FUN=sum))
colnames(words) = c("word", "n")
words = words[order(-words$n), ]

# compare articles with 'crimi' to articles without
c = corpus.split(f$aid, f$lemma, f$n, 'crimi')
r = corpus.compare(c$words.x, c$freq.x, c$words.y, c$freq.y)

r = r[r$freq.x > 10, ]
r = r[order(-r$chi), ]
