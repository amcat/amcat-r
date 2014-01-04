library(reshape)

# get the features file. This should become an amcat call at some point!
# expects columns aid, lemma, pos, and n
f = read.csv("~/features_perart.csv")

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


# feature selection for lda
f2 = f[f$aid %in% f$aid[grepl('crimi', f$lemma)], ]
words = corpus.freqs(f2$aid, f2$lemma, f2$n)
words = merge(words, r, all.x=T)
words = words[words$tf > 5 &  words$over > 1 & words$chi > 10, ]
words = words[words$df.prop < .25 | words$chi > 100, ]

# do lda (requires lda_lib)
features = f2[f2$lemma %in% words$word,]
ldamatrix = lda.create.matrix(match(features$lemma, words$word), features$n, features$aid)
m = lda.cluster(ldamatrix, words$word, nclusters=30, niterations=100)

View(top.topic.words(m$topics))

