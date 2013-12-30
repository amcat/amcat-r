#install.packages("lda")
#install.packages("reshape")
#install.packages("rjson")
#install.packages("RCurl")
source("amcatr.r")

conn = amcat.connect("amcat-dev.labs.vu.nl")

setid = 2182
metadata = amcat.getobjects(conn, "articlemeta",  articleset=setid)

query = "bienen*"
data = amcat.runaction(conn, "Query", articlesets=setid, query=query)

merged = merge(metadata, data, all.x=T)
merged$score[is.na(merged$score)] = 0
merged$date = as.Date(merged$date)
average = aggregate(merged$score, list(merged$date), mean)
plot(average, type="l", ylab="Average hits", xlab="Date")
proportion = aggregate(merged$score>0, list(merged$date), mean)
plot(proportion, type="l", ylab="Proportion of articles", xlab="Date")

source("amcatcluster.r")
corpus = amcat.getcorpus(conn, setid)

View(corpus$words)
names = corpus$words[corpus$words$pos == "M", ]
names = names[order(-names$freq),]
View(names)
bienen = corpus$words[grepl("^[Bb]iene", corpus$words$word),]
t = corpus$tokens
articles.bienen = unique(t$article[t$wordid %in% bienen$wordid])
t$bienen = t$article %in% articles.bienen
f = cast(t, wordid ~ bienen, fun.aggregate=sum, value="n")
colnames(f)[2:3] = c("reference", "bienen")
words = merge(corpus$words, f, all.x=T)
nouns = words[words$pos == "N", ]
nouns = nouns[order(-nouns$bienen),]
head(nouns, n=10)

words$over = ((words$bienen / words$reference) * (sum(words$reference) / sum(words$bienen)))
words$chi = chi2(words$bienen, words$reference,
                 sum(words$bienen) - words$bienen,
                 sum(words$reference) - words$reference)
verbs = words[words$pos == "V", ]
head(verbs[order(-verbs$over),], n=20)
head(verbs[order(-verbs$chi),], n=20)
tokens.bienen = t[t$bienen,]
positions = match(tokens.bienen$wordid, words$wordid)
m = lda.create.matrix(positions, tokens.bienen$n, tokens.bienen$article)
clustering = lda.cluster(m, words$word, nclusters=5)
View(top.topic.words(clustering$topics))
words.bienen = words[words$bienen > 1 & words$over > 1.1
                    & words$chi > 9.6
                    & words$pos %in% c("N","M","V","A"), ]
tokens.bienen = t[t$bienen & t$wordid %in% words.bienen$wordid,]
m = lda.create.matrix(match(tokens.bienen$wordid, words.bienen$wordid),
                      tokens.bienen$n, tokens.bienen$article)
clustering = lda.cluster(m, words.bienen$word, nclusters=5)
View(top.topic.words(clustering$topics))
