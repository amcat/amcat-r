Vocabulary Analysis and LDA using the AmCAT API
===============================================

The AmCAT API has an endpoint for downloading token frequencies per document.
This allows for corpus linguistic analyses such as frequency analysis, collocations, 
corpus comparisons, and topic modeling. 

When downloading token frequencies, one usually wants lemmatized and/or POS-tagged tokens.
This means that the word 'means' is analysed as a verb (POS) with lemma mean.
Technically, this linguistic processing is performed by AmCAT using [xTas](www.xtas.net), 
which means that the first time a document is requested with a certain analysis it is cached,
so asking the same document again does not cause processing to be repeated.

Requesting tokens
-----------------

Tokens are requested from the `projects/articleset/ID/tokens` endpoint, 
specifying the set and analysis module to use. 
Currently, the useful modules are `corenlp_lemmatize` for English (default)
and `tadpole` for Dutch. 

*Note*: As of April 2014, the vocabulary analysis features are not yet available through 
the production version of amcat (release 3.3). 
However, [preview.amcat.nl](http://preview.amcat.nl)
gives access to the same database using the newest development version of AmCAT.
Therefore, this howto connects to that server rather than the default server:


```r
library(amcatr)
conn = amcat.connect("http://preview.amcat.nl")
t = amcat.gettokens(conn, project = 442, articleset = 10271, module = "corenlp_lemmatize", 
    page_size = 1, npages = 1)
tail(t, n = 10)
```

```
##          aid   lemma  pos pos1    word freq
## 233 81112681 website   NN    N website    1
## 234 81112681   which  WDT    D   which    2
## 235 81112681   White  NNP    M   White    2
## 236 81112681     who   WP    O     who    3
## 237 81112681    will   MD    V    will    1
## 238 81112681    with   IN    P    with    1
## 239 81112681     WMD  NNP    M     WMD    2
## 240 81112681   world   NN    N   world    1
## 241 81112681     you  PRP    O     you    1
## 242 81112681     you PRP$    O    your    2
```


The command displayed above requests the tokens for a single article by setting `page_size` to 1 and requesting only a single page.
In the output you can see that it gives the frequency per word per article, and gives lemma and pos information. 
For example, the last word `your` has lemma `you` and POS possessive pronoun (`PRP$`). 
The `pos1` column gives a simplified POS tag, with `N` for nouns, `V` for verbs, `A` for adjectives, and `M` for proper names. 

If we want to get the term frequencies for a larger set of articles, 
it can be useful to select only the data we really need. 
The following command requests all tokens for the publicly available set 10271 
containing wikinews articles on Iraq.
It filters on 'substantive' POS tags N (noun), M (name), V (verb), and A (adjective), 
and only downloads the lemma per article:


```r
t = amcat.gettokens(conn, project = 442, articleset = 10271, module = "corenlp_lemmatize", 
    page_size = 100, keep = c("aid", "lemma"), filters = c(pos1 = "N", pos1 = "M", 
        pos1 = "V", pos1 = "M"))
head(t, n = 10)
```

```
##         aid          lemma freq
## 1  81112681       accuracy    1
## 2  81112681 administration    6
## 3  81112681        America    1
## 4  81112681         answer    2
## 5  81112681         anyone    3
## 6  81112681            ask    2
## 7  81112681             be   19
## 8  81112681           blog    1
## 9  81112681           Bush    5
## 10 81112681            can    1
```


Term-document matrix
--------------------

The feature list (or actually: feature data frame) created above can be analysed directly,
for example by listing the most frequent words:


```r
wordfreqs = aggregate(t$freq, by = list(word = t$lemma), FUN = sum)
wordfreqs = wordfreqs[order(-wordfreqs$x), ]
head(wordfreqs)
```

```
##          word    x
## 390        be 6589
## 561  Category 4202
## 1431     have 2397
## 1636     Iraq 2156
## 2734      say 1517
## 3255   United  707
```


However, for most purposes it is better to first create a document-term matrix of the standard form defined in the `tm` package:


```r
dtm = amcat.dtm.create(t$aid, t$lemma, t$freq)
```


Since the resulting document-term matrix object is defined in the widely used `tm` package, 
a large number of existing packages and functions can be used to analyse it. 

Term Frequency
--------------

To make it easy to select words on term frequency and/or inverse document frequency,
we created a function to calculate a number of useful term statistics on a dtm object:


```r
terms = amcat.term.statistics(dtm)
head(terms, n = 10)
```

```
##                          term characters number nonalpha termfreq docfreq
## accuracy             accuracy          8  FALSE    FALSE        6       6
## administration administration         14  FALSE    FALSE       81      51
## America               America          7  FALSE    FALSE      203     179
## answer                 answer          6  FALSE    FALSE       23      19
## anyone                 anyone          6  FALSE    FALSE       17      14
## ask                       ask          3  FALSE    FALSE      113      87
## be                         be          2  FALSE    FALSE     6589     634
## blog                     blog          4  FALSE    FALSE        9       3
## Bush                     Bush          4  FALSE    FALSE      311     115
## can                       can          3  FALSE    FALSE      171     116
##                   tfidf
## accuracy       0.036326
## administration 0.029439
## America        0.015922
## answer         0.031095
## anyone         0.031019
## ask            0.020624
## be             0.005354
## blog           0.109372
## Bush           0.040001
## can            0.017838
```


For every term, the term frequency, document frequency, and tf-idf is given. 
Moreover, the number of characters in the word is given and whether the word contains a number or non-alphanumeric character.
These latter variables are useful to filter out nonsense terms, such as the %-sign in the list above.
For example, the following selects all words with at least three characters, not containing a number of symbol and 
with a term frequency of at least 10, and sorts them by td-idf:


```r
voca = terms[!terms$number & !terms$nonalpha & terms$termfreq > 10 & terms$characters >= 
    3, ]
voca = voca[order(voca$tfidf), ]
head(voca, n = 10)
```

```
##              term characters number nonalpha termfreq docfreq    tfidf
## Iraq         Iraq          4  FALSE    FALSE     2156     634 0.002162
## source     source          6  FALSE    FALSE      678     545 0.002974
## politics politics          8  FALSE    FALSE      520     512 0.003675
## conflict conflict          8  FALSE    FALSE      538     515 0.003680
## have         have          4  FALSE    FALSE     2397     577 0.005515
## East         East          4  FALSE    FALSE      444     433 0.006173
## Middle     Middle          6  FALSE    FALSE      445     433 0.006179
## Category Category          8  FALSE    FALSE     4202     624 0.006291
## United     United          6  FALSE    FALSE      707     409 0.008642
## say           say          3  FALSE    FALSE     1517     483 0.008674
```


These terms are presumably the more 'informative' terms in the vocabulary.

Comparing corpora
-----------------

It can be interesting to compare vocabulary use in two corpora. 
For example, lets split our corpus into those articles that mention Bush and those that do not:


```r
ncol(dtm)
```

```
## [1] 10707
```

```r
w = as.matrix(dtm[, "Bush"])
dtm.bush = dtm[w > 0, ]
dtm.rest = dtm[w == 0, ]
```



Now, we can compute and compare the term frequencies for both:


```r
terms.bush = amcat.term.statistics(dtm.bush)
terms.rest = amcat.term.statistics(dtm.rest)
freqs.rest = terms.rest[, c("term", "termfreq")]
terms.bush = merge(terms.bush, freqs.rest, all.x = TRUE, by = "term")
terms.bush[is.na(terms.bush)] = 0
head(terms.bush)
```

```
##   term characters number nonalpha termfreq.x docfreq   tfidf termfreq.y
## 1               0  FALSE    FALSE          1       1 0.01956          1
## 2   >>          2  FALSE     TRUE          2       2 0.05667          0
## 3    |          1  FALSE     TRUE        138      51 0.02090        519
## 4   __          2  FALSE    FALSE          2       2 0.01664          8
## 5    *          1  FALSE     TRUE         16      16 0.01744         64
## 6    %          1  FALSE     TRUE         71      14 0.05040         98
```


`terms.bush` now contains the term statistics in the articles mentioning bush, 
as well as the frequency in the reference corpus consisting of the other articles.
We can now compute and sort by the overrepresentation of terms:


```r
terms.bush$relfreq.x = terms.bush$termfreq.x/sum(terms.bush$termfreq.x)
terms.bush$relfreq.y = terms.bush$termfreq.y/sum(terms.rest$termfreq)
terms.bush$over = terms.bush$relfreq.x/(terms.bush$relfreq.y + 0.001)
terms.bush = terms.bush[order(-terms.bush$over), ]
head(terms.bush, n = 10)
```

```
##                term characters number nonalpha termfreq.x docfreq    tfidf
## 541            Bush          4  FALSE    FALSE        311     115 0.000000
## 2830      President          9  FALSE    FALSE        177      82 0.005675
## 1531         George          6  FALSE    FALSE         92      65 0.007598
## 3920             W.          2  FALSE     TRUE         78      59 0.008545
## 3937            war          3  FALSE    FALSE        155      54 0.013838
## 80   administration         14  FALSE    FALSE         67      39 0.013215
## 1712          House          5  FALSE    FALSE         78      44 0.012347
## 2310           memo          4  FALSE    FALSE         49      13 0.047050
## 793        Congress          8  FALSE    FALSE         53      26 0.023120
## 3983          White          5  FALSE    FALSE         52      29 0.016142
##      termfreq.y relfreq.x relfreq.y   over
## 541           0  0.013227 0.0000000 13.227
## 2830         75  0.007528 0.0008940  3.975
## 1531         14  0.003913 0.0001669  3.353
## 3920          5  0.003317 0.0000596  3.131
## 3937        131  0.006592 0.0015615  2.574
## 80           14  0.002849 0.0001669  2.442
## 1712         44  0.003317 0.0005245  2.176
## 2310          9  0.002084 0.0001073  1.882
## 793          18  0.002254 0.0002146  1.856
## 3983         19  0.002212 0.0002265  1.803
```


This gives a list of the words that occur 'too much' in the articles mentioning Bush,
or in other words the collocates of the word 'Bush'.
To make this easier, and to also provide statistical association measures such as chi-squared,
the function `amcat.compare.corpora` is provided. 
The following example selects all words that are underrepresented in the `bush` corpus,
and sorts them by chi-squared:


```r
terms = amcat.compare.corpora(dtm.bush, dtm.rest)
terms = terms[terms$over < 1, ]
terms = terms[order(-terms$chi), ]
head(terms)
```

```
##         term termfreq.x termfreq.y relfreq.x relfreq.y   over   chi
## 3406 soldier         22        326 0.0009357  0.003886 0.3962 49.49
## 2052    kill         58        512 0.0024667  0.006103 0.4881 46.00
## 304   attack         45        415 0.0019138  0.004947 0.4900 39.61
## 362  Baghdad         69        532 0.0029345  0.006341 0.5360 38.31
## 2779  police          8        193 0.0003402  0.002300 0.4061 37.78
## 643   charge          7        148 0.0002977  0.001764 0.4695 27.41
```


What can be seen from these two word lists is that the articles mentioning Bush are more political in nature,
while the other articles describe more (military) action. 

Of course, this can also be used to compare e.g. vocabulary differences between newspapers, speakers, periods, etc.
For example, the following uses the article metadata to compare vocabulary after 2012 with the vocabulary before that date.
Note that `rownames(dtm)` contains the article ids (as a character vector),
so by matching the ids in the meta file with the (numeric) row names we can put the years in the same order as the rows of `dtm`.


```r
meta = amcat.getarticlemeta(conn, set = 10271, dateparts = T)
years = meta$year[match(meta$id, as.numeric(rownames(dtm)))]
dtm.before = dtm[years < as.Date("2012-01-01"), ]
dtm.after = dtm[years >= as.Date("2012-01-01"), ]
terms = amcat.compare.corpora(dtm.after, dtm.before)
terms = terms[order(-terms$chi), ]
head(terms[terms$over > 1, ])
```

```
##        term termfreq.x termfreq.y relfreq.x relfreq.y  over    chi
## 3812  Islam        230          4  0.003708 8.813e-05 4.327 158.02
## 7006  Sunni        196          0  0.003160 0.000e+00 4.160 143.70
## 7759 weapon        138         19  0.002225 4.186e-04 2.273  58.60
## 7422 Turkey         83          3  0.001338 6.610e-05 2.193  53.02
## 7         %        142         27  0.002290 5.949e-04 2.063  47.91
## 5287  Party        109         16  0.001757 3.525e-04 2.039  44.50
```

```r
head(terms[terms$over < 1, ])
```

```
##            term termfreq.x termfreq.y relfreq.x relfreq.y   over   chi
## 6747    soldier        127        221 2.048e-03 0.0048691 0.5193 64.60
## 2836       Ford          1         40 1.612e-05 0.0008813 0.5401 51.41
## 5851       rape          2         36 3.225e-05 0.0007932 0.5757 42.91
## 4843     murder         24         68 3.870e-04 0.0014982 0.5552 37.82
## 3925 journalist         57        108 9.190e-04 0.0023795 0.5679 36.44
## 3604   incident         41         88 6.611e-04 0.0019388 0.5652 35.67
```


So, the later articles mention the scandals with Abu Ghraib and phosphorus munition more frequently, 
while the earlier articles metion the constitution and refugees. 

Topic modeling
--------------

An interesting technique to use on a document-term matrix is that of LDA topic modeling using the r `lda` package. 
Topic modeling essentially reduces the dimensionality of the word space 
by assuming that each document contains a number of (latent) topics, which in turn contain a number of words (Blei et al, JMLR 2003). 

Before fitting the topic model, it is best to reduce the vocabulary by selecting only informative words.
Then, we can fit a topic model using the `amcatr.lda.fit` function, which requires a dtm object and the number of topics:


```r
terms = amcat.term.statistics(dtm)
voca = terms[!terms$number & !terms$nonalpha & terms$termfreq > 10 & terms$characters >= 
    3 & terms$tfidf > 0.05, ]
dtm = dtm[, colnames(dtm) %in% voca$term]
m = amcat.lda.fit(dtm, 5)
```


The `m` object is the standard object returned by the `lda` package, 
which also provides functions for inspecting its contents:


```r
top.topic.words(m$topics)
```

```
##       [,1]        [,2]        [,3]         [,4]             
##  [1,] "Abu"       "election"  "Iran"       "journalist"     
##  [2,] "abuse"     "video"     "blast"      "Turkey"         
##  [3,] "Ghraib"    "Blair"     "resolution" "Green"          
##  [4,] "crash"     "oil"       "center"     "hostage"        
##  [5,] "Australia" "Basra"     "Romania"    "raid"           
##  [6,] "bill"      "poll"      "execution"  "Fallujah"       
##  [7,] "Ford"      "memo"      "Syria"      "truck"          
##  [8,] "Senator"   "sailor"    "company"    "gunman"         
##  [9,] "girl"      "Obama"     "marine"     "mosque"         
## [10,] "rape"      "Kurdistan" "embassy"    "son"            
## [11,] "Reid"      "Nay"       "jail"       "Carroll"        
## [12,] "Marines"   "Air"       "Petraeus"   "PKK"            
## [13,] "Group"     "Royal"     "WikiNews"   "Workers"        
## [14,] "Rights"    "Annan"     "brigade"    "phosphorus"     
## [15,] "convoy"    "Sadr"      "Muqtada"    "Zone"           
## [16,] "seat"      "voter"     "Musab"      "style"          
## [17,] "Harry"     "Gates"     "tape"       "Allawi"         
## [18,] "device"    "policeman" "Maliki"     "throw"          
## [19,] "Jordan"    "roadside"  "Tehran"     "Cheney"         
## [20,] "flight"    "crew"      "Basescu"    "Representatives"
##       [,5]          
##  [1,] "vote"        
##  [2,] "helicopter"  
##  [3,] "category"    
##  [4,] "constitution"
##  [5,] "accident"    
##  [6,] "figure"      
##  [7,] "Mosul"       
##  [8,] "Brown"       
##  [9,] "CIA"         
## [10,] "Rumsfeld"    
## [11,] "Talabani"    
## [12,] "Diyala"      
## [13,] "Muslims"     
## [14,] "flag"        
## [15,] "shooting"    
## [16,] "Sgrena"      
## [17,] "Human"       
## [18,] "Italy"       
## [19,] "City"        
## [20,] "align"
```


A final function provided by the `amcatr` package is the `amcat.lda.topics.per.document`,
which gives a data frame containing all articles and the topics occurring in each article.
By merging this with the metadata we can see which articles contained each topic:


```r
docs = amcat.lda.topics.per.document(m)
meta = amcat.getarticlemeta(conn, set = 10271, dateparts = T)
docs = merge(meta, docs, all.y = T)
head(docs)
```

```
##         id       date   medium length       year      month       week X1
## 1 81112681 2013-08-29 Wikinews    406 2013-01-01 2013-08-01 2013-08-26  0
## 2 81112682 2012-01-20 Wikinews    149 2012-01-01 2012-01-01 2012-01-16  8
## 3 81112683 2011-02-06 Wikinews    486 2011-01-01 2011-02-01 2011-01-31  2
## 4 81112684 2010-09-23 Wikinews    314 2010-01-01 2010-09-01 2010-09-20  0
## 5 81112685 2013-08-30 Wikinews    394 2013-01-01 2013-08-01 2013-08-26  0
## 6 81112686 2009-08-12 Wikinews    244 2009-01-01 2009-08-01 2009-08-10  7
##   X2 X3 X4 X5
## 1  3  3  0  1
## 2  1  0  3  2
## 3  0  3  1  0
## 4  0  0  3  4
## 5  3  5  2  2
## 6  1  0  0  9
```


And we can use this to e.g. plot the topic use over time:


```r
topics.per.year = aggregate(docs[, c("X1", "X2", "X3", "X4", "X5")], by = list(year = docs$year), 
    FUN = sum)
topics.per.year$total = rowSums(topics.per.year[-1])
rel = topics.per.year[2:6]/topics.per.year$total

plot(topics.per.year$year, topics.per.year$X1, type = "n", ylim = c(min(rel), 
    max(rel)), ylab = "Topic frequency", xlab = "Year", frame.plot = F)
for (i in 1:5) lines(topics.per.year$year, rel[, i], col = rainbow(5)[i])
legend("topleft", legend = colnames(rel), lwd = 2, col = rainbow(5))
```

![plot of chunk lda-plot](figure/lda-plot.png) 

