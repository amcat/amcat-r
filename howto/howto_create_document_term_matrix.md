Importing AmCAT articles as tokens and creating a document-term matrix
===============================================
  
The AmCAT API has an endpoint for downloading token frequencies per document.
This allows for corpus linguistic analyses such as frequency analysis, collocations, corpus comparisons, and topic modeling. 

For these types of text analysis (bag-of-word approaches) the common way to organize data is in a document-term matrix (DTM).
In this howto we demonstrate how to import tokens from AmCAT into R and create a DTM.

We use the DocumentTermMatrix class of the [tm](http://cran.r-project.org/web/packages/tm/vignettes/tm.pdf) package. 
Since the `tm` package is quite popular, its DocumentTermMatrix class is compatible with various packages for text analysis.

Requesting tokens
-----------------

When downloading token frequencies, one usually wants lemmatized and/or POS-tagged tokens.
This means that the word 'means' is analysed as a verb (POS) with lemma mean.
Technically, this linguistic processing is performed by AmCAT using [xTas](www.xtas.net), 
which means that the first time a document is requested with a certain analysis it is cached,
so asking the same document again does not cause processing to be repeated.

Tokens are requested from the `projects/articleset/ID/tokens` endpoint, 
specifying the set and analysis module to use. 
Currently, the useful modules are `corenlp_lemmatize` for English (default) and `tadpole` for Dutch. 

*Note*: As of May 2014, the vocabulary analysis features are not yet available through 
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
  ##      word  pos lemma      aid pos1 freq
  ## 233  were  VBD    be 81112681    V    4
  ## 234 which  WDT which 81112681    D    2
  ## 235 White  NNP White 81112681    M    2
  ## 236   who   WP   who 81112681    O    3
  ## 237  will   MD  will 81112681    V    1
  ## 238  with   IN  with 81112681    P    1
  ## 239   WMD  NNP   WMD 81112681    M    2
  ## 240 world   NN world 81112681    N    1
  ## 241   you  PRP   you 81112681    O    1
  ## 242  your PRP$   you 81112681    O    2
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
    page_size = 100, npages = 100, keep = c("aid", "lemma"), filters = c(pos1 = "N", 
        pos1 = "M", pos1 = "V", pos1 = "M"))
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



Document-Term Matrix
--------------------

The token data as imported above can easily be transformed into a dtm with the `amcat.dtm.create` function.
The input of the function is a vector for the tokens (terms), together with a vector indicating in which documents the token occured (ids) and how often (freqs).


```r
dtm = amcat.dtm.create(ids = t$aid, terms = t$lemma, freqs = t$freq)
dtm
```

```
## A document-term matrix (674 documents, 10707 terms)
## 
## Non-/sparse entries: 68649/7147869
## Sparsity           : 99%
## Maximal term length: 369 
## Weighting          : term frequency (tf)
```


Match document meta
--------------------

For many purposes it is usefull to include the document meta, 
such as the medium in which an article was published and the date at which it was published. 

The document meta can be imported from amcat with the `amcat.getarticlemeta` function. 


```r
meta = amcat.getarticlemeta(conn, set = 10271, dateparts = T)
```

```
## GET http://preview.amcat.nl/api/v4/articlemeta?articleset=10271&page_size=1000&format=csv&page=1
## GET http://preview.amcat.nl/api/v4/medium?pk=989907784&page_size=1000&format=csv&page=1
```

```r
head(meta)
```

```
##         id       date   medium length       year      month       week
## 1 81112681 2013-08-29 Wikinews    406 2013-01-01 2013-08-01 2013-08-26
## 2 81112682 2012-01-20 Wikinews    149 2012-01-01 2012-01-01 2012-01-16
## 3 81112683 2011-02-06 Wikinews    486 2011-01-01 2011-02-01 2011-01-31
## 4 81112684 2010-09-23 Wikinews    314 2010-01-01 2010-09-01 2010-09-20
## 5 81112685 2013-08-30 Wikinews    394 2013-01-01 2013-08-01 2013-08-26
## 6 81112686 2009-08-12 Wikinews    244 2009-01-01 2009-08-01 2009-08-10
```



```r
meta = meta[match(rownames(dtm), meta$id), ]
dim(dtm)
```

```
## [1]   674 10707
```

```r
dim(meta)
```

```
## [1] 674   7
```


