Extracting Source/Quote pairs using the AmCAT API
=================================================

(This howto (and source extraction in general) is work-in-progress. All feedback appreciated!)

Using graph transformations on the grammatical (dependency) structure of text, 
we can extract quotes/paraphrases and sources with reasonable accuracy 
(See e.g. [my 2013 LSE Text as data paper](http://www.kenbenoit.net/new-directions-in-analyzing-text-as-data-workshop-2013/))
Technically, extracting sources in not different from extracting tokens.
However, two strong caveats apply here:

- Currently, only Dutch is supported
- As dependency parsing takes quite long, expect *very* long load times if material has not been parsed before. 
  I would recommend preprocessing any non-trivial articleset. 
  
Getting sources
---------------

Sources can be extracted using the `amcat.gettokens` command by specifying `sources_nl` as the module and adding `sources=T` as a filter:


```r
library(amcatr)
conn = amcat.connect("http://preview.amcat.nl")
t = amcat.gettokens(conn, project = 403, articleset = 10284, module = "sources_nl", 
    filters = c(sources = T), page_size = 1, npages = 1, )
head(t, n = 10)
```

```
##        aid     lemma  pos pos1      pos_major
## 1  6403255      maar comp    C complementizer
## 2  6403255  verwacht verb    V           verb
## 3  6403255 resultaat noun    N           noun
## 4  6403255      geen  det    D     determiner
## 5  6403255      snel  adj    A      adjective
## 6  6403255        en   vg    C           conj
## 7  6403255   haak_af verb    V           verb
## 8  6403255       als comp    C complementizer
## 9  6403255      word verb    V           verb
## 10 6403255     vlieg verb    V           verb
##                             pos_minor source_id source_place       word
## 1                                root         2       source       Maar
## 2                hebben,sg,transitive         0        quote   verwacht
## 3                        het,count,pl         0        quote resultaten
## 4  geen,nwh,mod,pro,yparg,nwkpro,geen         0        quote       geen
## 5                                   e         0        quote     snelle
## 6                                  en         1        quote         en
## 7    hebben,sg1,part_intransitive(af)         2        quote       haak
## 8                                 als         2        quote        als
## 9                    unacc,pl,passive         2        quote     worden
## 10                hebben,psp,np_ld_pp         2        quote   gevlogen
##    freq
## 1     3
## 2     1
## 3     1
## 4     1
## 5     1
## 6     1
## 7     1
## 8     1
## 9     1
## 10    1
```


As can be seen above, this retrieves tokens from the specified articleset, with two additional columns: 
`source_id` and `source_place`. 
The former is the quote number within the article, and can be used to match specific quotes to specific sources.
The latter inficates whether a token is from the quote or from its source.

As before, the `keep=` argument and `pos1=` filters can be used to reduce the amount of information requested:

```r
library(amcatr)
conn = amcat.connect("http://preview.amcat.nl")
t = amcat.gettokens(conn, project = 403, articleset = 10284, module = "sources_nl", 
    filters = c(sources = T, pos1 = "M", pos1 = "N"), keep = c("aid", "lemma", 
        "pos1", "source_id", "source_place"), page_size = 1, npages = 1, )
tail(t, n = 10)
```

```
##        aid        lemma pos1 source_id source_place freq
## 11 6403255     lijk_zak    N         2        quote    2
## 12 6403255        niets    N         5        quote    2
## 13 6403255 oud_militair    N         3       source    1
## 14 6403255         paar    N         6        quote    1
## 15 6403255    resultaat    N         0        quote    1
## 16 6403255       Rwanda    M         5        quote    2
## 17 6403255         slag    N         3        quote    1
## 18 6403255    tegenslag    N         3        quote    8
## 19 6403255 vrede_missie    N         3        quote    2
## 20 6403255       wereld    N         5        quote    2
```


