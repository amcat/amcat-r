Using AmCAT codebooks in R 
==========================

Codebooks in AmCAT are essentially hierarchical lists of codes that have one or more labels attached to them. 
We can use this feature to create a query dictionary by adding codes with a 'human' label, for example in English,
and with the keyword query to use. 

The hierarchical nature of codebooks makes it possible to have more general codes (e.g. economy) 
with below them more specific codes (e.g. inflation). 

This howto will show how to download a codebook, use it for querying articles, 
and use the hierarchical nature to classify results.

Downloading codebooks
=====================

The `amcat.gethierarchy` function can be used to download all codes and their hierarchy from a codebook:


```r
library(amcatr)
conn = amcat.connect("http://amcat.nl")
h = amcat.gethierarchy(conn, codebook_id = 374, languages = c("en", "query"))
h
```

```
##              code parent        label.en                   label.query
## 1             wmd  goals             wmd "weapons of mass destruction"
## 2 democratization  goals democratization                      democra*
## 3           goals   <NA>           goals                          <NA>
## 4           means   <NA>           means                          <NA>
## 5        military  means        military      attack* invad* invasion*
## 6         nuclear    wmd         nuclear            "nuclear weapon"~5
```


The resulting data frame has a column `code` that specifies the code and a column `parent` that specifies the parent of the code.
For example, the parent of `nuclear` is `wmd`, which has `goals` as a parent, which does not itself have a parent. 

Querying codes
==============

The functions `amcat.hits` and `amcat.aggregate` described in the Querying howto can be directly used from a codebook:


```r
counts = amcat.aggregate(conn, queries = h$label.query, labels = h$label.en, 
    sets = 10271)
counts
```

```
##   count           query
## 1    20             wmd
## 2    89 democratization
## 3   267        military
## 4     1         nuclear
```




Adding categories
=================

These parent-child relations define the hierarchy, but they are not very easy to use.
The `amcat.hierarchy.cats` function creates category variables that specify in which part of the tree a code is found:


```r
h = amcat.hierarchy.cats(h, maxdepth = 1)
h
```

```
##              code parent        label.en                   label.query
## 2 democratization  goals democratization                      democra*
## 3           goals   <NA>           goals                          <NA>
## 1             wmd  goals             wmd "weapons of mass destruction"
## 6         nuclear    wmd         nuclear            "nuclear weapon"~5
## 4           means   <NA>           means                          <NA>
## 5        military  means        military      attack* invad* invasion*
##   cat.1           cat.2
## 2 goals democratization
## 3 goals           goals
## 1 goals             wmd
## 6 goals             wmd
## 4 means           means
## 5 means        military
```


So, nuclear has 'goals' as its main category (`cat.1`), and 'wmd' as a secondary category. 
This information can be directly merged with the counts data:


```r
counts = merge(h, counts, by.x = "code", by.y = "query")
counts
```

```
##              code parent        label.en                   label.query
## 1 democratization  goals democratization                      democra*
## 2        military  means        military      attack* invad* invasion*
## 3         nuclear    wmd         nuclear            "nuclear weapon"~5
## 4             wmd  goals             wmd "weapons of mass destruction"
##   cat.1           cat.2 count
## 1 goals democratization    89
## 2 means        military   267
## 3 goals             wmd     1
## 4 goals             wmd    20
```

```r
tapply(counts$count, counts$cat.1, sum)
```

```
## goals means 
##   110   267
```


Note that if the queries have a possible overlap, it is not correct to simply add counts like this. 
It is probably a better idea to get the results per article, and then count the number of unique articles
that contain any of the codes in a specific category: 


```r
arts = amcat.hits(conn, queries = h$label.query, labels = h$label.en, sets = 10271)
head(arts)
```

```
##   count       id           query
## 1     1 81112681 democratization
## 2     1 81112682 democratization
## 3     1 81112700 democratization
## 4     1 81112703 democratization
## 5     1 81112714 democratization
## 6     1 81112727 democratization
```

```r
table(arts$query)
```

```
## 
## democratization           goals           means        military 
##              89               4               4             267 
##         nuclear             wmd 
##               1              20
```


As can be seen in the output, `amcat.hits` returns a data frame with counts per article per query.
The tabulation gives the same results as `amcat.aggregate` shown above, as would be expected. 

Now, let's merge the data, and count unique article ids per cat.1 instead:


```r
arts = merge(h, arts, by.x = "code", by.y = "query")
head(arts)
```

```
##              code parent        label.en label.query cat.1           cat.2
## 1 democratization  goals democratization    democra* goals democratization
## 2 democratization  goals democratization    democra* goals democratization
## 3 democratization  goals democratization    democra* goals democratization
## 4 democratization  goals democratization    democra* goals democratization
## 5 democratization  goals democratization    democra* goals democratization
## 6 democratization  goals democratization    democra* goals democratization
##   count       id
## 1     1 81112681
## 2     1 81112682
## 3     1 81112700
## 4     1 81112703
## 5     1 81112714
## 6     1 81112727
```

```r
tapply(arts$id, arts$cat.1, function(x) length(unique(x)))
```

```
## goals means 
##   105   269
```


Compared to the earlier results, we can see that 5 goals articles and 2 means articles were indeed overlapping. 
If you plan to do a lot of different plots, categorizations, etc. on the same basic queries and data,
it is very flexible and often more efficient to retrieve all data from AmCAT first, 
and then do the tabulating, filtering, etc. in R rather than by using `amcat.aggregate`.
