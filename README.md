R bindings for the AmCAT API
======

This repository contains the package `amcatr`,
which has a number of useful functions for working with the AmCAT API in R.

Installing
----

You can install `amcatr` directly from this github repository uising devtools:

```
if (!require(devtools)) {install.packages("devtools"); library(devtools)}
install_github(repo="amcat-r", username="amcat")
```

Usage
-----

To help you get started with the AmCAT API, please see the following how-to guides:

+ [Howto: Querying](howto/howto_query.md) deals with querying and metadata retrieval. 
+ [Howto: Corpus](howto/howto_corpus.md) shows how you can analyse and comparse corpora and fit LDA topic models. 
