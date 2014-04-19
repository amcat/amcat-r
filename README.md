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

See [Howto: Querying](howto/howto_query.md) for an introduction to querying and metadata retrieval. 
