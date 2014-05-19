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

Note: Every time you run the `install_github` command, 
the latest version of the package is automatically installed from `github`. 


Connecting
----

Before you can use the AmCAT API, you need to connect to it using `amcat.connect`.
This requests an authentication token from the specified AmCAT server and stores it for further commands.


```r
library(amcatr)
conn = amcat.connect("http://amcat.nl", username = "example", passwd = "secret")
```

If you don't specify a username or password,
it will use the name of the logged in user and search for a password in a file `.amcatauth` in your home directory.
This file should be a csv file with one line per host, giving host, username, and password: 
`http://amcat.nl,example,secret`. 
When this is in place, you can connect without having to type the password every time 
(and without saving your password in a code file, which is usually a bad idea!): 

Usage
-----

All functions in `amcatr` start with `amcat.` and you can use the built-in help functions to view documentation. 

To help you get started with the AmCAT API, please see the following how-to guides:

+ [Howto: Querying](howto/howto_query.md) deals with querying and metadata retrieval. 
+ [Howto: Corpus](howto/howto_corpus.md) shows how you can analyse and comparse corpora and fit LDA topic models. 
