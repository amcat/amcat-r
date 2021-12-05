R bindings for the AmCAT API
======

This repository contains the package `amcatr`,
which has a number of useful functions for working with the AmCAT API in R.

Installing
----

You can install `amcatr` directly from this github repository uising `remotes`:

```
remotes::install_github("amcat/amcat-r")
```

Note: Every time you run the `install_github` command, 
the latest version of the package is automatically installed from `github`. 


Connecting
----

Before you can connect to the API, you need to store the password in a file "~/.amcatauth".
This can be done from R by calling `amcat.save.password`:

```r
library(amcatr)
amcat.save.password("https://amcat.nl", username="example", password="secret")
```

Note: You only need to run this line once per computer, so *do not save this line* in a script, and certainly do not publish or email it, to avoid compromising the security of your account!

Now, you can connect to AmCAT using `amcat.connect`.
This requests an authentication token from the specified AmCAT server and stores it for further commands.
The resulting data (named `conn`, below) is then used every time you call the API. 

```r
library(amcatr)
conn = amcat.connect("https://amcat.nl")
```

Usage
-----

All functions in `amcatr` start with `amcat.` and you can use the built-in help functions to view documentation. 

To help you get started with the AmCAT API, please see the following how-to guides:

+ [Howto: Querying](howto/howto_query.md) deals with querying and metadata retrieval. 
+ [Howto: DTM](howto/howto_create_document_term_matrix.md) shows how you can create a document term matrix, and refers to usefull packages for computational text analysis. 
