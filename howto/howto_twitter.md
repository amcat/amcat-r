Uploading tweets to AmCAT
=========================

By combining the twitter API and the AmCAT API we can upload tweets that match a given query directly to AmCAT.

Installing required packages
----

This howto uses the [twitteR](http://cran.r-project.org/web/packages/twitteR/index.html) and [amcatr](http://github.com/amcat/amcat-r) packages. 
The following code will load these packages, installing them if needed.
You can skip the 'install_github' steps after the first time. 


```r
if (!require("devtools")) {install.packages("devtools"); library(devtools)}
if (!require("plyr")) {install.packages("plyr"); library(plyr)}

library(devtools)
install_github("twitteR", username="geoffjentry")
install_github("amcat-r", "amcat")

library("twitteR")
library("amcatr")
```

Connecting to Twitter
----

First, you need to get a number of tokens (a kind of passwords) from twitter:

1. Sign in to twitter at http://twitter.com
2. Go to https://apps.twitter.com/ and 'create a new app'
3. After filling in the required information, go to 'keys and access tokens'
4. Select 'create access token', and refresh
5. Create variables with the consumer key, consumer secret, access token, and access token secret:


```r
token = '...'
token_secret = '...'
consumer_key = "..."
consumer_secret = "..."
```



Now you can connect using the setup_twitter_oauth function:


```r
setup_twitter_oauth(consumer_key, consumer_secret, token, token_secret)
```

```
## [1] "Using direct authentication"
```

Searching twitter
----

Please see the documentation for the Twitter API and the twitteR package for all the possibilities of the API. 
As the following simple example shows, you can search for keywords and get a list or results


```r
tweets = searchTwitteR("pvda", geocode="52.36656,4.90269,200km", result_type="recent")
tweets[[1]]
```

```
## [1] "Zarroy: RT @Paul_van_Meenen: Ja, leerlingen toelaten; het is wat! RT @michelrog: Scholen 'moeten' weer wat van de PvdA. Het CDA ... http://t.co/euk…"
```

```r
class(tweets[[1]])
```

```
## [1] "status"
## attr(,"package")
## [1] "twitteR"
```

```r
tweets[[1]]$text
```

```
## [1] "RT @Paul_van_Meenen: Ja, leerlingen toelaten; het is wat! RT @michelrog: Scholen 'moeten' weer wat van de PvdA. Het CDA ... http://t.co/euk…"
```

To make it easier to manipulate the tweets, we can convert them from a list of `status` objects to a data.frame, for which we use the `ldply` (list-dataframe-ply) function from the plyr package, taking advantage of the fact that `as.data.frame` works on a single status object:


```r
tweets = ldply(tweets, as.data.frame)
names(tweets)
```

```
##  [1] "text"          "favorited"     "favoriteCount" "replyToSN"    
##  [5] "created"       "truncated"     "replyToSID"    "id"           
##  [9] "replyToUID"    "statusSource"  "screenName"    "retweetCount" 
## [13] "isRetweet"     "retweeted"     "longitude"     "latitude"
```

Uploading tweets to AmCAT
----

Now we can directly upload these tweets to AmCAT using the `amcat.upload.articles` function. 


```r
conn = amcat.connect("http://amcat.vu.nl")
project=1 # change this to your project ID
set = amcat.add.articles.to.set(conn, project=project, articleset.name="Tweets", articles=NULL)
```

```
## Created articleset 15598: Tweets in project 1
```

```r
amcat.upload.articles(conn, project=project, articleset=set, medium="twitter",
                      text=tweets$text, headline=tweets$text, 
                      date=tweets$created, 
                      author=tweets$screenName, addressee=tweets$replyToSN)
```

```
## Uploading 25 articles to set 15598
```

Now we can inspect our articleset on the AmCAT web site, or do a query from r, e.g.:


```r
amcat.hits(conn, sets=set, queries="vvd")
```

```
## GET http://amcat.vu.nl/api/v4/search?q=count%23vvd&col=hits&sets=15598&minimal=TRUE&page_size=1000&format=csv&page=1
```

```
## Warning: Query vvd produced no results
```

```
## NULL
```
