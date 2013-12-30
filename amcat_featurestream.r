source('amcatr.r')
source('amcat_getdata.r')

## parameters voor featurestream moeten nog in de action beschikbaar worden. Momenteel is de instelling:
## Stemmed words (lowercase) with POS tags
## only used POS tags: noun, NN, verb

conn = amcat.connect('http://amcat-dev.labs.vu.nl') # AmCAT vraagt om je inloggegevens

## Spuug features uit voor een gegeven articleset
data = amcat.getFeatures(conn, articleset_id=4073, unit_level='article') # per artikel
data = amcat.getFeatures(conn, articleset_id=4073, unit_level='paragraph') # per paragraaf (per 'sentence' ook mogelijk)

## Filter op woordfrequentie
data_minfreq = amcat.getFeatures(conn, articleset_id=4073, unit_level='article', batchsize=100, min_freq=5) # gebruik alleen woorden die minstens [min_freq] voorkomen in corpus
data_minfreqperbatch = amcat.getFeatures(conn, articleset_id=4073, unit_level='article', batchsize=100, min_freq_perbatch=5) # features worden opgehaald per [batchsize] artikelen. min_freq_perbatch filtert op woordfrequentie per batch ipv over de hele corpus
nrow(data_minfreq) > nrow(data_minfreqperbatch) # De resultaten zijn (uiteraard) anders. min_freq is logischer om te gebruiken; min_freq_perbatch is vooral nuttig bij grote articlesets.

## Nog te doen --> Meer opties voor soorten features
## optie voor taal!!!
## optie om woorden/lemma uit database te gebruiken
## opties voor woordprocessing: stemming ja/nee, postags ja/nee, lowercase ja/nee, punctuatie ja/nee
## opties voor filtering: stopwoorden ja/nee, headline ja/nee/alleen, POSfilter (lijst)
## optie voor ngrams

