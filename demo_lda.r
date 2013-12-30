source('amcatr.r')
source('amcat_getdata.r')
source('lda_lib.r')

conn = amcat.connect('http://amcat-dev.labs.vu.nl') # AmCAT vraagt om je inloggegevens

articleset_id = 6438 # Columns Youp (n = 1323)
features = amcat.getFeatures(conn, articleset_id)

## Het is nog steeds mogelijk om een reference set te gebruiken om woorden te filteren. Volgens mij is dit echter niet cruciaal als er al gefilterd is op woord-document frequentie en POS tags.
## In lda.prepareFeatures kun je woorden filteren met docfreq.thres en docfreq_pct.max:
##    docfreq.thres: minimum aantal artikelen waarin een woord moet voorkomen (filtert 'too_rare' woorden weg)
##    docfreq_pct.max: Een percentage (1-100). het woord mag niet voorkomen in meer dan dit percentage van het totaal aantal artikelen (filtert 'too_common' woorden weg) )
## De beste waarde voor docfreq_pct.max is zeer afhankelijk van de data. Voor veel verschillende onderwerpen in kranten zijn veel voorkomende woorden niet 
## interessant, maar bij columns van dezelfde auteur zou je dan stijlelementen kunnen missen. 
data = lda.prepareFeatures(features, docfreq.thres=5, docfreq_pct.max=25) # Nog splitten in prepareVocabulary en prepareMatrix

m = lda.cluster(data$matrix, data$voca$word, nclusters=30, niterations=100)

write.csv(top.topic.words(m$topics), file='/tmp/out.csv')

## Zie eventueel amcat_featurestream.r voor aantekeningen over wat amcat.getFeatures nu doet en welke parameters nog toegevoegd kunnen worden.