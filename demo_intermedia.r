source('amcatr.r')
source('amcat_getdata.r')
source('intermedia_lib.r')

##### INLOGGEN OP AMCAT #####
conn = amcat.connect('http://amcat.vu.nl') # AmCAT vraagt om je inloggegevens

##### DATA OPHALEN #####
articlesets = c(3321,3322) # hier alle articlesets invoeren: c(articleset1, articleset2, ...)
media = c(999,77,4,6) 
from_date='2012-01-01' 
to_date='2013-01-01' 
queries = c('Frankrijk# frankrijk franse* fransman', 
            'Duitsland# duitsland duitse*',
            'Spanje# spanje spanjaard* spaanse')
#queries = getCodebook(conn, codebook_id=157)$queries # Gebruik de queries uit een codebook (uitsluitend voor hippe mensen)
                     
hits = amcat.getHits(conn, queries, articlesets)
meta = amcat.getMeta(conn, articlesets, media, from_date, to_date)

# DETERMINE PUBLICATION TIMES OF MEDIA
mediameta = data.frame(medium_id=unique(meta$medium), medium_string=amcat.getMediumString(unique(meta$medium)))
head(mediameta)
mediameta$pub_hour = c(20,18,6,6) # bepaal hier het uur waarin de artikelen van dit medium gepubliceerd worden. Als het medium geen vaste publicatietijden heeft, dan kan NA gebruikt worden. In dat geval wordt per artikel de publicatietijd zoals opgeslagen in amcat gebruikt. 

# PREPARE DATA
intermedia.mediumXweekday(meta) # Als bepaalde media niet op bepaalde dagen vd week publiceren, overweeg dan deze dagen buiten beschouwing te laten middels skip_weekdays.
skip_weekdays = c(7) # Hiermee kun je dagen buiten beschouwing laten (1=maandag, 2=dinsdag, etc). De dag daarvoor wordt dan gebruikt om de lag te berekenen. Bijvoorbeeld, vaak publiceren media niet op zondag, dus deze kun je weglaten met c(7). Het nieuws van maandag wordt dan voorspeld adhv het nieuws op zaterdag.

day_lag = 1 # Aantal dagen lag
split_hours = c(12) # Op de gegeven uren wordt de dag gesplit, om binnen de 24-uursdynamiek intermedia invloed te meten. Bij c(12) wordt bijvoorbeeld de dag gesplit in 0u-12u en 12u-24u
skip_missingdays = TRUE # Als om andere reden nog dagen ontbreken, bijvoorbeeld door een zomerstop van tv of simpelweg gaten in de data, overweeg dan deze dagen uit de analyse weg te laten (met = TRUE) voor alle media. 

d = intermedia.prepareData(hits, meta, day_lag=day_lag, split_hours=split_hours, skip_weekdays=skip_weekdays, 
                skip_missingdays=skip_missingdays, mediameta=mediameta)

models = intermedia.runAnalysis(d, lag=1, b_digits=2, se_digits=2, random_intercepts=F, binomial=T)

models$models # Toon gegevens per model, inclusief model.fit en verbetering van model door de intermedia variabelen (model.improvement)
intermedia.getRegressionTable(models$coefficients) # Toon resultaten met variabelen in rijen en modellen in kolommen