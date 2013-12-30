source('amcatr.r')
source('amcat_getdata.r')
source('codebook_lib.r')
source('associations_lib.r')

##### INLOGGEN OP AMCAT #####
conn = amcat.connect('http://amcat-dev.labs.vu.nl') # AmCAT vraagt om je inloggegevens

#### SELECT DATA ####
articlesets = c(3321,3322) # hier alle articlesets invoeren: c(articleset1, articleset2, ...)
meta = amcat.getMeta(conn, articlesets, media=c(999,77,4,6), from_date='2012-01-01', to_date='2013-01-01')

#### QUICK ANALYSIS ####
queries = c('Frankrijk# frankrijk franse* fransman', 
            'Duitsland# duitsland duitse*',
            'Spanje# spanje spanjaard* spaanse')
hits = amcat.getHits(conn, queries, articlesets)
hits = hits[hits$id %in% meta$id,]
associations.conProb(hits, meta, byMedium=F, byDateinterval=NA, calc_type='cp')
associations.conProb(hits, meta, byMedium=T, byDateinterval=NA, calc_type='cp')
associations.conProb(hits, meta, byMedium=F, byDateinterval='month', calc_type='cp')

##### CODEBOOK BASED ANALYSIS #####
codebook = amcat.getCodebook(conn, codebook_id=206)
codebook

hits = amcat.getHits(conn, codebook$queries, articlesets)
hits = hits[hits$id %in% meta$id,]

##### Use codebook hierarchy to aggregate data #####
codebook.getChildren(codebook$hierarchy, 'Political parties') # show children of code
codebook.aggCode(hits, codebook$hierarchy, 'Political parties') # aggregate hits: sum of code and children of code 
codebook.aggAllCodes(hits, codebook$hierarchy, codes=c()) # aggregate hits based on ontology. If codes parameter is an empty vector, all objects are aggregated.
hits = codebook.appendAggHits(hits, codebook$hierarchy)

##### Calculate Conditional Probability #####
codes = c('Political parties','')
only_from = c('CDA','VVD','PvdA','PVV'); only_to = c('Economy') # can be used to limit combination to be calculated. If empty, all combinations of codes are calculated.

# unaggregated, cp = zero since economy has no query (and thereby no hits) of its own
associations.conProb(hits, meta, codes, variable='hits', only_from=only_from, only_to=only_to, byMedium=F, byDateinterval=NA, calc_type='cp')
# aggregated, economy contains hits of its codebook children
associations.conProb(hits, meta, codes, variable='agg_hits', only_from=only_from, only_to=only_to, byMedium=F, byDateinterval=NA, calc_type='cp')
