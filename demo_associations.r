source('amcatr.r')
source('query_lib.r')
source('associations_lib.r')

##### Log
conn = amcat.connect('http://amcat.vu.nl') # AmCAT vraagt om je inloggegevens
articlesets = c(4626) # hier alle articlesets invoeren: c(articleset1, articleset2, ...)
queries = c('pvda', 'cda', 'vvd') 
h = query.hits(conn, queries, sets=articlesets)

# All associations
associations.from.hits(h$count, h$id, h$query)
# All associations, per medium
associations.from.hits(h$count, h$id, h$query, index=h$medium)
# Limit associations to from pvda
associations.from.hits(h$count, h$id, h$query, objects.from='pvda')


