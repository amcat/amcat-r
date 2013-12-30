source("amcatr.r")

conn = amcat.connect("amcat-dev.labs.vu.nl")

setid = 2546 # speeches on iraq

data = amcat.getobjects(conn, "articlemeta", filters=list(articleset=setid))
demo = amcat.runaction(conn, "Query", articlesets=setid, query="democra*")
colnames(demo)[2] = "demo"
data = merge(data, demo, all.x=T)
data$demo[is.na(data$demo)] = 0

dest = amcat.runaction(conn, "Query", articlesets=setid, query="destruction")
colnames(dest)[2] = "dest"
data = merge(data, dest, all.x=T)
data$dest[is.na(data$dest)] = 0

with(data, table(demo, dest))
# so democracy and destruction do not seem compatible:-)
