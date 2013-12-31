conn = amcat.connect("http://localhost:8000", "amcat", "amcat")
conn = amcat.connect("http://amcat.vu.nl", "wva")

x = codebook.gethierarchy(conn=conn, codebook_id=296, languages=c("nl", "query"))

x = codebook.addcats(x, maxdepth=1)
q = codebook.getqueries(x$label.query, x$code)

queries=q$query
labels=q$label

h = query.hits(queries, labels, sets=6263) 

c = query.aggregate(queries, labels=labels, sets=6263, axis1="year")
c = merge(x, c, by.x="code", by.y="query", all.x=T)

write.csv2(c, file="/tmp/out.csv")