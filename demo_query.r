conn = amcat.connect("http://localhost:8000", "amcat", "amcat")
conn = amcat.connect("http://amcat.vu.nl", "wva")

x = codebook.gethierarchy(conn=conn, codebook_id=296, languages=c("nl", "query"))

x = codebook.addcats(x, maxdepth=1)
queries = codebook.getqueries(x$label.query, x$code)

c = getcounts(queries$label, queries$query, sets=6263)

head(c)
head(x)

x2 = merge(x, c, by.x="code", by.y="query", all.x=T)

write.csv2(x2, file="/tmp/out.csv")