conn = amcat.connect("http://localhost:8000", "amcat", "amcat")
conn = amcat.connect("http://amcat.vu.nl", Sys.getenv('USER'))

x = amcat.gethierarchy(conn=conn, codebook_id=296, languages=c("nl", "query"))

x = get_codebook.cats(x, maxdepth=1)
q = amcat.getqueries(x$label.query, x$code)

queries=q$query
labels=q$label

h = get_hits(conn, queries, labels, sets=6263) 

c = get_aggregate(conn, queries, labels=labels, sets=6263, axis1="year")
c = merge(x, c, by.x="code", by.y="query", all.x=T)
