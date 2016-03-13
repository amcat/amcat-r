#' Unit tests for amcat-r
#' 
#' This requires an AmCAT server to be running on localhost:8000 with a password in ~/.amcatauth
#' Note: this *will* modify the contents of this server by creating sets and uploading articles,
#'       so don't run this on a "production" server!

context("AmCAT-R token tests")

connect <- function() tryCatch(amcat.connect("http://localhost:8000"), 
                               error=function(err) {warning(err); skip("No AmCAT server available at localhost:8000")})

test_that("We can retrieve (elastic) tokens", {
  conn = connect()
  texts = c("Dit is een text, joh!", "En dit is er nog een")
  headlines = c("Deze kop kost $2.00, a.u.b.", "kop twee")
  # add to new set
  aset = amcat.upload.articles(conn, project=1, articleset="Test set from unit tests", text=texts, headline=headlines, 
                               medium="test", date="2010-01-01T00:00")
  t = amcat.gettokens(conn, project=1, articleset=aset)
  expect_equal(length(unique(t$aid)), 2)
  expect_equal(paste(t$term[t$aid==min(t$aid) & t$position < 5], collapse = " "), "deze kop kost 2 00")
})

