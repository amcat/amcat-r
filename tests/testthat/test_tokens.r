#' Unit tests for amcat-r
#' 
#' This requires an AmCAT server to be running on localhost:8000 with a password in ~/.amcatauth
#' Note: this *will* modify the contents of this server by creating sets and uploading articles,
#'       so don't run this on a "production" server!

context("AmCAT-R token tests")

test_connect <- function() tryCatch(amcat_connect("http://localhost:8000"), 
                               error=function(err) NULL)
c = test_connect()

if(!is.null(c)){
  testthat::test_that("We can retrieve (elastic) tokens", {
    texts = c("Dit is een text, joh!", "En dit is er nog een")
    headlines = c("Deze kop kost $2.00, a.u.b.", "kop twee")
    # add to new set
    aset = upload_articles(conn, project=1, articleset="Test set from unit tests", text=texts, headline=headlines, 
                                 medium="test", date="2010-01-01T00:00")
    flush_elasticsearch(conn)
    t = get_tokens(conn, project=1, articleset=aset)
    testthat::expect_equal(length(unique(t$aid)), 2)
    testthat::expect_equal(paste(t$term[t$aid==min(t$aid) & t$position < 5], collapse = " "), "deze kop kost 2 00")
  })
}
