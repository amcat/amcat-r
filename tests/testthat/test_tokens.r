#context("AmCAT-R token tests")

#conn = conn_from_env()

#if(!is.null(conn)){
#  testthat::test_that("We can retrieve (elastic) tokens", {
#    texts = c("Dit is een text, joh!", "En dit is er nog een")
#    headlines = c("Deze kop kost $2.00, a.u.b.", "kop twee")
#    # add to new set
#    aset = upload_articles(project=1, articleset="Test set from unit tests", text=texts, headline=headlines, 
#                                 medium="test", date="2010-01-01T00:00")
#    t = get_tokens(project=1, articleset=aset)
#    testthat::expect_equal(length(unique(t$id)), 2)
#    testthat::expect_equal(paste(t$word[t$id==min(t$id) & t$position < 5], collapse = " "), "deze kop kost 2 00")
#  })
#}
