connect <- function() tryCatch(amcat.connect("http://localhost:8000"), 
                               error=function(err) {warning(err); skip("No AmCAT server available at localhost:8000")})

testthat::test_that("Article metadata", {
  c = connect()
  # add to new set
  aset = amcat.upload.articles(c, project=1, articleset="Test set from unit tests", text="text", headline="headline", 
                               medium="test", date="2010-02-03T12:34")
  amcat.flush(c)
  meta = amcat.getarticlemeta(c, 1, aset, columns=c("medium", "date", "headline", "text"))
  testthat::expect_equal(nrow(meta), 1)
  testthat::expect_equal(class(meta$date), "Date")
  testthat::expect_equal(meta$date, as.Date("2010-02-03"))
  testthat::expect_equal(class(meta$headline), "character")
  testthat::expect_equal(as.character(meta$headline), "headline")
  testthat::expect_equal(class(meta$text), "character")
  testthat::expect_equal(as.character(meta$text), "text")
  testthat::expect_equal(class(meta$medium), "factor")
  testthat::expect_equal(as.character(meta$medium), "test")
  
  meta = amcat.getarticlemeta(c, 1, aset, columns=c("date"), time=T, dateparts=T)
  testthat::expect_equal(class(meta$date)[1], "POSIXct")
  testthat::expect_equal(meta$date, strptime("2010-02-03 12:34:00", "%Y-%m-%d %H:%M:%S"))
  testthat::expect_equal(meta$year, as.Date("2010-01-01"))  
})