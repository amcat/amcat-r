#' Unit tests for amcat-r
#' 
#' This requires an AmCAT server to be running on localhost:8000 with a password in ~/.amcatauth
#' Note: this *will* modify the contents of this server by creating sets and uploading articles,
#'       so don't run this on a "production" server!

context("AmCAT-R article tests")

test_connect <- function() tryCatch(amcat_connect("http://localhost:8000"), 
                               error=function(err) NULL)
c = test_connect()

if(!is.null(c)){
  test_that("Article upload/download", {
    texts = c("test 1", "test 2")
    headlines = c("headline 1", "headline 2")
    # add to new set
    aset = upload_articles(c, project=1, articleset="Test set from unit tests", text=texts[1], headline=headlines[1], 
                                 medium="test", date="2010-01-01T12:34")
    flush_elasticsearch(c)
    meta = get_articles(c, project=1, articleset=aset, columns=c("medium", "date", "headline"))
    testthat::expect_equal(nrow(meta), 1)
  
    #add to existing set
    aset = upload_articles(c, project=1, articleset=aset, text=texts[-1], headline=headlines[-1], 
                                 medium="test", date="2010-01-01T00:00")
    flush_elasticsearch(c)
    meta = get_articles(c, project=1, articleset=aset, columns=c("medium", "date", "headline"))
    testthat::expect_equal(nrow(meta), 2)
    testthat::expect_equal(as.character(unique(meta$medium)), "test")
    testthat::expect_equal(sort(as.character(meta$headline)), sort(headlines))
    
    #add existing articles to new set
    aset2 = add_articles_to_set(c, project=1, articles=meta$id[1], articleset.name = "Another new test")
    flush_elasticsearch(c)
    meta2 = get_articles(c, project=1, articleset=aset2)
    #   testthat::expect_equal(sort(meta2$id), sort(meta$id[1]))
    
    #add existing articles to existing set
    add_articles_to_set(c, project=1, articles=meta$id, articleset=aset2)
    flush_elasticsearch(c)
    meta2 = get_articles(c, project=1, articleset=aset2)
    expect_equal(sort(meta2$id), sort(meta$id))
  })
}

