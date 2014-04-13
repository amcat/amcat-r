\name{amcat.features}
\alias{amcat.features}
\title{Get NLP features from AmCAT}
\usage{
amcat.features(conn, articleset_id, unit_level = "article",
  posfilter = c("noun", "verb", "NN"), use_stemming = T, batchsize = 500,
  min_freq = 0, min_freq_perbatch = 0, sample_pct = 100)
}
\arguments{
  \item{conn}{the connection object from
  \code{\link{amcat.connect}}}

  \item{articleset_id}{the id of the articleset to get
  features from}

  \item{unit_level}{the level of aggregation, e.g.
  'article' or 'sentence'}

  \item{posfilter}{which POS to include}

  \item{use_stemming}{stem words}

  \item{batchsize}{the number of features (articles?) to
  include per call}

  \item{min_freq}{the minimal total frequency of words}

  \item{min_freq_perbatch}{the minimal frequency of a word
  per batch}

  \item{sample_pct}{take a sample of the documents in the
  set}
}
\value{
A data frame of counts per feature per unit
}
\description{
Get NLP features (pos, lemma etc) from AmCAT for a
selection of sets
}

