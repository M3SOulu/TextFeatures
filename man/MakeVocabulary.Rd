% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dtm.R
\name{MakeVocabulary}
\alias{MakeVocabulary}
\title{Make vocabulary}
\usage{
MakeVocabulary(
  tokens,
  ngram = c(1, 1),
  stopwords = character(0),
  term.min = 10
)
}
\arguments{
\item{tokens}{List of character vectors containing tokens for each
document.}

\item{ngram}{Minimum and maximum ngram sizes.}

\item{stopwords}{Character vector with stopwords to remove.}

\item{term.min}{Minimum term count for ngrams to consider.}
}
\value{
A text2vec vocabulary object.
}
\description{
Create a vocabulary object with text2vec.
}
