#' Senti4SD features
#'
#' Loads Senti4SD data and computes features on given documents.
#'
#' @param text Character vector with text documents.
#' @param label Labels or ids of the text documents.
#' @param datadir Directory where Senti4SD data is stored
#' @return A list containing three matrices containing the semantic
#'   features, the lexicon-based features and the keyword-based
#'   features.
#' @export
Senti4SDFeatures <- function(text, label=1:length(text), datadir) {
  cleanNLP::cnlp_init_corenlp("en", 0)
  tokens <- CleanNLPTokens(text, label)

  lexicons <- ReadLexicons(datadir, FALSE)
  negations <- fread(file.path(datadir, "NegatingWordList"), header=FALSE)$V1

  logging::loginfo("Loading DSM")
  dsm <- LoadDSM(file.path(datadir, "dsm.bin"))
  gc()
  vectors <- with(lexicons, PolarityVectors(positive$word, negative$word,
                                            objective$word, dsm))
  logging::loginfo("Computing Senti4SD semantic features")
  semantic <- SemanticBasedFeatures(tokens, vectors, dsm)
  rm(dsm)
  gc()

  logging::loginfo("Computing Senti4SD lexicon-based features")
  lexicon <- LexiconBasedFeatures(tokens, lexicons)
  logging::loginfo("Computing Senti4SD keyword-based features")
  keyword <- KeywordBasedFeatures(tokens, negations)
  features <- list(lexicon=lexicon, keyword=keyword, semantic=semantic)
  features <- lapply(features, function(f) as.matrix(f[, -1, with=FALSE],
                                                     rownames=label))
  for(name in names(features)) {
    colnames(features[[name]]) <- sprintf("<%s:%s>", name,
                                          colnames(features[[name]]))
  }
  features
}

#' Generate ngrams
#'
#' Generates different document term matrix for different values of n.
#'
#' @param tokens List of character vectors containing tokens for each
#'   document.
#' @param ids Ids of the different documents.
#' @param n Generate DTMs with values for ngrams going from (1, 1) to
#'   (1, n).
#' @param all.ngrams If TRUE, also generates nrgams going from (1, 1)
#'   to (n, n).
#' @param keep.vocab If TURE, returns a list containing both the DTM
#'   and the vocabulary used to build it.
#' @param ... Additional arguments to pass to MakeVocabulary.
#' @return The DTM containing the ngrams or a list containing the DTM
#'   and the vocabulary used to build it.
#' @seealso MakeVocabulary
#' @export
GenerateNgrams <- function(tokens, ids, n=3, all.ngrams=FALSE,
                           keep.vocab=FALSE, ...) {
  dtms <- lapply(1:n, function(i) c(1, i))
  if (all.ngrams) {
    dtms <- c(dtms, lapply(2:n, function(i) c(i, i)))
    names(dtms) <- c(sprintf("dtm.1.%d", 1:n), sprintf("dtm.%d", 2:n))
  } else {
    names(dtms) <- sprintf("dtm.1.%d", 1:n)
  }
  lapply(dtms, function(ngram) {
    vocab <- MakeVocabulary(tokens, ngram=ngram, ...)
    dtm <- MakeDTM(tokens, ids, vocab)
    if (keep.vocab) list(vocab=vocab, dtm=dtm) else dtm
  })
}