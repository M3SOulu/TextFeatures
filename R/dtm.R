#' Make vocabulary
#'
#' Create a vocabulary object with text2vec.
#'
#' @param tokens List of character vectors containing tokens for each
#'   document.
#' @param ngram Minimum and maximum ngram sizes.
#' @param stopwords Character vector with stopwords to remove.
#' @param term.min Minimum term count for ngrams to consider.
#' @return A text2vec vocabulary object.
#' @export
MakeVocabulary <- function(tokens, ngram=c(1, 1), stopwords=character(0),
                           term.min=10) {
  if (is.data.frame(tokens)) {
    tokens <- split(tokens$word, tokens$id)
  }
  v <- create_vocabulary(itoken(tokens), ngram=ngram, stopwords=stopwords)
  prune_vocabulary(v, term_count_min=term.min)

}

#' Make DTM
#'
#' Creates a document term matrix with text2vec.
#'
#' @param tokens List of character vectors containing tokens for each
#'   document.
#' @param ids Ids of the different documents.
#' @param vocabulary Vocabulary object.
#' @return A dgTMatrix containing the DTM.
#' @seealso MakeVocabulary
#' @export
MakeDTM <- function(tokens, ids, vocabulary) {
  if (is.data.frame(tokens)) {
    tokens <- split(tokens$word, tokens$id)[ids]
  }
  create_dtm(itoken(tokens, ids=ids),
             vocab_vectorizer(vocabulary),
             type="dgTMatrix")
}
