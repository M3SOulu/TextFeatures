#' Senti4SD Semantic based features
#'
#' Computes semantic based features.
#'
#' @param tokens Token data.table containing ids for document (id),
#'   token word.
#' @param vectors Polarity vectors for positive, negative, objective
#'   and subjective words.
#' @param dsm word2vec object representing the Distributional Semantic
#'   Model.
#' @return A data.table with features computed for each document.
#' @seealso LoadDSM
#' @seealso PolarityVectors
#' @export
SemanticBasedFeatures <- function(tokens, vectors, dsm) {
  if(requireNamespace("data.table", quietly=TRUE)) {
    ids <- sort(unique(tokens$id))
    res <- dsm[[tokens$word, average=FALSE]]
    res <- cbind(as.data.table(matrix(res, nrow=nrow(res))), word=rownames(res))
    res <- merge(tokens[, list(id, word)], res, by="word")
    res$word <- NULL
    res <- as.matrix(res[, as.list(colSums(.SD)), by=id], rownames="id")
    colnames(res) <- paste("sim", colnames(res), sep=".")
    res <- cbind(id=rownames(res),
                 as.data.table(wordVectors::cosineSimilarity(res, vectors)))
    res <- setkey(res, id)[ids]
    res[is.na(res)] <- 0
    setkey(res, id)
  } else {
    stop("wordVectors must be installed to use semantic-based features")
  }
}

#' Load DSM
#'
#' Load DSM stored in word2vec binary format.
#'
#' @param filename Filename to the DSM binary file.
#' @return VectorSpaceModel object.
#' @export
LoadDSM <- function(filename) {
  if(requireNamespace("data.table", quietly=TRUE)) {
    wordVectors::read.binary.vectors(filename)
  } else {
    stop("wordVectors must be installed to use semantic-based features")
  }
}

#' Polarity Vectors
#'
#' Returns the polarity vectors for positive, negative, objective and
#' subjective (positive and negative) lexicons.
#'
#' @param positive Positive words lexicon.
#' @param negative Negative words lexicon.
#' @param objective Objective words lexicon.
#' @param dsm DSM.
#' @return Vector matrix for positive, negative, objective and
#'   subjective lexicons.
#' @seealso LoadDSM
#' @export
PolarityVectors <- function(positive, negative, objective, dsm) {
  words <- list(positive=positive, negative=negative,
                objective=objective, subjective=c(positive, negative))
  t(sapply(words, function(x) colSums(dsm[[x, average=FALSE]])))
}
