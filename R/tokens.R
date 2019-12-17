#' CleanNLP Tokens
#'
#' Computes tokens from text using cleanNLP and Stanford CoreNLP.
#'
#' @param text Character vector where each element is a different
#'   document.
#' @param ids Optional ids for each text document.
#' @return A data.table object containing cleanNLP tokens as a
#'   data.table object with token words to lower case and original
#'   token stored in word.orig column.
#' @export
CleanNLPTokens <- function(text, ids=1:length(text)) {
  names(text) <- ids
  ann <- cnlp_annotate(text, backend="corenlp")
  tokens <- as.data.table(ann$token)
  tokens[, word := tolower(token)]
  tokens
}

#' Last token
#'
#' Returns the last token for each document.
#'
#' @param tokens Token data.table containing ids for document (id),
#'   sentence (sid) and token (tid).
#' @return A subset of the data.table with one line per document
#'   containing the last token of the document.
LastToken <- function(tokens) {
  tokens[, .SD[sid == max(sid)][which.max(tid)], by=id]
}

#' Token Score
#'
#' Computes lexicon score for a set of tokens
#'
#' @param tokens Token data.table containing document id and word.
#' @param lexicon Lexicon as a data.table with word and score columns.
#' @param ids List of all document ids.
#' @return data.table resulting of merging tokens and lexicon by
#'   adding 0 to tokens not found in lexicon. Column id is set as key.
TokenScore <- function(tokens, lexicon, ids=unique(tokens$id)) {
  setkey(tokens, word)
  setkey(lexicon, word)
  tokens <- lexicon[tokens]
  tokens[is.na(score), score := 0]
  setkey(tokens, id)#[ids]
}
