#' Keyword based features
#'
#' Computes keyword based features.
#'
#' @param tokens Token data.table containing ids for document (id),
#'   sentence (sid) and token (tid), token word and original word.
#' @param negations Negation word list.
#' @return A data.table with features computed for each document.
#' @seealso CleanNLPTokens
#' @export
KeywordBasedFeatures <- function(tokens, negations) {
  ids <- sort(unique(tokens$id))

  res <- tokens[, list(uppercase.words=sum(IsUppercaseWord(word.orig)),
                       uppercase.words.unique=sum(IsUppercaseWord(word.orig) &
                                                  !duplicated(word.orig)),
                       repeated.marks=sum(IsRepeatedMarks(word)),
                       laughter=sum(IsLaughter(word)),
                       elongated=sum(IsElongated(word)),
                       negation.words=sum(word %in% negations),
                       user.mentions=sum(IsUserMention(word))), by=id]
  res <- setkey(res, id)[ids]

  last.token <- LastToken(tokens)[, list(id, word)]
  setkey(last.token, id)
  last.token <- last.token[ids]
  res <- cbind(res, end.exclamation=IsExclamation(last.token$word))
  setkey(res, id)
}

## TODO
## uppercase word ratios
## uppercase char ratios
## other ratios?

## EndExclamation <- function(tokens, col.name=NULL) {
##   res <- LastToken(tokens)[, list(id, N=IsExclamation(word))]
##   RenameColumn(res, col.name)
## }

## CountUppercaseWords <- function(tokens, col.name=NULL) {
##   res <- tokens[, list(N=sum(IsUppercaseWord(word.orig))), by=id]
##   RenameColumn(res, col.name)
## }

## CountUserMentions <- function(tokens, col.name=NULL) {
##   res <- tokens[, list(N=sum(IsUserMention(word))), by=id]
##   RenameColumn(res, col.name)
## }

## CountRepeatedMarks <- function(tokens, col.name=NULL) {
##   res <- tokens[, list(N=sum(IsRepeatedMarks(word))), by=id]
##   RenameColumn(res, col.name)
## }
