#' Lexicon based Features
#'
#' Computes lexicon based features for a given list of lexicons.
#'
#' @param tokens Token data.table containing ids for document (id),
#'   sentence (sid) and token (tid) and token word.
#' @param lexicons List of lexicons as data.table object with word and
#'   score columns.
#' @seealso ReadLexicons
#' @export
LexiconBasedFeatures <- function(tokens, lexicons) {
  lexicon.features <- lapply(names(lexicons), function(l) {
    LexiconFeatures(tokens, lexicons[[l]], l)
  })
  res <- Reduce(function(x, y) merge(x, y, by="id"), lexicon.features)
  setkey(res, id)
}

#' Lexicon features
#'
#' Computes features for a given lexicon.
#'
#' @param tokens Token data.table containing ids for document (id),
#'   sentence (sid) and token (tid) and token word.
#' @param lexicon Lexicon as a data.table with word and score columns.
#' @param prefix Optional prefix to add in front of column names.
#' @return A data.table with features computed for each document.
#' @seealso CleanNLPTokens
#' @export
LexiconFeatures <- function(tokens, lexicon, prefix=NULL) {
  ids <- sort(unique(tokens$id))
  setkey(lexicon, word)

  last.token <- TokenScore(LastToken(tokens)[, list(id, word)], lexicon, ids)
  tokens <- merge(tokens, lexicon, by="word")
  last.token.lexicon <- TokenScore(LastToken(tokens)[, list(id, word)],
                                   lexicon, ids)

  res <- tokens[, list("words.unique"=length(unique(word)),
                       "words"=.N,
                       "sum.score"=sum(score),
                       "max.score"=max(score),
                       "min.score"=min(score)), by=id]
  res <- setkey(res, id)[ids]
  res <- last.token.lexicon[, list(id, last.lexicon.token.score=score)][res]
  res[is.na(res)] <- 0

  res <- last.token[, list(id, last.token.score=score,
                           end.emph=IsExclamation(word))][res]
  res[, emph := max.score > 0 & end.emph]
  res[, end := last.token.score > 0]
  res[, end.emph := last.token.score > 0 & end.emph]
  if (!is.null(prefix)) {
    setnames(res, names(res)[names(res) != "id"],
             paste(prefix, names(res)[names(res) != "id"], sep="."))
  }
  setkey(res, id)
}

#' Read Lexicons
#'
#' Reads lexicons from Senti4SD directory.
#'
#' @param datadir Path where lexicons are stored.
#' @param emoticons If TRUE, includes lexicons based on emoticons.
#' @export
ReadLexicons <- function(datadir, emoticons=TRUE) {
  positive <- fread(file.path(datadir, "SStrength_Lexicons", "PositiveWordWithScore"),
                    col.names=c("word", "score"))
  negative <- fread(file.path(datadir, "SStrength_Lexicons", "NegativeWordWithScore"),
                    col.names=c("word", "score"))
  objective <- fread(file.path(datadir, "SStrength_Lexicons", "ObjectiveWordWithScore"),
                     col.names=c("word", "score"))
  subjective <- rbind(positive, negative)
  if (emoticons) {
    emoticons <- rbind(cbind(fread(file.path(datadir, "PositiveEmoticon"),
                                   col.names="word"), score=1),
                       cbind(fread(file.path(datadir, "NegativeEmoticon"),
                                   sep="\t", col.names="word"), score=-1))
    emoticons[, type := ifelse(score > 0, "positive", "negative")]
    emoticons[, score := abs(score)]
    positive.emoticons <- emoticons[type == "positive", list(word, score)]
    negative.emoticons <- emoticons[type == "negative", list(word, score)]
    positive.all <- rbind(positive, positive.emoticons)
    negative.all <- rbind(negative, negative.emoticons)
  }
  res <- as.list(environment())
  res[sapply(res, is.data.frame)]
}

## CountTokens <- function(tokens, scores, col.name=NULL, unique.tokens=FALSE) {
##   tokens <- merge(tokens, scores, by="word")
##   if (unique.tokens) {
##     res <- tokens[, list(N=length(unique(word))), by=id]
##   } else {
##     res <- tokens[, .N, by=id]
##   }
##   RenameColumn(res, col.name)
## }

## ScoreLastToken <- function(tokens, scores, col.name=NULL) {
##   tokens <- merge(tokens, scores, by="word")
##   RenameColumn(LastToken(tokens)[, list(id, N=score)], col.name)
## }

## SumScores <- function(tokens, scores, col.name=NULL) {
##   tokens <- merge(tokens, scores, by="word")
##   res <- tokens[, list(N=sum(score)), by=id]
##   RenameColumn(res, col.name)
## }

## MaxScore <- function(tokens, scores, col.name=NULL) {
##   tokens <- merge(tokens, scores, by="word")
##   res <- tokens[, list(N=max(score)), by=id]
##   RenameColumn(res, col.name)
## }

## HasTokenAndExclamation <- function(tokens, subset, col.name=NULL) {
##   res <- setkey(CountTokens(tokens, subset), id)
##   res <- res[setkey(LastToken(tokens), id)]
##   res[is.na(res)] <- 0
##   res <- res[, list(id, N=N > 0 & word == "!")]
##   RenameColumn(res, col.name)
## }

## LastTokenIn <- function(tokens, subset, col.name=NULL) {
##   res <- LastToken(tokens)[, list(id, N=word %in% subset$word)]
##   RenameColumn(res, col.name)
## }

## TokenBeforeExclationIn <- function(tokens, subset, col.name=NULL) {
##   res <- setkey(LastToken(tokens)[, list(id, N2=word == "!")], id)
##   res <- setkey(LastTokenIn(tokens[word != "!"], subset), id)[res]
##   res <- res[, list(id, N=N & N2)]
##   RenameColumn(res, col.name)
## }
