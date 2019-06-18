#' Is exclamation mark.
#'
#' Determine if words are exclamation marks.
#'
#' @param words Character vector of words.
#' @return TRUE if word only contains exclamation marks.
IsExclamation <- function(words) {
  grepl("^!+$", words)
}

#' Is uppercase word
#'
#' Determines if words are uppercase words.
#'
#' @param words Character vector of words.
#' @return TRUE if the word is all uppercase.
IsUppercaseWord <- function(words) {
  ## grepl("^[[:alnum:]]+$", words) &
  grepl("[A-Z]", words) & !grepl("[a-z]", words)
}

#' Is user mention
#'
#' Determines if words are uppercase words.
#'
#' @param words Character vector of words.
#' @return TRUE if the word is a user mention.
IsUserMention <- function(words) {
  grepl("^@\\w+$", words)
}

#' Is repeated marks
#'
#' Determines if words are uppercase words.
#'
#' @param words Character vector of words.
#' @return TRUE if the word is repeated (interrogation or exclamation) marks.
IsRepeatedMarks <- function(words) {
  grepl("^[?!][?!]+", words)
}

#' Is elongated word
#'
#' Determines if words are elongated.
#'
#' @param words Character vector of words.
#' @return TRUE if the word is elongated.
IsElongated <- function(words) {
  grepl("([[:alpha:]])\\1{2,}", words)
}

#' Is laughter
#'
#' Determines if words are laughter.
#'
#' @param words Character vector of words.
#' @return TRUE if the word is laughter.
IsLaughter <- function(words) {
  grepl("((ah|ha){2}(a|h)*)|((eh|he){2}(e|h)*)|((ih|hi){2}(i|h)*)", words)
}
