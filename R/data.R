#' Senti4SD Gold Dataset
#'
#' Senti4SD Gold Dataset from StackOverflow.
#'
#' @format A \code{data.table} object with 4423 rows and 9 columns:
#' \describe{
#'   \item{study}{Study that led to the row.}
#'   \item{label}{Unique label identifying the row.}
#'   \item{so.id}{StackOverflow id.}
#'   \item{post.type}{Type of StackOverflow post.}
#'   \item{text}{Raw text string.}
#'   \item{final}{Final polarity.}
#'   \item{r1}{Polarity according to rater 1.}
#'   \item{r2}{Polarity according to rater 2.}
#'   \item{r3}{Polarity according to rater 3.}
#' }
"senti4sd.gold"
