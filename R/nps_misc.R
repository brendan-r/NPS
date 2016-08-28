#' Strips Likert scale point labels, returns numeric or ordinal data
#'
#' Survey systems export responses to Likhert scales with the scale labels on,
#' meaning that R as factors or text as opposed to numeric data. This function
#' takes labelled scales and returns unlabelled numeric data (by default), or an
#' unlabelled ordered factor (if requested).
#'
#' @name scalestrip
#' @aliases scalestrip
#' @param x a \code{\link{vector}}, \code{\link{matrix}}, or
#'   \code{\link{data.frame}}, containing Likert data labelled in the format
#'   "Integer - some text", e.g. "10 - Extremely Likely"
#' @param ordinal \code{\link{logical}} (\code{TRUE}\\code{FALSE}). Should the
#'   data returned be an ordered factor? Otherwise the data returned is
#'   \code{\link{numeric}}. Defaults to \code{FALSE}.
#' @return Unlabelled numeric data (by defualt), or an unlabelled ordered factor
#'   (if requested).
#' @export
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
scalestrip <- function(x, ordinal = FALSE) {
  out <- function(x) switch(ordinal + 1, as.numeric(x), ordered(x))

  if (!(is.data.frame(x) | is.matrix(x))) {
    return(out(as.numeric(gsub("[[:alpha:]]|[[:punct:]]", "", x))))
  }

  else if (is.data.frame(x) | is.matrix(x)) {
    for (i in 1:ncol(x)) {
      x[, i] <- out(gsub("[[:alpha:]]|[[:punct:]]", "", x[, i]))
    }
    return(x)
  }
}
