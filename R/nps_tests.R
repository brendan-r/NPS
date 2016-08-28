#' Significance tests and confidence intervals for Net Promoter Scores
#'
#' This function performs one and two sample tests for the Net Promoter score(s)
#' of \emph{Recommend} data distributions. Currently, only a Wald type test is
#' supported.
#'
#' @param x A vector of \emph{Recommend} scores
#' @param y A vector of \emph{Recommend} scores, to compare to \code{x}. If not
#'   specified, a one sample test on \code{x} is performed. Defaults to
#'   \code{NULL}
#' @param test The type of test to perform. Currently only the Wald/Z-test
#'   ('\code{wald}') is supported
#' @param conf the confidence level of the test and intervals. Defaults to 0.95
#' @param \dots Not used.
#' @inheritParams nps
#' @return A \code{\link{list}} of class \code{nps.test} containing:
#'   \item{nps.x, nps.y}{The Net Promoter score(s)} \item{delta}{Where both
#'   \code{x} and \code{y} have been specified, the absolute difference between
#'   the two scores} \item{int}{The confidence interval generated. For a one
#'   sample test, this will be a confidence interval around \code{nps.x}. For a
#'   two sample test, this will be a confidence interval around the difference
#'   between \code{nps.x} and \code{nps.y}} \item{conf}{The confidence level
#'   used when performing the test, as specificed by \code{conf} in the function
#'   parameters} \item{p.value}{The p value of the significance test}
#'   \item{sig}{\code{\link{logical}}. Whether or not the \code{p.value} of the
#'   test exceeded 1-\code{conf}} \item{se.hat}{The estimated standard error of
#'   \code{delta} for a two sample test, otherwise of \code{x}}
#'   \item{type}{\code{\link{character}} string indicating whether a one or two
#'   sample test was performed} \item{n.x, n.y}{Counts for \code{x} and
#'   \code{y}}
#' @aliases print.nps_test
#' @export
#' @seealso \code{\link{nps_var}}, \code{\link{nps_se}}, \code{\link{nps}}
#' @author Brendan Rocks \email{foss@@brendanrocks.com}
nps_test <- function(x, y = NULL, test = "wald", conf = .95,
                     breaks = getOption("nps.breaks")){

  alpha <- 1-conf
  z     <- qnorm(1-alpha/2)

  nps.x <- nps(x)
  var.x <- nps.var(x)
  n.x   <- sum(!is.na(npc(x)))

  type <- if(is.null(y)) "One sample" else "Two sample"

  if (type == "One sample") {
    se.hat  <- sqrt(var.x/n.x)
    int     <- c(nps.x + z * sqrt(var.x/n.x), nps.x - z * se.hat)
    p.value <- 1 - (pnorm(abs(nps.x - 0) / se.hat) * 2 -1)
    delta   <- abs(0 - nps.x)
    nps.y   <- n.y <- NA
  }

  if (type == "Two sample") {
    nps.y  <- nps(y)
    var.y  <- nps.var(y)
    n.y    <- sum(!is.na(npc(y)))

    delta   <- abs(nps.x - nps.y)
    se.hat  <- sqrt((var.x/n.x) + (var.y/n.y))
    int     <- c(delta - z * se.hat, delta + z * se.hat)
    p.value <- 1 - (pnorm(delta / se.hat) * 2 -1)
  }

  out <- list(nps.x = nps.x, nps.y = nps.y, delta = delta, int = int,
              conf = conf, p.value = p.value, sig = p.value < alpha,
              se.hat = se.hat, type = type, n.x = n.x, n.y = n.y)

  class(out) <- "nps_test"
  return(out)
}


#' @return \code{NULL}
#'
#' @rdname nps_test
#' @export
print.nps_test <- function(x, ...) {
  cat(x$type, "Net Promoter Score Z test\n\n")

  cat("NPS of x: ", round(x$nps.x,2), " (n = " , x$n.x, ")\n", sep = "")

  if (x$type == "Two sample") {
    cat("NPS of y: ", round(x$nps.y,2), " (n = " , x$n.y, ")\n", sep = "")
    cat("Difference:", round(x$delta,2), "\n")
  }
  cat("\n")

  cat(
    if (x$type == "One sample") "Standard error of x:"
    else
      "Standard error of difference:"
    , round(x$se.hat, 3), "\n")

  cat("Confidence level:", x$conf, "\n")
  cat("p value:", x$p.value, "\n")
  cat("Confidence interval:", x$int, "\n\n")
}

