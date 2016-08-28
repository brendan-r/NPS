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
#'   \item{nps.x, nps.y}{The Net Promoter score(s)} \item{delta.hat}{Where both
#'   \code{x} and \code{y} have been specified, the absolute difference between
#'   the two scores} \item{int}{The confidence interval generated. For a one
#'   sample test, this will be a confidence interval around \code{nps.x}. For a
#'   two sample test, this will be a confidence interval around the difference
#'   between \code{nps.x} and \code{nps.y}} \item{conf}{The confidence level
#'   used when performing the test, as specificed by \code{conf} in the function
#'   parameters} \item{p.value}{The p value of the significance test}
#'   \item{sig}{\code{\link{logical}}. Whether or not the \code{p.value} of the
#'   test exceeded 1-\code{conf}} \item{se.hat}{The estimated standard error of
#'   \code{delta.hat} for a two sample test, otherwise of \code{x}}
#'   \item{type}{\code{\link{character}} string indicating whether a one or two
#'   sample test was performed} \item{n.x, n.y}{Counts for \code{x} and
#'   \code{y}}
#' @aliases print.nps_test, nps_test_
#' @export
#' @seealso \code{\link{nps_var}}, \code{\link{nps_se}}, \code{\link{nps}}
#' @author Brendan Rocks \email{foss@@brendanrocks.com}
nps_test <- function(
  x, y = NULL, test = "wald", conf = .95,
  pseudo_observations = c(0.75, 1.50, 0.75), breaks = getOption("nps.breaks"),
  nps.100 = getOption("nps.100")
) {

  if (is.null(y)) {
    # If there's no y supplied, don't pass it
    nps_test_(
      npc(x, breaks = breaks), test = test, conf = conf,
      pseudo_observations = pseudo_observations
    )
  } else {
    # But if there is, do
    nps_test_(
      npc(x, breaks = breaks), npc(y, breaks = breaks), test = test,
      conf = conf, pseudo_observations = pseudo_observations
    )
  }
}



#' @name nps_test
#' @export
nps_test_ <- function(
  x, y = NULL, test = "wald", conf = .95,
  pseudo_observations = c(0.75, 1.50, 0.75), nps.100 = getOption("nps.100")
) {

  # Working with the multiply by 100 thing is too much of a headache during
  # interval construction. Set to FALSE here, turn it back on before exiting
  nps.100.user_setting <- getOption("nps.100")
  options(nps.100 = FALSE)
  on.exit(options(nps.100 = nps.100.user_setting))

  # A function for adding Agresti-Coull weights, if reqested
  add_wts <- function(x) {
    if (test == "adjusted.wald") {
      x <- x + pseudo_observations
    }
    x
  }

  # Interval parameters
  alpha <- 1 - conf
  z     <- stats::qnorm(1 - alpha / 2)

  # Stats before the addition of weights
  nps.x.raw <- nps_(x)
  n.x.raw   <- sum(x)

  # Basic stats for x
  x.hat <- add_wts(x)

  nps.x <- nps_(x.hat)
  var.x <- nps_var_(x.hat)
  n.x   <- sum(x.hat)

  type <- if (is.null(y)) "One sample" else "Two sample"

  # At the moment, you're not supporting the iterative test in the two sample
  # case. Exit if this is selected.

  if (type != "One sample" & test == "iterative") {

    stop("The iterative interval estimation procedure is currently only",
         "supported for one sample tests.")

  } else if (type == "One sample" & test == "iterative") {

    int <- nps_format(iterative(x, alpha), nps.100)

    sig <- min(int) > 0
    delta.hat <- abs(0 - nps.x.raw)

    p.value <- nps.y.raw <- n.y.raw <- se.hat <- NULL

  } else if (type == "One sample" & test != "iterative") {

    se.hat    <- sqrt(var.x / n.x)
    int       <- c(nps.x - z * se.hat, nps.x + z * se.hat)
    p.value   <- 1 - (stats::pnorm(abs(nps.x - 0) / se.hat) * 2 - 1)
    delta.hat <- abs(0 - nps.x)
    sig       <- p.value < alpha

    # NULL out non-existent quantities for the return object
    nps.y.raw <- n.y.raw <- NULL

  } else if (type == "Two sample" & test != "iterative") {

    # Stats before the addition of weights
    nps.y.raw <- nps_(y)
    n.y.raw   <- sum(y)

    # Stats after the addition of weights (used for interval construction)
    y.hat  <- add_wts(y)
    nps.y  <- nps_(y.hat)
    var.y  <- nps_var_(y.hat)
    n.y    <- sum(y.hat)

    delta.hat <- abs(nps.x - nps.y)
    se.hat    <- sqrt((var.x / n.x) + (var.y / n.y))
    int       <- c(delta.hat - z * se.hat, delta.hat + z * se.hat)
    p.value   <- 1 - (stats::pnorm(delta.hat / se.hat) * 2 - 1)
    sig       <- p.value < alpha
  }

  # Re-set the user's preference for NPS units
  options(nps.100 = nps.100.user_setting)

  out <-
    list(
      nps.x = nps_format(nps.x.raw),
      nps.y = nps_format(nps.y.raw),
      delta.hat = nps_format(delta.hat),
      int = nps_format(int),
      conf = conf,
      p.value = p.value,
      sig = sig,
      se.hat = nps_format(se.hat),
      type = type,
      n.x = n.x.raw,
      n.y = n.y.raw
    )

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
    cat("Difference:", round(x$delta.hat,2), "\n")
  }
  cat("\n")

  if (!is.null(x$se.hat)) {
    cat(
      if (x$type == "One sample") "Standard error of x:"
      else
        "Standard error of difference:"
      , round(x$se.hat, 3), "\n"
    )
  }

  cat("Confidence level:", x$conf, "\n")

  if (!is.null(x$p.value)) {
    cat("p value:", x$p.value, "\n")
  }

  cat("Confidence interval:", x$int, "\n\n")
}

