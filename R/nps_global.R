#' Small function to emit warnings for using functions in the old syntax.
nps2_name_check <- function() {
  parent <- deparse(sys.calls()[[sys.nframe() - 1]])
  warning(parent, " is depricated, and will be removed in the next release.\n",
          "Use ", gsub("\\.", "_", parent), " instead.")
}

nps_format <- function(x) {
  # If the user has set an option to get all NPS stats multiplied by 100, do
  # this for them
  if (getOption("nps.100")) {
    return(x * 100)
  }

  # Otherwise, don't.
  return(x)
}


#' Display global options for NPS
#'
#' Run \code{nps_options()} to see what it's possible to set globally.
#'
#' @details
#' Options can be set in the usual way using \code{\link{options}}, and include:
#'
#' \describe{
#'   \item{\code{nps.breaks}}{
#'     A \code{\link{numeric}} \code{\link{vector}} of length 4, containing the
#'     extent of the measurement scale used for calculating NPS, and the
#'     'breaks' for transforming it into categories.
#'
#'     The four entries are:
#'
#'     1. The lowest valid value of the scale
#'
#'     2. The highest value for a Detractor
#'
#'     3. The highest value for a Passive
#'
#'     4. The highest valid value of the scale
#'
#'     So, for example, the 'best practice' values for Net Promoter (and the
#'     package's default), is the vector \code{c(0, 6, 8, 10)}.
#'   }
#'   \item{\code{nps.100}}{
#'     By default, \code{NPS} presents scores as being between -1 and 1.
#'     However, scores are often multiplied by 100 for presentational purposes
#'     (producing a score between -100 and 100). Should the \code{NPS} package
#'     do this by default? \code{\link{logical}}.
#'   }
#' }
#'
#' @return A \code{list} of the options available
#'
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
nps_options <- function() {
  avail <- c(
    "nps.breaks",
    "nps.100"
  )

  o <- options()

  message("To set an option, use options()")
  message("e.g.: options(nps.100 = TRUE)")

  return(o[names(o) %in% avail])
}
