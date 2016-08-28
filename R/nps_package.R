#' Convenience Functions and Tests for Working With the Net Promoter Score (NPS)
#'
#' Small functions to make working with survey data in the context of a Net
#' Promoter programme easier. Specifically, data transformation methods, some
#' methods for examining the statistical properties of the NPS, such as its
#' variance and standard errors, and some simple inferential testing procedures.
#' Net Promoter and NPS are registered trademarks of Bain & Company, Satmetrix
#' Systems and Fred Reichheld.
#'
#'
#' @name NPS-package
#' @aliases NPS
#' @docType package
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
#' @keywords package
NULL

#' Test Data
#'
#' An idealised test data set, for demonstrating some of the NPS functions
#'
#' @name testdata
#' @docType data
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
#' @keywords data
NULL

#' Changes in NPS 2.x
#'
#' @description {
#' NPS version 2.x makes substantial changes, some of which may disrupt or alter
#' the interpretation of existing code based on NPS. Most prominently:
#'
#'   \describe{
#'     \item{\bold{Updated Interval Estimation Procedures}}{
#'       Intervals for the Net Promoter Score are calculated using a new method,
#'       with superior performance. See ?\code{\link{nps_test}}  for details.
#'     }
#'     \item{\bold{Function names are now in \code{snake_case}}}{
#'       No more dots. For example, \code{nps.se} is now \code{nps_se}. For the
#'       first 2.x release, these will be \emph{depricated} (they will work for
#'       now, but with a \code{\link{warning}}). It is strongly suggested that
#'       you move to using the new function names as soon as possible.
#'     }
#'     \item{\bold{Global options}}{
#'       Are now available, meaning that you can set options for how \code{NPS}
#'       should work once, and it will stick with them for the rest of the
#'       session. For example, suppose that your Net Promoter study uses a
#'       5pt Likert scale, with 1-3 coded as Detractors, 4 as Passives, and 5
#'       as Detractors. Instead of having to supply an argument to the
#'       \code{breaks} parameter each time you wish to use an NPS function, you
#'       can now set \code{options(nps.breaks = c(1,3,4,5))} at the start of
#'       your session, and all calculations and tests will now be based on your
#'       category defintions.
#'     }
#'     \item{\bold{\code{NA} no longer always omitted by default}}{
#'       While certain parts of \code{R} return \code{NA} if there's an
#'       \code{NA} in the input, others (such as tests) tend to use
#'       \code{na.omit} by default. v2.x of NPS now follows this convention;
#'       \code{\link{nps}} will return \code{NA} if there's any \code{NA}s in
#'       the input, whereas \code{nps_test} will perform \code{na.omit} and
#'       emit a warning.
#'     }
#'     \item{\bold{Distinction between aggegate and non-aggregate functions}}{
#'       The two most common ways to describe 'Net Promoter responses' are as
#'       vectors of either Likert responses, e.g.:
#'
#'       \code{c(0, 0, 0, 7, 7, 10))}
#'
#'       or as aggregate counts of the categories, e.g., for the same
#'       distribution:
#'
#'       \code{c(Detractors = 3, Passives = 2, Promoters = 1))}
#'
#'       NPS v2.x delineates functions for these data types more clearly, with
#'       those for the latter style always ending in \code{_}. All v1.x
#'       functions have equivalents, but those with the old naming convention
#'       have been deprecated.
#'     }
#'   }
#' }
#'
#' @name npsv2
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
NULL
