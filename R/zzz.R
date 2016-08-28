.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0(
    "Welcome to NPS ", utils::packageVersion("NPS"), "!\n",
    "Bug reports: https://github.com/brendan-r/NPS/issues\n\n",
    "'Breaking' API changes since version 1.x: See ?npsv2 for a summary."
  ))
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.nps <- list(
    nps.breaks = c(0, 6, 8, 10),
    nps.100    = FALSE
  )

  toset <- !(names(op.nps) %in% names(op))
  if (any(toset)) options(op.nps[toset])

  invisible()
}
