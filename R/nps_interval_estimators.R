
# Here's Tago:
iterative <- function(x, p) {
  n <- sum(x)
  pa <- 2 * n
  z <- qnorm(p)
  c <- x[3]
  b <- x[1]
  nps <- (c - b) / n

  if (c == n) {
    ul <- 1
  } else {
    dp    <- 1 - nps
    niter <- 1
    while (niter <= 50) {
      dp    <- 0.5 * dp
      up2   <- nps + dp
      pb    <- -b - c + (2 * n - c + b) * up2
      pc    <- -b * up2 * (1 - up2)
      q21   <- (sqrt(pb ^ 2 - 4 * pa * pc) - pb) / (2 * pa)
      score <- (c - b - n * up2) / sqrt(n * (2 * q21 + up2 * (1 - up2)))

      if (abs(score) < z) {
        nps = up2
      }

      niter <- niter + 1
      if ((dp < 0.0000001) || (abs(z - score) < 0.000001)) {
        niter <- 51
        ul    <- up2
      }
    }
  }

  if (b == n) {
    ll <- -1
  } else {
    dp    <- 1 + nps
    niter <- 1
    while (niter <= 50) {
      dp    <- 0.5 * dp
      low2  <- nps - dp
      pb    <- -b - c + (2 * n - c + b) * low2
      pc    <- -b * low2 * (1 - low2)
      q21   <- (sqrt(pb ^ 2 - 4 * pa * pc) - pb) / (2 * pa)
      score <- (c - b - n * low2) / sqrt(n * (2 * q21 + low2 * (1 - low2)))

      if (abs(score) < z) {
        nps = low2
      }

      niter <- niter + 1

      if ((dp < 0.0000001) || (abs(z - score) < 0.000001)) {
        ll    <- low2
        niter <- 51
      }
    }
  }
  c(ll, ul)
}
