# Elicitation of the conjugate prior used by the relative belief ratio filter.
#
# For each class i the model is
#   1 / sigma_i^2  ~ Gamma(alpha0, beta0)
#   mu_i | sigma_i ~ Normal(mu0, lambda0^2 * sigma_i^2)
#
# rbr_prior() is the specification the paper states, in the paragraph following
# Equation (9): one fixed elicitation scheme reused for every gene.
#
#   mu_0i = 1                  prior mean of every class mean
#   s_1i^2 = 1, s_2i^2 = 4     bounds on the half-length sigma_i Phi^-1(0.9995)
#   alpha_0i = 2, beta_0i = 2  shape and rate, fixed outright
#
# mu0 and lambda0 follow in closed form from the elicited mean range (m1, m2)
# and s2_sqr:
#
#   mu0         = (m1 + m2) / 2
#   lambda_0^2  = ((m2 - m1) / 2)^2 / s_2i^2
#
# so the mean range (0, 2) gives mu0 = 1 and lambda0 = 1 / 2.
#
# The elicitation equations can instead be solved for alpha0 and beta0, taking
# 2 and 2 as the starting values of a Newton iteration. That route sits below
# as solve_prior_hyperparameters(), reachable as --prior solved, on the wider
# bounds of 4 and 16 it is written for; it lands at alpha0 = 6.4546 and
# beta0 = 26.8415. The paper fixes alpha0 and beta0 outright, so it reaches no
# solve and the bounds enter only through lambda0.


#' The prior exactly as specified in the paper.
#'
#' @param m1,m2 Endpoints of the elicited range of the class means.
#' @param s1_sqr,s2_sqr Bounds on the squared half-length of the data range.
#'   Only s2_sqr enters lambda0; s1_sqr is recorded for reference.
#' @param alpha0,beta0 Shape and rate of the Gamma prior on the precision,
#'   fixed by the paper rather than solved.
#' @return A list with alpha0, beta0, mu0 and lambda0, plus the inputs.
rbr_prior <- function(m1 = 0, m2 = 2,
                      s1_sqr = 1, s2_sqr = 4,
                      alpha0 = 2, beta0 = 2) {
  list(
    alpha0  = alpha0,
    beta0   = beta0,
    mu0     = (m1 + m2) / 2,
    lambda0 = sqrt((((m2 - m1) / 2)^2) / s2_sqr),
    s1_sqr  = s1_sqr,
    s2_sqr  = s2_sqr,
    source  = "paper"
  )
}


#' Residuals of the two prior elicitation equations.
#'
#' The reciprocal bound is squared here, giving s_sqr^-2 where the elicitation
#' equation reads s^-2, which is one reason the solved values below are not the
#' paper's fixed ones.
elicit <- function(a, b, alpha, s1_sqr, s2_sqr) {
  m <- matrix(0, 2, 1)
  m[1] <- qgamma((1 + alpha) / 2, a, b) - ((1 / s1_sqr) * qnorm((1 + alpha) / 2))^2
  m[2] <- qgamma((1 - alpha) / 2, a, b) - ((1 / s2_sqr) * qnorm((1 + alpha) / 2))^2
  m
}


#' Solve the elicitation equations for alpha0 and beta0 by Newton's method.
#'
#' An alternative to the paper's fixed values. The defaults use bounds of 4 and
#' 16 rather than the paper's 1 and 4, giving alpha0 = 6.4546, beta0 = 26.8415
#' and lambda0 = 0.25. Reachable from scripts/rank_features.R as
#' --prior solved.
#'
#' The solve is deterministic and draws no random numbers, so the
#' hyperparameters are computed once per run and reused for every gene.
#'
#' @return A list shaped like rbr_prior(), plus the solver's iteration count.
solve_prior_hyperparameters <- function(m1 = 0, m2 = 2,
                                        s1_sqr = 4, s2_sqr = 16,
                                        alpha = 0.999,
                                        a = 2, b = 2,
                                        tol = 0.5, maxiter = 1000,
                                        h = 1e-9) {
  x <- matrix(c(a, b), 2, 1)
  delta <- x
  iter <- 0

  while (norm(delta) > tol && iter < maxiter) {
    fx <- elicit(x[1], x[2], alpha, s1_sqr, s2_sqr)
    Jx <- matrix(0, 2, 2)

    for (i in 1:2) {
      xh <- x
      xh[i] <- x[i] + h
      fxh <- elicit(xh[1], xh[2], alpha, s1_sqr, s2_sqr)
      Jx[, i] <- (fxh - fx) / h
    }

    delta <- -solve(Jx) %*% elicit(x[1], x[2], alpha, s1_sqr, s2_sqr)
    x <- x + delta
    iter <- iter + 1
  }

  lambda0_sq <- (((m2 - m1) / 2)^2) / s2_sqr

  list(
    alpha0  = x[1],
    beta0   = x[2],
    mu0     = (m1 + m2) / 2,
    lambda0 = sqrt(lambda0_sq),
    s1_sqr  = s1_sqr,
    s2_sqr  = s2_sqr,
    source  = "solved",
    iterations = iter
  )
}
