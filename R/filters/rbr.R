# The relative belief ratio (RBR) filter for k-class problems.
#
# The two-class filter asks for the evidence against mu1 = mu2. Here the null is
# mu1 = ... = muk, and the discrepancy psi is the sum, over the k classes, of
# the squared distance from each class mean to their precision-weighted pool,
# in units of that class's variance. psi is sampled B times from the prior and B
# times from the posterior; the relative belief ratio at psi -> 0 is the ratio of
# the two distribution functions at the 1/M prior quantile, and the strength is
# the posterior mass on discrepancies no more surprising than that ratio.
#
# A SMALL strength means strong evidence against equality of the class means, so
# features are ranked by ASCENDING strength (see R/utils/rankings.R).
#
# All k classes share one set of prior hyperparameters.


#' Per-class sample size, mean and variance of one feature.
#'
#' Groups are taken in the order of the target's factor levels.
class_summaries <- function(data, target, feature) {
  groups <- data %>%
    dplyr::group_by(.data[[target]]) %>%
    dplyr::group_split()

  values <- lapply(groups, function(g) g[[feature]])

  list(
    n      = vapply(values, length, numeric(1)),
    xbar   = vapply(values, mean, numeric(1), na.rm = TRUE),
    s_sqr  = vapply(values, stats::var, numeric(1), na.rm = TRUE)
  )
}


#' Relative belief ratio and strength for one feature, from its class summaries.
#'
#' @param summaries Output of class_summaries().
#' @param k Number of classes.
#' @param prior Output of rbr_prior().
#' @param B Monte Carlo sample size for the prior and posterior discrepancies.
#' @param M Number of prior quantile bins used to discretise the ratio.
#' @param seed Seed set before the draws, once per feature.
#' @return A one-row matrix with columns RB_dist and strength.
rbr_multiclass <- function(summaries, k, prior, B = 5000, M = 20, seed = 2) {
  # Reseeding per feature was the published behaviour: it makes the prior sample
  # identical for every feature, so features differ only through the posterior.
  set.seed(seed)

  alpha0  <- prior$alpha0
  beta0   <- prior$beta0
  mu0     <- prior$mu0
  lambda0 <- prior$lambda0

  n     <- summaries$n
  xbar  <- summaries$xbar
  s_sqr <- summaries$s_sqr

  # Posterior hyperparameters are free of the Monte Carlo index.
  alpha_post <- alpha0 + (n / 2)
  beta_post  <- beta0 + (n - 1) * s_sqr / 2 +
    n * (xbar - mu0)^2 / (2 * (n * lambda0^2 + 1))
  mu_x_post  <- ((mu0 / lambda0^2) + n * xbar) / (n + 1 / lambda0^2)

  d_prior <- matrix(0, B, 1)
  d_post  <- matrix(0, B, 1)

  var_prior <- numeric(k); mu_prior <- numeric(k)
  var_post  <- numeric(k); mu_post  <- numeric(k)

  # The draw order below is rgamma then rnorm for each class in turn, first for
  # the prior and then for the posterior. It fixes the random number stream:
  # do not reorder or vectorise this loop.
  for (i in 1:B) {
    for (j in 1:k) {
      var_prior[j] <- 1 / rgamma(1, alpha0, rate = beta0)
      mu_prior[j]  <- rnorm(1, mean = mu0, sd = lambda0 * sqrt(var_prior[j]))
    }

    pool_prior <- sum(mu_prior / var_prior) / sum(1 / var_prior)
    d_prior[i] <- sum((pool_prior - mu_prior)^2 / var_prior) / 2

    for (j in 1:k) {
      var_post[j] <- 1 / rgamma(1, alpha_post[j], beta_post[j])
      mu_post[j]  <- rnorm(
        1,
        mean = mu_x_post[j],
        sd   = sqrt(var_post[j] / (n[j] + 1 / lambda0^2))
      )
    }

    pool_post <- sum(mu_post / var_post) / sum(1 / var_post)
    d_post[i] <- sum((pool_post - mu_post)^2 / var_post) / 2
  }

  # Relative belief ratio at the smallest prior quantile bin.
  psi_prior <- matrix(0, M + 1, 1)
  for (i in 1:M) psi_prior[i + 1] <- quantile(d_prior, i / M)
  psi_star <- psi_prior[2]

  Fn_prior <- ecdf(d_prior)
  Fn_post  <- ecdf(d_post)
  RB_dist  <- Fn_post(psi_star) / Fn_prior(psi_star)

  # Strength: posterior mass on bins whose ratio does not exceed RB_dist.
  RBR_hat <- matrix(0, M, 1)
  for (i in 1:M) {
    RBR_hat[i] <- M * (Fn_post(psi_prior[i + 1]) - Fn_post(psi_prior[i]))
  }
  bin_mass <- matrix(0, M, 1)
  for (i in 1:M) {
    if (RBR_hat[i] <= RB_dist) {
      bin_mass[i] <- Fn_post(psi_prior[i + 1]) - Fn_post(psi_prior[i])
    }
  }

  cbind(RB_dist = RB_dist, strength = sum(bin_mass))
}


#' Score every feature of a data frame with the k-class RBR filter.
#'
#' @param data Data frame of numeric features plus a k-level target.
#' @param target Name of the target column.
#' @param prior Output of rbr_prior().
#' @param B,M Monte Carlo sample size and number of prior quantile bins.
#' @param seed Seed set before each feature's draws.
#' @return A data frame with one row per feature: feature, RB_dist, strength.
rbr_filter <- function(data, target, prior, B = 5000, M = 20, seed = 2) {
  data[[target]] <- as.factor(data[[target]])
  k <- length(levels(data[[target]]))

  features <- setdiff(colnames(data), target)

  rows <- lapply(features, function(feature) {
    summaries <- class_summaries(data, target, feature)
    result <- rbr_multiclass(summaries, k = k, prior = prior, B = B, M = M, seed = seed)
    data.frame(
      feature  = feature,
      RB_dist  = as.numeric(result[1]),
      strength = as.numeric(result[2]),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- out$feature
  out
}
