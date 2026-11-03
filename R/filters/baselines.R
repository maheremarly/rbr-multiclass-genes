# The five filters the paper compares the RBR against.
#
# Information gain, symmetric uncertainty and mRMR score a relevant feature
# HIGH. ReliefF also scores it high. The ANOVA p-value scores it LOW. Direction
# is handled once, in R/utils/rankings.R, not here.


#' One-way ANOVA p-value per feature.
anova_filter <- function(data, target) {
  features <- setdiff(colnames(data), target)
  pvals <- vapply(features, function(feature) {
    fit <- stats::aov(data[[feature]] ~ data[[target]])
    summary(fit)[[1]][["Pr(>F)"]][1]
  }, numeric(1))

  out <- data.frame("ANOVA P-Value" = pvals, check.names = FALSE)
  rownames(out) <- features
  out
}


#' Minimum redundancy maximum relevance score per feature.
#'
#' Asking praznik for all features makes this a ranking of the whole set rather
#' than a selection of a subset.
mrmr_filter <- function(data, target) {
  predictors <- data[, setdiff(colnames(data), target), drop = FALSE]
  scores <- praznik::MRMR(predictors, data[[target]], ncol(predictors))$score
  data.frame(MRMR = as.numeric(scores), row.names = names(scores))
}


#' ReliefF score per feature, with the exponential-rank neighbour weighting.
#'
#' Draws random numbers, so it takes its own seed.
relieff_filter <- function(data, target, seed) {
  set.seed(seed)
  formula <- stats::as.formula(paste(target, "~ ."))
  CORElearn::attrEval(formula, data, estimator = "ReliefFexpRank")
}


#' Score every feature with all six filters and return one aligned table.
#'
#' @param data Data frame of numeric features plus the target column `y`.
#' @param prior Output of rbr_prior().
#' @param cfg Resolved dataset configuration.
#' @return A data frame with one row per feature and one column per filter:
#'   feature, RB_dist, strength, "Information Gain", "Symmetric Uncertainty",
#'   "ANOVA P-Value", MRMR, ReliefF.
score_all_filters <- function(data, prior, cfg) {
  message("==> RBR filter over ", ncol(data) - 1, " features")
  filters <- rbr_filter(
    data, "y",
    prior = prior, B = cfg$rbr_draws, M = cfg$rbr_bins, seed = cfg$rbr_seed
  )

  message("==> Information gain and symmetric uncertainty")
  ig <- FSelector::information.gain(y ~ ., data)
  colnames(ig) <- "Information Gain"
  su <- FSelector::symmetrical.uncertainty(y ~ ., data)
  colnames(su) <- "Symmetric Uncertainty"

  message("==> ANOVA")
  anova_scores <- anova_filter(data, "y")

  # information.gain, symmetrical.uncertainty and anova_filter all return rows
  # in the data's column order, which is the order rbr_filter used, so these
  # line up positionally as well as by name.
  filters <- cbind(filters, ig, su, anova_scores)

  message("==> mRMR")
  # merge() reorders the rows alphabetically by feature name. That ordering is
  # kept because it decides how ties are broken when the rankings are built.
  filters <- merge(filters, mrmr_filter(data, "y"), by = "row.names", all = TRUE)
  rownames(filters) <- filters$Row.names
  filters$Row.names <- NULL

  message("==> ReliefF")
  relief <- relieff_filter(data, "y", seed = cfg$relief_seed)
  filters$ReliefF <- relief[match(rownames(filters), names(relief))]

  filters
}
