# Scoring, the results table, and how folds are combined.
#
# Multiclass performance is reported as the micro-averaged F1 score: the true
# positives, false positives and false negatives are pooled over the classes
# first, and precision and recall are formed from those pooled counts, so every
# sample counts equally.


#' Micro-averaged F1 score.
#'
#' Counts are pooled across classes before precision and recall are formed.
#' Precision or recall is taken as 0 when its denominator is 0, which happens
#' when nothing is predicted or the evaluation fold is empty.
micro_f1 <- function(predicted, truth) {
  classes <- levels(truth)
  counts <- vapply(classes, function(class) {
    c(
      tp = sum(predicted == class & truth == class, na.rm = TRUE),
      fp = sum(predicted == class & truth != class, na.rm = TRUE),
      fn = sum(predicted != class & truth == class, na.rm = TRUE)
    )
  }, numeric(3))

  tp <- sum(counts["tp", ])
  fp <- sum(counts["fp", ])
  fn <- sum(counts["fn", ])

  precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0
  recall    <- if ((tp + fn) > 0) tp / (tp + fn) else 0

  if ((precision + recall) > 0) {
    2 * precision * recall / (precision + recall)
  } else {
    0
  }
}


#' An empty results table: one row per subset size, one column per method.
init_cv_results <- function(stepsize, methods) {
  results <- matrix(NA_real_, nrow = length(stepsize), ncol = length(methods))
  rownames(results) <- paste0(stepsize * 100, "%")
  colnames(results) <- methods
  results
}


#' Mean of one results table per fold, cell by cell.
average_folds <- function(fold_results) {
  apply(abind::abind(fold_results, along = 3), 1:2, mean)
}


#' Align two factors on one set of levels, erroring if they cannot be.
align_levels <- function(predicted, truth) {
  truth <- factor(truth)
  predicted <- factor(predicted, levels = levels(truth))
  if (any(levels(predicted) != levels(truth))) {
    stop("Prediction and actual labels have mismatched levels.", call. = FALSE)
  }
  list(predicted = predicted, truth = truth)
}


#' A model formula selecting `features` as predictors of `y`.
subset_formula <- function(features) {
  stats::as.formula(paste("y ~", paste(features, collapse = " + ")))
}
