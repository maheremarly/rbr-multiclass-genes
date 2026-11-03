# Paired comparison of the RBR ranking against each baseline.
#
# For one dataset and one classifier the ten subset sizes give ten paired micro
# F1 scores per baseline. Significance is a two-sided Wilcoxon signed-rank test
# on those pairs; the effect is the median paired difference, reported in
# percentage points, so that a positive value is a gain for RBR.


RBR_METHOD <- "Rb_St"


#' Paired test of the RBR ranking against one baseline.
#'
#' @param results One classifier's results table, subset sizes by method.
#' @param baseline Column to compare the reference against.
#' @param reference Column holding the RBR scores.
#' @return A one-row data frame: pairs used, p-value, median gap.
paired_comparison <- function(results, baseline, reference = RBR_METHOD) {
  for (column in c(reference, baseline)) {
    if (!column %in% colnames(results)) {
      stop("No '", column, "' column in the results table.", call. = FALSE)
    }
  }

  rbr <- results[, reference]
  other <- results[, baseline]
  usable <- is.finite(rbr) & is.finite(other)
  differences <- rbr[usable] - other[usable]

  p_value <- if (length(differences) == 0 || all(differences == 0)) {
    NA_real_
  } else {
    suppressWarnings(
      stats::wilcox.test(
        rbr[usable], other[usable],
        paired = TRUE, alternative = "two.sided"
      )$p.value
    )
  }

  data.frame(
    baseline = baseline,
    n_pairs = length(differences),
    p_value = p_value,
    median_gap_pp = if (length(differences) == 0) {
      NA_real_
    } else {
      100 * stats::median(differences)
    },
    stringsAsFactors = FALSE
  )
}


#' Every baseline comparison for one dataset, across all classifiers.
#'
#' @param results Named list of results tables, one per classifier.
#' @param dataset Name recorded in the `dataset` column.
#' @return A data frame, one row per classifier and baseline.
compare_dataset <- function(results, dataset, reference = RBR_METHOD) {
  baselines <- setdiff(colnames(results[[1]]), reference)

  rows <- lapply(names(results), function(classifier) {
    per_baseline <- lapply(
      baselines, paired_comparison,
      results = results[[classifier]], reference = reference
    )
    data.frame(
      dataset = dataset,
      classifier = classifier,
      do.call(rbind, per_baseline),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}


#' One dataset's comparisons as a baseline-by-classifier table.
#'
#' @param comparisons Rows from compare_dataset().
#' @param value Column to place in the cells.
comparison_table <- function(comparisons, value) {
  wide <- reshape2::acast(
    comparisons, baseline ~ classifier, value.var = value
  )
  wide[, intersect(unique(comparisons$classifier), colnames(wide)), drop = FALSE]
}
