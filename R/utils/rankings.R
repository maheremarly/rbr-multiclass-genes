# Turning filter scores into one feature ranking per method.
#
# The five filters do not agree on direction:
#   strength (RBR)         ascending  -- small means strong evidence of a
#                                        difference between the class means
#   Information Gain       descending
#   Symmetric Uncertainty  descending
#   MRMR                   descending
#   ReliefF                descending
#
# Each direction is declared once, in RANKING_SPEC, rather than being implied by
# a call to arrange() several hundred lines apart.


#' Score column and sort direction for each feature selection method.
#'
#' The RBR filter and the four baselines the paper benchmarks it against. The
#' ANOVA p-value is still computed and written to the scores file, where it
#' orders the genes for the screen in preprocess.R.
RANKING_SPEC <- list(
  Rb_St   = list(column = "strength",              decreasing = FALSE),
  IG      = list(column = "Information Gain",      decreasing = TRUE),
  SU      = list(column = "Symmetric Uncertainty", decreasing = TRUE),
  MRMR    = list(column = "MRMR",                  decreasing = TRUE),
  ReliefF = list(column = "ReliefF",               decreasing = TRUE)
)


#' One ranked feature vector per method, as the columns of a character matrix.
#'
#' Ties keep the row order of `scores`, which is the alphabetical order the
#' mRMR merge leaves behind.
#'
#' @param scores Output of score_all_filters().
#' @param shortlist Number of top-ranked features to keep per method.
#' @param methods Methods to rank with; defaults to all of RANKING_SPEC.
#' @return A character matrix, `shortlist` rows, one column per method.
build_rankings <- function(scores, shortlist = 100,
                           methods = names(RANKING_SPEC)) {
  if (shortlist > nrow(scores)) {
    stop(
      "shortlist (", shortlist, ") exceeds the ", nrow(scores),
      " scored features.",
      call. = FALSE
    )
  }

  ranked <- lapply(methods, function(method) {
    spec <- RANKING_SPEC[[method]]
    if (is.null(spec)) {
      stop(
        "Unknown ranking method '", method, "'. Available: ",
        paste(names(RANKING_SPEC), collapse = ", "),
        call. = FALSE
      )
    }
    values <- as.numeric(scores[[spec$column]])
    order_index <- order(values, decreasing = spec$decreasing)
    scores$feature[order_index][seq_len(shortlist)]
  })

  out <- do.call(cbind, ranked)
  colnames(out) <- methods
  out
}
