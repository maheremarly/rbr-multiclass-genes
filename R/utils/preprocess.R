# Loading, screening, cleaning and splitting one microarray dataset.
#
# The data come from the datamicroarray package, where each dataset is a list
# with an n x p expression matrix `x` and a class vector `y`.


#' Load one dataset from datamicroarray as a data frame with target `y`.
load_dataset <- function(name) {
  env <- new.env()
  utils::data(list = name, package = "datamicroarray", envir = env)
  if (!exists(name, envir = env)) {
    stop(
      "datamicroarray has no dataset '", name, "'. ",
      "See describe_data() in that package for the full list.",
      call. = FALSE
    )
  }

  dataset <- get(name, envir = env)
  out <- as.data.frame(dataset$x)
  out$y <- as.factor(dataset$y)
  out
}


#' Order the features by ascending FDR-adjusted ANOVA p-value.
#'
#' With `n_prefilter` at or above the feature count this is a REORDERING, not a
#' filter: every feature is kept, most significant first, which is why `x1` is
#' the most significant gene after clean_dataset() renames the columns.
prefilter_by_anova <- function(data, n_prefilter) {
  features <- setdiff(colnames(data), "y")

  pvals <- vapply(features, function(feature) {
    fit <- stats::aov(data[[feature]] ~ data$y)
    summary(fit)[[1]][["Pr(>F)"]][1]
  }, numeric(1))

  p_adjusted <- sort(stats::p.adjust(pvals, method = "fdr"))

  keep <- min(n_prefilter, length(p_adjusted))
  if (keep < n_prefilter) {
    message(
      "n_prefilter (", n_prefilter, ") exceeds the ", length(p_adjusted),
      " available features; keeping all of them, reordered by p-value."
    )
  }

  data[, c(names(p_adjusted)[seq_len(keep)], "y")]
}


#' Drop uninformative columns and put the frame in modelling form.
#'
#' Removes incomplete rows, constant columns, every factor column except the
#' target, and near-zero-variance columns; makes the remaining features
#' numeric; then renames the features x1 ... xp and relabels the target's
#' levels 1 ... k.
clean_dataset <- function(data) {
  data <- stats::na.omit(data)
  data <- Filter(function(x) length(unique(x[!is.na(x)])) > 1, data)

  binary_cols <- names(Filter(function(x) length(unique(x[!is.na(x)])) == 2, data))
  data[binary_cols] <- lapply(data[binary_cols], factor)
  drop_factors <- setdiff(names(Filter(is.factor, data)), "y")
  data <- data[, !names(data) %in% drop_factors]

  near_zero <- caret::nearZeroVar(data, freqCut = 99 / 1, names = TRUE)
  data[near_zero] <- NULL

  int_cols <- names(which(sapply(data, is.integer) & names(data) != "y"))
  data <- data %>% dplyr::mutate(dplyr::across(dplyr::all_of(int_cols), as.numeric))

  colnames(data) <- paste0("x", seq_len(ncol(data)))
  colnames(data)[ncol(data)] <- "y"

  data$y <- as.factor(data$y)
  levels(data$y) <- seq_along(levels(data$y))

  data
}


#' Stratified split into equal halves: one for ranking, one for evaluation.
split_dataset <- function(data, train_fraction, seed) {
  set_seed(seed)
  train_rows <- caret::createDataPartition(
    data[["y"]], p = train_fraction, list = FALSE
  )
  list(
    train = data[train_rows, ],
    test  = data[-train_rows, ],
    all   = data
  )
}


#' Load, screen, clean and split one dataset in a single call.
prepare_dataset <- function(cfg) {
  message("==> Loading ", cfg$dataset, " from datamicroarray")
  raw <- load_dataset(cfg$dataset)
  message(
    "    ", nrow(raw), " samples, ", ncol(raw) - 1, " genes, ",
    length(levels(raw$y)), " classes"
  )

  message("==> Ordering features by FDR-adjusted ANOVA p-value")
  screened <- prefilter_by_anova(raw, cfg$n_prefilter)

  message("==> Cleaning")
  clean <- clean_dataset(screened)
  message("    ", nrow(clean), " samples, ", ncol(clean) - 1, " genes retained")

  split <- split_dataset(clean, cfg$train_fraction, cfg$split_seed)
  message(
    "==> Split ", nrow(split$train), " for ranking / ",
    nrow(split$test), " for evaluation"
  )
  split
}
