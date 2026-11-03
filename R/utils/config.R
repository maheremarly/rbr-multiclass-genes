# Reads configs/datasets.yml and resolves one dataset's settings.
#
# Every dataset name, seed, fold count and shortlist size lives in the YAML
# file, so that no seed appears as a bare set.seed() call in the code.


#' Load the configuration file.
load_config <- function(path = "configs/datasets.yml") {
  if (!file.exists(path)) {
    stop("Configuration file not found: ", path, call. = FALSE)
  }
  yaml::yaml.load_file(path)
}


#' Resolve one dataset entry into a complete, validated run configuration.
#'
#' Per-dataset values override the `defaults:` block of the YAML file.
resolve_dataset <- function(dataset, config) {
  if (is.null(config$data[[dataset]])) {
    stop(
      "Unknown dataset '", dataset, "'. Available: ",
      paste(names(config$data), collapse = ", "),
      call. = FALSE
    )
  }

  cfg <- utils::modifyList(config$defaults, config$data[[dataset]])
  cfg$dataset <- dataset

  stopifnot(is.numeric(cfg$shortlist), cfg$shortlist >= 1)
  stopifnot(is.numeric(cfg$cv_folds), cfg$cv_folds >= 2)
  stopifnot(is.numeric(cfg$inner_folds), cfg$inner_folds >= 2)
  stopifnot(is.numeric(cfg$rbr_draws), is.numeric(cfg$rbr_bins))
  stopifnot(all(cfg$stepsize > 0), all(cfg$stepsize <= 1))
  stopifnot(cfg$train_numerator < cfg$train_denominator)

  # Held as two integers and divided here, so the split fraction is exactly the
  # one R computes rather than a rounded decimal from the YAML file.
  cfg$train_fraction <- cfg$train_numerator / cfg$train_denominator

  # One fit seed per classifier, keyed by the names in CLASSIFIERS.
  stopifnot(is.list(cfg$fit_seeds))

  cfg$stepsize <- as.numeric(cfg$stepsize)
  cfg
}


#' Seed to set before a given classifier's fits.
fit_seed <- function(cfg, classifier) {
  seed <- cfg$fit_seeds[[classifier]]
  if (is.null(seed)) {
    stop(
      "No fit seed configured for '", classifier, "'. Add it under ",
      "`fit_seeds:` in the configuration file.",
      call. = FALSE
    )
  }
  seed
}


#' Directory this dataset's outputs are written to, created if absent.
output_dir <- function(cfg, stage, root = "output") {
  path <- file.path(root, cfg$dataset, stage)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}
