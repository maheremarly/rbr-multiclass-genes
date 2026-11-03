# Packages, module loading and seeding.
#
# Scripts are run from the repository root (see docs/reproducing.md), so modules
# are sourced by their path relative to it.


REQUIRED_PACKAGES <- c(
  "yaml", "dplyr", "FSelector", "praznik", "CORElearn", "caret",
  "glmnet", "class", "e1071", "randomForest", "abind", "reshape2",
  "ggplot2", "gridExtra", "scales", "openxlsx"
)

# Not on CRAN: install with
#   remotes::install_github("ramhiser/datamicroarray")
DATA_PACKAGE <- "datamicroarray"


#' Stop with one actionable message if any required package is missing.
check_packages <- function(packages = REQUIRED_PACKAGES) {
  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing) > 0) {
    stop(
      "Install the following package(s) before running: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (!requireNamespace(DATA_PACKAGE, quietly = TRUE)) {
    stop(
      DATA_PACKAGE, " is not installed. It is not on CRAN:\n",
      '  remotes::install_github("ramhiser/datamicroarray")',
      call. = FALSE
    )
  }
  invisible(TRUE)
}


#' Attach the packages whose functions are called unqualified.
load_packages <- function() {
  check_packages()
  suppressPackageStartupMessages({
    library(dplyr)
    library(FSelector)
    library(caret)
    library(e1071)
    library(ggplot2)
  })
  invisible(TRUE)
}


#' Source the project's modules, checking first that the cwd is the repo root.
source_modules <- function(root = ".") {
  if (!dir.exists(file.path(root, "R"))) {
    stop(
      "R/ not found. Run this script from the repository root, ",
      "for example: Rscript scripts/rank_features.R --dataset sorlie",
      call. = FALSE
    )
  }
  modules <- c(
    "R/utils/args.R",
    "R/utils/config.R",
    "R/utils/preprocess.R",
    "R/utils/rankings.R",
    "R/utils/metrics.R",
    "R/utils/comparisons.R",
    "R/utils/plots.R",
    "R/filters/prior.R",
    "R/filters/rbr.R",
    "R/filters/baselines.R",
    "R/models/classifiers.R"
  )
  for (module in modules) source(file.path(root, module))
  invisible(TRUE)
}


#' Set the random seed.
set_seed <- function(seed) {
  set.seed(seed)
  invisible(seed)
}
