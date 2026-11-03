#!/usr/bin/env Rscript
# Stage 2: cross-validate the four classifiers over every ranking and every
# subset size, scoring each by micro-averaged F1. Hyperparameters are selected
# inside each training partition before the held-out split is scored.

source("R/utils/setup.R")
source_modules()


#' Read stage 1's shortlist back as a character matrix.
load_rankings <- function(cfg) {
  path <- file.path(output_dir(cfg, "ranking"), "feature_rankings.csv")
  if (!file.exists(path)) {
    stop(
      "Rankings not found: ", path,
      "\nRun scripts/rank_features.R --dataset ", cfg$dataset, " first.",
      call. = FALSE
    )
  }
  as.matrix(utils::read.csv(path, colClasses = "character"))
}


#' Cross-validate every classifier and write the results.
evaluate_classifiers <- function(cfg) {
  split <- prepare_dataset(cfg)
  rankings <- load_rankings(cfg)

  missing <- setdiff(as.vector(rankings), colnames(split$test))
  if (length(missing) > 0) {
    stop(
      length(missing), " shortlisted gene(s) are not in the prepared data, ",
      "starting with '", missing[1], "'. Rerun stage 1 with the current ",
      "configuration.",
      call. = FALSE
    )
  }

  set_seed(cfg$cv_seed)
  folds <- caret::createFolds(factor(split$test$y), k = cfg$cv_folds)
  message(
    "==> ", cfg$cv_folds, "-fold cross-validation on ", nrow(split$test),
    " evaluation samples, shortlist of ", nrow(rankings), " genes"
  )
  message("==> ", cfg$inner_folds, " inner folds per hyperparameter search")

  results <- list()
  for (name in names(CLASSIFIERS)) {
    message("==> ", name, " (seed ", fit_seed(cfg, name), ")")
    results[[name]] <- cv_over_subsets(
      eval_set    = split$test,
      folds       = folds,
      rankings    = rankings,
      stepsize    = cfg$stepsize,
      seed        = fit_seed(cfg, name),
      inner_folds = cfg$inner_folds,
      fit_predict = CLASSIFIERS[[name]]
    )
  }

  out <- output_dir(cfg, "evaluation")
  workbook <- file.path(out, paste0(cfg$dataset, "_results.xlsx"))
  openxlsx::write.xlsx(
    lapply(results, as.data.frame), file = workbook, rowNames = TRUE
  )
  saveRDS(results, file.path(out, "cv_results.rds"))

  message("Results written to ", workbook)
  invisible(results)
}


main <- function() {
  args <- parse_args(list(
    dataset = NULL,
    config  = "configs/datasets.yml"
  ))
  require_args(args, "dataset")

  load_packages()
  cfg <- resolve_dataset(args$dataset, load_config(args$config))

  evaluate_classifiers(cfg)
}


if (sys.nframe() == 0) main()
