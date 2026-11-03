#!/usr/bin/env Rscript
# Stage 3: the paper's four-panel micro-F1 figure for one dataset.

source("R/utils/setup.R")
source_modules()


#' Draw the figure from stage 2's saved results.
make_dataset_figure <- function(cfg) {
  path <- file.path(output_dir(cfg, "evaluation"), "cv_results.rds")
  if (!file.exists(path)) {
    stop(
      "Results not found: ", path,
      "\nRun scripts/evaluate_classifiers.R --dataset ", cfg$dataset, " first.",
      call. = FALSE
    )
  }
  results <- readRDS(path)

  make_figure(
    results_list = results,
    stepsize     = cfg$stepsize,
    methods      = colnames(results[[1]]),
    path         = file.path(
      output_dir(cfg, "figure"), paste0(cfg$dataset, "_results_micro.pdf")
    )
  )
}


main <- function() {
  args <- parse_args(list(
    dataset = NULL,
    config  = "configs/datasets.yml"
  ))
  require_args(args, "dataset")

  load_packages()
  cfg <- resolve_dataset(args$dataset, load_config(args$config))

  make_dataset_figure(cfg)
}


if (sys.nframe() == 0) main()
