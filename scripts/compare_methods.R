#!/usr/bin/env Rscript
# Stage 4: Wilcoxon signed-rank comparison of the RBR ranking against each
# baseline, over the subset sizes, for every dataset that has stage 2 results.

source("R/utils/setup.R")
source_modules()


#' Datasets named on the command line, or every one with saved results.
requested_datasets <- function(args, config) {
  if (!is.null(args$datasets)) {
    return(trimws(strsplit(args$datasets, ",")[[1]]))
  }

  available <- names(config$data)
  available[vapply(available, function(dataset) {
    file.exists(file.path("output", dataset, "evaluation", "cv_results.rds"))
  }, logical(1))]
}


#' Read one dataset's saved results tables.
load_results <- function(cfg) {
  path <- file.path(output_dir(cfg, "evaluation"), "cv_results.rds")
  if (!file.exists(path)) {
    stop(
      "Results not found: ", path,
      "\nRun scripts/evaluate_classifiers.R --dataset ", cfg$dataset, " first.",
      call. = FALSE
    )
  }
  readRDS(path)
}


#' Compare every dataset and write one workbook holding all of it.
compare_methods <- function(datasets, config, out_dir) {
  comparisons <- lapply(datasets, function(dataset) {
    cfg <- resolve_dataset(dataset, config)
    message("==> ", dataset)
    compare_dataset(load_results(cfg), dataset)
  })
  names(comparisons) <- datasets

  all_rows <- do.call(rbind, comparisons)
  sheets <- c(
    list(comparisons = all_rows),
    lapply(comparisons, comparison_table, value = "p_value")
  )

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  workbook <- file.path(out_dir, "method_comparisons.xlsx")
  openxlsx::write.xlsx(sheets, file = workbook, rowNames = TRUE)
  utils::write.csv(
    all_rows, file.path(out_dir, "method_comparisons.csv"), row.names = FALSE
  )

  significant <- sum(all_rows$p_value < 0.05, na.rm = TRUE)
  message(
    "==> ", nrow(all_rows), " comparisons, ", significant, " at p < 0.05"
  )
  message("Comparisons written to ", workbook)
  invisible(all_rows)
}


main <- function() {
  args <- parse_args(list(
    datasets = NULL,
    config   = "configs/datasets.yml",
    out_dir  = "output/comparisons"
  ))

  load_packages()
  config <- load_config(args$config)

  datasets <- requested_datasets(args, config)
  if (length(datasets) == 0) {
    stop(
      "No saved results found under output/. Run ",
      "scripts/evaluate_classifiers.R for at least one dataset first.",
      call. = FALSE
    )
  }

  compare_methods(datasets, config, args$out_dir)
}


if (sys.nframe() == 0) main()
