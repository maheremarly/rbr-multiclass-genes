#!/usr/bin/env Rscript
# Stage 1: score one dataset's genes with all six filters and shortlist the
# top-ranked genes for each.

source("R/utils/setup.R")
source_modules()


#' Prepare the dataset, score its genes, and write the scores and rankings.
rank_features <- function(cfg, args) {
  split <- prepare_dataset(cfg)

  # The paper fixes alpha0 and beta0; --prior solved instead reaches the
  # Newton elicitation route.
  prior <- switch(
    args$prior,
    paper  = rbr_prior(),
    solved = solve_prior_hyperparameters(),
    stop("--prior must be 'paper' or 'solved', not '", args$prior, "'.",
         call. = FALSE)
  )
  message(
    "==> Prior (", prior$source, "): ",
    sprintf(
      "alpha0 = %.4f, beta0 = %.4f, mu0 = %.4f, lambda0 = %.4f",
      prior$alpha0, prior$beta0, prior$mu0, prior$lambda0
    )
  )

  # Ranking uses the training half only; the evaluation half is untouched here.
  set_seed(cfg$score_seed)
  scores <- score_all_filters(split$train, prior = prior, cfg = cfg)

  rankings <- build_rankings(scores, shortlist = cfg$shortlist)

  out <- output_dir(cfg, "ranking")
  utils::write.csv(
    scores, file.path(out, paste0(cfg$dataset, "_important_score.csv")),
    row.names = FALSE
  )
  utils::write.csv(
    rankings, file.path(out, "feature_rankings.csv"), row.names = FALSE
  )

  message(
    "Scores and a shortlist of ", cfg$shortlist, " genes per method written to ", out
  )
  invisible(rankings)
}


main <- function() {
  args <- parse_args(list(
    dataset = NULL,
    config  = "configs/datasets.yml",
    prior   = "paper"
  ))
  require_args(args, "dataset")

  load_packages()
  cfg <- resolve_dataset(args$dataset, load_config(args$config))

  rank_features(cfg, args)
}


if (sys.nframe() == 0) main()
