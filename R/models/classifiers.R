# The four classifiers the feature rankings are evaluated with.
#
# cv_over_subsets() owns the outer fold loop, the descent through the subset
# sizes, the per-subset seed and the results layout. Each classifier contributes
# a fit_predict() that takes a training frame, an evaluation frame, a feature
# subset and an inner fold count, and returns one micro-averaged F1 score.
#
# Every classifier selects its hyperparameters by cross-validation inside the
# outer training partition, then refits on the whole of that partition before
# scoring the held-out split. Nothing from the held-out split enters the search.
# k-nearest neighbours and the support vector machine standardise their
# predictors, with the centre and scale taken from the training partition alone.


ELASTIC_NET_ALPHA <- c(0, 0.5, 1)
ELASTIC_NET_NLAMBDA <- 50
KNN_K <- c(1, 3, 5, 7, 9)
RANDOM_FOREST_TREES <- 500
RANDOM_FOREST_NODESIZE <- 1
SVM_COST <- c(0.1, 1, 10, 100)
SVM_GAMMA <- c(0.1, 1, 10)


#' Cross-validate one classifier over every ranking and every subset size.
#'
#' @param eval_set The frame the folds are cut from.
#' @param folds List of held-out row index vectors, from caret::createFolds().
#' @param rankings Character matrix from build_rankings().
#' @param stepsize Decreasing fractions of the ranked shortlist to retain.
#' @param seed Seed set before every single fit.
#' @param inner_folds Folds used for the hyperparameter search.
#' @param fit_predict Classifier function, as described above.
#' @return A results table averaged over the folds.
cv_over_subsets <- function(eval_set, folds, rankings, stepsize, seed,
                            inner_folds, fit_predict) {
  methods <- colnames(rankings)
  shortlist <- nrow(rankings)

  fold_results <- lapply(folds, function(held_out_rows) {
    train_data <- eval_set[-held_out_rows, ]
    test_data <- eval_set[held_out_rows, ]
    results <- init_cv_results(stepsize, methods)

    for (i in seq_along(methods)) {
      for (s in seq_along(stepsize)) {
        set_seed(seed)

        threshold <- floor(stepsize[s] * shortlist)
        features <- as.character(rankings[seq_len(threshold), i])

        results[s, i] <- fit_predict(
          train_data, test_data, features, inner_folds
        )
      }
      message("    ", methods[i])
    }

    results
  })

  average_folds(fold_results)
}


#' Held-out row indices for the inner search, stratified on the class.
#'
#' The count is reduced when the rarest class has fewer members than the
#' requested number of folds, so that every fold can hold one of them.
inner_fold_indices <- function(y, folds) {
  y <- factor(y)
  usable <- max(2, min(folds, min(table(y))))
  caret::createFolds(y, k = usable)
}


#' Centre and scale both matrices by the training matrix's own moments.
#'
#' A predictor that is constant across the training rows is left untouched
#' rather than divided by zero.
standardise_to_train <- function(train_x, test_x) {
  centre <- colMeans(train_x)
  spread <- apply(train_x, 2, stats::sd)
  spread[!is.finite(spread) | spread == 0] <- 1

  list(
    train = scale(train_x, center = centre, scale = spread),
    test = scale(test_x, center = centre, scale = spread)
  )
}


#' Micro F1 of one prediction vector against the matching truth.
score_prediction <- function(predicted, truth) {
  aligned <- align_levels(predicted, truth)
  micro_f1(aligned$predicted, aligned$truth)
}


#' Mean inner-fold score of every candidate, as a numeric vector.
#'
#' `score_fold` is called once per fold and returns one score per candidate,
#' in the order the candidates were given. A fold may return fewer scores than
#' there are candidates, which is what a penalty path that stops short looks
#' like; the remaining candidates go unscored for that fold. A fold that cannot
#' be fitted contributes nothing, and a candidate no fold could score is NA.
average_candidate_scores <- function(held_out, n_candidates, score_fold) {
  scores <- matrix(NA_real_, nrow = length(held_out), ncol = n_candidates)

  for (f in seq_along(held_out)) {
    fold_scores <- tryCatch(
      score_fold(held_out[[f]]),
      error = function(e) numeric(0)
    )
    scored <- seq_len(min(length(fold_scores), n_candidates))
    scores[f, scored] <- fold_scores[scored]
  }

  colMeans(scores, na.rm = TRUE)
}


#' Index of the highest mean score, or `default` if none could be computed.
best_candidate <- function(mean_scores, default = 1L) {
  if (all(is.na(mean_scores))) return(default)
  which.max(replace(mean_scores, is.na(mean_scores), -Inf))
}


#' The feature subset as a numeric matrix, and the class as a factor.
model_matrices <- function(train_data, test_data, features) {
  list(
    x_train = as.matrix(train_data[, features, drop = FALSE]),
    x_test = as.matrix(test_data[, features, drop = FALSE]),
    y_train = factor(train_data$y)
  )
}


#' Fit a penalised multinomial model over one penalty path.
#'
#' glmnet reports the penalty values it could not solve within its iteration
#' limit and returns the solutions it did reach, which on wide data is routine.
#' Callers read the columns they are given, so that report is muffled here.
#'
#' @param lambda An explicit penalty path, or NULL to let glmnet choose one.
fit_elastic_net <- function(x, y, alpha, lambda = NULL) {
  call_glmnet <- function() {
    if (is.null(lambda)) {
      glmnet::glmnet(
        x, y,
        family = "multinomial", alpha = alpha, nlambda = ELASTIC_NET_NLAMBDA
      )
    } else {
      glmnet::glmnet(
        x, y,
        family = "multinomial", alpha = alpha, lambda = lambda
      )
    }
  }

  withCallingHandlers(
    call_glmnet(),
    warning = function(w) {
      if (grepl("glmnet C\\+\\+ code", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}


#' Mixing parameter and penalty strength with the best inner-fold score.
#'
#' One penalty path is taken per mixing parameter from a fit on the whole
#' training partition, and the folds are then scored along that same path so
#' that their scores are comparable.
tune_elastic_net <- function(x, y, folds) {
  held_out <- inner_fold_indices(y, folds)
  best <- list(alpha = ELASTIC_NET_ALPHA[1], lambda = NULL, score = -Inf)

  for (alpha in ELASTIC_NET_ALPHA) {
    path <- tryCatch(
      fit_elastic_net(x, y, alpha)$lambda,
      error = function(e) NULL
    )
    if (is.null(path) || length(path) == 0) next

    mean_scores <- average_candidate_scores(held_out, length(path), function(rows) {
      fit <- fit_elastic_net(
        x[-rows, , drop = FALSE], y[-rows], alpha, lambda = path
      )
      predicted <- stats::predict(
        fit, newx = x[rows, , drop = FALSE], type = "class"
      )
      vapply(
        seq_len(ncol(predicted)),
        function(j) score_prediction(predicted[, j], y[rows]),
        numeric(1)
      )
    })

    k <- best_candidate(mean_scores)
    if (!is.na(mean_scores[k]) && mean_scores[k] > best$score) {
      best <- list(alpha = alpha, lambda = path[k], score = mean_scores[k])
    }
  }

  if (is.null(best$lambda)) best$lambda <- 0
  best
}


#' Elastic-net multinomial logistic regression.
fit_predict_lr <- function(train_data, test_data, features, inner_folds = 5) {
  m <- model_matrices(train_data, test_data, features)

  tuned <- tune_elastic_net(m$x_train, m$y_train, inner_folds)
  model <- fit_elastic_net(
    m$x_train, m$y_train, tuned$alpha, lambda = tuned$lambda
  )

  predicted <- stats::predict(model, newx = m$x_test, type = "class")
  score_prediction(as.vector(predicted), test_data$y)
}


#' Number of neighbours with the best inner-fold score.
tune_knn <- function(x, y, folds) {
  held_out <- inner_fold_indices(y, folds)
  smallest_train <- nrow(x) - max(lengths(held_out))
  grid <- KNN_K[KNN_K <= max(1, smallest_train)]
  if (length(grid) == 0) grid <- 1

  mean_scores <- average_candidate_scores(held_out, length(grid), function(rows) {
    scaled <- standardise_to_train(
      x[-rows, , drop = FALSE], x[rows, , drop = FALSE]
    )
    vapply(
      grid,
      function(k) {
        predicted <- class::knn(scaled$train, scaled$test, y[-rows], k = k)
        score_prediction(predicted, y[rows])
      },
      numeric(1)
    )
  })

  grid[best_candidate(mean_scores)]
}


#' k-nearest neighbours on standardised predictors.
fit_predict_knn <- function(train_data, test_data, features, inner_folds = 5) {
  m <- model_matrices(train_data, test_data, features)

  k <- tune_knn(m$x_train, m$y_train, inner_folds)
  scaled <- standardise_to_train(m$x_train, m$x_test)
  predicted <- class::knn(scaled$train, scaled$test, m$y_train, k = k)

  score_prediction(predicted, test_data$y)
}


#' Candidate split widths for a given number of predictors.
random_forest_mtry <- function(p) {
  grid <- c(sqrt(p), p / 10, p / 3)
  grid <- pmax(1, pmin(p, round(grid)))
  sort(unique(grid))
}


#' Split width with the best inner-fold score.
tune_random_forest <- function(x, y, folds) {
  held_out <- inner_fold_indices(y, folds)
  grid <- random_forest_mtry(ncol(x))

  mean_scores <- average_candidate_scores(held_out, length(grid), function(rows) {
    vapply(
      grid,
      function(mtry) {
        model <- randomForest::randomForest(
          x = x[-rows, , drop = FALSE], y = y[-rows],
          ntree = RANDOM_FOREST_TREES, mtry = mtry,
          nodesize = RANDOM_FOREST_NODESIZE
        )
        predicted <- stats::predict(model, x[rows, , drop = FALSE])
        score_prediction(predicted, y[rows])
      },
      numeric(1)
    )
  })

  grid[best_candidate(mean_scores)]
}


#' Random forest of 500 trees.
fit_predict_rf <- function(train_data, test_data, features, inner_folds = 5) {
  m <- model_matrices(train_data, test_data, features)

  mtry <- tune_random_forest(m$x_train, m$y_train, inner_folds)
  model <- randomForest::randomForest(
    x = m$x_train, y = m$y_train,
    ntree = RANDOM_FOREST_TREES, mtry = mtry,
    nodesize = RANDOM_FOREST_NODESIZE
  )

  predicted <- stats::predict(model, m$x_test)
  score_prediction(predicted, test_data$y)
}


#' Cost and kernel width pairs, including the reciprocal of the subset size.
svm_grid <- function(p) {
  gamma <- sort(unique(c(1 / max(1, p), SVM_GAMMA)))
  expand.grid(cost = SVM_COST, gamma = gamma)
}


#' Cost and kernel width with the best inner-fold score.
tune_svm <- function(x, y, folds) {
  held_out <- inner_fold_indices(y, folds)
  grid <- svm_grid(ncol(x))

  mean_scores <- average_candidate_scores(held_out, nrow(grid), function(rows) {
    scaled <- standardise_to_train(
      x[-rows, , drop = FALSE], x[rows, , drop = FALSE]
    )
    vapply(
      seq_len(nrow(grid)),
      function(g) {
        model <- e1071::svm(
          scaled$train, y[-rows],
          kernel = "radial", cost = grid$cost[g], gamma = grid$gamma[g]
        )
        predicted <- stats::predict(model, scaled$test)
        score_prediction(predicted, y[rows])
      },
      numeric(1)
    )
  })

  grid[best_candidate(mean_scores), ]
}


#' Support vector machine with a radial kernel, on standardised predictors.
fit_predict_svm <- function(train_data, test_data, features, inner_folds = 5) {
  m <- model_matrices(train_data, test_data, features)

  tuned <- tune_svm(m$x_train, m$y_train, inner_folds)
  scaled <- standardise_to_train(m$x_train, m$x_test)
  model <- e1071::svm(
    scaled$train, m$y_train,
    kernel = "radial", cost = tuned$cost, gamma = tuned$gamma
  )

  predicted <- stats::predict(model, scaled$test)
  score_prediction(predicted, test_data$y)
}


#' The four classifiers, keyed by the sheet name used in the exported workbook.
CLASSIFIERS <- list(
  "logistic regression" = fit_predict_lr,
  "KNN"                 = fit_predict_knn,
  "random forest"       = fit_predict_rf,
  "SVM"                 = fit_predict_svm
)
