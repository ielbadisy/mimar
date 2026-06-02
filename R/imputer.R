.imputer_catalog <- function() {
  data.frame(
    imputer = c("mean", "median", "mode", "naive", "norm", "pmm", "spmm",
                "logreg", "polyreg", "rf", "ranger", "rpart", "nbayes",
                "svm", "bart", "glmnet", "gbm", "xgboost", "knn", "hotdeck",
                "famd", "superlearner", "sl"),
    implementation = c(rep("mimar", 9), rep("wrapped", 9), "mimar", "mimar", "wrapped",
                       "mimar", "mimar"),
    package = c(rep(NA_character_, 9), "ranger", "ranger", "rpart", "naivebayes",
                "e1071", "BART", "glmnet", "gbm", "xgboost", NA, NA, "missMDA",
                NA, NA),
    supports_numeric = rep(TRUE, 23),
    supports_binary = rep(TRUE, 23),
    supports_multiclass = rep(TRUE, 23),
    stochastic = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE,
                   TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
                   TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    description = c(
      "Mean imputation for numeric targets",
      "Median imputation for numeric targets",
      "Mode imputation for categorical targets",
      "Median/mode chained baseline",
      "Bayesian normal-style linear regression draw",
      "Predictive mean matching",
      "Single-step predictive mean matching",
      "Binary logistic regression draw",
      "One-vs-rest multinomial logistic regression draw",
      "MissForest-style random forest imputer",
      "Random forest imputer through ranger",
      "Tree imputer through rpart",
      "Naive Bayes imputer",
      "Support vector machine imputer",
      "BART imputer",
      "Penalized regression imputer through glmnet",
      "Gradient boosting imputer through gbm",
      "Gradient boosted tree imputer through xgboost",
      "Nearest-neighbor donor imputer",
      "Hot-deck donor imputer",
      "FAMD-assisted donor imputer",
      "Cross-validated Super Learner-style ensemble imputer",
      "Short alias for superlearner"
    ),
    stringsAsFactors = FALSE
  )
}

#' List available mimar imputers
#'
#' `imputer_registry()` returns the imputer names accepted by `impute()` and
#' metadata describing target-type support and backend packages. Native
#' `mimar` methods display `package = "internal"`. The result is returned as a
#' tibble.
#'
#' @return A tibble.
#' @export
imputer_registry <- function() {
  out <- .imputer_catalog()
  out$available <- vapply(out$package, function(pkg) {
    is.na(pkg) || requireNamespace(pkg, quietly = TRUE)
  }, logical(1))
  out$status <- ifelse(out$available, "available", paste0("requires ", out$package))
  out$package[is.na(out$package)] <- "internal"
  .as_tibble(out)
}

#' @describeIn imputer Construct a `mimar_imputer`.
#' @param spec Optional learner specification retained for future extensions.
#' @param ... Hyperparameters retained for later use by `impute()` or `fit()`.
#' @export
imputer.default <- function(method, spec = NULL, ...) {
  method <- as.character(method)[1]
  catalog <- .imputer_catalog()
  if (!method %in% catalog$imputer) .mimar_stop("Unknown imputer '", method, "'.")
  row <- catalog[catalog$imputer == method, , drop = FALSE]
  structure(list(method = method, implementation = row$implementation[[1]], package = row$package[[1]],
                 supports_numeric = row$supports_numeric[[1]],
                 supports_binary = row$supports_binary[[1]],
                 supports_multiclass = row$supports_multiclass[[1]],
                 stochastic = row$stochastic[[1]],
                 spec = spec, args = list(...)),
            class = c("mimar_imputer", "list"))
}

#' @export
imputer.mimar_imputer <- function(method, ...) {
  method
}

#' Fit an imputer learner
#'
#' `fit()` trains a `mimar_imputer` on complete observed rows for one target
#' variable. In the imputation loop, `x` is the predictor block
#' \eqn{X_{-j}^{obs}} and `y` is the observed target vector \eqn{X_j^{obs}}.
#'
#' The fitted object stores the original imputer descriptor and the model needed
#' by `predict()`. Native imputers are implemented directly in `mimar`; wrapped
#' imputers call their original learner packages directly:
#'
#' \itemize{
#'   \item `"norm"` fits a linear model and draws
#'     \eqn{\tilde y = \hat y + \epsilon}, with
#'     \eqn{\epsilon \sim N(0, \hat\sigma^2)}.
#'   \item `"pmm"` fits the same linear model but imputes by predictive mean
#'     matching: among observed donors with fitted values closest to
#'     \eqn{\hat y_{mis}}, one donor value is sampled.
#'   \item `"logreg"` fits a binomial GLM and draws classes from fitted
#'     Bernoulli probabilities.
#'   \item `"polyreg"` fits one-vs-rest binomial GLMs and samples classes from
#'     normalized class probabilities.
#' }
#'
#' @param object Object to fit.
#' @param ... Passed to methods.
#' @return A fitted object.
#' @export
fit <- function(object, ...) UseMethod("fit")

#' @describeIn fit Fit a `mimar_imputer`.
#' @param x Predictor data frame containing observed rows for the current target
#'   variable.
#' @param y Observed target vector for the current variable.
#' @param target Original target vector, used to validate type support and
#'   restore imputed values to the correct storage type.
#' @param variable Variable name used in diagnostics and error messages.
#' @param donors Number of predictive mean matching donors for `"pmm"`.
#' @param seed Optional random seed.
#' @export
fit.mimar_imputer <- function(object, x, y, target = y, variable = "target",
                              donors = 5, seed = NULL, ...) {
  .validate_imputer_support(object, target, variable)
  observed <- !is.na(y)
  y <- y[observed]
  x <- x[observed, , drop = FALSE]
  if (!length(y)) .mimar_stop("Variable '", variable, "' has no observed values.")
  fit_args <- object$args %||% list()
  extra_args <- list(...)
  if (length(extra_args)) {
    fit_args <- utils::modifyList(fit_args, extra_args)
  }
  tunable_methods <- c("rf", "ranger", "rpart", "nbayes", "svm", "bart",
                       "glmnet", "gbm", "xgboost", "famd", "superlearner", "sl")
  if (length(fit_args) && !(object$method %in% tunable_methods)) {
    .mimar_stop("Imputer '", object$method, "' does not accept additional hyperparameters via `...`. ",
                "Use `donors` for donor-based imputers or choose a learner-backed imputer.")
  }

  fitted <- switch(object$method,
    mean = .fit_constant_imputer(object, mean(.target_to_numeric(y), na.rm = TRUE), target),
    median = .fit_constant_imputer(object, stats::median(.target_to_numeric(y), na.rm = TRUE), target),
    mode = .fit_constant_imputer(object, .mode_value(y), target),
    naive = .fit_constant_imputer(object, .simple_fill_value(target), target),
    norm = .fit_linear_imputer(object, x, y, target, variable, donors),
    pmm = .fit_linear_imputer(object, x, y, target, variable, donors),
    spmm = .fit_linear_imputer(object, x, y, target, variable, donors),
    logreg = .fit_logreg_imputer(object, x, y, target, variable),
    polyreg = .fit_polyreg_imputer(object, x, y, target, variable),
    rf = do.call(.fit_ranger_imputer, c(list(object, x, y, target, variable), fit_args)),
    ranger = do.call(.fit_ranger_imputer, c(list(object, x, y, target, variable), fit_args)),
    rpart = do.call(.fit_rpart_imputer, c(list(object, x, y, target, variable), fit_args)),
    nbayes = do.call(.fit_naive_bayes_imputer, c(list(object, x, y, target, variable), fit_args)),
    svm = do.call(.fit_svm_imputer, c(list(object, x, y, target, variable), fit_args)),
    bart = do.call(.fit_bart_imputer, c(list(object, x, y, target, variable), fit_args)),
    glmnet = do.call(.fit_glmnet_imputer, c(list(object, x, y, target, variable), fit_args)),
    gbm = do.call(.fit_gbm_imputer, c(list(object, x, y, target, variable), fit_args)),
    xgboost = do.call(.fit_xgboost_imputer, c(list(object, x, y, target, variable), fit_args)),
    knn = .fit_donor_imputer(object, x, y, target, variable, donors, method = "knn"),
    hotdeck = .fit_donor_imputer(object, x, y, target, variable, donors, method = "hotdeck"),
    famd = do.call(.fit_famd_imputer, c(list(object, x, y, target, variable, donors), fit_args)),
    superlearner = do.call(.fit_superlearner_imputer, c(list(object, x, y, target, variable, donors), fit_args)),
    sl = do.call(.fit_superlearner_imputer, c(list(object, x, y, target, variable, donors), fit_args))
  )
  fitted
}

#' @export
predict.mimar_imputer_fit <- function(object, newdata, ...) {
  method <- object$imputer$method
  if (!is.null(object$value)) return(rep(object$value, nrow(newdata)))
  switch(method,
    mean = rep(object$value, nrow(newdata)),
    median = rep(object$value, nrow(newdata)),
    mode = rep(object$value, nrow(newdata)),
    naive = rep(object$value, nrow(newdata)),
    norm = .predict_linear_imputer(object, newdata, stochastic = TRUE),
    pmm = .predict_pmm_imputer(object, newdata),
    spmm = .predict_pmm_imputer(object, newdata),
    logreg = .predict_logreg_imputer(object, newdata),
    polyreg = .predict_polyreg_imputer(object, newdata),
    rf = .predict_ranger_imputer(object, newdata),
    ranger = .predict_ranger_imputer(object, newdata),
    rpart = .predict_generic_model_imputer(object, newdata),
    nbayes = .predict_generic_model_imputer(object, newdata),
    svm = .predict_generic_model_imputer(object, newdata),
    bart = .predict_bart_imputer(object, newdata),
    glmnet = .predict_glmnet_imputer(object, newdata),
    gbm = .predict_gbm_imputer(object, newdata),
    xgboost = .predict_xgboost_imputer(object, newdata),
    knn = .predict_donor_imputer(object, newdata),
    hotdeck = .predict_donor_imputer(object, newdata),
    famd = .predict_donor_imputer(object, newdata),
    superlearner = .predict_superlearner_imputer(object, newdata),
    sl = .predict_superlearner_imputer(object, newdata)
  )
}

.validate_imputer_support <- function(object, target, variable) {
  task <- .target_task(target)
  ok <- switch(task,
               numeric = object$supports_numeric,
               binary = object$supports_binary,
               multiclass = object$supports_multiclass)
  if (!isTRUE(ok)) {
    .mimar_stop("Imputer '", object$method, "' does not support ", task,
                " target variable '", variable, "'.")
  }
  invisible(TRUE)
}

.target_task <- function(x) {
  if (is.numeric(x) || is.integer(x) || inherits(x, "Date")) return("numeric")
  if (is.logical(x)) return("binary")
  nlev <- length(unique(x[!is.na(x)]))
  if (nlev <= 2) "binary" else "multiclass"
}

.fit_constant_imputer <- function(object, value, target) {
  structure(list(imputer = object, value = value, target = target),
            class = c("mimar_imputer_fit", "list"))
}

.fit_linear_imputer <- function(object, x, y, target, variable, donors) {
  dat <- data.frame(y = .target_to_numeric(y), .model_predictors(x), check.names = FALSE)
  fitted <- try(suppressWarnings(stats::lm(y ~ ., data = dat)), silent = TRUE)
  if (inherits(fitted, "try-error")) .mimar_stop("Could not fit imputer '", object$method, "' for variable '", variable, "'.")
  resid_sd <- stats::sd(stats::residuals(fitted), na.rm = TRUE)
  if (!is.finite(resid_sd)) resid_sd <- 0
  structure(list(imputer = object, fit = fitted, y = y, fitted = stats::fitted(fitted),
                 resid_sd = resid_sd, target = target, donors = donors),
            class = c("mimar_imputer_fit", "list"))
}

.fit_logreg_imputer <- function(object, x, y, target, variable) {
  y_fac <- factor(y)
  if (nlevels(y_fac) < 2) return(.fit_constant_imputer(object, .mode_value(target), target))
  if (nlevels(y_fac) != 2) .mimar_stop("Imputer 'logreg' requires binary target variable '", variable, "'.")
  dat <- data.frame(y = y_fac, .model_predictors(x), check.names = FALSE)
  fitted <- try(suppressWarnings(stats::glm(y ~ ., data = dat, family = stats::binomial())), silent = TRUE)
  if (inherits(fitted, "try-error")) return(.fit_constant_imputer(object, .mode_value(target), target))
  structure(list(imputer = object, fit = fitted, levels = levels(y_fac), y = y, target = target),
            class = c("mimar_imputer_fit", "list"))
}

.fit_polyreg_imputer <- function(object, x, y, target, variable) {
  y_fac <- factor(y)
  if (nlevels(y_fac) < 2) return(.fit_constant_imputer(object, .mode_value(target), target))
  if (nlevels(factor(target)) < 3) .mimar_stop("Imputer 'polyreg' requires a multiclass target variable '", variable, "'.")
  dat <- data.frame(y = y_fac, .model_predictors(x), check.names = FALSE)
  fits <- lapply(levels(y_fac), function(lvl) {
    dat_lvl <- dat
    dat_lvl$y <- as.integer(y_fac == lvl)
    try(suppressWarnings(stats::glm(y ~ ., data = dat_lvl, family = stats::binomial())), silent = TRUE)
  })
  if (any(vapply(fits, inherits, logical(1), "try-error"))) {
    return(.fit_constant_imputer(object, .mode_value(target), target))
  }
  structure(list(imputer = object, fits = fits, levels = levels(y_fac), y = y, target = target),
            class = c("mimar_imputer_fit", "list"))
}

.predict_linear_imputer <- function(object, newdata, stochastic) {
  pred <- suppressWarnings(stats::predict(object$fit, newdata = .model_predictors(newdata)))
  if (stochastic) pred <- pred + stats::rnorm(length(pred), sd = object$resid_sd)
  pred
}

.predict_pmm_imputer <- function(object, newdata) {
  pred_mis <- suppressWarnings(stats::predict(object$fit, newdata = .model_predictors(newdata)))
  sapply(pred_mis, function(pm) {
    d <- abs(object$fitted - pm)
    take <- order(d)[seq_len(min(object$donors, length(d)))]
    object$y[sample(take, 1)]
  })
}

.predict_logreg_imputer <- function(object, newdata) {
  prob <- suppressWarnings(stats::predict(object$fit, newdata = .model_predictors(newdata), type = "response"))
  fallback <- mean(as.character(object$y) == object$levels[2], na.rm = TRUE)
  if (!is.finite(fallback)) fallback <- 0.5
  prob[!is.finite(prob)] <- fallback
  prob <- pmin(pmax(prob, 0), 1)
  object$levels[1 + stats::rbinom(length(prob), 1, prob)]
}

.predict_polyreg_imputer <- function(object, newdata) {
  xnew <- .model_predictors(newdata)
  probs <- vapply(object$fits, function(mod) {
    suppressWarnings(stats::predict(mod, newdata = xnew, type = "response"))
  }, numeric(nrow(xnew)))
  probs[!is.finite(probs)] <- 0
  zero_rows <- rowSums(probs) <= .Machine$double.eps
  if (any(zero_rows)) probs[zero_rows, ] <- 1 / length(object$levels)
  probs <- probs / pmax(rowSums(probs), .Machine$double.eps)
  apply(probs, 1, function(p) object$levels[sample(seq_along(object$levels), 1, prob = p)])
}

.fit_ranger_imputer <- function(object, x, y, target, variable, num.trees = 100, ...) {
  .require_backend("ranger", object$method)
  dat <- data.frame(y = .model_target(y), .model_predictors(x), check.names = FALSE)
  task <- .target_task(target)
  fitted <- try(suppressWarnings(ranger::ranger(y ~ ., data = dat, num.trees = num.trees,
                                                probability = task != "numeric", ...)),
                silent = TRUE)
  if (inherits(fitted, "try-error")) return(.fit_constant_imputer(object, .simple_fill_value(target), target))
  structure(list(imputer = object, fit = fitted, target = target, task = task, y = y),
            class = c("mimar_imputer_fit", "list"))
}

.predict_ranger_imputer <- function(object, newdata) {
  pred <- stats::predict(object$fit, data = .model_predictors(newdata))$predictions
  if (object$task != "numeric" && is.matrix(pred)) {
    lev <- colnames(pred)
    return(apply(pred, 1, function(p) lev[sample(seq_along(lev), 1, prob = p)]))
  }
  pred
}

.fit_rpart_imputer <- function(object, x, y, target, variable, ...) {
  .require_backend("rpart", object$method)
  dat <- data.frame(y = .model_target(y), .model_predictors(x), check.names = FALSE)
  method <- if (.target_task(target) == "numeric") "anova" else "class"
  fitted <- try(rpart::rpart(y ~ ., data = dat, method = method, ...), silent = TRUE)
  if (inherits(fitted, "try-error")) return(.fit_constant_imputer(object, .simple_fill_value(target), target))
  structure(list(imputer = object, fit = fitted, target = target, task = .target_task(target)),
            class = c("mimar_imputer_fit", "list"))
}

.fit_naive_bayes_imputer <- function(object, x, y, target, variable, ...) {
  .require_backend("naivebayes", object$method)
  task <- .target_task(target)
  if (task == "numeric") {
    breaks <- unique(stats::quantile(as.numeric(y), probs = seq(0, 1, length.out = min(6, length(unique(y)) + 1)),
                                     na.rm = TRUE, names = FALSE))
    if (length(breaks) < 3) return(.fit_constant_imputer(object, .simple_fill_value(target), target))
    y_bin <- cut(as.numeric(y), breaks = breaks, include.lowest = TRUE, ordered_result = TRUE)
    dat <- data.frame(y = y_bin, .model_predictors(x), check.names = FALSE)
    fitted <- try(suppressWarnings(naivebayes::naive_bayes(y ~ ., data = dat, ...)), silent = TRUE)
    if (inherits(fitted, "try-error")) return(.fit_constant_imputer(object, .simple_fill_value(target), target))
    return(structure(list(imputer = object, fit = fitted, target = target, task = task,
                          y = y, y_bin = y_bin, levels = levels(y_bin)),
                     class = c("mimar_imputer_fit", "list")))
  }
  dat <- data.frame(y = factor(y), .model_predictors(x), check.names = FALSE)
  fitted <- try(suppressWarnings(naivebayes::naive_bayes(y ~ ., data = dat, ...)), silent = TRUE)
  if (inherits(fitted, "try-error")) return(.fit_constant_imputer(object, .mode_value(target), target))
  structure(list(imputer = object, fit = fitted, target = target, task = task),
            class = c("mimar_imputer_fit", "list"))
}

.fit_svm_imputer <- function(object, x, y, target, variable, ...) {
  .require_backend("e1071", object$method)
  dat <- data.frame(y = .model_target(y), .model_predictors(x), check.names = FALSE)
  fitted <- try(e1071::svm(y ~ ., data = dat, probability = .target_task(target) != "numeric", ...), silent = TRUE)
  if (inherits(fitted, "try-error")) return(.fit_constant_imputer(object, .simple_fill_value(target), target))
  structure(list(imputer = object, fit = fitted, target = target, task = .target_task(target)),
            class = c("mimar_imputer_fit", "list"))
}

.fit_bart_imputer <- function(object, x, y, target, variable, rm.const = FALSE, ...) {
  .require_backend("BART", object$method)
  task <- .target_task(target)
  x_mat <- .model_matrix(x)
  if (task == "numeric") {
    fitted <- try(BART::wbart(x.train = x_mat, y.train = as.numeric(y), verbose = FALSE,
                              rm.const = rm.const, ...), silent = TRUE)
    if (inherits(fitted, "try-error")) return(.fit_constant_imputer(object, .simple_fill_value(target), target))
    return(structure(list(imputer = object, fit = fitted, target = target, task = task,
                          x_cols = colnames(x_mat), x_levels = .predictor_levels(x)),
                     class = c("mimar_imputer_fit", "list")))
  }
  levels <- levels(factor(y))
  if (length(levels) < 2) return(.fit_constant_imputer(object, .mode_value(target), target))
  fits <- lapply(levels[-1], function(lvl) {
    y_bin <- as.integer(as.character(y) == lvl)
    fit <- try({
      utils::capture.output(
        out <- BART::pbart(x.train = x_mat, y.train = y_bin, printevery = 1000L,
                           rm.const = rm.const, ...)
      )
      out
    }, silent = TRUE)
    fit
  })
  if (any(vapply(fits, inherits, logical(1), what = "try-error"))) {
    return(.fit_constant_imputer(object, .mode_value(target), target))
  }
  structure(list(imputer = object, fit = fits, target = target, task = task, levels = levels,
                 x_cols = colnames(x_mat), x_levels = .predictor_levels(x)),
            class = c("mimar_imputer_fit", "list"))
}

.fit_glmnet_imputer <- function(object, x, y, target, variable, ...) {
  .require_backend("glmnet", object$method)
  task <- .target_task(target)
  fam <- if (task == "numeric") "gaussian" else if (task == "binary") "binomial" else "multinomial"
  x_mat <- .model_matrix(x)
  y_fit <- if (task == "numeric") as.numeric(y) else factor(y)
  fitted <- try(suppressWarnings(glmnet::cv.glmnet(x = x_mat, y = y_fit, family = fam, ...)), silent = TRUE)
  if (inherits(fitted, "try-error")) return(.fit_constant_imputer(object, .simple_fill_value(target), target))
  structure(list(imputer = object, fit = fitted, target = target, task = task, levels = if (task == "numeric") NULL else levels(factor(y)),
                 x_cols = colnames(x_mat), x_levels = .predictor_levels(x)),
            class = c("mimar_imputer_fit", "list"))
}

.fit_gbm_imputer <- function(object, x, y, target, variable, n.trees = 100, interaction.depth = 2,
                             n.minobsinnode = 1, shrinkage = 0.05, bag.fraction = 1, ...) {
  .require_backend("gbm", object$method)
  task <- .target_task(target)
  dat <- data.frame(y = if (task == "numeric") as.numeric(y) else factor(y), .model_predictors(x), check.names = FALSE)
  dist <- if (task == "numeric") "gaussian" else if (task == "binary") "bernoulli" else "multinomial"
  if (task == "binary") dat$y <- as.integer(dat$y == levels(dat$y)[2])
  fitted <- try(suppressWarnings(gbm::gbm(y ~ ., data = dat, distribution = dist, n.trees = n.trees,
                                          interaction.depth = interaction.depth,
                                          n.minobsinnode = n.minobsinnode,
                                          shrinkage = shrinkage, bag.fraction = bag.fraction,
                                          verbose = FALSE, ...)), silent = TRUE)
  if (inherits(fitted, "try-error")) return(.fit_constant_imputer(object, .simple_fill_value(target), target))
  structure(list(imputer = object, fit = fitted, target = target, task = task,
                 levels = if (task == "numeric") NULL else levels(factor(y)), n.trees = n.trees),
            class = c("mimar_imputer_fit", "list"))
}

.fit_xgboost_imputer <- function(object, x, y, target, variable, nrounds = 25, verbose = 0, ...) {
  .require_backend("xgboost", object$method)
  task <- .target_task(target)
  x_mat <- .model_matrix(x)
  if (task == "numeric") {
    label <- as.numeric(y); objective <- "reg:squarederror"; num_class <- NULL
  } else {
    yf <- factor(y); label <- as.integer(yf) - 1
    objective <- if (nlevels(yf) == 2) "binary:logistic" else "multi:softprob"
    num_class <- nlevels(yf)
  }
  params <- c(list(objective = objective), if (!is.null(num_class) && num_class > 2) list(num_class = num_class) else list())
  dtrain <- xgboost::xgb.DMatrix(data = x_mat, label = label)
  fitted <- try(suppressWarnings(xgboost::xgb.train(params = params, data = dtrain,
                                                    nrounds = nrounds, verbose = verbose,
                                                    ...)), silent = TRUE)
  if (inherits(fitted, "try-error")) return(.fit_constant_imputer(object, .simple_fill_value(target), target))
  structure(list(imputer = object, fit = fitted, target = target, task = task,
                 levels = if (task == "numeric") NULL else levels(factor(y)),
                 x_cols = colnames(x_mat), x_levels = .predictor_levels(x)),
            class = c("mimar_imputer_fit", "list"))
}

.fit_donor_imputer <- function(object, x, y, target, variable, donors, method = "knn") {
  structure(list(imputer = object, x_df = .model_predictors(x), y = y, target = target,
                 task = .target_task(target), donors = donors, method = method,
                 x_levels = .predictor_levels(x)),
            class = c("mimar_imputer_fit", "list"))
}

.fit_famd_imputer <- function(object, x, y, target, variable, donors, ncp = 5, ...) {
  .require_backend("missMDA", object$method)
  coords <- try(missMDA::imputeFAMD(.model_predictors(x), ncp = min(ncp, max(1, ncol(x))))$completeObs, silent = TRUE)
  if (inherits(coords, "try-error")) coords <- x
  fit <- .fit_donor_imputer(object, coords, y, target, variable, donors, method = "famd")
  fit
}

.fit_superlearner_imputer <- function(object, x, y, target, variable, donors,
                                      library = c("pmm", "knn", "rpart"),
                                      folds = 5,
                                      metalearner = c("inverse_loss", "best", "equal"),
                                      ...) {
  metalearner <- match.arg(metalearner)
  candidate_library <- unique(as.character(library))
  candidate_library <- setdiff(candidate_library, c("superlearner", "sl"))
  if (!length(candidate_library)) {
    .mimar_stop("`library` must contain at least one non-superlearner imputer.")
  }
  methods <- unique(vapply(candidate_library, .compatible_imputer_method, character(1), x = target))
  methods <- methods[methods != object$method]
  if (!length(methods)) return(.fit_constant_imputer(object, .simple_fill_value(target), target))

  n <- length(y)
  folds <- min(max(2L, as.integer(folds)), n)
  fold_id <- sample(rep(seq_len(folds), length.out = n))
  losses <- vapply(methods, function(method) {
    fold_losses <- vapply(seq_len(folds), function(fold) {
      train <- fold_id != fold
      valid <- fold_id == fold
      if (!any(train) || !any(valid)) return(NA_real_)
      learner <- imputer(method)
      fitted <- try(fit(learner, x = x[train, , drop = FALSE], y = y[train],
                        target = target, variable = variable, donors = donors),
                    silent = TRUE)
      if (inherits(fitted, "try-error")) return(NA_real_)
      pred <- try(stats::predict(fitted, x[valid, , drop = FALSE]), silent = TRUE)
      if (inherits(pred, "try-error")) return(NA_real_)
      .superlearner_loss(y[valid], pred, target)
    }, numeric(1))
    mean(fold_losses, na.rm = TRUE)
  }, numeric(1))

  fits <- lapply(methods, function(method) {
    try(fit(imputer(method), x = x, y = y, target = target, variable = variable,
            donors = donors), silent = TRUE)
  })
  ok <- !vapply(fits, inherits, logical(1), what = "try-error")
  methods <- methods[ok]
  losses <- losses[ok]
  fits <- fits[ok]
  if (!length(fits)) return(.fit_constant_imputer(object, .simple_fill_value(target), target))

  weights <- .superlearner_weights(losses, metalearner = metalearner)
  structure(list(imputer = object, fits = fits, methods = methods, weights = weights,
                 losses = losses, target = target, task = .target_task(target),
                 metalearner = metalearner, library = candidate_library),
            class = c("mimar_imputer_fit", "list"))
}

.superlearner_loss <- function(truth, pred, target) {
  keep <- !is.na(truth) & !is.na(pred)
  truth <- truth[keep]
  pred <- pred[keep]
  if (!length(truth)) return(NA_real_)
  if (is.numeric(target) || is.integer(target) || inherits(target, "Date")) {
    pred <- suppressWarnings(as.numeric(pred))
    truth <- as.numeric(truth)
    keep <- is.finite(truth) & is.finite(pred)
    if (!any(keep)) return(NA_real_)
    return(mean((pred[keep] - truth[keep])^2, na.rm = TRUE))
  }
  mean(as.character(pred) != as.character(truth), na.rm = TRUE)
}

.superlearner_weights <- function(losses, metalearner) {
  losses[!is.finite(losses)] <- Inf
  if (identical(metalearner, "equal") || !any(is.finite(losses))) {
    return(rep(1 / length(losses), length(losses)))
  }
  if (identical(metalearner, "best")) {
    out <- rep(0, length(losses))
    out[which.min(losses)] <- 1
    return(out)
  }
  inv <- 1 / pmax(losses, .Machine$double.eps)
  inv[!is.finite(inv)] <- 0
  if (!sum(inv)) rep(1 / length(inv), length(inv)) else inv / sum(inv)
}

.predict_superlearner_imputer <- function(object, newdata) {
  preds <- lapply(object$fits, function(fit) {
    try(stats::predict(fit, newdata), silent = TRUE)
  })
  ok <- !vapply(preds, inherits, logical(1), what = "try-error")
  preds <- preds[ok]
  weights <- object$weights[ok]
  if (!length(preds) || !sum(weights)) {
    return(rep(.simple_fill_value(object$target), nrow(newdata)))
  }
  weights <- weights / sum(weights)
  if (object$task == "numeric") {
    mat <- do.call(cbind, lapply(preds, function(p) suppressWarnings(as.numeric(p))))
    out <- vapply(seq_len(nrow(mat)), function(i) {
      vals <- mat[i, ]
      keep <- is.finite(vals) & weights > 0
      if (!any(keep)) return(as.numeric(.simple_fill_value(object$target)))
      sum(vals[keep] * weights[keep]) / sum(weights[keep])
    }, numeric(1))
    return(out)
  }
  lev <- if (is.factor(object$target)) levels(object$target) else unique(as.character(object$target[!is.na(object$target)]))
  if (!length(lev)) lev <- as.character(.simple_fill_value(object$target))
  mat <- do.call(cbind, lapply(preds, as.character))
  vapply(seq_len(nrow(mat)), function(i) {
    scores <- stats::setNames(rep(0, length(lev)), lev)
    for (j in seq_along(weights)) {
      val <- mat[i, j]
      if (!is.na(val) && val %in% names(scores)) scores[[val]] <- scores[[val]] + weights[[j]]
    }
    if (!sum(scores)) return(as.character(.simple_fill_value(object$target)))
    names(scores)[which.max(scores)]
  }, character(1))
}

.predict_generic_model_imputer <- function(object, newdata) {
  if (object$imputer$method == "nbayes" && object$task == "numeric") {
    pred_bin <- try(stats::predict(object$fit, newdata = .model_predictors(newdata)), silent = TRUE)
    if (inherits(pred_bin, "try-error")) return(rep(.simple_fill_value(object$target), nrow(newdata)))
    pred_bin <- as.character(pred_bin)
    return(vapply(pred_bin, function(bin) {
      donors <- object$y[as.character(object$y_bin) == bin]
      donors <- donors[!is.na(donors)]
      if (!length(donors)) return(.simple_fill_value(object$target))
      sample(donors, 1)
    }, numeric(1)))
  } else if (object$imputer$method == "nbayes") {
    pred <- try(stats::predict(object$fit, newdata = .model_predictors(newdata)), silent = TRUE)
    if (inherits(pred, "try-error")) return(rep(.simple_fill_value(object$target), nrow(newdata)))
    return(as.character(pred))
  }
  if (object$imputer$method == "rpart" && object$task != "numeric") {
    pred <- try(stats::predict(object$fit, newdata = .model_predictors(newdata), type = "class"), silent = TRUE)
    if (inherits(pred, "try-error")) return(rep(.simple_fill_value(object$target), nrow(newdata)))
    return(as.character(pred))
  }
  pred <- try(suppressWarnings(stats::predict(object$fit, newdata = .model_predictors(newdata))), silent = TRUE)
  if (inherits(pred, "try-error")) return(rep(.simple_fill_value(object$target), nrow(newdata)))
  .as_prediction_vector(pred)
}

.predict_bart_imputer <- function(object, newdata) {
  x_mat <- .model_matrix_align(newdata, object$x_cols)
  if (object$task == "numeric") {
    pred <- stats::predict(object$fit, newdata = x_mat)$yhat.test.mean
    return(as.numeric(pred))
  }
  probs_pos <- sapply(object$fit, function(fit) {
    pred <- try(utils::capture.output(out <- stats::predict(fit, x_mat)), silent = TRUE)
    if (inherits(pred, "try-error")) return(rep(NA_real_, nrow(newdata)))
    out$prob.test.mean
  })
  if (is.null(dim(probs_pos))) probs_pos <- matrix(probs_pos, ncol = 1)
  probs <- cbind(pmax(0, 1 - rowSums(probs_pos)), probs_pos)
  probs <- t(apply(probs, 1, function(p) {
    p <- pmax(p, 0)
    if (!sum(p)) rep(1 / length(p), length(p)) else p / sum(p)
  }))
  apply(probs, 1, function(p) object$levels[sample(seq_along(object$levels), 1, prob = p)])
}

.predict_glmnet_imputer <- function(object, newdata) {
  x_mat <- .model_matrix_align(newdata, object$x_cols)
  if (object$task == "numeric") return(as.numeric(stats::predict(object$fit, newx = x_mat, s = "lambda.min")))
  if (object$task == "binary") {
    p <- as.numeric(stats::predict(object$fit, newx = x_mat, s = "lambda.min", type = "response"))
    return(object$levels[1 + stats::rbinom(length(p), 1, pmin(pmax(p, 0), 1))])
  }
  cl <- stats::predict(object$fit, newx = x_mat, s = "lambda.min", type = "class")
  as.character(cl)
}

.predict_gbm_imputer <- function(object, newdata) {
  pred <- stats::predict(object$fit, newdata = .model_predictors(newdata), n.trees = object$n.trees, type = "response")
  if (object$task == "numeric") return(as.numeric(pred))
  if (object$task == "binary") return(object$levels[1 + stats::rbinom(length(pred), 1, pmin(pmax(pred, 0), 1))])
  if (length(dim(pred)) == 3) pred <- pred[, , 1, drop = FALSE][, , 1]
  apply(pred, 1, function(p) object$levels[sample(seq_along(object$levels), 1, prob = p / sum(p))])
}

.predict_xgboost_imputer <- function(object, newdata) {
  dnew <- xgboost::xgb.DMatrix(data = .model_matrix_align(newdata, object$x_cols))
  pred <- try(stats::predict(object$fit, newdata = dnew), silent = TRUE)
  if (inherits(pred, "try-error")) return(rep(.simple_fill_value(object$target), nrow(newdata)))
  if (object$task == "numeric") return(as.numeric(pred))
  if (object$task == "binary") return(object$levels[1 + stats::rbinom(length(pred), 1, pmin(pmax(pred, 0), 1))])
  if (length(pred) != nrow(newdata) * length(object$levels)) {
    return(rep(.simple_fill_value(object$target), nrow(newdata)))
  }
  probs <- matrix(pred, ncol = length(object$levels), byrow = TRUE)
  apply(probs, 1, function(p) object$levels[sample(seq_along(object$levels), 1, prob = p / sum(p))])
}

.predict_donor_imputer <- function(object, newdata) {
  mats <- .paired_model_matrices(object$x_df, .model_predictors(newdata))
  xobs <- mats$observed
  xnew <- mats$new
  if (!nrow(xnew)) return(object$y[0])
  d <- as.matrix(stats::dist(rbind(xobs, xnew)))
  d <- d[seq_len(nrow(xobs)), nrow(xobs) + seq_len(nrow(xnew)), drop = FALSE]
  vapply(seq_len(ncol(d)), function(i) {
    take <- order(d[, i])[seq_len(min(object$donors, nrow(xobs)))]
    as.character(object$y[sample(take, 1)])
  }, character(1))
}

.model_predictors <- function(x) {
  out <- as.data.frame(x, stringsAsFactors = FALSE)
  for (nm in names(out)) {
    if (is.character(out[[nm]]) || is.logical(out[[nm]])) out[[nm]] <- factor(out[[nm]])
    if (inherits(out[[nm]], "Date")) out[[nm]] <- as.numeric(out[[nm]])
  }
  out
}

.model_matrix <- function(x) {
  x <- .model_predictors(x)
  if (!ncol(x)) return(matrix(0, nrow(x), 1))
  keep <- vapply(x, function(v) !(is.factor(v) && nlevels(droplevels(v)) < 2), logical(1))
  x <- x[keep]
  if (!ncol(x)) return(matrix(0, nrow(x), 1, dimnames = list(NULL, "intercept")))
  stats::model.matrix(~ . - 1, data = x)
}

.model_matrix_align <- function(x, cols) {
  mat <- try(.model_matrix(x), silent = TRUE)
  if (inherits(mat, "try-error")) mat <- matrix(0, nrow(x), 0)
  out <- matrix(0, nrow(x), length(cols), dimnames = list(NULL, cols))
  common <- intersect(colnames(mat), cols)
  if (length(common)) out[, common] <- mat[, common, drop = FALSE]
  out
}

.paired_model_matrices <- function(observed, newdata) {
  combined <- rbind(observed, newdata)
  mat <- .model_matrix(combined)
  list(
    observed = mat[seq_len(nrow(observed)), , drop = FALSE],
    new = mat[nrow(observed) + seq_len(nrow(newdata)), , drop = FALSE]
  )
}

.predictor_levels <- function(x) {
  lapply(.model_predictors(x), function(v) if (is.factor(v)) levels(v) else NULL)
}

.model_target <- function(y) {
  if (is.character(y) || is.logical(y)) return(factor(y))
  y
}

.target_to_numeric <- function(x) {
  if (inherits(x, "Date")) return(as.numeric(x))
  as.numeric(x)
}

.restore_imputed_values <- function(target, pred, n) {
  pred <- pred[seq_len(n)]
  if (inherits(target, "Date")) return(as.Date(round(as.numeric(pred)), origin = "1970-01-01"))
  if (is.numeric(target)) return(as.numeric(pred))
  if (is.integer(target)) return(as.integer(round(as.numeric(pred))))
  if (is.factor(target)) return(factor(as.character(pred), levels = levels(target), ordered = is.ordered(target)))
  if (is.logical(target)) return(as.logical(as.character(pred)))
  if (is.character(target)) return(as.character(pred))
  pred
}

.require_backend <- function(package, imputer) {
  if (!requireNamespace(package, quietly = TRUE)) {
    .mimar_stop("Package '", package, "' is required for imputer = '", imputer, "'. Install it or choose another imputer.")
  }
  invisible(TRUE)
}
