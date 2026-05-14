.imputer_catalog <- function() {
  data.frame(
    imputer = c("mean", "median", "mode", "norm", "pmm", "logreg", "polyreg",
                "randomForest", "knn", "xgboost", "svm", "bart", "naive_bayes",
                "rpart", "glmnet"),
    learner = c("mean", "median", "mode", "norm", "pmm", "logreg", "polyreg",
                "randomForest", "kknn", "xgboost", "e1071_svm", "bart", "naivebayes",
                "rpart", "glmnet"),
    source = c(rep("mimar", 7), rep("funcml", 8)),
    supports_numeric = c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE,
                         TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE),
    supports_binary = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE,
                        TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE),
    supports_multiclass = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE,
                            TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
}

#' @describeIn imputer Construct a `mimar_imputer`.
#' @param spec Optional learner specification passed to `funcml::fit()` for
#'   `funcml`-sourced imputers.
#' @export
imputer.default <- function(method, spec = NULL, ...) {
  method <- as.character(method)[1]
  catalog <- .imputer_catalog()
  if (!method %in% catalog$imputer) .mimar_stop("Unknown imputer '", method, "'.")
  row <- catalog[catalog$imputer == method, , drop = FALSE]
  structure(list(method = method, learner = row$learner[[1]], source = row$source[[1]],
                 supports_numeric = row$supports_numeric[[1]],
                 supports_binary = row$supports_binary[[1]],
                 supports_multiclass = row$supports_multiclass[[1]],
                 spec = spec, args = list(...)),
            class = c("mimar_imputer", "list"))
}

#' Fit an imputer learner
#'
#' `fit()` trains a `mimar_imputer` on complete observed rows for one target
#' variable. In the imputation loop, `x` is the predictor block
#' \eqn{X_{-j}^{obs}} and `y` is the observed target vector \eqn{X_j^{obs}}.
#'
#' The fitted object stores the original imputer descriptor and the model needed
#' by `predict()`. For `funcml`-sourced imputers, `fit()` calls
#' `funcml::fit(y ~ ., data = ..., model = learner)`. For local classical
#' imputers, `mimar` fits the corresponding model directly:
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
#' @param seed Optional random seed passed to `funcml::fit()`.
#' @export
fit.mimar_imputer <- function(object, x, y, target = y, variable = "target",
                              donors = 5, seed = NULL, ...) {
  .validate_imputer_support(object, target, variable)
  observed <- !is.na(y)
  y <- y[observed]
  x <- x[observed, , drop = FALSE]
  if (!length(y)) .mimar_stop("Variable '", variable, "' has no observed values.")

  if (identical(object$source, "funcml")) {
    dat <- data.frame(y = .model_target(y), .model_predictors(x), check.names = FALSE)
    args <- c(list(formula = stats::as.formula("y ~ ."), data = dat,
                   model = object$learner, spec = object$spec, seed = seed),
              object$args, list(...))
    fitted <- try(do.call(funcml::fit, args), silent = TRUE)
    if (inherits(fitted, "try-error")) {
      .mimar_stop("Could not fit imputer '", object$method, "' for variable '", variable, "'.")
    }
    return(structure(list(imputer = object, fit = fitted, target = target),
                     class = c("mimar_imputer_fit", "list")))
  }

  fitted <- switch(object$method,
    mean = .fit_constant_imputer(object, mean(.target_to_numeric(y), na.rm = TRUE), target),
    median = .fit_constant_imputer(object, stats::median(.target_to_numeric(y), na.rm = TRUE), target),
    mode = .fit_constant_imputer(object, .mode_value(y), target),
    norm = .fit_linear_imputer(object, x, y, target, variable, donors),
    pmm = .fit_linear_imputer(object, x, y, target, variable, donors),
    logreg = .fit_logreg_imputer(object, x, y, target, variable),
    polyreg = .fit_polyreg_imputer(object, x, y, target, variable)
  )
  fitted
}

#' @export
predict.mimar_imputer_fit <- function(object, newdata, ...) {
  method <- object$imputer$method
  if (identical(object$imputer$source, "funcml")) {
    pred <- try(stats::predict(object$fit, newdata = .model_predictors(newdata), ...), silent = TRUE)
    if (inherits(pred, "try-error")) .mimar_stop("Could not predict with imputer '", method, "'.")
    return(.as_prediction_vector(pred))
  }

  switch(method,
    mean = rep(object$value, nrow(newdata)),
    median = rep(object$value, nrow(newdata)),
    mode = rep(object$value, nrow(newdata)),
    norm = .predict_linear_imputer(object, newdata, stochastic = TRUE),
    pmm = .predict_pmm_imputer(object, newdata),
    logreg = .predict_logreg_imputer(object, newdata),
    polyreg = .predict_polyreg_imputer(object, newdata)
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
  if (nlevels(y_fac) != 2) .mimar_stop("Imputer 'logreg' requires binary target variable '", variable, "'.")
  dat <- data.frame(y = y_fac, .model_predictors(x), check.names = FALSE)
  fitted <- try(suppressWarnings(stats::glm(y ~ ., data = dat, family = stats::binomial())), silent = TRUE)
  if (inherits(fitted, "try-error")) .mimar_stop("Could not fit imputer 'logreg' for variable '", variable, "'.")
  structure(list(imputer = object, fit = fitted, levels = levels(y_fac), y = y, target = target),
            class = c("mimar_imputer_fit", "list"))
}

.fit_polyreg_imputer <- function(object, x, y, target, variable) {
  y_fac <- factor(y)
  if (nlevels(y_fac) < 3) .mimar_stop("Imputer 'polyreg' requires a multiclass target variable '", variable, "'.")
  dat <- data.frame(y = y_fac, .model_predictors(x), check.names = FALSE)
  fits <- lapply(levels(y_fac), function(lvl) {
    dat_lvl <- dat
    dat_lvl$y <- as.integer(y_fac == lvl)
    try(suppressWarnings(stats::glm(y ~ ., data = dat_lvl, family = stats::binomial())), silent = TRUE)
  })
  if (any(vapply(fits, inherits, logical(1), "try-error"))) {
    .mimar_stop("Could not fit imputer 'polyreg' for variable '", variable, "'.")
  }
  structure(list(imputer = object, fits = fits, levels = levels(y_fac), y = y, target = target),
            class = c("mimar_imputer_fit", "list"))
}

.predict_linear_imputer <- function(object, newdata, stochastic) {
  pred <- stats::predict(object$fit, newdata = .model_predictors(newdata))
  if (stochastic) pred <- pred + stats::rnorm(length(pred), sd = object$resid_sd)
  pred
}

.predict_pmm_imputer <- function(object, newdata) {
  pred_mis <- stats::predict(object$fit, newdata = .model_predictors(newdata))
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

.model_predictors <- function(x) {
  out <- as.data.frame(x, stringsAsFactors = FALSE)
  for (nm in names(out)) {
    if (is.character(out[[nm]]) || is.logical(out[[nm]])) out[[nm]] <- factor(out[[nm]])
    if (inherits(out[[nm]], "Date")) out[[nm]] <- as.numeric(out[[nm]])
  }
  out
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
  if (is.integer(target)) return(as.integer(round(as.numeric(pred))))
  if (is.factor(target)) return(factor(as.character(pred), levels = levels(target), ordered = is.ordered(target)))
  if (is.logical(target)) return(as.logical(as.character(pred)))
  if (is.character(target)) return(as.character(pred))
  pred
}
