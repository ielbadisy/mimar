#' Impute new rows with a fitted imputation
#'
#' `predict()` on a `mimar_imputation` imputes the missing cells of new data
#' (for example a test set) using the training imputation only. For each
#' completed training data set \eqn{k = 1,\ldots,m} and each variable
#' \eqn{X_j} missing in `newdata`, the imputer of \eqn{X_j} is fitted once on
#' the training rows where \eqn{X_j} was observed, with predictors taken from
#' the \eqn{k}-th completed training data:
#' \deqn{\hat f_j^{(k)}: X_{-j}^{(k),obs} \rightarrow X_j^{obs}.}
#' The missing cells of `newdata` are then filled by chained updates over
#' `maxit` iterations, using \eqn{\hat f_j^{(k)}} only. The training data and
#' the fitted models are never updated with the new rows, so no information
#' flows from the test set into the imputation models (as with the `ignore`
#' argument of `mice::mice()`).
#'
#' @param object A `mimar_imputation` from [impute()] on the training data.
#' @param newdata A data frame with the same columns as the training data.
#' @param maxit Number of chained iterations over the new rows. Default: the
#'   `maxit` of the training imputation.
#' @param seed Optional random seed.
#' @param ... Unused.
#' @return A `mimar_imputation` holding `m` completed copies of `newdata`,
#'   usable with [complete()], [with()][with.mimar_imputation()] and the pooling functions. The
#'   \eqn{k}-th copy is imputed from the \eqn{k}-th training imputation.
#' @examples
#' train <- airquality[1:120, ]
#' test <- airquality[121:153, ]
#' imp <- impute(train, m = 3, seed = 1, progress = FALSE)
#' imp_test <- predict(imp, test, seed = 2)
#' anyNA(complete(imp_test, 1))
#' @export
predict.mimar_imputation <- function(object, newdata, maxit = NULL, seed = NULL, ...) {
  .check_data_frame(newdata)
  newdata <- as.data.frame(newdata)
  train <- as.data.frame(object$data_original)
  missing_cols <- setdiff(names(train), names(newdata))
  if (length(missing_cols)) {
    .mimar_stop("`newdata` lacks training column(s): ", paste(missing_cols, collapse = ", "), ".")
  }
  newdata <- newdata[, names(train), drop = FALSE]
  if (identical(object$imputer, "missknn")) {
    .mimar_stop("predict() is not available for imputer = 'missknn'; use missknn directly.")
  }
  for (nm in names(train)) {
    if (is.factor(train[[nm]])) {
      extra <- setdiff(unique(as.character(newdata[[nm]][!is.na(newdata[[nm]])])), levels(train[[nm]]))
      if (length(extra)) {
        .mimar_stop("`newdata$", nm, "` has level(s) not seen in training: ", paste(extra, collapse = ", "), ".")
      }
      newdata[[nm]] <- factor(as.character(newdata[[nm]]), levels = levels(train[[nm]]),
                              ordered = is.ordered(train[[nm]]))
    }
  }
  maxit <- as.integer(maxit %||% object$maxit %||% 5L)
  if (!is.null(seed)) set.seed(seed)
  vars <- names(newdata)[colSums(is.na(newdata)) > 0]
  methods <- stats::setNames(vapply(vars, function(nm) {
    m_nm <- object$variable_methods[[nm]]
    if (is.null(m_nm) || identical(m_nm, "none")) {
      m_nm <- .chained_method_for(train[[nm]], method = object$imputer, variable = nm)
    }
    m_nm
  }, character(1)), vars)
  spec <- object$imputer_spec
  donors <- object$donors %||% 5L

  imputations <- lapply(object$imputations, function(tk) {
    tk <- as.data.frame(tk)
    fits <- lapply(vars, function(nm) {
      obs <- which(!is.na(train[[nm]]))
      if (!length(obs)) .mimar_stop("Variable `", nm, "` is never observed in the training data.")
      method_nm <- methods[[nm]]
      rows <- if (method_nm %in% c("norm", "pmm", "spmm")) obs else sample(obs, replace = TRUE)
      predictors <- setdiff(names(train), nm)
      fit(.clone_imputer_spec(spec, method_nm),
          x = tk[rows, predictors, drop = FALSE],
          y = train[[nm]][rows], target = train[[nm]],
          variable = nm, donors = donors)
    })
    names(fits) <- vars
    completed <- newdata
    for (nm in vars) {
      fill <- .simple_fill_value(tk[[nm]])
      completed[[nm]] <- .fill_missing_vector(completed[[nm]], fill)
    }
    for (iter in seq_len(max(1L, maxit))) {
      for (nm in vars) {
        mis <- is.na(newdata[[nm]])
        predictors <- setdiff(names(train), nm)
        pred <- stats::predict(fits[[nm]], completed[mis, predictors, drop = FALSE])
        completed[[nm]][mis] <- .restore_imputed_values(train[[nm]], pred, n = sum(mis))
      }
    }
    .as_dt(completed)
  })

  out <- list(imputations = imputations, data = imputations[[1]],
              call = match.call(), data_original = .as_dt(newdata),
              m = length(imputations), imputer = object$imputer,
              imputer_spec = spec, maxit = maxit, seed = seed,
              donors = as.integer(donors), ncore = 1L,
              variable_methods = c(stats::setNames(rep("none", ncol(newdata)), names(newdata))[
                setdiff(names(newdata), vars)], methods)[names(newdata)],
              imputed_cells = .imputed_cells_summary(newdata, imputations[[1]]),
              diagnostics = list(strategy = "predict_from_training",
                                 imputer = object$imputer,
                                 engine = "internal_fit_predict",
                                 trace = .rbind_or_empty(list())),
              stochastic = TRUE, training_rows = nrow(train))
  class(out) <- c("mimar_imputation", "list")
  out
}
