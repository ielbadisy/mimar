.impute_chained <- function(x, m = 5, maxit = 5, seed = NULL, method = NULL,
                            donors = 5, ...) {
  init <- .impute_meanmode(x, m = 1, seed = seed)$imputations[[1]]
  missing_vars <- names(x)[colSums(is.na(x)) > 0]
  imputations <- vector("list", m)
  variable_methods <- stats::setNames(rep("none", ncol(x)), names(x))
  if (length(missing_vars)) {
    variable_methods[missing_vars] <- vapply(missing_vars, function(nm) {
      .chained_method_for(x[[nm]], method = method, variable = nm)
    }, character(1))
  }

  for (mi in seq_len(m)) {
    if (!is.null(seed)) set.seed(seed + mi - 1)
    completed <- init
    for (iter in seq_len(maxit)) {
      for (nm in missing_vars) {
        obs <- !is.na(x[[nm]])
        mis <- is.na(x[[nm]])
        if (!any(obs) || !any(mis)) next
        predictors <- setdiff(names(x), nm)
        method_nm <- variable_methods[[nm]]
        boot <- sample(which(obs), replace = TRUE)
        learner <- imputer(method_nm)
        fitted <- fit(
          learner,
          x = completed[boot, predictors, drop = FALSE],
          y = x[[nm]][boot],
          target = x[[nm]],
          variable = nm,
          donors = donors,
          ...
        )
        pred <- stats::predict(fitted, completed[mis, predictors, drop = FALSE], ...)
        completed[[nm]][mis] <- .restore_imputed_values(x[[nm]], pred, n = sum(mis))
      }
    }
    imputations[[mi]] <- completed
  }

  list(imputations = imputations, variable_methods = variable_methods,
       diagnostics = list(strategy = "chained_equations",
                          engine = "internal_fit_predict",
                          donor_count = donors),
       stochastic = TRUE)
}

.chained_method_for <- function(x, method = NULL, variable = NULL) {
  if (!is.null(method)) {
    if (length(method) == 1 && is.null(names(method))) return(as.character(method))
    if (!is.null(names(method)) && variable %in% names(method)) return(as.character(method[[variable]]))
  }
  if (is.numeric(x) || is.integer(x) || inherits(x, "Date")) return("pmm")
  if (is.logical(x)) return("logreg")
  nlev <- length(unique(x[!is.na(x)]))
  if (nlev <= 2) "logreg" else "polyreg"
}
