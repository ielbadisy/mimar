.impute_meanmode <- function(x, m = 5, seed = NULL, ...) {
  if (!is.null(seed)) set.seed(seed)
  completed <- x
  variable_methods <- stats::setNames(rep("none", ncol(x)), names(x))
  for (nm in names(completed)) {
    if (!any(is.na(completed[[nm]]))) next
    if (!.supported_variable(completed[[nm]])) {
      warning("Variable '", nm, "' has unsupported type and was left unchanged.", call. = FALSE)
      next
    }
    val <- .simple_fill_value(completed[[nm]])
    completed[[nm]] <- .fill_missing_vector(completed[[nm]], val)
    variable_methods[[nm]] <- if (is.numeric(x[[nm]]) || is.integer(x[[nm]]) || inherits(x[[nm]], "Date")) "median" else "mode"
  }
  list(imputations = rep(list(completed), m), variable_methods = variable_methods,
       diagnostics = list(strategy = "median_mode"), stochastic = FALSE)
}
