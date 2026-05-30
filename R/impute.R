#' @describeIn impute Impute a data frame.
#' @param m Number of completed data sets to generate.
#' @param imputer Imputer name. Any name returned by `imputer_registry()` can
#'   be supplied to use that imputer for all incomplete variables it supports.
#' @param maxit Number of chained-equation iterations.
#' @param seed Optional random seed.
#' @export
impute.data.frame <- function(x, m = 5, imputer = "pmm", maxit = 5, seed = NULL, ...) {
  .check_data_frame(x)
  if (!is.numeric(m) || length(m) != 1 || m < 1) .mimar_stop("`m` must be a positive integer.")
  m <- as.integer(m)
  imputer <- as.character(imputer)[1]
  imputer(imputer)
  res <- .impute_chained(x, m = m, maxit = maxit, seed = seed, method = imputer, ...)
  first <- res$imputations[[1]]
  out <- list(imputations = res$imputations, data = first,
              call = match.call(), data_original = x,
              m = m, imputer = imputer, maxit = maxit, seed = seed,
              variable_methods = res$variable_methods,
              imputed_cells = .imputed_cells_summary(x, first),
              diagnostics = res$diagnostics, stochastic = res$stochastic)
  class(out) <- c("mimar_imputation", "list")
  out
}

#' @describeIn impute Impute a `mimar_amputation` object and retain truth masks
#'   for later evaluation.
#' @export
impute.mimar_amputation <- function(x, m = 5, imputer = "pmm", maxit = 5, seed = NULL, ...) {
  out <- impute.data.frame(x$data, m = m, imputer = imputer, maxit = maxit, seed = seed, ...)
  out$truth <- x$data_original
  out$amputation <- x
  out$mask_added <- x$mask_added
  out$mechanism <- x$mechanism
  out
}
