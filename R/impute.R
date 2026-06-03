#' @describeIn impute Impute a data frame.
#' @param m Number of completed data sets to generate.
#' @param imputer Imputer name or a `mimar_imputer` specification. Any name
#'   returned by `imputer_registry()` can be supplied to use that imputer for
#'   all incomplete variables it supports.
#' @param maxit Number of chained-equation iterations.
#' @param seed Optional random seed.
#' @param donors Number of donor candidates used by donor-based imputers.
#' @param ncore Number of CPU cores used to run completed datasets in
#'   parallel. The default, `1`, runs sequentially. Values greater than one are
#'   passed to `functionals::fmap()` as `ncores`.
#' @param verbose Logical; if `TRUE`, print an informative progress log for the
#'   chained-imputation workflow. The default, `FALSE`, runs silently.
#' @export
impute.data.frame <- function(x, m = 5, imputer = "pmm", maxit = 5, seed = NULL,
                              donors = 5, ncore = 1, verbose = FALSE, ...) {
  .check_data_frame(x)
  if (!is.numeric(m) || length(m) != 1 || m < 1) .mimar_stop("`m` must be a positive integer.")
  m <- as.integer(m)
  imputer_fun <- get("imputer", mode = "function")
  fit_args <- list(...)
  if (inherits(imputer, "mimar_imputer")) {
    imputer_spec <- imputer
    if (length(fit_args)) {
      imputer_spec$args <- utils::modifyList(imputer_spec$args, fit_args)
    }
  } else {
    imputer_name <- as.character(imputer)[1]
    imputer_spec <- do.call(imputer_fun, c(list(imputer_name), fit_args))
  }
  res <- .impute_chained(x, m = m, maxit = maxit, seed = seed,
                         method = imputer_spec$method,
                         imputer_spec = imputer_spec,
                         donors = donors, ncore = ncore,
                         verbose = verbose)
  imputations <- lapply(res$imputations, .as_tibble)
  first <- imputations[[1]]
  out <- list(imputations = imputations, data = first,
              call = match.call(), data_original = .as_tibble(x),
              m = m, imputer = imputer_spec$method, imputer_spec = imputer_spec,
              maxit = maxit, seed = seed,
              donors = as.integer(donors),
              ncore = as.integer(ncore),
              variable_methods = res$variable_methods,
              imputed_cells = .imputed_cells_summary(x, first),
              diagnostics = res$diagnostics, stochastic = res$stochastic)
  class(out) <- c("mimar_imputation", "list")
  out
}

#' @describeIn impute Impute a `mimar_amputation` object and retain truth masks
#'   for later evaluation.
#' @param donors Number of donor candidates used by donor-based imputers.
#' @export
impute.mimar_amputation <- function(x, m = 5, imputer = "pmm", maxit = 5, seed = NULL,
                                    donors = 5, ncore = 1, verbose = FALSE, ...) {
  out <- impute.data.frame(x$data, m = m, imputer = imputer, maxit = maxit,
                           seed = seed, donors = donors, ncore = ncore,
                           verbose = verbose, ...)
  out$truth <- x$data_original
  out$amputation <- x
  out$mask_added <- x$mask_added
  out$mechanism <- x$mechanism
  out
}
