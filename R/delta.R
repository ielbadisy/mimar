#' Delta-adjust imputed values for a sensitivity analysis
#'
#' `deltaadjust()` shifts the imputed values of chosen numeric variables by a
#' fixed amount \eqn{\delta} in every completed data set. Observed values are
#' never changed. This is the delta-adjustment (pattern-mixture) approach to
#' missing-not-at-random sensitivity analysis: under MAR the imputations draw
#' \eqn{\tilde X_{ij}} from \eqn{p(X_j \mid X_{-j}, R_{ij} = 1)}; the adjusted
#' imputations use
#' \deqn{\tilde X_{ij}^{(\delta)} = \tilde X_{ij} + \delta_j,}
#' so that missing values are assumed to be on average \eqn{\delta_j} higher
#' (or lower) than MAR predicts. The adjustment is applied after imputation,
#' as in `rbmi::delta_template()`; it does not feed back into the chained
#' models of other variables.
#'
#' Use `rows` to restrict the shift to a subgroup, for example the treatment
#' arm only. [tippingpoint()] repeats the adjustment over a grid of
#' \eqn{\delta} values and pools an analysis model at each.
#'
#' @param object A `mimar_imputation` object.
#' @param delta Numeric shift. Either one value applied to every variable in
#'   `variables`, or a named numeric vector with one shift per variable.
#' @param variables Names of the numeric variables to shift. Defaults to the
#'   names of `delta`.
#' @param rows Optional logical vector (length `nrow(data)`) or integer row
#'   indices selecting the rows whose imputed values are shifted. Default: all
#'   rows.
#' @return The `mimar_imputation` with shifted completed data sets. The
#'   applied shifts are stored in `$delta`.
#' @examples
#' imp <- impute(airquality, m = 3, seed = 1, progress = FALSE)
#' adj <- deltaadjust(imp, delta = c(Ozone = -10))
#' mean(complete(adj)$Ozone) - mean(complete(imp)$Ozone)
#' @seealso [tippingpoint()]
#' @export
deltaadjust <- function(object, delta, variables = names(delta), rows = NULL) {
  if (!inherits(object, "mimar_imputation")) {
    .mimar_stop("`object` must be a `mimar_imputation` from impute().")
  }
  if (!is.numeric(delta) || !length(delta) || anyNA(delta)) {
    .mimar_stop("`delta` must be a non-missing numeric value or named numeric vector.")
  }
  if (is.null(variables) || !length(variables)) {
    .mimar_stop("Name the variables to shift in `variables` or as names of `delta`.")
  }
  variables <- as.character(variables)
  original <- as.data.frame(object$data_original)
  unknown <- setdiff(variables, names(original))
  if (length(unknown)) .mimar_stop("Unknown variable(s): ", paste(unknown, collapse = ", "), ".")
  if (length(delta) == 1L && is.null(names(delta))) {
    delta <- stats::setNames(rep(delta, length(variables)), variables)
  }
  if (is.null(names(delta)) || !all(variables %in% names(delta))) {
    .mimar_stop("`delta` must have one named value per variable in `variables`.")
  }
  delta <- delta[variables]
  n <- nrow(original)
  if (is.null(rows)) {
    rows <- rep(TRUE, n)
  } else if (is.logical(rows)) {
    if (length(rows) != n || anyNA(rows)) .mimar_stop("Logical `rows` must have length nrow(data) and no NA.")
  } else if (is.numeric(rows)) {
    if (any(rows < 1 | rows > n)) .mimar_stop("`rows` indices are out of range.")
    rows <- seq_len(n) %in% rows
  } else {
    .mimar_stop("`rows` must be a logical vector or integer row indices.")
  }
  for (v in variables) {
    if (!is.numeric(original[[v]])) {
      .mimar_stop("Variable `", v, "` is not numeric; delta adjustment needs a numeric variable.")
    }
  }
  object$imputations <- lapply(object$imputations, function(d) {
    d <- as.data.frame(d)
    for (v in variables) {
      shift <- is.na(original[[v]]) & rows
      d[[v]][shift] <- d[[v]][shift] + delta[[v]]
    }
    .as_dt(d)
  })
  object$data <- object$imputations[[1]]
  object$delta <- c(object$delta, delta)
  object
}

#' Tipping-point analysis over a grid of delta shifts
#'
#' `tippingpoint()` applies [deltaadjust()] for each value in `deltas`, fits
#' `model` on every completed data set, pools the fits with Rubin's rules, and
#' returns the pooled estimate of `term` at each shift. The tipping point is
#' the smallest shift at which the conclusion changes, for example where the
#' confidence interval first covers zero.
#'
#' @param object A `mimar_imputation` object.
#' @param deltas Numeric vector of shifts to try.
#' @param variables Names of the numeric variables to shift (the same shift is
#'   applied to each).
#' @param model A model formula, or a function of one completed data frame
#'   that returns a fitted model (see [fit()]).
#' @param term Name of the coefficient to report. Default: all coefficients.
#' @param fun Model-fitting function for a formula `model`. Default
#'   [stats::lm()].
#' @param pooler Function that pools a list of fits and returns a `mimar_pool`
#'   object, such as [poollm()] or [poolglm()]. Default: [pool()] on
#'   `coef()` and `vcov()` of each fit.
#' @param rows Passed to [deltaadjust()].
#' @param ... Passed to `fun`.
#' @return A basetable of class `mimar_tippingpoint` with one row per shift and term: `delta`,
#'   `term`, `estimate`, `std.error`, `conf.low`, `conf.high`, `p.value`. `plot()` draws it (see
#'   [plot.mimar_tippingpoint()]).
#' @examples
#' imp <- impute(airquality, m = 5, seed = 1, progress = FALSE)
#' tippingpoint(imp, deltas = c(0, -10, -20), variables = "Ozone",
#'              model = Ozone ~ Wind, term = "Wind", pooler = poollm)
#' @seealso [deltaadjust()]
#' @export
tippingpoint <- function(object, deltas, variables, model, term = NULL,
                         fun = stats::lm, pooler = NULL, rows = NULL, ...) {
  if (!is.numeric(deltas) || !length(deltas) || anyNA(deltas)) {
    .mimar_stop("`deltas` must be a non-missing numeric vector.")
  }
  pool_fits <- pooler %||% function(fits) {
    pool(lapply(fits, stats::coef), covariance = lapply(fits, stats::vcov))
  }
  out <- lapply(deltas, function(d) {
    adj <- deltaadjust(object, delta = d, variables = variables, rows = rows)
    fits <- fit(adj, model, fun = fun, ...)
    res <- as.data.frame(pool_fits(fits)$pooled)
    if (!is.null(term)) {
      res <- res[res$term %in% term, , drop = FALSE]
      if (!nrow(res)) .mimar_stop("`term` not found in the pooled results.")
    }
    data.frame(delta = d, res[, c("term", "estimate", "std.error",
                                  "conf.low", "conf.high", "p.value")],
               row.names = NULL)
  })
  out <- .as_dt(do.call(rbind, out))
  class(out) <- c("mimar_tippingpoint", class(out))
  out
}
