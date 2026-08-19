.pool_model_check_fits <- function(fits, expected_class) {
  if (!is.list(fits) || length(fits) < 2) {
    .mimar_stop("`fits` must be a list of at least two fitted models, one per completed data set.")
  }
  ok <- vapply(fits, function(f) inherits(f, expected_class), logical(1))
  if (!all(ok)) {
    .mimar_stop("All elements of `fits` must be `", expected_class, "` objects.")
  }
  invisible(fits)
}

.pool_model_extract <- function(fits) {
  coefs <- lapply(fits, stats::coef)
  term_names <- names(coefs[[1]])
  if (is.null(term_names) || !length(term_names)) {
    .mimar_stop("Fitted models must have named coefficients.")
  }
  same_terms <- vapply(coefs, function(cf) identical(names(cf), term_names), logical(1))
  if (!all(same_terms)) {
    .mimar_stop("All models must share the same coefficient names in the same order. Fit the same formula on each completed data set.")
  }
  vars <- lapply(fits, function(f) {
    d <- diag(as.matrix(stats::vcov(f)))
    vn <- names(d)
    # some models (e.g. survreg) report extra nuisance parameters (like
    # "Log(scale)") in vcov() beyond the coefficient vector; select by name
    # when available rather than assuming positional alignment.
    if (!is.null(vn)) {
      if (!all(term_names %in% vn)) {
        .mimar_stop("vcov() output is missing one or more coefficient names.")
      }
      d <- d[term_names]
    } else if (length(d) == length(term_names)) {
      names(d) <- term_names
    } else {
      .mimar_stop("vcov() output has no names and does not match the coefficient length.")
    }
    d
  })
  list(coefs = coefs, vars = vars, term_names = term_names, m = length(fits))
}

.pool_model_terms <- function(coefs, vars, term_names, conf.level) {
  .rbind_or_empty(lapply(term_names, function(term) {
    q <- vapply(coefs, function(cf) unname(cf[[term]]), numeric(1))
    u <- vapply(vars, function(vv) unname(vv[[term]]), numeric(1))
    .pool_scalar(q, variance = u, name = term, rule = "rubin", conf.level = conf.level)
  }))
}

# Rubin's (1987) degrees of freedom, `df = (m-1)(1+1/r)^2`, diverges to
# implausibly large values whenever between-imputation variance is small
# relative to within-imputation variance (a common situation with large n
# and modest missingness) since r -> 0 there. The Barnard & Rubin (1999)
# correction bounds this using the complete-data degrees of freedom
# `dfcom`, and is what `mice::pool()` uses by default; verified to
# reproduce `mice`'s pooled df, statistic, and p-value to numerical
# precision on cross-checked examples.
.barnard_rubin_df <- function(dfcom, b, t, m) {
  if (!is.finite(dfcom) || dfcom <= 0) return(rep(NA_real_, length(b)))
  mapply(function(bi, ti) {
    if (!is.finite(ti) || ti <= 0) return(dfcom)
    lambda <- (1 + 1 / m) * bi / ti
    lambda <- min(max(lambda, 0), 1)
    if (lambda <= .Machine$double.eps) return(dfcom)
    df_old <- (m - 1) / lambda^2
    df_obs <- ((dfcom + 1) / (dfcom + 3)) * dfcom * (1 - lambda)
    (df_old * df_obs) / (df_old + df_obs)
  }, b, t)
}

# Overwrite `.pool_scalar()`'s classic-Rubin df/statistic/p-value/CI with
# the Barnard-Rubin corrected versions once a complete-data df is known
# (only possible here, not in the generic `pool()` engine, because a
# fitted model provides `dfcom`; a bare scalar/vector quantity does not).
.pool_model_apply_dfcom <- function(pooled, dfcom, m, conf.level) {
  if (!is.finite(dfcom) || dfcom <= 0) return(pooled)
  alpha <- 1 - conf.level
  df_adj <- .barnard_rubin_df(dfcom, pooled$between_variance, pooled$total_variance, m)
  crit <- stats::qt(1 - alpha / 2, df = df_adj)
  pooled$df <- df_adj
  pooled$statistic <- pooled$estimate / pooled$std.error
  pooled$p.value <- 2 * stats::pt(abs(pooled$statistic), df = df_adj, lower.tail = FALSE)
  pooled$conf.low <- pooled$estimate - crit * pooled$std.error
  pooled$conf.high <- pooled$estimate + crit * pooled$std.error
  pooled
}

#' Pool Cox proportional hazards models across imputations
#'
#' `pool_coxph()` combines a list of `coxph` models, one fitted on each
#' completed data set (see `complete(x, "all")`), using Rubin's rules on the
#' log-hazard scale. The printed output mirrors `summary.coxph()`: a
#' coefficient table with `coef`, `exp(coef)`, `se(coef)`, and a hazard-ratio
#' confidence interval table, with an added `df` column for the pooled
#' (Barnard-Rubin) degrees of freedom.
#'
#' For term \eqn{j}, let \eqn{\hat\beta_{jk}} and \eqn{U_{jk} = \widehat{\mathrm{Var}}(\hat\beta_{jk})}
#' be the coefficient and its model-based variance from the fit on completed
#' data set \eqn{k = 1,\ldots,m}. Rubin's rules give the pooled coefficient
#' \eqn{\bar\beta_j = m^{-1}\sum_k \hat\beta_{jk}}, within-imputation variance
#' \eqn{\bar U_j = m^{-1}\sum_k U_{jk}}, between-imputation variance
#' \eqn{B_j = (m-1)^{-1}\sum_k (\hat\beta_{jk} - \bar\beta_j)^2}, and total
#' variance \eqn{T_j = \bar U_j + (1+m^{-1})B_j}. Inference uses a \eqn{t}
#' reference distribution with Barnard-Rubin degrees of freedom. Hazard
#' ratios are obtained by exponentiating the pooled coefficient and its
#' confidence limits.
#'
#' @param fits A list of `coxph` model fits, one per completed data set,
#'   fitted with the same formula (identical coefficient names and order).
#' @param conf.level Confidence level for the hazard-ratio interval.
#' @param ... Currently unused.
#' @return A `mimar_pool_coxph` object with `coefficients` (pooled log-hazard
#'   table) and `conf_int` (pooled hazard-ratio table).
#' @seealso [pool_glm()], [pool()]
#' @examples
#' \donttest{
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   library(survival)
#'   imp <- impute(survival::lung, m = 3, imputer = "pmm", seed = 1)
#'   fits <- lapply(complete(imp, "all"), function(d) {
#'     coxph(Surv(time, status) ~ age + sex + ph.ecog, data = d)
#'   })
#'   pool_coxph(fits)
#' }
#' }
#' @export
pool_coxph <- function(fits, conf.level = 0.95, ...) {
  .pool_coxlike(fits, conf.level, expected_class = "coxph",
                out_class = "mimar_pool_coxph", label = "Pooled Cox proportional hazards model")
}

#' Pool conditional logistic regression models across imputations
#'
#' `pool_clogit()` combines a list of `clogit` models (`survival::clogit()`),
#' one fitted on each completed data set, using Rubin's rules. `clogit()`
#' fits a stratified Cox model internally and returns an object of class
#' `c("clogit", "coxph")`, so `pool_clogit()` pools and formats output
#' identically to [pool_coxph()] (a `coef`/`exp(coef)`/`se(coef)` table plus
#' an odds-ratio confidence-interval table), including using `nevent - p` as
#' the complete-data df for the Barnard-Rubin correction.
#'
#' @inherit pool_coxph details
#' @param fits A list of `clogit` model fits, one per completed data set,
#'   fitted with the same formula (identical coefficient names and order).
#' @param conf.level Confidence level for the odds-ratio interval.
#' @param ... Currently unused.
#' @return A `mimar_pool_coxph`-family object (see [pool_coxph()]).
#' @seealso [pool_coxph()], [pool_glm()], [pool()]
#' @examples
#' \donttest{
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   library(survival)
#'   d <- data.frame(
#'     status = rep(c(1, 0), 50),
#'     age = rnorm(100),
#'     id = rep(1:50, each = 2)
#'   )
#'   d$age[sample(100, 10)] <- NA
#'   imp <- impute(d, m = 3, imputer = "pmm", seed = 1)
#'   fits <- lapply(complete(imp, "all"), function(dd) {
#'     clogit(status ~ age + strata(id), data = dd)
#'   })
#'   pool_clogit(fits)
#' }
#' }
#' @export
pool_clogit <- function(fits, conf.level = 0.95, ...) {
  .pool_coxlike(fits, conf.level, expected_class = "clogit",
                out_class = "mimar_pool_clogit", label = "Pooled conditional logistic regression model")
}

# Shared implementation for `coxph` and `clogit` fits: `clogit()` objects are
# themselves `coxph` objects (class `c("clogit", "coxph")`, same `coef()`,
# `vcov()`, `n`, `nevent` fields), so the pooling and formatting logic is
# identical; only the class check and the printed header differ.
.pool_coxlike <- function(fits, conf.level, expected_class, out_class, label) {
  .pool_model_check_fits(fits, expected_class)
  ex <- .pool_model_extract(fits)
  pooled <- .pool_model_terms(ex$coefs, ex$vars, ex$term_names, conf.level)

  # Neither `coxph` nor `clogit` fits have a `df.residual()` method (partial
  # likelihoods have no residual df in the glm sense). We use `nevent - p`,
  # the number of events minus estimated parameters, as the complete-data
  # df: events, not rows, drive the information in these likelihoods. This
  # is a documented convention, not a value the models themselves report.
  dfcom <- tryCatch(fits[[1]]$nevent - length(ex$term_names), error = function(e) NA_real_)
  pooled <- .pool_model_apply_dfcom(pooled, dfcom, ex$m, conf.level)

  coefficients <- data.frame(
    term = pooled$term,
    coef = pooled$estimate,
    `exp(coef)` = exp(pooled$estimate),
    `se(coef)` = pooled$std.error,
    df = pooled$df,
    z = pooled$statistic,
    `Pr(>|z|)` = pooled$p.value,
    check.names = FALSE,
    row.names = NULL
  )

  pct <- round(100 * conf.level)
  conf_int <- data.frame(
    term = pooled$term,
    `exp(coef)` = exp(pooled$estimate),
    `exp(-coef)` = exp(-pooled$estimate),
    lower = exp(pooled$conf.low),
    upper = exp(pooled$conf.high),
    check.names = FALSE,
    row.names = NULL
  )
  names(conf_int)[4] <- sprintf("lower .%d", pct)
  names(conf_int)[5] <- sprintf("upper .%d", pct)

  out <- list(
    call = match.call(),
    label = label,
    coefficients = coefficients,
    conf_int = conf_int,
    pooled = pooled,
    m = ex$m,
    conf.level = conf.level,
    n = tryCatch(fits[[1]]$n, error = function(e) NA_integer_),
    nevent = tryCatch(fits[[1]]$nevent, error = function(e) NA_integer_)
  )
  class(out) <- c(out_class, "mimar_pool_coxlike", "list")
  out
}

#' @export
print.mimar_pool_coxlike <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat(x$label, "\n", sep = "")
  cat("Pooled across m =", x$m, "imputations (Rubin's rules)\n")
  if (!is.na(x$n)) {
    cat("  n=", x$n, if (!is.na(x$nevent)) paste0(", number of events=", x$nevent), "\n", sep = "")
  }
  cat("\n")

  mat <- as.matrix(x$coefficients[, c("coef", "exp(coef)", "se(coef)", "df", "z", "Pr(>|z|)")])
  rownames(mat) <- x$coefficients$term
  stats::printCoefmat(mat, digits = digits, P.values = TRUE, has.Pvalue = TRUE, cs.ind = 1:3)

  cat("\n")
  ci_mat <- as.matrix(x$conf_int[, -1])
  rownames(ci_mat) <- x$conf_int$term
  print(round(ci_mat, digits))
  invisible(x)
}

#' Pool generalized linear models across imputations
#'
#' `pool_glm()` combines a list of `glm` models, one fitted on each completed
#' data set (see `complete(x, "all")`), using Rubin's rules on the linear
#' predictor scale. The printed output mirrors `summary.glm()`'s coefficient
#' table (`Estimate`, `Std. Error`, test statistic, p-value), with an added
#' `df` column for the pooled (Barnard-Rubin) degrees of freedom. As with
#' [pool_coxph()], the reference distribution is Student's \eqn{t} throughout;
#' the printed statistic is labelled `t value` for families with an estimated
#' dispersion (gaussian, Gamma, inverse.gaussian) and `z value` otherwise, to
#' match the labelling convention of `summary.glm()`, though both derive from
#' the same pooled \eqn{t} reference distribution.
#'
#' @inherit pool_coxph details
#' @param fits A list of `glm` model fits, one per completed data set, fitted
#'   with the same formula (identical coefficient names and order) and the
#'   same family.
#' @param conf.level Confidence level; currently only used for internal
#'   pooled-quantity computation, not printed by default.
#' @param ... Currently unused.
#' @return A `mimar_pool_glm` object with a pooled `coefficients` table.
#' @seealso [pool_coxph()], [pool()]
#' @examples
#' imp <- impute(mtcars, m = 3, imputer = "pmm", seed = 1)
#' fits <- lapply(complete(imp, "all"), function(d) {
#'   glm(vs ~ mpg + wt, data = d, family = binomial())
#' })
#' pool_glm(fits)
#' @export
pool_glm <- function(fits, conf.level = 0.95, ...) {
  .pool_model_check_fits(fits, "glm")
  ex <- .pool_model_extract(fits)
  pooled <- .pool_model_terms(ex$coefs, ex$vars, ex$term_names, conf.level)

  dfcom <- tryCatch(stats::df.residual(fits[[1]]), error = function(e) NA_real_)
  pooled <- .pool_model_apply_dfcom(pooled, dfcom, ex$m, conf.level)

  fam <- tryCatch(fits[[1]]$family$family, error = function(e) "gaussian")
  use_t <- fam %in% c("gaussian", "Gamma", "inverse.gaussian")
  stat_label <- if (use_t) "t value" else "z value"
  p_label <- if (use_t) "Pr(>|t|)" else "Pr(>|z|)"

  coefficients <- data.frame(
    term = pooled$term,
    Estimate = pooled$estimate,
    `Std. Error` = pooled$std.error,
    df = pooled$df,
    statistic = pooled$statistic,
    p.value = pooled$p.value,
    check.names = FALSE,
    row.names = NULL
  )
  names(coefficients)[names(coefficients) == "statistic"] <- stat_label
  names(coefficients)[names(coefficients) == "p.value"] <- p_label

  out <- list(
    call = match.call(),
    coefficients = coefficients,
    pooled = pooled,
    m = ex$m,
    family = fam,
    conf.level = conf.level
  )
  class(out) <- c("mimar_pool_glm", "list")
  out
}

.print_pool_coef_table <- function(x, digits) {
  mat <- as.matrix(x$coefficients[, -1])
  rownames(mat) <- x$coefficients$term
  stats::printCoefmat(mat, digits = digits, P.values = TRUE, has.Pvalue = TRUE, cs.ind = 1:2)
}

#' @export
print.mimar_pool_glm <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("Pooled generalized linear model (family: ", x$family, ")\n", sep = "")
  cat("Pooled across m =", x$m, "imputations (Rubin's rules)\n\n")
  .print_pool_coef_table(x, digits)
  invisible(x)
}

#' Pool linear models across imputations
#'
#' `pool_lm()` combines a list of `lm` models, one fitted on each completed
#' data set (see `complete(x, "all")`), using Rubin's rules. The printed
#' output mirrors `summary.lm()`'s coefficient table (`Estimate`,
#' `Std. Error`, `t value`, `Pr(>|t|)`), with an added `df` column for the
#' pooled (Barnard-Rubin) degrees of freedom, using `df.residual()` of a
#' single fit as the complete-data df.
#'
#' @inherit pool_coxph details
#' @param fits A list of `lm` model fits, one per completed data set, fitted
#'   with the same formula (identical coefficient names and order).
#' @param conf.level Confidence level; currently only used for internal
#'   pooled-quantity computation, not printed by default.
#' @param ... Currently unused.
#' @return A `mimar_pool_lm` object with a pooled `coefficients` table.
#' @seealso [pool_glm()], [pool_coxph()], [pool()]
#' @examples
#' imp <- impute(mtcars, m = 3, imputer = "pmm", seed = 1)
#' fits <- lapply(complete(imp, "all"), function(d) lm(mpg ~ wt + hp, data = d))
#' pool_lm(fits)
#' @export
pool_lm <- function(fits, conf.level = 0.95, ...) {
  .pool_model_check_fits(fits, "lm")
  ex <- .pool_model_extract(fits)
  pooled <- .pool_model_terms(ex$coefs, ex$vars, ex$term_names, conf.level)

  dfcom <- tryCatch(stats::df.residual(fits[[1]]), error = function(e) NA_real_)
  pooled <- .pool_model_apply_dfcom(pooled, dfcom, ex$m, conf.level)

  coefficients <- data.frame(
    term = pooled$term,
    Estimate = pooled$estimate,
    `Std. Error` = pooled$std.error,
    df = pooled$df,
    `t value` = pooled$statistic,
    `Pr(>|t|)` = pooled$p.value,
    check.names = FALSE,
    row.names = NULL
  )

  out <- list(
    call = match.call(),
    coefficients = coefficients,
    pooled = pooled,
    m = ex$m,
    conf.level = conf.level
  )
  class(out) <- c("mimar_pool_lm", "list")
  out
}

#' @export
print.mimar_pool_lm <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("Pooled linear model\n")
  cat("Pooled across m =", x$m, "imputations (Rubin's rules)\n\n")
  .print_pool_coef_table(x, digits)
  invisible(x)
}

#' Pool parametric survival regression models across imputations
#'
#' `pool_survreg()` combines a list of `survreg` accelerated failure time
#' models (`survival::survreg()`), one fitted on each completed data set,
#' using Rubin's rules. The printed output mirrors the coefficient table of
#' `summary.survreg()` (`Value`, `Std. Error`, `z`, `p`), with an added `df`
#' column for the pooled (Barnard-Rubin) degrees of freedom, using
#' `df.residual()` of a single fit as the complete-data df. Only the
#' regression coefficients are pooled; the distribution's scale (or shape)
#' parameter is treated as a nuisance parameter and is not pooled, matching
#' how these models are typically reported after multiple imputation.
#'
#' @inherit pool_coxph details
#' @param fits A list of `survreg` model fits, one per completed data set,
#'   fitted with the same formula and distribution (identical coefficient
#'   names and order).
#' @param conf.level Confidence level; currently only used for internal
#'   pooled-quantity computation, not printed by default.
#' @param ... Currently unused.
#' @return A `mimar_pool_survreg` object with a pooled `coefficients` table.
#' @seealso [pool_coxph()], [pool_glm()], [pool()]
#' @examples
#' \donttest{
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   library(survival)
#'   imp <- impute(survival::lung, m = 3, imputer = "pmm", seed = 1)
#'   fits <- lapply(complete(imp, "all"), function(d) {
#'     survreg(Surv(time, status) ~ age + sex, data = d, dist = "weibull")
#'   })
#'   pool_survreg(fits)
#' }
#' }
#' @export
pool_survreg <- function(fits, conf.level = 0.95, ...) {
  .pool_model_check_fits(fits, "survreg")
  ex <- .pool_model_extract(fits)
  pooled <- .pool_model_terms(ex$coefs, ex$vars, ex$term_names, conf.level)

  dfcom <- tryCatch(stats::df.residual(fits[[1]]), error = function(e) NA_real_)
  pooled <- .pool_model_apply_dfcom(pooled, dfcom, ex$m, conf.level)

  coefficients <- data.frame(
    term = pooled$term,
    Value = pooled$estimate,
    `Std. Error` = pooled$std.error,
    df = pooled$df,
    z = pooled$statistic,
    p = pooled$p.value,
    check.names = FALSE,
    row.names = NULL
  )

  out <- list(
    call = match.call(),
    coefficients = coefficients,
    pooled = pooled,
    m = ex$m,
    dist = tryCatch(fits[[1]]$dist, error = function(e) NA_character_),
    conf.level = conf.level
  )
  class(out) <- c("mimar_pool_survreg", "list")
  out
}

#' @export
print.mimar_pool_survreg <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("Pooled parametric survival regression model", if (!is.na(x$dist)) paste0(" (", x$dist, ")"), "\n", sep = "")
  cat("Pooled across m =", x$m, "imputations (Rubin's rules)\n\n")
  .print_pool_coef_table(x, digits)
  invisible(x)
}
