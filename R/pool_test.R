# Coefficients and covariance for the tested terms, one column/slice per
# imputation.
.pool_test_wald_parts <- function(fits, terms) {
  coefs <- lapply(fits, stats::coef)
  vcovs <- lapply(fits, stats::vcov)
  missing <- setdiff(terms, names(coefs[[1]]))
  if (length(missing)) {
    .mimar_stop(sprintf("Terms not found in the fitted models: %s.", paste(missing, collapse = ", ")))
  }
  qhat <- vapply(coefs, function(b) b[terms], numeric(length(terms)))
  qhat <- matrix(qhat, nrow = length(terms), dimnames = list(terms, NULL))
  uhat <- array(unlist(lapply(vcovs, function(v) v[terms, terms, drop = FALSE])),
                dim = c(length(terms), length(terms), length(fits)))
  if (anyNA(qhat) || anyNA(uhat)) .mimar_stop("Tested coefficients or their variances contain NA.")
  list(qhat = qhat, uhat = uhat)
}

# F denominator df shared by D1 (without dfcom) and D3 (Li, Raghunathan and
# Rubin, 1991).
.pool_test_df <- function(k, m, r) {
  t <- k * (m - 1)
  if (t > 4) 4 + (t - 4) * (1 + (1 - 2 / t) / r)^2 else t * (1 + 1 / k) * (1 + 1 / r)^2 / 2
}

.pool_test_d1 <- function(qhat, uhat, dfcom) {
  k <- nrow(qhat)
  m <- ncol(qhat)
  qbar <- rowMeans(qhat)
  ubar <- apply(uhat, c(1, 2), mean)
  b <- stats::cov(t(qhat))
  ubar_inv <- solve(ubar)
  r <- (1 + 1 / m) * sum(diag(b %*% ubar_inv)) / k
  statistic <- drop(t(qbar) %*% ubar_inv %*% qbar) / (k * (1 + r))
  t <- k * (m - 1)
  if (!is.null(dfcom) && is.finite(dfcom) && t > 4) {
    # Reiter (2007) small-sample denominator df.
    a <- r * t / (t - 2)
    vstar <- ((dfcom + 1) / (dfcom + 3)) * dfcom
    c0 <- 1 / (t - 4)
    c1 <- vstar - 2 * (1 + a)
    c2 <- vstar - 4 * (1 + a)
    z <- 1 / c2 +
      c0 * (a^2 * c1 / ((1 + a)^2 * c2)) +
      c0 * (8 * a^2 * c1 / ((1 + a) * c2^2) + 4 * a^2 / ((1 + a) * c2)) +
      c0 * (4 * a^2 / (c2 * c1) + 16 * a^2 * c1 / c2^3) +
      c0 * (8 * a^2 / c2^2)
    df2 <- 4 + 1 / z
  } else {
    df2 <- .pool_test_df(k, m, r)
  }
  list(statistic = statistic, df1 = k, df2 = df2, riv = r)
}

.pool_test_d2 <- function(d, k) {
  m <- length(d)
  r <- (1 + 1 / m) * stats::var(sqrt(pmax(d, 0)))
  statistic <- (mean(d) / k - (m + 1) / (m - 1) * r) / (1 + r)
  df2 <- k^(-3 / m) * (m - 1) * (1 + 1 / r)^2
  list(statistic = statistic, df1 = k, df2 = df2, riv = r)
}

# Log-likelihood of an lm/glm fit evaluated at supplied parameters, used by
# D3. Constants that cancel in a likelihood-ratio difference are kept so the
# value equals logLik() at the fit's own estimates.
.pool_test_loglik <- function(fit, beta, sigma2 = NULL) {
  X <- stats::model.matrix(fit)
  eta <- drop(X[, names(beta), drop = FALSE] %*% beta)
  offset <- stats::model.offset(stats::model.frame(fit))
  if (!is.null(offset)) eta <- eta + offset
  w <- stats::weights(fit)
  if (is.null(w)) w <- rep(1, length(eta))
  if (inherits(fit, "glm") && !identical(fit$family$family, "gaussian")) {
    fam <- fit$family
    mu <- fam$linkinv(eta)
    y <- fit$y
    wt <- fit$prior.weights
    dev <- sum(fam$dev.resids(y, mu, wt))
    return(-fam$aic(y, wt, mu, wt, dev) / 2)
  }
  y <- stats::model.response(stats::model.frame(fit))
  keep <- w > 0
  res <- (y - eta)[keep]
  w <- w[keep]
  0.5 * (sum(log(w)) - length(res) * log(2 * pi * sigma2) - sum(w * res^2) / sigma2)
}

.pool_test_d3_parts <- function(fits) {
  ok <- vapply(fits, function(f) {
    inherits(f, "lm") && (!inherits(f, "glm") || f$family$family %in% c("gaussian", "binomial", "poisson"))
  }, logical(1))
  if (!all(ok)) {
    .mimar_stop("D3 needs `lm` fits or `glm` fits with a gaussian, binomial, or poisson family; use D1 or D2 for other models.")
  }
  if (inherits(fits[[1]], "glm") && !identical(fits[[1]]$family$family, "gaussian")) {
    sigma2 <- rep(list(NULL), length(fits))
    sigma2_bar <- NULL
  } else {
    sigma2 <- lapply(fits, function(f) {
      w <- stats::weights(f) %||% rep(1, length(stats::residuals(f)))
      sum(w * stats::residuals(f)^2) / sum(w > 0)
    })
    sigma2_bar <- mean(unlist(sigma2))
  }
  coefs <- lapply(fits, stats::coef)
  if (any(vapply(coefs, anyNA, logical(1)))) .mimar_stop("D3 requires fits without aliased (NA) coefficients.")
  beta_bar <- Reduce(`+`, coefs) / length(coefs)
  own <- mapply(.pool_test_loglik, fits, coefs, sigma2)
  pooled <- vapply(fits, .pool_test_loglik, numeric(1), beta = beta_bar, sigma2 = sigma2_bar)
  list(own = own, pooled = pooled, npar = length(beta_bar) + !is.null(sigma2_bar))
}

.pool_test_d3 <- function(fits, fits0) {
  full <- .pool_test_d3_parts(fits)
  null <- .pool_test_d3_parts(fits0)
  k <- full$npar - null$npar
  m <- length(fits)
  d_bar <- mean(2 * (full$own - null$own))
  d_tilde <- mean(2 * (full$pooled - null$pooled))
  r <- (m + 1) / (k * (m - 1)) * (d_bar - d_tilde)
  list(statistic = d_tilde / (k * (1 + r)), df1 = k, df2 = .pool_test_df(k, m, r), riv = r)
}

#' Pooled multi-parameter tests across imputations
#'
#' `pool_test()` tests several coefficients at once after multiple
#' imputation, for example all dummy variables of a categorical predictor or a
#' full model against a nested one. Testing each pooled coefficient with its
#' own \eqn{t} statistic does not answer that question, and the per-imputation
#' Wald or likelihood-ratio statistics cannot simply be averaged, because
#' that ignores between-imputation variability. The three standard combining
#' rules each refer a pooled statistic to an \eqn{F(k, \nu)} distribution,
#' where \eqn{k} is the number of tested parameters.
#'
#' * `"D1"` (Li, Raghunathan and Rubin, 1991): multivariate Wald test built
#'   from the pooled coefficients and the within- and between-imputation
#'   covariance matrices. It needs `coef()` and `vcov()` for every fit, and
#'   assumes the fraction of missing information is similar across the tested
#'   parameters. With `dfcom`, the denominator df uses the Reiter (2007)
#'   small-sample correction.
#' * `"D2"` (Li, Meng, Raghunathan and Rubin, 1991): combines the \eqn{m}
#'   per-imputation \eqn{\chi^2} statistics, either Wald statistics
#'   (`use = "wald"`) or likelihood-ratio statistics from `logLik()`
#'   (`use = "likelihood"`, needs `fits0`). It works whenever those statistics
#'   exist but is known to be less powerful and can be anti-conservative, so
#'   prefer D1 or D3 when available.
#' * `"D3"` (Meng and Rubin, 1992): likelihood-ratio test that also
#'   evaluates each imputation's likelihood at the pooled estimates. It needs
#'   nested `fits0` and is implemented for `lm` fits and for `glm` fits with a
#'   gaussian, binomial, or poisson family.
#'
#' The terms tested are either `terms`, or, when `fits0` is supplied, the
#' coefficients present in `fits` but absent from `fits0`. The estimated
#' relative increase in variance due to nonresponse (`riv`) is reported
#' untruncated; a negative value, possible for D2 and D3 with small `m`,
#' signals that more imputations are needed. Results match
#' `mitml::testModels()`. For `glm` fits, D3 evaluates each likelihood
#' exactly at the pooled coefficients; `mice::D3()` instead refits an
#' intercept with the pooled linear predictor as offset, so the two can differ
#' slightly.
#'
#' @param fits A list of fitted models, one per completed data set.
#' @param fits0 Optional list of nested (null) models fitted on the same
#'   completed data sets. Required for D3 and for D2 with
#'   `use = "likelihood"`.
#' @param terms Character vector of coefficient names to test against zero.
#'   Used by D1 and Wald-based D2 when `fits0` is `NULL`.
#' @param method One of `"D1"`, `"D2"`, or `"D3"`.
#' @param use For D2, whether to combine Wald (`"wald"`) or
#'   likelihood-ratio (`"likelihood"`) statistics.
#' @param dfcom Optional complete-data degrees of freedom, used by D1 only.
#' @return A `mimar_pool_test` object: a one-row table with the method,
#'   `statistic` (the F value), `df1`, `df2`, `p.value`, relative increase in
#'   variance `riv`, and `m`, plus the tested `terms`.
#' @references
#' Li, K. H., Raghunathan, T. E., and Rubin, D. B. (1991). Large-sample
#' significance levels from multiply imputed data using moment-based
#' statistics and an F reference distribution. *Journal of the American
#' Statistical Association*, 86, 1065-1073.
#'
#' Li, K. H., Meng, X.-L., Raghunathan, T. E., and Rubin, D. B. (1991).
#' Significance levels from repeated p-values with multiply-imputed data.
#' *Statistica Sinica*, 1, 65-92.
#'
#' Meng, X.-L., and Rubin, D. B. (1992). Performing likelihood ratio tests
#' with multiply-imputed data sets. *Biometrika*, 79, 103-111.
#'
#' Reiter, J. P. (2007). Small-sample degrees of freedom for multi-component
#' significance tests with multiple imputation for missing data.
#' *Biometrika*, 94, 502-508.
#' @seealso [pool_lm()], [pool_glm()], [pool()]
#' @examples
#' d <- mtcars
#' d$cyl <- factor(d$cyl)
#' set.seed(1)
#' d$hp[sample(32, 6)] <- NA
#' imp <- impute(d, m = 5, imputer = "pmm", seed = 1)
#' comp <- complete(imp, "all")
#' fits <- lapply(comp, function(x) lm(mpg ~ cyl + hp + wt, data = x))
#' fits0 <- lapply(comp, function(x) lm(mpg ~ hp + wt, data = x))
#' # Does cyl (two dummy coefficients) matter?
#' pool_test(fits, fits0, method = "D1")
#' pool_test(fits, fits0, method = "D3")
#' pool_test(fits, terms = c("cyl6", "cyl8"), method = "D2")
#' @export
pool_test <- function(fits, fits0 = NULL, terms = NULL,
                      method = c("D1", "D2", "D3"),
                      use = c("wald", "likelihood"), dfcom = NULL) {
  method <- match.arg(method)
  use <- match.arg(use)
  dfcom <- .check_dfcom(dfcom)
  if (!is.list(fits) || length(fits) < 2L) .mimar_stop("`fits` must be a list of at least two fitted models.")
  m <- length(fits)
  if (!is.null(fits0) && (!is.list(fits0) || length(fits0) != m)) {
    .mimar_stop("`fits0` must be a list of fitted models with the same length as `fits`.")
  }
  if (!is.null(fits0)) {
    extra <- setdiff(names(stats::coef(fits[[1]])), names(stats::coef(fits0[[1]])))
    if (!length(extra)) .mimar_stop("`fits0` must be nested in `fits`: no coefficients of `fits` are absent from `fits0`.")
    if (!is.null(terms) && !setequal(terms, extra)) {
      .mimar_stop("Supply either `terms` or `fits0`, or make `terms` the coefficients that `fits0` drops.")
    }
    terms <- extra
  }
  needs_null <- method == "D3" || (method == "D2" && use == "likelihood")
  if (needs_null && is.null(fits0)) .mimar_stop(sprintf("Method %s%s needs nested models in `fits0`.", method,
                                                        if (method == "D2") " with use = \"likelihood\"" else ""))
  if (is.null(terms)) .mimar_stop("Supply `terms` to test, or nested models in `fits0`.")

  res <- switch(method,
    D1 = {
      parts <- .pool_test_wald_parts(fits, terms)
      .pool_test_d1(parts$qhat, parts$uhat, dfcom)
    },
    D2 = if (use == "wald") {
      parts <- .pool_test_wald_parts(fits, terms)
      d <- vapply(seq_len(m), function(i) {
        q <- parts$qhat[, i]
        drop(t(q) %*% solve(parts$uhat[, , i], q))
      }, numeric(1))
      .pool_test_d2(d, length(terms))
    } else {
      ll <- lapply(fits, stats::logLik)
      ll0 <- lapply(fits0, stats::logLik)
      k <- attr(ll[[1]], "df") - attr(ll0[[1]], "df")
      .pool_test_d2(2 * (unlist(ll) - unlist(ll0)), k)
    },
    D3 = .pool_test_d3(fits, fits0)
  )

  tab <- data.frame(
    method = method,
    statistic = res$statistic,
    df1 = res$df1,
    df2 = res$df2,
    p.value = stats::pf(res$statistic, res$df1, res$df2, lower.tail = FALSE),
    riv = res$riv,
    m = m,
    row.names = NULL
  )
  out <- list(call = match.call(), test = .as_dt(tab), terms = terms, method = method,
              use = if (method == "D2") use else NULL, dfcom = dfcom)
  class(out) <- c("mimar_pool_test", "list")
  out
}

#' @export
print.mimar_pool_test <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  label <- switch(x$method,
    D1 = "D1 (multivariate Wald)",
    D2 = sprintf("D2 (combined %s chi-square)", if (identical(x$use, "likelihood")) "likelihood-ratio" else "Wald"),
    D3 = "D3 (Meng-Rubin likelihood ratio)"
  )
  cat("Pooled multi-parameter test:", label, "\n")
  cat("Terms:", paste(x$terms, collapse = ", "), "\n\n")
  tab <- as.data.frame(x$test)
  tab$method <- NULL
  print(format(tab, digits = digits), row.names = FALSE)
  invisible(x)
}
