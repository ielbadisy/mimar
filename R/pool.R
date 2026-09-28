.identity <- function(x) x

.pool_rule <- function(rule, has_variance) {
  if (!is.null(rule)) return(match.arg(rule, c("rubin", "robust", "mean")))
  if (has_variance) "rubin" else "robust"
}

.check_dfcom <- function(dfcom) {
  if (is.null(dfcom)) return(NULL)
  if (!is.numeric(dfcom) || length(dfcom) != 1L || is.na(dfcom) || dfcom <= 0) {
    .mimar_stop("`dfcom` must be a single positive number (use `Inf` for a large-sample analysis).")
  }
  dfcom
}

.pool_links <- c("identity", "log", "logit", "cloglog", "fisherz")

# Named pooling scales: forward map, inverse, and derivative dz/dq used to
# carry original-scale variances onto the pooling scale (delta method).
.pool_link <- function(transform = NULL, inverse = NULL) {
  if (!is.character(transform)) {
    return(list(name = NULL, fun = transform %||% .identity,
                inverse = inverse %||% .identity, deriv = NULL))
  }
  if (length(transform) != 1L || !transform %in% .pool_links) {
    .mimar_stop(sprintf("`transform` must be a function or one of %s.",
                        paste0('"', .pool_links, '"', collapse = ", ")))
  }
  if (!is.null(inverse)) .mimar_stop("`inverse` is set automatically when `transform` is a named scale.")
  eps <- 1e-12
  unit <- function(p) pmin(pmax(p, eps), 1 - eps)
  switch(transform,
    identity = list(name = "identity", fun = .identity, inverse = .identity,
                    deriv = function(q) rep(1, length(q))),
    log = list(name = "log", fun = function(q) log(pmax(q, eps)), inverse = exp,
               deriv = function(q) 1 / pmax(q, eps)),
    logit = list(name = "logit", fun = function(q) stats::qlogis(unit(q)),
                 inverse = stats::plogis,
                 deriv = function(q) 1 / (unit(q) * (1 - unit(q)))),
    cloglog = list(name = "cloglog", fun = function(q) log(-log(unit(q))),
                   inverse = function(z) exp(-exp(z)),
                   deriv = function(q) 1 / (unit(q) * log(unit(q)))),
    fisherz = list(name = "fisherz",
                   fun = function(q) atanh(pmin(pmax(q, -1 + eps), 1 - eps)),
                   inverse = tanh,
                   deriv = function(q) 1 / (1 - pmin(q^2, 1 - eps)))
  )
}

.pool_scalar <- function(q, variance = NULL, std.error = NULL, name = "quantity",
                         rule = NULL, transform = NULL, inverse = NULL,
                         conf.level = 0.95, dfcom = NULL) {
  q <- as.numeric(q)
  if (!length(q)) .mimar_stop("Cannot pool an empty quantity.")
  if (!is.null(std.error)) variance <- as.numeric(std.error)^2
  has_variance <- !is.null(variance)
  rule <- .pool_rule(rule, has_variance)
  link <- .pool_link(transform, inverse)
  inverse <- link$inverse
  z <- link$fun(q)
  m <- length(z)
  # Back-transform an interval; a decreasing inverse (e.g. cloglog) swaps ends.
  interval <- function(lo, hi) {
    a <- inverse(lo)
    b <- inverse(hi)
    list(low = pmin(a, b), high = pmax(a, b))
  }

  if (identical(rule, "rubin")) {
    if (!has_variance) .mimar_stop("Rubin pooling requires complete-data variances or standard errors.")
    u <- as.numeric(variance)
    if (length(u) != m) .mimar_stop("`variance` or `std.error` must have one value per imputation.")
    if (!is.null(link$deriv)) u <- u * link$deriv(q)^2
    qbar <- mean(z, na.rm = TRUE)
    ubar <- mean(u, na.rm = TRUE)
    b <- stats::var(z, na.rm = TRUE)
    if (!is.finite(b)) b <- 0
    total <- ubar + (1 + 1 / m) * b
    se <- sqrt(total)
    r <- if (isTRUE(all.equal(ubar, 0))) Inf else ((1 + 1 / m) * b) / ubar
    df <- if (is.finite(r) && r > 0) (m - 1) * (1 + 1 / r)^2 else Inf
    if (!is.null(dfcom) && is.finite(dfcom)) df <- .barnard_rubin_df(dfcom, b, total, m)
    alpha <- 1 - conf.level
    crit <- stats::qt(1 - alpha / 2, df = df)
    statistic <- qbar / se
    p.value <- 2 * stats::pt(abs(statistic), df = df, lower.tail = FALSE)
    ci <- interval(qbar - crit * se, qbar + crit * se)
    est <- inverse(qbar)
    # Named scales report the standard error on the original scale.
    if (!is.null(link$deriv)) se <- se / abs(link$deriv(est))
    return(data.frame(
      term = name,
      estimate = est,
      std.error = se,
      statistic = statistic,
      df = df,
      p.value = p.value,
      conf.low = ci$low,
      conf.high = ci$high,
      m = m,
      within_variance = ubar,
      between_variance = b,
      total_variance = total,
      relative_increase_variance = r,
      rule = "rubin",
      row.names = NULL
    ))
  }

  if (identical(rule, "mean")) {
    est <- mean(z, na.rm = TRUE)
    b <- stats::var(z, na.rm = TRUE)
    if (!is.finite(b)) b <- 0
    se <- sqrt(b / m)
    crit <- stats::qnorm(1 - (1 - conf.level) / 2)
    ci <- interval(est - crit * se, est + crit * se)
    if (!is.null(link$deriv)) se <- se / abs(link$deriv(inverse(est)))
    return(data.frame(
      term = name,
      estimate = inverse(est),
      std.error = se,
      conf.low = ci$low,
      conf.high = ci$high,
      m = m,
      between_variance = b,
      rule = "mean",
      row.names = NULL
    ))
  }

  qs <- stats::quantile(z, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE)
  range <- interval(min(z, na.rm = TRUE), max(z, na.rm = TRUE))
  quart <- interval(qs[[1]], qs[[3]])
  data.frame(
    term = name,
    estimate = inverse(qs[[2]]),
    std.error = NA_real_,
    conf.low = range$low,
    conf.high = range$high,
    m = m,
    mean = inverse(mean(z, na.rm = TRUE)),
    median = inverse(qs[[2]]),
    q25 = quart$low,
    q75 = quart$high,
    iqr = qs[[3]] - qs[[1]],
    min = range$low,
    max = range$high,
    rule = "robust",
    row.names = NULL
  )
}

.same_dims <- function(x) {
  dims <- lapply(x, dim)
  if (all(vapply(dims, is.null, logical(1)))) {
    lengths <- vapply(x, length, integer(1))
    return(all(lengths == lengths[[1]]))
  }
  first <- dims[[1]]
  all(vapply(dims, function(d) identical(d, first), logical(1)))
}

.quantity_names <- function(x) {
  d <- dim(x)
  if (is.null(d)) return(names(x) %||% paste0("q", seq_along(x)))
  idx <- arrayInd(seq_along(x), .dim = d)
  apply(idx, 1, function(z) paste0("q[", paste(z, collapse = ","), "]"))
}

.pool_elementwise <- function(x, variance = NULL, std.error = NULL, rule = NULL,
                              transform = NULL, inverse = NULL,
                              conf.level = 0.95, dfcom = NULL) {
  if (!length(x) || !.same_dims(x)) .mimar_stop("Quantity lists must be non-empty and have consistent dimensions.")
  first <- x[[1]]
  qmat <- do.call(rbind, lapply(x, as.numeric))
  vmat <- if (!is.null(variance)) do.call(rbind, lapply(variance, as.numeric)) else NULL
  semat <- if (!is.null(std.error)) do.call(rbind, lapply(std.error, as.numeric)) else NULL
  names <- .quantity_names(first)
  pooled <- .rbind_or_empty(lapply(seq_len(ncol(qmat)), function(j) {
    .pool_scalar(
      qmat[, j],
      variance = if (!is.null(vmat)) vmat[, j] else NULL,
      std.error = if (!is.null(semat)) semat[, j] else NULL,
      name = names[[j]],
      rule = rule,
      transform = transform,
      inverse = inverse,
      conf.level = conf.level,
      dfcom = dfcom
    )
  }))
  estimate <- pooled$estimate
  dim(estimate) <- dim(first)
  dimnames(estimate) <- dimnames(first)
  list(pooled = pooled, estimate = estimate)
}

.pool_vector_covariance <- function(x, covariance, conf.level = 0.95, dfcom = NULL) {
  if (!length(x) || !all(vapply(x, is.numeric, logical(1))) || !.same_dims(x)) {
    .mimar_stop("Vector pooling requires a non-empty list of numeric vectors with consistent lengths.")
  }
  p <- length(x[[1]])
  if (!length(covariance) || length(covariance) != length(x)) {
    .mimar_stop("`covariance` must contain one covariance matrix per imputation.")
  }
  if (!all(vapply(covariance, function(u) is.matrix(u) && identical(dim(u), c(p, p)), logical(1)))) {
    .mimar_stop("Each covariance matrix must be p by p, where p is the vector length.")
  }
  qmat <- do.call(rbind, x)
  m <- nrow(qmat)
  qbar <- colMeans(qmat, na.rm = TRUE)
  ubar <- Reduce(`+`, covariance) / m
  centered <- sweep(qmat, 2, qbar, "-")
  b <- if (m > 1) stats::cov(qmat, use = "pairwise.complete.obs") else matrix(0, p, p)
  total <- ubar + (1 + 1 / m) * b
  names <- names(x[[1]]) %||% paste0("q", seq_len(p))
  diag_pooled <- .rbind_or_empty(lapply(seq_len(p), function(j) {
    .pool_scalar(qmat[, j], variance = vapply(covariance, function(u) u[j, j], numeric(1)),
                 name = names[[j]], conf.level = conf.level, dfcom = dfcom)
  }))
list(pooled = diag_pooled, estimate = qbar, variance = total,
       within_variance = ubar, between_variance = b)
}

.surv_cloglog <- function(p, clip = 1e-12) {
  p <- pmin(pmax(p, clip), 1 - clip)
  log(-log(p))
}

.surv_inv_cloglog <- function(z) exp(-exp(z))

.surv_backtransform_pooled <- function(pooled, z_estimate, rule) {
  estimate <- .surv_inv_cloglog(z_estimate)
  estimate_vec <- as.numeric(estimate)
  pooled$estimate <- estimate_vec
  if ("std.error" %in% names(pooled) && any(!is.na(pooled$std.error))) {
    pooled$std.error <- pooled$std.error * estimate_vec * (-log(estimate_vec))
  }
  if ("conf.low" %in% names(pooled) && "conf.high" %in% names(pooled)) {
    low_z <- pooled$conf.low
    high_z <- pooled$conf.high
    pooled$conf.low <- .surv_inv_cloglog(high_z)
    pooled$conf.high <- .surv_inv_cloglog(low_z)
  }
  if ("mean" %in% names(pooled)) pooled$mean <- as.numeric(.surv_inv_cloglog(pooled$mean))
  if ("median" %in% names(pooled)) pooled$median <- estimate_vec
  if ("q25" %in% names(pooled) && "q75" %in% names(pooled)) {
    q25_z <- pooled$q25
    q75_z <- pooled$q75
    pooled$q25 <- as.numeric(.surv_inv_cloglog(q75_z))
    pooled$q75 <- as.numeric(.surv_inv_cloglog(q25_z))
  }
  if ("min" %in% names(pooled) && "max" %in% names(pooled)) {
    min_z <- pooled$min
    max_z <- pooled$max
    pooled$min <- as.numeric(.surv_inv_cloglog(max_z))
    pooled$max <- as.numeric(.surv_inv_cloglog(min_z))
  }
  if (!is.null(dim(z_estimate))) {
    dim(estimate) <- dim(z_estimate)
    dimnames(estimate) <- dimnames(z_estimate)
  }
  list(pooled = pooled, estimate = estimate)
}

#' Pool survival-probability matrices across imputations
#'
#' `pool_survmat()` pools a list of same-shaped survival-probability matrices
#' or arrays by applying Rubin-style pooling on the complementary
#' log-log scale and back-transforming the result. The helper is designed for
#' predicted survival probabilities at a grid of times, subjects, or covariate
#' profiles.
#'
#' Let \eqn{S_{ijk}} be the survival probability for imputation \eqn{j}, row
#' index \eqn{i}, and column index \eqn{k}, with \eqn{j = 1,\ldots,m}. Define
#' the complementary log-log transform
#' \deqn{Z_{ijk} = g(S_{ijk}) = \log\{-\log(S_{ijk})\}.}
#' Rubin pooling is then applied elementwise on \eqn{Z_{ijk}}:
#' \deqn{\bar Z_{ik} = m^{-1}\sum_{j=1}^m Z_{ijk},}
#' \deqn{\bar U_{ik} = m^{-1}\sum_{j=1}^m U_{ijk},}
#' \deqn{B_{ik} = (m-1)^{-1}\sum_{j=1}^m (Z_{ijk} - \bar Z_{ik})^2,}
#' \deqn{T_{ik} = \bar U_{ik} + (1 + m^{-1})B_{ik}.}
#' The pooled survival probability is
#' \deqn{\hat S_{ik} = g^{-1}(\bar Z_{ik}) = \exp\{-\exp(\bar Z_{ik})\}.}
#' A delta-method standard error on the original scale is
#' \deqn{\mathrm{SE}(\hat S_{ik}) = \left|\frac{d}{dz} g^{-1}(z)\right|_{z=\bar Z_{ik}}
#' \sqrt{T_{ik}} = \hat S_{ik}\{-\log(\hat S_{ik})\}\sqrt{T_{ik}}.}
#' Confidence intervals are obtained on the transformed scale and then
#' back-transformed:
#' \deqn{[\;g^{-1}(\bar Z_{ik} + t_{\nu,1-\alpha/2}\sqrt{T_{ik}}),\;
#' g^{-1}(\bar Z_{ik} - t_{\nu,1-\alpha/2}\sqrt{T_{ik}})\;],}
#' where \eqn{\nu} is the Rubin (1987) degrees of freedom, or the
#' Barnard-Rubin (1999) degrees of freedom when `dfcom` is supplied. Because \eqn{g^{-1}} is
#' decreasing, the lower survival bound comes from the upper transformed bound.
#'
#' Probabilities are clipped to \code{[clip, 1 - clip]} before transformation to
#' avoid \code{log(0)} at the boundaries.
#'
#' @param x A non-empty list of numeric matrices or arrays containing survival
#'   probabilities. All elements must have the same dimensions.
#' @param variance Optional list of within-imputation variances with the same
#'   dimensions as \code{x}. When supplied, the variance is pooled on the
#'   transformed scale.
#' @param std.error Optional list of within-imputation standard errors with the
#'   same dimensions as \code{x}. Ignored when \code{variance} is supplied.
#' @param rule Pooling rule. Defaults to Rubin pooling when within-imputation
#'   variance is supplied and to the robust median/IQR/range summary otherwise.
#' @param conf.level Confidence level for interval estimates.
#' @param clip Small positive value used to keep probabilities away from 0 and 1
#'   before applying the cloglog transform.
#' @param dfcom Optional complete-data degrees of freedom. See [pool()].
#' @param ... Passed to lower-level pooling helpers.
#' @return A `mimar_pool` object with pooled survival probabilities.
#' @examples
#' surv <- list(
#'   matrix(c(0.90, 0.80, 0.70, 0.60), 2, 2),
#'   matrix(c(0.91, 0.79, 0.72, 0.61), 2, 2),
#'   matrix(c(0.89, 0.81, 0.71, 0.59), 2, 2)
#' )
#' pool_survmat(surv)
#' @export
pool_survmat <- function(x, variance = NULL, std.error = NULL, rule = NULL,
                         conf.level = 0.95, clip = 1e-12, dfcom = NULL, ...) {
  dfcom <- .check_dfcom(dfcom)
  if (!is.list(x) || !length(x)) .mimar_stop("`x` must be a non-empty list of survival-probability matrices or arrays.")
  if (!all(vapply(x, function(u) is.numeric(u) && !is.null(dim(u)), logical(1)))) {
    .mimar_stop("`x` must contain only numeric matrices or arrays.")
  }
  if (!.same_dims(x)) .mimar_stop("Survival matrices must have consistent dimensions across imputations.")
  if (any(vapply(x, function(u) any(u < 0 | u > 1, na.rm = TRUE), logical(1)))) {
    .mimar_stop("Survival probabilities must lie in [0, 1].")
  }
  z_x <- lapply(x, .surv_cloglog, clip = clip)
  res <- .pool_elementwise(
    z_x,
    variance = variance,
    std.error = std.error,
    rule = rule,
    transform = NULL,
    inverse = NULL,
    conf.level = conf.level,
    dfcom = dfcom
  )
  bt <- .surv_backtransform_pooled(res$pooled, res$estimate, rule = .pool_rule(rule, !is.null(variance) || !is.null(std.error)))
  out <- list(
    call = match.call(),
    pooled = .as_dt(bt$pooled),
    estimate = bt$estimate,
    type = "survival_matrix",
    data = x
  )
  class(out) <- c("mimar_pool", "list")
  out
}

#' @describeIn pool Pool a scalar quantity observed across imputations.
#' @param variance Complete-data variance for the quantity in each imputation.
#'   For vector quantities this may also be a list of elementwise variance
#'   vectors or matrices matching `x`.
#' @param std.error Complete-data standard error for the quantity in each
#'   imputation. Ignored when `variance` is supplied.
#' @param covariance For a list of numeric vectors, optional list of covariance
#'   matrices. When supplied, vector pooling uses Rubin's multivariate matrix
#'   form and returns the pooled covariance matrix.
#' @param rule Pooling rule. `"rubin"` applies Rubin's rules and requires
#'   `variance`, `std.error`, or `covariance`. `"robust"` reports median, IQR,
#'   and range across imputations. `"mean"` reports the mean and the
#'   between-imputation standard error \eqn{\sqrt{B/m}}, with a normal-theory
#'   interval at `conf.level`. It ignores within-imputation variance, so it
#'   describes Monte Carlo spread across imputations rather than total
#'   uncertainty; use `"rubin"` whenever complete-data variances are available.
#'   Defaults to `"rubin"` when variance is available and `"robust"` otherwise.
#' @param transform Scale on which to pool. Either a named scale, one of
#'   `"identity"`, `"log"` (positive quantities: hazard ratios, odds ratios,
#'   standard deviations, times), `"logit"` (probabilities, AUC, C-index),
#'   `"cloglog"` (survival probabilities), or `"fisherz"` (correlations); or a
#'   function applied to each estimate before pooling. See Details.
#' @param inverse Inverse of a function `transform`, applied to pooled
#'   estimates and interval limits. Set automatically for a named scale.
#' @param conf.level Confidence level for interval estimates.
#' @param name Name of a scalar quantity.
#' @param dfcom Optional complete-data degrees of freedom, i.e. the degrees of
#'   freedom the analysis would have had without missing data (for example
#'   `n - p` for a regression coefficient, or `n - 1` for a mean). When
#'   supplied, Rubin pooling uses the Barnard and Rubin (1999) small-sample
#'   degrees of freedom, which never exceed `dfcom`. When `NULL` (the
#'   default), the classic Rubin (1987) degrees of freedom are used; they
#'   assume a normal complete-data reference distribution and can be far too
#'   large in small samples.
#' @export
pool.numeric <- function(x, variance = NULL, std.error = NULL, covariance = NULL,
                         rule = NULL, transform = NULL, inverse = NULL,
                         conf.level = 0.95, name = "quantity", dfcom = NULL, ...) {
  dfcom <- .check_dfcom(dfcom)
  out <- list(
    call = match.call(),
    pooled = .as_dt(.pool_scalar(x, variance = variance, std.error = std.error,
                                     name = name, rule = rule, transform = transform,
                                     inverse = inverse, conf.level = conf.level,
                                     dfcom = dfcom)),
    estimate = NULL,
    type = "scalar",
    data = x
  )
  out$estimate <- out$pooled$estimate[[1]]
  class(out) <- c("mimar_pool", "list")
  out
}

#' @details Rubin's rules assume that each complete-data estimate is
#'   approximately normal with the supplied variance, and inference refers
#'   the pooled estimate to a \eqn{t} distribution. For bounded or skewed
#'   quantities (probabilities, AUC, C-index, correlations, ratios) that
#'   assumption holds far better on a transformed scale, so pool there and
#'   back-transform (Marshall et al., 2009). With a named `transform`,
#'   `variance`/`std.error` are given on the original scale and carried to
#'   the pooling scale with the delta method; the reported `estimate`,
#'   `std.error`, and interval are back on the original scale, while
#'   `statistic`, `p.value`, and the variance components refer to the pooling
#'   scale (for `"log"` the test is of a ratio equal to 1, for `"logit"` of a
#'   probability equal to 0.5). With a function `transform`, variances must
#'   already be on the transformed scale and `std.error` is reported there.
#'   Interval limits are ordered correctly for decreasing inverses such as
#'   the complementary log-log.
#' @describeIn pool Pool a list of scalar, vector, matrix, or array quantities.
#' @details A list is the preferred input for post-fit quantities. Use a list of
#'   length `m`, one element per imputation. Each element can be a scalar,
#'   vector, matrix, or array. When `covariance` is supplied for a list of
#'   vectors, Rubin's multivariate matrix rule is used. Otherwise list elements
#'   are pooled element by element.
#' @export
pool.list <- function(x, variance = NULL, std.error = NULL, covariance = NULL,
                      rule = NULL, transform = NULL, inverse = NULL,
                      conf.level = 0.95, dfcom = NULL, ...) {
  if (!length(x)) .mimar_stop("`x` must be a non-empty list of quantities.")
  dfcom <- .check_dfcom(dfcom)
  if (all(vapply(x, is.data.frame, logical(1)))) {
    for (i in seq_along(x)) if (!"imputation" %in% names(x[[i]])) x[[i]]$imputation <- i
    return(pool.data.frame(.rbind_or_empty(x), rule = rule, conf.level = conf.level,
                           dfcom = dfcom, ...))
  }
  if (!is.null(covariance)) {
    res <- .pool_vector_covariance(x, covariance = covariance, conf.level = conf.level,
                                   dfcom = dfcom)
    out <- c(list(call = match.call(), type = "vector", data = x), res)
  } else {
    res <- .pool_elementwise(x, variance = variance, std.error = std.error, rule = rule,
                             transform = transform, inverse = inverse,
                             conf.level = conf.level, dfcom = dfcom)
    out <- c(list(call = match.call(), type = if (is.null(dim(x[[1]]))) "vector_elementwise" else "array_elementwise",
                  data = x), res)
  }
  class(out) <- c("mimar_pool", "list")
  out
}

#' @describeIn pool Pool a matrix whose rows are imputations and columns are
#'   scalar quantities.
#' @export
pool.matrix <- function(x, variance = NULL, std.error = NULL, covariance = NULL,
                        rule = NULL, transform = NULL, inverse = NULL,
                        conf.level = 0.95, dfcom = NULL, ...) {
  quantities <- lapply(seq_len(nrow(x)), function(i) x[i, ])
  if (!is.null(variance) && is.matrix(variance) && identical(dim(variance), dim(x))) {
    variance <- lapply(seq_len(nrow(variance)), function(i) variance[i, ])
  }
  if (!is.null(std.error) && is.matrix(std.error) && identical(dim(std.error), dim(x))) {
    std.error <- lapply(seq_len(nrow(std.error)), function(i) std.error[i, ])
  }
  pool.list(quantities, variance = variance, std.error = std.error, covariance = covariance,
            rule = rule, transform = transform, inverse = inverse,
            conf.level = conf.level, dfcom = dfcom, ...)
}

#' @describeIn pool Tabular adapter for tidy scalar estimates or metrics.
#' @details Data frames are accepted as a convenience adapter, but the pooled
#'   object is not the data frame itself. Rows must encode post-fit scalar
#'   quantities: `term`, `estimate`, `std.error`, and `imputation` for Rubin
#'   pooling, or `metric`, `value`, and `imputation` for metric summaries.
#'   Metric rows that also carry a `std.error` column are pooled with Rubin's
#'   rules (for example `transform = "logit"` for a C-index or AUC); without
#'   it they get the robust summary.
#' @export
pool.data.frame <- function(x, variance = NULL, std.error = NULL, covariance = NULL,
                            rule = NULL, transform = NULL, inverse = NULL,
                            conf.level = 0.95, dfcom = NULL, ...) {
  x <- as.data.frame(x)
  dfcom <- .check_dfcom(dfcom)
  if (all(c("term", "estimate", "std.error", "imputation") %in% names(x))) {
    spl <- split(x, x$term)
    pooled <- .rbind_or_empty(lapply(names(spl), function(term) {
      d <- spl[[term]]
      .pool_scalar(d$estimate, std.error = d$std.error, name = term, rule = rule,
                   transform = transform, inverse = inverse, conf.level = conf.level,
                   dfcom = dfcom)
    }))
    out <- list(call = match.call(), pooled = pooled, estimate = pooled$estimate,
                type = "tidy_scalar", data = .as_dt(x))
  } else if (all(c("metric", "value", "imputation") %in% names(x))) {
    spl <- split(x, x$metric)
    pooled <- .rbind_or_empty(lapply(names(spl), function(metric) {
      d <- spl[[metric]]
      out <- .pool_scalar(d$value, std.error = d$std.error, name = metric, rule = rule,
                          transform = transform, inverse = inverse,
                          conf.level = conf.level, dfcom = dfcom)
      names(out)[names(out) == "term"] <- "metric"
      out
    }))
    out <- list(call = match.call(), pooled = pooled, estimate = pooled$estimate,
                type = "tidy_metric", data = .as_dt(x))
  } else {
    .mimar_stop("Tabular pooling requires scalar quantity rows with `term`, `estimate`, `std.error`, `imputation`, or metric rows with `metric`, `value`, `imputation`. To pool vectors, matrices, arrays, or scalar values directly, pass them as a list or numeric vector.")
  }
  class(out) <- c("mimar_pool", "list")
  out
}
