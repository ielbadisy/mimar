.identity <- function(x) x

.pool_rule <- function(rule, has_variance) {
  if (!is.null(rule)) return(match.arg(rule, c("rubin", "robust", "mean")))
  if (has_variance) "rubin" else "robust"
}

.pool_scalar <- function(q, variance = NULL, std.error = NULL, name = "quantity",
                         rule = NULL, transform = NULL, inverse = NULL,
                         conf.level = 0.95) {
  q <- as.numeric(q)
  if (!length(q)) .mimar_stop("Cannot pool an empty quantity.")
  if (!is.null(std.error)) variance <- as.numeric(std.error)^2
  has_variance <- !is.null(variance)
  rule <- .pool_rule(rule, has_variance)
  transform <- transform %||% .identity
  inverse <- inverse %||% .identity
  z <- transform(q)
  m <- length(z)

  if (identical(rule, "rubin")) {
    if (!has_variance) .mimar_stop("Rubin pooling requires complete-data variances or standard errors.")
    u <- as.numeric(variance)
    if (length(u) != m) .mimar_stop("`variance` or `std.error` must have one value per imputation.")
    qbar <- mean(z, na.rm = TRUE)
    ubar <- mean(u, na.rm = TRUE)
    b <- stats::var(z, na.rm = TRUE)
    if (!is.finite(b)) b <- 0
    total <- ubar + (1 + 1 / m) * b
    se <- sqrt(total)
    r <- if (isTRUE(all.equal(ubar, 0))) Inf else ((1 + 1 / m) * b) / ubar
    df <- if (is.finite(r) && r > 0) (m - 1) * (1 + 1 / r)^2 else Inf
    alpha <- 1 - conf.level
    crit <- stats::qt(1 - alpha / 2, df = df)
    statistic <- qbar / se
    p.value <- 2 * stats::pt(abs(statistic), df = df, lower.tail = FALSE)
    return(data.frame(
      term = name,
      estimate = inverse(qbar),
      std.error = se,
      statistic = statistic,
      df = df,
      p.value = p.value,
      conf.low = inverse(qbar - crit * se),
      conf.high = inverse(qbar + crit * se),
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
    return(data.frame(
      term = name,
      estimate = inverse(est),
      std.error = se,
      conf.low = inverse(est - stats::qnorm(0.975) * se),
      conf.high = inverse(est + stats::qnorm(0.975) * se),
      m = m,
      between_variance = b,
      rule = "mean",
      row.names = NULL
    ))
  }

  qs <- stats::quantile(z, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE)
  data.frame(
    term = name,
    estimate = inverse(qs[[2]]),
    std.error = NA_real_,
    conf.low = inverse(min(z, na.rm = TRUE)),
    conf.high = inverse(max(z, na.rm = TRUE)),
    m = m,
    mean = inverse(mean(z, na.rm = TRUE)),
    median = inverse(qs[[2]]),
    q25 = inverse(qs[[1]]),
    q75 = inverse(qs[[3]]),
    iqr = qs[[3]] - qs[[1]],
    min = inverse(min(z, na.rm = TRUE)),
    max = inverse(max(z, na.rm = TRUE)),
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
                              conf.level = 0.95) {
  if (!length(x) || !.same_dims(x)) .mimar_stop("Quantity lists must be non-empty and have consistent dimensions.")
  first <- x[[1]]
  qmat <- do.call(rbind, lapply(x, as.numeric))
  vmat <- if (!is.null(variance)) do.call(rbind, lapply(variance, as.numeric)) else NULL
  semat <- if (!is.null(std.error)) do.call(rbind, lapply(std.error, as.numeric)) else NULL
  names <- .quantity_names(first)
  pooled <- .as_tibble(do.call(rbind, lapply(seq_len(ncol(qmat)), function(j) {
    .pool_scalar(
      qmat[, j],
      variance = if (!is.null(vmat)) vmat[, j] else NULL,
      std.error = if (!is.null(semat)) semat[, j] else NULL,
      name = names[[j]],
      rule = rule,
      transform = transform,
      inverse = inverse,
      conf.level = conf.level
    )
  })))
  estimate <- pooled$estimate
  dim(estimate) <- dim(first)
  dimnames(estimate) <- dimnames(first)
  list(pooled = pooled, estimate = estimate)
}

.pool_vector_covariance <- function(x, covariance, conf.level = 0.95) {
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
  diag_pooled <- .as_tibble(do.call(rbind, lapply(seq_len(p), function(j) {
    .pool_scalar(qmat[, j], variance = vapply(covariance, function(u) u[j, j], numeric(1)),
                 name = names[[j]], conf.level = conf.level)
  })))
  list(pooled = diag_pooled, estimate = qbar, variance = total,
       within_variance = ubar, between_variance = b)
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
#'   and range across imputations. `"mean"` reports the mean and
#'   between-imputation standard error. Defaults to `"rubin"` when variance is
#'   available and `"robust"` otherwise.
#' @param transform Optional function applied before pooling, for example
#'   `log`, `qlogis`, or `function(p) log(-log(p))`.
#' @param inverse Optional inverse transformation applied to pooled estimates
#'   and intervals.
#' @param conf.level Confidence level for interval estimates.
#' @param name Name of a scalar quantity.
#' @export
pool.numeric <- function(x, variance = NULL, std.error = NULL, covariance = NULL,
                         rule = NULL, transform = NULL, inverse = NULL,
                         conf.level = 0.95, name = "quantity", ...) {
  out <- list(
    call = match.call(),
    pooled = .as_tibble(.pool_scalar(x, variance = variance, std.error = std.error,
                                     name = name, rule = rule, transform = transform,
                                     inverse = inverse, conf.level = conf.level)),
    estimate = NULL,
    type = "scalar",
    data = x
  )
  out$estimate <- out$pooled$estimate[[1]]
  class(out) <- c("mimar_pool", "list")
  out
}

#' @describeIn pool Pool a list of scalar, vector, matrix, or array quantities.
#' @details A list is the preferred input for post-fit quantities. Use a list of
#'   length `m`, one element per imputation. Each element can be a scalar,
#'   vector, matrix, or array. When `covariance` is supplied for a list of
#'   vectors, Rubin's multivariate matrix rule is used. Otherwise list elements
#'   are pooled element by element.
#' @export
pool.list <- function(x, variance = NULL, std.error = NULL, covariance = NULL,
                      rule = NULL, transform = NULL, inverse = NULL,
                      conf.level = 0.95, ...) {
  if (!length(x)) .mimar_stop("`x` must be a non-empty list of quantities.")
  if (all(vapply(x, is.data.frame, logical(1)))) {
    for (i in seq_along(x)) if (!"imputation" %in% names(x[[i]])) x[[i]]$imputation <- i
    return(pool.data.frame(do.call(rbind, x), rule = rule, conf.level = conf.level, ...))
  }
  if (!is.null(covariance)) {
    res <- .pool_vector_covariance(x, covariance = covariance, conf.level = conf.level)
    out <- c(list(call = match.call(), type = "vector", data = x), res)
  } else {
    res <- .pool_elementwise(x, variance = variance, std.error = std.error, rule = rule,
                             transform = transform, inverse = inverse,
                             conf.level = conf.level)
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
                        conf.level = 0.95, ...) {
  quantities <- lapply(seq_len(nrow(x)), function(i) x[i, ])
  if (!is.null(variance) && is.matrix(variance) && identical(dim(variance), dim(x))) {
    variance <- lapply(seq_len(nrow(variance)), function(i) variance[i, ])
  }
  if (!is.null(std.error) && is.matrix(std.error) && identical(dim(std.error), dim(x))) {
    std.error <- lapply(seq_len(nrow(std.error)), function(i) std.error[i, ])
  }
  pool.list(quantities, variance = variance, std.error = std.error, covariance = covariance,
            rule = rule, transform = transform, inverse = inverse,
            conf.level = conf.level, ...)
}

#' @describeIn pool Tabular adapter for tidy scalar estimates or metrics.
#' @details Data frames are accepted as a convenience adapter, but the pooled
#'   object is not the data frame itself. Rows must encode post-fit scalar
#'   quantities: `term`, `estimate`, `std.error`, and `imputation` for Rubin
#'   pooling, or `metric`, `value`, and `imputation` for metric summaries.
#' @export
pool.data.frame <- function(x, variance = NULL, std.error = NULL, covariance = NULL,
                            rule = NULL, transform = NULL, inverse = NULL,
                            conf.level = 0.95, ...) {
  if (all(c("term", "estimate", "std.error", "imputation") %in% names(x))) {
    spl <- split(x, x$term)
    pooled <- .as_tibble(do.call(rbind, lapply(names(spl), function(term) {
      d <- spl[[term]]
      .pool_scalar(d$estimate, std.error = d$std.error, name = term, rule = rule,
                   transform = transform, inverse = inverse, conf.level = conf.level)
    })))
    out <- list(call = match.call(), pooled = pooled, estimate = pooled$estimate,
                type = "tidy_scalar", data = .as_tibble(x))
  } else if (all(c("metric", "value", "imputation") %in% names(x))) {
    spl <- split(x, x$metric)
    pooled <- .as_tibble(do.call(rbind, lapply(names(spl), function(metric) {
      d <- spl[[metric]]
      out <- .pool_scalar(d$value, name = metric, rule = rule, conf.level = conf.level)
      names(out)[names(out) == "term"] <- "metric"
      out
    })))
    out <- list(call = match.call(), pooled = pooled, estimate = pooled$estimate,
                type = "tidy_metric", data = .as_tibble(x))
  } else {
    .mimar_stop("Tabular pooling requires scalar quantity rows with `term`, `estimate`, `std.error`, `imputation`, or metric rows with `metric`, `value`, `imputation`. To pool vectors, matrices, arrays, or scalar values directly, pass them as a list or numeric vector.")
  }
  class(out) <- c("mimar_pool", "list")
  out
}
