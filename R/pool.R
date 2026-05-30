#' @describeIn pool Pool a data frame of estimates or metrics.
#' @details Estimate pooling expects one row per term and imputation with
#'   `estimate` equal to \eqn{Q_k} and `std.error^2` equal to \eqn{U_k}.
#'   Metric pooling expects one row per metric and imputation.
#' @export
pool.data.frame <- function(x, ...) {
  if (all(c("term", "estimate", "std.error", "imputation") %in% names(x))) {
    spl <- split(x, x$term)
    pooled <- .as_tibble(do.call(rbind, lapply(names(spl), function(term) {
      d <- spl[[term]]
      q <- d$estimate
      u <- d$std.error^2
      m <- length(q)
      qbar <- mean(q, na.rm = TRUE)
      ubar <- mean(u, na.rm = TRUE)
      b <- stats::var(q, na.rm = TRUE)
      if (!is.finite(b)) b <- 0
      total <- ubar + (1 + 1 / m) * b
      se <- sqrt(total)
      r <- if (isTRUE(all.equal(ubar, 0))) Inf else ((1 + 1 / m) * b) / ubar
      df <- if (is.finite(r) && r > 0) (m - 1) * (1 + 1 / r)^2 else Inf
      crit <- stats::qt(.975, df = df)
      statistic <- qbar / se
      p.value <- 2 * stats::pt(abs(statistic), df = df, lower.tail = FALSE)
      data.frame(term = term, estimate = qbar, std.error = se,
                 statistic = statistic, df = df, p.value = p.value,
                 conf.low = qbar - crit * se,
                 conf.high = qbar + crit * se, m = m,
                 within_variance = ubar, between_variance = b,
                 total_variance = total, relative_increase_variance = r,
                 row.names = NULL)
    })))
    out <- list(call = match.call(), pooled = pooled, type = "estimate", data = .as_tibble(x))
  } else if (all(c("metric", "value", "imputation") %in% names(x))) {
    spl <- split(x, x$metric)
    pooled <- .as_tibble(do.call(rbind, lapply(names(spl), function(metric) {
      d <- spl[[metric]]
      q <- d$value
      m <- length(q)
      est <- mean(q, na.rm = TRUE)
      b <- stats::var(q, na.rm = TRUE)
      if (!is.finite(b)) b <- 0
      se <- sqrt(b / m)
      data.frame(metric = metric, estimate = est, std.error = se,
                 conf.low = est - stats::qnorm(.975) * se,
                 conf.high = est + stats::qnorm(.975) * se, m = m,
                 between_variance = b, row.names = NULL)
    })))
    out <- list(call = match.call(), pooled = pooled, type = "metric", data = .as_tibble(x))
  } else {
    .mimar_stop("Pooling requires columns `term`, `estimate`, `std.error`, `imputation` or `metric`, `value`, `imputation`.")
  }
  class(out) <- c("mimar_pool", "list")
  out
}

#' @describeIn pool Pool a list of data frames, adding an `imputation` column
#'   from list position where needed.
#' @export
pool.list <- function(x, ...) {
  if (!length(x) || !all(vapply(x, is.data.frame, logical(1)))) .mimar_stop("`x` must be a list of data frames.")
  for (i in seq_along(x)) if (!"imputation" %in% names(x[[i]])) x[[i]]$imputation <- i
  pool.data.frame(do.call(rbind, x), ...)
}
