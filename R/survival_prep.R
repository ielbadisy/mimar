# Preprocessing helpers for imputation with time-to-event data.
#
# White and Royston (2009) show that the raw survival time must never be used
# as a predictor when imputing partially observed covariates: the correct
# representation of the outcome is the Nelson-Aalen cumulative hazard together
# with the event indicator. `nelsonaalen()` builds that predictor. `ipcw()`
# builds inverse-probability-of-censoring weights for weighted post-imputation
# analyses.

.need_survival <- function(what) {
  if (!requireNamespace("survival", quietly = TRUE)) {
    .mimar_stop("The 'survival' package is required for ", what,
                ". Install it with install.packages(\"survival\").")
  }
  invisible(TRUE)
}

.resolve_col <- function(expr, data, env) {
  if (is.character(expr)) return(as.character(expr))
  if (is.symbol(expr)) {
    nm <- as.character(expr)
    if (nm %in% names(data)) return(nm)
    val <- tryCatch(eval(expr, env), error = function(e) NULL)
    if (is.character(val) && length(val) == 1L) return(val)
    return(nm)
  }
  as.character(eval(expr, env))
}

.surv_prep_input <- function(data, time_expr, status_expr, aux, env) {
  .check_data_frame(data)
  tv <- .resolve_col(time_expr, data, env)
  sv <- .resolve_col(status_expr, data, env)
  if (!tv %in% names(data)) .mimar_stop("`timevar` (\"", tv, "\") is not a column of `data`.")
  if (!sv %in% names(data)) .mimar_stop("`statusvar` (\"", sv, "\") is not a column of `data`.")

  time <- suppressWarnings(as.numeric(data[[tv]]))
  status <- data[[sv]]
  if (is.logical(status)) status <- as.integer(status)
  if (is.factor(status)) status <- suppressWarnings(as.integer(as.character(status)))
  status <- suppressWarnings(as.numeric(status))
  obs_status <- status[!is.na(status)]
  if (!length(obs_status) || !all(obs_status %in% c(0, 1))) {
    .mimar_stop("`statusvar` must be coded 0/1 (or FALSE/TRUE), 1 = event.")
  }

  if (!is.null(aux)) {
    if (!is.character(aux)) .mimar_stop("`aux` must be a character vector of column names.")
    miss_aux <- setdiff(aux, names(data))
    if (length(miss_aux)) {
      .mimar_stop("`aux` column(s) not found in `data`: ", paste(miss_aux, collapse = ", "), ".")
    }
  }

  ok <- !is.na(time) & !is.na(status)
  if (any(time[ok] < 0)) .mimar_stop("`timevar` has negative values.")
  if (!is.null(aux)) ok <- ok & stats::complete.cases(data[aux])

  list(time = time, status = status, ok = ok, timevar = tv, statusvar = sv)
}

.surv_prep_method <- function(method, aux_df, cox_label) {
  if (!identical(method, "auto")) return(method)
  is_cat <- vapply(aux_df, function(z) is.factor(z) || is.character(z) || is.logical(z), logical(1))
  if (all(is_cat)) "strata" else cox_label
}

.surv_strata <- function(aux_df) {
  do.call(interaction, c(lapply(aux_df, factor), list(drop = TRUE)))
}

# Marginal Nelson-Aalen cumulative hazard evaluated at each element of `time`.
.na_cumhaz <- function(time, status) {
  et <- sort(unique(time[status == 1]))
  out <- rep(0, length(time))
  if (!length(et)) return(out)
  d <- vapply(et, function(tt) sum(time == tt & status == 1), numeric(1))
  r <- vapply(et, function(tt) sum(time >= tt), numeric(1))
  ch <- cumsum(d / r)
  idx <- findInterval(time, et)
  out[idx > 0] <- ch[idx[idx > 0]]
  out
}

# Per-subject cumulative hazard H0(T_i) * exp(x_i'beta) from a Cox model,
# using survival's own "expected" prediction (each row scored at its own time).
.cox_cumhaz <- function(time, status, aux_df) {
  .need_survival("`aux` with numeric variables (method = \"breslow\")")
  d <- data.frame(.time = time, .status = status, aux_df, check.names = FALSE)
  form <- stats::as.formula(paste0(
    "survival::Surv(.time, .status) ~ ",
    paste(sprintf("`%s`", names(aux_df)), collapse = " + ")
  ))
  fit <- survival::coxph(form, data = d, ties = "breslow")
  as.numeric(stats::predict(fit, newdata = d, type = "expected"))
}

# Kaplan-Meier survivor function of the `event` process, evaluated
# left-continuously (just before) each element of `at`.
.km_surv_left <- function(time, event, at) {
  et <- sort(unique(time[event == 1]))
  if (!length(et)) return(rep(1, length(at)))
  d <- vapply(et, function(tt) sum(time == tt & event == 1), numeric(1))
  r <- vapply(et, function(tt) sum(time >= tt), numeric(1))
  surv <- cumprod(1 - d / r)
  idx <- findInterval(at - sqrt(.Machine$double.eps), et)
  out <- rep(1, length(at))
  out[idx > 0] <- surv[idx[idx > 0]]
  out
}

# Subject-specific censoring survivor function G_i(T_i-) from a Cox model of
# the censoring process on `aux_df`.
.cox_cens_surv <- function(time, status, aux_df) {
  .need_survival("`aux` with method = \"cox\"")
  d <- data.frame(.time = time, .cens = 1 - status, aux_df, check.names = FALSE)
  form <- stats::as.formula(paste0(
    "survival::Surv(.time, .cens) ~ ",
    paste(sprintf("`%s`", names(aux_df)), collapse = " + ")
  ))
  fit <- survival::coxph(form, data = d, ties = "breslow")
  sf <- survival::survfit(fit, newdata = d)
  st <- sf$time
  sm <- sf$surv
  if (is.null(dim(sm))) sm <- matrix(sm, ncol = 1L)
  idx <- findInterval(time - sqrt(.Machine$double.eps), st)
  vapply(seq_along(time), function(i) if (idx[i] > 0L) sm[idx[i], i] else 1, numeric(1))
}

#' Nelson-Aalen cumulative hazard as an imputation predictor
#'
#' `nelsonaalen()` returns the Nelson-Aalen estimate of the cumulative hazard
#' \eqn{\hat H(t)} evaluated at each subject's event or censoring time.
#' Following White and Royston (2009), the cumulative hazard together with the
#' event indicator is the recommended way to carry time-to-event information
#' into the imputation model for other, partially observed variables: the raw
#' survival time must never be used as a predictor, as it biases the imputed
#' values.
#'
#' With no `aux`, the marginal Nelson-Aalen estimator is used, matching
#' `mice::nelsonaalen()`. When `aux` names one or more auxiliary variables the
#' estimate is made conditional on them: `method = "strata"` estimates a
#' separate cumulative hazard within each combination of the (categorical)
#' `aux` variables, while `method = "breslow"` fits a Cox model on `aux` and
#' returns the per-subject cumulative hazard
#' \eqn{\hat H_0(t)\exp(x_i^\top\hat\beta)} (this path requires the
#' \pkg{survival} package). The default, `method = "auto"`, picks `"strata"`
#' when every `aux` variable is categorical and `"breslow"` otherwise.
#'
#' @param data A data frame.
#' @param timevar Follow-up time. A column name given as a string or as an
#'   unquoted symbol.
#' @param statusvar Event indicator, `1`/`TRUE` for an event and `0`/`FALSE`
#'   for censoring. A column name given as a string or as an unquoted symbol.
#' @param aux Optional character vector of auxiliary column names used to make
#'   the estimate conditional (strata for categorical variables, Cox
#'   covariates otherwise).
#' @param method How `aux` is used: `"auto"` (the default), `"strata"` for
#'   stratified estimation, or `"breslow"` for a Cox model with a
#'   Nelson-Aalen (Breslow) baseline.
#' @return A numeric vector, `nrow(data)` long, giving the cumulative hazard at
#'   each subject's time. Rows with a missing time or status (or missing `aux`,
#'   when `aux` is supplied) yield `NA`.
#' @references White, I. R. and Royston, P. (2009). Imputing missing covariate
#'   values for the Cox model. \emph{Statistics in Medicine}, 28(15),
#'   1982-1998.
#' @seealso [ipcw()]
#' @examples
#' set.seed(1)
#' n <- 120
#' df <- data.frame(
#'   sex = factor(sample(c("F", "M"), n, TRUE)),
#'   bmi = rnorm(n, 26, 4)
#' )
#' event_time <- rexp(n, rate = 1 / 8)
#' cens_time <- rexp(n, rate = 1 / 12)
#' df$time <- pmin(event_time, cens_time)
#' df$status <- as.integer(event_time <= cens_time)
#'
#' ## add H0 as a predictor to use in place of the raw survival time
#' df$H0 <- nelsonaalen(df, "time", "status")
#' df$H0_bysex <- nelsonaalen(df, time, status, aux = "sex")
#'
#' df$bmi[sample(n, 25)] <- NA
#' impute(df[c("bmi", "sex", "status", "H0")], m = 5, imputer = "pmm", seed = 1)
#' @export
nelsonaalen <- function(data, timevar, statusvar, aux = NULL,
                        method = c("auto", "strata", "breslow")) {
  method <- match.arg(method)
  parsed <- .surv_prep_input(data, substitute(timevar), substitute(statusvar),
                             aux, parent.frame())
  time <- parsed$time
  status <- parsed$status
  ok <- parsed$ok

  out <- rep(NA_real_, nrow(data))
  if (!any(ok)) return(out)

  if (is.null(aux)) {
    out[ok] <- .na_cumhaz(time[ok], status[ok])
    return(out)
  }

  aux_df <- data[aux]
  method <- .surv_prep_method(method, aux_df, "breslow")
  if (identical(method, "strata")) {
    strata <- .surv_strata(aux_df)
    for (lv in levels(strata)) {
      idx <- ok & !is.na(strata) & strata == lv
      if (any(idx)) out[idx] <- .na_cumhaz(time[idx], status[idx])
    }
  } else {
    out[ok] <- .cox_cumhaz(time[ok], status[ok], aux_df[ok, , drop = FALSE])
  }
  out
}

#' Inverse probability of censoring weights
#'
#' `ipcw()` computes subject-level inverse-probability-of-censoring weights
#' \eqn{w_i = \Delta_i / \hat G(T_i-)}, where \eqn{\Delta_i} is the event
#' indicator and \eqn{\hat G} is the Kaplan-Meier estimate of the censoring
#' survivor function (Kaplan-Meier applied to the reversed indicator
#' \eqn{1 - \texttt{status}}). Weights of this form remove the bias that
#' right-censoring introduces in a complete-case analysis and can be carried
#' into weighted post-imputation analyses or into [evaluate()].
#'
#' `type = "all"` instead returns \eqn{1 / \hat G(T_i-)} for every subject
#' (censored subjects keep a positive weight), which is the more useful form
#' when the weights are an auxiliary quantity rather than analysis weights.
#' `stabilized = TRUE` multiplies the weights by the marginal censoring
#' survivor function \eqn{\hat G_0(T_i-)}, giving stabilized weights with mean
#' near 1. `truncate` winsorises the weights at the given upper quantile
#' (e.g. `0.99`) to limit the influence of a few very large weights.
#'
#' With `aux` and `method = "cox"` the censoring survivor function is
#' subject-specific, from a Cox model of the censoring process on `aux`
#' (requires \pkg{survival}); with `method = "km"` and categorical `aux`,
#' \eqn{\hat G} is estimated within strata.
#'
#' @inheritParams nelsonaalen
#' @param method How `aux` enters the censoring model: `"km"` (stratified
#'   Kaplan-Meier, the default) or `"cox"` (Cox model).
#' @param type `"event"` (the default) sets the weight to `0` for censored
#'   subjects; `"all"` returns `1 / G(T-)` for every subject.
#' @param stabilized Multiply the weights by the marginal censoring survivor
#'   function, giving stabilized weights with mean near 1.
#' @param truncate Optional upper quantile in `(0, 1)` at which to winsorise
#'   the weights; `NULL` (the default) applies no truncation.
#' @return A numeric vector of weights, `nrow(data)` long. Rows with a missing
#'   time or status (or missing `aux`, when `aux` is supplied) yield `NA`.
#' @references Robins, J. M. and Finkelstein, D. M. (2000). Correcting for
#'   noncompliance and dependent censoring in an AIDS clinical trial with
#'   inverse probability of censoring weighted (IPCW) log-rank tests.
#'   \emph{Biometrics}, 56(3), 779-788.
#' @seealso [nelsonaalen()]
#' @examples
#' set.seed(1)
#' n <- 120
#' sex <- factor(sample(c("F", "M"), n, TRUE))
#' event_time <- rexp(n, rate = 1 / 8)
#' cens_time <- rexp(n, rate = 1 / 12)
#' df <- data.frame(
#'   sex = sex,
#'   time = pmin(event_time, cens_time),
#'   status = as.integer(event_time <= cens_time)
#' )
#'
#' w <- ipcw(df, "time", "status")
#' summary(w[w > 0])
#' summary(ipcw(df, time, status, type = "all"))
#' summary(ipcw(df, time, status, aux = "sex", type = "all", truncate = 0.95))
#' @export
ipcw <- function(data, timevar, statusvar, aux = NULL,
                 method = c("km", "cox"),
                 type = c("event", "all"),
                 stabilized = FALSE, truncate = NULL) {
  method <- match.arg(method)
  type <- match.arg(type)
  if (!is.null(truncate) &&
      (!is.numeric(truncate) || length(truncate) != 1L || truncate <= 0 || truncate >= 1)) {
    .mimar_stop("`truncate` must be a single number in (0, 1), or NULL.")
  }

  parsed <- .surv_prep_input(data, substitute(timevar), substitute(statusvar),
                             aux, parent.frame())
  time <- parsed$time
  status <- parsed$status
  ok <- parsed$ok
  n <- nrow(data)

  G <- rep(NA_real_, n)
  if (!any(ok)) return(G)

  if (is.null(aux)) {
    G[ok] <- .km_surv_left(time[ok], 1 - status[ok], time[ok])
  } else if (identical(method, "km")) {
    strata <- .surv_strata(data[aux])
    for (lv in levels(strata)) {
      idx <- ok & !is.na(strata) & strata == lv
      if (any(idx)) G[idx] <- .km_surv_left(time[idx], 1 - status[idx], time[idx])
    }
  } else {
    G[ok] <- .cox_cens_surv(time[ok], status[ok], data[ok, aux, drop = FALSE])
  }

  G[is.finite(G) & G <= 0] <- NA_real_
  w <- 1 / G
  if (identical(type, "event")) w <- ifelse(status == 1, w, 0)

  if (isTRUE(stabilized)) {
    G0 <- rep(NA_real_, n)
    G0[ok] <- .km_surv_left(time[ok], 1 - status[ok], time[ok])
    w <- w * G0
  }

  if (!is.null(truncate)) {
    pool_w <- w[is.finite(w) & w > 0]
    if (length(pool_w)) {
      cap <- stats::quantile(pool_w, probs = truncate, names = FALSE, na.rm = TRUE)
      w[is.finite(w) & w > cap] <- cap
    }
  }

  w[!ok] <- NA_real_
  as.numeric(w)
}
