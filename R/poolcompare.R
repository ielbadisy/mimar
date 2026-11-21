#' Compare one term across several analyses
#'
#' `poolcompare()` collects the estimate of one model term from several analyses of the same question,
#' such as a complete-case fit, a single imputation and several pooled multiple-imputation fits, so that
#' they can be tabulated or drawn side by side with [plot.mimar_poolcompare()].
#'
#' Each element of `fits` is either a pooled result from mimar ([poolcoxph()], [poolglm()], [poollm()],
#' [poolsurvreg()], [poolclogit()], [poolpanglm()]) or a single fitted model with `coef()` and `vcov()`
#' methods (`coxph`, `glm`, `lm`, `survreg`, ...). Pooled rows keep their Rubin's-rules interval
#' (t reference with the Barnard-Rubin degrees of freedom). Single fits get a Wald interval,
#' \eqn{\hat\beta \pm z_{1-\alpha/2}\,\mathrm{SE}}, as `confint()` gives for `coxph` and `survreg` and
#' `confint.default()` for `glm`; `lm` fits use the t quantile on the residual degrees of freedom, as
#' `confint.lm()`.
#'
#' The coefficient, its standard error and limits are multiplied by `scale`, which turns a per-unit
#' coefficient into one per `scale` units: for a log-transformed covariate `log(x)`, `scale = log(2)`
#' gives the effect per doubling of `x`, and `scale = 10` the effect per 10 units of a raw covariate.
#'
#' A plain `plot(list(...))` method is not provided because a method for bare lists would capture
#' `plot()` calls on any list once mimar is loaded.
#'
#' @param fits A named list of pooled results and/or fitted models. Names label the analyses (in
#'   order); unnamed elements are labelled by position.
#' @param term Name of the coefficient, as in `names(coef(fit))`, for example `"lcopper"` or `"sexF"`.
#' @param scale Multiplier of the coefficient (default 1). For example `log(2)` for a per-doubling effect
#'   of a log-transformed covariate.
#' @param conf.level Confidence level for single fits. Pooled results keep the level they were pooled
#'   with.
#' @return A data frame of class `mimar_poolcompare` with one row per analysis and columns `analysis`,
#'   `term`, `estimate`, `std.error`, `conf.low`, `conf.high` (on the coefficient scale, multiplied by
#'   `scale`), `type` (`"pooled"` or `"single"`), `m` (number of imputations, 1 for single fits) and
#'   `fmi` (`NA` for single fits). Attributes `scale` and `model` (`"cox"`, `"logistic"`, `"log"` or
#'   `"other"`) are used by the plot.
#' @examples
#' imp <- impute(airquality, m = 5, seed = 1, progress = FALSE)
#' cc <- lm(Ozone ~ Wind + Temp, airquality)
#' mi <- poollm(lapply(complete(imp, "all"), function(d) lm(Ozone ~ Wind + Temp, d)))
#' pc <- poolcompare(list(CC = cc, MI = mi), term = "Wind", scale = 5)
#' pc
#' plot(pc)
#' @export
poolcompare <- function(fits, term, scale = 1, conf.level = 0.95) {
  nm <- names(fits) %||% rep("", length(fits))
  nm[nm == ""] <- paste0("Analysis ", seq_along(fits))[nm == ""]
  out <- do.call(rbind, lapply(fits, .pc_row, term = term, conf.level = conf.level))
  out[c("estimate", "std.error", "conf.low", "conf.high")] <- out[c("estimate", "std.error", "conf.low",
                                                                    "conf.high")] * scale
  out$std.error <- abs(out$std.error)
  lim <- out[c("conf.low", "conf.high")]
  out$conf.low <- pmin(lim$conf.low, lim$conf.high); out$conf.high <- pmax(lim$conf.low, lim$conf.high)
  out <- data.frame(analysis = factor(nm, levels = unique(nm)), term = term, out, row.names = NULL)
  mod <- unique(vapply(fits, .pc_model, ""))
  structure(out, class = c("mimar_poolcompare", "data.frame"), scale = scale,
            model = if (length(mod) == 1L) mod else "other")
}

.pc_row <- function(f, term, conf.level) {
  if (inherits(f, "mimar_pool")) {
    p <- as.data.frame(f$pooled)
    if (!"term" %in% names(p) || !term %in% p$term)
      stop(sprintf("term '%s' not found in a pooled result", term), call. = FALSE)
    p <- p[match(term, p$term), ]
    return(data.frame(estimate = p$estimate, std.error = p$std.error, conf.low = p$conf.low,
                      conf.high = p$conf.high, type = "pooled", m = p$m, fmi = p$fmi))
  }
  b <- stats::coef(f)
  if (!term %in% names(b)) stop(sprintf("term '%s' not found in a fitted model", term), call. = FALSE)
  se <- sqrt(diag(as.matrix(stats::vcov(f))))[[term]]
  a <- (1 - conf.level) / 2
  crit <- if (inherits(f, "lm") && !inherits(f, "glm")) stats::qt(1 - a, f$df.residual) else stats::qnorm(1 - a)
  data.frame(estimate = b[[term]], std.error = se, conf.low = b[[term]] - crit * se,
             conf.high = b[[term]] + crit * se, type = "single", m = 1L, fmi = NA_real_)
}

.pc_model <- function(f) {
  if (inherits(f, c("coxph", "mimar_pool_coxlike"))) return("cox")
  link <- if (inherits(f, "glm")) stats::family(f)$link else if (inherits(f, "mimar_pool_glm"))
    c(binomial = "logit", quasibinomial = "logit", poisson = "log", quasipoisson = "log")[f$family]
  if (identical(unname(link), "logit")) "logistic" else if (identical(unname(link), "log")) "log" else "other"
}

#' @export
print.mimar_poolcompare <- function(x, digits = 4, ...) {
  s <- attr(x, "scale")
  cat("Term:", x$term[1], if (s != 1) paste0("(coefficient x ", format(signif(s, 4)), ")"), "\n")
  y <- as.data.frame(unclass(x))[setdiff(names(x), "term")]
  print(y, digits = digits, row.names = FALSE, ...)
  invisible(x)
}

#' Forest plot of one term across several analyses
#'
#' Draws the estimate of one term with its confidence interval for each analysis collected by
#' [poolcompare()], one row per analysis in list order, with a dashed line at no effect.
#'
#' @param x A `mimar_poolcompare` object from [poolcompare()].
#' @param exponentiate If `TRUE`, estimates and limits are exponentiated and drawn on a log axis
#'   (hazard, odds or rate ratios). Default `TRUE` when every analysis is a Cox or conditional
#'   logistic model, or a logistic or log-link GLM.
#' @param labels Optional named character vector of display labels for analyses, such as
#'   `c(CC = "Complete cases")`. Unnamed analyses keep their names.
#' @param ref Optional name of one analysis: its estimate is marked by a dotted vertical line.
#' @param xlab Axis title. Default from the model type, the term and `scale`.
#' @param palette Optional colours: one colour for all rows, a vector with one colour per analysis
#'   (named by analysis or in order), or the names `interval` and `ref` for the interval and the
#'   `ref` line.
#' @param ... Unused.
#' @return A `ggplot` object.
#' @examples
#' imp <- impute(airquality, m = 5, seed = 1, progress = FALSE)
#' cc <- lm(Ozone ~ Wind + Temp, airquality)
#' mi <- poollm(lapply(complete(imp, "all"), function(d) lm(Ozone ~ Wind + Temp, d)))
#' plot(poolcompare(list(CC = cc, MI = mi), term = "Wind"), labels = c(CC = "Complete cases"))
#' @export
plot.mimar_poolcompare <- function(x, exponentiate = attr(x, "model") != "other", labels = NULL, ref = NULL,
                                   xlab = NULL, palette = NULL, ...) {
  an <- levels(x$analysis)
  pp <- palette[names(palette) %in% c("interval", "ref")]
  pal <- c(.pc_cols(palette, an), .pal(if (length(pp)) pp, c(interval = "grey30", ref = "grey50")))
  d <- as.data.frame(unclass(x))
  f <- if (exponentiate) exp else identity
  d[c("estimate", "conf.low", "conf.high")] <- lapply(d[c("estimate", "conf.low", "conf.high")], f)
  d$label <- factor(.lab(d$analysis, labels), levels = rev(.lab(an, labels)))
  d$col <- pal[as.character(d$analysis)]
  s <- attr(x, "scale")
  xlab <- xlab %||% paste0(if (!exponentiate) "Estimate" else switch(attr(x, "model"), cox = "Hazard ratio",
                                                                     logistic = "Odds ratio", log = "Rate ratio",
                                                                     "exp(estimate)"),
                           " for ", d$term[1], if (s != 1) paste0(" (coefficient x ", format(signif(s, 4)), ")"))
  ggplot2::ggplot(d, .gg_aes(x = "estimate", y = "label")) +
    ggplot2::geom_vline(xintercept = f(0), colour = "grey60", linewidth = 0.35, linetype = 2) +
    (if (!is.null(ref)) ggplot2::geom_vline(xintercept = d$estimate[d$analysis == ref], colour = pal[["ref"]],
                                            linewidth = 0.4, linetype = 3)) +
    ggplot2::geom_errorbar(.gg_aes(xmin = "conf.low", xmax = "conf.high"), orientation = "y", width = 0.18,
                           colour = if (length(unique(d$col)) > 1) d$col else pal[["interval"]]) +
    ggplot2::geom_point(size = 2.2, colour = d$col) +
    (if (exponentiate) ggplot2::scale_x_log10()) +
    ggplot2::labs(x = xlab, y = NULL) +
    ggplot2::theme_minimal(base_size = 12)
}

# point colours per analysis: default one blue, else one per analysis (named or in order)
.pc_cols <- function(p, an) {
  out <- stats::setNames(rep("#0072B2", length(an)), an)
  nm <- names(p) %||% rep("", length(p)); p <- p[!nm %in% c("interval", "ref")]
  if (!length(p)) return(out)
  if (all(names(p) %||% "" == "")) return(stats::setNames(rep_len(unname(p), length(an)), an))
  out[intersect(names(p), an)] <- p[intersect(names(p), an)]
  out
}
