#' Plot missing-data summaries
#'
#' `plot()` on a `describe()` result draws either the share of missing values per variable
#' (`type = "bar"`, optionally by group) or the missing-data pattern matrix (`type = "pattern"`):
#' one row per pattern of observed and missing variables, labelled by its number of rows, most
#' frequent first, as in `mice::md.pattern()`.
#'
#' @param x A `mimar_missing` object from [describe()].
#' @param type `"bar"` (missing share per variable) or `"pattern"` (pattern matrix).
#' @param by Optional name of a grouping column of the described data (for `type = "bar"`): the
#'   missing share is then drawn per group as dodged bars.
#' @param complete If `FALSE` (default), variables without missing values are left out.
#' @param labels Optional named character vector of display labels for variables, such as
#'   `c(lchol = "log cholesterol")`. Unnamed variables keep their names.
#' @param npattern Maximum number of patterns drawn by `type = "pattern"` (the most frequent).
#' @param palette Optional colours: the bar fill, one colour per group with `by`, or for
#'   `"pattern"` the names `observed` and `missing`.
#' @param ... Unused.
#' @return A `ggplot` object.
#' @examples
#' plot(describe(airquality))
#' plot(describe(airquality), type = "pattern")
#' plot(describe(airquality), by = "Month")
#' @export
plot.mimar_missing <- function(x, type = c("bar", "pattern"), by = NULL, complete = FALSE, labels = NULL,
                               npattern = 20L, palette = NULL, ...) {
  type <- match.arg(type)
  dat <- as.data.frame(x$data)
  vars <- setdiff(names(dat), by)
  nmis <- colSums(is.na(dat[vars]))
  if (!complete) vars <- vars[nmis[vars] > 0]
  if (!length(vars)) return(.empty_ggplot("No missing values"))
  if (type == "pattern") return(.plot_pattern(dat[vars], labels, npattern, palette))
  # room on the right for the last percent label, which is centred on the panel edge
  th <- ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.margin = ggplot2::margin(5.5, 18, 5.5, 5.5))
  if (is.null(by)) {
    d <- data.frame(variable = .lab(vars, labels), prop = colMeans(is.na(dat[vars])))
    d$variable <- stats::reorder(d$variable, d$prop)
    return(ggplot2::ggplot(d, .gg_aes(x = "prop", y = "variable")) +
             ggplot2::geom_col(fill = .pal(palette, c(fill = "#0072B2")), width = 0.72) +
             ggplot2::scale_x_continuous(labels = .pct) +
             ggplot2::labs(x = "Missing proportion", y = NULL) + th)
  }
  g <- dat[[by]]
  keep <- !is.na(g); g <- factor(g[keep])
  d <- do.call(rbind, lapply(vars, function(v) {
    data.frame(variable = .lab(v, labels), group = levels(g),
               prop = as.numeric(tapply(is.na(dat[[v]][keep]), g, mean)))
  }))
  d$variable <- stats::reorder(d$variable, d$prop)
  d$group <- factor(d$group, levels = rev(levels(g)))
  ggplot2::ggplot(d, .gg_aes(x = "prop", y = "variable", fill = "group")) +
    ggplot2::geom_col(position = ggplot2::position_dodge(0.8), width = 0.75) +
    ggplot2::scale_x_continuous(labels = .pct) +
    ggplot2::scale_fill_manual(values = .pal(palette, .group_palette(levels(g))), breaks = levels(g)) +
    ggplot2::labs(x = "Missing proportion", y = NULL, fill = .lab(by, labels)) + th +
    ggplot2::theme(legend.position = "bottom")
}

.plot_pattern <- function(dat, labels, npattern, palette) {
  vars <- names(dat)[order(colSums(is.na(dat)))]
  m <- is.na(dat[vars])
  key <- apply(m, 1, function(z) paste(as.integer(z), collapse = ""))
  tab <- table(key)
  # most frequent first, ties broken by fewer missing variables
  pat <- names(tab)[order(-tab, nchar(gsub("0", "", names(tab))))][seq_len(min(npattern, length(tab)))]
  d <- do.call(rbind, lapply(seq_along(pat), function(i) {
    data.frame(pattern = i, variable = vars, status = ifelse(strsplit(pat[i], "")[[1]] == "1", "missing",
                                                              "observed"))
  }))
  d$variable <- factor(.lab(d$variable, labels), levels = .lab(vars, labels))
  d$pattern <- factor(d$pattern, levels = rev(seq_along(pat)))
  ggplot2::ggplot(d, .gg_aes(x = "variable", y = "pattern", fill = "status")) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
    ggplot2::scale_y_discrete(labels = stats::setNames(format(as.integer(tab[pat])), seq_along(pat))) +
    ggplot2::scale_fill_manual(values = .pal(palette, c(observed = "grey85", missing = "#0072B2")),
                               breaks = c("observed", "missing")) +
    ggplot2::labs(x = NULL, y = "Rows with pattern", fill = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "bottom",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' @export
plot.mimar_amputation <- function(x, labels = NULL, palette = NULL, ...) {
  d <- as.data.frame(summary(x))
  d <- d[d$variable %in% x$target | d$added > 0, , drop = FALSE]
  long <- data.frame(variable = .lab(d$variable, labels), n = c(d$original, d$added),
                     status = rep(c("original", "added"), each = nrow(d)))
  long <- long[long$status == "added" | sum(d$original) > 0, ]
  long$variable <- factor(long$variable, levels = .lab(d$variable, labels)[order(d$total)])
  long$status <- factor(long$status, levels = c("added", "original"))
  ggplot2::ggplot(long, .gg_aes(x = "n", y = "variable", fill = "status")) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::scale_fill_manual(values = .pal(palette, c(original = "grey60", added = "#0072B2")),
                               breaks = c("original", "added"),
                               labels = c(original = "Originally missing", added = "Added by amputation")) +
    ggplot2::labs(x = "Missing cells", y = NULL, fill = NULL) +
    ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(legend.position = "bottom")
}

#' Diagnostic plots for mimar imputations
#'
#' `plot.mimar_imputation()` draws imputation diagnostics. By default it shows
#' imputed cell counts. Other plot types show a cell-status map, observed versus
#' imputed distributions, boxplots across imputations, bivariate diagnostics,
#' categorical proportions, convergence traces, variable-level imputation
#' methods, or between-imputation variability.
#'
#' @param x A `mimar_imputation` object.
#' @param type Plot type: `"imputed"`, `"missing"`, `"density"`, `"strip"`,
#'   `"boxplot"`, `"xy"`, `"proportion"`, `"trace"`, `"methods"`, or
#'   `"variability"`.
#' @param variable Optional variable name or names used by distribution plots.
#' @param formula Optional formula for bivariate and stratified diagnostics.
#'   Use forms such as `height ~ weight`, `height ~ weight | gender`, or
#'   `group ~ sex`.
#' @param statistic Trace statistic, either `"mean"` or `"sd"`.
#' @param palette Optional colours, so the defaults can be changed without adding a second ggplot2
#'   scale. A named vector replaces the named entries only; an unnamed vector is recycled in order.
#'   For `"density"`, `"strip"`, `"boxplot"` and `"xy"` the names are `observed` and `imputed`
#'   (default dark grey and blue); for `"trace"` one colour per chain (default grey shades, a single
#'   colour draws all chains alike); for `"proportion"` `observed` then one per imputation (default
#'   dark grey then blue shades); for `"missing"` `observed`, `imputed` and `still missing`; for
#'   `"methods"` one per method; for `"imputed"` and `"variability"` the bar fill.
#' @param average If `TRUE`, `"trace"` adds the across-chain average as a thick line (colour
#'   `palette["average"]`, default black) and drops the per-chain legend.
#' @param labels Optional named character vector of display labels for variables (facet strips,
#'   axes), such as `c(lcopper = "log copper")`. Unnamed variables keep their names.
#' @param ... Unused.
#' @return A `ggplot` object.
#' @export
plot.mimar_imputation <- function(x, type = c("imputed", "missing", "density", "strip", "boxplot",
                                              "xy", "proportion", "trace", "methods", "variability"),
                                  variable = NULL, formula = NULL,
                                  statistic = c("mean", "sd"), palette = NULL, average = FALSE,
                                  labels = NULL, ...) {
  type <- match.arg(type)
  statistic <- match.arg(statistic)
  .plot_imputation_ggplot(x, type = type, variable = variable, formula = formula,
                          statistic = statistic, palette = palette, average = average, labels = labels)
}

#' Plot recovery errors of an evaluated imputation
#'
#' Bars of the recovery error per variable: the normalized RMSE (RMSE divided by the SD of the true
#' values, comparable across variables) or the raw RMSE for numeric variables, and the proportion of
#' falsely classified cells (PFC) for categorical ones.
#'
#' @param x A `mimar_evaluation` object from [evaluate()] with known truth.
#' @param metric `"nrmse"` (default) or `"rmse"` for numeric variables.
#' @param compare Optional named list of further `mimar_evaluation` objects (other imputers) drawn as
#'   dodged bars next to `x`, for example `list(rf = evaluate(imp2))`. `x` is named by its imputer.
#' @param labels Optional named character vector of display labels for variables.
#' @param palette Optional colours, one per metric (`NRMSE`/`RMSE`, `PFC`) or per imputer with `compare`.
#' @param ... Unused.
#' @return A `ggplot` object.
#' @export
plot.mimar_evaluation <- function(x, metric = c("nrmse", "rmse"), compare = NULL, labels = NULL,
                                  palette = NULL, ...) {
  metric <- match.arg(metric)
  ev <- c(stats::setNames(list(x), x$summary$imputer[1] %||% "x"), compare)
  d <- do.call(rbind, lapply(names(ev), function(nm) {
    r <- as.data.frame(ev[[nm]]$recovery)
    if (!nrow(r)) return(NULL)
    num <- r$type == "numeric"
    data.frame(imputer = nm, variable = .lab(r$variable, labels), value = ifelse(num, r[[metric]], r$pfc),
               metric = ifelse(num, toupper(metric), "PFC"))
  }))
  if (is.null(d)) return(.empty_ggplot("No recovery metrics available"))
  d$variable <- stats::reorder(d$variable, d$value)
  th <- ggplot2::theme_minimal(base_size = 12)
  xl <- paste0("Recovery error (", paste(unique(d$metric), collapse = " or "), ")")
  if (is.null(compare)) {
    return(ggplot2::ggplot(d, .gg_aes(x = "value", y = "variable", fill = "metric")) +
             ggplot2::geom_col(width = 0.72) +
             ggplot2::scale_fill_manual(values = .pal(palette, c(NRMSE = "#0072B2", RMSE = "#0072B2",
                                                                 PFC = "#D55E00"))) +
             ggplot2::labs(x = xl, y = NULL, fill = NULL) + th +
             ggplot2::theme(legend.position = if (length(unique(d$metric)) > 1) "bottom" else "none"))
  }
  d$imputer <- factor(d$imputer, levels = rev(names(ev)))
  ggplot2::ggplot(d, .gg_aes(x = "value", y = "variable", fill = "imputer")) +
    ggplot2::geom_col(position = ggplot2::position_dodge(0.8), width = 0.75) +
    ggplot2::scale_fill_manual(values = .pal(palette, .group_palette(names(ev))), breaks = names(ev)) +
    ggplot2::labs(x = xl, y = NULL, fill = NULL) + th + ggplot2::theme(legend.position = "bottom")
}

.plot_imputation_ggplot <- function(x, type, variable = NULL, formula = NULL,
                                    statistic = "mean", palette = NULL, average = FALSE, labels = NULL) {
  if (identical(type, "imputed")) {
    d <- .imputation_variable_summary(x)
    d$variable <- .lab(d$variable, labels)
    d$variable <- stats::reorder(d$variable, d$n_imputed)
    return(
      ggplot2::ggplot(d, .gg_aes(x = "n_imputed", y = "variable")) +
        ggplot2::geom_col(fill = .pal(palette, c(fill = "#0072B2")), width = 0.72) +
        ggplot2::labs(x = "Imputed cells", y = NULL) +
        ggplot2::theme_minimal(base_size = 12)
    )
  }
  if (identical(type, "missing")) return(.plot_missing_map_ggplot(x, palette, labels))
  if (identical(type, "boxplot")) return(.plot_boxplot_ggplot(x, variable, palette, labels))
  if (identical(type, "xy")) return(.plot_xy_ggplot(x, formula = formula, palette = palette, labels = labels))
  if (identical(type, "proportion")) return(.plot_proportion_ggplot(x, variable = variable, formula = formula,
                                                                       palette = palette, labels = labels))
  if (identical(type, "trace")) return(.plot_trace_ggplot(x, variable = variable, statistic = statistic,
                                                             palette = palette, average = average,
                                                             labels = labels))
  if (identical(type, "methods")) {
    d <- .imputation_variable_summary(x)
    d <- d[d$n_missing_before > 0, , drop = FALSE]
    if (!nrow(d)) return(.empty_ggplot("No variables were imputed"))
    d$variable <- .lab(d$variable, labels)
    d$variable <- stats::reorder(d$variable, d$n_missing_before)
    return(
      ggplot2::ggplot(d, .gg_aes(x = "n_missing_before", y = "variable", fill = "method")) +
        ggplot2::geom_col(width = 0.72) +
        ggplot2::scale_fill_manual(values = .pal(palette, .mimar_palette(unique(d$method))),
                                   na.value = "grey60") +
        ggplot2::labs(x = "Missing cells before imputation", y = NULL, fill = "Method") +
        ggplot2::theme_minimal(base_size = 12)
    )
  }
  if (identical(type, "variability")) {
    d <- .imputation_variable_summary(x)
    d <- d[!is.na(d$between_imputation_sd), , drop = FALSE]
    if (!nrow(d) || !any(is.finite(d$between_imputation_sd))) {
      return(.empty_ggplot("No between-imputation variability available"))
    }
    d$variable <- .lab(d$variable, labels)
    d$variable <- stats::reorder(d$variable, d$between_imputation_sd)
    return(
      ggplot2::ggplot(d, .gg_aes(x = "between_imputation_sd", y = "variable")) +
        ggplot2::geom_col(fill = .pal(palette, c(fill = "#0072B2")), width = 0.72) +
        ggplot2::labs(x = "Between-imputation SD", y = NULL) +
        ggplot2::theme_minimal(base_size = 12)
    )
  }
  .plot_distribution_ggplot(x, type = type, variable = variable, palette = palette, labels = labels)
}

.plot_missing_map_ggplot <- function(x, palette = NULL, labels = NULL) {
  original <- x$data_original
  first <- x$imputations[[1]]
  d <- .rbind_or_empty(lapply(names(original), function(nm) {
    before <- is.na(original[[nm]])
    after <- is.na(first[[nm]])
    status <- ifelse(before & !after, "imputed", ifelse(before & after, "still missing", "observed"))
    data.frame(row = seq_len(nrow(original)), variable = nm, status = status, row.names = NULL)
  }))
  d$variable <- factor(.lab(d$variable, labels), levels = .lab(names(original), labels))
  d$status <- factor(d$status, levels = c("observed", "imputed", "still missing"))
  ggplot2::ggplot(d, .gg_aes(x = "variable", y = "row", fill = "status")) +
    ggplot2::geom_tile() +
    ggplot2::scale_y_reverse() +
    ggplot2::scale_fill_manual(values = .pal(palette, c(observed = "grey92", imputed = "#0072B2",
                                                         `still missing` = "#D55E00"))) +
    ggplot2::labs(x = NULL, y = "Row", fill = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

.plot_distribution_ggplot <- function(x, type, variable = NULL, palette = NULL, labels = NULL) {
  d <- .observed_imputed_plot_data(x, variable = variable)
  if (!nrow(d)) return(.empty_ggplot("No plottable imputed variables available"))
  d$variable <- .lab(d$variable, labels)
  if (identical(type, "density")) {
    d <- d[d$value_type == "numeric", , drop = FALSE]
    if (!nrow(d)) return(.empty_ggplot("Density diagnostics require numeric imputed variables"))
    d$value <- as.numeric(d$value)
    d$series <- ifelse(d$imputation == 0, "observed", paste0("imputation ", d$imputation))
    d$group_id <- interaction(d$variable, d$series, drop = TRUE)
    return(
      ggplot2::ggplot(d, .gg_aes(x = "value", colour = "status", group = "group_id")) +
        # observed drawn last and thicker so the imputed curves do not hide it
        ggplot2::geom_density(data = d[d$status == "imputed", ], linewidth = 0.5, alpha = 0.8, na.rm = TRUE,
                              key_glyph = "path") +
        ggplot2::geom_density(data = d[d$status == "observed", ], linewidth = 1.1, na.rm = TRUE,
                              key_glyph = "path") +
        ggplot2::facet_wrap(stats::as.formula("~ variable"), scales = "free") +
        ggplot2::scale_x_continuous(n.breaks = 4) +
        ggplot2::scale_colour_manual(values = .pal(palette, .mimar_status_palette()),
                                     breaks = c("observed", "imputed")) +
        ggplot2::labs(x = "Value", y = "Density", colour = NULL) +
        ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(legend.position = "bottom")
    )
  }
  d$imputation <- factor(d$imputation)
  ggplot2::ggplot(d, .gg_aes(x = "imputation", y = "value", colour = "status")) +
    ggplot2::geom_jitter(width = 0.18, height = 0, alpha = 0.55, na.rm = TRUE) +
    ggplot2::facet_wrap(stats::as.formula("~ variable"), scales = "free_y") +
    ggplot2::scale_colour_manual(values = .pal(palette, .mimar_status_palette())) +
    ggplot2::labs(x = "Imputation number", y = NULL, colour = NULL) +
    ggplot2::theme_minimal(base_size = 12)
}

.observed_imputed_plot_data <- function(x, variable = NULL) {
  original <- x$data_original
  vars <- names(original)[colSums(is.na(original)) > 0]
  if (!is.null(variable)) vars <- intersect(vars, variable)
  .rbind_or_empty(lapply(vars, function(nm) {
    observed <- original[[nm]][!is.na(original[[nm]])]
    numeric_var <- is.numeric(original[[nm]]) || is.integer(original[[nm]]) || inherits(original[[nm]], "Date")
    obs <- data.frame(
      variable = nm,
      imputation = 0L,
      status = "observed",
      value = if (numeric_var) as.numeric(observed) else as.character(observed),
      value_type = if (numeric_var) "numeric" else "categorical",
      row.names = NULL
    )
    imp <- .rbind_or_empty(lapply(seq_along(x$imputations), function(mi) {
      completed <- x$imputations[[mi]]
      imputed <- completed[[nm]][is.na(original[[nm]]) & !is.na(completed[[nm]])]
      if (!length(imputed)) return(NULL)
      data.frame(
        variable = nm,
        imputation = mi,
        status = "imputed",
        value = if (numeric_var) as.numeric(imputed) else as.character(imputed),
        value_type = if (numeric_var) "numeric" else "categorical",
        row.names = NULL
      )
    }))
    out <- rbind(obs, imp)
    if (!any(out$status == "observed") || !any(out$status == "imputed")) return(NULL)
    out
  }))
}

.plot_boxplot_ggplot <- function(x, variable = NULL, palette = NULL, labels = NULL) {
  d <- .observed_imputed_plot_data(x, variable = variable)
  d <- d[d$value_type == "numeric", , drop = FALSE]
  if (!nrow(d)) return(.empty_ggplot("Boxplot diagnostics require numeric imputed variables"))
  d$variable <- .lab(d$variable, labels)
  d$imputation <- factor(d$imputation, levels = sort(unique(d$imputation)))
  ggplot2::ggplot(d, .gg_aes(x = "imputation", y = "value", colour = "status")) +
    ggplot2::geom_boxplot(outlier.alpha = 0.45, width = 0.62, na.rm = TRUE) +
    ggplot2::stat_summary(fun = mean, geom = "point", size = 2, na.rm = TRUE) +
    ggplot2::facet_wrap(stats::as.formula("~ variable"), scales = "free_y") +
    ggplot2::scale_colour_manual(values = .pal(palette, .mimar_status_palette())) +
    ggplot2::labs(x = "Imputation number", y = NULL, colour = NULL) +
    ggplot2::theme_minimal(base_size = 12)
}

.plot_trace_ggplot <- function(x, variable = NULL, statistic = "mean", palette = NULL, average = FALSE,
                               labels = NULL) {
  d <- x$diagnostics$trace
  if (is.null(d) || !nrow(d)) return(.empty_ggplot("No convergence trace available"))
  d <- d[!is.na(d[[statistic]]), , drop = FALSE]
  if (!is.null(variable)) d <- d[d$variable %in% variable, , drop = FALSE]
  if (!nrow(d)) return(.empty_ggplot("No numeric trace available for selected variables"))
  d$variable <- .lab(d$variable, labels)
  d$imputation <- factor(d$imputation)
  lev <- levels(d$imputation)
  chains <- if (is.null(names(palette))) palette else palette[names(palette) != "average"]
  pal <- .pal(chains, .grey_shades(lev))
  p <- ggplot2::ggplot(d, .gg_aes(x = "iteration", y = statistic, colour = "imputation",
                                  group = "imputation")) +
    ggplot2::geom_line(linewidth = 0.55, alpha = 0.85) +
    ggplot2::geom_point(size = 1.2, alpha = 0.75) +
    ggplot2::facet_wrap(stats::as.formula("~ variable"), scales = "free_y") +
    ggplot2::scale_colour_manual(values = pal[lev]) +
    ggplot2::scale_x_continuous(breaks = function(l) unique(round(pretty(l)))) +
    ggplot2::labs(x = "Iteration", colour = "Imputation",
                  y = c(mean = "Mean of imputed values", sd = "SD of imputed values")[[statistic]]) +
    ggplot2::theme_minimal(base_size = 12)
  # one grey legend entry per chain carries no information once chains are many or averaged
  if (average || length(lev) > 8) p <- p + ggplot2::guides(colour = "none")
  if (!average) return(p)
  a <- stats::aggregate(d[statistic], d[c("variable", "iteration")], mean)
  p + ggplot2::geom_line(data = a, .gg_aes(x = "iteration", y = statistic), inherit.aes = FALSE,
                         colour = if ("average" %in% names(palette)) palette[["average"]] else "black",
                         linewidth = 1.2)
}

.plot_xy_ggplot <- function(x, formula = NULL, palette = NULL, labels = NULL) {
  if (is.null(formula)) .mimar_stop("`formula` is required for type = 'xy'.")
  spec <- .parse_xy_formula(formula)
  d <- .xy_plot_data(x, spec)
  if (!nrow(d)) return(.empty_ggplot("No bivariate diagnostic data available"))
  p <- ggplot2::ggplot(d, .gg_aes(x = "x", y = "y", colour = "status", fill = "status")) +
    ggplot2::geom_point(.gg_aes(shape = "status"), alpha = 0.7, size = 1.8, na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = .pal(palette, .mimar_status_palette())) +
    ggplot2::scale_fill_manual(values = c(observed = "#FFFFFF",
                                          imputed = .pal(palette, .mimar_status_palette())[["imputed"]])) +
    ggplot2::scale_shape_manual(values = c(observed = 21, imputed = 19)) +
    ggplot2::labs(x = .lab(spec$x, labels), y = .lab(spec$y, labels), colour = NULL, fill = NULL,
                  shape = NULL) +
    ggplot2::theme_minimal(base_size = 12)
  if (!is.null(spec$by)) p <- p + ggplot2::facet_wrap(stats::as.formula("~ by"), scales = "free")
  p
}

.xy_plot_data <- function(x, spec) {
  original <- x$data_original
  needed <- c(spec$x, spec$y, spec$by)
  if (!all(needed %in% names(original))) {
    .mimar_stop("All variables in `formula` must be present in the imputed data.")
  }
  observed_idx <- !is.na(original[[spec$x]]) & !is.na(original[[spec$y]])
  obs <- data.frame(
    imputation = 0L,
    status = "observed",
    x = as.numeric(original[[spec$x]][observed_idx]),
    y = as.numeric(original[[spec$y]][observed_idx]),
    by = if (is.null(spec$by)) "all" else as.character(original[[spec$by]][observed_idx]),
    row.names = NULL
  )
  imputed_idx <- is.na(original[[spec$x]]) | is.na(original[[spec$y]])
  imp <- .rbind_or_empty(lapply(seq_along(x$imputations), function(mi) {
    completed <- x$imputations[[mi]]
    keep <- imputed_idx & !is.na(completed[[spec$x]]) & !is.na(completed[[spec$y]])
    if (!any(keep)) return(NULL)
    data.frame(
      imputation = mi,
      status = "imputed",
      x = as.numeric(completed[[spec$x]][keep]),
      y = as.numeric(completed[[spec$y]][keep]),
      by = if (is.null(spec$by)) "all" else as.character(completed[[spec$by]][keep]),
      row.names = NULL
    )
  }))
  .rbind_or_empty(list(obs, imp))
}

.parse_xy_formula <- function(formula) {
  if (!inherits(formula, "formula") || length(formula) != 3) {
    .mimar_stop("`formula` must have the form y ~ x or y ~ x | group.")
  }
  y <- all.vars(formula[[2]])
  rhs <- formula[[3]]
  if (is.call(rhs) && identical(as.character(rhs[[1]]), "|")) {
    x <- all.vars(rhs[[2]])
    by <- all.vars(rhs[[3]])
  } else {
    x <- all.vars(rhs)
    by <- character()
  }
  if (length(y) != 1 || length(x) != 1 || length(by) > 1) {
    .mimar_stop("`formula` must identify one y variable, one x variable, and at most one grouping variable.")
  }
  list(y = y, x = x, by = if (length(by)) by else NULL)
}

.plot_proportion_ggplot <- function(x, variable = NULL, formula = NULL, palette = NULL, labels = NULL) {
  spec <- .parse_proportion_formula(formula)
  if (!is.null(spec$variable)) variable <- spec$variable
  d <- .proportion_plot_data(x, variable = variable, strata = spec$strata)
  if (!nrow(d)) return(.empty_ggplot("No categorical imputation proportions available"))
  if (!length(spec$strata)) d$panel <- .lab(d$panel, labels)
  d$imputation <- factor(d$imputation)
  d$series <- ifelse(d$imputation == "0", "observed", paste0("imp ", d$imputation))
  p <- ggplot2::ggplot(d, .gg_aes(x = "value", y = "proportion", fill = "series")) +
    ggplot2::geom_col(position = "dodge", width = 0.72) +
    ggplot2::facet_wrap(stats::as.formula("~ panel"), scales = "free_x") +
    ggplot2::scale_fill_manual(values = .pal(palette, .series_palette(d$series))) +
    ggplot2::labs(x = NULL, y = "Proportion", fill = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  p
}

.parse_proportion_formula <- function(formula) {
  if (is.null(formula)) return(list(variable = NULL, strata = character()))
  if (!inherits(formula, "formula") || length(formula) != 3) {
    .mimar_stop("`formula` must have the form variable ~ strata.")
  }
  rhs <- all.vars(formula[[3]])
  list(variable = all.vars(formula[[2]]), strata = rhs)
}

.proportion_plot_data <- function(x, variable = NULL, strata = character()) {
  original <- x$data_original
  vars <- names(original)[colSums(is.na(original)) > 0]
  vars <- vars[!vapply(original[vars], function(v) is.numeric(v) || is.integer(v) || inherits(v, "Date"), logical(1))]
  if (!is.null(variable)) vars <- intersect(vars, variable)
  if (!length(vars)) return(.as_dt(data.frame()))
  if (length(strata) && !all(strata %in% names(original))) {
    .mimar_stop("All stratifying variables in `formula` must be present in the imputed data.")
  }
  raw <- .rbind_or_empty(lapply(vars, function(nm) {
    obs_idx <- !is.na(original[[nm]])
    obs <- data.frame(
      variable = nm,
      imputation = 0L,
      value = as.character(original[[nm]][obs_idx]),
      panel = .strata_label(original, obs_idx, strata, nm),
      row.names = NULL
    )
    imp <- .rbind_or_empty(lapply(seq_along(x$imputations), function(mi) {
      completed <- x$imputations[[mi]]
      idx <- is.na(original[[nm]]) & !is.na(completed[[nm]])
      if (!any(idx)) return(NULL)
      data.frame(
        variable = nm,
        imputation = mi,
        value = as.character(completed[[nm]][idx]),
        panel = .strata_label(completed, idx, strata, nm),
        row.names = NULL
      )
    }))
    .rbind_or_empty(list(obs, imp))
  }))
  if (!nrow(raw)) return(raw)
  keys <- c("variable", "panel", "imputation")
  counts <- basetable::count(raw, by = c(keys, "value"), name = "n")
  totals <- basetable::count(raw, by = keys, name = "total")
  out <- basetable::merge(counts, totals, by = keys, all.x = TRUE)
  out$proportion <- out$n / out$total
  .as_dt(out)
}

.strata_label <- function(data, idx, strata, variable) {
  if (!length(strata)) return(rep(variable, sum(idx)))
  parts <- lapply(strata, function(nm) paste0(nm, "=", as.character(data[[nm]][idx])))
  do.call(paste, c(parts, sep = " | "))
}

.lab <- function(v, labels) {
  v <- as.character(v)
  if (is.null(labels)) return(v)
  i <- v %in% names(labels)
  v[i] <- labels[v[i]]
  v
}

# axis labels in percent without rounding 2.5% to "2%"
.pct <- function(z) paste0(format(100 * z, trim = TRUE, drop0trailing = TRUE), "%")

# blue and vermillion first, then Okabe-Ito
.group_palette <- function(x) {
  pal <- c("#0072B2", "#D55E00", "#009E73", "#E69F00", "#CC79A7", "#56B4E9", "#F0E442", "#000000")
  stats::setNames(rep(pal, length.out = length(x)), x)
}

# Okabe-Ito: colour-blind safe
.mimar_palette <- function(x) {
  pal <- c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#56B4E9", "#D55E00", "#F0E442", "#000000")
  stats::setNames(rep(pal, length.out = length(x)), x)
}

.mimar_status_palette <- function() c(observed = "grey20", imputed = "#0072B2")

.grey_shades <- function(x) {
  v <- round(seq(60, 180, length.out = length(x)))
  stats::setNames(sprintf("#%02X%02X%02X", v, v, v), x)
}

.series_palette <- function(s) {
  s <- unique(s); k <- sum(s != "observed")
  stats::setNames(c("grey20", grDevices::colorRampPalette(c("#92C5DE", "#08306B"))(k)),
                  c("observed", s[s != "observed"]))
}

# named palette entries replace defaults by name; an unnamed one is recycled in order
.pal <- function(p, def) {
  if (is.null(p)) return(def)
  if (is.null(names(p))) return(stats::setNames(rep_len(p, length(def)), names(def)))
  def[names(p)] <- p
  def
}

.gg_aes <- function(...) {
  args <- list(...)
  do.call(ggplot2::aes, lapply(args, as.name))
}

.empty_ggplot <- function(label) {
  ggplot2::ggplot(data.frame(x = 0, y = 0, label = label), .gg_aes(x = "x", y = "y")) +
    ggplot2::geom_text(.gg_aes(label = "label"), size = 4) +
    ggplot2::xlim(-1, 1) +
    ggplot2::ylim(-1, 1) +
    ggplot2::theme_void()
}

#' Forest plot of pooled estimates
#'
#' `plot()` on any pooled result (from [pool()], [poollm()], [poolglm()], [poolcoxph()],
#' [poolclogit()], [poolsurvreg()], [poolpanglm()]) draws the pooled estimates with their confidence
#' intervals, one row per term in model order.
#'
#' @param x A `mimar_pool` object.
#' @param exponentiate If `TRUE`, estimates and limits are exponentiated and drawn on a log axis with a
#'   reference line at 1 (hazard or odds ratios). Default `TRUE` for Cox and conditional logistic
#'   fits, `FALSE` otherwise.
#' @param labels Optional named character vector of display labels for terms, such as
#'   `c(lcopper = "log copper")`. Unnamed terms keep their names.
#' @param intercept If `FALSE` (default), the `(Intercept)` row is left out.
#' @param xlab Axis title. Default from the model type.
#' @param palette Optional colours with names `point` and `interval`.
#' @param ... Unused.
#' @return A `ggplot` object.
#' @export
plot.mimar_pool <- function(x, exponentiate = inherits(x, "mimar_pool_coxlike"), labels = NULL,
                            intercept = FALSE, xlab = NULL, palette = NULL, ...) {
  pal <- .pal(palette, c(point = "#0072B2", interval = "grey30"))
  d <- as.data.frame(x$pooled)
  d$label <- if ("term" %in% names(d)) d$term else d$metric
  if (!intercept) d <- d[d$label != "(Intercept)", , drop = FALSE]
  d$label <- factor(.lab(d$label, labels), levels = rev(unique(.lab(d$label, labels))))
  f <- if (exponentiate) exp else identity
  d[c("estimate", "conf.low", "conf.high")] <- lapply(d[c("estimate", "conf.low", "conf.high")], f)
  xlab <- xlab %||% if (!exponentiate) "Pooled estimate" else if (inherits(x, "mimar_pool_coxph"))
    "Hazard ratio" else if (inherits(x, "mimar_pool_clogit")) "Odds ratio" else "exp(pooled estimate)"
  ggplot2::ggplot(d, .gg_aes(x = "estimate", y = "label")) +
    ggplot2::geom_vline(xintercept = f(0), colour = "grey60", linewidth = 0.35, linetype = 2) +
    ggplot2::geom_errorbar(.gg_aes(xmin = "conf.low", xmax = "conf.high"),
                           orientation = "y", width = 0.18, colour = pal[["interval"]]) +
    ggplot2::geom_point(size = 2.2, colour = pal[["point"]]) +
    (if (exponentiate) ggplot2::scale_x_log10()) +
    ggplot2::labs(x = xlab, y = NULL) +
    ggplot2::theme_minimal(base_size = 12)
}

#' Plot a tipping-point analysis
#'
#' Draws the pooled estimate and its confidence band against the shift \eqn{\delta} from
#' [tippingpoint()], with a reference line at the null value and, if the interval reaches it within
#' the grid, a dotted line at the tipping point: the shift, linearly interpolated between grid
#' values, at which the confidence limit nearest the null first reaches it.
#'
#' @param x A `mimar_tippingpoint` object from [tippingpoint()].
#' @param null Null value of the estimate. Default 0.
#' @param tipping If `TRUE` (default), mark the tipping point.
#' @param labels Optional named character vector of display labels for terms (facet strips when
#'   several terms are shown).
#' @param xlab,ylab Axis titles.
#' @param palette Optional colours with names `line`, `band` and `tipping`.
#' @param ... Unused.
#' @return A `ggplot` object. The tipping points are in its `tipping` attribute.
#' @examples
#' imp <- impute(airquality, m = 5, seed = 1, progress = FALSE)
#' tp <- tippingpoint(imp, deltas = seq(0, -30, by = -5), variables = "Ozone",
#'                    model = Ozone ~ Wind, term = "Wind", pooler = poollm)
#' plot(tp)
#' @export
plot.mimar_tippingpoint <- function(x, null = 0, tipping = TRUE, labels = NULL, xlab = "Shift delta",
                                    ylab = "Pooled estimate", palette = NULL, ...) {
  pal <- .pal(palette, c(line = "#0072B2", band = "#0072B2", tipping = "#D55E00"))
  d <- as.data.frame(x)
  d$term <- factor(.lab(d$term, labels), levels = unique(.lab(d$term, labels)))
  tp <- do.call(rbind, lapply(split(d, d$term), function(z) {
    data.frame(term = z$term[1], delta = .tipping(z, null), row.names = NULL)
  }))
  p <- ggplot2::ggplot(d, .gg_aes(x = "delta", y = "estimate")) +
    ggplot2::geom_hline(yintercept = null, colour = "grey50", linewidth = 0.35) +
    ggplot2::geom_ribbon(.gg_aes(ymin = "conf.low", ymax = "conf.high"), fill = pal[["band"]], alpha = 0.2) +
    ggplot2::geom_line(colour = pal[["line"]], linewidth = 0.8) +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::theme_minimal(base_size = 12)
  if (tipping && any(!is.na(tp$delta)))
    p <- p + ggplot2::geom_vline(data = tp[!is.na(tp$delta), ], .gg_aes(xintercept = "delta"),
                                 colour = pal[["tipping"]], linetype = 3, linewidth = 0.7)
  if (nlevels(d$term) > 1) p <- p + ggplot2::facet_wrap(stats::as.formula("~ term"), scales = "free_y")
  attr(p, "tipping") <- tp
  p
}

# shift where the confidence limit nearest the null first reaches it, moving away from delta = 0
.tipping <- function(z, null) {
  z <- z[order(abs(z$delta)), ]
  if (z$conf.low[1] <= null && z$conf.high[1] >= null) return(NA_real_)
  b <- if (z$conf.low[1] > null) z$conf.low else z$conf.high
  s <- sign(b - null)
  i <- which(s != s[1])[1]
  if (is.na(i)) return(NA_real_)
  stats::approx(b[c(i - 1, i)], z$delta[c(i - 1, i)], null)$y
}

#' @export
plot.mimar_imputers <- function(x, palette = NULL, ...) {
  d <- as.data.frame(x)
  d$supported_tasks <- rowSums(d[, c("supports_numeric", "supports_binary", "supports_multiclass")])
  d$imputer <- stats::reorder(d$imputer, d$supported_tasks)
  ggplot2::ggplot(d, .gg_aes(x = "supported_tasks", y = "imputer", fill = "implementation")) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::scale_x_continuous(breaks = 0:3, limits = c(0, 3)) +
    ggplot2::scale_fill_manual(values = .pal(palette, c(mimar = "#0072B2", wrapped = "grey60"))) +
    ggplot2::labs(x = "Supported task types", y = NULL, fill = NULL) +
    ggplot2::theme_minimal(base_size = 12)
}
