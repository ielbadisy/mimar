#' @export
plot.mimar_missing <- function(x, ...) {
  d <- x$variable_summary
  d$variable <- stats::reorder(d$variable, d$prop_missing)
  ggplot2::ggplot(d, .gg_aes(x = "prop_missing", y = "variable")) +
    ggplot2::geom_col(fill = "#2F6F73", width = 0.72) +
    ggplot2::scale_x_continuous(labels = function(z) paste0(round(100 * z), "%")) +
    ggplot2::labs(x = "Missing proportion", y = NULL) +
    ggplot2::theme_minimal(base_size = 12)
}

#' @export
plot.mimar_amputation <- function(x, ...) {
  d <- summary(x)
  long <- .rbind_or_empty(lapply(c("original", "added", "total"), function(status) {
    data.frame(variable = d$variable, status = status, n_missing = d[[status]], row.names = NULL)
  }))
  long$status <- factor(long$status, levels = c("original", "added", "total"))
  ggplot2::ggplot(long, .gg_aes(x = "variable", y = "n_missing", fill = "status")) +
    ggplot2::geom_col(position = "dodge", width = 0.72) +
    ggplot2::scale_fill_manual(values = c(original = "#4F5D75", added = "#B84A62", total = "#2F6F73")) +
    ggplot2::labs(x = NULL, y = "Missing cells", fill = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
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
#' @param ... Unused.
#' @return A `ggplot` object.
#' @export
plot.mimar_imputation <- function(x, type = c("imputed", "missing", "density", "strip", "boxplot",
                                              "xy", "proportion", "trace", "methods", "variability"),
                                  variable = NULL, formula = NULL,
                                  statistic = c("mean", "sd"), ...) {
  type <- match.arg(type)
  statistic <- match.arg(statistic)
  .plot_imputation_ggplot(x, type = type, variable = variable,
                          formula = formula, statistic = statistic)
}

#' @export
plot.mimar_evaluation <- function(x, ...) {
  d <- x$recovery
  if (is.null(d) || !nrow(d)) return(.empty_ggplot("No recovery metrics available"))
  d$value <- ifelse(!is.na(d$rmse), d$rmse, 1 - d$accuracy)
  d$metric <- ifelse(!is.na(d$rmse), "RMSE", "1 - accuracy")
  d$variable <- stats::reorder(d$variable, d$value)
  ggplot2::ggplot(d, .gg_aes(x = "value", y = "variable", fill = "metric")) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::scale_fill_manual(values = c(RMSE = "#2F6F73", `1 - accuracy` = "#B84A62")) +
    ggplot2::labs(x = "Recovery error", y = NULL, fill = NULL) +
    ggplot2::theme_minimal(base_size = 12)
}

.plot_imputation_ggplot <- function(x, type, variable = NULL, formula = NULL,
                                    statistic = "mean") {
  if (identical(type, "imputed")) {
    d <- .imputation_variable_summary(x)
    d$variable <- stats::reorder(d$variable, d$n_imputed)
    return(
      ggplot2::ggplot(d, .gg_aes(x = "n_imputed", y = "variable")) +
        ggplot2::geom_col(fill = "#2F6F73", width = 0.72) +
        ggplot2::labs(x = "Imputed cells", y = NULL) +
        ggplot2::theme_minimal(base_size = 12)
    )
  }
  if (identical(type, "missing")) return(.plot_missing_map_ggplot(x))
  if (identical(type, "boxplot")) return(.plot_boxplot_ggplot(x, variable = variable))
  if (identical(type, "xy")) return(.plot_xy_ggplot(x, formula = formula))
  if (identical(type, "proportion")) return(.plot_proportion_ggplot(x, variable = variable, formula = formula))
  if (identical(type, "trace")) return(.plot_trace_ggplot(x, variable = variable, statistic = statistic))
  if (identical(type, "methods")) {
    d <- .imputation_variable_summary(x)
    d <- d[d$n_missing_before > 0, , drop = FALSE]
    if (!nrow(d)) return(.empty_ggplot("No variables were imputed"))
    d$variable <- stats::reorder(d$variable, d$n_missing_before)
    return(
      ggplot2::ggplot(d, .gg_aes(x = "n_missing_before", y = "variable", fill = "method")) +
        ggplot2::geom_col(width = 0.72) +
        ggplot2::scale_fill_manual(values = .mimar_palette(unique(d$method)), na.value = "#8A8F98") +
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
    d$variable <- stats::reorder(d$variable, d$between_imputation_sd)
    return(
      ggplot2::ggplot(d, .gg_aes(x = "between_imputation_sd", y = "variable")) +
        ggplot2::geom_col(fill = "#7768AE", width = 0.72) +
        ggplot2::labs(x = "Between-imputation SD", y = NULL) +
        ggplot2::theme_minimal(base_size = 12)
    )
  }
  .plot_distribution_ggplot(x, type = type, variable = variable)
}

.plot_missing_map_ggplot <- function(x) {
  original <- x$data_original
  first <- x$imputations[[1]]
  d <- do.call(rbind, lapply(names(original), function(nm) {
    before <- is.na(original[[nm]])
    after <- is.na(first[[nm]])
    status <- ifelse(before & !after, "imputed", ifelse(before & after, "still missing", "observed"))
    data.frame(row = seq_len(nrow(original)), variable = nm, status = status, row.names = NULL)
  }))
  d$status <- factor(d$status, levels = c("observed", "imputed", "still missing"))
  ggplot2::ggplot(d, .gg_aes(x = "variable", y = "row", fill = "status")) +
    ggplot2::geom_tile() +
    ggplot2::scale_y_reverse() +
    ggplot2::scale_fill_manual(values = c(observed = "#E8ECEF", imputed = "#2F6F73", `still missing` = "#B84A62")) +
    ggplot2::labs(x = NULL, y = "Row", fill = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

.plot_distribution_ggplot <- function(x, type, variable = NULL) {
  d <- .observed_imputed_plot_data(x, variable = variable)
  if (!nrow(d)) return(.empty_ggplot("No plottable imputed variables available"))
  if (identical(type, "density")) {
    d <- d[d$value_type == "numeric", , drop = FALSE]
    if (!nrow(d)) return(.empty_ggplot("Density diagnostics require numeric imputed variables"))
    d$value <- as.numeric(d$value)
    d$series <- ifelse(d$imputation == 0, "observed", paste0("imputation ", d$imputation))
    d$group_id <- interaction(d$variable, d$series, drop = TRUE)
    return(
      ggplot2::ggplot(d, .gg_aes(x = "value", colour = "status", group = "group_id")) +
        ggplot2::geom_density(linewidth = 0.65, na.rm = TRUE) +
        ggplot2::facet_wrap(stats::as.formula("~ variable"), scales = "free") +
        ggplot2::scale_colour_manual(values = .mimar_status_palette()) +
        ggplot2::labs(x = NULL, y = "Density", colour = NULL) +
        ggplot2::theme_minimal(base_size = 12)
    )
  }
  d$imputation <- factor(d$imputation)
  ggplot2::ggplot(d, .gg_aes(x = "imputation", y = "value", colour = "status")) +
    ggplot2::geom_jitter(width = 0.18, height = 0, alpha = 0.55, na.rm = TRUE) +
    ggplot2::facet_wrap(stats::as.formula("~ variable"), scales = "free_y") +
    ggplot2::scale_colour_manual(values = .mimar_status_palette()) +
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

.plot_boxplot_ggplot <- function(x, variable = NULL) {
  d <- .observed_imputed_plot_data(x, variable = variable)
  d <- d[d$value_type == "numeric", , drop = FALSE]
  if (!nrow(d)) return(.empty_ggplot("Boxplot diagnostics require numeric imputed variables"))
  d$imputation <- factor(d$imputation, levels = sort(unique(d$imputation)))
  ggplot2::ggplot(d, .gg_aes(x = "imputation", y = "value", colour = "status")) +
    ggplot2::geom_boxplot(outlier.alpha = 0.45, width = 0.62, na.rm = TRUE) +
    ggplot2::stat_summary(fun = mean, geom = "point", size = 2, na.rm = TRUE) +
    ggplot2::facet_wrap(stats::as.formula("~ variable"), scales = "free_y") +
    ggplot2::scale_colour_manual(values = .mimar_status_palette()) +
    ggplot2::labs(x = "Imputation number", y = NULL, colour = NULL) +
    ggplot2::theme_minimal(base_size = 12)
}

.plot_trace_ggplot <- function(x, variable = NULL, statistic = "mean") {
  d <- x$diagnostics$trace
  if (is.null(d) || !nrow(d)) return(.empty_ggplot("No convergence trace available"))
  d <- d[!is.na(d[[statistic]]), , drop = FALSE]
  if (!is.null(variable)) d <- d[d$variable %in% variable, , drop = FALSE]
  if (!nrow(d)) return(.empty_ggplot("No numeric trace available for selected variables"))
  d$imputation <- factor(d$imputation)
  ggplot2::ggplot(d, .gg_aes(x = "iteration", y = statistic, colour = "imputation", group = "imputation")) +
    ggplot2::geom_line(linewidth = 0.55, alpha = 0.85) +
    ggplot2::geom_point(size = 1.2, alpha = 0.75) +
    ggplot2::facet_wrap(stats::as.formula("~ variable"), scales = "free_y") +
    ggplot2::scale_colour_manual(values = .mimar_palette(levels(d$imputation))) +
    ggplot2::labs(x = "Iteration", y = statistic, colour = "Imputation") +
    ggplot2::theme_minimal(base_size = 12)
}

.plot_xy_ggplot <- function(x, formula = NULL) {
  if (is.null(formula)) .mimar_stop("`formula` is required for type = 'xy'.")
  spec <- .parse_xy_formula(formula)
  d <- .xy_plot_data(x, spec)
  if (!nrow(d)) return(.empty_ggplot("No bivariate diagnostic data available"))
  p <- ggplot2::ggplot(d, .gg_aes(x = "x", y = "y", colour = "status", fill = "status")) +
    ggplot2::geom_point(.gg_aes(shape = "status"), alpha = 0.7, size = 1.8, na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = .mimar_status_palette()) +
    ggplot2::scale_fill_manual(values = c(observed = "#FFFFFF", imputed = .mimar_status_palette()[["imputed"]])) +
    ggplot2::scale_shape_manual(values = c(observed = 21, imputed = 19)) +
    ggplot2::labs(x = spec$x, y = spec$y, colour = NULL, fill = NULL, shape = NULL) +
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

.plot_proportion_ggplot <- function(x, variable = NULL, formula = NULL) {
  spec <- .parse_proportion_formula(formula)
  if (!is.null(spec$variable)) variable <- spec$variable
  d <- .proportion_plot_data(x, variable = variable, strata = spec$strata)
  if (!nrow(d)) return(.empty_ggplot("No categorical imputation proportions available"))
  d$imputation <- factor(d$imputation)
  d$series <- ifelse(d$imputation == "0", "observed", paste0("imp ", d$imputation))
  p <- ggplot2::ggplot(d, .gg_aes(x = "value", y = "proportion", fill = "series")) +
    ggplot2::geom_col(position = "dodge", width = 0.72) +
    ggplot2::facet_wrap(stats::as.formula("~ panel"), scales = "free_x") +
    ggplot2::scale_fill_manual(values = .mimar_palette(unique(d$series))) +
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
  if (!length(vars)) return(.as_tibble(data.frame()))
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
  counts <- stats::aggregate(list(n = rep(1L, nrow(raw))),
                             raw[c("variable", "panel", "imputation", "value")],
                             sum)
  totals <- stats::aggregate(list(total = counts$n),
                             counts[c("variable", "panel", "imputation")],
                             sum)
  out <- merge(counts, totals, by = c("variable", "panel", "imputation"), all.x = TRUE)
  out$proportion <- out$n / out$total
  .as_tibble(out)
}

.strata_label <- function(data, idx, strata, variable) {
  if (!length(strata)) return(rep(variable, sum(idx)))
  parts <- lapply(strata, function(nm) paste0(nm, "=", as.character(data[[nm]][idx])))
  do.call(paste, c(parts, sep = " | "))
}

.mimar_palette <- function(x) {
  pal <- c("#2F6F73", "#D99A3D", "#6E5F9F", "#C76E4C", "#3F7D8C", "#7A8B4F")
  stats::setNames(rep(pal, length.out = length(x)), x)
}

.mimar_status_palette <- function() {
  c(observed = "#264653", imputed = "#D99A3D")
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

#' @export
plot.mimar_pool <- function(x, ...) {
  d <- x$pooled
  d$label <- if ("term" %in% names(d)) d$term else d$metric
  d$label <- stats::reorder(d$label, d$estimate)
  ggplot2::ggplot(d, .gg_aes(x = "estimate", y = "label")) +
    ggplot2::geom_vline(xintercept = 0, colour = "#B8C0C7", linewidth = 0.35) +
    ggplot2::geom_errorbar(.gg_aes(xmin = "conf.low", xmax = "conf.high"),
                           orientation = "y", width = 0.18, colour = "#4F5D75") +
    ggplot2::geom_point(size = 2.2, colour = "#2F6F73") +
    ggplot2::labs(x = "Pooled estimate", y = NULL) +
    ggplot2::theme_minimal(base_size = 12)
}

#' @export
plot.mimar_imputers <- function(x, ...) {
  d <- as.data.frame(x)
  d$supported_tasks <- rowSums(d[, c("supports_numeric", "supports_binary", "supports_multiclass")])
  d$imputer <- stats::reorder(d$imputer, d$supported_tasks)
  ggplot2::ggplot(d, .gg_aes(x = "supported_tasks", y = "imputer", fill = "implementation")) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::scale_x_continuous(breaks = 0:3, limits = c(0, 3)) +
    ggplot2::scale_fill_manual(values = c(mimar = "#2F6F73", wrapped = "#7768AE")) +
    ggplot2::labs(x = "Supported task types", y = NULL, fill = NULL) +
    ggplot2::theme_minimal(base_size = 12)
}
