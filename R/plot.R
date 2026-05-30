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
#' imputed distributions, variable-level imputation methods, or
#' between-imputation variability.
#'
#' @param x A `mimar_imputation` object.
#' @param type Plot type: `"imputed"`, `"missing"`, `"density"`, `"strip"`,
#'   `"methods"`, or `"variability"`.
#' @param variable Optional variable name or names used by distribution plots.
#' @param ... Unused.
#' @return A `ggplot` object.
#' @export
plot.mimar_imputation <- function(x, type = c("imputed", "missing", "density", "strip", "methods", "variability"),
                                  variable = NULL, ...) {
  type <- match.arg(type)
  .plot_imputation_ggplot(x, type = type, variable = variable)
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

.plot_imputation_ggplot <- function(x, type, variable = NULL) {
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
    return(
      ggplot2::ggplot(d, .gg_aes(x = "value", colour = "status", fill = "status")) +
        ggplot2::geom_density(alpha = 0.22, na.rm = TRUE) +
        ggplot2::facet_wrap(stats::as.formula("~ variable"), scales = "free") +
        ggplot2::scale_colour_manual(values = c(observed = "#4F5D75", imputed = "#B84A62")) +
        ggplot2::scale_fill_manual(values = c(observed = "#4F5D75", imputed = "#B84A62")) +
        ggplot2::labs(x = NULL, y = "Density", colour = NULL, fill = NULL) +
        ggplot2::theme_minimal(base_size = 12)
    )
  }
  ggplot2::ggplot(d, .gg_aes(x = "status", y = "value", colour = "status")) +
    ggplot2::geom_jitter(width = 0.18, height = 0, alpha = 0.55, na.rm = TRUE) +
    ggplot2::facet_wrap(stats::as.formula("~ variable"), scales = "free_y") +
    ggplot2::scale_colour_manual(values = c(observed = "#4F5D75", imputed = "#B84A62")) +
    ggplot2::labs(x = NULL, y = NULL, colour = NULL) +
    ggplot2::theme_minimal(base_size = 12)
}

.observed_imputed_plot_data <- function(x, variable = NULL) {
  original <- x$data_original
  first <- x$imputations[[1]]
  vars <- names(original)[colSums(is.na(original)) > 0]
  if (!is.null(variable)) vars <- intersect(vars, variable)
  .rbind_or_empty(lapply(vars, function(nm) {
    observed <- original[[nm]][!is.na(original[[nm]])]
    imputed <- first[[nm]][is.na(original[[nm]]) & !is.na(first[[nm]])]
    if (!length(observed) || !length(imputed)) return(NULL)
    numeric_var <- is.numeric(original[[nm]]) || is.integer(original[[nm]]) || inherits(original[[nm]], "Date")
    data.frame(
      variable = nm,
      status = rep(c("observed", "imputed"), c(length(observed), length(imputed))),
      value = if (numeric_var) as.numeric(c(observed, imputed)) else as.character(c(observed, imputed)),
      value_type = if (numeric_var) "numeric" else "categorical",
      row.names = NULL
    )
  }))
}

.mimar_palette <- function(x) {
  pal <- c("#2F6F73", "#B84A62", "#7768AE", "#D99A3D", "#4F5D75", "#5B8E7D")
  stats::setNames(rep(pal, length.out = length(x)), x)
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
